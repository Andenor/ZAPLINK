class ZAPLINK_SHLP_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_DICTIONARY
                 ZAPLINK_EASYXML .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .

  types TO_ME type ref to ZAPLINK_SHLP_DATA .
  types:
    tt_dd30t TYPE SORTED TABLE OF dd30t WITH UNIQUE KEY ddlanguage .
  types:
    tt_dd31v TYPE STANDARD TABLE OF dd31v WITH DEFAULT KEY .
  types:
    tt_dd32p TYPE STANDARD TABLE OF dd32p WITH DEFAULT KEY .
  types:
    tt_dd33v TYPE STANDARD TABLE OF dd33v WITH DEFAULT KEY .
  types:
    BEGIN OF ts_fm_data,
        header        TYPE dd30v,
        texts         TYPE tt_dd30t,
        docs          TYPE tt_docs,
        fields        TYPE tt_dd32p,
        sub_sh        TYPE tt_dd31v,
        ssh_params    TYPE tt_dd33v,
      END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd30v AS hdr.
    TYPES:
      END OF ts_maindata .
  types:
    tt_fields TYPE SORTED TABLE OF dd32p WITH UNIQUE KEY fieldname .
  types:
    tt_params TYPE SORTED TABLE OF dd33v WITH UNIQUE KEY subfield .
  types:
    BEGIN OF ts_sub_sh.
    INCLUDE TYPE dd31v AS hdr.
    TYPES:
        params        TYPE tt_params,
        zl_object     TYPE to_me,
      END OF ts_sub_sh .
  types:
    tt_sub_sh TYPE SORTED TABLE OF ts_sub_sh WITH UNIQUE KEY shposition .

  data A0_MAINDATA type TS_MAINDATA .
  data PARAMS type TT_FIELDS .
  data SUB_SEARCH_HELPS type TT_SUB_SH .
  data TEXTS type TT_TEXTS .

  methods ANONYMIZE .
  class-methods CLASS_CONSTRUCTOR .
  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods UNANONYMIZE .
protected section.

  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .

  constants:
    BEGIN OF doc_ids,
      header TYPE td_doc_id VALUE 'TT',
    END OF doc_ids .
  class-data R_DOC_IDS type TR_DOCID .
private section.
ENDCLASS.



CLASS ZAPLINK_SHLP_DATA IMPLEMENTATION.


  method ANONYMIZE.
  FIELD-SYMBOLS   <s> LIKE LINE OF sub_search_helps.

  CLEAR: a0_maindata-as4user, a0_maindata-as4date, a0_maindata-as4time.
  LOOP AT sub_search_helps ASSIGNING <s>.
    IF <s>-zl_object IS BOUND.    <s>-zl_object->anonymize( ).    ENDIF.
  ENDLOOP.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method FROM_DATA.
  DATA s_text    LIKE LINE OF texts.
  DATA s_param   LIKE LINE OF params.
  DATA s_ssh     LIKE LINE OF sub_search_helps.
  DATA s_sshp    LIKE LINE OF s_ssh-params.
  DATA t_docs    TYPE tt_docs.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <s>  LIKE LINE OF fm_data-sub_sh,
    <sp> LIKE LINE OF fm_data-ssh_params,
    <t>  LIKE LINE OF fm_data-texts.

  a0_maindata-hdr = fm_data-header.   CLEAR a0_maindata-ddtext.
  params = fm_data-fields.    MODIFY params FROM s_param TRANSPORTING shlpname WHERE table_line IS NOT INITIAL.

* Texts
  t_docs = fm_data-docs.
  READ TABLE t_docs ASSIGNING <d>
       WITH KEY id = doc_ids-header
            object = a0_maindata-shlpname.
  LOOP AT fm_data-texts ASSIGNING <t>
          WHERE shlpname = a0_maindata-shlpname.
    CLEAR s_text.   s_text-langu = <t>-ddlanguage.    s_text-short_txt = <t>-ddtext.
    IF <d> IS ASSIGNED.
      READ TABLE <d>-texts ASSIGNING <dt>
          WITH KEY tdspras = s_text-langu.
      IF sy-subrc = 0.    s_text-long_txt = <dt>.   CLEAR s_text-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.    ENDIF.
    ENDIF.
    INSERT s_text INTO TABLE texts.
  ENDLOOP.
  IF <d> IS ASSIGNED.
    LOOP AT <d>-texts ASSIGNING <dt>.
      CLEAR s_text.    s_text-langu = <dt>-tdspras.   s_text-long_txt = <dt>.   CLEAR s_text-long_txt-tdspras.    INSERT s_text INTO TABLE texts.
    ENDLOOP.
  ENDIF.

* SUB_SEARCH_HELPS
  LOOP AT fm_data-sub_sh ASSIGNING <s>
          WHERE shlpname = a0_maindata-shlpname.
    CLEAR s_ssh.   s_ssh-hdr = <s>.   CLEAR s_ssh-shlpname.
    IF s_ssh-viashlp = a0_maindata-shlpname.  CLEAR s_ssh-viashlp.  ENDIF.
    LOOP AT fm_data-ssh_params ASSIGNING <sp>
            WHERE shlpname = a0_maindata-shlpname
              AND subshlp = s_ssh-subshlp.
      CLEAR s_sshp.   s_sshp = <sp>.    CLEAR: s_sshp-shlpname, s_sshp-subshlp, s_sshp-valuedirec.    "auto determinded
      INSERT s_sshp INTO TABLE s_ssh-params.
    ENDLOOP.
    DELETE s_ssh-params WHERE fieldname IS INITIAL.
    INSERT s_ssh INTO TABLE sub_search_helps.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_txt      LIKE LINE OF fm_data-texts.
  DATA s_field    LIKE LINE OF fm_data-fields.
  DATA s_doc      LIKE LINE OF fm_data-docs.
  DATA s_doc_txt  LIKE LINE OF s_doc-texts.
  DATA s_ssh      LIKE LINE OF fm_data-sub_sh.
  DATA s_sshp     LIKE LINE OF fm_data-ssh_params.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <s>  LIKE LINE OF sub_search_helps,
    <sp> LIKE LINE OF <s>-params,
    <t>  LIKE LINE OF texts.

  fm_data-header = a0_maindata-hdr.
  s_field-shlpname = fm_data-header-shlpname.   fm_data-fields = params.    MODIFY fm_data-fields FROM s_field TRANSPORTING shlpname WHERE table_line IS NOT INITIAL.

* Texts
*APPLICATION                                      DOKU
*ID                                      RE
*OBJECT                                      SAPLxxxx
*LANGU                                      E
*MASTERLANG                                      X
*TYP                                      E
*DOKFORM                                      S_DOCU_SHOW
*DOKSTYLE                                      S_DOCUS1
  s_doc-application = 'DOKU'.   s_doc-id = doc_ids-header.   s_doc-object = fm_data-header-shlpname.
  s_doc-masterlang = abap_true.   s_doc-typ = 'E'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
  LOOP AT texts ASSIGNING <t>.
    IF NOT <t>-short_txt IS INITIAL.    CLEAR s_txt.    s_txt-shlpname = fm_data-header-shlpname.   s_txt-ddlanguage = <t>-langu.   s_txt-ddtext = <t>-short_txt.   INSERT s_txt INTO TABLE fm_data-texts.    ENDIF.
    IF NOT <t>-long_txt IS INITIAL.
      CLEAR s_doc_txt.    s_doc_txt = <t>-long_txt.   s_doc_txt-tdspras = <t>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
      IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.

* SUB_SEARCH_HELPS
  LOOP AT sub_search_helps ASSIGNING <s>.
    CLEAR s_ssh.   s_ssh = <s>-hdr.   s_ssh-shlpname = a0_maindata-shlpname.
    IF s_ssh-viashlp IS INITIAL.    s_ssh-viashlp = a0_maindata-shlpname.  ENDIF.
    INSERT s_ssh INTO TABLE fm_data-sub_sh.
    LOOP AT <s>-params ASSIGNING <sp>.
      CLEAR s_sshp.   s_sshp = <sp>.    s_sshp-shlpname = a0_maindata-shlpname.   s_sshp-subshlp = <s>-subshlp.
      INSERT s_sshp INTO TABLE fm_data-ssh_params.
    ENDLOOP.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
