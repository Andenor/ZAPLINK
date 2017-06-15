class ZAPLINK_TTYP_DATA definition
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

  types:
    tt_dd42v TYPE STANDARD TABLE OF dd42v WITH DEFAULT KEY .
  types:
    tt_dd40t TYPE SORTED TABLE OF dd40t WITH UNIQUE KEY ddlanguage .
  types:
    BEGIN OF ts_fm_data,
      header        TYPE dd40v,
      texts         TYPE tt_dd40t,
      keys          TYPE tt_dd42v,
      docs          type tt_docs,
    END OF ts_fm_data .
  types:
    tt_keys type SORTED TABLE OF dd42v with UNIQUE key KEYFDPOS .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd40v AS hdr.
    TYPES:
      END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .
  data KEYS type TT_KEYS .

  class-methods CLASS_CONSTRUCTOR .
  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.

  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .

  class-data R_DOC_IDS type TR_DOCID .
  constants:
    BEGIN OF doc_ids,
      header TYPE td_doc_id VALUE 'TT',
    END OF doc_ids .
private section.
ENDCLASS.



CLASS ZAPLINK_TTYP_DATA IMPLEMENTATION.


  method ANONYMIZE.
  FIELD-SYMBOLS   <t> like LINE OF texts.

  clear: a0_maindata-AS4USER, a0_maindata-AS4DATE, a0_maindata-AS4TIME.
  loop at texts ASSIGNING <t>.    clear: <t>-LONG_TXT-TDFUSER, <t>-LONG_TXT-TDFDATE, <t>-LONG_TXT-TDFTIME.    endloop.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method FROM_DATA.
  DATA s_text    LIKE LINE OF texts.
  DATA s_key     LIKE LINE OF keys.
  DATA t_docs    TYPE tt_docs.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t>  LIKE LINE OF fm_data-texts.

  a0_maindata-hdr = fm_data-header.   CLEAR a0_maindata-ddtext.
  keys = fm_data-keys.    MODIFY keys FROM s_key TRANSPORTING typename rowtypepos WHERE table_line IS NOT INITIAL.

* Texts
  t_docs = fm_data-docs.
  READ TABLE t_docs ASSIGNING <d>
       WITH KEY id = doc_ids-header
            object = a0_maindata-typename.
  LOOP AT fm_data-texts ASSIGNING <t>.
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
  endmethod.


  method TO_DATA.
  DATA s_txt      LIKE LINE OF fm_data-texts.
  DATA s_key      LIKE LINE OF fm_data-keys.
  DATA s_doc      LIKE LINE OF fm_data-docs.
  DATA s_doc_txt  LIKE LINE OF s_doc-texts.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <t>  LIKE LINE OF texts.

  fm_data-header = a0_maindata-hdr.
  s_key-typename = fm_data-header-typename.   fm_data-keys = keys.    MODIFY fm_data-keys FROM s_key TRANSPORTING typename WHERE table_line IS NOT INITIAL.

* Texts
*APPLICATION                                      DOKU
*ID	                                   	RE
*OBJECT	                                   	SAPLxxxx
*LANGU                                      E
*MASTERLANG	                                   	X
*TYP                                      E
*DOKFORM                                      S_DOCU_SHOW
*DOKSTYLE	                                   	S_DOCUS1
  s_doc-application = 'DOKU'.   s_doc-id = doc_ids-header.   s_doc-object = fm_data-header-typename.
  s_doc-masterlang = abap_true.   s_doc-typ = 'E'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
  LOOP AT texts ASSIGNING <t>.
    IF NOT <t>-short_txt IS INITIAL.    CLEAR s_txt.    s_txt-typename = fm_data-header-typename.   s_txt-ddlanguage = <t>-langu.   s_txt-ddtext = <t>-short_txt.   INSERT s_txt INTO TABLE fm_data-texts.    ENDIF.
    IF NOT <t>-long_txt IS INITIAL.
      CLEAR s_doc_txt.    s_doc_txt = <t>-long_txt.   s_doc_txt-tdspras = <t>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
      IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
