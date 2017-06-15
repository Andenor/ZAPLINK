class ZAPLINK_ENQU_DATA definition
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
    tt_dd26e TYPE STANDARD TABLE OF dd26e WITH DEFAULT KEY .
  types:
    tt_dd27p TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY .
  types:
    tt_dd25t TYPE SORTED TABLE OF dd25t WITH UNIQUE KEY ddlanguage .
  types:
    BEGIN OF ts_fm_data,
          header        TYPE dd25v,
          texts         TYPE tt_dd25t,
          docs          TYPE tt_docs,
          tables        TYPE tt_dd26e,
          fields        TYPE tt_dd27p,
        END OF ts_fm_data .
  types:
    tt_fields TYPE SORTED TABLE OF dd53d WITH UNIQUE KEY viewfield .
  types:
    tt_tables TYPE SORTED TABLE OF dd26e WITH UNIQUE KEY tabpos .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd25v AS hdr.
    TYPES:
      END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .
  data FIELDS type TT_FIELDS .
  data TABLES type TT_TABLES .

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



CLASS ZAPLINK_ENQU_DATA IMPLEMENTATION.


  method ANONYMIZE.
*  FIELD-SYMBOLS   <t> like LINE OF texts.
*
*  clear: a0_maindata-AS4USER, a0_maindata-AS4DATE, a0_maindata-AS4TIME.
*  loop at texts ASSIGNING <t>.    clear: <t>-LONG_TXT-TDFUSER, <t>-LONG_TXT-TDFDATE, <t>-LONG_TXT-TDFTIME.    endloop.
  endmethod.


  method CLASS_CONSTRUCTOR.
* Link beetween Lock Object and Module in :
* LSD50F01 : form l_show_lock_functions.
*  concatenate '*QUEUE_' df-ddxx-viewname into func_name.
*  call function 'F4_FUNCTIONMODULE'
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method FROM_DATA.
  DATA s_text    LIKE LINE OF texts.
  DATA s_field   LIKE LINE OF fields.
  DATA s_table   LIKE LINE OF tables.
  DATA t_docs    TYPE tt_docs.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <f>  LIKE LINE OF fm_data-fields,
    <ta> LIKE LINE OF fm_data-tables,
    <t>  LIKE LINE OF fm_data-texts.

  a0_maindata-hdr = fm_data-header.   CLEAR a0_maindata-ddtext.

* Texts
  t_docs = fm_data-docs.
  READ TABLE t_docs ASSIGNING <d>
       WITH KEY id = doc_ids-header
            object = a0_maindata-viewname.
  LOOP AT fm_data-texts ASSIGNING <t>
          WHERE viewname = a0_maindata-viewname.
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

* Tables
  LOOP AT fm_data-tables ASSIGNING <ta>
          WHERE viewname = a0_maindata-viewname.
    CLEAR s_table.   s_table = <ta>.   CLEAR: s_table-viewname, s_table-ddlanguage, s_table-fordir. ", s_table-forfield.
    IF s_table-tabpos = '0001'.  CLEAR s_table-fortabname.  ENDIF.    " Primary Table
    INSERT s_table INTO TABLE tables.
  ENDLOOP.

*Fields
  LOOP AT fm_data-fields ASSIGNING <f>
          WHERE viewname = a0_maindata-viewname.
    CLEAR s_field.   MOVE-CORRESPONDING <f> TO s_field.   CLEAR: s_field-keyflag, s_field-rollnamevi, s_field-rollname, s_field-rollchange, s_field-datatype, s_field-flength, s_field-rdonly, s_field-ddtext.
    INSERT s_field INTO TABLE fields.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_txt      LIKE LINE OF fm_data-texts.
  DATA s_doc      LIKE LINE OF fm_data-docs.
  DATA s_doc_txt  LIKE LINE OF s_doc-texts.
  DATA s_field    LIKE LINE OF fm_data-fields.
  DATA s_table    LIKE LINE OF fm_data-tables.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <f>  LIKE LINE OF fields,
    <ta> LIKE LINE OF tables,
    <t>  LIKE LINE OF texts.

  fm_data-header = a0_maindata-hdr.

* Texts
*APPLICATION                                      DOKU
*ID                                      RE
*OBJECT                                      SAPLxxxx
*LANGU                                      E
*MASTERLANG                                      X
*TYP                                      E
*DOKFORM                                      S_DOCU_SHOW
*DOKSTYLE                                      S_DOCUS1
  s_doc-application = 'DOKU'.   s_doc-id = doc_ids-header.   s_doc-object = fm_data-header-viewname.
  s_doc-masterlang = abap_true.   s_doc-typ = 'E'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
  LOOP AT texts ASSIGNING <t>.
    IF NOT <t>-short_txt IS INITIAL.    CLEAR s_txt.    s_txt-viewname = fm_data-header-viewname.   s_txt-ddlanguage = <t>-langu.   s_txt-ddtext = <t>-short_txt.   INSERT s_txt INTO TABLE fm_data-texts.    ENDIF.
    IF NOT <t>-long_txt IS INITIAL.
      CLEAR s_doc_txt.    s_doc_txt = <t>-long_txt.   s_doc_txt-tdspras = <t>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
      IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.

* Tables
  LOOP AT tables ASSIGNING <ta>.
    CLEAR s_table.   s_table = <ta>.   s_table-viewname = a0_maindata-viewname.
    IF s_table-tabpos = '0001'.  s_table-fortabname = s_table-tabname.  ENDIF.    " Primary Table
    INSERT s_table INTO TABLE fm_data-tables.
  ENDLOOP.

*Fields
  LOOP AT fields ASSIGNING <f>.
    CLEAR s_field.   s_field-objpos = sy-tabix.   MOVE-CORRESPONDING <f> TO s_field.   s_field-keyflag = abap_true.    s_field-viewname = a0_maindata-viewname.
    INSERT s_field INTO TABLE fm_data-fields.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
