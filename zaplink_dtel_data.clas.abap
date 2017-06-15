class ZAPLINK_DTEL_DATA definition
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
    tt_dd04t TYPE SORTED TABLE OF dd04t WITH UNIQUE KEY ddlanguage .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
        header        TYPE dd04v,
        param         TYPE tpara,
        texts         TYPE tt_dd04t,
        docs          TYPE tt_docs,
      END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd04v AS hdr.
    INCLUDE TYPE tpara AS param.
    TYPES:
      END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .

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
      header TYPE td_doc_id VALUE 'DE',
    END OF doc_ids .
private section.
ENDCLASS.



CLASS ZAPLINK_DTEL_DATA IMPLEMENTATION.


  method ANONYMIZE.
  FIELD-SYMBOLS:
    <t> like LINE OF texts.

  clear: a0_maindata-AS4USER, a0_maindata-AS4DATE, a0_maindata-AS4TIME.
  loop at texts ASSIGNING <t>.
    clear: <t>-LONG_TXT-TDFUSER, <t>-LONG_TXT-TDFDATE, <t>-LONG_TXT-TDFTIME.
  endloop.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method FROM_DATA.
  DATA s_text  LIKE LINE OF texts.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t> LIKE LINE OF fm_data-texts.

  a0_maindata-hdr = fm_data-header.
  a0_maindata-param = fm_data-param.

* Texts
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-langu = <t>-ddlanguage.
    s_text-short_txt = <t>-ddtext.
    READ TABLE fm_data-docs ASSIGNING <d>
         WITH KEY id = doc_ids-header
              object = a0_maindata-rollname.
    IF sy-subrc = 0.
      READ TABLE <d>-texts ASSIGNING <dt>
          WITH KEY tdspras = s_text-langu.
      IF sy-subrc = 0.
        s_text-long_txt = <dt>.
        CLEAR s_text-long_txt-tdspras.
      ENDIF.
    ENDIF.
    INSERT s_text INTO TABLE texts.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_text  LIKE LINE OF fm_data-texts.
  DATA s_doc   LIKE LINE OF fm_data-docs.
  DATA s_dtxt  LIKE LINE OF s_doc-texts.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-docs,
    <t> LIKE LINE OF texts.

  fm_data-header = a0_maindata-hdr.

* Texts
  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-rollname = fm_data-header-rollname.
    s_text-ddlanguage = <t>-langu.
    s_text-as4local = 'A'. " Active
    s_text-as4vers = '0000'. " Active
    s_text-ddtext = <t>-short_txt.
    INSERT s_text INTO TABLE fm_data-texts.
    IF NOT <t>-long_txt IS INITIAL.
      IF fm_data-docs IS INITIAL.
*APPLICATION    DOKU
*ID	            DO
*OBJECT	        ____
*LANGU          E
*MASTERLANG	    X
*TYP            T
*DOKFORM        S_DOCU_SHOW
*DOKSTYLE	      S_DOCUS1
*SELFDEF
        s_doc-application = 'DOKU'.
        s_doc-id = doc_ids-header.
        s_doc-object = fm_data-header-rollname.
        s_doc-langu = <t>-langu.
        s_doc-masterlang = abap_true.
        s_doc-typ = 'T'.
        s_doc-dokform = 'S_DOCU_SHOW'.
        s_doc-dokstyle = 'S_DOCUS1'.
        INSERT s_doc INTO TABLE fm_data-docs.
      ENDIF.
      READ TABLE fm_data-docs ASSIGNING <d> INDEX 1.
      s_dtxt = <t>-long_txt.
      s_dtxt-tdspras = <t>-langu.
      INSERT s_dtxt INTO TABLE <d>-texts.
    ENDIF.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
