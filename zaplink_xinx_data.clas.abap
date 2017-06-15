class ZAPLINK_XINX_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_DICTIONARY
                 ZAPLINK_EASYXML
                 ZAPLINK_XINX_RAW .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .

  types:
    tt_dd17v TYPE STANDARD TABLE OF dd17v WITH DEFAULT KEY .
  types:
    tt_dd12t TYPE STANDARD TABLE OF dd12t WITH DEFAULT KEY .
  types:
    BEGIN OF ts_idx_key,
        table  TYPE tabname,
        index  TYPE indexid,
      END OF ts_idx_key .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
                header        TYPE dd12v,
                texts         TYPE tt_dd12t,
                fields        TYPE tt_dd17v,
                docs          TYPE tt_docs,
              END OF ts_fm_data .
  types:
    tt_fields TYPE SORTED TABLE OF dd17v WITH UNIQUE KEY position .
  types TS_MAINDATA type DD12V .

  data A0_MAINDATA type TS_MAINDATA .
  data FIELDS type TT_FIELDS .
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

  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .

  class-data R_DOC_IDS type TR_DOCID .
  constants:
    BEGIN OF doc_ids,
      header TYPE td_doc_id VALUE 'TB',
    END OF doc_ids .
  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .

  class-methods NAME_2_KEY
    importing
      !DATA type TD_COMPNAME
    returning
      value(RESULT) type TS_IDX_KEY .
  class-methods KEY_2_NAME
    importing
      !DATA type TS_IDX_KEY
    returning
      value(RESULT) type TD_COMPNAME .
private section.
ENDCLASS.



CLASS ZAPLINK_XINX_DATA IMPLEMENTATION.


  method ANONYMIZE.
  CLEAR: a0_maindata-as4user, a0_maindata-as4date, a0_maindata-as4time.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method FROM_DATA.
  DATA s_text  LIKE LINE OF texts.
  DATA s_field LIKE LINE OF fields.
  DATA d_str   TYPE string.
  DATA s_key   TYPE ts_idx_key.
  FIELD-SYMBOLS:
    <f> LIKE LINE OF fm_data-fields,
    <d> LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t> LIKE LINE OF fm_data-texts.

  a0_maindata = fm_data-header.
  s_key-table = a0_maindata-sqltab.   s_key-index = a0_maindata-indexname.
  CLEAR: a0_maindata-sqltab, a0_maindata-indexname.

* Texts
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-langu = <t>-ddlanguage.
    s_text-short_txt = <t>-ddtext.
    READ TABLE fm_data-docs ASSIGNING <d>
         WITH KEY id = doc_ids-header
              object = s_key.
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

* fields
  LOOP AT fm_data-fields ASSIGNING <f>
          WHERE sqltab = s_key-table
         AND indexname = s_key-index.
    CLEAR s_field.    s_field = <f>.       CLEAR: s_field-sqltab, s_field-indexname.
    INSERT s_field INTO TABLE fields.
  ENDLOOP.
  endmethod.


  method KEY_2_NAME.
  DATA d_obj_name TYPE  e071-obj_name.

  CALL FUNCTION 'DD_DD_TO_E071'
    EXPORTING
      type                = zaplink_dictionary=>supportedtypes-ext_index
      name                = data-table
      id                  = data-index
    IMPORTING
*     PGMID               =
*     OBJECT              =
      obj_name            = d_obj_name
    EXCEPTIONS
      illegal_input       = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    mac_raise_mf 'DD_DD_TO_E071' sy-subrc.
  ELSE.
    result = d_obj_name.
  ENDIF.
  endmethod.


  method NAME_2_KEY.
  DATA d_obj_name TYPE  e071-obj_name.

  d_obj_name = data.
  CALL FUNCTION 'DD_E071_TO_DD'
    EXPORTING
      object              = zaplink_dictionary=>supportedtypes-ext_index
      obj_name            = d_obj_name
    IMPORTING
*      TYPE                =
      name                = result-table
      id                  = result-index
    EXCEPTIONS
      illegal_input       = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
    mac_raise_mf 'DD_DD_TO_E071' sy-subrc.
  ENDIF.
  endmethod.


  method TO_DATA.
  DATA s_text  LIKE LINE OF fm_data-texts.
  DATA s_field LIKE LINE OF fm_data-fields.
  data s_doc   like LINE OF fm_data-docs.
  DATA s_key   TYPE ts_idx_key.
  FIELD-SYMBOLS:
    <f>  LIKE LINE OF fields,
    <d>  LIKE LINE OF fm_data-docs,
    <t>  LIKE LINE OF texts.
  data s_dtxt like LINE OF <d>-texts.

  fm_data-header = a0_maindata.
  s_key-table = a0_maindata-sqltab.   s_key-index = a0_maindata-indexname.

* Texts
  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-sqltab = fm_data-header-sqltab.
    s_text-indexname = fm_data-header-indexname.
    s_text-ddlanguage = <t>-langu.
    s_text-as4local = 'A'. " Active
    s_text-as4vers = '0000'. " Active
    s_text-ddtext = <t>-short_txt.
    INSERT s_text INTO TABLE fm_data-texts.
    IF NOT <t>-long_txt IS INITIAL.
      IF fm_data-docs IS INITIAL.
        s_doc-application = 'DOKU'.
        s_doc-id = doc_ids-header.
        s_doc-object = s_key.
        s_doc-langu = <t>-langu.
        s_doc-masterlang = abap_true.
        s_doc-typ = 'E'.
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

* fields
  LOOP AT fields ASSIGNING <f>.
    CLEAR s_field.      s_field = <f>.      s_field-sqltab = s_key-table.   s_field-indexname = s_key-index.
    INSERT s_field INTO TABLE fm_data-fields.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
