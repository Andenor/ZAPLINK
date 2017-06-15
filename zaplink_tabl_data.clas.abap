class ZAPLINK_TABL_DATA definition
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
    tt_dd03p TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY .
  types:
    tt_dd05m TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY .
  types:
    tt_dd08v TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY .
  types:
    tt_dd12v TYPE STANDARD TABLE OF dd12v WITH DEFAULT KEY .
  types:
    tt_dd12t TYPE STANDARD TABLE OF dd12t WITH DEFAULT KEY .
  types:
    tt_dd17v TYPE STANDARD TABLE OF dd17v WITH DEFAULT KEY .
  types:
    tt_dd35v TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY .
  types:
    tt_dd36m TYPE STANDARD TABLE OF dd36m WITH DEFAULT KEY .
  types:
    tt_dd02t TYPE SORTED TABLE OF dd02t WITH UNIQUE KEY ddlanguage .
  types:
    tt_dd03t TYPE SORTED TABLE OF dd03t WITH UNIQUE KEY fieldname ddlanguage .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
                header        TYPE dd02v,
                tech          TYPE dd09v,
                texts         TYPE tt_dd02t,
                docs          TYPE tt_docs,
                fields        TYPE tt_dd03p,
                fields_texts  TYPE tt_dd03t,
                forein_keys   TYPE tt_dd08v,      " Header
                fk_fields     TYPE tt_dd05m,
                indexes       TYPE tt_dd12v,
                idx_texts     TYPE tt_dd12t,
                idx_fields    TYPE tt_dd17v,
                search_helps  TYPE tt_dd35v,
                sh_fields     TYPE tt_dd36m,
                with_content  TYPE abap_bool,     " With table content
              END OF ts_fm_data .
  types:
    BEGIN OF ts_search_help.
    INCLUDE TYPE dd35v AS hdr.
    TYPES:
        fields TYPE SORTED TABLE OF dd36m WITH UNIQUE KEY shlpfield,
      END OF ts_search_help .
  types:
    BEGIN OF ts_forein_key.
    INCLUDE TYPE dd08v AS hdr.
    TYPES:
        fields TYPE SORTED TABLE OF dd05m WITH UNIQUE KEY primpos,
      END OF ts_forein_key .
  types:
    BEGIN OF ts_field.
    INCLUDE TYPE dd03p AS hdr.
    TYPES:
        texts       TYPE tt_texts,
        search_help TYPE ts_search_help,
        forein_key  TYPE ts_forein_key,
      END OF ts_field .
  types:
    tt_fields TYPE SORTED TABLE OF ts_field WITH UNIQUE KEY position .
  types TO_INDEX type ref to ZAPLINK_XINX_RAW .
  types:
    BEGIN OF ts_index,
        zl_object TYPE to_index ,
        END OF ts_index .
  types:
    tt_indexes TYPE STANDARD TABLE OF ts_index WITH DEFAULT KEY .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd02v AS hdr.
*    INCLUDE TYPE dd09v AS tech.
*    types:
*      BEGIN OF ts_dd09v_light,
** required fields of dd09V
*        TABKAT TABKAT  CHAR  2 0 Size category
*        TABART TABART  CHAR  5 0 Data class in technical settings
*        PUFFERUNG  PUFFERUNG CHAR  1 0 Buffering type indicator
*        SCHFELDANZ SCHFELDANZ  NUMC  3 0 No. of key fields for generic buffers
*        PROTOKOLL  DDPROTOCOL  CHAR  1 0 Log data changes
*        SPEICHPUFF SPEICHPUFF  CHAR  1 0 Flag for the storage type in the buffer
*        AS4USER  AS4USER CHAR  12  0 Last Changed by
*        AS4DATE  AS4DATE DATS  8 0 Date of Last Change
*        AS4TIME  AS4TIME TIMS  6 0 Last changed at
*        TRANSPFLAG DDTRANSP  CHAR  1 0 Convert to transparent table or maintain transparency
*        RESERVE  DDRESERVE CHAR  1 0 Reserved field in the ABAP Dictionary
*        UEBERSETZ  UEBERSETZ CHAR  1 0 Flag whether table is relevant for translation
*        ACTFLAG  ACTFLAG CHAR  1 0 Activation flag
*        BUFALLOW BUFALLOW  CHAR  1 0 Indicator for buffering allowed/not allowed
*        JAVAONLY DDJAVAONLY  CHAR  1 0 Write access only with JAVA
*      END OF ts_dd09v_light .
*    INCLUDE TYPE ts_dd09v_light AS tech.
    TYPES:
        technical_settings TYPE dd09v,
      END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .
  data FIELDS type TT_FIELDS .
  data INDEXES type TT_INDEXES .
  data CONTENT type ref to DATA .

  class-methods CLASS_CONSTRUCTOR .
  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA
    raising
      ZAPLINK_CX .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE
    raising
      ZAPLINK_CX .
  methods UNANONYMIZE .
protected section.

  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .

  types:
    tt_fm_data TYPE STANDARD TABLE OF zaplink_xinx_data=>ts_fm_data WITH DEFAULT KEY .

  class-data R_DOC_IDS type TR_DOCID .
  constants:
    BEGIN OF doc_ids,
      header TYPE td_doc_id VALUE 'TB',
    END OF doc_ids .

  class-methods CONV_INDEX
    importing
      !FM_DATA type TS_FM_DATA
    returning
      value(RESULT) type TT_FM_DATA .
  class-methods CONV_INDEXES
    importing
      !INDEXES type TT_FM_DATA
    changing
      !FM_DATA type TS_FM_DATA .
private section.
ENDCLASS.



CLASS ZAPLINK_TABL_DATA IMPLEMENTATION.


  method ANONYMIZE.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF indexes,
    <t> LIKE LINE OF texts.

  CLEAR: a0_maindata-as4user, a0_maindata-as4date, a0_maindata-as4time,
         a0_maindata-technical_settings-as4user, a0_maindata-technical_settings-as4date, a0_maindata-technical_settings-as4time.
  LOOP AT texts ASSIGNING <t>.    CLEAR: <t>-long_txt-tdfuser, <t>-long_txt-tdfdate, <t>-long_txt-tdftime.    ENDLOOP.
  LOOP AT indexes ASSIGNING <i>.    <i>-zl_object->anonymize( ).    ENDLOOP.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method CONV_INDEX.
  DATA s_index LIKE LINE OF result.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF fm_data-indexes.

  LOOP AT fm_data-indexes ASSIGNING <i>.
    CLEAR s_index.    s_index-header = <i>.
    s_index-fields = fm_data-idx_fields.    DELETE s_index-fields WHERE indexname <> <i>-indexname.
    s_index-texts = fm_data-idx_texts.    DELETE s_index-texts WHERE indexname <> <i>-indexname.
    s_index-docs = fm_data-docs.
    APPEND s_index TO result.
  ENDLOOP.
  endmethod.


  method CONV_INDEXES.
  DATA s_index LIKE LINE OF fm_data-indexes.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF indexes.

  LOOP AT indexes ASSIGNING <i>.
    CLEAR s_index.    s_index = <i>-header.       APPEND s_index TO fm_data-indexes.
    APPEND LINES OF <i>-fields TO fm_data-idx_fields.
    APPEND LINES OF <i>-texts  TO fm_data-idx_texts.
    APPEND LINES OF <i>-docs   TO fm_data-docs.
  ENDLOOP.
  endmethod.


  method FROM_DATA.
  DATA s_text  LIKE LINE OF texts.
  DATA s_field LIKE LINE OF fields.
  DATA s_sh_f  LIKE LINE OF s_field-search_help-fields.
  DATA s_fk_f  LIKE LINE OF s_field-forein_key-fields.
  DATA s_f_txt LIKE LINE OF s_field-texts.
  DATA s_index LIKE LINE OF indexes.
*  DATA s_idx_f LIKE LINE OF s_index-fields.
  DATA d_str   TYPE string.
  DATA t_index TYPE tt_fm_data.
  FIELD-SYMBOLS:
    <f> LIKE LINE OF fm_data-fields,
    <ft> LIKE LINE OF fm_data-fields_texts,
    <s> LIKE LINE OF fm_data-search_helps,
    <sf> LIKE LINE OF fm_data-sh_fields,
    <k> LIKE LINE OF fm_data-forein_keys,
    <ff> LIKE LINE OF fm_data-fk_fields,
    <id> LIKE LINE OF t_index,
    <i> LIKE LINE OF fm_data-indexes,
    <if> LIKE LINE OF fm_data-idx_fields,
    <d> LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t> LIKE LINE OF fm_data-texts.
  DATA p_table TYPE REF TO data.
  FIELD-SYMBOLS:
    <tc> TYPE STANDARD TABLE,
    <cl> TYPE ANY,
    <mandt> TYPE sy-mandt.

  a0_maindata-hdr = fm_data-header.
  a0_maindata-technical_settings = fm_data-tech.
  CLEAR: a0_maindata-technical_settings-tabname,
         a0_maindata-technical_settings-as4local,
         a0_maindata-technical_settings-as4vers.

* Texts
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-langu = <t>-ddlanguage.
    s_text-short_txt = <t>-ddtext.
    READ TABLE fm_data-docs ASSIGNING <d>
         WITH KEY id = doc_ids-header
              object = a0_maindata-tabname.
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
          WHERE tabname = a0_maindata-tabname.
    CLEAR s_field.      s_field-hdr = <f>.      CLEAR: s_field-tabname.

* Texts
    LOOP AT fm_data-fields_texts ASSIGNING <ft>
            WHERE tabname = <f>-tabname
            AND fieldname = <f>-fieldname.
      CLEAR s_f_txt.
      s_f_txt-langu = <ft>-ddlanguage.
      s_f_txt-short_txt = <ft>-ddtext.
      INSERT s_f_txt INTO TABLE s_field-texts.
    ENDLOOP.

* Search help
    LOOP AT fm_data-search_helps ASSIGNING <s>
            WHERE tabname = <f>-tabname
            AND fieldname = <f>-fieldname.
      IF s_field-search_help IS NOT INITIAL. sy-subrc = sy-tabix / 0. ENDIF.    " should not append
      s_field-search_help-hdr = <s>.      CLEAR: s_field-search_help-tabname, s_field-search_help-fieldname.
      LOOP AT fm_data-sh_fields ASSIGNING <sf>
              WHERE tabname = <s>-tabname
              AND fieldname = <s>-fieldname
               AND shlpname = <s>-shlpname.
        CLEAR s_sh_f.     s_sh_f = <sf>.    CLEAR: s_sh_f-tabname, s_sh_f-fieldname, s_sh_f-shlpname.
        IF s_sh_f-shtable = a0_maindata-tabname.    CLEAR: s_sh_f-shtable.    IF s_sh_f-shfield = s_field-fieldname.    CLEAR: s_sh_f-shfield. ENDIF. ENDIF.
        INSERT s_sh_f INTO TABLE s_field-search_help-fields.
      ENDLOOP.
    ENDLOOP.

* Foreign Keys
    LOOP AT fm_data-forein_keys ASSIGNING <k>
            WHERE tabname = <f>-tabname
            AND fieldname = <f>-fieldname.
      IF s_field-forein_key IS NOT INITIAL. sy-subrc = sy-tabix / 0. ENDIF.    " should not append
      s_field-forein_key-hdr = <k>.      CLEAR: s_field-forein_key-tabname, s_field-forein_key-fieldname, s_field-checktable.
      LOOP AT fm_data-fk_fields ASSIGNING <ff>
              WHERE tabname = <k>-tabname
              AND fieldname = <k>-fieldname
             AND checktable = <k>-checktable.
        CLEAR s_fk_f.     s_fk_f = <ff>.    CLEAR: s_fk_f-tabname, s_fk_f-fieldname, s_fk_f-checktable.
        IF s_fk_f-fortable = a0_maindata-tabname.    CLEAR: s_fk_f-fortable.    IF s_fk_f-forkey = s_field-fieldname.    CLEAR: s_fk_f-forkey. ENDIF. ENDIF.
        INSERT s_fk_f INTO TABLE s_field-forein_key-fields.
      ENDLOOP.
    ENDLOOP.

    INSERT s_field INTO TABLE fields.
  ENDLOOP.

* indexes
  t_index = conv_index( fm_data ).
  SORT t_index BY header-indexname.
  LOOP AT t_index ASSIGNING <id>.
    CLEAR s_index.
    CREATE OBJECT s_index-zl_object
      EXPORTING
        fm_data = <id>.
    APPEND s_index TO indexes.
  ENDLOOP.
*  LOOP AT fm_data-indexes ASSIGNING <i>
*          WHERE sqltab = a0_maindata-tabname.
*    CLEAR s_index.      s_index-hdr = <i>.      CLEAR: s_index-sqltab.
*    CONCATENATE <i>-sqltab '~' <i>-indexname INTO d_str.
*    IF d_str = s_index-dbindex. CLEAR s_index-dbindex. ENDIF.     " clear default naming convention
*
** fields
*    LOOP AT fm_data-idx_fields ASSIGNING <if>
*            WHERE sqltab = <i>-sqltab
*           AND indexname = <i>-indexname.
*      CLEAR s_idx_f.    s_idx_f = <if>.       CLEAR: s_idx_f-sqltab, s_idx_f-indexname.
*      INSERT s_idx_f INTO TABLE s_index-fields.
*    ENDLOOP.
*
*    INSERT s_index INTO TABLE indexes.
*  ENDLOOP.

* Table content
  IF fm_data-with_content = abap_true AND fm_data-header-tabclass <> 'INTTAB'.    " skip for structures
    TRY.
        CREATE DATA content TYPE STANDARD TABLE OF (a0_maindata-tabname).
        ASSIGN content->* TO <tc>.
        ASSERT sy-subrc = 0.
        SELECT *
          FROM (a0_maindata-tabname)
          INTO TABLE <tc>.
        IF NOT <tc> IS INITIAL.
* Remove mandant information if exists
          READ TABLE fm_data-fields ASSIGNING <f>
              WITH KEY tabname = a0_maindata-tabname
                       keyflag = abap_true
                      datatype = 'CLNT'.   " Mandant
          IF sy-subrc = 0.
            LOOP AT <tc> ASSIGNING <cl>.
              ASSIGN COMPONENT <f>-fieldname OF STRUCTURE <cl> TO <mandt>.
              ASSERT sy-subrc = 0.
              CLEAR <mandt>.
            ENDLOOP.
          ENDIF.
        ENDIF.
      CATCH cx_sy_create_data_error.
    ENDTRY.
  ENDIF.
  endmethod.


  method TO_DATA.
  DATA s_text  LIKE LINE OF fm_data-texts.
  DATA s_doc   LIKE LINE OF fm_data-docs.
  DATA s_dtxt  LIKE LINE OF s_doc-texts.
  DATA s_idx_f LIKE LINE OF fm_data-idx_fields.
  DATA s_field LIKE LINE OF fm_data-fields.
  DATA s_f_txt LIKE LINE OF fm_data-fields_texts.
  DATA s_sh    LIKE LINE OF fm_data-search_helps.
  DATA s_sh_f  LIKE LINE OF fm_data-sh_fields.
  DATA s_fk    LIKE LINE OF fm_data-forein_keys.
  DATA s_fk_f  LIKE LINE OF fm_data-fk_fields.
  DATA t_index TYPE tt_fm_data.
  DATA s_index LIKE LINE OF t_index.
  FIELD-SYMBOLS:
    <i>  LIKE LINE OF indexes,
*    <if> LIKE LINE OF <i>-fields,
    <f>  LIKE LINE OF fields,
    <ft> LIKE LINE OF <f>-texts,
    <sf> LIKE LINE OF <f>-search_help-fields,
    <ff> LIKE LINE OF <f>-forein_key-fields,
    <d>  LIKE LINE OF fm_data-docs,
    <t>  LIKE LINE OF texts.

  fm_data-header = a0_maindata-hdr.
  IF NOT a0_maindata-technical_settings IS INITIAL.         " Issue 84
    fm_data-tech = a0_maindata-technical_settings.
    fm_data-tech-tabname = fm_data-header-tabname.
    fm_data-tech-as4local = 'A'.
    fm_data-tech-as4vers = '0000'.
  ENDIF.

* Texts
  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-tabname = fm_data-header-tabname.
    s_text-ddlanguage = <t>-langu.
    s_text-as4local = 'A'. " Active
    s_text-as4vers = '0000'. " Active
    s_text-ddtext = <t>-short_txt.
    INSERT s_text INTO TABLE fm_data-texts.
    IF NOT <t>-long_txt IS INITIAL.
      IF fm_data-docs IS INITIAL.
        s_doc-application = 'DOKU'.
        s_doc-id = doc_ids-header.
        s_doc-object = fm_data-header-tabname.
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

* indexes
  LOOP AT indexes ASSIGNING <i>.
    s_index = <i>-zl_object->to_data( ).
    APPEND s_index TO t_index.
  ENDLOOP.
  conv_indexes( EXPORTING indexes = t_index
                 CHANGING fm_data = fm_data ).
*  LOOP AT indexes ASSIGNING <i>.
*    CLEAR s_index.      s_index = <i>-hdr.      s_index-sqltab = a0_maindata-tabname.
*    IF s_index-dbindex IS INITIAL. CONCATENATE s_index-sqltab '~' s_index-indexname INTO s_index-dbindex. ENDIF.     " retrieve default naming convention
*    INSERT s_index INTO TABLE fm_data-indexes.
*
** fields
*    LOOP AT <i>-fields ASSIGNING <if>.
*      CLEAR s_idx_f.    s_idx_f = <if>.       s_idx_f-sqltab = s_index-sqltab. s_idx_f-indexname = s_index-indexname.
*      INSERT s_idx_f INTO TABLE fm_data-idx_fields.
*    ENDLOOP.
*  ENDLOOP.

* fields
  LOOP AT fields ASSIGNING <f>.
    CLEAR s_field.      s_field = <f>-hdr.      s_field-tabname = a0_maindata-tabname.

* Texts
    LOOP AT <f>-texts ASSIGNING <ft>.
      CLEAR s_f_txt.    s_f_txt-ddlanguage = <ft>-langu.    s_f_txt-ddtext = <ft>-short_txt.    s_f_txt-tabname = s_field-tabname.    s_f_txt-fieldname = s_field-fieldname.
      INSERT s_f_txt INTO TABLE fm_data-fields_texts.
    ENDLOOP.

* Search help
    IF NOT <f>-search_help IS INITIAL.
      CLEAR s_sh.   s_sh = <f>-search_help-hdr.   s_sh-tabname = s_field-tabname.   s_sh-fieldname = s_field-fieldname.
      INSERT s_sh INTO TABLE fm_data-search_helps.
      LOOP AT <f>-search_help-fields ASSIGNING <sf>.
        CLEAR s_sh_f.     s_sh_f = <sf>.    s_sh_f-tabname = s_sh-tabname.    s_sh_f-fieldname = s_sh-fieldname.    s_sh_f-shlpname = s_sh-shlpname.
        IF s_sh_f-shtable IS INITIAL. s_sh_f-shtable = a0_maindata-tabname.    IF s_sh_f-shfield IS INITIAL. s_sh_f-shfield = <f>-fieldname.    ENDIF. ENDIF.
        INSERT s_sh_f INTO TABLE fm_data-sh_fields.
      ENDLOOP.
    ENDIF.

* Foreign Keys
    IF NOT <f>-forein_key IS INITIAL.
      CLEAR s_fk.   s_fk = <f>-forein_key-hdr.   s_fk-tabname = s_field-tabname.   s_fk-fieldname = s_field-fieldname.
      INSERT s_fk INTO TABLE fm_data-forein_keys.
      s_field-checktable = s_fk-checktable.
      LOOP AT <f>-forein_key-fields ASSIGNING <ff>.
        CLEAR s_fk_f.     s_fk_f = <ff>.    s_fk_f-tabname = s_fk-tabname.   s_fk_f-fieldname = s_fk-fieldname.   s_fk_f-checktable = s_fk-checktable.
        IF s_fk_f-fortable IS INITIAL.    s_fk_f-fortable = a0_maindata-tabname.    IF s_fk_f-forkey IS INITIAL.  s_fk_f-forkey = s_field-fieldname.   ENDIF. ENDIF.
        INSERT s_fk_f INTO TABLE fm_data-fk_fields.
      ENDLOOP.
    ENDIF.
    INSERT s_field INTO TABLE fm_data-fields.
  ENDLOOP.

* Content
  IF NOT content IS INITIAL.
    fm_data-with_content = abap_true.
  ENDIF.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
