class ZAPLINK_VIEW_DATA definition
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
    tt_dd26v TYPE STANDARD TABLE OF dd26v WITH DEFAULT KEY .
  types:
    tt_dd27p TYPE STANDARD TABLE OF dd27p WITH DEFAULT KEY .
  types:
    tt_dd28j TYPE STANDARD TABLE OF dd28j WITH DEFAULT KEY .
  types:
    tt_dd28v TYPE STANDARD TABLE OF dd28v WITH DEFAULT KEY .
  types:
    tt_dd25t TYPE SORTED TABLE OF dd25t WITH UNIQUE KEY ddlanguage .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
            header        TYPE dd25v,
            tech          TYPE dd09v,
            texts         TYPE tt_dd25t,
            docs          TYPE tt_docs,
            tables        TYPE tt_dd26v,
*  types:       " If alias is used
*    BEGIN OF ts_table,
*      TABNAME     type DD26V-TABNAME,
*      TABPOS	    type DD26V-TABPOS,
*      FORTABNAME	type DD26V-FORTABNAME,
*      FORFIELD	  type DD26V-FORFIELD,
*      FORDIR	    type DD26V-FORDIR,
*    END OF ts_table.
            fields        TYPE tt_dd27p,
            joins         TYPE tt_dd28j,
            wheres        TYPE tt_dd28v,
          END OF ts_fm_data .
  types:
    tt_tables TYPE SORTED TABLE OF dd26v WITH UNIQUE KEY tabpos .
  types:
    tt_fields TYPE SORTED TABLE OF dd27p WITH UNIQUE KEY objpos .
  types:
    tt_joins TYPE STANDARD TABLE OF dd28j WITH DEFAULT KEY .
  types:
    tt_wheres TYPE SORTED TABLE OF dd28v WITH UNIQUE KEY position .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd25v AS hdr.
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
  data TABLES type TT_TABLES .
  data FIELDS type TT_FIELDS .
  data INNER_JOIN type TT_JOINS .
  data CONDITIONS type TT_WHERES .

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
      header TYPE td_doc_id VALUE 'VW',
    END OF doc_ids .
private section.
ENDCLASS.



CLASS ZAPLINK_VIEW_DATA IMPLEMENTATION.


  method ANONYMIZE.
  FIELD-SYMBOLS:
    <t> like LINE OF texts.

  clear: a0_maindata-AS4USER, a0_maindata-AS4DATE, a0_maindata-AS4TIME,
         a0_maindata-technical_settings-AS4USER, a0_maindata-technical_settings-AS4DATE, a0_maindata-technical_settings-AS4TIME.
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
  DATA s_table LIKE LINE OF tables.
  DATA s_field LIKE LINE OF fields.
  DATA s_join  LIKE LINE OF inner_join.
  DATA s_where LIKE LINE OF conditions.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t> LIKE LINE OF fm_data-texts.

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
              object = a0_maindata-viewname.
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

* Tables
  tables = fm_data-tables.
  MODIFY tables FROM s_table TRANSPORTING viewname WHERE NOT viewname IS INITIAL.

* fields
  fields = fm_data-fields.
  MODIFY fields FROM s_field TRANSPORTING viewname WHERE NOT viewname IS INITIAL.

* joins
  inner_join = fm_data-joins.
  MODIFY inner_join FROM s_join TRANSPORTING viewname WHERE NOT viewname IS INITIAL.

* where
  conditions = fm_data-wheres.
  MODIFY conditions FROM s_where TRANSPORTING condname WHERE NOT condname IS INITIAL.
  endmethod.


  method TO_DATA.
  DATA s_text  LIKE LINE OF fm_data-texts.
  DATA s_doc   LIKE LINE OF fm_data-docs.
  DATA s_dtxt  LIKE LINE OF s_doc-texts.
  DATA s_table LIKE LINE OF fm_data-tables.
  DATA s_field LIKE LINE OF fm_data-fields.
  DATA s_join  LIKE LINE OF fm_data-joins.
  DATA s_where LIKE LINE OF fm_data-wheres.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-docs,
    <t> LIKE LINE OF texts.

  fm_data-header = a0_maindata-hdr.
  fm_data-tech = a0_maindata-technical_settings.
  fm_data-tech-tabname = a0_maindata-viewname.
  fm_data-tech-as4local = a0_maindata-as4local.
  fm_data-tech-as4vers = a0_maindata-as4vers.

* Texts
  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-viewname = fm_data-header-viewname.
    s_text-ddlanguage = <t>-langu.
    s_text-as4local = 'A'. " Active
    s_text-as4vers = '0000'. " Active
    s_text-ddtext = <t>-short_txt.
    INSERT s_text INTO TABLE fm_data-texts.
    IF NOT <t>-long_txt IS INITIAL.
      IF fm_data-docs IS INITIAL.
        s_doc-application = 'DOKU'.
        s_doc-id = doc_ids-header.
        s_doc-object = fm_data-header-viewname.
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

* Tables
  fm_data-tables = tables.        s_table-viewname = a0_maindata-viewname.
  MODIFY fm_data-tables FROM s_table TRANSPORTING viewname WHERE viewname <> s_table-viewname.

* fields
  fm_data-fields = fields.        s_field-viewname = a0_maindata-viewname.
  MODIFY fm_data-fields FROM s_field TRANSPORTING viewname WHERE viewname <> s_field-viewname.

* joins
  fm_data-joins = inner_join.     s_join-viewname = a0_maindata-viewname.
  MODIFY fm_data-joins FROM s_join TRANSPORTING viewname WHERE viewname <> s_join-viewname.

* where
  fm_data-wheres = conditions.    s_where-condname = a0_maindata-viewname.
  MODIFY fm_data-wheres FROM s_where TRANSPORTING condname WHERE condname <> s_where-condname.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
