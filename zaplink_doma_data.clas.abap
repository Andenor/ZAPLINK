class ZAPLINK_DOMA_DATA definition
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
    tt_dd07v TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY .
  types:
    tt_dd07t TYPE SORTED TABLE OF dd07t WITH UNIQUE KEY valpos ddlanguage .
  types:
    tt_dd01t TYPE SORTED TABLE OF dd01t WITH UNIQUE KEY ddlanguage .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
                header        TYPE dd01v,
                texts         TYPE tt_dd01t,
                docs          TYPE tt_docs,
                values        TYPE tt_dd07v,
                values_texts  TYPE tt_dd07t,
              END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE dd01v AS hdr.
    TYPES:
      END OF ts_maindata .
  types:
    BEGIN OF ts_value.
    INCLUDE TYPE dd07v AS main.
    TYPES:
        texts TYPE tt_texts,
      END OF ts_value .
  types:
    tt_values TYPE SORTED TABLE OF ts_value WITH UNIQUE KEY valpos .

  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .
  data VALUES type TT_VALUES .

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
      header TYPE td_doc_id VALUE 'DO',
    END OF doc_ids .
private section.
ENDCLASS.



CLASS ZAPLINK_DOMA_DATA IMPLEMENTATION.


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
  DATA s_value LIKE LINE OF values.
  DATA s_vtext LIKE LINE OF s_value-texts.
  DATA s_text  LIKE LINE OF texts.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <vt> LIKE LINE OF fm_data-values_texts,
    <t> LIKE LINE OF fm_data-texts,
    <v> LIKE LINE OF fm_data-values.

  a0_maindata-hdr = fm_data-header.

* Values
  LOOP AT fm_data-values ASSIGNING <v>.
    CLEAR s_value. s_value-main = <v>. CLEAR s_value-domname.
    LOOP AT fm_data-values_texts ASSIGNING <vt>
      WHERE valpos = <v>-valpos.
      CLEAR s_vtext.
      s_vtext-langu = <vt>-ddlanguage.
*      IF <v>-DOMVALUE_H is INITIAL.
      s_vtext-short_txt = <vt>-ddtext.
*      else.
*      endif.
      INSERT s_vtext INTO TABLE s_value-texts.
    ENDLOOP.
    INSERT s_value INTO TABLE values.
  ENDLOOP.

* Texts
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-langu = <t>-ddlanguage.
    s_text-short_txt = <t>-ddtext.
    READ TABLE fm_data-docs ASSIGNING <d>
         WITH KEY id = doc_ids-header
              object = a0_maindata-domname.
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
  DATA s_value LIKE LINE OF fm_data-values.
  DATA s_vtext LIKE LINE OF fm_data-values_texts.
  DATA s_text  LIKE LINE OF fm_data-texts.
  DATA s_doc   LIKE LINE OF fm_data-docs.
  DATA s_dtxt  LIKE LINE OF s_doc-texts.
  FIELD-SYMBOLS:
    <v> LIKE LINE OF values,
    <t> LIKE LINE OF texts,
    <d> LIKE LINE OF fm_data-docs,
    <vt> LIKE LINE OF <v>-texts.

  fm_data-header = a0_maindata-hdr.

* Values
  LOOP AT values ASSIGNING <v>.
    CLEAR s_value. s_value = <v>-main. s_value-domname = fm_data-header-domname.
    INSERT s_value INTO TABLE fm_data-values.
    LOOP AT <v>-texts ASSIGNING <vt>.
      CLEAR s_vtext.
      MOVE-CORRESPONDING <v> TO s_vtext.
      s_vtext-domname = fm_data-header-domname.
      s_vtext-ddlanguage = <vt>-langu.
      s_vtext-as4local = 'A'. " Active
      s_vtext-valpos = <v>-valpos.
      s_vtext-as4vers = '0000'. " Active
      s_vtext-ddtext = <vt>-short_txt.
      INSERT s_vtext INTO TABLE fm_data-values_texts.
    ENDLOOP.
  ENDLOOP.

* Texts
  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-domname = fm_data-header-domname.
    s_text-ddlanguage = <t>-langu.
    s_text-as4local = 'A'. " Active
    s_text-as4vers = '0000'. " Active
    s_text-ddtext = <t>-short_txt.
    INSERT s_text INTO TABLE fm_data-texts.
    IF NOT <t>-long_txt IS INITIAL.
      IF fm_data-docs IS INITIAL.
        CLEAR s_doc.
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
        s_doc-object = fm_data-header-domname.
        s_doc-langu = <t>-langu.
        s_doc-masterlang = abap_true.
        s_doc-typ = 'T'.
        s_doc-dokform = 'S_DOCU_SHOW'.
        s_doc-dokstyle = 'S_DOCUS1'.
        INSERT s_doc INTO TABLE fm_data-docs.
      ENDIF.
      s_dtxt = <t>-long_txt.
      s_dtxt-tdspras = <t>-langu.
      INSERT s_dtxt INTO TABLE s_doc-texts.
    ENDIF.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
