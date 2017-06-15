class ZAPLINK_ENSC_DATA definition
  public
  inheriting from ZAPLINK_ENHANCEMENT_DATA
  create protected

  global friends ZAPLINK_EASYXML
                 ZAPLINK_ENHANCEMENT .

public section.
  type-pools SEEX .

  types:
    BEGIN OF ts_header.
    INCLUDE TYPE enhspotcomphead AS main.
    TYPES:
        texts TYPE tt_ltexts,
      END OF ts_header .
  types:
    BEGIN OF ts_comp_enh,
          name        TYPE enhspotcompositename,
          zl_object   TYPE to_raw,
        END OF ts_comp_enh .
  types:
    tt_comp_enh TYPE STANDARD TABLE OF ts_comp_enh WITH DEFAULT KEY .
  types:
    BEGIN OF ts_enh,
          name        TYPE enhspotname,
          zl_object   TYPE to_raw,
        END OF ts_enh .
  types:
    tt_enh TYPE STANDARD TABLE OF ts_enh WITH DEFAULT KEY .
  types:
    BEGIN OF ts_fm_data,
          header          TYPE enhspotcomphead,
*          texts           TYPE ts_sotr,
          comp_enh        TYPE enhspotcompositename_it,
          enh             TYPE enhspotname_it,
*     documentation   TYPE tt_docs,
        END OF ts_fm_data .

  data A0_MAINDATA type TS_HEADER read-only .
  data ENHANCEMENTS type TT_ENH read-only .
  data COMPOSIT_ENHANCEMENTS type TT_COMP_ENH read-only .

  methods ZAPLINK_CNX_EXT_CLEANER_4DATA~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER_4DATA~UNANONYMIZE
    redefinition .
protected section.

  methods FROM_DATA
    importing
      !DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(DATA) type TS_FM_DATA .
private section.
ENDCLASS.



CLASS ZAPLINK_ENSC_DATA IMPLEMENTATION.


  method FROM_DATA.
  DATA s_enh    LIKE LINE OF enhancements.
  DATA s_comp   LIKE LINE OF composit_enhancements.
  DATA s_txt    TYPE ts_sotr.
  FIELD-SYMBOLS:
    <enh>   LIKE LINE OF data-enh,
    <comp>  LIKE LINE OF data-comp_enh.

  a0_maindata-main = data-header.
  s_txt-short = a0_maindata-shorttextid.
  s_txt-long = a0_maindata-docuid.
  a0_maindata-texts = sotr_2_texts( s_txt ).
  CLEAR: a0_maindata-shorttextid, a0_maindata-docuid.
  LOOP AT data-enh ASSIGNING <enh>.
    CLEAR s_enh.
    s_enh-name = <enh>.
    APPEND s_enh TO enhancements.
  ENDLOOP.
  LOOP AT data-comp_enh ASSIGNING <comp>.
    CLEAR s_comp.
    s_comp-name = <comp>.
    APPEND s_comp TO composit_enhancements.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_enh    LIKE LINE OF data-enh.
  DATA s_comp   LIKE LINE OF data-comp_enh.
  FIELD-SYMBOLS:
    <enh>   LIKE LINE OF enhancements,
    <comp>  LIKE LINE OF composit_enhancements.

  data-header = a0_maindata-main.
  LOOP AT enhancements ASSIGNING <enh>.
    CLEAR s_enh.
    s_enh = <enh>-name.
    APPEND s_enh TO data-enh.
  ENDLOOP.
  LOOP AT composit_enhancements ASSIGNING <comp>.
    CLEAR s_comp.
    s_comp = <comp>-name.
    APPEND s_comp TO data-comp_enh.
  ENDLOOP.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER_4DATA~ANONYMIZE.
  CLEAR a0_maindata-createdon.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER_4DATA~UNANONYMIZE.
  endmethod.
ENDCLASS.
