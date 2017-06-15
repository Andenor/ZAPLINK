class ZAPLINK_ENHANCEMENT_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  abstract
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_ENHANCEMENT .

public section.
  type-pools SEEX .

  interfaces ZAPLINK_CNX_EXT_CLEANER_4DATA .
  interfaces ZAPLINK_DATATYPES .

  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_CHECKBOX
    for ZAPLINK_DATATYPES~TD_CHECKBOX .
  aliases TS_SOURCEASSTRUC
    for ZAPLINK_DATATYPES~TS_SOURCEASSTRUC .
  aliases TT_LTEXTS
    for ZAPLINK_DATATYPES~TT_LTEXTS .

  data TEXTS_2_DEL type TT_LTEXTS read-only .
protected section.

  types TO_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .
  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .
  types TO_MSG_COLL type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TO_RAW type ref to ZAPLINK_RAW .
  types TD_SOTR_CONCEPT type SOTR_TEXT-CONCEPT .
  types:
    BEGIN OF ts_sotr,
      short TYPE td_SOTR_CONCEPT,
      long  TYPE td_SOTR_CONCEPT,
    END OF ts_sotr .

  class-data O_MYCX type TO_EXCEPTION .

  methods SOTR_2_TEXTS
    importing
      !DATA type TS_SOTR
    returning
      value(RESULTS) type TT_LTEXTS
    raising
      ZAPLINK_CX_CONNECTOR .
  methods TEXTS_2_SOTR
    importing
      !SOTR type TS_SOTR
      !TEXTS type TT_LTEXTS
    raising
      ZAPLINK_CX_CONNECTOR .
private section.
ENDCLASS.



CLASS ZAPLINK_ENHANCEMENT_DATA IMPLEMENTATION.


  method SOTR_2_TEXTS.
  TYPES:
    BEGIN OF ts_lang,
      lang       TYPE sotr_text-langu,
    END OF ts_lang.
  DATA tt_lang    TYPE STANDARD TABLE OF ts_lang WITH DEFAULT KEY.
  DATA key        TYPE sotr_key.
  DATA s_text     LIKE LINE OF results.
  DATA sotr_text  TYPE sotr_text.
  DATA btfr_str   TYPE btfr_string.
  FIELD-SYMBOLS <l> LIKE LINE OF tt_lang.

  SELECT langu INTO TABLE tt_lang
    FROM sotr_text
    WHERE concept = data-short
      AND object = space.

  SELECT langu APPENDING TABLE tt_lang
    FROM sotr_textu
    WHERE concept = data-long
      AND object = space.

  SORT tt_lang.     DELETE ADJACENT DUPLICATES FROM tt_lang.

  LOOP AT tt_lang ASSIGNING <l>.
    CLEAR s_text.
* Short
    IF data-short IS NOT INITIAL.
      CLEAR: key, sotr_text.
      key-concept = data-short.
      CALL FUNCTION 'SOTR_READ_TEXT_WITH_KEY'
        EXPORTING
          langu                  = <l>-lang
*          CONTEXT                =
          sotr_key               = key
*          FLAG_CONTEXT           =
        IMPORTING
*       HEADER                 =
          entry                  = sotr_text
        EXCEPTIONS
          no_entry_found         = 1
          language_missing       = 0        " Could exist only for long for this language
          OTHERS                 = 3.
      IF sy-subrc <> 0.
        mac_raise_mf 'SOTR_READ_TEXT_WITH_KEY' sy-subrc.
      ELSE.
        s_text-text = sotr_text-text.
      ENDIF.
    ENDIF.

* Long
    IF data-long IS NOT INITIAL.
      CLEAR: btfr_str.
      CALL FUNCTION 'BTFR_SELECT_SINGLE'
        EXPORTING
          concept                   = data-long
          langu                     = <l>-lang
          flag_string               = abap_true
          flag_system_context       = abap_false
*         CONTEXT                   =
        IMPORTING
*         TEXT_WA                   =
          string_wa                 = btfr_str
*         HEADER                    =
        EXCEPTIONS
          concept_not_found         = 1
          text_not_found            = 0     " Could exist only for long for this language
          OTHERS                    = 3.
      IF sy-subrc <> 0.
        mac_raise_mf 'BTFR_SELECT_SINGLE' sy-subrc.
      ELSE.
        s_text-_ = btfr_str-text.
      ENDIF.
    ENDIF.

    IF s_text IS NOT INITIAL.
      s_text-lang = <l>.
      INSERT s_text INTO TABLE results.
    ENDIF.

  ENDLOOP.
  endmethod.


  method TEXTS_2_SOTR.
  DATA _txt     TYPE string.
  FIELD-SYMBOLS: <t> LIKE LINE OF texts.

  LOOP AT texts ASSIGNING <t>.
    IF <t>-text IS NOT INITIAL.
      _txt = <t>-text.
      CALL FUNCTION 'BTFR_MAINTAIN_SINGLE_TEXT'
        EXPORTING
*         PACKAGE                            =
*         OBJECT_TYPE                        =
          langu                              = <t>-lang
          concept                            = sotr-short
*         ALIAS                              =
*         TRANSL_TYPE                        =
*         FLAG_STRING                        =
*         OPTIONS_EDITOR                     =
*         OPTIONS_CHECK                      =
*         FLAG_NO_CONTEXT                    =
*         FLAG_DISPLAY                       =
          flag_no_screen                     = abap_true
*         FLAG_CALLED_FROM_EDIT_SCREEN       =
          default_text                       = _txt
          flag_correction_entry              = abap_false
*         CORR_NUM                           =
*         USE_KORRNUM_IMMEDIATEDLY           =
*       IMPORTING
*         FLAG_CANCELLED                     =
*         HEADER                             =
*         TEXT                               =
*         STRING                             =
        EXCEPTIONS
          parameter_error                    = 1
          text_not_found                     = 2
          invalid_object_type                = 3
          invalid_package                    = 4
          header_diff_interface              = 5
          invalid_tadir_entry                = 6
          invalid_transl_type                = 7
          invalid_concept                    = 8
          OTHERS                             = 9.
      IF sy-subrc <> 0.
        mac_raise_mf 'BTFR_MAINTAIN_SINGLE_TEXT' sy-subrc.
      ENDIF.
    ENDIF.
    IF <t>-_ IS NOT INITIAL.
      _txt = <t>-_.
      CALL FUNCTION 'BTFR_MAINTAIN_SINGLE_TEXT'
        EXPORTING
*         PACKAGE                            =
*         OBJECT_TYPE                        =
          langu                              = <t>-lang
          concept                            = sotr-long
*         ALIAS                              =
*         TRANSL_TYPE                        =
*         FLAG_STRING                        =
*         OPTIONS_EDITOR                     =
*         OPTIONS_CHECK                      =
*         FLAG_NO_CONTEXT                    =
*         FLAG_DISPLAY                       =
          flag_no_screen                     = abap_true
*         FLAG_CALLED_FROM_EDIT_SCREEN       =
          default_text                       = _txt
          flag_correction_entry              = abap_false
*         CORR_NUM                           =
*         USE_KORRNUM_IMMEDIATEDLY           =
*       IMPORTING
*         FLAG_CANCELLED                     =
*         HEADER                             =
*         TEXT                               =
*         STRING                             =
        EXCEPTIONS
          parameter_error                    = 1
          text_not_found                     = 2
          invalid_object_type                = 3
          invalid_package                    = 4
          header_diff_interface              = 5
          invalid_tadir_entry                = 6
          invalid_transl_type                = 7
          invalid_concept                    = 8
          OTHERS                             = 9.
      IF sy-subrc <> 0.
        mac_raise_mf 'BTFR_MAINTAIN_SINGLE_TEXT' sy-subrc.
      ENDIF.
    ENDIF.
  ENDLOOP.
  endmethod.
ENDCLASS.
