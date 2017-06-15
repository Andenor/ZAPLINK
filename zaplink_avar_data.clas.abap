class ZAPLINK_AVAR_DATA definition
  public
  inheriting from ZAPLINK_CHECKPOINTS_DATA
  create public

  global friends ZAPLINK_CHECKPOINTS
                 ZAPLINK_EASYXML .

public section.

  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
protected section.

  types:
    BEGIN OF ts_name,
      name    TYPE aab_var_name,
      user    TYPE aab_var_local,
    END OF ts_name .
  types:
    BEGIN OF ts_fm_data,
      header          TYPE aab_var_prop,     " aab_var_sfields,
      descriptions    TYPE STANDARD TABLE OF aab_var_propt WITH DEFAULT KEY,
      modes           TYPE aab_var_obj_act_tab,
    END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
  INCLUDE TYPE aab_var_sfields.
  TYPES:
    END OF ts_maindata .
  types:
    BEGIN OF ts_definition,
      type    TYPE td_comptype,
      name    type td_compname.
  INCLUDE TYPE ts_activation AS activation.
  TYPES:
    END OF ts_definition .
  types:
    tt_definitions TYPE SORTED TABLE OF ts_definition WITH UNIQUE KEY type name .

  data A0_MAINDATA type TS_MAINDATA .
  data DEFINITIONS type TT_DEFINITIONS .
  data DESCRIPTIONS type TT_LTEXTS .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
  class-methods NAME_2_KEY
    importing
      !DATA type TD_COMPNAME
    returning
      value(RESULT) type TS_NAME .
  class-methods KEY_2_NAME
    importing
      !DATA type TS_NAME
    returning
      value(RESULT) type TD_COMPNAME .
private section.

  constants SEPARATOR type CHAR01 value '~' ##NO_TEXT.
ENDCLASS.



CLASS ZAPLINK_AVAR_DATA IMPLEMENTATION.


  method ANONYMIZE.
  CLEAR: a0_maindata-create_user,  a0_maindata-create_date.
*ACT_USER
*ACT_DATE
*ACT_TIME
  endmethod.


  method FROM_DATA.
  DATA s_def      LIKE LINE OF definitions.
  DATA s_desc     LIKE LINE OF descriptions.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-descriptions,
    <m> LIKE LINE OF fm_data-modes.

*  a0_maindata = fm_data-header.
*  CLEAR: a0_maindata-devclass_txt, a0_maindata-masterlang_txt, a0_maindata-component_txt.
  LOOP AT fm_data-descriptions ASSIGNING <d>.
    CLEAR s_desc.   s_desc-lang = <d>-langu.     s_desc-text = <d>-descript.   INSERT s_desc INTO TABLE descriptions.
  ENDLOOP.

  LOOP AT fm_data-modes ASSIGNING <m>.
    CLEAR s_def.    s_def-type = <m>-object.    s_def-name = <m>-name.    s_def-activation = conv_mode2activation( <m>-actmode ).    INSERT s_def INTO TABLE definitions.
  ENDLOOP.
  endmethod.


  method KEY_2_NAME.
  IF data-user IS INITIAL.
    result = data-name.
  ELSE.
    CONCATENATE data-name separator data-user INTO result.
  ENDIF.
  endmethod.


  method NAME_2_KEY.
  SPLIT data AT separator INTO result-name result-user.
  endmethod.


  method TO_DATA.
  DATA s_mode     LIKE LINE OF fm_data-modes.
  DATA s_desc     LIKE LINE OF fm_data-descriptions.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF descriptions,
    <s> LIKE LINE OF definitions.

  LOOP AT descriptions ASSIGNING <d>.
    CLEAR s_desc.   s_desc-langu = <d>-lang.     s_desc-descript = <d>-text.   INSERT s_desc INTO TABLE fm_data-descriptions.
  ENDLOOP.

  LOOP AT definitions ASSIGNING <s>.
    CLEAR s_mode.     s_mode-object = <s>-type.   s_mode-name = <s>-name.   s_mode-actmode = conv_activation2mode( <s>-activation ).    INSERT s_mode INTO TABLE fm_data-modes.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
