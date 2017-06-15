class ZAPLINK_CHECKPOINTS_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_CHECKPOINTS
                 ZAPLINK_EASYXML .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_CHECKBOX
    for ZAPLINK_DATATYPES~TD_CHECKBOX .
  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_DYNPROS
    for ZAPLINK_DATATYPES~TT_DYNPROS .
  aliases TT_LTEXTS
    for ZAPLINK_DATATYPES~TT_LTEXTS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
protected section.

  types TD_MODE type AAB_ID_MODE .
  types:
    BEGIN OF ts_bp_act,
      inactive    TYPE td_checkbox,
      break       TYPE td_checkbox,
    END OF ts_bp_act .
  types:
    BEGIN OF ts_lp_act,
      inactive    TYPE td_checkbox,
      log         TYPE td_checkbox,
    END OF ts_lp_act .
  types:
    BEGIN OF ts_a_act,
      inactive    TYPE td_checkbox,
      log         TYPE td_checkbox,
      abort       TYPE td_checkbox,
      BEGIN OF break,
        log         TYPE td_checkbox,
        abort       TYPE td_checkbox,
      END OF break,
    END OF ts_a_act .
  types:
    BEGIN OF ts_activation,
      breakpoints TYPE ts_bp_act,
      logpoints   TYPE ts_lp_act,
      assertions  TYPE ts_a_act,
    END OF ts_activation .

  class-methods CONV_MODE2ACTIVATION
    importing
      !MODE type TD_MODE
    returning
      value(RESULT) type TS_ACTIVATION .
  class-methods CONV_ACTIVATION2MODE
    importing
      !ACTIVATION type TS_ACTIVATION
    returning
      value(RESULT) type TD_MODE .
private section.
ENDCLASS.



CLASS ZAPLINK_CHECKPOINTS_DATA IMPLEMENTATION.


  method CONV_ACTIVATION2MODE.
* From LSAABP01 : Methode pai_0310.

* convert activation mode
  CALL METHOD cl_aab_tool=>convert_input_to_mode
    EXPORTING
      bp_inactive            = activation-breakpoints-inactive
      bp_stop                = activation-breakpoints-break
      lp_inactive            = activation-logpoints-inactive
      lp_log                 = activation-logpoints-log
      as_fg_inactive         = activation-assertions-inactive
      as_fg_stop_bg_protocol = activation-assertions-break-log
      as_fg_stop_bg_rabax    = activation-assertions-break-abort
      as_fg_protocol         = activation-assertions-log
      as_fg_rabax            = activation-assertions-abort
    IMPORTING
      mode                   = result
    EXCEPTIONS
      mode_not_valid         = 1
      OTHERS                 = 2.
  ASSERT sy-subrc = 0.
  endmethod.


  method CONV_MODE2ACTIVATION.
* From LSAABP01 : Methode pbo_0310.

* convert activation mode
  CALL METHOD cl_aab_tool=>convert_mode_to_output
    EXPORTING
      mode                   = mode
    IMPORTING
      bp_inactive            = result-breakpoints-inactive
      bp_stop                = result-breakpoints-break
      lp_inactive            = result-logpoints-inactive
      lp_log                 = result-logpoints-log
      as_fg_inactive         = result-assertions-inactive
      as_fg_stop_bg_protocol = result-assertions-break-log
      as_fg_stop_bg_rabax    = result-assertions-break-abort
      as_fg_protocol         = result-assertions-log
      as_fg_rabax            = result-assertions-abort.
*      as_bg_text             = s_fmd_acid-header-as_bg_act_txt.
*  IF NOT result-assertions-break-log IS INITIAL
*    OR NOT result-assertions-break-abort IS INITIAL.
*    CALL METHOD cl_aab_tool=>convert_flags_comb_to_single
*      EXPORTING
*        as_fg_stop_bg_protocol = result-assertions-break-log
*        as_fg_stop_bg_rabax    = result-assertions-break-abort
*      IMPORTING
*        as_fg_stop             = s_fmd_acid-header-as_fg_stop
*        as_bg_protocol         = s_fmd_acid-header-as_bg_protocol
*        as_bg_rabax            = s_fmd_acid-header-as_bg_rabax.
*  ENDIF.
  endmethod.
ENDCLASS.
