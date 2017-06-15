*&---------------------------------------------------------------------*
*& Subroutine Pool   ZAPLINK_VARI
*&---------------------------------------------------------------------*
PROGRAM  zaplink_vari.

TYPE-POOLS: abap.
* Because might be include for standalone versions type and data name must be more unique
TYPES to_vari_object TYPE REF TO zaplink_program.
*TYPES to_vari_cx TYPE REF TO zaplink_cx.
DATA o_vari_prog TYPE to_vari_object.
*DATA o_vari_cx   TYPE to_vari_cx.
*DATA s_vari_text TYPE string.

*DEFINE mac_raise.
*  s_text = &1.
*  create object o_vari_cx
*    exporting
*      textid = zaplink_cx=>system_error
**        previous =
**        messages =
**        object =
**        subobject =
**        prev_cx =
**        ext_id =
**        _cx_list =
*      cx_name = s_vari_text.
*  raise exception o_vari_cx.
*END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  check_vari_form_exists
*&---------------------------------------------------------------------*
FORM check_vari_form_exists  CHANGING ok TYPE abap_bool.
  ok = abap_true.
ENDFORM.                    "check_vari_form_exists
*&---------------------------------------------------------------------*
*&      Form  set_object
*&---------------------------------------------------------------------*
FORM set_object USING object TYPE to_vari_object
*              RAISING zaplink_cx
  .
*  ASSERT ID ZAPLINK CONDITION o_prog IS BOUND.
  CHECK o_vari_prog IS NOT BOUND.
*  IF o_prog IS BOUND.
*    mac_raise text-sko.
*  ENDIF.
  o_vari_prog = object.
ENDFORM.                    "set_object

*&---------------------------------------------------------------------*
*&      Form  clear_object
*&---------------------------------------------------------------------*
FORM clear_object USING object TYPE to_vari_object
*                RAISING zaplink_cx
  .
*  ASSERT ID ZAPLINK CONDITION o_VARI_prog <> object.
  CHECK o_vari_prog = object.
*  IF o_prog <> object.
*    mac_raise text-cko.
*  ENDIF.
  CLEAR o_vari_prog.
ENDFORM.                    "clear_object
*&---------------------------------------------------------------------*
*&      Form  get_val
*&---------------------------------------------------------------------*
FORM get_val_p USING p_param value(p_text) p_subrc
*             RAISING zaplink_cx
  .
  DATA l_value TYPE string.
*  ASSERT ID ZAPLINK CONDITION NOT o_prog IS BOUND.
  CHECK o_vari_prog IS BOUND.
*  IF NOT o_prog IS BOUND.
*    mac_raise text-nse.
*  ENDIF.
  CHECK NOT p_param IS INITIAL.
  l_value = p_param.
  o_vari_prog->get_param_value( l_value ).
ENDFORM.                    "get_val
*&---------------------------------------------------------------------*
*&      Form  get_val_S
*&---------------------------------------------------------------------*
FORM get_val_s TABLES p_selopt                              "#EC *
                USING p_sign p_option p_low p_high
                      p_desc STRUCTURE rsselint p_subrc
*              RAISING zaplink_cx
  .
*  ASSERT ID ZAPLINK CONDITION NOT o_prog IS BOUND.
  CHECK o_vari_prog IS BOUND.
*  IF NOT o_prog IS BOUND.
*    mac_raise text-nse.
*  ENDIF.
  o_vari_prog->get_selectoption_value( p_selopt[] ).

ENDFORM.                    "get_val_s
*&---------------------------------------------------------------------*
*&      Form  set_val_p
*&---------------------------------------------------------------------*
FORM set_val_p USING p_param value(p_text) p_subrc
*             RAISING zaplink_cx
  .
  DATA l_value TYPE string.
*  ASSERT ID ZAPLINK CONDITION NOT o_prog IS BOUND.
  CHECK o_vari_prog IS BOUND.
*  IF NOT o_prog IS BOUND.
*    mac_raise text-nse.
*  ENDIF.
  l_value = o_vari_prog->set_param_value( ).
*  CHECK NOT l_value IS INITIAL.    " always set value : Clearing (blank in variant) default not null value
  p_param = l_value.
ENDFORM.                    "set_val_p
*&---------------------------------------------------------------------*
*&      Form  set_val_s
*&---------------------------------------------------------------------*
FORM set_val_s TABLES p_selopt                              "#EC *
                USING p_sign p_option p_low p_high
                      p_desc STRUCTURE rsselint p_subrc
*              RAISING zaplink_cx
  .
  DATA t_value  TYPE zaplink_program=>tr_so_value.
  FIELD-SYMBOLS:
    <v> LIKE LINE OF t_value.

*  ASSERT ID ZAPLINK CONDITION NOT o_prog IS BOUND.
  CHECK o_vari_prog IS BOUND.
*  IF NOT o_prog IS BOUND.
*    mac_raise text-nse.
*  ENDIF.
  t_value = o_vari_prog->set_selectoption_value( ).
  refresh p_selopt.       " always set value : Clearing (blank in variant) default not null value
  LOOP AT t_value ASSIGNING <v>.
    CLEAR p_selopt.   MOVE-CORRESPONDING <v> TO p_selopt.   APPEND p_selopt.
  ENDLOOP.

ENDFORM.                    "set_val_s
