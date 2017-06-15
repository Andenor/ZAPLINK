*&---------------------------------------------------------------------*
*& Module Pool       ZAPLINK_LIST
*&---------------------------------------------------------------------*

PROGRAM  zaplink_list.

TYPE-POOLS: slis.

TYPES to_list TYPE REF TO zaplink_list.
TYPES td_action TYPE rsmpe-func.

DATA o_list_u0zi0gu276j00cars51w  TYPE to_list.

CONSTANTS:
  c_act_cancel  TYPE td_action VALUE 'CANCEL',
  c_act_confirm TYPE td_action VALUE 'CONFIRM'.
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'SELECT_TOOLBAR' OF PROGRAM 'ZAPLINK_LIST' EXCLUDING rt_extab.
ENDFORM.                    "pf_status_set

*&---------------------------------------------------------------------*
*&      Form  pf_status_set_no_action
*&---------------------------------------------------------------------*
FORM pf_status_no_action USING rt_extab TYPE slis_t_extab.
  DATA lt_extab TYPE slis_t_extab.
  DATA s_fcode LIKE LINE OF rt_extab.

  lt_extab = rt_extab.
  s_fcode-fcode = c_act_confirm.     APPEND s_fcode TO lt_extab.
  s_fcode-fcode = c_act_cancel.      APPEND s_fcode TO lt_extab.
  PERFORM pf_status_set USING lt_extab.
ENDFORM.                    "pf_status_set

*&---------------------------------------------------------------------*
*&      Form  user_command_user
*&---------------------------------------------------------------------*
FORM user_command_user USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.

  rs_selfield-exit = 'X'.
  CASE r_ucomm.
    WHEN c_act_confirm.
      IF o_list_u0zi0gu276j00cars51w IS BOUND.   o_list_u0zi0gu276j00cars51w->is_confirmed = abap_true.   ENDIF.
    WHEN c_act_cancel.
      IF o_list_u0zi0gu276j00cars51w IS BOUND.   o_list_u0zi0gu276j00cars51w->is_confirmed = abap_false.   ENDIF.
    WHEN OTHERS.
      CLEAR rs_selfield-exit.
  ENDCASE.
ENDFORM.                    "user_command_user

*&---------------------------------------------------------------------*
*&      Form  set_list
*&---------------------------------------------------------------------*
FORM set_list USING list TYPE to_list.
  o_list_u0zi0gu276j00cars51w = list.
  IF o_list_u0zi0gu276j00cars51w IS BOUND.   o_list_u0zi0gu276j00cars51w->is_confirmed = abap_false.   ENDIF.
ENDFORM.                    "set_list

*&---------------------------------------------------------------------*
*&      Form  clear_list
*&---------------------------------------------------------------------*
FORM clear_list.
  CLEAR o_list_u0zi0gu276j00cars51w.
ENDFORM.                    "clear_list

**&---------------------------------------------------------------------*
**&      Form  Show_list
**&---------------------------------------------------------------------*
*FORM show_list USING layout TYPE slis_layout_alv
*                     fields TYPE slis_t_fieldcat_alv
*                     data TYPE STANDARD TABLE.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program       = 'ZAPLINK_LIST'
*      i_callback_pf_status_set = 'PF_STATUS_SET'
*      i_callback_user_command  = 'USER_COMMAND_USER'
*      i_grid_title             = 'Select objects'(lst)
*      it_fieldcat              = fields
*      is_layout                = layout
*    TABLES
*      t_outtab                 = data
*    EXCEPTIONS
*      OTHERS                   = 0.
*
*ENDFORM.                    "Show_list
