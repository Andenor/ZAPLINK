*======================================================================
* standard sy-messages
*======================================================================
DEFINE mac_symsg_send.
  message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
END-OF-DEFINITION.

DEFINE mac_symsg_send_as_type.
  message id sy-msgid type &1 number sy-msgno
          display like sy-msgty
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
END-OF-DEFINITION.

DEFINE mac_symsg_raise.
  message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          raising &1.
END-OF-DEFINITION.

DEFINE mac_symsg_raise_as_type.
  message id sy-msgid type &1 number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          raising &2.
END-OF-DEFINITION.

DEFINE mac_symsg_raise_on_subrc.
  if sy-subrc = &1.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising &2.
  endif.
END-OF-DEFINITION.

DEFINE mac_symsg_into_text.
  message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          into &1.
END-OF-DEFINITION.

DEFINE mac_symsg_set.
  sy-msgid = &1.
  sy-msgty = &2.
  sy-msgno = &3.
  sy-msgv1 = &4.
  sy-msgv2 = &5.
  sy-msgv3 = &6.
  sy-msgv4 = &7.
END-OF-DEFINITION.

DEFINE mac_symsg_set_from_msg.
  sy-msgid = &1-msgid.
  sy-msgty = &1-msgty.
  sy-msgno = &1-msgno.
  sy-msgv1 = &1-msgv1.
  sy-msgv2 = &1-msgv2.
  sy-msgv3 = &1-msgv3.
  sy-msgv4 = &1-msgv4.
END-OF-DEFINITION.

DEFINE mac_msg_set_from_symsg.
  &1-msgid = sy-msgid.
  &1-msgty = sy-msgty.
  &1-msgno = sy-msgno.
  &1-msgv1 = sy-msgv1.
  &1-msgv2 = sy-msgv2.
  &1-msgv3 = sy-msgv3.
  &1-msgv4 = sy-msgv4.
END-OF-DEFINITION.


*======================================================================
* bapi messages
*======================================================================
DEFINE mac_bapimsg_to_symsg.
  sy-msgty = &1-type.
  sy-msgid = &1-id.
  sy-msgno = &1-number.
  sy-msgv1 = &1-message_v1.
  sy-msgv2 = &1-message_v2.
  sy-msgv3 = &1-message_v3.
  sy-msgv4 = &1-message_v4.
END-OF-DEFINITION.

DEFINE mac_msg_to_bapimsg.
  call function 'BALW_BAPIRETURN_GET2'
    exporting
      type       = &1-msgty
      cl         = &1-msgid
      number     = &1-msgno
      par1       = &1-msgv1
      par2       = &1-msgv2
      par3       = &1-msgv3
      par4       = &1-msgv4
*     LOG_NO     = ' '
*     LOG_MSG_NO = ' '
*     PARAMETER  = ' '
*     ROW        = 0
*     FIELD      = ' '
    importing
      return     = &2.
END-OF-DEFINITION.


*======================================================================
*======================================================================
DEFINE mac_msglist_add.
  call method &1->add
    exporting
      id_msgty = &2
      id_msgid = &3
      id_msgno = &4
      id_msgv1 = &5
      id_msgv2 = &6
      id_msgv3 = &7
      id_msgv4 = &8.
END-OF-DEFINITION.


*======================================================================
* formatting
*======================================================================
DEFINE mac_symsg_format_out.
  write &1 to &2 left-justified.
  condense &2.
END-OF-DEFINITION.

DEFINE mac_symsg_format_num.
  if &1 is not initial.
    write &1 to &2 no-zero.
    condense &2.
  else.
    clear &2.
  endif.
END-OF-DEFINITION.


*======================================================================
* assertions / fatal error handling
*======================================================================

DEFINE mac_invalid_precondition.
  assert 0 = 1.
END-OF-DEFINITION.

DEFINE mac_invalid_postcondition.
  assert 0 = 1.
END-OF-DEFINITION.

DEFINE mac_assert_initial.
  assert fields &1 condition &1 is initial.
END-OF-DEFINITION.

DEFINE mac_assert_not_initial.
  assert &1 is not initial.
END-OF-DEFINITION.

DEFINE mac_assert_ref.
  assert &1 is bound.
END-OF-DEFINITION.

DEFINE mac_assert_subrc.
  assert fields sy-subrc condition sy-subrc = 0.
END-OF-DEFINITION.


*======================================================================
* switch boolean value
*======================================================================
DEFINE mac_switch_flag.
  if &1 <> ' '.
    &1 = ' '.
  else.
    &1 = 'X'.
  endif.
END-OF-DEFINITION.

*======================================================================
* date
*======================================================================
*DEFINE mac_set_date_if_initial.
*  if cl_reca_date=>is_date_initial( &1 ) = abap_true.
*    &1 = &2.
*  endif.
*END-OF-DEFINITION.

DEFINE mac_clear_date_if_initial.
  if is_date_initial( &1 ) = abap_true.
    clear &1.
  endif.
END-OF-DEFINITION.

*DEFINE mac_clear_date_if_min_max.
*  if &1 = reca0_date-min or &1 = reca0_date-max.
*    clear &1.
*  endif.
*END-OF-DEFINITION.

*DEFINE mac_refresh_support_list.
** also refresh support message list
*  if cl_reca_trace=>ms_options-traceon = abap_true.
*    call method get_list_x
*      importing
*        et_list_x = mt_sup_msg.
*    md_sup_count = lines( mt_sup_msg ).
*  endif.
*END-OF-DEFINITION.
