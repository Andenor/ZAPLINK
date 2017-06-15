*---------------------------------------------------------------------*
*  Include           ZAPLINK_MACROS
*---------------------------------------------------------------------*
* Contains macros uses in framework objects
*---------------------------------------------------------------------*

* Macro for raising exception from symsg
DEFINE mac_raise_symsg.
  try.
      o_mycx ?= zaplink_cx=>create_from_symsg( classname = &1 " 'ZAPLINK_CX_CONNECTOR'
                                               textid    = &2 ). " zaplink_cx_connector=>system_error
    catch zaplink_cx into o_cx.
      raise exception o_cx.
  endtry.
  raise exception o_mycx.
END-OF-DEFINITION.

* Raise CNX exception : Type not supported
DEFINE mac_raise_type_not_supported.
* Type '&TYPE&' is not supported by this connector '&CONNCLASS&'
  raise exception type zaplink_cx_connector
    exporting
      textid = zaplink_cx_connector=>type_not_supported
      type   = &2
      connclass = &1.
END-OF-DEFINITION.

* Create a ZAPLink Cascade exception
DEFINE mac_cascade_raise.
* Exception '&CX_NAME&' has occured
  create object &1
    exporting
      textid  = zaplink_cx=>system_error
      previous = &2.
  &1->update( ).
  raise exception &1.
END-OF-DEFINITION.

* Issue 93
** Create a Cascade exception
*DEFINE mac_cascade_raise.
** Exception '&CX_NAME&' has occured
*  create object &1
*    exporting
*      textid   = zaplink_cx=>system_error
*      previous = &2.
*  &1->update( ).
*  raise exception &1.
*END-OF-DEFINITION.

* Add application logs if message present
DEFINE mac_add_log.
  clear sy-subrc.
  if &1 is bound and &2 is bound.             " for security
    if &1->has_messages_of_msgt( id_msgty = &3 ) = abap_true.
      &2->add_from_instance( io_msglist = &1
                      if_add_as_subnode = abap_true ).
      sy-subrc = 8.
    endif.
  endif.
END-OF-DEFINITION.

* Create application_log
* &1 Aplication log
* &2 Sub_object
* &3 Ext number
DEFINE mac_create_log.
  if not &1 is bound. create object &1. endif.
  if &1->get_handle( ) is initial.
    call method &1->init
      exporting
        id_object      = zaplink_datatypes=>ballog_object
        id_subobject   = &2
        id_extnumber   = &3
        auto_upd_custo = abap_true
        id_activity    = zaplink_message_collector=>actions-create
      exceptions
        error          = 1
        others         = 2.
    if sy-subrc <> 0.
      message id sy-msgid type 'I' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
END-OF-DEFINITION.

DEFINE mac_add_mf_and_raise.
  o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = &1
                                              subrc = &2
                                          classname = 'ZAPLINK_CX_CONNECTOR' ).
  application_log->add_symsg( ).
  application_log->add_exception( o_mycx ).
  raise exception o_mycx.
END-OF-DEFINITION.

DEFINE mac_raise_mf.
  o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = &1
                                              subrc = &2
                                          classname = 'ZAPLINK_CX_CONNECTOR' ).
  raise exception o_mycx.
END-OF-DEFINITION.

DEFINE mac_add_mf_and_raise_class.
  o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = &2
                                              subrc = &3
                                          classname = &1 ).
  application_log->add_symsg( ).
  application_log->add_exception( o_mycx ).
  raise exception o_mycx.
END-OF-DEFINITION.

DEFINE mac_add_name_meth_and_raise.
  o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = &1
                                                   method = &2
                                                    subrc = &3
                                             cx_classname = &4 ).
  application_log->add_symsg( ).
  application_log->add_exception( o_mycx ).
  raise exception o_mycx.
END-OF-DEFINITION.

DEFINE mac_add_obj_meth_and_raise.
  o_mycx ?= zaplink_cx=>create_from_method_cx( class = &1
                                              method = &2
                                               subrc = &3
                                        cx_classname = 'ZAPLINK_CX_CONNECTOR').
  application_log->add_symsg( ).
  application_log->add_exception( o_mycx ).
  raise exception o_mycx.
END-OF-DEFINITION.

DEFINE mac_raise_obj_meth_by_name.
  o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = &1
                                                   method = &2
                                                    subrc = &3
                                             cx_classname = 'ZAPLINK_CX_CONNECTOR').
  application_log->add_symsg( ).
  application_log->add_exception( o_mycx ).
  raise exception o_mycx.
END-OF-DEFINITION.

* Default catch : Catch and raise component exception or cascade any other ZAPLINK exception
DEFINE mac_def_catch.
  catch &1 into o_mycx.
    raise exception o_mycx.
    catch zaplink_cx into o_cx.
      mac_cascade_raise o_mycx o_cx.
    END-OF-DEFINITION.
