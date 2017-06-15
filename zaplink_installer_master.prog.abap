*---------------------------------------------------------------------*
* Report  ZAPLINK_INSTALLER
*---------------------------------------------------------------------*

REPORT zaplink_installer MESSAGE-ID zaplink.

INCLUDE zaplink_installer_master_cl.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_file FOR FIELD p_file.
PARAMETERS p_file TYPE zaplink_file=>td_filename OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE t_action.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_dsp FOR FIELD p_dsp.
PARAMETERS p_dsp TYPE flag RADIOBUTTON GROUP mod DEFAULT 'X' USER-COMMAND just_to_call_event.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_inst FOR FIELD p_inst.
PARAMETERS p_inst TYPE flag RADIOBUTTON GROUP mod.
SELECTION-SCREEN COMMENT 35(60) t_conf.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_uninst FOR FIELD p_uninst.
PARAMETERS p_uninst TYPE flag RADIOBUTTON GROUP mod.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK mode.

SELECTION-SCREEN BEGIN OF BLOCK devclass WITH FRAME TITLE t_devc.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_novw FOR FIELD p_novw MODIF ID dev.
PARAMETERS p_novw TYPE flag MODIF ID dev RADIOBUTTON GROUP devc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_usec FOR FIELD p_usec MODIF ID dev.
PARAMETERS p_usec TYPE flag MODIF ID dev RADIOBUTTON GROUP devc DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) t_local FOR FIELD p_local MODIF ID dev.
PARAMETERS p_local TYPE flag MODIF ID dev RADIOBUTTON GROUP devc.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK devclass.

DATA s_scr_name_devc     TYPE RANGE OF screen-name.
DATA o_file     TYPE REF TO zaplink_file.
DATA cx_file    TYPE REF TO zaplink_cx_file.

INITIALIZATION.
  PERFORM on_initialization.
  zaplink_list=>callback_program = sy-repid.

AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
  PERFORM on_scr_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  TRY.
      o_file->load_dialog( p_file ).
      p_file = o_file->get_filename( ).
    CATCH zaplink_cx_file INTO cx_file.
      IF cx_file->is_exception_text( zaplink_cx_file=>dialog_canceled ) IS INITIAL.
        cx_file->write( ).
      ENDIF.
  ENDTRY.

START-OF-SELECTION.
  DATA o_list     TYPE zaplink_container_4inst=>to_list.
  DATA cx_list    TYPE REF TO zaplink_cx_list.
  DATA o_xml     	TYPE REF TO zaplink_container_4inst.
  DATA cx_xml     TYPE REF TO zaplink_cx_container.
  DATA o_options  TYPE REF TO zaplink_options.
  DATA o_o_dir    TYPE REF TO zaplink_opt_directory.
  DATA o_o_devc   TYPE REF TO zaplink_opt_devclass.
  DATA o_o_tr     TYPE REF TO zaplink_opt_transport_request.
  DATA o_comp     TYPE zaplink_container_4inst=>to_component.
  DATA t_comps    TYPE zaplink_list=>tt_compkeys.
  DATA s_comp     LIKE LINE OF t_comps.

  TRY.
      o_file->set_filename( p_file ).
    CATCH zaplink_cx_file INTO cx_file.
      cx_file->write( ).
  ENDTRY.

  TRY.
      CREATE OBJECT o_options.
      CREATE OBJECT o_o_dir.
      CREATE OBJECT o_o_devc.
      CREATE OBJECT o_o_tr.
      o_options->set_directory( o_o_dir ).
      o_o_dir->set_devclass( o_o_devc ).
      o_o_dir->set_transport_request( o_o_tr ).
      CASE abap_true.
        WHEN p_novw.    o_o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-no_overwrite ).
        WHEN p_usec.    o_o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-use_container ).
        WHEN p_local.   o_o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-local ).
      ENDCASE.
      o_o_tr->set_substitutionkind( zaplink_opt_transport_request=>substitutionkinds-user ). " let user select a transport request for all components
      o_file->set_filename( p_file ).
    CATCH zaplink_cx_file INTO cx_file.
      cx_file->write( ).
  ENDTRY.

  TRY.
      CREATE OBJECT o_xml
        EXPORTING
          o_file = o_file.
      o_xml->set_options( o_options ).
      o_xml->load_from_file( ).
      TRY.
          o_list = o_xml->get_content( ).
          CASE 'X'.
            WHEN p_dsp.
              o_list->display( ).
            WHEN p_inst.
              o_list->select_default( zaplink_list=>sel_actions-import ).
              o_list->change_selection( ).
              o_xml->transport_to_sap( o_list ).
              o_xml->log->write( ).
              PERFORM activate.
            WHEN p_uninst.
              o_list->select_default( zaplink_list=>sel_actions-uninstall ).
              o_list->change_selection( ).
              o_xml->uninstall_from_sap( o_list ).
              o_xml->log->write( ).
          ENDCASE.
        CATCH zaplink_cx_list INTO cx_list.
          cx_list->write( ).
      ENDTRY.
    CATCH zaplink_cx_container INTO cx_xml.
      cx_xml->write( ).
  ENDTRY.

END-OF-SELECTION.
*---------------------------------------------------------------------*
*      Form  Add_list
*---------------------------------------------------------------------*
FORM add_list USING o_lst TYPE zaplink_container_4inst=>to_list.
  DATA o_comp     TYPE zaplink_container_4inst=>to_component.
  DATA o_sublist  TYPE zaplink_container_4inst=>to_list.
  STATICS: d_bool TYPE abap_bool.

  IF d_bool IS INITIAL.  o_lst->init_sel_iter( ).   ELSE.   o_lst->init_iter( ). ENDIF.
  d_bool = abap_true.
  o_lst->display_progress = abap_false.
  o_comp = o_lst->get_next( ).
  WHILE o_comp IS BOUND.
    s_comp = o_comp->get_key( ).    APPEND s_comp TO t_comps.
    o_sublist = o_comp->get_subcomponents( ).
    IF o_sublist IS BOUND.    PERFORM add_list USING o_sublist.   ENDIF.
    o_comp = o_lst->get_next( ).
  ENDWHILE.

ENDFORM.                    "Add_list
*---------------------------------------------------------------------*
*      Form  ACTIVATE
*---------------------------------------------------------------------*
FORM activate .
  DATA o_activ    TYPE REF TO zaplink_activate.
  DATA d_count    TYPE i.
  DATA d_answer   TYPE c.
  DATA d_msg      TYPE string.
  FIELD-SYMBOLS <c> LIKE LINE OF t_comps.

  PERFORM add_list USING o_list.
  IF t_comps IS INITIAL. RETURN. ENDIF.
  CREATE OBJECT o_activ.
  o_activ->add_keys( t_comps ).
  t_comps = o_activ->ask_for_activation( ).
  IF o_activ->activation_requested IS INITIAL.
    WRITE:/ 'No activation requested'(nar).                 "#EC NOTEXT
  ELSEIF t_comps IS INITIAL.
    WRITE:/ 'All objects sucessfully activated :'(aok), o_activ->object_count. "#EC NOTEXT
  ELSE.
    WRITE:/ 'Here is the list of the component(s) that can''t be activated :'(sko). "#EC NOTEXT
    LOOP AT t_comps ASSIGNING <c>.    WRITE:/ <c>-type, <c>-name.   ENDLOOP.
  ENDIF.
ENDFORM.                    " ACTIVATE
*&---------------------------------------------------------------------*
*&      Form  ON_INITIALIZATION
*&---------------------------------------------------------------------*
FORM on_initialization .
*  DATA clskey TYPE seoclskey.
*  DATA not_active TYPE seox_boolean.
  DATA t_fields TYPE  dyfatc_tab.
  DATA _scr LIKE LINE OF s_scr_name_devc.
  FIELD-SYMBOLS:
    <f> LIKE LINE OF t_fields.

  t_file = 'Container''s file'.                             "#EC NOTEXT
  t_dsp = 'Display container''s containt'.                  "#EC NOTEXT
  t_inst = 'Install container'.                             "#EC NOTEXT
  t_conf = 'Confirm Selection with OK Code "CONFIRM" in the next screen'.       "#EC NOTEXT
  t_uninst = 'Uninstall container'.                         "#EC NOTEXT
  t_action = 'Action :'.                                    "#EC NOTEXT
  t_devc = 'Package :'.                                     "#EC NOTEXT
  t_novw = 'Do not overwrite'.                              "#EC NOTEXT
  t_usec = 'Use container''s package'.                      "#EC NOTEXT
  t_local = 'Use $TMP'.                                     "#EC NOTEXT
  TRY.
      CREATE OBJECT o_file.
      o_file->set_filetype( zaplink_file=>filetypes-local ).
    CATCH zaplink_cx_file INTO cx_file.
      cx_file->write( ).
  ENDTRY.

  CALL FUNCTION 'RPY_DYNPRO_READ'
    EXPORTING
      progname                    = sy-cprog
      dynnr                       = sy-dynnr
*     SUPPRESS_EXIST_CHECKS       = ' '
*     SUPPRESS_CORR_CHECKS        = ' '
*   IMPORTING
*     HEADER                      =
    TABLES
*     CONTAINERS                  =
      fields_to_containers        = t_fields
*     FLOW_LOGIC                  =
*     PARAMS                      =
    EXCEPTIONS
      cancelled                   = 1
      not_found                   = 2
      permission_error            = 3
      OTHERS                      = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  _scr-sign = 'I'. _scr-option = 'EQ'.
  LOOP AT t_fields ASSIGNING <f>.
    _scr-low = <f>-name.
    CASE <f>-group1.
      WHEN 'DEV'.
        APPEND _scr TO s_scr_name_devc.
    ENDCASE.
*    _scr-high = <f>-push_fcode.
*    CASE <f>-push_fcode.      "USER-COMMAND
*      WHEN 'DEV'.
*        APPEND _scr TO s_scr_name_devc.
*    ENDCASE.
  ENDLOOP.
ENDFORM.                    " ON_INITIALIZATION
*---------------------------------------------------------------------*
*      Form  on_scr_output
*---------------------------------------------------------------------*
FORM on_scr_output .
* Options : DEVC & TR
  DATA _tab LIKE LINE OF s_scr_name_devc.
  DATA _active TYPE i.
  DATA _name TYPE string.

  IF p_inst IS INITIAL.   _active = 0.    ELSE.   _active = 1.    ENDIF.
  CASE sy-dynnr.
    WHEN '1000'.
      LOOP AT SCREEN.
        CHECK screen-name IN s_scr_name_devc.
        screen-active = _active. MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " ON_SCR_OUTPUT
