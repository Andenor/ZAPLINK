*&---------------------------------------------------------------------*
*&  Include           ZAPLINLK_SCR
*&---------------------------------------------------------------------*

*/------------------------SELECTION SCREEN----------------------------\

*SELECTION-SCREEN BEGIN OF BLOCK main WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE text-bac.
PARAMETER a_create TYPE c RADIOBUTTON GROUP r3 USER-COMMAND just_to_call_event.
PARAMETER a_dsp TYPE c RADIOBUTTON GROUP r3 DEFAULT 'X'.
SELECTION-SCREEN ULINE.
PARAMETER a_import TYPE c RADIOBUTTON GROUP r3. "
SELECTION-SCREEN ULINE.
PARAMETER a_uninst TYPE c RADIOBUTTON GROUP r3.  "uninstall
SELECTION-SCREEN ULINE.
PARAMETER a_auto TYPE c RADIOBUTTON GROUP r3.
PARAMETER a_synch TYPE c RADIOBUTTON GROUP r3.
SELECTION-SCREEN ULINE.
PARAMETER a_add TYPE c RADIOBUTTON GROUP r3.
PARAMETER a_add_p TYPE c RADIOBUTTON GROUP r3.
PARAMETER a_add_tr TYPE c RADIOBUTTON GROUP r3.
SELECTION-SCREEN END OF BLOCK mode.

* File
SELECTION-SCREEN BEGIN OF BLOCK fil WITH FRAME TITLE text-bfi.
PARAMETER f_local TYPE c RADIOBUTTON GROUP fil MODIF ID fil DEFAULT 'X' USER-COMMAND just_to_call_event.
PARAMETER f_server TYPE c RADIOBUTTON GROUP fil MODIF ID fil.
PARAMETERS p_file LOWER CASE TYPE string MODIF ID fil.
PARAMETERS p_zip AS CHECKBOX TYPE abap_bool MODIF ID fil.
SELECTION-SCREEN END OF BLOCK fil.

*Options
SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE text-bop.
PARAMETER c_name  TYPE zaplink_gui=>td_contname MODIF ID cre.
PARAMETER co_type TYPE ko100-object MODIF ID aco.
PARAMETER co_name TYPE zaplink_gui=>td_compname MODIF ID aco.
SELECT-OPTIONS:
  s_srcs FOR tadir-srcsystem MODIF ID add DEFAULT sy-sysid,
  s_type FOR tadir-object MODIF ID add,
  s_name FOR tadir-obj_name MODIF ID add,
  s_pack FOR tadir-devclass MODIF ID add.
PARAMETERS:
  p_ext TYPE abap_bool AS CHECKBOX MODIF ID add DEFAULT abap_true.
SELECT-OPTIONS:
  s_author FOR tadir-author  MODIF ID add DEFAULT sy-uname.
*PARAMETER package      TYPE tadir-devclass MODIF ID adp.
SELECT-OPTIONS p_tr FOR e071-trkorr NO INTERVALS NO-EXTENSION MODIF ID atr.

SELECTION-SCREEN:
  BEGIN OF TABBED BLOCK t_block FOR 5 LINES ,
    TAB (35) text-oth USER-COMMAND sub DEFAULT SCREEN 130,
    TAB (35) text-dev USER-COMMAND devc DEFAULT SCREEN 110,
    TAB (35) text-tro USER-COMMAND tr DEFAULT SCREEN 120,
    TAB (35) text-act USER-COMMAND act DEFAULT SCREEN 140,
  END OF BLOCK t_block.

SELECTION-SCREEN END OF BLOCK opt.
*SELECTION-SCREEN END OF BLOCK main.

SELECTION-SCREEN BEGIN OF SCREEN 110 AS SUBSCREEN.
PARAMETERS devc_no TYPE c RADIOBUTTON GROUP rd DEFAULT 'X' MODIF ID imp.
PARAMETERS devc_uc TYPE c RADIOBUTTON GROUP rd MODIF ID 110.
PARAMETERS devc_ke TYPE c RADIOBUTTON GROUP rd MODIF ID 110.
PARAMETERS devc_lo TYPE c RADIOBUTTON GROUP rd MODIF ID 110.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS devc_f TYPE c RADIOBUTTON GROUP rd MODIF ID 110.
SELECTION-SCREEN COMMENT (35) text-dvf FOR FIELD devclass ID 110.
PARAMETERS devclass TYPE tadir-devclass MODIF ID 110 DEFAULT '$TMP'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF SCREEN 110.

SELECTION-SCREEN BEGIN OF SCREEN 120 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS tr_fi TYPE c RADIOBUTTON GROUP rt MODIF ID 120.
SELECTION-SCREEN COMMENT (35) text-trf FOR FIELD tr ID 120.
PARAMETERS tr TYPE zaplink_opt_transport_request=>td_transport_request MODIF ID 120.
SELECTION-SCREEN END OF LINE.
PARAMETERS tr_uc TYPE c RADIOBUTTON GROUP rt MODIF ID 120.
PARAMETERS tr_us TYPE c RADIOBUTTON GROUP rt MODIF ID 120 DEFAULT 'X'.
PARAMETERS tr_in TYPE c RADIOBUTTON GROUP rt MODIF ID 120.
SELECTION-SCREEN END OF SCREEN 120.

* Sub component
SELECTION-SCREEN BEGIN OF SCREEN 130 AS SUBSCREEN.
PARAMETERS f_sub TYPE zaplink_datatypes=>td_with_subcomp DEFAULT zaplink_datatypes=>default_sub_component_level MODIF ID sub.
SELECTION-SCREEN END OF SCREEN 130.

SELECTION-SCREEN BEGIN OF SCREEN 140 AS SUBSCREEN.
PARAMETERS p_activ AS CHECKBOX TYPE abap_bool MODIF ID 140.
PARAMETERS p_force AS CHECKBOX TYPE abap_bool MODIF ID 140.
SELECTION-SCREEN END OF SCREEN 140.

*\--------------------------------------------------------------------/

DATA s_scr_name_create  TYPE RANGE OF screen-name.
DATA s_scr_name_display TYPE RANGE OF screen-name.
DATA s_scr_name_import  TYPE RANGE OF screen-name.
DATA s_scr_name_auto    TYPE RANGE OF screen-name.
DATA s_scr_name_synch   TYPE RANGE OF screen-name.
DATA s_scr_name_add     TYPE RANGE OF screen-name.
DATA s_scr_name_add_tr  TYPE RANGE OF screen-name.
DATA s_scr_name_add_comp TYPE RANGE OF screen-name.
DATA s_scr_name_uninst  TYPE RANGE OF screen-name.
DATA s_scr_name_tabs    TYPE RANGE OF screen-name.
DATA s_scr_name_activetabs TYPE RANGE OF screen-name.
DATA s_scr_name_frame   TYPE RANGE OF screen-name.
DATA s_scr_name_all     TYPE RANGE OF screen-name.
DATA d_file_type        TYPE string.

*/----------------------selection screen events-----------------------\
INITIALIZATION.
  PERFORM on_initialization.

AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
  PERFORM on_scr_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR tr.
  DATA s_selection TYPE  trwbo_selection.
  DATA s_request TYPE  trwbo_request_header.
  DATA s_task TYPE  trwbo_request_header.
  s_selection-client = sy-mandt.
  s_selection-reqfunctions = 'K'.
  s_selection-taskstatus = s_selection-reqstatus = 'DL'.
  s_selection-taskfunctions = 'RSX'.
  s_selection-connect_req_task_conditions = abap_true.
  CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
    EXPORTING
      iv_organizer_type          = 'W'
*     IV_USERNAME                = SY-UNAME
      is_selection               = s_selection
      iv_title                   = text-ttr
*     IS_NEW_REQUEST_PROPS       =
*     IV_PROG_TOP_OF_PAGE        = ' '
*     IV_FORM_TOP_OF_PAGE        =
    IMPORTING
      es_selected_request        = s_request
      es_selected_task           = s_task.
  tr = s_request-trkorr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR co_type.
  zaplink_connectors=>get_values( CHANGING type = co_type ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR co_name.
  PERFORM on_name_request USING co_type
                       CHANGING co_name.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_type-low.
  zaplink_connectors=>get_values( CHANGING type = s_type-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_type-high.
  zaplink_connectors=>get_values( CHANGING type = s_type-high ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-low.
  PERFORM on_name_request USING s_type-low
                       CHANGING s_name-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-high.
  DATA d_type TYPE t_obj_type.
  IF s_type-high IS INITIAL.    d_type = s_type-low.    ELSE.   d_type = s_type-high.   ENDIF.
  PERFORM on_name_request USING d_type
                       CHANGING s_name-low.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_file.
  PERFORM show_help_markups.

AT SELECTION-SCREEN ON HELP-REQUEST FOR c_name.
  PERFORM show_help_markups.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM on_file_request.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tr-low.
  PERFORM on_tr_request.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tr-high.
  PERFORM on_tr_request.

*&---------------------------------------------------------------------*
*&      Form  ON_INITIALIZATION
*&---------------------------------------------------------------------*
FORM on_initialization .
  DATA clskey TYPE seoclskey.
  DATA not_active TYPE seox_boolean.
  DATA t_fields TYPE  dyfatc_tab.
  DATA _scr LIKE LINE OF s_scr_name_import.
  FIELD-SYMBOLS:
    <f> LIKE LINE OF t_fields.

  clskey-clsname = 'ZAPLINK_FILE_ZIP'.
  CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
    EXPORTING
      clskey        = clskey
    IMPORTING
      not_active    = not_active
    EXCEPTIONS
      not_specified = 1
      not_existing  = 2
      is_interface  = 3
      no_text       = 4
      inconsistent  = 5
      OTHERS        = 6.
  IF sy-subrc = 0 AND not_active IS INITIAL.
    d_file_type = clskey-clsname.
  ENDIF.

  CREATE OBJECT zaplink_gui=>o_options.

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
      WHEN 'CRE'.
        APPEND _scr TO s_scr_name_create.
        APPEND _scr TO s_scr_name_auto.
        APPEND _scr TO s_scr_name_all.
      WHEN 'ADD'.
        APPEND _scr TO s_scr_name_add.
        APPEND _scr TO s_scr_name_auto.
        APPEND _scr TO s_scr_name_all.
      WHEN '110' OR '120' OR '140'.
        APPEND _scr TO s_scr_name_import.
        APPEND _scr TO s_scr_name_all.
      WHEN '130'.
        APPEND _scr TO s_scr_name_add.
        APPEND _scr TO s_scr_name_synch.
        APPEND _scr TO s_scr_name_auto.
        APPEND _scr TO s_scr_name_add_comp.
        APPEND _scr TO s_scr_name_add_tr.
        APPEND _scr TO s_scr_name_uninst.
        APPEND _scr TO s_scr_name_import.
        APPEND _scr TO s_scr_name_all.
      WHEN 'ACO'.
        APPEND _scr TO s_scr_name_add_comp.
        APPEND _scr TO s_scr_name_all.
      WHEN 'ATR'.
        APPEND _scr TO s_scr_name_add_tr.
        APPEND _scr TO s_scr_name_all.
      WHEN 'FIL'.
        IF <f>-name = 'P_ZIP' AND d_file_type IS INITIAL. CONTINUE. ENDIF.
        APPEND _scr TO s_scr_name_create.
        APPEND _scr TO s_scr_name_add.
        APPEND _scr TO s_scr_name_synch.
        APPEND _scr TO s_scr_name_auto.
        APPEND _scr TO s_scr_name_add_comp.
        APPEND _scr TO s_scr_name_add_tr.
        APPEND _scr TO s_scr_name_display.
        APPEND _scr TO s_scr_name_uninst.
        APPEND _scr TO s_scr_name_import.
        APPEND _scr TO s_scr_name_all.
    ENDCASE.
    _scr-high = <f>-push_fcode.
    CASE <f>-push_fcode.      "USER-COMMAND
      WHEN 'DEVC' OR 'TR' OR 'ACT'.
        APPEND _scr TO s_scr_name_tabs.
        APPEND _scr TO s_scr_name_import.
        APPEND _scr TO s_scr_name_all.
      WHEN 'SUB'.
        APPEND _scr TO s_scr_name_tabs.
        APPEND _scr TO s_scr_name_add.
        APPEND _scr TO s_scr_name_auto.
        APPEND _scr TO s_scr_name_synch.
        APPEND _scr TO s_scr_name_add_comp.
        APPEND _scr TO s_scr_name_add_tr.
        APPEND _scr TO s_scr_name_uninst.
        APPEND _scr TO s_scr_name_import.
        APPEND _scr TO s_scr_name_all.
    ENDCASE.
    IF <f>-group4 = 'SRI'.   " tab's frame
      APPEND _scr TO s_scr_name_frame.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ON_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  ON_SCR_OUTPUT
*&---------------------------------------------------------------------*
FORM on_scr_output .
* Options : DEVC & TR
  DATA _tab LIKE LINE OF s_scr_name_activetabs.
  DATA _active TYPE i.
  DATA _name TYPE string.

  CASE sy-dynnr.
    WHEN '1000'.
      LOOP AT SCREEN.
        CHECK screen-name IN s_scr_name_all.

        _active = 0.
        CASE abap_true.
          WHEN a_create.    IF screen-name IN s_scr_name_create.    _active = 1.    ENDIF.
          WHEN a_auto.      IF screen-name IN s_scr_name_auto.      _active = 1.    ENDIF.
          WHEN a_synch.     IF screen-name IN s_scr_name_synch.     _active = 1.    ENDIF.
          WHEN a_add.       IF screen-name IN s_scr_name_add.       _active = 1.    ENDIF.
          WHEN a_import.    IF screen-name IN s_scr_name_import.    _active = 1.    ENDIF.
          WHEN a_dsp.       IF screen-name IN s_scr_name_display.   _active = 1.    ENDIF.
          WHEN a_uninst.    IF screen-name IN s_scr_name_uninst.    _active = 1.    ENDIF.
          WHEN a_add_p.     IF screen-name IN s_scr_name_add_comp.     _active = 1.    ENDIF.
          WHEN a_add_tr.    IF screen-name IN s_scr_name_add_tr.    _active = 1.    ENDIF.
        ENDCASE.

        screen-active = _active. MODIFY SCREEN.
      ENDLOOP.

      _active = 0. REFRESH s_scr_name_activetabs. _tab-sign = 'I'. _tab-option = 'EQ'.
      LOOP AT SCREEN.
        CHECK screen-name IN s_scr_name_tabs.
        IF screen-active = 1.
          _active = 1.
          READ TABLE s_scr_name_tabs INTO _tab
              WITH KEY low = screen-name.
          _name = _tab-high. _tab-high = _tab-low. _tab-low = _name.
          APPEND _tab TO s_scr_name_activetabs.
        ENDIF.
      ENDLOOP.
      IF NOT s_scr_name_frame IS INITIAL.
        LOOP AT SCREEN.
          CHECK screen-name IN s_scr_name_frame.
          screen-active = _active. MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
      IF t_block-activetab NOT IN s_scr_name_activetabs.
        t_block-activetab = _name.
        CASE t_block-activetab.
          WHEN 'DEVC'.
            t_block-dynnr = '0110'.
          WHEN 'TR'.
            t_block-dynnr = '0120'.
          WHEN 'SUB'.
            t_block-dynnr = '0130'.
          WHEN 'ACT'.
            t_block-dynnr = '0140'.
        ENDCASE.
      ENDIF.
    WHEN '0110' OR '0120' OR '0140'.
      IF a_import = 'X'. _active = 1. ELSE. _active = 0. ENDIF.
      LOOP AT SCREEN. screen-active = _active. MODIFY SCREEN. ENDLOOP.
    WHEN '0130'.
      _active = 1.
      IF a_create = 'X' OR a_dsp = 'X'. _active = 0. ENDIF.
      LOOP AT SCREEN. screen-active = _active. MODIFY SCREEN. ENDLOOP.
  ENDCASE.
ENDFORM.                    " ON_SCR_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SHOW_HELP_MARKUPS
*&---------------------------------------------------------------------*
FORM show_help_markups .
  DATA t_links TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.
  CALL FUNCTION 'HELP_OBJECT_SHOW'
    EXPORTING
      dokclass                            = 'TX'
*     DOKLANGU                            = SY-LANGU
      dokname                             = 'ZAPLINK-P_FILE'
      doktitle                            = 'Filename documentation'(hfi)
      called_by_program                   = sy-repid
      called_by_dynp                      = sy-dynnr
*     CALLED_FOR_TAB                      = ' '
      called_for_field                    = 'P_FILE'
*     CALLED_FOR_TAB_FLD_BTCH_INPUT       = ' '
*     MSG_VAR_1                           = ' '
*     MSG_VAR_2                           = ' '
*     MSG_VAR_3                           = ' '
*     MSG_VAR_4                           = ' '
*     CALLED_BY_CUAPROG                   = ' '
*     CALLED_BY_CUASTAT                   =
*     SHORT_TEXT                          = ' '
*     CLASSIC_SAPSCRIPT                   = ' '
    TABLES
      links                               = t_links
    EXCEPTIONS
      object_not_found                    = 1
      sapscript_error                     = 2
      OTHERS                              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " SHOW_HELP_MARKUPS
*&---------------------------------------------------------------------*
*&      Form  ON_FILE_REQUEST
*&---------------------------------------------------------------------*
FORM on_file_request .
  DATA l_file TYPE zaplink_gui=>td_filename.

  IF f_local IS NOT INITIAL.
    l_file = p_file.
    CALL METHOD zaplink_gui=>containerfile_select
      CHANGING
        filename = l_file.
    p_file = l_file.
  ELSEIF f_server IS NOT INITIAL.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
*     EXPORTING
*       DIRECTORY              = ' '
*       FILEMASK               = ' '
      IMPORTING
        serverfile             = l_file
      EXCEPTIONS
        canceled_by_user       = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      p_file = l_file.
    ENDIF.
  ENDIF.
ENDFORM.                    " ON_FILE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  ON_TR_REQUEST
*&---------------------------------------------------------------------*
FORM on_tr_request .
  DATA:
    d_org_type   TYPE trwbo_calling_organizer,
    d_request TYPE trwbo_request_header,
    d_task    TYPE trwbo_request_header.
  d_org_type = 'W'.
*  is_selection-reqstatus = 'R'.
  CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
    EXPORTING
      iv_organizer_type   = d_org_type
      is_selection        = is_selection
    IMPORTING
      es_selected_request = d_request
      es_selected_task    = d_task.

  p_tr-low = d_request-trkorr.
ENDFORM.                    " ON_TR_REQUEST
*&---------------------------------------------------------------------*
*&      Form  ON_NAME_REQUEST
*&---------------------------------------------------------------------*
FORM on_name_request  USING p_type TYPE t_obj_type
                   CHANGING p_name.
  DATA d_name TYPE t_obj_name.
  d_name = p_name.
  zaplink_gui=>get_values( EXPORTING type = p_type
                           CHANGING value = d_name ).
  p_name = d_name.
ENDFORM.                    " ON_NAME_REQUEST
