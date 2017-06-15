class ZAPLINK_OS_COMMAND definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.
  type-pools ABAP .

  types:
    begin of TS_OS_CMD_KEY,
      name     type SXPGCOSTAB-name,
      OPSYSTEM type SXPGCOSTAB-OPSYSTEM,
    end of TS_OS_CMD_KEY .

  constants FIELD_SEPARATOR type ABAP_CHAR1 value '~' ##NO_TEXT.
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_OS_COMMAND' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .
  class-methods COMP_TO_KEY
    importing
      !DATA type TD_COMPNAME
    returning
      value(RESULT) type TS_OS_CMD_KEY .
  class-methods KEY_TO_COMP
    importing
      value(DATA) type TS_OS_CMD_KEY
    returning
      value(RESULT) type TD_COMPNAME .

  methods ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE
    redefinition .
  methods ZAPLINK_CONNECTOR~CREATE_NEW_RAW
    redefinition .
  methods ZAPLINK_CONNECTOR~DELETE_FROM_SAP
    redefinition .
  methods ZAPLINK_CONNECTOR~DO_EXISTS
    redefinition .
  methods ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION
    redefinition .
  methods ZAPLINK_CONNECTOR~READ_FROM_SAP
    redefinition .
  methods ZAPLINK_CONNECTOR~WRITE_TO_SAP
    redefinition .
protected section.

  types TO_DATA type ref to ZAPLINK_EXCC_DATA .

  class-methods CALC_SIGNATURE
    importing
      value(DATA) type SXPGCOLIST
    returning
      value(RESULT) type TD_CHECKSUM
    raising
      ZAPLINK_CX_CONNECTOR .
private section.

  constants ST_OS_CMD type TD_COMPTYPE value 'EXCC' ##NO_TEXT.
  class-data R_DOC_ID type TR_DOCID .
  constants _UUID type TD_CONNUUID value 'DF0813D1BA8C0EF1BEAE000C291B645B' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  constants C_MSG_ID type TD_DOC_ID value 'NA' ##NO_TEXT.
ENDCLASS.



CLASS ZAPLINK_OS_COMMAND IMPLEMENTATION.


  method CALC_SIGNATURE.
  DATA d_string TYPE string.

  CLEAR: data-name, data-opsystem, data-addpar, data-sapcommand, data-permission, data-traceon.

  d_string = data.
  CONDENSE d_string.
  result = zaplink_tools=>calculate_md5_hash( d_string ).
  endmethod.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*NA       Message
  DATA _id LIKE LINE OF r_doc_id.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = C_MSG_ID. APPEND _id TO r_doc_id.
  endmethod.


  method COMP_TO_KEY.
SPLIT data AT field_separator INTO result-name result-opsystem.
  endmethod.


  method CONSTRUCTOR.
DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = st_os_cmd. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method KEY_TO_COMP.
CONCATENATE data-name field_separator data-opsystem INTO result.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
  DATA d_name TYPE td_compname.
  DATA d_type TYPE td_comptype.
  DATA s_name TYPE ts_os_cmd_key.
  data s_data type SXPGCOLIST.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_os_cmd.
        d_name = component->get_name( ).
        s_name = comp_to_key( d_name ).

* Main data
        CALL FUNCTION 'SXPG_COMMAND_GET'
          EXPORTING
            commandname             = s_name-name
            operatingsystem         = s_name-opsystem
*         TARGETSYSTEM            = '*'
          IMPORTING
            command                 = s_data
          EXCEPTIONS
            command_not_found       = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zaplink_cx_connector
             EXPORTING
               textid = zaplink_cx_connector=>not_found
               name   = d_name
               type   = st_os_cmd.
        ENDIF.
        result = calc_signature( s_data ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_data TYPE to_data.

  CREATE OBJECT object.
  CASE type.
    WHEN st_os_cmd.
      CREATE OBJECT o_data.
      object->raw ?= o_data.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA d_name TYPE td_compname.
  DATA s_name TYPE ts_os_cmd_key.
  DATA d_dialog TYPE c.
* FROM LSXPTTOP @ 62
  TYPES: BEGIN OF s_ext_cmd.
  INCLUDE TYPE sxpgcostab.
  TYPES:  sapcommand TYPE sxpgcolist-sapcommand,
          type(8)    TYPE c,
        END OF s_ext_cmd.
  DATA: external_command TYPE s_ext_cmd.
*FROM RSXPGDEF @ 132
  TYPES retcode TYPE i.
  DATA d_rc     TYPE retcode.
  CONSTANTS:
** errors for missing parameters
*   rc_first_missing_error         TYPE retcode VALUE 100,   "#EC NEEDED
*   rc_empty_name                  TYPE retcode VALUE 101,
*   rc_empty_opsystem              TYPE retcode VALUE 102,
*   rc_empty_command               TYPE retcode VALUE 103,
*   rc_empty_command_type          TYPE retcode VALUE 104,
*   rc_empty_target_server         TYPE retcode VALUE 105,
*   rc_empty_destination           TYPE retcode VALUE 106,
*   rc_parameter_expected          TYPE retcode VALUE 107,
*   rc_empty_list                  TYPE retcode VALUE 108,
*   rc_last_missing_error          TYPE retcode VALUE 199,   "#EC NEEDED
*
** DB errors
*   rc_first_db_error              TYPE retcode VALUE 200,   "#EC NEEDED
*   rc_db_insert_sxpgcotabe_error  TYPE retcode VALUE 201,
*   rc_db_insert_sxpgcostab_error  TYPE retcode VALUE 202,
*   rc_db_delete_sxpgcotabe_error  TYPE retcode VALUE 203,
*   rc_db_delete_sxpgcostab_error  TYPE retcode VALUE 204,
*   rc_db_update_sxpgcotabe_error  TYPE retcode VALUE 205,
*   rc_db_update_sxpgcostab_error  TYPE retcode VALUE 206,
*   rc_error_update_tbtcp          TYPE retcode VALUE 207,
*   rc_last_db_error               TYPE retcode VALUE 299,   "#EC NEEDED
*
** lock errors
*   rc_first_lock_error            TYPE retcode VALUE 300,   "#EC NEEDED
*   rc_foreign_lock                TYPE retcode VALUE 301,
*   rc_lock_system_failure         TYPE retcode VALUE 302,
*   rc_unknown_lock_error          TYPE retcode VALUE 303,
*   rc_last_lock_error             TYPE retcode VALUE 399,   "#EC NEEDED
*
** 'cannot get' errors
*   rc_first_cannot_get_error      TYPE retcode VALUE 400,   "#EC NEEDED
*   rc_no_server_list              TYPE retcode VALUE 401,
*   rc_no_servers_found            TYPE retcode VALUE 402,
*   rc_cannot_determine_local_host TYPE retcode VALUE 403,
*   rc_unknown_server_list_error   TYPE retcode VALUE 404,
*   rc_cannot_get_rfc_dests        TYPE retcode VALUE 405,
*   rc_cant_enq_tbtco_entry        TYPE retcode VALUE 406,   "#EC NEEDED
*   rc_jobcount_generation_error   TYPE retcode VALUE 407,   "#EC NEEDED
*   rc_last_cannot_get_error       TYPE retcode VALUE 499,   "#EC NEEDED
*
** no authority
*   rc_first_no_authority_error    TYPE retcode VALUE 500,   "#EC NEEDED
*   rc_no_permission               TYPE retcode VALUE 501,
*   rc_no_change_authority         TYPE retcode VALUE 502,
*   rc_no_execute_authority        TYPE retcode VALUE 503,
*   rc_cannot_change_sap_command   TYPE retcode VALUE 504,
*   rc_last_no_authority_error     TYPE retcode VALUE 599,   "#EC NEEDED
*
** errors for wrong parameters
*   rc_first_wrong_par_error       TYPE retcode VALUE 600,   "#EC NEEDED
*   rc_prohibited_command_name     TYPE retcode VALUE 601,
*   rc_prohibited_sap_command_name TYPE retcode VALUE 602,
*   rc_parameters_too_long         TYPE retcode VALUE 603,
*   rc_too_many_parameters         TYPE retcode VALUE 604,
*   rc_wrong_check_call_interface  TYPE retcode VALUE 605,
*   rc_illegal_command             TYPE retcode VALUE 606,
*   rc_screen_not_init             TYPE retcode VALUE 607,
*   rc_last_wrong_par_error        TYPE retcode VALUE 699,   "#EC NEEDED
*
** errors for 'object exists already'
*   rc_first_already_exists_error  TYPE retcode VALUE 700,   "#EC NEEDED
*   rc_sap_command_exists_already  TYPE retcode VALUE 701,
*   rc_cus_command_exists_already  TYPE retcode VALUE 702,
*   rc_command_already_exists      TYPE retcode VALUE 703,
*   rc_last_already_exists_error   TYPE retcode VALUE 799,   "#EC NEEDED
*
** errors for 'object does not exist'
*   rc_first_does_not_exist_error  TYPE retcode VALUE 800,   "#EC NEEDED
    rc_command_not_found           TYPE retcode VALUE 801,
*   rc_host_does_not_exist         TYPE retcode VALUE 802,   "#EC NEEDED
*   rc_job_does_not_exist          TYPE retcode VALUE 803,
*   rc_last_does_not_exist_error   TYPE retcode VALUE 899,   "#EC NEEDED
*
** unknown errors
*   rc_first_unknown_error         TYPE retcode VALUE 900,   "#EC NEEDED
*   rc_unknown_error               TYPE retcode VALUE 901,
*   rc_x_error                     TYPE retcode VALUE 902,
*   rc_external_error              TYPE retcode VALUE 903,
*   rc_last_unknown_error          TYPE retcode VALUE 999,   "#EC NEEDED
*
** action cancelled
*   rc_first_cancelled_error       TYPE retcode VALUE 1000,  "#EC NEEDED
*   rc_security_risk               TYPE retcode VALUE 1001,
*   rc_action_cancelled            TYPE retcode VALUE 1002,
*   rc_program_start_error         TYPE retcode VALUE 1003,
*   rc_program_termination_error   TYPE retcode VALUE 1004,
*   rc_communication_error         TYPE retcode VALUE 1005,
*   rc_system_error                TYPE retcode VALUE 1006,
*   rc_cannot_get_rfc_dest         TYPE retcode VALUE 1007,  "#EC NEEDED
*   rc_wrong_asynch_params         TYPE retcode VALUE 1008,  "#EC NEEDED
*   rc_last_cancelled_error        TYPE retcode VALUE 1099,  "#EC NEEDED
*
** error while pasring parameters
*   rc_first_parse_error          TYPE retcode VALUE 1100,   "#EC NEEDED
*   rc_parse_separator_missing    TYPE retcode VALUE 1101,   "#EC NEEDED
*   rc_parse_result_too_long      TYPE retcode VALUE 1102,   "#EC NEEDED
*   rc_parse_lgflnm_doesnt_exist  TYPE retcode VALUE 1103,   "#EC NEEDED
*   rc_parse_error_file_set_name  TYPE retcode VALUE 1104,   "#EC NEEDED
*   rc_parse_par_expected         TYPE retcode VALUE 1105,   "#EC NEEDED
*   rc_parse_add_par_not_allowed  TYPE retcode VALUE 1106,   "#EC NEEDED
*   rc_parse_cust_fm_doesnt_exist TYPE retcode VALUE 1107,   "#EC NEEDED
*   rc_parse_sap_fm_doesnt_exist  TYPE retcode VALUE 1108,   "#EC NEEDED
*   rc_parse_opsys_conv_broken    TYPE retcode VALUE 1109,   "#EC NEEDED
*   rc_parse_wrong_interface      TYPE retcode VALUE 1110,   "#EC NEEDED
*   rc_last_parse_error           TYPE retcode VALUE 1199,   "#EC NEEDED

* last error
    rc_last_error                  TYPE retcode VALUE 9999. "#EC NEEDED
  DATA d_type TYPE td_comptype.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_os_cmd.
        d_name = component->get_name( ).
        s_name = comp_to_key( d_name ).
        external_command-name = s_name-name.
        external_command-opsystem = s_name-opsystem.
        PERFORM command_delete IN PROGRAM saplsxpt
                USING d_dialog
             CHANGING external_command
                      d_rc.
        IF d_rc <> 0.
          CASE d_rc.
            WHEN rc_command_not_found.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>not_found.
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>system_error.
          ENDCASE.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  result = abap_true.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = d_type
                                                name = d_name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA d_name TYPE td_compname.
  DATA d_type TYPE td_comptype.
  DATA s_name TYPE ts_os_cmd_key.

  TRY.
    exists = me->exists-not_exists.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_os_cmd.
        d_name = component->get_name( ).
        s_name = comp_to_key( d_name ).
        CALL FUNCTION 'SXPG_COMMAND_GET'
          EXPORTING
            commandname             = s_name-name
            operatingsystem         = s_name-opsystem
*         TARGETSYSTEM            = '*'
*       IMPORTING
*         COMMAND                 =
          EXCEPTIONS
            command_not_found       = 1
            OTHERS                  = 2.
        IF sy-subrc = 0.
          exists = me->exists-exists.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
result = abap_false.
  IF version = _ver.
    result = abap_true.
  ENDIF.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA d_name TYPE td_compname.
  DATA d_type TYPE td_comptype.
  DATA s_name TYPE ts_os_cmd_key.
  DATA o_data  TYPE to_data.

  TRY.
    CREATE OBJECT object.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_os_cmd.
        d_name = component->get_name( ).
        s_name = comp_to_key( d_name ).

        CREATE OBJECT o_data.
* Main data
        CALL FUNCTION 'SXPG_COMMAND_GET'
          EXPORTING
            commandname             = s_name-name
            operatingsystem         = s_name-opsystem
*         TARGETSYSTEM            = '*'
          IMPORTING
            command                 = o_data->a0_maindata
          EXCEPTIONS
            command_not_found       = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zaplink_cx_connector
             EXPORTING
               textid = zaplink_cx_connector=>not_found
               name   = d_name
               type   = st_os_cmd.
        ENDIF.
        object->set_component( component ).
        CLEAR: o_data->a0_maindata-name,
               o_data->a0_maindata-opsystem.
        object->raw ?= o_data.
        object->code_signature = calc_signature( o_data->a0_maindata ).
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA d_name TYPE td_compname.
  DATA d_type TYPE td_comptype.
  DATA s_name TYPE ts_os_cmd_key.
  DATA o_data  TYPE to_data.
  DATA o_comp  TYPE to_component.

  TRY.
    CREATE OBJECT components.                         " Issue 92
    d_type = object->type.
    CASE d_type.
      WHEN st_os_cmd.
        o_data ?= object->raw.
*      CALL FUNCTION 'RS_CORR_INSERT'
*           EXPORTING
*                author              = sy-uname
*                global_lock         = 'X'
*                object              = ls_t100a-arbgb
*                object_class        = 'T100'
*                devclass            = '$TMP'
*                master_language     = sy-langu
*                mode                = 'INSERT'
**       IMPORTING
**            AUTHOR              = UNAME
**            KORRNUM             = CORRNUMBER_LOCAL
**            DEVCLASS            = DEVCLASS_LOCAL
*           EXCEPTIONS
*                cancelled           = 1
*                permission_failure  = 2
*                unknown_objectclass = 3.
*
*      IF sy-subrc <> 0.
*        CASE sy-subrc.
*          WHEN 2.
*            RAISE EXCEPTION TYPE zaplink_cx_connector
*                  EXPORTING
*                      name = _name
*                      type = object->type
*                  devclass = '$TMP'
*                    textid = zaplink_cx_connector=>not_authorized.
*          WHEN OTHERS.
*            RAISE EXCEPTION TYPE zaplink_cx_connector
*                  EXPORTING
*                      name = _name
*                      type = object->type
*                    textid = zaplink_cx_connector=>system_error.
*        ENDCASE.
*      ENDIF.

* Main data
        d_name = object->get_name( ).
        s_name = comp_to_key( d_name ).
        o_data->a0_maindata-name = s_name-name.
        o_data->a0_maindata-opsystem = s_name-opsystem.
        CALL FUNCTION 'SXPG_COMMAND_INSERT'
          EXPORTING
            command                      = o_data->a0_maindata
*         PUBLIC                       =
*         DIALOG                       =
          EXCEPTIONS
            command_already_exists       = 1
            no_permission                = 2
            parameters_wrong             = 3
            foreign_lock                 = 4
            system_failure               = 5
            OTHERS                       = 6.
        IF sy-subrc <> 0.
          IF sy-subrc <> 1.
            o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname  = 'SXPG_COMMAND_INSERT'
                                                     subrc     = sy-subrc
                                                     classname = 'ZAPLINK_CX_CONNECTOR'
*                                                   textid    = SYSTEM_ERROR
                                                   ).
            RAISE EXCEPTION o_mycx.
          ENDIF.
          CALL FUNCTION 'SXPG_COMMAND_MODIFY'
            EXPORTING
              command                = o_data->a0_maindata
*           DIALOG                 =
            EXCEPTIONS
              no_permission          = 1
              parameters_wrong       = 2
              system_failure         = 3
              foreign_lock           = 4
              OTHERS                 = 5.
          IF sy-subrc <> 0.
            o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname  = 'SXPG_COMMAND_MODIFY'
                                                     subrc     = sy-subrc
                                                     classname = 'ZAPLINK_CX_CONNECTOR'
*                                                   textid    = SYSTEM_ERROR
                                                   ).
            RAISE EXCEPTION o_mycx.
          ENDIF.
        ENDIF.

*        CREATE OBJECT o_comp.
*        o_comp->set_type( st_os_cmd ).
*        MOVE-CORRESPONDING o_data->a0_maindata TO s_name.
*        d_name = key_to_comp( s_name ).
*        o_comp->set_name( d_name ).
*        CREATE OBJECT components.
*        components->add( o_comp ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.

    check_component_list( EXPORTING     object = object
                           CHANGING components = components ). " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
