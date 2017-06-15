class ZAPLINK_FUNCTION_GROUP definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public

  global friends ZAPLINK_FUGR_DATA
                 ZAPLINK_FUNC_DATA
                 ZAPLINK_FUNC_RAW .

public section.
  type-pools ABAP .
  class ZAPLINK_FUGR_DATA definition load .

  types TD_FUNCTION_NAME type RS38L-NAME .
  types T_RAW type RSWSOURCET .
  types:
    begin of ts_text,
      langu   type SPRAS,
      ENTRY	  type TEXTPOOLTX,
      LENGTH  type TEXTPOOLLN,
    end of ts_text .
  types TD_FUNCTIONGROUP_NAME type TLIBG-AREA .
  types:
    tt_texts type SORTED TABLE OF ts_text WITH UNIQUE KEY langu .
  types:
    begin of ts_textpool,
      ID    type  TEXTPOOLID,
      KEY	  type  TEXTPOOLKY,
      texts type  tt_texts,
    end of ts_textpool .
  types:
    TT_TEXTPOOLs type SORTED TABLE OF ts_textpool WITH UNIQUE key ID KEY .
  types TS_SOURCE type STRING .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_FUNCTION_GROUP' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  methods GET_GROUP_SIGNATURE
    importing
      !GROUP_NAME type TD_FUNCTIONGROUP_NAME
    returning
      value(RESULT) type ZAPLINK_DATATYPES~TD_CHECKSUM .
  methods GET_FUNCTION_SIGNATURE
    importing
      !NAME type TD_FUNCTION_NAME
    returning
      value(RESULT) type ZAPLINK_DATATYPES~TD_CHECKSUM
    raising
      ZAPLINK_CX_CONNECTOR .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE
    redefinition .
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

  types TO_GROUP type ref to ZAPLINK_FUGR_DATA .
  types TO_FUNCTION type ref to ZAPLINK_FUNC_DATA .
  types TT_INCLUDES type ZAPLINK_FUGR_DATA=>TT_RAWINCLUDES .
  types TD_INCLUDENAME type ZAPLINK_FUGR_DATA=>TD_INCLUDENAME .
  types TS_FMD_FUGR type ZAPLINK_FUGR_DATA=>TS_FM_DATA .
  types TS_FMD_FUNC type ZAPLINK_FUNC_DATA=>TS_FM_DATA .
  types TS_INCLUDE type ZAPLINK_FUGR_DATA=>TS_RAWINCLUDE .

  class-data:
    BEGIN OF supportedkinds,
      group             TYPE td_transport_kind,
      function          TYPE td_transport_kind,
    END OF supportedkinds .
  constants:
    BEGIN OF supportedtypes,
      group             TYPE td_comptype VALUE 'FUGR',      "#EC NOTEXT
      function          TYPE td_comptype VALUE 'FUNC',      "#EC NOTEXT
    END OF supportedtypes .

  methods CALC_FUNCTION_SIGNATURE
    importing
      !FUNCTION type TS_FMD_FUNC
    returning
      value(RESULT) type ZAPLINK_DATATYPES~TD_CHECKSUM .
  methods READ_FUNCTION
    importing
      !NAME type TD_FUNCTION_NAME
    returning
      value(RESULT) type TS_FMD_FUNC
    raising
      ZAPLINK_CX_CONNECTOR .
  methods DEL_FUNCTION
    importing
      value(FUNC) type TS_FMD_FUNC
    raising
      ZAPLINK_CX_CONNECTOR .
  methods WRITE_FUNCTION
    importing
      value(FUNC) type TS_FMD_FUNC
    raising
      ZAPLINK_CX_CONNECTOR .
  methods WRITE_INCLUDE
    importing
      value(INCLUDE) type TS_INCLUDE
      !MAINPROGRAM type TRDIR-NAME
      !AREA type TLIBG-AREA
    changing
      !SOURCE type TT_ABAPRAWSOURCE
    raising
      ZAPLINK_CX_CONNECTOR .
private section.

  types:
    BEGIN OF ts_prog_attr.
  INCLUDE TYPE ts_base_attributs AS base.
  TYPES:
    END OF ts_prog_attr .
  types:
    tt_rs38l_incl TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY .
  types TO_RAW_FUNCTION type ZAPLINK_FUGR_DATA=>TO_FUNCTION .

  constants _UUID type TD_CONNUUID value 'F1838F4B202C2806E1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  constants C_MAINPROG_PREFIX type TRDIR-NAME value 'SAPL' ##NO_TEXT.
  data:
    BEGIN OF prev_func,
     name  TYPE  td_function_name,
     data  TYPE  ts_fmd_func,
   END OF prev_func .

  class-methods GET_ALL_INCLUDES
    importing
      !NAME type TD_FUNCTIONGROUP_NAME
      !WITH_CODE type ABAP_BOOL optional
    returning
      value(RESULTS) type TT_INCLUDES .
  class-methods GET_MAINPROGRAM
    importing
      !NAME type RS38L-AREA
    returning
      value(RESULT) type TRDIR-NAME .
ENDCLASS.



CLASS ZAPLINK_FUNCTION_GROUP IMPLEMENTATION.


  method CALC_FUNCTION_SIGNATURE.
  DATA d_src      TYPE string.
  DATA d_lcount   TYPE i.
  DATA t_code     TYPE tt_abaprawsource.
  FIELD-SYMBOLS: <s> LIKE LINE OF function-source.

  CHECK function-source IS NOT INITIAL.   t_code = function-source.
  READ TABLE t_code ASSIGNING <s> INDEX 1.
  IF <s> CP 'FUNCTION *.'.    DELETE t_code INDEX 1.   ENDIF.
  d_lcount = LINES( t_code ).
  READ TABLE t_code ASSIGNING <s> INDEX d_lcount.
  IF <s> CP 'ENDFUNCTION*.'.    DELETE t_code INDEX d_lcount.   ENDIF.

* Calcul MD5
  zaplink_tools=>pretty_printer( CHANGING table = t_code ).
  zaplink_tools=>condense_abap_source( CHANGING table = t_code ).
  d_src = zaplink_tools=>table_2_string( t_code ).
  d_src = zaplink_tools=>clean_abap_string( d_src ).
  result = zaplink_tools=>calculate_md5_hash( d_src ).
  endmethod.


  method CLASS_CONSTRUCTOR.
  supportedkinds-group =  zaplink_connectors=>get_typekind( supportedtypes-group ).
  supportedkinds-function =  zaplink_connectors=>get_typekind( supportedtypes-function ).
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = supportedtypes-group.    INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-function. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method DEL_FUNCTION.
  DATA d_pname     TYPE tfdir-pname.
  DATA d_group     TYPE rs38l-area.
  DATA d_msg       TYPE string.
  DATA t_params    TYPE STANDARD TABLE OF spar WITH DEFAULT KEY.
  DATA s_param     LIKE LINE OF t_params.
  DATA d_answer    TYPE c.

  TRY.

* FROM  FUNCTION 'RPY_FUNCTIONMODULE_READ'
    SELECT SINGLE pname INTO d_pname FROM tfdir WHERE funcname = func-header-name.
    IF sy-subrc <> 0.   EXIT.   ENDIF.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        program                            = d_pname
*          SUPPRESS_SELECT                    = 'X'
*          COMPLETE_AREA                      = ' '
      IMPORTING
*          NAMESPACE                          =
*          FUNCTION_NOT_EXISTS                =
        group                              = d_group
*          FUNCNAME                           =
*          INCLUDE_NUMBER                     =
*          NO_FUNCTION_INCLUDE                =
*          NO_FUNCTION_MODULE                 =
*               SUFFIX                             =
*               RESERVED_NAME                      =
*               TOO_MANY_DELIMITERS                =
*               RESERVED_FOR_EXITS                 =
*               HIDDEN_NAME                        =
*              CHANGING
*                INCLUDE                            = d_pname
      EXCEPTIONS
        include_not_exists                 = 1
        group_not_exists                   = 2
        no_selections                      = 3
        no_function_include                = 4
        no_function_pool                   = 5
        delimiter_wrong_position           = 6
        no_customer_function_group         = 7
        no_customer_function_include       = 8
        reserved_name_customer             = 9
        namespace_too_long                 = 10
        area_length_error                  = 11
        OTHERS                             = 12.
    IF sy-subrc <> 0.   EXIT.   ENDIF.

    IF d_group <> func-header-str_area.    " No problem on overwrite
      IF sy-batch = abap_true.   "only delete in forground
        EXIT.
      ENDIF.

* The function &1 already exist (&2), do you confirm deletion ?
      MESSAGE i000 WITH d_pname d_group INTO d_msg.
      REFRESH t_params.
      s_param-param = 'FUNCTION'.   s_param-value = func-header-name.         APPEND s_param TO t_params.
      s_param-param = 'GROUP'.      s_param-value = d_group.                  APPEND s_param TO t_params.
      s_param-param = 'CURRENT_G'.  s_param-value = func-header-str_area.     APPEND s_param TO t_params.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirm module function deletion'(d00)
          diagnose_object       = 'ZAPLINK_FUNCTION_GROUP-CONF'
          text_question         = d_msg
          text_button_1         = 'Yes'(yes)
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'No'(no_)
          icon_button_2         = 'ICON_SKIP'
          default_button        = '3'
          display_cancel_button = 'X'
          userdefined_f1_help   = 'ZAPLINK_FUNCTION_GROUP-CONFH'
          start_column          = 10
          start_row             = 4
          popup_type            = 'ICON_MESSAGE_WARNING'
          iv_quickinfo_button_1 = 'Function is deleted. Function group will be created with this function.'(hye)
          iv_quickinfo_button_2 = 'Function is not deleted and process to next existing function. Function group will be created without this function.'(hno)
        IMPORTING
          answer                = d_answer
        TABLES
          parameter             = t_params
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        mac_add_mf_and_raise 'POPUP_TO_CONFIRM' sy-subrc.
      ELSEIF d_answer = 'N' OR d_answer = '2'.
        EXIT.
      ELSEIF d_answer = 'A'.    " abend
        ROLLBACK WORK.
        mac_add_mf_and_raise 'RS_FUNCTION_DELETE' 1.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'RS_FUNCTION_DELETE'
      EXPORTING
        funcname                 = func-header-name
        suppress_popups          = abap_true
*               SUPPRESS_CHECKS          = ' '
*               SUPPRESS_DELE_ENHA       = ' '
*               TRREQUEST                =
      EXCEPTIONS
        cancelled                = 1
        function_released        = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_FUNCTION_DELETE' sy-subrc.
    ENDIF.
    DELETE FROM tftit WHERE funcname = func-header-name.
    DELETE FROM funct WHERE funcname = func-header-name.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method GET_ALL_INCLUDES.
  DATA: _result           LIKE LINE OF results.
  DATA d_include          TYPE rs38l-include.
  DATA d_is_hidden        TYPE abap_bool.
  DATA d_is_extern        TYPE abap_bool.
  DATA d_not_funct        TYPE abap_bool.
  DATA d_suffix           TYPE rs38l-suffix.
* Code from FM 'RS_GET_ALL_INCLUDES'
  DATA: program           TYPE trdir-name.
  DATA: _fugr_name        TYPE rs38l-area.
  DATA: _is_reserved_name TYPE c.
  DATA: includes          TYPE STANDARD TABLE OF rseuinc.
  FIELD-SYMBOLS:
    <wa> LIKE LINE OF includes.

  program = get_mainprogram( name ).
* Code from FM 'RS_GET_ALL_INCLUDES'
  SELECT * FROM d010inc
           APPENDING CORRESPONDING FIELDS OF TABLE includes
*           appending table includes
           WHERE master = program.
  CALL FUNCTION 'RS_WORKING_AREA_ACTIVE_CHECK'
    EXCEPTIONS
      nok = 1.
  IF sy-subrc = 0.
    SELECT * FROM rseuinc
             APPENDING CORRESPONDING FIELDS OF TABLE includes
*             appending table includes
             WHERE master = program.
  ENDIF.
  SORT includes BY include.
  DELETE ADJACENT DUPLICATES FROM includes COMPARING include.
  LOOP AT includes ASSIGNING <wa>.
* change from code from FM 'RS_GET_ALL_INCLUDES'
*    CALL FUNCTION 'RS_PROGNAME_SPLIT'
    d_include = <wa>-include.
    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
*     EXPORTING
*       PROGRAM                            =
*       SUPPRESS_SELECT                    = 'X'
*       COMPLETE_AREA                      = ' '
      IMPORTING
*       NAMESPACE                          =
*       FUNCTION_NOT_EXISTS                =
        group                              = _fugr_name
*       FUNCNAME                           =
*       INCLUDE_NUMBER                     =
        no_function_include                = d_is_extern
        no_function_module                 = d_not_funct
        suffix                             = d_suffix
        reserved_name                      = _is_reserved_name
*       TOO_MANY_DELIMITERS                =
*       RESERVED_FOR_EXITS                 =
        hidden_name                        = d_is_hidden
      CHANGING
        include                            = d_include
      EXCEPTIONS
        include_not_exists                 = 1
        group_not_exists                   = 2
        no_selections                      = 3
        no_function_include                = 4
        no_function_pool                   = 5
        delimiter_wrong_position           = 6
        no_customer_function_group         = 7
        no_customer_function_include       = 8
        reserved_name_customer             = 9
        namespace_too_long                 = 10
        area_length_error                  = 11
        OTHERS                             = 12.
    IF sy-subrc = 0 AND _fugr_name = name AND d_is_extern = abap_false AND
      ( with_code = abap_true OR
              ( d_not_funct = abap_true AND d_is_hidden = abap_false AND
                        ( d_suffix = 'TOP' OR _is_reserved_name = abap_false ) ) ) .
      _result-name = <wa>-include.
      _result-source = get_prog_rawsource( _result-name ).
      APPEND _result TO results.
    ENDIF.
  ENDLOOP.
  endmethod.


  method GET_FUNCTION_SIGNATURE.
  DATA d_src      TYPE string.
  DATA s_fm_func  TYPE ts_fmd_func.
*  DATA d_lcount   TYPE i.
*  FIELD-SYMBOLS: <s> LIKE LINE OF s_fm_func-source.

  s_fm_func = read_function( name ).
  result = calc_function_signature( s_fm_func ).
  endmethod.


  method GET_GROUP_SIGNATURE.
  DATA t_includes  TYPE tt_includes.
  DATA t_code      TYPE tt_abaprawsource.
*  DATA t_code_tmp  TYPE tt_abaprawsource.
  DATA d_src       TYPE string.
  DATA d_mainprog  TYPE td_progname.
*  DATA t_functions TYPE tt_rs38l_incl.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF t_includes.
*    <f> LIKE LINE OF t_functions.

* Main Prog
  d_mainprog = get_mainprogram( group_name ).
  t_code = get_prog_rawsource( d_mainprog ).

** Functions
*  CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
*    EXPORTING
*      function_pool           = group_name
*    TABLES
*      functab                 = t_functions
*    EXCEPTIONS
*      function_pool_not_found = 1
*      OTHERS                  = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*  LOOP AT t_functions ASSIGNING <f>.  t_code_tmp = get_prog_rawsource( <f>-include ).     APPEND LINES OF t_code_tmp TO t_code.   ENDLOOP.

* Includes
  t_includes = get_all_includes( name = group_name
                            with_code = abap_true  ).
  LOOP AT t_includes ASSIGNING <i>.   APPEND LINES OF <i>-source TO t_code.   ENDLOOP.

* Calcul MD5
  zaplink_tools=>pretty_printer( CHANGING table = t_code ).
  zaplink_tools=>condense_abap_source( CHANGING table = t_code ).
  d_src = zaplink_tools=>table_2_string( t_code ).
  d_src = zaplink_tools=>clean_abap_string( d_src ).
  result = zaplink_tools=>calculate_md5_hash( d_src ).
  endmethod.


  method GET_MAINPROGRAM.
  DATA s_rs38l TYPE rs38l.
* From LSFUNCTION_BUILDERI01 : module okcode_105 input.
  CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
    EXPORTING
      complete_area = name
    IMPORTING
      namespace     = s_rs38l-namespace
      group         = s_rs38l-str_area
    EXCEPTIONS
      OTHERS        = 6.
  CONCATENATE s_rs38l-namespace c_mainprog_prefix s_rs38l-str_area INTO result.
  endmethod.


  method READ_FUNCTION.
*  STATICS prev_func_name  TYPE  td_function_name.
*  STATICS prev_func_data  TYPE  ts_fmd_func.
  DATA o_data     TYPE to_group.
  DATA s_fmd_fugr TYPE ts_fmd_fugr.
  DATA d_name     TYPE rs38l-name.
  DATA type       TYPE td_comptype.
  DATA d_mainprog TYPE td_progname.
  DATA d_soft_comp TYPE tdevc-dlvunit.
*  DATA t_functions TYPE tt_rs38l_incl.
  DATA s_func LIKE LINE OF s_fmd_fugr-functions.
  DATA t_doc TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY.
  DATA t_src TYPE STANDARD TABLE OF rssource WITH DEFAULT KEY.
  data t_src_new type RSFB_SOURCE.    " Issue 98
  DATA t_tmp_doc TYPE tt_docs.
  DATA d_doc TYPE doku_obj.
  DATA s_doc_param TYPE zaplink_fugr_data=>ts_doc_param.
*  FIELD-SYMBOLS:
*    <f> LIKE LINE OF t_functions.

  IF prev_func-name = name.   result = prev_func-data.    RETURN.   ENDIF.    " Use Cache
  TRY.
    result-header-name = s_doc_param-func =  name.
*          s_func-source = get_prog_rawsource( <f>-include ).
    s_doc_param-param = '*'.    d_doc = s_doc_param.
    result-docs = zaplink_documentation=>get( ids = zaplink_fugr_data=>r_doc_ids-function
                                           object =  d_doc ).
*    CALL FUNCTION 'RPY_FUNCTIONMODULE_READ'    " Issue 98 : Source might be wider than 72 Chars
    CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
      EXPORTING
        functionname             = result-header-name
      IMPORTING
        global_flag              = result-header-global
        remote_call              = result-header-remote
        update_task              = result-header-utask
*        SHORT_TEXT               =
        function_pool            = result-header-str_area
      TABLES
        import_parameter         = result-import_p
        changing_parameter       = result-changing_p
        export_parameter         = result-export_p
        tables_parameter         = result-tables
        exception_list           = result-exceptions
        documentation            = t_doc
        SOURCE                   = t_src
      CHANGING
        NEW_SOURCE               = t_src_new
      EXCEPTIONS
        error_message            = 1
        function_not_found       = 2
        invalid_name             = 3
        OTHERS                   = 4.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RPY_FUNCTIONMODULE_READ_NEW' sy-subrc.
    ENDIF.
    If not t_src_new is INITIAL.    result-source = t_src_new.    else.   result-source = t_src.    endif.
    SELECT * INTO TABLE result-texts FROM tftit WHERE funcname = result-header-name.
    SELECT * INTO TABLE result-param_texts FROM funct WHERE funcname = result-header-name.

    prev_func-data = result.    prev_func-name = name.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method WRITE_FUNCTION.
  DATA t_doc       TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY.
  DATA t_src       TYPE STANDARD TABLE OF rssource WITH DEFAULT KEY.
*  DATA t_src_new   TYPE rsfb_source.                       " Issue 122
  DATA fm_include  TYPE rs38l-include.                      " Issue 122

  TRY.
*    t_src_new = func-source.
    CALL FUNCTION 'RPY_FUNCTIONMODULE_INSERT'
      EXPORTING
        funcname                      = func-header-name
        function_pool                 = func-header-str_area
        interface_global              = func-header-global
        remote_call                   = func-header-remote
        short_text                    = space
        suppress_corr_check           = abap_false
        update_task                   = func-header-utask
*        CORRNUM                       = ' '
*        NAMESPACE                     = ' '
*        new_source                    = t_src_new                    " Issue 122
      IMPORTING
        function_include              = fm_include                    " Issue 122
      TABLES
        import_parameter              = func-import_p
        export_parameter              = func-export_p
        tables_parameter              = func-tables
        changing_parameter            = func-changing_p
        exception_list                = func-exceptions
        parameter_docu                = t_doc
        SOURCE                        = t_src
      EXCEPTIONS
        double_task                   = 1
        error_message                 = 2
        function_already_exists       = 3
        invalid_function_pool         = 4
        invalid_name                  = 5
        too_many_functions            = 6
        no_modify_permission          = 7
        no_show_permission            = 8
        enqueue_system_failure        = 9
        canceled_in_corr              = 10
        OTHERS                        = 11.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      mac_add_mf_and_raise 'RPY_FUNCTIONMODULE_INSERT' sy-subrc.
    ENDIF.
    set_prog_rawsource( program = fm_include                        " Issue 122
                            raw = func-source ).
    MODIFY tftit FROM TABLE func-texts.
    MODIFY funct FROM TABLE func-param_texts.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method WRITE_INCLUDE.
  DATA _oname   TYPE e071-obj_name.
  DATA _rep     TYPE progdir.
  DATA _fugr    TYPE rs38l-area.
  DATA _sufix   TYPE rs38l-suffix.
  DATA f_noinc  TYPE abap_bool.
  DATA _include TYPE rs38l-include.

  _rep-name = _oname = _include = include-name.
  CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
*   EXPORTING
*     PROGRAM                            =
*     SUPPRESS_SELECT                    = 'X'
*     COMPLETE_AREA                      = ' '
    IMPORTING
*     NAMESPACE                          =
*     FUNCTION_NOT_EXISTS                =
      group                              = _fugr
*     FUNCNAME                           =
*     INCLUDE_NUMBER                     =
      no_function_include                = f_noinc
*     NO_FUNCTION_MODULE                 =
      suffix                             = _sufix
*     RESERVED_NAME                      =
*     TOO_MANY_DELIMITERS                =
*     RESERVED_FOR_EXITS                 =
*     HIDDEN_NAME                        =
    CHANGING
      include                            = _include
    EXCEPTIONS
      include_not_exists                 = 1
      group_not_exists                   = 2
      no_selections                      = 3
      no_function_include                = 4
      no_function_pool                   = 5
      delimiter_wrong_position           = 6
      no_customer_function_group         = 7
      no_customer_function_include       = 8
      reserved_name_customer             = 9
      namespace_too_long                 = 10
      area_length_error                  = 11
      OTHERS                             = 12.
  IF sy-subrc <> 0.
    mac_add_mf_and_raise 'FUNCTION_INCLUDE_SPLIT' sy-subrc.
  ENDIF.
  IF _sufix <> 'TOP' OR _fugr <> area OR NOT f_noinc IS INITIAL.
    CALL FUNCTION 'RS_CREATE_NEW_INCLUDE'
      EXPORTING
        master                   = mainprogram
        include_name             = _rep-name
        new_include              = abap_true
        insert_include_statement = abap_true
      TABLES
        master_source            = source
      EXCEPTIONS
        program_enqueued         = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_CREATE_NEW_INCLUDE' sy-subrc.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
    EXPORTING
      object                  = 'REPS'
      obj_name                = _oname
*      DELETED_FLAG            = ' '
*    IMPORTING
*      OBJECT_INSERTED         =
    EXCEPTIONS
      wrong_object_name       = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    mac_add_mf_and_raise 'RS_INSERT_INTO_WORKING_AREA' sy-subrc.
  ENDIF.

* Issue 137 Should not get REPT (no text with includes)
*  CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
*    EXPORTING
*      object                  = 'REPT'
*      obj_name                = _oname
**      DELETED_FLAG            = ' '
**    IMPORTING
**      OBJECT_INSERTED         =
*    EXCEPTIONS
*      wrong_object_name       = 1
*      OTHERS                  = 2.
*  IF sy-subrc <> 0.
*    mac_add_mf_and_raise 'RS_INSERT_INTO_WORKING_AREA' sy-subrc.
*  ENDIF.

  _rep-subc = 'I'.
  _rep-varcl = abap_true.   " Case sensitive
  _rep-appl = 'S'.    " System
  _rep-state = 'I'.   MODIFY progdir FROM _rep.
* MODIFY reposrc FROM _rep. "The databae view "REPOSRC" is write-protected, so it cannot be changed.
  _rep-state = 'A'.   MODIFY progdir FROM _rep.

  set_prog_rawsource( program = include-name
                          raw = include-source ).
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_group    TYPE to_group.
  DATA o_function TYPE to_function.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN supportedtypes-group.
      o_group ?= object->raw.
      o_group->anonymize( ).
    WHEN supportedtypes-function.
      o_function ?= object->raw.
      o_function->anonymize( ).
    WHEN OTHERS.
       mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE.
*  DATA o_prog TYPE to_program.
*
*  TRY.
*      super->zaplink_cnx_ext_cleaner~unanonymize( object ).
*    CATCH zaplink_cx_connector INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
*
*  o_prog ?= object->raw.
*  o_prog->A0_MAINDATA-CNAM = sy-uname.
**    o_prog->A0_MAINDATA-CDAT,
**    o_prog->A0_MAINDATA-UNAM,
**    o_prog->A0_MAINDATA-UDAT,
**    o_prog->A0_MAINDATA-SDATE,
**    o_prog->A0_MAINDATA-STIME,
**    o_prog->A0_MAINDATA-IDATE,
**    o_prog->A0_MAINDATA-ITIME.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
  DATA d_group TYPE td_functiongroup_name.
  DATA d_func  TYPE td_function_name.
  DATA d_type  TYPE td_comptype.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN supportedtypes-group.
        d_group = component->get_name( ).
        result = get_group_signature( d_group ).
      WHEN supportedtypes-function.
        d_func = component->get_name( ).
        result = get_function_signature( d_func ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_group    TYPE to_group.
  DATA o_function TYPE to_function.
  DATA o_raw_func TYPE to_raw_function.

  CASE type.
    WHEN supportedtypes-group.
      CREATE OBJECT object.
      CREATE OBJECT o_group.
      object->raw = o_group.
    WHEN supportedtypes-function.
      CREATE OBJECT o_raw_func.
      object = o_raw_func.
      CREATE OBJECT o_function.
      object->raw = o_function.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA pool   TYPE rs38l-area.
  DATA d_func TYPE rs38l-name.
  DATA type   TYPE td_comptype.
  DATA name   TYPE td_compname.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN supportedtypes-function.
        d_func = name = component->get_name( ).
        CALL FUNCTION 'RS_FUNCTION_DELETE'
          EXPORTING
            funcname                 = d_func
            suppress_popups          = abap_true
*            SUPPRESS_CHECKS          = ' '
*            SUPPRESS_DELE_ENHA       = ' '
*            TRREQUEST                =
          EXCEPTIONS
            cancelled                = 1
            function_released        = 2
            OTHERS                   = 3.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'RS_FUNCTION_DELETE' sy-subrc.
        ENDIF.
      WHEN supportedtypes-group.
        pool = name = component->get_name( ).
        CALL FUNCTION 'FUNCTION_POOL_DELETE'
          EXPORTING
            pool              = pool
            skip_progress_ind = 'X'
          EXCEPTIONS
            functions_exists  = 1
            not_found         = 0
            OTHERS            = 3.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'FUNCTION_POOL_DELETE' sy-subrc.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.

  result = abap_true.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = type
                                                name = name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA d_group    TYPE td_functiongroup_name.
  DATA d_function TYPE TD_FUNCTION_NAME.
  DATA type       TYPE td_comptype.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN supportedtypes-group.
        d_group = component->get_name( ).
        CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
          EXPORTING
            function_pool   = d_group
          EXCEPTIONS
            pool_not_exists = 1
            OTHERS          = 2.
        IF sy-subrc = 0.    exists = me->exists-exists.   ENDIF.
      WHEN supportedtypes-function.
        d_function = component->get_name( ).
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname                 = d_function
*         IMPORTING
*           GROUP                    =
*           INCLUDE                  =
*           NAMESPACE                =
*           STR_AREA                 =
          EXCEPTIONS
            FUNCTION_NOT_EXIST       = 1
            OTHERS                   = 2.
        IF sy-subrc = 0.    exists = me->exists-exists.   ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.
    result = abap_true.
  endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA o_group    TYPE to_group.
  DATA o_function TYPE to_function.
  DATA s_fmd_fugr TYPE ts_fmd_fugr.
  DATA s_fmd_func TYPE ts_fmd_func.
  DATA d_name     TYPE td_compname.
  DATA type       TYPE td_comptype.
  DATA d_mainprog TYPE td_progname.
  DATA d_soft_comp TYPE tdevc-dlvunit.
  DATA t_functions TYPE tt_rs38l_incl.
*  DATA s_func LIKE LINE OF s_fmd_fugr-functions.
*  DATA t_doc TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY.
*  DATA t_src TYPE STANDARD TABLE OF rssource WITH DEFAULT KEY.
*  DATA t_tmp_doc TYPE tt_docs.
  DATA d_doc TYPE doku_obj.
*  DATA s_doc_param TYPE zaplink_fugr_data=>ts_doc_param.
*    BEGIN OF s_doc_param,
*      func   TYPE rs38l_fnam,
*      param TYPE rs38l_par_,
*    END OF s_doc_param.
  FIELD-SYMBOLS:
    <f> LIKE LINE OF t_functions.

  TRY.
    type = component->get_type( ).
    CREATE OBJECT object.
    object->set_component( component ).
    CASE type.
      WHEN supportedtypes-function.
        s_fmd_func-header-name = object->name.
        s_fmd_func = read_function( s_fmd_func-header-name ).
        s_fmd_func-code_signature = calc_function_signature( s_fmd_func ).
        CREATE OBJECT o_function.
        o_function->from_data( s_fmd_func ).
        CLEAR o_function->a0_maindata-name.
        object->raw = o_function.
      WHEN supportedtypes-group.
        s_fmd_fugr-header-area = d_name = object->name.
        CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
          EXPORTING
            function_pool           = s_fmd_fugr-header-area
          TABLES
            functab                 = t_functions
          EXCEPTIONS
            function_pool_not_found = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'RS_FUNCTION_POOL_CONTENTS' sy-subrc.
        ENDIF.

        d_doc = d_mainprog = get_mainprogram( s_fmd_fugr-header-area ).
        s_fmd_fugr-docs = zaplink_documentation=>get( ids = zaplink_fugr_data=>r_doc_ids-group
                                                  object = d_doc  ).

        SELECT * INTO TABLE s_fmd_fugr-texts FROM tlibt WHERE area = s_fmd_fugr-header-area.
* Issue 95
*        SELECT * INTO TABLE s_fmd_fugr-function_texts FROM tftit FOR ALL ENTRIES IN t_functions WHERE funcname = t_functions-funcname.
*        SELECT * INTO TABLE s_fmd_fugr-param_texts FROM funct FOR ALL ENTRIES IN t_functions WHERE funcname = t_functions-funcname.
        LOOP AT t_functions ASSIGNING <f>.
          s_fmd_func = read_function( <f>-funcname ).   clear s_fmd_func-header-str_area.
          s_fmd_func-code_signature = calc_function_signature( s_fmd_func ).
          INSERT s_fmd_func INTO TABLE s_fmd_fugr-functions.
        ENDLOOP.

        SELECT SINGLE dlvunit INTO d_soft_comp
          FROM v_tralan
          WHERE pgmid = supportedkinds-group
            AND object = supportedtypes-group
            AND obj_name = s_fmd_fugr-header-area.

        s_fmd_fugr-includes = get_all_includes( s_fmd_fugr-header-area ).

*          s_fmd_fugr-includes = remove_unwanted_includes( includes = s_fmd_fugr-includes
*                                                        functions = t_functions
*                                                        soft_comp = d_soft_comp ).

* Texts Pool
        s_fmd_fugr-textspool = get_prog_textpool( d_mainprog ).

* Dynpro
        s_fmd_fugr-dynpros = get_dynpros( d_mainprog ).

* Menu Painter
        s_fmd_fugr-menupainter = get_menus( d_mainprog ).
** Documentation
**      CONCATENATE _name '*' INTO _obj. " Issue:7
*          s_obj-prog = _name.
*          s_obj-wildcard = '*'.
*          _obj = s_obj.
**Issue:7
*          o_group->documentation = zaplink_documentation=>get( ids = r_doc_id
*                                                          object = _obj ).
        CREATE OBJECT o_group.
        o_group->from_data( s_fmd_fugr ).
        o_group->_code_signature = get_group_signature( o_group->a0_maindata-area ).
        CLEAR o_group->a0_maindata-area.
        object->raw = o_group.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA o_group      TYPE to_group.
  DATA o_function   TYPE to_function.
  DATA s_fmd_fugr   TYPE ts_fmd_fugr.
  DATA s_fmd_func   TYPE ts_fmd_func.
  DATA _comp        TYPE to_component.
  DATA d_pname      TYPE tfdir-pname.
  DATA d_group      TYPE rs38l-area.
  DATA _name        TYPE td_compname.
  DATA d_mainprog   TYPE td_progname.
  DATA t_functions  TYPE tt_rs38l_incl.
  DATA d_msg        TYPE string.
  DATA d_answer     TYPE c.
  DATA t_doc        TYPE STANDARD TABLE OF rsfdo WITH DEFAULT KEY.
  DATA t_src        TYPE STANDARD TABLE OF rssource WITH DEFAULT KEY.
  DATA t_params     TYPE STANDARD TABLE OF spar WITH DEFAULT KEY.
  DATA t_source     TYPE tt_abaprawsource.
  DATA s_param      LIKE LINE OF t_params.
  DATA o_source     TYPE REF TO cl_wb_source.
  DATA t_key        TYPE  trkey.
  DATA source_name  TYPE  trdir-name.
  FIELD-SYMBOLS:
    <i>  LIKE LINE OF s_fmd_fugr-includes,
    <fi> LIKE LINE OF t_functions,
    <f>  LIKE LINE OF s_fmd_fugr-functions.

  TRY.
      CREATE OBJECT components.
      CASE object->type.
        WHEN supportedtypes-function.
          o_function ?= object->raw.
          o_function->a0_maindata-name = object->name.
          s_fmd_func = o_function->to_data( ).

          del_function( s_fmd_func ).
          write_function( s_fmd_func ).
          CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
            EXPORTING
              function_pool           = s_fmd_func-header-str_area
            TABLES
              functab                 = t_functions
            EXCEPTIONS
              function_pool_not_found = 1
              OTHERS                  = 2.
          IF sy-subrc <> 0.
            ROLLBACK WORK.
            mac_add_mf_and_raise 'RS_FUNCTION_POOL_CONTENTS' sy-subrc.
          ENDIF.
          READ TABLE t_functions ASSIGNING <fi> WITH KEY funcname = s_fmd_func-header-name.
          IF sy-subrc <> 0. mac_add_mf_and_raise 'RS_FUNCTION_POOL_CONTENTS' 2. ENDIF.
*          set_prog_rawsource( program = <fi>-include
*                                  raw = s_fmd_func-source ).

        WHEN supportedtypes-group.
          o_group ?= object->raw.
          o_group->a0_maindata-area = object->name.
          s_fmd_fugr = o_group->to_data( ).
          d_mainprog = get_mainprogram( s_fmd_fugr-header-area ).

          CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
            EXPORTING
              area                         = s_fmd_fugr-header-area
*             CORRNUM                      = ' '
*             TEXT                         = ' '
*             UNAME                        = ' '
*             WITH_KORR                    = 'X'
*             WB_FB_MANAGER                =
              suppress_popups              = abap_true
              skip_progress_ind            = abap_true
*           IMPORTING
*             E_CORRNUM                    =
            EXCEPTIONS
              canceled_in_corr             = 1
              enqueue_system_failure       = 2
              function_exist               = 3
              not_executed                 = 4
              no_modify_permission         = 5
              no_show_permission           = 6
              permission_failure           = 7
              pool_not_exist               = 0
              cancelled                    = 9
              OTHERS                       = 10.
          IF sy-subrc <> 0.
            mac_add_mf_and_raise 'RS_FUNCTION_POOL_DELETE' sy-subrc.
          ENDIF.

* FUNCTIONS
          IF sy-batch = abap_false.   "only delete in foreground
            LOOP AT s_fmd_fugr-functions ASSIGNING <f>.
              del_function( <f> ).
            ENDLOOP.
          ENDIF.

          CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
            EXPORTING
              function_pool                 = s_fmd_fugr-header-area
              short_text                    = space
*             RESPONSIBLE                   = SY-UNAME
*             NAMESPACE                     = ' '
*             DEVCLASS                      =
*             CORRNUM                       =
*             SUPPRESS_LANGUAGE_CHECK       = 'X'
*             AUTHORITY_CHECK               = 'X'
*             SUPPRESS_CORR_CHECK           = 'X'
*             UNICODE_CHECKS                = 'X'
*           IMPORTING
*             CORRNUM                       =
            EXCEPTIONS
              name_already_exists           = 1
              name_not_correct              = 2
              function_already_exists       = 3
              invalid_function_pool         = 4
              invalid_name                  = 5
              too_many_functions            = 6
              no_modify_permission          = 7
              no_show_permission            = 8
              enqueue_system_failure        = 9
              canceled_in_corr              = 10
              undefined_error               = 11
              OTHERS                        = 12.
          IF sy-subrc <> 0.
            ROLLBACK WORK.
            mac_add_mf_and_raise 'RS_FUNCTION_POOL_INSERT' sy-subrc.
          ENDIF.

* Text Pool
          set_prog_textpool( program = d_mainprog
                            textpool = s_fmd_fugr-textspool ).
* Dynpro
          set_dynpros( program = d_mainprog
                       dynpros = s_fmd_fugr-dynpros ).

* Menu Painter
          set_menus( program = d_mainprog
                       menus = s_fmd_fugr-menupainter ).

*Texts
          DELETE FROM tlibt WHERE area = s_fmd_fugr-header-area.
          MODIFY tlibt FROM TABLE s_fmd_fugr-texts.

*          MODIFY tftit FROM TABLE s_fmd_fugr-function_texts.     " Issue 100
*          MODIFY funct FROM TABLE s_fmd_fugr-param_texts.        " Issue 100

** Documentation
*          zaplink_documentation=>set( t_docs = o_group->documentation ).

* INCLUDES
*ISSUE 125
          t_key-obj_type = supportedtypes-group.
          t_key-obj_name = s_fmd_fugr-header-area.
          t_key-sub_type = 'REPS'.
          t_key-sub_name = d_mainprog.
          CREATE OBJECT o_source
             EXPORTING
               transport_key = t_key
               source_name = d_mainprog
*              modification_mode =
*              abap_editor =
*              method_key =
              .
          CALL METHOD o_source->read_source
*            EXPORTING
*              status                        = SPACE
*              toggle_enh_status             = SPACE
*              source_name                   = space
*              initialize_edit_control       = ' '
*              load_from_temp_storage        = 'X'
*              load_from_temp_storage_uncond = SPACE
*              with_conversion               = 'X'
*              mode                          =
*              with_enhancements             = 'X'
*              insert_impl_enh_points        = ' '
*              p_current_enhname             =
             IMPORTING
               source_tab                    = t_source
*            CHANGING
*              mod_instance                  =
             EXCEPTIONS
               cancelled                     = 1
               enhancement_locked            = 2
               not_found                     = 3
               read_protected                = 4
               OTHERS                        = 5.
          IF sy-subrc <> 0.
            mac_add_obj_meth_and_raise o_source 'READ_SOURCE' sy-subrc.
          ENDIF.
*ISSUE 125 - END
* Issue 97 : Includes not handled
          LOOP AT s_fmd_fugr-includes ASSIGNING <i>.
            write_include( EXPORTING area = s_fmd_fugr-header-area " ISSUE 125
                              mainprogram = d_mainprog
                                  include = <i>
                          CHANGING source = t_source ).
          ENDLOOP.
* Issue 97 : End
          o_source->set_source_tab( t_source ).             " ISSUE 125

          LOOP AT s_fmd_fugr-functions ASSIGNING <f>.
            write_function( <f> ).
          ENDLOOP.
* Issue 137
          Commit work.      " Otherwise Pool conent is empty because inlcude UXX is not up to date

          CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
            EXPORTING
              function_pool           = s_fmd_fugr-header-area
            TABLES
              functab                 = t_functions
            EXCEPTIONS
              function_pool_not_found = 1
              OTHERS                  = 2.
          IF sy-subrc <> 0.
            ROLLBACK WORK.
            mac_add_mf_and_raise 'RS_FUNCTION_POOL_CONTENTS' sy-subrc.
          ENDIF.

          LOOP AT s_fmd_fugr-functions ASSIGNING <f>.
            READ TABLE t_functions ASSIGNING <fi>
                 WITH KEY funcname = <f>-header-name.
            IF sy-subrc <> 0.
              ROLLBACK WORK.
*              mac_add_mf_and_raise 'RS_FUNCTIONMODULE_INSERT' 12.    " CF write_function
              mac_add_mf_and_raise 'RPY_FUNCTIONMODULE_INSERT' 11.    " Issue 137
            ENDIF.
*            set_prog_rawsource( program = <fi>-include
*                                    raw = <f>-source ).
          ENDLOOP.

        WHEN OTHERS.
          mac_raise_type_not_supported me->class_name object->type.
      ENDCASE.
      check_component_list( EXPORTING     object = object
                             CHANGING components = components ). " Issue 92

    CATCH zaplink_cx_connector INTO o_mycx.
      ROLLBACK WORK.
      RAISE EXCEPTION o_mycx.
    CATCH zaplink_cx INTO o_cx.
      ROLLBACK WORK.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.
ENDCLASS.
