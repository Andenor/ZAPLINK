class ZAPLINK_PACKAGE definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.
  type-pools ABAP .
  type-pools SEOF .
  type-pools SEOK .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .
  type-pools SLIS .

  data TRANSPORT_REQUEST type E070-TRKORR .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE
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

  types TO_PACKAGE type ref to ZAPLINK_DEVC_DATA .
  types TO_INTERFACE type ref to ZAPLINK_PINF_DATA .
  types TD_PACKAGE type DEVCLASS .
  types TD_INTERFACE type SCOMIFNAM .
  types TO_PACKAGE_RAW type ref to ZAPLINK_DEVC_RAW .
  types TO_INTERFACE_RAW type ref to ZAPLINK_PINF_RAW .

  methods CONV_PACKAGE_RAW
    importing
      !PACKAGE type ref to IF_PACKAGE
      !SUBCOMPONENTS type TO_LIST
    returning
      value(O_DATA) type TO_PACKAGE_RAW
    raising
      ZAPLINK_CX_CONNECTOR .
private section.

  types:
    BEGIN OF ts_ele,
    select    TYPE flag,
    elem_type	TYPE trobjtype,
    elem_key  TYPE elemgenkey,
    td_pack  	TYPE devclass,
    elem_pack	TYPE devclass,
  END OF ts_ele .
  types:
    tt_ele TYPE STANDARD TABLE OF ts_ele WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_doc_key,                                    " equiv DOKU_OBJ = CHAR 60
    name       TYPE seoclsname,                             " CHAR 30
    other(30)  TYPE c,
  END OF ts_doc_key .
  types:
    BEGIN OF ts_clas_attr.
  INCLUDE TYPE ts_base_attributs AS base.
  TYPES:
    END OF ts_clas_attr .
  types TO_RAW_INTERFACE type ZAPLINK_DEVC_DATA=>TO_PINF_RAW .
  types TO_RAW_PACKAGE type ZAPLINK_DEVC_DATA=>TO_DEVC_RAW .

  constants ST_PACKAGE type TD_COMPTYPE value 'DEVC' ##NO_TEXT.
  constants ST_INTERFACE type TD_COMPTYPE value 'PINF' ##NO_TEXT.
  class-data CAT_ELE type SLIS_T_FIELDCAT_ALV .
  class-data CAT_LAYOUT type SLIS_LAYOUT_ALV .
  constants _UUID type TD_CONNUUID value '29AD0A4B07A5A05AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_PACKAGE' ##NO_TEXT.

  methods ADD_ELEMENTS_LIST
    importing
      !INTERFACE type TO_INTERFACE
      !ASK_USER type FLAG optional
      !ADD_REMAIN type FLAG default 'X'
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods BUILD_ERROR_MSG
    importing
      !ELEMENTS type SCOMELDATA
    returning
      value(ERRORMSG) type STRING .
  methods CHANGE_TADIR
    changing
      !ALV_DATA type TT_ELE .
  methods CONV_INTERFACE
    importing
      !INTERFACE type ref to IF_PACKAGE_INTERFACE
      !SUBCOMPONENTS type TO_LIST
    returning
      value(O_DATA) type TO_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods CONV_INTERFACE_RAW
    importing
      !INTERFACE type ref to IF_PACKAGE_INTERFACE
      !SUBCOMPONENTS type TO_LIST
    returning
      value(O_DATA) type TO_INTERFACE_RAW
    raising
      ZAPLINK_CX_CONNECTOR .
  methods CONV_PACKAGE
    importing
      !PACKAGE type ref to IF_PACKAGE
      !SUBCOMPONENTS type TO_LIST
      !WITH_SUBCOMP type TD_WITH_SUBCOMP default ABAP_TRUE
    returning
      value(O_DATA) type TO_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods CREATE_INTERFACE
    importing
      !OVERWRITE type FLAG
      !INTERFACE type TO_INTERFACE
      !COMPONENTS type TO_LIST
    raising
      ZAPLINK_CX .
  methods CREATE_PACKAGE
    importing
      !OVERWRITE type FLAG
      !PACKAGE type TO_PACKAGE
      !COMPONENTS type TO_LIST
    raising
      ZAPLINK_CX .
  class-methods ELEMENTS2ALV
    importing
      !ELEMENTS type SCOMELDATA
    returning
      value(ALV_DATA) type TT_ELE .
  methods EXPORT_FROM_SAP_INTERFACE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_FROM_SAP_PACKAGE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TO_SAP_INTERFACE
    importing
      !O_DATA type TO_INTERFACE
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TO_SAP_PACKAGE
    importing
      !O_DATA type TO_PACKAGE
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods LOAD_DEVC
    importing
      !KEY type TD_PACKAGE
    returning
      value(OBJ) type ref to IF_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods LOAD_INTF
    importing
      !KEY type TD_INTERFACE
    returning
      value(OBJ) type ref to IF_PACKAGE_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods SET_INTERFACE_ATTRIB
    importing
      !INTERFACE type TO_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods SET_PACKAGE_ATTRIB
    importing
      !PACKAGE type TO_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods UPDATE_INTERFACE
    importing
      !DATA type SCOMPIDTLN
    changing
      !INTERFACE type ref to IF_PACKAGE_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods UPDATE_PACKAGE
    importing
      !DATA type SCOMPKDTLN
    changing
      !PACKAGE type ref to IF_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _DELETE_DEVC
    importing
      !PACKAGE type ref to IF_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _DELETE_ELEMENTS
    importing
      !PACKAGE type ref to IF_PACKAGE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _DELETE_INTF
    importing
      !INTERFACE type ref to IF_PACKAGE_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
ENDCLASS.



CLASS ZAPLINK_PACKAGE IMPLEMENTATION.


  method ADD_ELEMENTS_LIST.
  DATA _err       TYPE scomeldata.
  DATA _err_lst   TYPE scomeldata.
  DATA alv_data   TYPE tt_ele.
  DATA a_data     LIKE LINE OF alv_data.
  DATA exception  TYPE REF TO zaplink_cx_connector.
  DATA title      TYPE lvc_title.
  DATA errormsg   TYPE string.
*  DATA _itf_e     LIKE LINE OF itf_elements.
  FIELD-SYMBOLS <e> LIKE LINE OF _err.
  FIELD-SYMBOLS <l> LIKE LINE OF interface->elements.

  CALL METHOD interface->if->add_elements
    EXPORTING
      i_elements_data        = interface->elements
    IMPORTING
*        e_new_elements         =
*        e_existing_elem_data   =
      e_mismatched_elem_data = _err " Should be empty if all OK
    EXCEPTIONS
      object_invalid         = 1
      intern_err             = 2
      OTHERS                 = 3
          .
  IF sy-subrc <> 0 OR NOT _err IS INITIAL.
    IF _err IS INITIAL.
      errormsg = 'Package interface elements creation failed !'(e91).
*    ELSEIF NOT add_remain IS INITIAL.
*      _itf_e-interface = interface->if.
*      _itf_e-elements = _err.
*      APPEND _itf_e TO itf_elements.
*      EXIT.
    ELSEIF sy-batch = 'X' OR ask_user IS INITIAL.
      errormsg = build_error_msg( _err ).

      CONCATENATE 'Package interface elements failed to add : '(e92) errormsg INTO errormsg.
    ELSE. " ask user
      LOOP AT _err ASSIGNING <e>.
        READ TABLE interface->elements ASSIGNING <l>
             WITH KEY intf_name = <e>-intf_name
                      elem_type = <e>-elem_type
                       elem_key = <e>-elem_key.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zaplink_cx_connector
            EXPORTING
              textid = zaplink_cx_connector=>system_error.
        ENDIF.
        <e> = <l>. " back to original value
      ENDLOOP.
      alv_data = elements2alv( _err ).
      a_data-elem_pack = interface->if->publisher_package_name.
      MODIFY alv_data FROM a_data TRANSPORTING elem_pack WHERE elem_pack IS INITIAL.   "update package
      title = 'Inconsistant TADIR entries please select elements that will change package. All unselected entries won''t be in this interface'(alv).
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
*           I_INTERFACE_CHECK                 = ' '
*           I_BYPASSING_BUFFER                = ' '
*           I_BUFFER_ACTIVE                   = ' '
*           I_CALLBACK_PROGRAM                = ' '
*           I_CALLBACK_PF_STATUS_SET          = ' '
*           I_CALLBACK_USER_COMMAND           = ' '
*           I_CALLBACK_TOP_OF_PAGE            = ' '
*           I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*           I_CALLBACK_HTML_END_OF_LIST       = ' '
*           I_STRUCTURE_NAME                  =
*           I_BACKGROUND_ID                   = ' '
          i_grid_title                      = title
*           I_GRID_SETTINGS                   =
          is_layout                         = cat_layout
          it_fieldcat                       = cat_ele
*           IT_EXCLUDING                      =
*           IT_SPECIAL_GROUPS                 =
*           IT_SORT                           =
*           IT_FILTER                         =
*           IS_SEL_HIDE                       =
*           I_DEFAULT                         = 'X'
*           I_SAVE                            = ' '
*           IS_VARIANT                        =
*           IT_EVENTS                         =
*           IT_EVENT_EXIT                     =
*           IS_PRINT                          =
*           IS_REPREP_ID                      =
*           I_SCREEN_START_COLUMN             = 0
*           I_SCREEN_START_LINE               = 0
*           I_SCREEN_END_COLUMN               = 0
*           I_SCREEN_END_LINE                 = 0
*           I_HTML_HEIGHT_TOP                 = 0
*           I_HTML_HEIGHT_END                 = 0
*           IT_ALV_GRAPHICS                   =
*           IT_HYPERLINK                      =
*           IT_ADD_FIELDCAT                   =
*           IT_EXCEPT_QINFO                   =
*           IR_SALV_FULLSCREEN_ADAPTER        =
*         IMPORTING
*           E_EXIT_CAUSED_BY_CALLER           =
*           ES_EXIT_CAUSED_BY_USER            =
        TABLES
          t_outtab                          = alv_data
        EXCEPTIONS
         program_error                     = 1
         OTHERS                            = 2.
      IF sy-subrc <> 0.
        errormsg = build_error_msg( _err ).

        CONCATENATE 'Package interface elements failed to add : '(e92) errormsg INTO errormsg.
      ELSE.
        change_tadir( CHANGING alv_data = alv_data ).
        LOOP AT _err ASSIGNING <e>.
          CLEAR a_data.
          READ TABLE alv_data INTO a_data WITH KEY elem_type = <e>-elem_type
                                                    elem_key = <e>-elem_key.
          IF sy-subrc <> 0 OR a_data-select IS INITIAL.
            APPEND <e> TO _err_lst.
            DELETE _err.
          ENDIF.
        ENDLOOP.

        TRY.
            interface->elements = _err.
            CALL METHOD add_elements_list
              EXPORTING
                interface	= interface
*                list      = _err
*                ask_user =
*                add_remain = add_remain
                .
          CATCH zaplink_cx_connector INTO exception.
            RAISE EXCEPTION exception.
        ENDTRY.
        IF _err_lst IS INITIAL.
          EXIT. " if every thing is OK
        ELSE.
          errormsg = build_error_msg( _err_lst ).

          CONCATENATE 'Package interface elements failed to add : '(e92) errormsg INTO errormsg.
        ENDIF.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid = zaplink_cx_connector=>system_error
       cx_name = errormsg.
  ENDIF.

  CALL METHOD interface->if->save_elements
    EXPORTING
      i_transport_request   = transport_request
    IMPORTING
      e_transport_request   = transport_request
    EXCEPTIONS
      cancelled_in_corr     = 1
      intern_err            = 2
      object_invalid        = 3
      object_not_changeable = 4
      permission_failure    = 5
      unexpected_error      = 6
      OTHERS                = 7.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_method_cx( class = interface->if
                                                method = 'SAVE_ELEMENTS'
                                                 subrc = sy-subrc
                                          cx_classname = 'ZAPLINK_CX_CONNECTOR' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.
  endmethod.


  method BUILD_ERROR_MSG.
  DATA obj_key    TYPE string.
  FIELD-SYMBOLS:
    <e> LIKE LINE OF elements.

  LOOP AT elements ASSIGNING <e>.
    CONCATENATE <e>-elem_key '(' <e>-elem_type ')' INTO obj_key.
    IF errormsg IS INITIAL.
      errormsg = obj_key.
    ELSE.
      CONCATENATE errormsg ', ' obj_key INTO errormsg.
    ENDIF.
  ENDLOOP.
  endmethod.


  method CHANGE_TADIR.
  DATA object   TYPE tadir-object.
  DATA obj_name TYPE tadir-obj_name.
  DATA korrnum  TYPE tadir-korrnum.
  DATA d_kind   TYPE tadir-pgmid.
  FIELD-SYMBOLS:
    <e> LIKE LINE OF alv_data.

  korrnum = transport_request.
  LOOP AT alv_data ASSIGNING <e>
          WHERE NOT select IS INITIAL.
    object = <e>-elem_type.
    obj_name = <e>-elem_key.
    d_kind = zaplink_connectors=>get_typekind( object ).
    CALL FUNCTION 'TRINT_TADIR_INTERFACE'
      EXPORTING
*       WI_DELETE_TADIR_ENTRY                = ' '
*       WI_REMOVE_REPAIR_FLAG                = ' '
*       WI_SET_REPAIR_FLAG                   = ' '
        wi_test_modus                        = ' '
        wi_tadir_pgmid                       = d_kind
        wi_tadir_object                      = object
        wi_tadir_obj_name                    = obj_name
        wi_tadir_korrnum                     = korrnum
*       WI_TADIR_SRCSYSTEM                   = ' '
*       WI_TADIR_AUTHOR                      = ' '
        wi_tadir_devclass                    = <e>-elem_pack
*       WI_TADIR_MASTERLANG                  = ' '
*       WI_TADIR_CPROJECT                    = ' '
*       WI_TADIR_VERSID                      = ' '
*       WI_REMOVE_GENFLAG                    = ' '
*       WI_SET_GENFLAG                       = ' '
*       WI_READ_ONLY                         = ' '
*       IV_SET_EDTFLAG                       = ' '
*       IV_WBO_INTERNAL                      = ' '
*       IV_INSERT_MODE                       = ' '
*       IV_TRANSL_TECH_TEXT                  = ' '
*       IV_DELFLAG                           = ' '
*       IV_NO_PAK_CHECK                      = ' '
*       IV_OBJ_STABILITY                     = ' '
*       IV_TRANSL_TECH_TEXT_SUPP             = ' '
*       IV_DELFLAG_SUPP                      = ''
*       IV_NO_PAK_CHECK_SUPP                 = ' '
*       IV_OBJ_STABILITY_SUPP                = ' '
*     IMPORTING
*       NEW_GTADIR_ENTRY                     =
*       NEW_TADIR_ENTRY                      =
     EXCEPTIONS
        tadir_entry_not_existing             = 1
        tadir_entry_ill_type                 = 2
        no_systemname                        = 3
        no_systemtype                        = 4
        original_system_conflict             = 5
        object_reserved_for_devclass         = 6
        object_exists_global                 = 7
        object_exists_local                  = 8
        object_is_distributed                = 9
        obj_specification_not_unique         = 10
        no_authorization_to_delete           = 11
        devclass_not_existing                = 12
        simultanious_set_remove_repair       = 13
        order_missing                        = 14
        no_modification_of_head_syst         = 15
        pgmid_object_not_allowed             = 16
        masterlanguage_not_specified         = 17
        devclass_not_specified               = 18
        specify_owner_unique                 = 19
        loc_priv_objs_no_repair              = 20
        gtadir_not_reached                   = 21
        object_locked_for_order              = 22
        change_of_class_not_allowed          = 23
        no_change_from_sap_to_tmp            = 24
        OTHERS                               = 25
              .
    IF sy-subrc <> 0.
      CLEAR <e>-select.
    ENDIF.
  ENDLOOP.
  endmethod.


  method CLASS_CONSTRUCTOR.
* ALV Catalog
  DATA ls_fieldcat  LIKE LINE OF  cat_ele.
*** Display list to select the objects for downloading
  ls_fieldcat-fieldname = 'ELEM_TYPE'.
  ls_fieldcat-seltext_l = 'Object Type'(c01).
  APPEND ls_fieldcat TO cat_ele.

  ls_fieldcat-fieldname = 'ELEM_KEY'.
  ls_fieldcat-seltext_l = 'Object Name'(c02).
  APPEND ls_fieldcat TO cat_ele.

  ls_fieldcat-fieldname = 'TD_PACK'.
  ls_fieldcat-seltext_l = 'TADIR Package'(c03).
  APPEND ls_fieldcat TO cat_ele.

  ls_fieldcat-fieldname = 'ELEM_PACK'.
  ls_fieldcat-seltext_s = 'New package'(c04).
  ls_fieldcat-seltext_l = 'New package when validated'(l04).
  APPEND ls_fieldcat TO cat_ele.

  cat_layout-box_fieldname     = 'SELECT'.
*    cat_layout-lights_fieldname  = 'TD_PACK'.
  cat_layout-colwidth_optimize = 'X'.
*  ls_layout-f2code            = 'MYPICK' .
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  call method super->constructor.
  mac_create_log application_log ballog_subobject space.      " create my application before parent do the same
  application_log->msgid = 'ZAPLINK_PACKAGE'.
  type-type = st_package. INSERT type INTO TABLE supported_types.
  type-type = st_interface. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method CONV_INTERFACE.
  DATA elements   TYPE tpak_package_interf_elem_list.
  DATA elem       LIKE LINE OF o_data->elements.
  DATA _component  TYPE to_component.
  FIELD-SYMBOLS:
    <e> LIKE LINE OF elements.

  TRY.

    CREATE OBJECT o_data.
    CALL METHOD interface->get_all_attributes
      IMPORTING
        e_package_interface_data = o_data->a0_maindata
      EXCEPTIONS
        object_invalid           = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zaplink_cx_connector
        EXPORTING
          textid  = zaplink_cx_connector=>system_error
          cx_name = 'error in interface->get_all_attributes'.
    ENDIF.

    CREATE OBJECT _component.
    _component->set_type( st_interface ).
    _component->set_name( o_data->a0_maindata-intf_name ).
    subcomponents->add( _component ).

* Elements
    CALL METHOD interface->get_elements
      EXPORTING
        i_with_deleted_elements = 'X'
      IMPORTING
        e_elements              = elements
      EXCEPTIONS
        object_invalid          = 1
        intern_err              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zaplink_cx_connector
        EXPORTING
          textid  = zaplink_cx_connector=>system_error
          cx_name = 'interface->get_elements'.
    ELSEIF NOT elements IS INITIAL.
      LOOP AT elements ASSIGNING <e>.
        CALL METHOD <e>->get_all_attributes
          IMPORTING
            e_element_data = elem
          EXCEPTIONS
            object_invalid = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zaplink_cx_connector
            EXPORTING
              textid  = zaplink_cx_connector=>system_error
              cx_name = 'error in interface->get_all_attributes'.
        ENDIF.
        APPEND elem TO o_data->elements.
      ENDLOOP.
    ENDIF.

* ACLS
    IF NOT o_data->a0_maindata-acl_flag IS INITIAL.
      CALL METHOD interface->get_access_control_list
        IMPORTING
          e_user_list    = o_data->acls
        EXCEPTIONS
          object_invalid = 1
          no_acl         = 0
          intern_err     = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid  = zaplink_cx_connector=>system_error
            cx_name = 'interface->get_access_control_list'.
      ENDIF.
    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method CONV_INTERFACE_RAW.
  DATA o_comp     TYPE to_component.
  DATA _data      TYPE to_interface.
  DATA _cx        TYPE REF TO zaplink_cx.

  TRY.
      _data = conv_interface( interface = interface
                          subcomponents = subcomponents ).
      CREATE OBJECT o_comp.
      o_comp->set_type( st_interface ).
      o_comp->set_name( _data->a0_maindata-intf_name ).
      CREATE OBJECT o_data
        EXPORTING
          o_comp = o_comp.
      o_data->raw = _data.
    CATCH zaplink_cx INTO _cx.
      CREATE OBJECT o_mycx
        EXPORTING
          textid   = zaplink_cx=>system_error
          previous = _cx.
      o_mycx->update( ).
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method CONV_PACKAGE.
  DATA perm_data   LIKE LINE OF o_data->acls.
  DATA permissions TYPE tpak_permission_to_use_list.
  DATA interfaces  TYPE tpak_package_interface_list.
  DATA intf        LIKE LINE OF o_data->interfaces.
  DATA packages    TYPE scompaklis.
  DATA pack        LIKE LINE OF o_data->packages.
  DATA _component  TYPE to_component.
  FIELD-SYMBOLS:
    <pack> LIKE LINE OF packages,
    <itf> LIKE LINE OF interfaces,
    <p> LIKE LINE OF permissions.

  TRY.

    CLEAR o_mycx.
    CREATE OBJECT o_data.
    CALL METHOD package->get_all_attributes
      IMPORTING
        e_package_data  = o_data->a0_maindata
      EXCEPTIONS
        object_invalid  = 1
        package_deleted = 2
        intern_err      = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zaplink_cx_connector
        EXPORTING
          textid  = zaplink_cx_connector=>system_error
          cx_name = 'error in package->get_all_attributes'.
    ENDIF.

* Issue 132
    CLEAR: o_data->a0_maindata-dlvu_text, o_data->a0_maindata-component, o_data->a0_maindata-comp_text,
           o_data->a0_maindata-namespace, o_data->a0_maindata-layer_text.

    CREATE OBJECT _component.
    _component->set_type( st_package ).
    _component->set_name( o_data->a0_maindata-devclass ).
    subcomponents->add( _component ).

* ACLs
    CALL METHOD package->get_permissions_to_use
      IMPORTING
        e_permissions    = permissions
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zaplink_cx_connector
        EXPORTING
          textid  = zaplink_cx_connector=>system_error
          cx_name = 'package->get_permissions_to_use'.
    ELSEIF NOT permissions IS INITIAL.
      LOOP AT permissions ASSIGNING <p>.
        CHECK <p>->deleted_in_memory IS INITIAL.
        perm_data-intf_name = <p>->package_interface_name.
        perm_data-err_sever = <p>->error_severity.
        APPEND perm_data TO o_data->acls.
      ENDLOOP.
    ENDIF.

    IF with_subcomp = abap_true.
* Interfaces
      CALL METHOD package->get_interfaces
        IMPORTING
          e_package_interfaces = interfaces
        EXCEPTIONS
          object_invalid       = 1
          unexpected_error     = 2
          intern_err           = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid = zaplink_cx_connector=>system_error.
      ELSEIF NOT interfaces IS INITIAL.
        LOOP AT interfaces ASSIGNING <itf>.
          TRY.
              intf-zl_object = conv_interface_raw( interface = <itf>
                                               subcomponents = subcomponents ).
            CATCH zaplink_cx_connector INTO o_mycx.
              application_log->add_exception( o_mycx ).
          ENDTRY.
          APPEND intf TO o_data->interfaces.
        ENDLOOP.
      ENDIF.

* Packages
      CALL METHOD package->get_sub_packages
        IMPORTING
          e_sub_packages   = packages
        EXCEPTIONS
          object_invalid   = 1
          leaf_package     = 0   " 053(PAK) : Package &1 does not contain any sub-packages
          unexpected_error = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid = zaplink_cx_connector=>system_error.
      ELSEIF NOT packages IS INITIAL.
        LOOP AT packages ASSIGNING <pack>.
          TRY.
              pack-zl_object = conv_package_raw( package = <pack>
                                           subcomponents = subcomponents ).
            CATCH zaplink_cx_connector INTO o_mycx.
              application_log->add_exception( o_mycx ).
          ENDTRY.
          APPEND pack TO o_data->packages.
        ENDLOOP.
      ENDIF.

    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.

  IF o_mycx IS BOUND.   RAISE EXCEPTION o_mycx.   ENDIF.
  endmethod.


  method CONV_PACKAGE_RAW.
  DATA o_comp     TYPE to_component.
  DATA _data      TYPE to_package.
  DATA _cx        TYPE REF TO zaplink_cx.

  TRY.
      _data = conv_package( package = package
                      subcomponents = subcomponents ).
      CREATE OBJECT o_comp.
      o_comp->set_type( st_package ).
      o_comp->set_name( _data->a0_maindata-devclass ).
      CREATE OBJECT o_data
        EXPORTING
          o_comp = o_comp.
      o_data->raw = _data.
      CLEAR: _data->a0_maindata-devclass, _data->a0_maindata-parentcl. " Issue 48
    CATCH zaplink_cx INTO _cx.
      CREATE OBJECT o_mycx
        EXPORTING
          textid   = zaplink_cx=>system_error
          previous = _cx.
      o_mycx->update( ).
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method CREATE_INTERFACE.
  DATA _component      TYPE to_component.
  DATA name       TYPE td_interface.
  DATA transport_request  TYPE  e070-trkorr.

  name = interface->a0_maindata-intf_name.
* Processing Interface &1
  SET EXTENDED CHECK OFF.
  IF 1 = 2. MESSAGE e013 WITH name. ENDIF.
  SET EXTENDED CHECK ON.
  application_log->add_info( id_msgno = 013
                             id_msgv1 = name ).

* Done in method S_LOCK_DB de CL_PACKAGE_INTERFACE
*  CALL FUNCTION 'RS_ACCESS_PERMISSION'
*  CALL FUNCTION 'RS_CORR_INSERT'

*  TRY.
    CALL METHOD update_interface
      EXPORTING
        data      = interface->a0_maindata
      CHANGING
        interface = interface->if.

*    IF NOT interface->if IS BOUND.
*      RAISE EXCEPTION TYPE zaplink_cx_connector
*        EXPORTING
*          textid  = zaplink_cx_connector=>system_error
*          cx_name = 'Failed to create package interface'.
*    ENDIF.

    CALL METHOD interface->if->save
      EXPORTING
        i_transport_request   = transport_request
      IMPORTING
        e_transport_request   = transport_request
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( class = interface->if
                                                  method = 'SAVE'
                                                   subrc = sy-subrc
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR' ).
      RAISE EXCEPTION o_mycx.
    ELSE.
      CREATE OBJECT _component.
      _component->set_type( st_interface ).
      _component->set_name( name ).
      components->add( _component ).
    ENDIF.

*    mac_def_catch zaplink_cx_connector.
*  ENDTRY.
  endmethod.


  method CREATE_PACKAGE.
  DATA name               TYPE td_package.
  DATA transport_request  TYPE  e070-trkorr.
  DATA _component         TYPE to_component.
  DATA o_intf             TYPE to_interface.
  DATA o_package          TYPE to_package.
  FIELD-SYMBOLS:
    <p> LIKE LINE OF package->packages,
    <i> LIKE LINE OF package->interfaces.

  name = package->a0_maindata-devclass.
* Processing Package &1
  SET EXTENDED CHECK OFF.
  IF 1 = 2. MESSAGE e003 WITH space. ENDIF.
  SET EXTENDED CHECK ON.
  application_log->add_info( id_msgno = 003
                             id_msgv1 = name ).

* Done in method S_LOCK_DB de CL_PACKAGE
*  CALL FUNCTION 'RS_ACCESS_PERMISSION'
*  CALL FUNCTION 'RS_CORR_INSERT'

*  TRY.
    CALL METHOD update_package
      EXPORTING
        data    = package->a0_maindata
      CHANGING
        package = package->if.

*    IF NOT package->if IS BOUND.
*      RAISE EXCEPTION TYPE zaplink_cx_connector
*        EXPORTING
*          textid  = zaplink_cx_connector=>system_error
*          cx_name = 'Failed to create package'.
*    ENDIF.

    CALL METHOD package->if->save
      EXPORTING
        i_transport_request   = transport_request
      IMPORTING
        e_transport_request   = transport_request
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7.
    IF sy-subrc <> 0.
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e009 WITH space. ENDIF.
      SET EXTENDED CHECK ON.
* Can't save package &1
      application_log->add_error( id_msgno = 009
                                  id_msgv1 = name ).
      application_log->add_symsg( ).
*      o_mycx ?= zaplink_cx=>create_from_method_cx( class = interface->if
*                                                  method = 'SAVE'
*                                                   subrc = sy-subrc
*                                            cx_classname = 'ZAPLINK_CX_CONNECTOR' ).
*      RAISE EXCEPTION o_mycx.
    ELSE.
      CREATE OBJECT _component.
      _component->set_type( st_package ).
      _component->set_name( name ).
      components->add( _component ).
    ENDIF.

    LOOP AT package->interfaces ASSIGNING <i>.
      o_intf ?= <i>-zl_object->raw.
      CALL METHOD create_interface
        EXPORTING
          interface  = o_intf
          overwrite  = overwrite
          components = components.
    ENDLOOP.

    LOOP AT package->packages ASSIGNING <p>.
      o_package ?= <p>-zl_object->raw.
      o_package->a0_maindata-devclass = <p>-zl_object->name.    o_package->a0_maindata-parentcl = package->a0_maindata-devclass. " Issue 48
      CALL METHOD create_package
        EXPORTING
          package    = o_package
          overwrite  = overwrite
          components = components.
    ENDLOOP.

*    mac_def_catch zaplink_cx_connector.
*  ENDTRY.
  endmethod.


  method ELEMENTS2ALV.
  DATA _wa     LIKE LINE OF alv_data.
  DATA d_kind  TYPE td_transport_kind.
  FIELD-SYMBOLS: <e> LIKE LINE OF elements.

  LOOP AT elements ASSIGNING <e>.
    MOVE-CORRESPONDING <e> TO _wa.
    d_kind = zaplink_connectors=>get_typekind( _wa-elem_type ).
    SELECT SINGLE devclass
      INTO _wa-td_pack
      FROM tadir
      WHERE pgmid = d_kind
        AND object = _wa-elem_type
        AND obj_name = _wa-elem_key.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    IF _wa-td_pack = _wa-elem_pack. CONTINUE. ENDIF.
    IF _wa-td_pack = '$TMP'. _wa-select = abap_true. ENDIF.
    APPEND _wa TO alv_data.
  ENDLOOP.
  endmethod.


  method EXPORT_FROM_SAP_INTERFACE.
  DATA sap_obj   TYPE REF TO if_package_interface.
  DATA name       TYPE td_interface.
  DATA o_list     TYPE to_list.

  TRY.

    CHECK component->get_type( ) = st_interface.

    name = component->get_name( ).
    CREATE OBJECT o_list.
    component->set_subcomponents( o_list ).

    sap_obj = load_intf( name ).

    object = conv_interface( interface = sap_obj
                         subcomponents = o_list ).

    o_list->remove( component ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_FROM_SAP_PACKAGE.
  DATA sap_obj    TYPE REF TO if_package.
  DATA devclass	  TYPE devclass.
  DATA o_list     TYPE to_list.
  DATA f_subcomp  TYPE td_with_subcomp.

  TRY.

    CHECK component->get_type( ) = st_package.
    f_subcomp = component->get_with_subcomp( ).

    devclass = component->get_name( ).

    CREATE OBJECT o_list.
    component->set_subcomponents( o_list ).

    sap_obj = load_devc( devclass ).

    object = conv_package( package = sap_obj
                      with_subcomp = f_subcomp
                     subcomponents = o_list ).

    o_list->remove( component ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TO_SAP_INTERFACE.

  TRY.

    CREATE OBJECT components.
* Create bulk package
    create_interface( overwrite = 'X'
                      interface = o_data
                     components = components ).

    set_interface_attrib( interface = o_data ).

    application_log->raise_on_error( ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TO_SAP_PACKAGE.

  TRY.
    CREATE OBJECT components.
* Create bulk package
    create_package( overwrite = 'X'
                      package = o_data
                   components = components ).

    set_package_attrib( package = o_data ).

    application_log->raise_on_error( ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method LOAD_DEVC.
  CALL METHOD cl_package=>if_package~load_package
    EXPORTING
      i_package_name             = key
      i_force_reload             = 'X'
    IMPORTING
      e_package                  = obj
    EXCEPTIONS
      object_not_existing        = 1
      unexpected_error           = 2
      intern_err                 = 3
      object_locked_and_modified = 4
      OTHERS                     = 5.
  IF sy-subrc <> 0.
    CLEAR obj.
    CASE sy-subrc.
*      WHEN 1.
** Package &1 Not found
*        IF 1 = 2. MESSAGE ID 'ZAPLINK_PACKAGE' TYPE 'E' NUMBER 006. ENDIF.
*        application_log->add_warning( id_msgno = 006
*                     id_msgv1 = key ).
*        RAISE EXCEPTION TYPE ZAPLINK_CX_CONNECTOR
*          EXPORTING
*            textid = ZAPLINK_CX_CONNECTOR=>not_found.
*      WHEN 4.
** Package &1 is locked
*        IF 1 = 2. MESSAGE ID 'ZAPLINK_PACKAGE' TYPE 'E' NUMBER 007. ENDIF.
*        application_log->add_warning( id_msgno = 007
*                     id_msgv1 = key ).
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>error_message
*            msg = 'Object Locked'.
      WHEN OTHERS.
* Unexpected error on &1->&2
        SET EXTENDED CHECK OFF.
        IF 1 = 2. MESSAGE e008 WITH space. ENDIF.
        SET EXTENDED CHECK ON.
        application_log->add_warning( id_msgno = 008
                     id_msgv1 = 'ZAPLINK_PACKAGE'
                     id_msgv2 = 'LOAD_DEVC' ).
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid = zaplink_cx_connector=>system_error.
    ENDCASE.
  ENDIF.
  endmethod.


  method LOAD_INTF.
  CALL METHOD cl_package_interface=>if_package_interface~load_package_interface
    EXPORTING
      i_package_interface_name   = key
      i_force_reload             = 'X'
    IMPORTING
      e_package_interface        = obj
    EXCEPTIONS
      db_read_error              = 1
      unexpected_error           = 2
      object_not_existing        = 3
      shorttext_not_existing     = 4
      object_locked_and_modified = 5
      OTHERS                     = 6.
  IF sy-subrc <> 0.
    CLEAR obj.
    CASE sy-subrc.
*      WHEN 3.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>error_message
*            msg = 'Object Not found'.
*      WHEN 5.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>error_message
*            msg = 'Object Locked'.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid = ZAPLINK_CX_CONNECTOR=>system_error.
    ENDCASE.
  ENDIF.
  endmethod.


  method SET_INTERFACE_ATTRIB.
  DATA name       TYPE scomifnam.
  DATA csubrc     TYPE string.
  DATA transport_request  TYPE  e070-trkorr.

  name = interface->a0_maindata-intf_name.
* Processing Interface &1 attributs (ACLs, Elements, ...)
  SET EXTENDED CHECK OFF.
  IF 1 = 2. MESSAGE i014 WITH space. ENDIF.
  SET EXTENDED CHECK ON.
  application_log->add_info( id_msgno = 014
                             id_msgv1 = name ).

  IF NOT interface->if IS BOUND.
* System Error : Object '&3' is not bound in method &1->&2
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE a016 WITH space space space. ENDIF.
    SET EXTENDED CHECK ON.
    application_log->add_abend( id_msgno = 016
                                id_msgv1 = 'ZAPLINK_PACKAGE'
                                id_msgv2 = 'SET_INTERFACE_ATTRIB'
                                id_msgv3 = 'interface->if'
*                               id_msgv4 =
*                           id_detlevel  =
                            id_probclass = '1' ).
    EXIT.
  ENDIF.

  IF NOT interface->acls IS INITIAL.
    CALL METHOD interface->if->add_access_control_elements
      EXPORTING
        i_user_list            = interface->acls
      EXCEPTIONS
        no_acl                 = 1
        entry_already_existing = 2
        object_not_changeable  = 3
        object_invalid         = 4
        local_package          = 5
        intern_err             = 6
        OTHERS                 = 7.
    IF sy-subrc <> 0.
      csubrc = sy-subrc.
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e017 WITH space space space space. ENDIF.
      SET EXTENDED CHECK ON.
      application_log->add_error( id_msgno = 017
                                  id_msgv1 = 'ZAPLINK_PACKAGE'
                                  id_msgv2 = 'SET_INTERFACE_ATTRIB'
                                  id_msgv3 = 'add_access_control_elements'
                                  id_msgv4 = csubrc ).
    ENDIF.
  ENDIF.

  TRY.
      CALL METHOD add_elements_list
        EXPORTING
          interface	 = interface
*          list       = interface->elements
          ask_user   = abap_true
          add_remain = abap_true.
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.

  CALL METHOD interface->if->save
    EXPORTING
      i_transport_request   = transport_request
    IMPORTING
      e_transport_request   = transport_request
    EXCEPTIONS
      object_invalid        = 1
      object_not_changeable = 2
      cancelled_in_corr     = 3
      permission_failure    = 4
      unexpected_error      = 5
      intern_err            = 6
      OTHERS                = 7.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_method_cx( class = interface->if
                                                method = 'SAVE'
                                                 subrc = sy-subrc
                                          cx_classname = 'ZAPLINK_CX_CONNECTOR' ).
    RAISE EXCEPTION o_mycx.
*    RAISE EXCEPTION TYPE zaplink_cx_connector
*      EXPORTING
*        textid  = zaplink_cx_connector=>system_error
*        cx_name = 'Package interface save failed !'.
  ENDIF.
  endmethod.


  method SET_PACKAGE_ATTRIB.
  DATA name       TYPE scomifnam.
  DATA csubrc     TYPE string.
  DATA o_intf     TYPE to_interface.
  DATA o_package  TYPE to_package.
  FIELD-SYMBOLS:
    <a> LIKE LINE OF package->acls,
    <p> LIKE LINE OF package->packages,
    <i> LIKE LINE OF package->interfaces.

  name = package->a0_maindata-devclass.
* Processing Package &1 attributs
  SET EXTENDED CHECK OFF.
  IF 1 = 2. MESSAGE e015 WITH space. ENDIF.
  SET EXTENDED CHECK ON.
  application_log->add_info( id_msgno = 015
                             id_msgv1 = name ).

  IF NOT package->if IS BOUND.
* System Error : Object '&3' is not bound in method &1->&2
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE e016 WITH space space space. ENDIF.
    SET EXTENDED CHECK ON.
    application_log->add_abend( id_msgno = 016
                                id_msgv1 = 'ZAPLINK_PACKAGE'
                                id_msgv2 = 'SET_PACKAGE_ATTRIB'
                                id_msgv3 = 'package->if'
*                                id_msgv4 =
*                             id_detlevel =
                                id_probclass = '1' ).
    EXIT.
  ENDIF.

  LOOP AT package->acls ASSIGNING <a>.
    CALL METHOD package->if->add_permission_to_use
      EXPORTING
        i_pkg_permission_data   = <a>
*      IMPORTING
*        e_pkg_permission        =
      EXCEPTIONS
        object_not_changeable   = 1
        object_access_error     = 2
        object_already_existing = 3
        object_invalid          = 4
        unexpected_error        = 5
        OTHERS                  = 6
            .
    IF sy-subrc <> 0.
      csubrc = sy-subrc.
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e017 WITH space space space space. ENDIF.
      SET EXTENDED CHECK ON.
      application_log->add_error( id_msgno = 017
                                  id_msgv1 = 'ZAPLINK_PACKAGE'
                                  id_msgv2 = 'SET_PACKAGE_ATTRIB'
                                  id_msgv3 = 'add_permission_to_use'
                                  id_msgv4 = csubrc ).
    ENDIF.
  ENDLOOP.

  LOOP AT package->interfaces ASSIGNING <i>.
    TRY.
        o_intf ?= <i>-zl_object->raw.
        CALL METHOD set_interface_attrib
          EXPORTING
            interface = o_intf.
      CATCH zaplink_cx_connector INTO o_mycx.
        RAISE EXCEPTION o_mycx.
    ENDTRY.
  ENDLOOP.

  LOOP AT package->packages ASSIGNING <p>.
    TRY.
        o_package ?= <p>-zl_object->raw.
        CALL METHOD set_package_attrib
          EXPORTING
            package = o_package.
      CATCH zaplink_cx_connector INTO o_mycx.
        RAISE EXCEPTION o_mycx.
    ENDTRY.
  ENDLOOP.
  endmethod.


  method UPDATE_INTERFACE.
  DATA _data      TYPE scompidtln.
  DATA _data_f    TYPE scompisign.

* Done in method S_LOCK_DB de CL_PACKAGE
*  CALL FUNCTION 'RS_ACCESS_PERMISSION'
*  CALL FUNCTION 'RS_CORR_INSERT'

  _data = data.

  IF NOT interface IS BOUND.        " Issue 106 : Check if interface exists
    CALL METHOD cl_package_interface=>if_package_interface~load_package_interface
      EXPORTING
        i_package_interface_name   = data-intf_name
*        i_force_reload             =
      IMPORTING
        e_package_interface        = interface
      EXCEPTIONS
        db_read_error              = 1
        object_locked_and_modified = 2
        object_not_existing        = 0
        shorttext_not_existing     = 4
        unexpected_error           = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                              class_name = 'CL_PACKAGE_INTERFACE'
                                                  method = 'IF_PACKAGE_INTERFACE~LOAD_PACKAGE_INTERFACE'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ENDIF.

  IF interface IS BOUND.
    _data_f = sy-uline.
    CALL METHOD interface->set_changeable
      EXPORTING
        i_changeable                = 'X'
      EXCEPTIONS
        object_already_changeable   = 0
        object_already_unlocked     = 0
        object_deleted              = 3
        object_invalid              = 4
        object_just_created         = 5
        object_locked_by_other_user = 6
        object_modified             = 7
        object_not_existing         = 8
        permission_failure          = 9
        unexpected_error            = 10
        OTHERS                      = 11.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                                   class = interface
                                                  method = 'set_changeable'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
*      CASE sy-subrc.
**      WHEN 1.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>Lock_error.
**        WHEN 2.
**          RAISE EXCEPTION TYPE zaplink_cx_connector
**            EXPORTING
**              textid = zaplink_cx_connector=>not_authorized.
*        WHEN OTHERS.
*          RAISE EXCEPTION TYPE zaplink_cx_connector
*            EXPORTING
*              textid = zaplink_cx_connector=>system_error.
*      ENDCASE.
    ENDIF.
    CALL METHOD interface->set_all_attributes
      EXPORTING
        i_package_interface_data = _data
        i_data_sign              = _data_f
      EXCEPTIONS
        acl_not_empty            = 1
        author_not_existing      = 2
        interface_not_empty      = 3
        object_deleted           = 4
        object_invalid           = 5
        object_not_changeable    = 6
        object_type_mismatch     = 7
        OTHERS                   = 8.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                                   class = interface
                                                  method = 'set_all_attributes'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ELSE.
    CALL METHOD cl_package_interface=>if_package_interface~create_new_package_interface
      EXPORTING
*        i_reuse_deleted_object  = 'X'
        i_pkg_interface_name    = _data-intf_name
        i_publisher_pkg_name    = _data-pack_name
        i_pkg_interface_data    = _data
      IMPORTING
        e_package_interface     = interface
      EXCEPTIONS
        interface_name_invalid  = 1
        no_changes_allowed      = 2
        object_already_existing = 3
        object_just_created     = 4
        unexpected_error        = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'cl_package_interface'
                                                       method = 'if_package_interface~create_new_package_interface'
                                                        subrc = sy-subrc
                                                 cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ENDIF.

*  IF sy-subrc <> 0.
*    CASE sy-subrc.
**      WHEN 2.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>not_authorized.
**      WHEN 103.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>error_message
**            msg = 'Problem with package interface name'.
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>system_error.
*    ENDCASE.
*  ENDIF.
  endmethod.


  method UPDATE_PACKAGE.
  DATA _data      TYPE scompkdtln.
  DATA _data_f    TYPE scompksign.

* Done in method S_LOCK_DB de CL_PACKAGE
*  CALL FUNCTION 'RS_ACCESS_PERMISSION'
*  CALL FUNCTION 'RS_CORR_INSERT'

  _data = data.

  IF NOT package IS BOUND.        " Issue 106 : Check if package exists
    CALL METHOD cl_package=>if_package~load_package
      EXPORTING
        i_package_name             = data-devclass
*        i_force_reload             =
      IMPORTING
        e_package                  = package
      EXCEPTIONS
        intern_err                 = 1
        object_locked_and_modified = 2
        object_not_existing        = 0
        unexpected_error           = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                              class_name = 'CL_PACKAGE'
                                                  method = 'if_package~load_package'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ENDIF.

  IF package IS BOUND.
    _data_f = sy-uline.
    CALL METHOD package->set_changeable
      EXPORTING
        i_changeable                = 'X'
      EXCEPTIONS
        object_already_changeable   = 0
        object_already_unlocked     = 0
        object_deleted              = 3
        object_invalid              = 4
        object_just_created         = 5
        object_locked_by_other_user = 6
        object_modified             = 7
        object_not_existing         = 8
        permission_failure          = 9
        unexpected_error            = 10
        OTHERS                      = 11.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                                   class = package
                                                  method = 'set_changeable'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
*      CASE sy-subrc.
**      WHEN 1.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>Lock_error.
**        WHEN 2.
**          RAISE EXCEPTION TYPE zaplink_cx_connector
**            EXPORTING
**              textid = zaplink_cx_connector=>not_authorized.
*        WHEN OTHERS.
*          RAISE EXCEPTION TYPE zaplink_cx_connector
*            EXPORTING
*              textid = zaplink_cx_connector=>system_error.
*      ENDCASE.
    ENDIF.

    CALL METHOD package->set_all_attributes
      EXPORTING
        i_package_data             = _data
        i_data_sign                = _data_f
      EXCEPTIONS
        authorize_failure          = 1
        author_not_existing        = 99
        component_not_existing     = 3
        component_missing          = 4
        intern_err                 = 5
        korrflag_invalid           = 6
        layer_invalid              = 7
        local_package              = 8
        object_deleted             = 9
        object_invalid             = 10
        object_not_changeable      = 11
        prefix_in_use              = 12
        short_text_missing         = 13
        software_component_invalid = 14
        unexpected_error           = 15
        OTHERS                     = 16.
    IF sy-subrc <> 0 AND sy-subrc <> 99.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                                   class = package
                                                  method = 'set_all_attributes'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ELSE.
    CALL METHOD cl_package=>if_package~create_new_package
*    EXPORTING
*      i_reuse_deleted_object     = 'X'
      IMPORTING
        e_package                  = package
      CHANGING
        c_package_data             = _data
      EXCEPTIONS
        author_not_existing        = 99
        component_not_existing     = 2
        component_missing          = 3
        intern_err                 = 4
        invalid_package_name       = 5
        layer_invalid              = 6
        not_authorized             = 7
        object_already_existing    = 8
        object_just_created        = 9
        prefix_in_use              = 10
        reserved_local_name        = 11
        short_text_missing         = 12
        software_component_invalid = 13
        undefined_name             = 14
        unexpected_error           = 15
        wrong_name_prefix          = 16
        OTHERS                     = 17.
    IF sy-subrc <> 0 AND sy-subrc <> 99.
      o_mycx ?= zaplink_cx=>create_from_method_cx( subrc = sy-subrc
                                              class_name = 'CL_PACKAGE'
                                                  method = 'if_package~create_new_package'
                                            cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ENDIF.

  IF sy-subrc = 99 AND _data-as4user <> sy-uname.           "#EC *
    _data-as4user = sy-uname.
    CALL METHOD update_package
      EXPORTING
        data    = _data
      CHANGING
        package = package.
  ENDIF.
*  ELSEIF sy-subrc <> 0.
*    CASE sy-subrc.
**      WHEN 3.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>not_authorized.
**      WHEN 4 OR 5 OR 6 OR 7.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>error_message
**            msg = 'Problem with Dev Class name'.
**      WHEN 8.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>error_message
**            msg = 'Problem with Short Text'.
**      WHEN 9.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>error_message
**            msg = 'Problem with SoftwareComponent'.
**      WHEN 10.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>error_message
**            msg = 'Problem with Layer'.
**      WHEN 12 OR 13.
**        RAISE EXCEPTION TYPE zaplink_cx_connector
**          EXPORTING
**            textid = zaplink_cx_connector=>error_message
**            msg = 'Problem with Component'.
*      WHEN OTHERS.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>system_error.
*    ENDCASE.
*  ENDIF.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_package   TYPE to_package.
  DATA o_interface TYPE to_interface.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN st_package.
      o_package ?= object->raw.
      o_package->anonymize( ).
    WHEN st_interface.
      o_interface ?= object->raw.
      o_interface->anonymize( ).
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
  DATA o_package   TYPE to_package.
  DATA o_interface TYPE to_interface.

  TRY.
      super->zaplink_cnx_ext_cleaner~unanonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.

  CASE object->type.
    WHEN st_package.
      o_package ?= object->raw.
      o_package->unanonymize( ).
    WHEN st_interface.
      o_interface ?= object->raw.
      o_interface->unanonymize( ).
    WHEN OTHERS.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_devc   TYPE to_package.
  DATA o_pinf   TYPE to_interface.
  data o_raw_i  type TO_RAW_INTERFACE.
  data o_raw_p  type TO_RAW_package.

  CASE type.
    WHEN st_package.
      CREATE OBJECT o_raw_p.
      object = o_raw_p.
      CREATE OBJECT o_devc.
      object->raw = o_devc.
    WHEN st_interface.
      CREATE OBJECT o_raw_i.
      object = o_raw_i.
      CREATE OBJECT o_pinf.
      object->raw = o_pinf.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA key_devc   TYPE devclass.
  DATA key_intf   TYPE scomifnam.
  DATA package    TYPE REF TO if_package.
  DATA interface  TYPE REF TO if_package_interface.
  DATA d_type     TYPE td_comptype.
  DATA d_name     TYPE td_compname.

  TRY.
    d_type = component->get_type( ).
    d_name = component->get_name( ).
    CASE d_type.
      WHEN st_package.
        key_devc = d_name.
        TRY.
            package = load_devc( key_devc ).
            _delete_devc( package ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
      WHEN st_interface.
        key_intf = d_name.
        TRY.
            interface = load_intf( key_intf ).
            _delete_intf( interface ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
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
  DATA key   TYPE scomifnam.
  DATA _devc TYPE REF TO if_package.              "#EC NEEDED for debug
  DATA _pinf TYPE REF TO if_package_interface.    "#EC NEEDED for debug
  DATA type  TYPE td_comptype.

  TRY.

      key = component->get_name( ).
      type = component->get_type( ).
      CASE type.
        WHEN st_package.
          CALL METHOD cl_package=>if_package~load_package
            EXPORTING
              i_package_name             = key
              i_force_reload             = 'X'
            IMPORTING
              e_package                  = _devc
            EXCEPTIONS
              object_not_existing        = 1
*      unexpected_error           = 2  => DUMP
*      intern_err                 = 3  => DUMP
              object_locked_and_modified = 0
*      others                     = 5  => DUMP
                  .
          IF sy-subrc = 0.
            exists = 'X'.
          ENDIF.
        WHEN st_interface.
          CALL METHOD cl_package_interface=>if_package_interface~load_package_interface
            EXPORTING
              i_package_interface_name   = key
              i_force_reload             = 'X'
            IMPORTING
              e_package_interface        = _pinf
            EXCEPTIONS
*      db_read_error              = 1 => DUMP
*      unexpected_error           = 2 => DUMP
              object_not_existing        = 3
              shorttext_not_existing     = 0
              object_locked_and_modified = 0
*      others                     = 6 => DUMP
                  .
          IF sy-subrc = 0.
            exists = 'X'.
          ENDIF.
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
  DATA o_package   TYPE to_package.
  DATA o_interface TYPE to_interface.
  DATA type  TYPE td_comptype.

  TRY.

    CREATE OBJECT object.
    object->set_component( component ).

    type = component->get_type( ).
    CASE type.
      WHEN st_package.
        TRY.
            o_package = export_from_sap_package( component ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
        CLEAR o_package->a0_maindata-devclass.
        object->raw = o_package.
      WHEN st_interface.
        TRY.
            o_interface = export_from_sap_interface( component ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
        CLEAR o_interface->a0_maindata-intf_name.
        object->raw = o_interface.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA _o_class       TYPE to_package.
  DATA _o_interface   TYPE to_interface.
  DATA o_comp         TYPE to_component.                    " Issue 92

  TRY.
    CREATE OBJECT components.                         " Issue 92
    CASE object->type.
      WHEN st_package.
        _o_class ?= object->raw.
        _o_class->a0_maindata-devclass = object->name.
        TRY.
            components = import_to_sap_package( _o_class ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
      WHEN st_interface.
        _o_interface ?= object->raw.
        _o_interface->a0_maindata-intf_name = object->name.
        TRY.
            components = import_to_sap_interface( _o_interface ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name object->type.
    ENDCASE.
    check_component_list( EXPORTING     object = object
                           CHANGING components = components ). " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method _DELETE_DEVC.
  DATA interfaces  TYPE tpak_package_interface_list.
  DATA packages    TYPE scompaklis.
  DATA cx_name     TYPE string.
  FIELD-SYMBOLS:
    <pack> LIKE LINE OF packages,
    <itf> LIKE LINE OF interfaces.

  CALL METHOD package->set_changeable
    EXPORTING
      i_changeable                = 'X'
    EXCEPTIONS
      object_locked_by_other_user = 1
      permission_failure          = 2
      object_already_changeable   = 0
      object_already_unlocked     = 0
      object_just_created         = 5
      object_deleted              = 6
      object_modified             = 7
      object_not_existing         = 8
      object_invalid              = 9
      unexpected_error            = 10
      OTHERS                      = 11.
  IF sy-subrc <> 0.
    CASE sy-subrc.
*      WHEN 1.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>Lock_error.
*      WHEN 2.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid = zaplink_cx_connector=>system_error.
    ENDCASE.
  ENDIF.

* Interfaces
  CALL METHOD package->get_interfaces
    IMPORTING
      e_package_interfaces = interfaces
    EXCEPTIONS
      object_invalid       = 1
      unexpected_error     = 2
      intern_err           = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid = zaplink_cx_connector=>system_error.
  ELSEIF NOT interfaces IS INITIAL.
    LOOP AT interfaces ASSIGNING <itf>.
      TRY.
          _delete_intf( <itf> ).
        CATCH zaplink_cx_connector INTO o_mycx.
          RAISE EXCEPTION o_mycx.
*          application_log->add_exception( o_mycx ).
      ENDTRY.
    ENDLOOP.
  ENDIF.

* Packages
  CALL METHOD package->get_sub_packages
    IMPORTING
      e_sub_packages   = packages
    EXCEPTIONS
      object_invalid   = 1
      leaf_package     = 0   " has no sub packages
      unexpected_error = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid = zaplink_cx_connector=>system_error.
  ELSEIF NOT packages IS INITIAL.
    LOOP AT packages ASSIGNING <pack>.
      TRY.
          _delete_devc( <pack> ).
        CATCH zaplink_cx_connector INTO o_mycx.
          RAISE EXCEPTION o_mycx.
*          application_log->add_exception( o_mycx ).
      ENDTRY.
    ENDLOOP.
  ENDIF.

  _delete_elements( package ). " try to delete TADIR entries that prevent package deletion

  CALL METHOD package->delete
    EXCEPTIONS
      object_not_empty      = 1
      object_not_changeable = 2
      object_invalid        = 3
      intern_err            = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.
    CALL METHOD application_log->add_symsg( ).
* Application_log is allready accessible. doing this will duplicate messages
*    o_cx_cnx ?= zaplink_cx_connector=>new_with_ballog( cx_name = 'ZAPLINK_CX_CONNECTOR'
*                                             message_collector = application_log
*                                                        textid = zaplink_cx_connector=>system_error ).
*    RAISE EXCEPTION o_cx_cnx.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO cx_name.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        cx_name = cx_name
        textid  = zaplink_cx_connector=>system_error.
  ENDIF.

  CALL METHOD package->save
    EXPORTING
      i_transport_request   = space
*   IMPORTING
*     e_transport_request   =
    EXCEPTIONS
      object_invalid        = 1
      object_not_changeable = 2
      cancelled_in_corr     = 3
      permission_failure    = 4
      unexpected_error      = 5
      intern_err            = 6
      OTHERS                = 7.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid = zaplink_cx_connector=>system_error.
  ENDIF.
  endmethod.


  method _DELETE_ELEMENTS.
  DATA t_elements TYPE pakdevelemtab.
  DATA f_errors   TYPE abap_bool.
  DATA d_name     TYPE tadir-obj_name.
  DATA s_tadir    TYPE tadir.
  DATA f_exists   TYPE saus_dpara-tadirexist.
  FIELD-SYMBOLS:
    <e> LIKE LINE OF t_elements.

  f_errors = abap_false.
  CALL METHOD package->get_elements
*    EXPORTING
*      i_check_existence = TPAK_C_FALSE
     IMPORTING
       e_elements        = t_elements.
  LOOP AT t_elements ASSIGNING <e>.
    IF <e>->dev_elem_type = st_package.   " do not try to remove DEVC
      DELETE t_elements.
      CONTINUE.
    ENDIF.
    d_name = <e>->dev_elem_key.
    CALL FUNCTION 'TRINT_TADIR_DELETE'
      EXPORTING
        object                   = <e>->dev_elem_type
        obj_name                 = d_name
        pgmid                    = <e>->dev_elem_pgmid
        iv_test_mode             = abap_true
      EXCEPTIONS
        tadir_entry_not_existing = 1
        object_exists            = 2
        object_locked            = 3
        object_distributed       = 4
        OTHERS                   = 5.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 2 OR 3.
          CALL METHOD application_log->add_symsg( ).
* Because message is not engouth =>
* Object TADIR entry : &1 &2 &3
          SET EXTENDED CHECK OFF.
          IF 1 = 2. MESSAGE i020(zaplink_package) WITH <e>->dev_elem_pgmid <e>->dev_elem_type d_name. ENDIF.
          SET EXTENDED CHECK ON.
          CALL METHOD application_log->add
             EXPORTING
*            is_message   =
               id_msgty     = 'I'
               id_msgid     = 'ZAPLINK_PACKAGE'
               id_msgno     = '020'
               id_msgv1     = <e>->dev_elem_pgmid
               id_msgv2     = <e>->dev_elem_type
               id_msgv3     = d_name
*            id_msgv4     =
              .
        WHEN OTHERS.
          CALL METHOD application_log->add_symsg( ).
      ENDCASE.
      f_errors = abap_true.
    ENDIF.
  ENDLOOP.
  IF f_errors = abap_false.
    LOOP AT t_elements ASSIGNING <e>.
      d_name = <e>->dev_elem_key.
      CALL FUNCTION 'TRINT_TADIR_QUERY'
        EXPORTING
          iv_pgmid           = <e>->dev_elem_pgmid
          iv_object          = <e>->dev_elem_type
          iv_obj_name        = d_name
        IMPORTING
*          EV_OBJECT          = s_TADIR-OBJECT
          ev_srcsystem       = s_tadir-srcsystem
*          EV_DEVCLASS        = s_TADIR-DEVCLASS
          ev_author          = s_tadir-author
          ev_genflag         = s_tadir-genflag
          ev_exist           = f_exists.

      CALL FUNCTION 'TRINT_TADIR_DELETE'
        EXPORTING
          object                   = <e>->dev_elem_type
          obj_name                 = d_name
          pgmid                    = <e>->dev_elem_pgmid
          iv_test_mode             = abap_false
        EXCEPTIONS
          tadir_entry_not_existing = 1
          object_exists            = 2
          object_locked            = 3
          object_distributed       = 4
          OTHERS                   = 5.
      IF sy-subrc <> 0.
        ROLLBACK WORK.
        CALL METHOD application_log->add_symsg( ).
        EXIT.
      ELSEIF NOT f_exists IS INITIAL.
* Delete TADIR entry of &1 &2 &3 (not existing)
        SET EXTENDED CHECK OFF.
        IF 1 = 2. MESSAGE w018(zaplink_package) WITH <e>->dev_elem_pgmid <e>->dev_elem_type d_name. ENDIF.
        SET EXTENDED CHECK ON.
        CALL METHOD application_log->add
           EXPORTING
*            is_message   =
             id_msgty     = 'W'
             id_msgid     = 'ZAPLINK_PACKAGE'
             id_msgno     = '018'
             id_msgv1     = <e>->dev_elem_pgmid
             id_msgv2     = <e>->dev_elem_type
             id_msgv3     = d_name
*            id_msgv4     =
            .
* TADIR entry : Source System (&1) Author (&2) GenFlag (&3)
        SET EXTENDED CHECK OFF.
        IF 1 = 2. MESSAGE i019(zaplink_package) WITH <e>->dev_elem_pgmid <e>->dev_elem_type d_name. ENDIF.
        SET EXTENDED CHECK ON.
        CALL METHOD application_log->add
           EXPORTING
*            is_message   =
             id_msgty     = 'I'
             id_msgid     = 'ZAPLINK_PACKAGE'
             id_msgno     = '019'
             id_msgv1     = s_tadir-srcsystem
             id_msgv2     = s_tadir-author
             id_msgv3     = s_tadir-genflag
*            id_msgv4     =
            .
      ENDIF.
    ENDLOOP.
  ENDIF.
  endmethod.


  method _DELETE_INTF.

  CALL METHOD interface->set_changeable
    EXPORTING
      i_changeable                = 'X'
    EXCEPTIONS
      object_locked_by_other_user = 1
      permission_failure          = 2
      object_already_changeable   = 0
      object_already_unlocked     = 0
      object_just_created         = 5
      object_deleted              = 6
      object_modified             = 7
      object_not_existing         = 8
      object_invalid              = 9
      unexpected_error            = 10
      OTHERS                      = 11.
  IF sy-subrc <> 0.
    CASE sy-subrc.
*      WHEN 1.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>Lock_error.
*      WHEN 2.
*        RAISE EXCEPTION TYPE zaplink_cx_connector
*          EXPORTING
*            textid = zaplink_cx_connector=>not_authorized.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid = zaplink_cx_connector=>system_error.
    ENDCASE.
  ENDIF.

  CALL METHOD interface->delete
    EXCEPTIONS
      object_not_empty      = 1
      object_not_changeable = 2
      object_invalid        = 3
      intern_err            = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid = zaplink_cx_connector=>system_error.
  ENDIF.

  CALL METHOD interface->save
    EXPORTING
      i_transport_request   = space
*   IMPORTING
*     e_transport_request   =
    EXCEPTIONS
      object_invalid        = 1
      object_not_changeable = 2
      cancelled_in_corr     = 3
      permission_failure    = 4
      unexpected_error      = 5
      intern_err            = 6
      OTHERS                = 7
          .
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid = zaplink_cx_connector=>system_error.
  ENDIF.
  endmethod.
ENDCLASS.
