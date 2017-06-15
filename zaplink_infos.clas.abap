class ZAPLINK_INFOS definition
  public
  final
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases FRAMEWORK_VERSION
    for ZAPLINK_DATATYPES~FRAMEWORK_VERSION .
  aliases TD_CONNUUID
    for ZAPLINK_DATATYPES~TD_CONNUUID .
  aliases TD_CONNVER
    for ZAPLINK_DATATYPES~TD_CONNVER .
  aliases TD_FRAMEWORKVER
    for ZAPLINK_DATATYPES~TD_FRAMEWORKVER .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  types:
    BEGIN OF ts_zl_comp.
    INCLUDE TYPE ts_compkey AS hdr.
    TYPES:
         devclass         TYPE string,
         signature        TYPE string,
         raw_data_type    TYPE abap_bool,        " component that can't be removed
       END OF ts_zl_comp .
  types:
    tt_zl_comps TYPE SORTED TABLE OF ts_zl_comp WITH UNIQUE KEY type name .
  types:
    BEGIN OF ts_zl_cnx,
          uuid               TYPE td_connuuid.
    INCLUDE TYPE ts_zl_comp.
    TYPES:
          version            TYPE td_connver,
          supported_types    TYPE tt_types,
          _components        TYPE tt_zl_comps,
        END OF ts_zl_cnx .
  types:
    tt_zl_cnx TYPE SORTED TABLE OF ts_zl_cnx WITH UNIQUE KEY uuid .
  types:
    BEGIN OF ts_comp_detail.
    INCLUDE TYPE spam_cvers AS hdr.
    TYPES:
         level        TYPE spam_fpdef-version,
         high_package TYPE pat03-patch,
         high_spp     TYPE pat03-patch,
         spp_level    TYPE spam_fpdef-version,
         state        TYPE comp_state,
         active       TYPE spam_cvers-comp_type,
         patchable    TYPE spam_cvers-comp_type,
       END OF ts_comp_detail .
  types:
    tt_hot_pack TYPE STANDARD TABLE OF pat03 WITH DEFAULT KEY .
  types:
    tt_comps TYPE STANDARD TABLE OF ts_comp_detail WITH DEFAULT KEY .
  types:
    BEGIN OF ts_status_info,
                    sap_ver              TYPE string,
                    os                   TYPE string,
                    machine_type         TYPE string,
                    unicode              TYPE string,
                    db_sys               TYPE string,
                    db_release           TYPE string,
                    db_lib               TYPE string,
                    kernel               TYPE string,
                    abap_ver             TYPE string,
                    cua_ver              TYPE string,
                  END OF ts_status_info .
  types:
    BEGIN OF ts_sap,
            status TYPE ts_status_info,
            _hot_packages TYPE tt_hot_pack,
            _components TYPE tt_comps,
          END OF  ts_sap .
  types:
    BEGIN OF ts_zl,
            framework_version TYPE td_frameworkver,
            components TYPE tt_zl_comps,
            connectors TYPE tt_zl_cnx,
          END OF  ts_zl .

  data SAP type TS_SAP read-only .
  data ZAPLINK type TS_ZL read-only .
  data O_CX type ref to ZAPLINK_CX .
  data O_MYCX type ref to ZAPLINK_CX .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    raising
      ZAPLINK_CX .
protected section.

  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TS_COMPONENT
    for ZAPLINK_DATATYPES~TS_COMPONENT .
  aliases TS_COMPTYPE
    for ZAPLINK_DATATYPES~TS_COMPTYPE .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .

  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .
  types TI_CODE_SIGNATURE type ref to ZAPLINK_CNX_EXT_CODE_SIGNATURE .
  types TO_GENERATOR type ref to ZAPLINK_DEPENDENCIES_ANALYSER .
  types TO_RAW type ref to ZAPLINK_RAW .
  types TD_CLASSNAME type ZAPLINK_DEPENDENCIES_ANALYSER=>TD_NAME .

  class-data CNX_PACKAGES type TR_PACKAGES .

  methods GET_SAP_DATA .
  methods GET_ZL_DATA
    raising
      ZAPLINK_CX .
private section.

  class-data C_CLASS type TS_COMPTYPE .
  class-data C_INTERFACE type TS_COMPTYPE .
  class-data C_PROGRAM type TS_COMPTYPE .
  class-data C_MSG_CLASS type TS_COMPTYPE .
  class-data C_TYPEGROUP type TS_COMPTYPE .
ENDCLASS.



CLASS ZAPLINK_INFOS IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  data s_pack like LINE OF cnx_packages.

  s_pack-sign = 'I'.    s_pack-option = 'EQ'.
  s_pack-low = '$TMP'.      append s_pack to cnx_packages.
  s_pack-low = 'ZAPLINK-CONNECTORS'.      append s_pack to cnx_packages.
  s_pack-low = 'ZAPLINK-CONNECTORS-MAIN'.      append s_pack to cnx_packages.
  s_pack-low = 'ZAPLINK-CONNECTORS-OTHERS'.      append s_pack to cnx_packages.

  c_class-type = 'CLAS'.    c_class-kind = zaplink_connectors=>get_typekind( c_class-type ).
  c_interface-type = 'INTF'.    c_interface-kind = zaplink_connectors=>get_typekind( c_interface-type ).
  c_program-type = 'PROG'.    c_program-kind = zaplink_connectors=>get_typekind( c_program-type ).
  c_typegroup-type = 'TYPE'.    c_typegroup-kind = zaplink_connectors=>get_typekind( c_typegroup-type ).
  c_msg_class-type = 'MSAG'.    c_msg_class-kind = zaplink_connectors=>get_typekind( c_msg_class-type ).
  endmethod.


  method CONSTRUCTOR.
  me->get_sap_data( ).
  me->get_zl_data( ).
  endmethod.


  method GET_SAP_DATA.
* FROM FUNC : STATUS_ANZEIGEN
* Machine type
  TYPES:
    BEGIN OF ts_ver_tab,
      line(80) TYPE c,
    END OF ts_ver_tab.
  DATA version_info TYPE STANDARD TABLE OF ts_ver_tab.
  FIELD-SYMBOLS:
    <v> LIKE LINE OF version_info.
  TYPES:
    BEGIN OF ts_ver_info,
      name TYPE string,
      value TYPE string,
    END OF ts_ver_info.
  DATA t_values_ver TYPE STANDARD TABLE OF ts_ver_info.
  DATA s_value LIKE LINE OF t_values_ver.
  DATA d_char(255) TYPE c.
  DATA it_comptab    TYPE STANDARD TABLE OF spam_cvers.
*  DATA it_components TYPE STANDARD TABLE OF comp_list_type.
  DATA s_comp        LIKE LINE OF sap-_components.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF it_comptab.

  DEFINE mac_read_val.
    read table t_values_ver into s_value
         with key name = &1.
    if sy-subrc = 0.    &2 = s_value-value.   endif.
  END-OF-DEFINITION.


*CALL 'SAPCORE' ID 'ID' FIELD 'VERSION'
*               ID 'TABLE' FIELD version_info-*sys*.
  CALL 'SAPCORE' ID 'ID' FIELD 'VERSION'
                 ID 'TABLE' FIELD version_info.
  LOOP AT version_info ASSIGNING <v>.
    CLEAR s_value.
    s_value-name = <v>(21).   s_value-value = <v>+21.
    APPEND s_value TO t_values_ver.
  ENDLOOP.
  mac_read_val:
    'SAP version' sap-status-sap_ver,             "#EC NOTEXT
    'operating system' sap-status-os,             "#EC NOTEXT
    'machine type' sap-status-machine_type,       "#EC NOTEXT
    'database system' sap-status-db_sys,          "#EC NOTEXT
    'kernel release' sap-status-kernel,           "#EC NOTEXT
    'database library' sap-status-db_lib,         "#EC NOTEXT
    'ABAP load version' sap-status-abap_ver,      "#EC NOTEXT
    'CUA load version' sap-status-cua_ver.        "#EC NOTEXT

* Database name
*  CALL FUNCTION 'DB_DBNAME'
*    IMPORTING
*      dbname = status-database_name.
** Database server
*  CALL FUNCTION 'DB_DBHOST'
*    IMPORTING
*      dbhost = status-database_host.
** Database schema/owner
*  CALL FUNCTION 'DB_DBSCHEMA'
*    IMPORTING
*      dbschema = status-database_owner.
* Database release
  CALL FUNCTION 'DB_GET_RELEASE'
    IMPORTING
      release = d_char.
  sap-status-db_release = d_char.

* Unicode information
  IF cl_abap_char_utilities=>charsize = 1.
    sap-status-unicode = 'Yes'(oui).
  ELSE.
    sap-status-unicode = 'No'(non).
  ENDIF.

*  CALL FUNCTION 'UPG_GET_COMPONENT_RELEASES'
**   EXPORTING
**     IV_COMPONENT                      = '*'
**     IV_BUFFERED                       = 'X'
**   IMPORTING
**     EV_COMPCNT                        =
**     EV_BASISSTATE                     =
*    TABLES
*      tt_cvers                          = components_releases
*    EXCEPTIONS
*      no_release_found                  = 0
*      component_release_not_found       = 0
*      OTHERS                            = 0.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  CALL FUNCTION 'SPAM_READ_PAT03'
*   EXPORTING
*     IV_PATCH_NAME                    = '*'
*     IV_PATCH_TYPE                    = '*'
*     IV_BASE_RELEASE                  = 'CURR'
*     IV_COMPONENT_ID                  = 'CURR'
*     IV_COMPONENT_RELEASE             = 'CURR'
*     IV_ADDON_ID                      = 'CURR'
*     IV_ADDON_RELEASE                 = 'CURR'
*     IV_ACTIV                         = 'X'
*     IV_ONLY_SUPP_PACKS               = 'X'
*     IV_RSTLAN_IO_MODE                = ' '
*   IMPORTING
*     EV_PAT03_ENTRIES                 =
    TABLES
      tt_pat03_selected                = sap-_hot_packages
    EXCEPTIONS
      wrong_release_string             = 1
      wrong_patch_type                 = 2
      no_component_available           = 3
      wrong_component_id_release       = 4
      wrong_addon_id_release           = 5
      OTHERS                           = 6.
  IF sy-subrc <> 0.   clear sap-_hot_packages.    ENDIF.

  SORT sap-_hot_packages BY component  ASCENDING
                            addon_id   ASCENDING
                            imple_date ASCENDING
                            imple_time ASCENDING.

* FROM (LSHSYO01) MODULE init OUTPUT.
  CALL FUNCTION 'OCS_GET_INSTALLED_COMPS'
    TABLES
      tt_comptab = it_comptab
    EXCEPTIONS
      OTHERS     = 0.
  CALL FUNCTION 'OCS_SORT_COMPS_HIERARCHICAL'
    TABLES
      tt_comptab = it_comptab
    EXCEPTIONS
      OTHERS     = 0.

  LOOP AT it_comptab ASSIGNING <c>.
    CLEAR s_comp.   s_comp-hdr = <c>.
    IF sy-saprl < '701'.                                    " Issue 90
      CALL FUNCTION 'OCS_GET_COMPONENT_STATE'
        EXPORTING
          iv_component  = <c>-component
        IMPORTING
          ev_comp_rel             = s_comp-release
          ev_comp_type            = s_comp-comp_type
          ev_comp_level           = s_comp-level
          ev_comp_spp_level       = s_comp-spp_level
          ev_last_patch           = s_comp-high_package
          ev_last_spp             = s_comp-high_spp
          ev_comp_active          = s_comp-active
          ev_comp_patchable       = s_comp-patchable
          ev_is_subcomp           = s_comp-state
*       ES_MASTERCOMP           =
        EXCEPTIONS
          unknown_component       = 1
          OTHERS                  = 2.
    ELSE.
      CALL FUNCTION 'OCS_GET_COMPONENT_STATE'
        EXPORTING
          iv_component  = <c>-component
        IMPORTING
          ev_comp_rel             = s_comp-release
          ev_comp_type            = s_comp-comp_type
          ev_comp_level           = s_comp-level
*        ev_comp_spp_level       = s_comp-spp_level     " Issue 90
          ev_last_patch           = s_comp-high_package
*        ev_last_spp             = s_comp-high_spp      " Issue 90
          ev_comp_active          = s_comp-active
          ev_comp_patchable       = s_comp-patchable
          ev_is_subcomp           = s_comp-state
*       ES_MASTERCOMP           =
        EXCEPTIONS
          unknown_component       = 1
          OTHERS                  = 2.
    ENDIF.
    IF sy-subrc = 0.    APPEND s_comp TO sap-_components.   ENDIF.
  ENDLOOP.
  endmethod.


  method GET_ZL_DATA.
  DATA t_comps      TYPE tt_compkeys.
  DATA r_packages   TYPE tr_packages.
  DATA s_package    LIKE LINE OF r_packages.
  DATA s_comp       LIKE LINE OF zaplink-components.
  DATA o_list       TYPE to_list.
  DATA o_cnx        TYPE to_connector.
  DATA o_raw        TYPE to_raw.
  DATA o_ref        TYPE REF TO cl_abap_classdescr.
  DATA o_comp       TYPE to_component.
  DATA o_c_n        TYPE to_component.
  DATA i_sign       TYPE ti_code_signature.
  DATA s_cnx        LIKE LINE OF zaplink-connectors.
  DATA s_subcomp    LIKE LINE OF s_cnx-_components.
*  DATA t_connectors TYPE tt_zl_comps.
  DATA _comp        TYPE ts_component.
  DATA cnx_name     TYPE seoclskey.
  DATA o_generator  TYPE to_generator.
*  DATA o_cnxlist    TYPE to_list.
  DATA d_classname  TYPE td_classname.
  DATA o_cnxcomp    TYPE to_component.
  DATA t_global_c   TYPE tt_zl_comps.
  DATA t_commun_c   TYPE tt_zl_comps.
  DATA d_cnx        LIKE LINE OF t_global_c.
  DATA d_kind       TYPE td_transport_kind.
  DATA _regex TYPE string.
  FIELD-SYMBOLS:
*    <m> LIKE LINE OF o_generator->messages,
*    <i> LIKE LINE OF o_generator->includes,
    <st> LIKE LINE OF o_cnx->supported_types,
    <x> LIKE LINE OF zaplink-connectors,
    <sc> LIKE LINE OF <x>-_components,
    <o> LIKE LINE OF o_generator->all_components,
    <c> LIKE LINE OF t_comps.

* Framework
  zaplink-framework_version = framework_version.

* Connectors
  o_list = zaplink_connectors_4inst=>get_connectorlist( ).
  o_list->select_default( zaplink_list=>sel_actions-export ).

* Initialize Interator
  o_list->order_kind = zaplink_list=>order_kinds-no_order.
  o_list->init_sel_iter( ).
  o_list->display_progress = abap_true.
  o_c_n = o_list->get_next( ).
  WHILE o_c_n IS BOUND.
    o_comp = o_c_n.     o_c_n = o_list->get_next( ).
    CLEAR: s_cnx, _comp.
    _comp = o_comp->get_key( ).
    d_kind = o_comp->get_typekind( ).
    CREATE OBJECT o_cnx TYPE (_comp-name).
    s_cnx-hdr = _comp.    s_cnx-uuid = o_cnx->uuid.     s_cnx-version = o_cnx->version.   s_cnx-supported_types = o_cnx->supported_types.
    cnx_name-clsname = _comp-name.  s_cnx-signature = zaplink_object=>get_class_signature( cnx_name ).

    SELECT SINGLE devclass INTO s_cnx-devclass
      FROM tadir
      WHERE    pgmid = d_kind
        AND   object = _comp-type
        AND obj_name = _comp-name.

    CREATE OBJECT o_generator.
    o_generator->packages = cnx_packages.

    d_classname = _comp-name.
    o_generator->add_object( d_classname ).
    LOOP AT o_generator->all_components ASSIGNING <o>.
      CHECK <o>-type <> _comp-type OR <o>-name <> _comp-name.
      CLEAR s_subcomp.    s_subcomp-name = <o>-name.    s_subcomp-type = <o>-type.    s_subcomp-devclass = <o>-devclass.
      READ TABLE t_commun_c TRANSPORTING NO FIELDS
           WITH TABLE KEY type = s_subcomp-type
                          name = s_subcomp-name.
      CHECK sy-subrc <> 0.    " not commun comp
      READ TABLE t_global_c TRANSPORTING NO FIELDS
           WITH TABLE KEY type = s_subcomp-type
                          name = s_subcomp-name.
      IF sy-subrc = 0.
        INSERT s_subcomp INTO TABLE t_commun_c.   " component is present twice time on 2 differents connectors
        CONTINUE.
      ENDIF.

      INSERT s_subcomp INTO TABLE s_cnx-_components.
      CLEAR d_cnx. d_cnx-name = s_subcomp-name.   d_cnx-type = s_subcomp-type.
      INSERT d_cnx INTO TABLE t_global_c.
    ENDLOOP.

* Force RAW data type
    TRY.
*        _regex = '\\CLASS='.
      CONCATENATE '\\' c_class-type '=' into _regex.
      LOOP AT o_cnx->supported_types ASSIGNING <st>.
        o_raw = o_cnx->create_new_raw( <st>-type ).
        CHECK o_raw->raw IS BOUND.
*        o_ref ?= cl_abap_typedescr=>describe_by_object_ref( o_raw->raw ).
*        CHECK o_ref IS BOUND.
        CLEAR s_subcomp.    s_subcomp-name = cl_abap_classdescr=>get_class_name( o_raw->raw ).    s_subcomp-type = c_class-type.
        REPLACE FIRST OCCURRENCE OF REGEX _regex IN s_subcomp-name WITH ``.
*        REPLACE FIRST OCCURRENCE OF REGEX '\\CLASS=' IN s_subcomp-name WITH ``.

        SELECT SINGLE devclass INTO s_subcomp-devclass
          FROM v_tralan
          WHERE pgmid = c_class-kind
            AND object = s_subcomp-type
            AND obj_name = s_subcomp-name.
        s_subcomp-raw_data_type = abap_true.
        MODIFY TABLE s_cnx-_components FROM s_subcomp.
        IF sy-subrc <> 0.   INSERT s_subcomp INTO TABLE s_cnx-_components.    ENDIF.
        CLEAR d_cnx. d_cnx-name = s_subcomp-name.   d_cnx-type = s_subcomp-type.
        INSERT d_cnx INTO TABLE t_global_c.
      ENDLOOP.
    ENDTRY.

    IF NOT s_cnx IS INITIAL.  INSERT s_cnx INTO TABLE zaplink-connectors.   ENDIF.
    CLEAR d_cnx. d_cnx-name = _comp-name.
    IF NOT d_cnx IS INITIAL.  d_cnx-type = c_class-type.  INSERT d_cnx INTO TABLE t_global_c.   ENDIF.
  ENDWHILE.

* Remove missed commun components & update component data
  LOOP AT zaplink-connectors ASSIGNING <x>.
    LOOP AT <x>-_components ASSIGNING <sc>
            WHERE raw_data_type = abap_false.
      READ TABLE t_commun_c TRANSPORTING NO FIELDS
           WITH TABLE KEY type = <sc>-type
                          name = <sc>-name.
      CHECK sy-subrc = 0.                 " component is a commun comp
      DELETE TABLE <x>-_components FROM <sc>.
    ENDLOOP.
    LOOP AT <x>-_components ASSIGNING <sc>.
      o_comp = zaplink_component_4inst=>create_new( <sc>-hdr ).
      CHECK o_comp IS BOUND.    o_cnx = o_comp->connector( ).
      CHECK o_cnx IS BOUND.
      CHECK o_cnx->do_exists( o_comp ) = abap_true.
      TRY.
          i_sign ?= o_cnx.
          <sc>-signature = i_sign->get_signature( o_comp ).
        CATCH cx_root.
      ENDTRY.
      DELETE TABLE t_commun_c FROM <sc>.
    ENDLOOP.
    DELETE TABLE t_commun_c
           WITH TABLE KEY type = <x>-type
                          name = <x>-name.
  ENDLOOP.

  s_package-sign = 'I'. s_package-option = 'EQ'.    s_package-low = 'ZAPLINK'.
  APPEND s_package TO r_packages.
  t_comps = zaplink_tools=>get_keys_from_package( packages = r_packages ).
  SORT t_comps BY type name.
  LOOP AT t_comps ASSIGNING <c>.
    READ TABLE t_global_c TRANSPORTING NO FIELDS
         WITH TABLE KEY type = <c>-type
                        name = <c>-name.
    IF sy-subrc = 0.
      READ TABLE t_commun_c TRANSPORTING NO FIELDS
           WITH TABLE KEY type = <sc>-type
                          name = <sc>-name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR s_comp.   s_comp-hdr = <c>.
    o_comp = zaplink_component_4inst=>create_new( <c> ).
    CHECK o_comp IS BOUND.    o_cnx = o_comp->connector( ).
    CHECK o_cnx IS BOUND.
    CHECK o_cnx->do_exists( o_comp ) = abap_true.

    d_kind = o_comp->get_typekind( ).
    SELECT SINGLE devclass INTO s_comp-devclass
      FROM tadir
      WHERE    pgmid = d_kind
        AND   object = <c>-type
        AND obj_name = <c>-name.

    TRY.
        i_sign ?= o_cnx.
      CATCH cx_root.
    ENDTRY.
    IF i_sign IS BOUND.   s_comp-signature = i_sign->get_signature( o_comp ).     ENDIF.

    APPEND s_comp TO zaplink-components.
  ENDLOOP.
  endmethod.
ENDCLASS.
