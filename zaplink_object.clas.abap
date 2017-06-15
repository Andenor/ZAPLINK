class ZAPLINK_OBJECT definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public

  global friends ZAPLINK_OBJECT_DATA .

public section.
  type-pools ABAP .
  type-pools SEOF .
  type-pools SEOK .
  type-pools SEOP .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  type-pools SEOX .

  types TD_SOURCECODE type SEOP_SOURCE_STRING .

  class-methods CHECK_CLASS_SECTIONS
    importing
      !APPLICATION_LOG type TO_MSG_COLL
      !CLASSKEY type SEOCLSKEY
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods CLEANUP_CLASS
    importing
      !CLASSKEY type SEOCLSKEY
    returning
      value(HAS_CHANGE) type ABAP_BOOL
    raising
      ZAPLINK_CX_CONNECTOR .
  methods CONSTRUCTOR .
  class-methods GET_CLASS_GLOBALSOURCE
    importing
      !CLASS_NAME type SEOCLSKEY
    returning
      value(RESULT) type TD_SOURCECODE .
  class-methods GET_CLASS_SIGNATURE
    importing
      !CLASS_NAME type SEOCLSKEY
    returning
      value(RESULT) type ZAPLINK_DATATYPES~TD_CHECKSUM
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods GET_INCLUDES
    importing
      !CLSNAME type SEOCLSNAME
    returning
      value(INCLUDES) type ZAPLINK_OBJECT_DATA=>TS_INCLUDES .
  class-methods GET_INTERFACE_GLOBALSOURCE
    importing
      !INTERFACE_NAME type SEOCLSKEY
    returning
      value(RESULT) type TD_SOURCECODE
    exceptions
      FAILED
      NOT_A_CLASS .
  class-methods GET_INTERFACE_SIGNATURE
    importing
      !INTERFACE_NAME type SEOCLSKEY
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

  types TO_CLASS type ref to ZAPLINK_CLAS_DATA .
  types TO_INTERFACE type ref to ZAPLINK_INTF_DATA .
  types TS_INCLUDE type ZAPLINK_OBJECT_DATA=>TS_INCLUDE .
  types TT_TYPEUSAGES type SEOT_TYPEPUSAGES_R .
  types T_FM_DATA type ZAPLINK_OBJECT_DATA=>T_FM_DATA .

  methods CHECK_CLAS_SECTIONS
    importing
      !CLASS type T_FM_DATA
    raising
      ZAPLINK_CX_CONNECTOR .
  methods CHECK_INTF_SECTION
    importing
      !INTERFACE type T_FM_DATA
    raising
      ZAPLINK_CX_CONNECTOR .
  methods CLEANUP_TYPEUSAGE
    importing
      !INCLUDE type TS_INCLUDE
      !TEXT type BAL_S_MSG-MSGV4
    returning
      value(HAS_CHANGE) type ABAP_BOOL
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_FROM_SAP_CLASS
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_CLASS
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_FROM_SAP_INTERFACE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_INTERFACE
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods GENERATE_CLASS
    importing
      value(CLASS) type SEOCLSNAME
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX .
  class-methods GET_CLASSSOURCE_4SIGNATURE
    importing
      !CLASS_NAME type SEOCLSKEY
    returning
      value(RESULT) type TD_SOURCECODE
    exceptions
      NOT_A_CLASS .
  methods IMPORT_TO_SAP_CLASS
    importing
      !O_DATA type TO_CLASS
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TO_SAP_INTERFACE
    importing
      !O_DATA type TO_INTERFACE
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods SEARCH_DEFERED_DECLARATION
    importing
      !SOURCECODE type TT_ABAPRAWSOURCE
      !TYPEPOOLS type SEOT_TYPEPUSAGES_R
    exporting
      !NEW_SOURCECODE type TT_ABAPRAWSOURCE
      !NEW_TYPEPOOLS type SEOT_TYPEPUSAGES_R .
  class-methods _CHECK_CLASS_SECTIONS
    importing
      !APPLICATION_LOG type TO_MSG_COLL
      !CLASSKEY type SEOCLSKEY
      !PRI_SOURCE type TT_ABAPRAWSOURCE
      !PRO_SOURCE type TT_ABAPRAWSOURCE
      !PUB_SOURCE type TT_ABAPRAWSOURCE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _CLEANUP_CLASS
    returning
      value(HAS_CHANGE) type ABAP_BOOL
    raising
      ZAPLINK_CX_CONNECTOR .
private section.

  types:
    BEGIN OF ts_bapi_data,
    class                       TYPE  vseoclass,
    attributes                  TYPE  seoo_attributes_r,
    methods                     TYPE  seoo_methods_r,
    events                      TYPE  seoo_events_r,
    types                       TYPE  seoo_types_r,
    parameters                  TYPE  seos_parameters_r,
    exceps                      TYPE  seos_exceptions_r,
    implementings               TYPE  seor_implementings_r,
    inheritance                 TYPE  vseoextend,
    redefinitions               TYPE  seor_redefinitions_r,
    impl_details                TYPE  seor_redefinitions_r,
    friendships                 TYPE  seof_friendships_r,
    typepusages                 TYPE  seot_typepusages_r,
    clsdeferrds                 TYPE  seot_clsdeferrds_r,
    intdeferrds                 TYPE  seot_intdeferrds_r,
    explore_inheritance         TYPE  seok_cls_typeinfos,
    explore_implementings       TYPE  seok_int_typeinfos,
    aliases                     TYPE  seoo_aliases_r,
    enhancement_methods         TYPE  enhmeth_tabheader,
    enhancement_attributes      TYPE  enhclasstabattrib,
    enhancement_events          TYPE  enhclasstabevent,
    enhancement_implementings   TYPE enhclasstabimplementing,
  END OF ts_bapi_data .
  types:
    BEGIN OF ts_clas_attr.
  INCLUDE TYPE ts_base_attributs AS base.
  TYPES:
    END OF ts_clas_attr .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_OBJECT' ##NO_TEXT.
  data OBJECT_DATA type T_FM_DATA .
  constants _UUID type TD_CONNUUID value '39AC0A4B07A5A05AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.01' ##NO_TEXT.

  methods LOAD_CLAS
    importing
      !CLASSKEY type SEOCLSKEY
    raising
      ZAPLINK_CX_CONNECTOR .
ENDCLASS.



CLASS ZAPLINK_OBJECT IMPLEMENTATION.


  method CHECK_CLASS_SECTIONS.
  DATA includes      TYPE t_fm_data-includes.
  DATA _msgid        TYPE symsgid.

  IF application_log IS BOUND.    _msgid = application_log->msgid.    application_log->msgid = 'ZAPLINK_OBJECT'.    ENDIF.
  includes = get_includes( classkey-clsname ).

  CALL METHOD _check_class_sections
    EXPORTING
      classkey        = classkey
      pub_source      = includes-pubsec-raw_source
      pro_source      = includes-prosec-raw_source
      pri_source      = includes-prisec-raw_source
      application_log = application_log.

  IF application_log IS BOUND.    application_log->msgid = _msgid.    ENDIF.
  endmethod.


  method CHECK_CLAS_SECTIONS.
  DATA classkey      TYPE seoclskey.

  classkey-clsname = class-class-clsname.

  CALL METHOD _check_class_sections
    EXPORTING
      classkey        = classkey
      pub_source      = class-includes-pubsec-raw_source
      pro_source      = class-includes-prosec-raw_source
      pri_source      = class-includes-prisec-raw_source
      application_log = application_log.
  endmethod.


  method CHECK_INTF_SECTION.
  DATA pubsec_source TYPE seo_section_source.
*  DATA code          TYPE tt_abaprawsource.
  DATA intkey        TYPE seoclskey.

  CHECK interface-interface-state <> seoc_state_model_only.        " Issue 111
  intkey-clsname = interface-interface-clsname.
  CALL FUNCTION 'SEO_INTERFACE_GENERATE_SECTION'
    EXPORTING
      intkey                               = intkey
      return_generated_sections_only       = seox_true
*     TYPEINFO                             =
*     LINE_SIZE                            = 255
*     SUPPRESS_INDEX_UPDATE                = SEOX_FALSE
    IMPORTING
      interface_source                     = pubsec_source
    EXCEPTIONS
      not_existing                         = 1
      model_only                           = 2
      interf_section_not_generated         = 3
      interf_section_not_initialised       = 4
      OTHERS                               = 5.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTERFACE_GENERATE_SECTION'
                                                subrc = sy-subrc
                                            classname = 'ZAPLINK_CX_CONNECTOR' ).
    application_log->add_symsg( ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

*  code = get_prog_rawsource( interface-includes-intfsec ).
  IF pubsec_source = interface-includes-intfsec-raw_source.
    CLEAR pubsec_source.
  ELSE.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE w003 WITH text-pub intkey. ENDIF.
    SET EXTENDED CHECK ON.
    CALL METHOD application_log->add_warning
      EXPORTING
*        id_msgid     =
        id_msgno     = '003'
        id_msgv1     = 'Public section'(pub)
        id_msgv2     = intkey.
*        id_msgv3     =
*            id_msgv4     =
*            id_detlevel  =
*            id_probclass =
  ENDIF.
  IF NOT pubsec_source IS INITIAL.
    CALL FUNCTION 'SEO_INTERFACE_GENERATE_SECTION'
      EXPORTING
        intkey                               = intkey
        return_generated_sections_only       = seox_false
*      TYPEINFO                             =
*      LINE_SIZE                            = 255
*      SUPPRESS_INDEX_UPDATE                = SEOX_FALSE
*    IMPORTING
*      interface_source                     = pubsec_source
      EXCEPTIONS
        not_existing                         = 1
        model_only                           = 2
        interf_section_not_generated         = 3
        interf_section_not_initialised       = 4
        OTHERS                               = 5.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTERFACE_GENERATE_SECTION'
                                                  subrc = sy-subrc
                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
      application_log->add_symsg( ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ENDIF.
  endmethod.


  method CLEANUP_CLASS.
  DATA o_cnx TYPE REF TO zaplink_object.

  CREATE OBJECT o_cnx.
  o_cnx->load_clas( EXPORTING classkey = classkey ).
  has_change = o_cnx->_cleanup_class( ).
  endmethod.


  method CLEANUP_TYPEUSAGE.
  DATA new_code     TYPE tt_abaprawsource.
  DATA t_typegrp    TYPE seot_typepusages_r.
  DATA s_tgroup     LIKE LINE OF t_typegrp.
  DATA s_grp_key    TYPE seotpukey.
  DATA s_clas_key   TYPE seotpukey.

  search_defered_declaration( EXPORTING sourcecode = include-raw_source
                                         typepools = object_data-typeusages
                          IMPORTING new_sourcecode = new_code
                                     new_typepools = t_typegrp ).
  IF NOT new_code IS INITIAL.
    set_prog_rawsource( program = include-name
                            raw = new_code ).
    has_change = abap_true.
  ENDIF.

  IF NOT t_typegrp IS INITIAL.
    SORT t_typegrp BY typegroup.  DELETE ADJACENT DUPLICATES FROM t_typegrp COMPARING typegroup.
    LOOP AT t_typegrp INTO s_tgroup.
      s_tgroup-clsname = object_data-class-clsname.
      s_tgroup-version = object_data-class-version.
*      READ TABLE object_data-typeusages TRANSPORTING NO FIELDS WITH KEY typegroup = s_tgroup-typegroup.
      CASE s_tgroup-tputype.

        WHEN 0.     " Type-pool
          MOVE-CORRESPONDING s_tgroup TO s_grp_key.
          CALL FUNCTION 'SEO_TYPEPUSAGE_GET'
            EXPORTING
              tplkey              = s_grp_key
*             VERSION             = SEOC_VERSION_INACTIVE
*           IMPORTING
*             TYPEPUSAGE          =
            EXCEPTIONS
              not_existing        = 1
              deleted             = 2
              is_clsdeferrd       = 3
              is_intdeferrd       = 4
              OTHERS              = 5.
          IF sy-subrc = 0.
            CALL FUNCTION 'SEO_TYPEPUSAGE_DELETE_W_DEPS'
              EXPORTING
                tplkey        = s_grp_key
                save          = seox_false
              EXCEPTIONS
                not_existing  = 1
                is_clsdeferrd = 2
                is_intdeferrd = 3
                not_deleted   = 4
                db_error      = 5
                OTHERS        = 6.
            IF sy-subrc <> 0.
              o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_TYPEPUSAGE_DELETE_W_DEPS'
                                                          subrc = sy-subrc
                                                      classname = 'ZAPLINK_CX_CONNECTOR' ).
              application_log->add_symsg( ).
              RAISE EXCEPTION o_mycx.
            ENDIF.
          ENDIF.
          CALL FUNCTION 'SEO_TYPEPUSAGE_CREATE_F_DATA'
            EXPORTING
              save                    = seox_false
*          NOT_AFFECT_PUBLIC       = SEOX_FALSE
            CHANGING
              typepusage              = s_tgroup
            EXCEPTIONS
              existing                = 1
              is_clsdeferrd           = 2
              is_intdeferrd           = 3
              not_created             = 4
              db_error                = 5
              OTHERS                  = 6.
          IF sy-subrc <> 0.
            o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_TYPEPUSAGE_CREATE_F_DATA'
                                                        subrc = sy-subrc
                                                    classname = 'ZAPLINK_CX_CONNECTOR' ).
            application_log->add_symsg( ).
            RAISE EXCEPTION o_mycx.
          ENDIF.

        WHEN 1.     " Class
          s_clas_key-clsname = s_tgroup-clsname.      s_clas_key-typegroup = s_tgroup-typegroup.
          CALL FUNCTION 'SEO_CLSDEFERRD_GET'
            EXPORTING
              cdfkey              = s_clas_key
*             VERSION             = SEOC_VERSION_INACTIVE
*           IMPORTING
*             CLSDEFERRD          =
            EXCEPTIONS
              not_existing        = 1
              deleted             = 2
              is_intdeferrd       = 3
              is_typepusage       = 4
              OTHERS              = 5.
          IF sy-subrc = 0.
            CALL FUNCTION 'SEO_CLSDEFERRD_DELETE_W_DEPS'
              EXPORTING
                cdfkey        = s_clas_key
                save          = seox_false
              EXCEPTIONS
                not_existing  = 1
                is_intdeferrd = 2
                is_typepusage = 3
                not_deleted   = 4
                db_error      = 5
                OTHERS        = 6.
            IF sy-subrc <> 0.
              o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLSDEFERRD_DELETE_W_DEPS'
                                                          subrc = sy-subrc
                                                      classname = 'ZAPLINK_CX_CONNECTOR' ).
              application_log->add_symsg( ).
              RAISE EXCEPTION o_mycx.
            ENDIF.
          ENDIF.
          CALL FUNCTION 'SEO_CLSDEFERRD_CREATE_F_DATA'
            EXPORTING
              save                    = seox_false
*             NOT_AFFECT_PUBLIC       = SEOX_FALSE
            CHANGING
              clsdeferrd              = s_tgroup
            EXCEPTIONS
              existing                = 1
              is_intdeferrd           = 2
              is_typepusage           = 3
              not_created             = 4
              db_error                = 5
              OTHERS                  = 6.
          IF sy-subrc <> 0.
            o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLSDEFERRD_CREATE_F_DATA'
                                                        subrc = sy-subrc
                                                    classname = 'ZAPLINK_CX_CONNECTOR' ).
            application_log->add_symsg( ).
            RAISE EXCEPTION o_mycx.
          ENDIF.

        WHEN 2.     " Interface
          s_clas_key-clsname = s_tgroup-clsname.      s_clas_key-typegroup = s_tgroup-typegroup.
          CALL FUNCTION 'SEO_INTDEFERRD_GET'
            EXPORTING
              idfkey              = s_clas_key
*             VERSION             = SEOC_VERSION_INACTIVE
*           IMPORTING
*             INTDEFERRD          =
            EXCEPTIONS
              not_existing        = 1
              deleted             = 2
              is_typepusage       = 3
              is_clsdeferrd       = 4
              OTHERS              = 5.
          IF sy-subrc = 0.
            CALL FUNCTION 'SEO_INTDEFERRD_DELETE_W_DEPS'
              EXPORTING
                idfkey        = s_clas_key
                save          = seox_false
              EXCEPTIONS
                not_existing  = 1
                is_typepusage = 2
                is_clsdeferrd = 3
                not_deleted   = 4
                db_error      = 5
                OTHERS        = 6.
            IF sy-subrc <> 0.
              o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTDEFERRD_DELETE_W_DEPS'
                                                          subrc = sy-subrc
                                                      classname = 'ZAPLINK_CX_CONNECTOR' ).
              application_log->add_symsg( ).
              RAISE EXCEPTION o_mycx.
            ENDIF.
          ENDIF.
          CALL FUNCTION 'SEO_INTDEFERRD_CREATE_F_DATA'
            EXPORTING
              save                    = seox_false
*             NOT_AFFECT_PUBLIC       = SEOX_FALSE
            CHANGING
              intdeferrd              = s_tgroup
            EXCEPTIONS
              existing                = 1
              is_typepusage           = 2
              is_clsdeferrd           = 3
              not_created             = 4
              db_error                = 5
              OTHERS                  = 6.
          IF sy-subrc <> 0.
            o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTDEFERRD_CREATE_F_DATA'
                                                        subrc = sy-subrc
                                                    classname = 'ZAPLINK_CX_CONNECTOR' ).
            application_log->add_symsg( ).
            RAISE EXCEPTION o_mycx.
          ENDIF.

        WHEN OTHERS.
          ASSERT s_tgroup-tputype >= 0 AND s_tgroup-tputype <= 2.
      ENDCASE.
      INSERT s_tgroup INTO TABLE object_data-typeusages.
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE w002 WITH s_tgroup-typegroup object_data-class-clsname text. ENDIF.
      SET EXTENDED CHECK ON.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     =
          id_msgno     = '002'
          id_msgv1     = s_tgroup-typegroup
          id_msgv2     = object_data-class-clsname
          id_msgv3     = text
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
    ENDLOOP.
  ENDIF.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  application_log->msgid = 'ZAPLINK_OBJECT'.
  type-type = zaplink_object_data=>object_types-class.       INSERT type INTO TABLE supported_types.
  type-type = zaplink_object_data=>object_types-interface.    INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  CALL FUNCTION 'SEO_BUFFER_INIT'.        " Init buffers
  endmethod.


  method EXPORT_FROM_SAP_CLASS.
  DATA obj_mask   TYPE doku_obj.
  DATA docu_key   TYPE zaplink_object_data=>ts_doc_key.
  DATA classkey   TYPE seoclskey.
*  DATA _d         TYPE t_fm_data.
  DATA d_flag     TYPE abap_bool.

  TRY.
    CHECK component->get_type( ) = zaplink_object_data=>object_types-class.

    classkey-clsname = component->get_name( ).
    CALL FUNCTION 'SEO_BUFFER_REFRESH'.
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = classkey
        version      = seoc_version_inactive     " Issue 111
        state        = seoc_state_model_only     " Issue 111
      IMPORTING
        class        = object_data-class
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SEO_CLASS_GET' sy-subrc.
    ENDIF.

    load_clas( EXPORTING classkey = classkey ).

* Includes
    object_data-includes = get_includes( object_data-class-clsname ).

* Issue 71 : Code signature different due to slight change in sections
    check_clas_sections( object_data ).
* Issue 71 : Code signature different due to implicite type-pool declaration within public, protected or private section
* From SEO_CLIF_SAVE_ALL
** Clean-up unused implicite type pool usages (MS 20060921)
*      DATA type_pool_usage_cleaner TYPE REF TO lcl_type_pool_usage_cleaner.
*      TRY.
*          type_pool_usage_cleaner = lcl_type_pool_usage_cleaner=>create( classkey ).
*          type_pool_usage_cleaner->clean_up( ).
*        CATCH lcx_internal_error.                       "#EC NO_HANDLER
**     No handler needed here => default behavior
*      ENDTRY.

*      DELETE OBJECT_DATA-typeusages WHERE version <> OBJECT_DATA-class-version.   " Remove type usage of the wrong version
* Type-pools declaration might be present within method's declaration section (auto added by SAP when needed).
* Have to detect it, add type pool to type pool list and delete type pool declaration
    d_flag = _cleanup_class( ).

    IF NOT d_flag IS INITIAL.   load_clas( EXPORTING classkey = classkey ).   ENDIF.
* End of Issue 71

* Documentations
    docu_key-name = component->get_name( ).
    docu_key-other = '*'.
    obj_mask = docu_key.

    object_data-documentation = zaplink_documentation=>get( ids = zaplink_object_data=>r_doc_ids-class
                                                object = obj_mask ).

    CREATE OBJECT object.
    object->complete_data( CHANGING fm_data = object_data ).
    object_data-dynpros = get_dynpros( object_data-includes-classpool-name ). " Issue 8
    object_data-menupainter = get_menus( object_data-includes-classpool-name ). " Issue 9
    object_data-textspool = get_prog_textpool( object_data-includes-classpool-name ). " Issue 1
    object->from_data( object_data ).
    object->_code_signature = get_class_signature( classkey ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_FROM_SAP_INTERFACE.
  DATA obj_mask       TYPE doku_obj.
  DATA docu_key       TYPE zaplink_object_data=>ts_doc_key.
  DATA classkey TYPE seoclskey.
  DATA _d       TYPE t_fm_data.

  TRY.

    CHECK component->get_type( ) = zaplink_object_data=>object_types-interface.

    classkey-clsname = component->get_name( ).
    CALL FUNCTION 'SEO_BUFFER_REFRESH'.
    CALL FUNCTION 'SEO_INTERFACE_GET'
      EXPORTING
        intkey             = classkey
        version            = seoc_version_inactive   " Issue 111
        state              = seoc_state_model_only   " Issue 111
      IMPORTING
        INTERFACE          = _d-interface
      EXCEPTIONS
        NOT_EXISTING       = 1
        DELETED            = 2
        IS_CLASS           = 3
        MODEL_ONLY         = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SEO_INTERFACE_GET' sy-subrc.
    ENDIF.

    CALL FUNCTION 'SEO_INTERFACE_TYPEINFO_GET'
      EXPORTING
        intkey                        = classkey
        version                       = _d-interface-version
        state                         = _d-interface-state
        with_master_language          = seox_true
        with_enhancements             = seox_true
*     READ_ACTIVE_ENHA              = SEOX_FALSE
      IMPORTING
        interface                     = _d-interface
        attributes                    = _d-attributes
        methods                       = _d-methods
        events                        = _d-events
        PARAMETERS                    = _d-parameters
        exceps                        = _d-exceps
        comprisings                   = _d-comprisings
        typepusages                   = _d-typeusages
        clsdeferrds                   = _d-clsdeferrds
        intdeferrds                   = _d-intdeferrds
        explore_comprisings           = _d-explore_comprisings
        aliases                       = _d-aliases
        types                         = _d-types
        enhancement_methods           = _d-enhancement_methods
        enhancement_attributes        = _d-enhancement_attributes
        enhancement_events            = _d-enhancement_events
        enhancement_comprisings       = _d-enhancement_comprisings
      EXCEPTIONS
        not_existing                  = 1
        is_class                      = 2
        model_only                    = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SEO_INTERFACE_TYPEINFO_GET' sy-subrc.
    ENDIF.

* Includes
    _d-includes = get_includes( _d-interface-clsname ).

* Issue 71 : Code signature different due to slight change in sections
    check_intf_section( _d ).

* Documentations
    docu_key-name = component->get_name( ).
    docu_key-other = '*'.
    obj_mask = docu_key.

    _d-documentation = zaplink_documentation=>get( ids = zaplink_object_data=>r_doc_ids-interface
                                                object = obj_mask ).

    CREATE OBJECT object.
    object->complete_data( CHANGING fm_data = _d ).
    object->from_data( _d ).
    object->_code_signature = get_interface_signature( classkey ).


    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method GENERATE_CLASS.
  DATA t_comps      TYPE tt_compkeys.
  DATA o_activ      TYPE REF TO zaplink_activate.
  DATA s_comp       LIKE LINE OF t_comps.
  DATA t_err        TYPE tt_compkeys.

  s_comp-type = zaplink_object_data=>object_types-class.    s_comp-name = class.
  APPEND s_comp TO t_comps.
  CREATE OBJECT o_activ.
  o_activ->add_keys( t_comps ).
  t_err = o_activ->generate( ).
  IF t_err IS INITIAL.    result = abap_true.    ENDIF.
  endmethod.


  method GET_CLASSSOURCE_4SIGNATURE.
  DATA t_upcode    TYPE td_sourcecode.
  DATA _line       LIKE LINE OF t_upcode.
  DATA d_class     TYPE  vseoclass.
  DATA t_attrs     TYPE  seoo_attributes_r.
  FIELD-SYMBOLS:
    <a> LIKE LINE OF t_attrs.

  CALL FUNCTION 'SEO_CLASS_GET'
    EXPORTING
      clskey       = class_name
      version      = seoc_version_inactive
      state        = '0'
    IMPORTING
      class        = d_class
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_interface = 3
      model_only   = 4
      OTHERS       = 5.
  IF sy-subrc <> 0.
    RAISE not_a_class.
  ENDIF.
  result = get_class_globalsource( class_name ).
  zaplink_tools=>pretty_printer( CHANGING table = result ).
  IF d_class-category = '40'.  " 40	Exception Class
* Issue 71 : Exception class SOTR GUID
    CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
      EXPORTING
        clskey                              = class_name
*       VERSION                             = SEOC_VERSION_INACTIVE
*       STATE                               = '1'
*       WITH_DESCRIPTIONS                   = SEOX_TRUE
*       RESOLVE_EVENTHANDLER_TYPEINFO       = SEOX_FALSE
*       WITH_MASTER_LANGUAGE                = SEOX_FALSE
*       WITH_ENHANCEMENTS                   = SEOX_FALSE
*       READ_ACTIVE_ENHA                    = SEOX_FALSE
      IMPORTING
*       CLASS                               =
        attributes                          = t_attrs
*       METHODS                             =
*       EVENTS                              =
*       TYPES                               =
*       PARAMETERS                          =
*       EXCEPS                              =
*       IMPLEMENTINGS                       =
*       INHERITANCE                         =
*       REDEFINITIONS                       =
*       IMPL_DETAILS                        =
*       FRIENDSHIPS                         =
*       TYPEPUSAGES                         =
*       CLSDEFERRDS                         =
*       INTDEFERRDS                         =
*       EXPLORE_INHERITANCE                 =
*       EXPLORE_IMPLEMENTINGS               =
*       ALIASES                             =
*       ENHANCEMENT_METHODS                 =
*       ENHANCEMENT_ATTRIBUTES              =
*       ENHANCEMENT_EVENTS                  =
*       ENHANCEMENT_IMPLEMENTINGS           =
      EXCEPTIONS
        not_existing                        = 1
        is_interface                        = 2
        model_only                          = 3
        OTHERS                              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DELETE t_attrs
           WHERE   exposure <> 2    " Public
              OR attdecltyp <> 2    " Cosntant
              OR NOT type CP 'SOTR_CONC'.
    LOOP AT t_attrs ASSIGNING <a>.
      REPLACE ALL OCCURRENCES OF <a>-attvalue IN TABLE result WITH space.   " Issue 71, remove SOTR GUID values when calculating code signature
    ENDLOOP.
  ENDIF.
*  Issue 71 : dot at the end. => Corrected with Pretty Printer.
*  LOOP AT result ASSIGNING <t>.
*    REPLACE FIRST OCCURRENCE OF REGEX '\.$' IN <t> WITH ' .'.   " change '.' to ' .'
*  ENDLOOP.
  zaplink_tools=>condense_abap_source( CHANGING table = result ).
  t_upcode = result.
  zaplink_tools=>conv_table_2upcase( CHANGING table = t_upcode ).

*Remove text before CLASS Zxxx DEFINITION
  CONCATENATE 'CLASS' class_name 'DEFINITION' INTO _line SEPARATED BY space.
  READ TABLE t_upcode TRANSPORTING NO FIELDS  FROM _line.
  CHECK sy-subrc = 0.
  SUBTRACT 1 FROM sy-tabix.   CHECK sy-tabix >= 1.    DELETE result TO sy-tabix.
  endmethod.


  method GET_CLASS_GLOBALSOURCE.
*  DATA d_size TYPE i.
*  FIELD-SYMBOLS <line> TYPE string.
* Copy of CL_OO_SOURCE->CONSTRUCTOR
*  DATA cpool TYPE seo_section_source.
*  DATA pubsec TYPE seo_section_source.
*  DATA prisec TYPE seo_section_source.
*  DATA prosec TYPE seo_section_source.
*  DATA cl TYPE seo_section_source.
*  DATA ccdef TYPE seop_source_string.
*  DATA ccimp TYPE seop_source_string.
*  DATA ccmac TYPE seop_source_string.

  DATA includes TYPE zaplink_object_data=>ts_includes.      " Issue 82
*  DATA incname TYPE program.
*  DATA pubsec_incname TYPE program.
*  DATA prisec_incname TYPE program.
*  DATA prosec_incname TYPE program.
*  DATA cl_incname TYPE program.
*  DATA cl_test_incname TYPE program.
*  DATA ccdef_incname TYPE program.
*  DATA ccimp_incname TYPE program.
*  DATA ccmac_incname TYPE program.

*  DATA meth_inc TYPE seop_method_w_include.
*  DATA meth_incs TYPE seop_methods_w_include.
*  DATA source_tmp TYPE seop_source_string.

  FIELD-SYMBOLS:
    <m> LIKE LINE OF includes-methods,
    <source_line> LIKE LINE OF includes-classpool-raw_source.

  includes = get_includes( class_name-clsname ).            " Issue 82
  CHECK NOT includes-classpool-name IS INITIAL. " check it's a class
*  incname = cl_oo_classname_service=>get_classpool_name( class_name-clsname ).   " Issue 82
*  cpool = get_prog_rawsource( includes-incname ).                                          " Issue 73
*  READ REPORT incname INTO cpool STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT incname INTO cpool STATE 'A'.
*  ENDIF.

* pubsec_incname = cl_oo_classname_service=>get_pubsec_name( class_name-clsname )." Issue 82
*  pubsec = get_prog_rawsource( pubsec_incname ).                                  " Issue 73
*  READ REPORT pubsec_incname INTO pubsec STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT pubsec_incname INTO pubsec STATE 'A'.
*  ENDIF.

* prosec_incname = cl_oo_classname_service=>get_prosec_name( class_name-clsname )." Issue 82
*  prosec = get_prog_rawsource( prosec_incname ).                                  " Issue 73
*  READ REPORT prosec_incname INTO prosec STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT prosec_incname INTO prosec STATE 'A'.
*  ENDIF.

* prisec_incname = cl_oo_classname_service=>get_prisec_name( class_name-clsname )." Issue 82
*  prisec = get_prog_rawsource( prisec_incname ).                                  " Issue 73
*  READ REPORT prisec_incname INTO prisec STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT prisec_incname INTO prisec STATE 'A'.
*  ENDIF.

*  cl_incname = cl_oo_classname_service=>get_cl_name( class_name-clsname ).       " Issue 82
*  cl = get_prog_rawsource( cl_incname ).                                          " Issue 73
*  READ REPORT cl_incname INTO cl STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT cl_incname INTO cl STATE 'A'.
*  ENDIF.

*  ccdef_incname = cl_oo_classname_service=>get_ccdef_name( class_name-clsname ). " Issue 82
*  ccdef = get_prog_rawsource( ccdef_incname ).                                    " Issue 73
*  READ REPORT ccdef_incname INTO ccdef STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT ccdef_incname INTO ccdef STATE 'A'.
*  ENDIF.

*  ccimp_incname = cl_oo_classname_service=>get_ccimp_name( class_name-clsname ). " Issue 82
*  ccimp = get_prog_rawsource( ccimp_incname ).                                    " Issue 73
*  READ REPORT ccimp_incname INTO ccimp STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT ccimp_incname INTO ccimp STATE 'A'.
*  ENDIF.

*  ccmac_incname = cl_oo_classname_service=>get_ccmac_name( class_name-clsname ). " Issue 82
*  ccmac = get_prog_rawsource( ccmac_incname ).                                    " Issue 73
*  READ REPORT ccmac_incname INTO ccmac STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT ccmac_incname INTO ccmac STATE 'A'.
*  ENDIF.

*  meth_incs = cl_oo_classname_service=>get_all_method_includes( class_name-clsname ). " Issue 82

* Issue 68 : Methods are in historical order on the development system and in alphabetic order on a fresh install system
  SORT includes-methods BY cpdkey.         " Issue 68 : Force alphabetic order
* End of change in Copy of CL_OO_SOURCE->CONSTRUCTOR

  LOOP AT includes-classpool-raw_source ASSIGNING <source_line>.
    IF <source_line> CS includes-pubsec-name.
      APPEND LINES OF includes-pubsec-raw_source TO result.
    ELSEIF <source_line> CS includes-prosec-name.
      APPEND LINES OF includes-prosec-raw_source TO result.
    ELSEIF <source_line> CS includes-prisec-name.
      APPEND LINES OF includes-prisec-raw_source TO result.
    ELSEIF <source_line> CS includes-cl-name.
      APPEND LINES OF includes-cl-raw_source TO result.
    ELSEIF <source_line> CS includes-ccdef-name.
      APPEND LINES OF includes-ccdef-raw_source TO result.
    ELSEIF <source_line> CS includes-ccimp-name.
      APPEND LINES OF includes-ccimp-raw_source TO result.
    ELSEIF <source_line> CS includes-ccmac-name.
      APPEND LINES OF includes-ccmac-raw_source TO result.
    ELSEIF <source_line> CS includes-localtestclass-name.   " Ignore this include
    ELSEIF <source_line> CS 'include methods'.
      LOOP AT includes-methods ASSIGNING <m>
        WHERE NOT raw_source IS INITIAL.
*        source_tmp = get_prog_rawsource( meth_inc-incname ).                     " Issue 73
**        READ REPORT meth_inc-incname INTO source_tmp STATE 'I'.
**        IF sy-subrc <> 0.
**          READ REPORT meth_inc-incname INTO source_tmp STATE 'A'.
**        ENDIF.
**        IF sy-subrc = 0.
*        IF NOT source_tmp IS INITIAL.
        APPEND LINES OF <m>-raw_source TO result.
*        ENDIF.
      ENDLOOP.
    ELSE.
      APPEND <source_line> TO result.
    ENDIF .
  ENDLOOP.
  endmethod.


  method GET_CLASS_SIGNATURE.
  DATA t_code    TYPE td_sourcecode.
  DATA d_src     TYPE string.
  DATA _key      TYPE string.

  t_code = get_classsource_4signature( class_name ).
  d_src = zaplink_tools=>table_2_string( t_code ).
  d_src = zaplink_tools=>clean_abap_string( d_src ).
  result = zaplink_tools=>calculate_md5_hash( d_src ).
  CONCATENATE zaplink_object_data=>object_types-class '/' class_name INTO _key.
  LOG-POINT ID zaplink  SUBKEY _key FIELDS result d_src.
  endmethod.


  method GET_INCLUDES.
* Copy of CL_OO_SOURCE->CONSTRUCTOR
  DATA d_cifkey  TYPE seoclskey.
  DATA d_clstype TYPE seoclstype.
  DATA t_methods TYPE seop_methods_w_include.
  DATA s_method  LIKE LINE OF includes-methods.
  FIELD-SYMBOLS <m> LIKE LINE OF t_methods.

  DEFINE lmac_get_load.
    includes-&1-name = cl_oo_classname_service=>&2( clsname ).
    if not includes-&1-name is initial.   includes-&1-raw_source = get_prog_rawsource( includes-&1-name ).    endif.
  END-OF-DEFINITION.

  d_cifkey-clsname = clsname.
  CALL FUNCTION 'SEO_CLIF_GET'
    EXPORTING
      cifkey             = d_cifkey
*     VERSION            = SEOC_VERSION_INACTIVE
*     STATE              = '0'
    IMPORTING
      clstype            = d_clstype
*     CLASS              =
*     INTERFACE          =
    EXCEPTIONS
      not_existing       = 1
      deleted            = 2
      model_only         = 3
      OTHERS             = 4.
  IF sy-subrc = 0.
    IF d_clstype EQ 0.
      lmac_get_load classpool get_classpool_name.
      lmac_get_load pubsec get_pubsec_name.
      lmac_get_load prosec get_prosec_name.
      lmac_get_load prisec get_prisec_name.
      lmac_get_load cl get_cl_name.
      lmac_get_load ccdef get_ccdef_name.
      lmac_get_load ccimp get_ccimp_name.
      lmac_get_load ccmac get_ccmac_name.
      lmac_get_load localtestclass get_local_testclasses_include. " Issue 82
      CALL METHOD cl_oo_classname_service=>get_all_method_includes
        EXPORTING
          clsname            = clsname
        RECEIVING
          result             = t_methods
        EXCEPTIONS
          class_not_existing = 0
          OTHERS             = 0.   " Ignore exception : Class may not exits
      LOOP AT t_methods ASSIGNING <m>.
        CLEAR s_method.   s_method-hdr = <m>.
        s_method-raw_source = get_prog_rawsource( <m>-incname ).
        APPEND s_method TO includes-methods.
      ENDLOOP.
    ELSEIF d_clstype EQ 1.
      lmac_get_load interfacepool get_interfacepool_name.
      lmac_get_load intfsec get_intfsec_name.
    ENDIF.
  ENDIF.
  endmethod.


  method GET_INTERFACE_GLOBALSOURCE.
  DATA d_prog     TYPE program.
  DATA t_upcode   TYPE td_sourcecode.
  DATA _line      LIKE LINE OF t_upcode.

  d_prog = cl_oo_classname_service=>get_intfsec_name( interface_name-clsname ).

  READ REPORT d_prog INTO result.
  zaplink_tools=>pretty_printer( CHANGING table = result ).
  zaplink_tools=>condense_abap_source( CHANGING table = result ).
  t_upcode = result.    zaplink_tools=>conv_table_2upcase( CHANGING table = t_upcode ).

*Remove text before CLASS Zxxx DEFINITION
  CONCATENATE 'INTERFACE' interface_name INTO _line SEPARATED BY space.
  READ TABLE t_upcode TRANSPORTING NO FIELDS  FROM _line.
  CHECK sy-subrc = 0.
  SUBTRACT 1 FROM sy-tabix.   CHECK sy-tabix >= 1.   DELETE result TO sy-tabix.
  endmethod.


  method GET_INTERFACE_SIGNATURE.
  DATA t_code    TYPE td_sourcecode.
  DATA d_src     TYPE string.

  t_code = get_interface_globalsource( interface_name ).
  d_src = zaplink_tools=>table_2_string( t_code ).
  d_src = zaplink_tools=>clean_abap_string( d_src ).
  result = zaplink_tools=>calculate_md5_hash( d_src ).
  endmethod.


  method IMPORT_TO_SAP_CLASS.
  DATA _d               TYPE t_fm_data.
  DATA i_korrnr         TYPE trkorr.              "#EC NEEDED for debug
  DATA _component       TYPE to_component.
  DATA _name            TYPE td_compname.
  DATA d_key            TYPE seoclskey.
  DATA f_exists         TYPE abap_bool.

  TRY.

    _d = o_data->to_data( ).

* Exceptions texts
    IF NOT _d-exceptions_texts IS INITIAL.
      zaplink_clas_data=>update_concept( EXPORTING msg_col = application_log
                                         CHANGING  fm_data = _d ).
    ENDIF.

    d_key-clsname = _d-class-clsname.
    CALL FUNCTION 'SEO_CLIF_EXISTENCE_CHECK'                " Issue 119
      EXPORTING
        cifkey              = d_key
      EXCEPTIONS
        not_specified       = 1
        not_existing        = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      f_exists = abap_true.
    ENDIF.

* Allready done in overwrite
*    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
*      EXPORTING
*        clskey        = d_key
**      IMPORTING
**        not_active    = not_active
*      EXCEPTIONS
*        not_specified = 1
*        not_existing  = 2
*        is_interface  = 3
*        no_text       = 4
*        inconsistent  = 5
*        OTHERS        = 6.
*    IF sy-subrc = 0.
*      CALL FUNCTION 'SEO_CLASS_DELETE_W_DEPS'
*        EXPORTING
*          clskey       = d_key
*        EXCEPTIONS
*          not_existing = 1
*          is_interface = 2
*          not_deleted  = 3
*          db_error     = 4
*          OTHERS       = 5.
*      IF sy-subrc <> 0.
*        application_log->add_symsg( ).
*      ENDIF.
*    ENDIF.

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
     EXPORTING
*     corrnr                             = e_corrnr
*     devclass                           = '$TMP'                "devclass
*     version                            = e_version
*     genflag                            = e_genflag
*     authority_check                    = e_authority_check
       overwrite                          = 'X'                   "overwrite
*   SUPPRESS_METHOD_GENERATION         = e_suppress_meth_gen
*   SUPPRESS_REFACTORING_SUPPORT       = e_suppress_refac_gen
       method_sources                     = _d-method_sources    " Methods source
       locals_def                         = _d-locals_def        " LOCAL_TYPES
       locals_imp                         = _d-locals_imp        " LOCAL_IMP
       locals_mac                         = _d-locals_mac        " MACROS
*   SUPPRESS_INDEX_UPDATE              = e_suppress_ind_update
     IMPORTING
       korrnr                             = i_korrnr
     TABLES
       class_descriptions                 = _d-class_desc
       component_descriptions             = _d-component_desc
       subcomponent_descriptions          = _d-subcomponent_desc
     CHANGING
       class                              = _d-class
       inheritance                        = _d-inheritance
       redefinitions                      = _d-redefinitions
       implementings                      = _d-implementings
       impl_details                       = _d-impl_details
       attributes                         = _d-attributes
       methods                            = _d-methods
       events                             = _d-events
       types                              = _d-types
*   TYPE_SOURCE                        = ch_type_source "???
       PARAMETERS                         = _d-parameters
       exceps                             = _d-exceps
       aliases                            = _d-aliases
       typepusages                        = _d-typeusages
       clsdeferrds                        = _d-clsdeferrds
       intdeferrds                        = _d-intdeferrds
       friendships                        = _d-friendships
     EXCEPTIONS
       existing                           = 1
       is_interface                       = 2
       db_error                           = 3
       component_error                    = 4
       no_access                          = 5
       other                              = 6
       OTHERS                             = 7.
    IF sy-subrc = 4 AND f_exists IS INITIAL.                " Issue 119
      CALL FUNCTION 'SEO_BSP_CLASS_DELETE_COMPLETE'         " Brute force delete
        EXPORTING
          clskey = d_key.
* Do it again
      CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
       EXPORTING
*     corrnr                             = e_corrnr
*     devclass                           = '$TMP'                "devclass
*     version                            = e_version
*     genflag                            = e_genflag
*     authority_check                    = e_authority_check
         overwrite                          = 'X'                   "overwrite
*   SUPPRESS_METHOD_GENERATION         = e_suppress_meth_gen
*   SUPPRESS_REFACTORING_SUPPORT       = e_suppress_refac_gen
         method_sources                     = _d-method_sources    " Methods source
         locals_def                         = _d-locals_def        " LOCAL_TYPES
         locals_imp                         = _d-locals_imp        " LOCAL_IMP
         locals_mac                         = _d-locals_mac        " MACROS
*   SUPPRESS_INDEX_UPDATE              = e_suppress_ind_update
       IMPORTING
         korrnr                             = i_korrnr
       TABLES
         class_descriptions                 = _d-class_desc
         component_descriptions             = _d-component_desc
         subcomponent_descriptions          = _d-subcomponent_desc
       CHANGING
         class                              = _d-class
         inheritance                        = _d-inheritance
         redefinitions                      = _d-redefinitions
         implementings                      = _d-implementings
         impl_details                       = _d-impl_details
         attributes                         = _d-attributes
         methods                            = _d-methods
         events                             = _d-events
         types                              = _d-types
*   TYPE_SOURCE                        = ch_type_source "???
         PARAMETERS                         = _d-parameters
         exceps                             = _d-exceps
         aliases                            = _d-aliases
         typepusages                        = _d-typeusages
         clsdeferrds                        = _d-clsdeferrds
         intdeferrds                        = _d-intdeferrds
         friendships                        = _d-friendships
       EXCEPTIONS
         existing                           = 1
         is_interface                       = 2
         db_error                           = 3
         component_error                    = 4
         no_access                          = 5
         other                              = 6
         OTHERS                             = 7.
    ENDIF.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_CREATE_COMPLETE'
                                                  subrc = sy-subrc
                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
      application_log->add_symsg( ).
      RAISE EXCEPTION o_mycx.
    ENDIF.

    _d-includes = get_includes( _d-class-clsname ).

    IF NOT _d-class-with_unit_tests IS INITIAL.             " Issue 82
      d_key-clsname = _d-class-clsname.
      CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
        EXPORTING
          clskey                       = d_key
*         FORCE                        = SEOX_FALSE
*         CORRNR                       =
*         IMPLEMENTATION               =
*         LOCALS_DEF                   =
*         LOCALS_IMP                   =
*         LOCALS_MAC                   =
          locals_testclasses           = _d-locals_tst
*         SUPPRESS_CORR                = SEOX_FALSE
       EXCEPTIONS
         not_existing                 = 1
         model_only                   = 2
         locals_not_generated         = 3
         locals_not_initialised       = 4
         OTHERS                       = 5.
      IF sy-subrc <> 0.
        o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_GENERATE_LOCALS'
                                                    subrc = sy-subrc
                                                classname = 'ZAPLINK_CX_CONNECTOR' ).
        application_log->add_symsg( ).
        RAISE EXCEPTION o_mycx.
      ENDIF.
    ENDIF.                                          " End of Issue 82

    set_dynpros( program = _d-includes-classpool-name       " Issue 8
                 dynpros = _d-dynpros ).
    set_menus( program = _d-includes-classpool-name         " Issue 9
                 menus = _d-menupainter ).
    set_prog_textpool( program = _d-includes-classpool-name " Issue 1
                      textpool = _d-textspool ).

    zaplink_documentation=>set( _d-documentation ).

    zaplink_object=>check_class_sections( classkey = d_key
                                   application_log = application_log ).

    CREATE OBJECT _component.
    _component->set_type( zaplink_object_data=>object_types-class ).
    _name = _d-class-clsname.
    _component->set_name( _name ).
    CREATE OBJECT components.
    components->add( _component ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TO_SAP_INTERFACE.
  DATA _d           TYPE t_fm_data.
  DATA i_korrnr     TYPE trkorr.                  "#EC NEEDED for debug
  DATA _component   TYPE to_component.
  DATA _name        TYPE td_compname.
* Patch SAP do not create type alias.
  DATA intkey       TYPE seoclskey.
  DATA type         TYPE vseotype.
  DATA f_exists     TYPE abap_bool.
  FIELD-SYMBOLS <alias> LIKE LINE OF _d-aliases.

  TRY.

    _d = o_data->to_data( ).
    MOVE-CORRESPONDING _d-interface TO intkey.

    CALL FUNCTION 'SEO_CLIF_EXISTENCE_CHECK'      " Issue 119
      EXPORTING
        cifkey              = intkey
      EXCEPTIONS
        NOT_SPECIFIED       = 1
        NOT_EXISTING        = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      f_exists = abap_true.
    ENDIF.

    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
      EXPORTING
*        CORRNR                             =
*        devclass                           = '$TMP'
*        VERSION                            = SEOC_VERSION_INACTIVE
*        GENFLAG                            = ' '
*        AUTHORITY_CHECK                    = SEOX_TRUE
        overwrite                          = seox_true
*        SUPPRESS_REFACTORING_SUPPORT       = SEOX_TRUE
      IMPORTING
        korrnr                             = i_korrnr
      TABLES
        class_descriptions                 = _d-class_desc
        component_descriptions             = _d-component_desc
        subcomponent_descriptions          = _d-subcomponent_desc
      CHANGING
        interface                          = _d-interface
        comprisings                        = _d-comprisings
        attributes                         = _d-attributes
        methods                            = _d-methods
        events                             = _d-events
        PARAMETERS                         = _d-parameters
        exceps                             = _d-exceps
        aliases                            = _d-aliases
        typepusages                        = _d-typeusages
        clsdeferrds                        = _d-clsdeferrds
        intdeferrds                        = _d-intdeferrds
        types                              = _d-types
      EXCEPTIONS
        existing                           = 1
        is_class                           = 2
        db_error                           = 3
        component_error                    = 4
        no_access                          = 5
        other                              = 6
        OTHERS                             = 7.
    IF sy-subrc = 4 AND f_exists IS INITIAL.            " Issue 119
      CALL FUNCTION 'SEO_BSP_CLASS_DELETE_COMPLETE'     " Brute force delete
        EXPORTING
          clskey = intkey.
* Do it again
      CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
        EXPORTING
*        CORRNR                             =
*        devclass                           = '$TMP'
*        VERSION                            = SEOC_VERSION_INACTIVE
*        GENFLAG                            = ' '
*        AUTHORITY_CHECK                    = SEOX_TRUE
          overwrite                          = seox_true
*        SUPPRESS_REFACTORING_SUPPORT       = SEOX_TRUE
        IMPORTING
          korrnr                             = i_korrnr
        TABLES
          class_descriptions                 = _d-class_desc
          component_descriptions             = _d-component_desc
          subcomponent_descriptions          = _d-subcomponent_desc
        CHANGING
          interface                          = _d-interface
          comprisings                        = _d-comprisings
          attributes                         = _d-attributes
          methods                            = _d-methods
          events                             = _d-events
          PARAMETERS                         = _d-parameters
          exceps                             = _d-exceps
          aliases                            = _d-aliases
          typepusages                        = _d-typeusages
          clsdeferrds                        = _d-clsdeferrds
          intdeferrds                        = _d-intdeferrds
          types                              = _d-types
        EXCEPTIONS
          existing                           = 1
          is_class                           = 2
          db_error                           = 3
          component_error                    = 4
          no_access                          = 5
          other                              = 6
          OTHERS                             = 7.
    ENDIF.
    IF sy-subrc <> 0.                                       " Issue 118
      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTERFACE_CREATE_COMPLETE'
                                                  subrc = sy-subrc
                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
      application_log->add_symsg( ).
      RAISE EXCEPTION o_mycx.
    ENDIF.

* Patch SAP do not create type alias.
    LOOP AT _d-aliases ASSIGNING <alias>
            WHERE cmptype = seoo_cmptype_type.
      MOVE-CORRESPONDING <alias> TO type.
      type-alias = seox_true.
      type-state = seoc_state_implemented.
      CALL FUNCTION 'SEO_TYPE_CREATE_F_DATA'
        EXPORTING
          save   = seox_false
        CHANGING
          type   = type
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        IF sy-msgid = 'OO' AND sy-msgno = '002'.            " Issue 113
          IF 1 = 2. MESSAGE w002(oo) WITH type-cmpname.   ENDIF.
        ELSE.
          CALL METHOD application_log->add_symsg
            EXPORTING
              id_msgty = 'W'.
        ENDIF.
      ENDIF.
    ENDLOOP.
* End of patch

    CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
      EXPORTING
        cifkey                        = intkey
        suppress_refactoring_entries  = seox_true
        suppress_modification_support = seox_true
*    CHANGING
*      corrnr                        = corrnr
*      devclass                      = devclass
*      genflag                       = genflag
      EXCEPTIONS
        db_error                      = 1
        OTHERS                        = 2.
    IF sy-subrc <> 0.
      CALL METHOD application_log->add_symsg
        EXPORTING
          id_msgty = 'W'.
    ENDIF.

    zaplink_documentation=>set( _d-documentation ).

    CREATE OBJECT _component.
    _component->set_type( zaplink_object_data=>object_types-interface ).
    _name = _d-interface-clsname.
    _component->set_name( _name ).
    CREATE OBJECT components.
    components->add( _component ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method LOAD_CLAS.
  CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
    EXPORTING
      clskey                              = classkey
      version                             = object_data-class-version
      state                               = object_data-class-state
*     WITH_DESCRIPTIONS                   = SEOX_TRUE
      resolve_eventhandler_typeinfo       = seox_true
      with_master_language                = seox_true
      with_enhancements                   = seox_true
*     READ_ACTIVE_ENHA                    = SEOX_FALSE
    IMPORTING
      class                               = object_data-class
      attributes                          = object_data-attributes
      methods                             = object_data-methods
      events                              = object_data-events
      types                               = object_data-types
      PARAMETERS                          = object_data-parameters
      exceps                              = object_data-exceps
      implementings                       = object_data-implementings
      inheritance                         = object_data-inheritance
      redefinitions                       = object_data-redefinitions
      impl_details                        = object_data-impl_details
      friendships                         = object_data-friendships
      typepusages                         = object_data-typeusages
      clsdeferrds                         = object_data-clsdeferrds
      intdeferrds                         = object_data-intdeferrds
      explore_inheritance                 = object_data-explore_inheritance
      explore_implementings               = object_data-explore_implementings
      aliases                             = object_data-aliases
      enhancement_methods                 = object_data-enhancement_methods
      enhancement_attributes              = object_data-enhancement_attributes
      enhancement_events                  = object_data-enhancement_events
      enhancement_implementings           = object_data-enhancement_implementings
    EXCEPTIONS
      not_existing                        = 1
      is_interface                        = 2
      model_only                          = 3
      OTHERS                              = 4.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_TYPEINFO_GET'
                                                subrc = sy-subrc
                                            classname = 'ZAPLINK_CX_CONNECTOR' ).
    application_log->add_symsg( ).
    RAISE EXCEPTION o_mycx.
  ENDIF.
* Begin of Issue 128 : Get Inheritance informations
  CALL FUNCTION 'SEO_CLASS_RESOLVE_INHERITANCE'
    EXPORTING
      clskey                              = classkey
      version                             = object_data-class-version
      state                               = object_data-class-state
*     RESOLVE_EVENTHANDLER_TYPEINFO       = SEOX_FALSE
*      WITH_ENHANCEMENTS                   = SEOX_TRUE
*     IGNORE_SWITCHES                     = 'X'
    IMPORTING
      attributes                          = object_data-attributes
      methods                             = object_data-methods
      events                              = object_data-events
      types                               = object_data-types
      PARAMETERS                          = object_data-parameters
      exceps                              = object_data-exceps
      implementings                       = object_data-implementings
      aliases                             = object_data-aliases
* Do not exist on ECC6.0 and not yet used so removed to ensure maximum workability
*      enhancement_methods                 = object_data-enhancement_methods
*      enhancement_attributes              = object_data-enhancement_attributes
*      enhancement_events                  = object_data-enhancement_events
*      enhancement_implementings           = object_data-enhancement_implementings
*      enhancement_premeth                 = object_data-enhancement_premeth
*      enhancement_postmeth                = object_data-enhancement_postmeth
*      enhancement_owrmeth                 = object_data-enhancement_owrmeth
    EXCEPTIONS
      not_existing                        = 1
      is_interface                        = 2
      model_only                          = 3
      OTHERS                              = 4.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_RESOLVE_INHERITANCE'
                                                subrc = sy-subrc
                                            classname = 'ZAPLINK_CX_CONNECTOR' ).
    application_log->add_symsg( ).
    RAISE EXCEPTION o_mycx.
  ENDIF.
* End of Issue 128 : Get Inheritance informations
  endmethod.


  method SEARCH_DEFERED_DECLARATION.
  DATA d_tabix        TYPE sy-tabix.
  DATA t_tokens       TYPE STANDARD TABLE OF stokes.
  DATA t_statements   TYPE STANDARD TABLE OF sstmnt.
  DATA s_tp           LIKE LINE OF new_typepools.
  DATA d_flag         TYPE abap_bool.
  DATA d_comment      TYPE abap_bool.
  DATA d_line         TYPE i.
  DATA is_public      TYPE abap_bool.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF new_sourcecode,
    <st> LIKE LINE OF t_statements,
    <t> LIKE LINE OF t_tokens,
    <n> LIKE LINE OF t_tokens.

  CHECK NOT sourcecode IS INITIAL.
  new_sourcecode = sourcecode.    zaplink_tools=>conv_table_2upcase( CHANGING table = new_sourcecode ).

  SCAN ABAP-SOURCE new_sourcecode
                   TOKENS INTO t_tokens
                   STATEMENTS INTO t_statements.

  READ TABLE t_statements ASSIGNING <st> INDEX 2.      " public section. ?
  IF <st> IS ASSIGNED.
    READ TABLE new_sourcecode ASSIGNING <s> INDEX <st>-trow.
    ASSERT <s> IS ASSIGNED.
    IF <s> = 'PUBLIC SECTION.'.
      is_public = abap_true.
      LOOP AT new_sourcecode FROM <st>-trow TRANSPORTING NO FIELDS WHERE table_line IS INITIAL.
        d_line = sy-tabix.
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDIF.

  zaplink_tools=>inverse_table_order( CHANGING my_table = t_statements ).

  new_sourcecode = sourcecode.

  LOOP AT t_statements ASSIGNING <st>.
    CHECK is_public IS INITIAL OR <st>-trow > d_line.
    READ TABLE t_tokens ASSIGNING <t> INDEX <st>-from.
    ASSERT sy-subrc = 0.
    CLEAR d_comment.
    CASE <t>-str.
      WHEN 'TYPE-POOLS'.
        d_tabix = <st>-from + 1.
        d_comment = abap_true.
        LOOP AT t_tokens ASSIGNING <n> FROM d_tabix TO <st>-to.     " do all type pools
          READ TABLE typepools TRANSPORTING NO FIELDS
               WITH KEY typegroup = <n>-str
                         explicit = abap_true.
          CHECK sy-subrc <> 0.
* Type pool doesn't exist in class definition but is declared in the middle of sections
          CLEAR s_tp.
          s_tp-typegroup = <n>-str.
          s_tp-tputype = 0.     " Type group use                (TYPE-POOLS tp)
          s_tp-explicit = abap_true.
          s_tp-implicit = abap_false.
          APPEND s_tp TO new_typepools.             d_flag = abap_true.
        ENDLOOP.
      WHEN 'CLASS' OR 'INTERFACE'.
        d_tabix = <st>-to - 1.
        READ TABLE t_tokens ASSIGNING <n> INDEX d_tabix.
        CHECK sy-subrc = 0.
        CHECK <n>-str = 'DEFINITION'.

        READ TABLE t_tokens ASSIGNING <n> INDEX <st>-to.
        CHECK sy-subrc = 0.
        CHECK <n>-str = 'LOAD'.

        d_tabix = <st>-from + 1.
        READ TABLE t_tokens ASSIGNING <n> INDEX d_tabix.
        CHECK sy-subrc = 0.
        d_comment = abap_true.

        READ TABLE typepools TRANSPORTING NO FIELDS
             WITH KEY typegroup = <n>-str
                       explicit = abap_true.
        CHECK sy-subrc <> 0.
        CLEAR s_tp.
        s_tp-typegroup = <n>-str.
        CASE <t>-str.
          WHEN 'CLASS'.
            s_tp-tputype = 1.     " 1	Forward declaration class     (CLASS c DEFINITION DEFERRED)
          WHEN 'INTERFACE'.
            s_tp-tputype = 2.     " 2	Forward declaration interface (INTERFACE i DEFINITION DEF...
          WHEN OTHERS.
            ASSERT <t>-str = 'CLASS' OR <t>-str = 'INTERFACE'.
        ENDCASE.
        s_tp-explicit = abap_true.
        s_tp-implicit = abap_false.
        APPEND s_tp TO new_typepools.             d_flag = abap_true.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    IF NOT d_comment IS INITIAL.
* Line has to be commented
      zaplink_tools=>comment_statement( EXPORTING statement = <st>
                                                     tokens = t_tokens
                                              CHANGING code = new_sourcecode ).
*      READ TABLE new_sourcecode ASSIGNING <s> INDEX <n>-row.
*      CONCATENATE '*' <s> '"Auto commented by ZAPLINK_OBJECT connector and added as Explicit type group'(cmt) INTO <s>.
    ENDIF.

  ENDLOOP.

  IF d_flag IS INITIAL. CLEAR new_sourcecode. ENDIF.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA _o_class       TYPE to_class.
  DATA _o_interface   TYPE to_interface.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN zaplink_object_data=>object_types-class.
      _o_class ?= object->raw.
      _o_class->anonymize( ).
    WHEN zaplink_object_data=>object_types-interface.
      _o_interface ?= object->raw.
      _o_interface->anonymize( ).
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
  DATA _o_class       TYPE to_class.
  DATA _o_interface   TYPE to_interface.

  TRY.
      super->zaplink_cnx_ext_cleaner~unanonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.

  CASE object->type.
    WHEN zaplink_object_data=>object_types-class.
      _o_class ?= object->raw.
      _o_class->unanonymize( ).
    WHEN zaplink_object_data=>object_types-interface.
      _o_interface ?= object->raw.
      _o_interface->unanonymize( ).
    WHEN OTHERS.
  ENDCASE.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
  DATA clskey TYPE seoclskey.
  DATA d_type TYPE td_comptype.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN zaplink_object_data=>object_types-class.
        clskey-clsname = component->get_name( ).
        result = get_class_signature( clskey ).
      WHEN zaplink_object_data=>object_types-interface.
        clskey-clsname = component->get_name( ).
        result = get_interface_signature( clskey ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_clas TYPE to_class.
  DATA o_intf TYPE to_interface.

  CREATE OBJECT object.
  CASE type.
    WHEN zaplink_object_data=>object_types-class.
      CREATE OBJECT o_clas.
      object->raw = o_clas.
*      create object object type TO_CLASS.
    WHEN zaplink_object_data=>object_types-interface.
      CREATE OBJECT o_intf.
      object->raw = o_intf.
*      create object object type TO_CLASS.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA clskey  TYPE seoclskey.
  DATA d_type  TYPE td_comptype.
  DATA s_subrc TYPE sy-subrc.
  DATA d_name  TYPE td_compname.

  TRY.
    d_type = component->get_type( ).
    clskey-clsname = d_name = component->get_name( ).
    CASE d_type.
      WHEN zaplink_object_data=>object_types-class.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey       = clskey
*           GENFLAG                    = ' '
*           AUTHORITY_CHECK            = SEOX_TRUE
*           SUPPRESS_DOCU_DELETE       = SEOX_FALSE
*           SUPPRESS_COMMIT            = SEOX_FALSE
*         CHANGING
*           CORRNR                     =
          EXCEPTIONS
            not_existing               = 1
            is_interface               = 2
            db_error                   = 3
            no_access                  = 4
            other                      = 5
            OTHERS                     = 6.
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>not_found.
            WHEN OTHERS.
              s_subrc = sy-subrc.
              CALL FUNCTION 'SEO_BSP_CLASS_DELETE_COMPLETE'     " Issue 119 : Brute force delete
                EXPORTING
                  clskey = clskey.
              IF do_exists( component ) = abap_true.
                o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_DELETE_W_DEPS'
                                                            subrc = s_subrc
                                                        classname = 'ZAPLINK_CX_CONNECTOR' ).
                application_log->add_symsg( ).
                RAISE EXCEPTION o_mycx.
              ENDIF.
          ENDCASE.
        ENDIF.
      WHEN zaplink_object_data=>object_types-interface.
* Not working
*      CALL FUNCTION 'SEO_INTERFACE_DELETE_W_DEPS'
*        EXPORTING
*          intkey       = clskey
*          save         = seox_true
*        EXCEPTIONS
*          not_existing = 1
*          is_class     = 2
*          not_deleted  = 3
*          db_error     = 4
*          OTHERS       = 5.
        CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
          EXPORTING
            intkey                     = clskey
*         GENFLAG                    = ' '
            authority_check            = seox_true
            suppress_docu_delete       = seox_true
*         SUPPRESS_COMMIT            = SEOX_FALSE
*       CHANGING
*         CORRNR                     =
          EXCEPTIONS
            not_existing               = 1
            is_class                   = 2
            db_error                   = 3
            no_access                  = 4
            other                      = 5
            OTHERS                     = 6
                  .
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>not_found.
            WHEN OTHERS.
              s_subrc = sy-subrc.
              CALL FUNCTION 'SEO_BSP_CLASS_DELETE_COMPLETE'     " Issue 119 : Brute force delete
                EXPORTING
                  clskey = clskey.
              IF do_exists( component ) = abap_true.
                o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTERFACE_DELETE_COMPLETE'
                                                            subrc = s_subrc
                                                        classname = 'ZAPLINK_CX_CONNECTOR' ).
                application_log->add_symsg( ).
                RAISE EXCEPTION o_mycx.
              ENDIF.
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
  DATA clskey TYPE seoclskey.
  DATA d_type TYPE td_comptype.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN zaplink_object_data=>object_types-class.
        clskey-clsname = component->get_name( ).

        CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
          EXPORTING
            clskey        = clskey
*        IMPORTING
*          not_active    = not_active
          EXCEPTIONS
            not_specified = 1
            not_existing  = 2
            is_interface  = 3
            no_text       = 4
            inconsistent  = 5
            OTHERS        = 6.

        IF sy-subrc = 0.
          exists = 'X'.
        ENDIF.
      WHEN zaplink_object_data=>object_types-interface.
        clskey-clsname = component->get_name( ).

        CALL FUNCTION 'SEO_INTERFACE_EXISTENCE_CHECK'
          EXPORTING
            intkey              = clskey
*       IMPORTING
*         NOT_ACTIVE          =
         EXCEPTIONS
           not_specified       = 1
           not_existing        = 2
           is_class            = 3
           no_text             = 4
           inconsistent        = 5
           OTHERS              = 6.

        IF sy-subrc = 0.
          exists = 'X'.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver OR version = '1.0'.   " version 1.01 is downward compatible with version 1.0
    result = abap_true.
  endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA o_clas TYPE to_class.
  DATA o_intf TYPE to_interface.
  DATA d_type TYPE td_comptype.

  TRY.

    CREATE OBJECT object.
    object->set_component( component ).
    d_type = component->get_type( ).
    CASE d_type.
      WHEN zaplink_object_data=>object_types-class.
        TRY.
            o_clas = export_from_sap_class( component ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
        CLEAR o_clas->a0_maindata-clsname.
        object->raw = o_clas.
      WHEN zaplink_object_data=>object_types-interface.
        TRY.
            o_intf = export_from_sap_interface( component ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
        CLEAR o_intf->a0_maindata-clsname.
        object->raw = o_intf.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA _o_class       TYPE to_class.
  DATA _o_interface   TYPE to_interface.
  DATA o_comp         TYPE to_component.                    " Issue 92

  TRY.
    CASE object->type.
      WHEN zaplink_object_data=>object_types-class.
        _o_class ?= object->raw.
        _o_class->a0_maindata-clsname = object->name.
        TRY.
            components = import_to_sap_class( _o_class ).
          CATCH zaplink_cx_connector INTO o_mycx.
            IF NOT o_mycx->messages IS BOUND.
              o_mycx->messages = application_log.
              o_mycx->update( ).
            ENDIF.
            RAISE EXCEPTION o_mycx.
        ENDTRY.
      WHEN zaplink_object_data=>object_types-interface.
        _o_interface ?= object->raw.
        _o_interface->a0_maindata-clsname = object->name.
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


  method _CHECK_CLASS_SECTIONS.
  DATA pubsec_source TYPE seo_section_source.
  DATA prosec_source TYPE seo_section_source.
  DATA prisec_source TYPE seo_section_source.
  DATA d_text        TYPE string.
  DATA d_name        TYPE td_compname.

  IF NOT application_log IS BOUND.
    d_text = 'APPLICATION_LOG can''t be null'(eal).
    RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING textid = zaplink_cx_connector=>system_error
                    cx_name = d_text.
  ENDIF.

  CALL FUNCTION 'SEO_CLASS_TYPEINFO_BY_VIS'         " Issue 105 : First call for nothing to ensure method order
    EXPORTING
      clskey                              = classkey
      version                             = seoc_version_inactive
      state                               = seoc_state_implemented
*     RESOLVE_EVENTHANDLER_TYPEINFO       = SEOX_FALSE
*     WITH_MASTER_LANGUAGE                = SEOX_FALSE
*     WITH_ENHANCEMENTS                   = SEOX_FALSE
*   IMPORTING
    EXCEPTIONS
      not_existing                        = 1
      is_interface                        = 2
      model_only                          = 3
      OTHERS                              = 4.
  IF sy-subrc = 3.        " Issue 111
    return.
  ELSEIF sy-subrc = 1.
    d_name = classkey.
    RAISE EXCEPTION TYPE zaplink_cx_connector EXPORTING textid = zaplink_cx_connector=>not_found
                                                          type = zaplink_object_data=>object_types-class
                                                          name = d_name.
  ELSEIF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_TYPEINFO_BY_VIS'
                                                subrc = sy-subrc
                                            classname = 'ZAPLINK_CX_CONNECTOR' ).
    application_log->add_symsg( ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  CALL FUNCTION 'SEO_CLASS_GENERATE_SECTIONS'
    EXPORTING
      clskey                               = classkey
      public                               = seox_true
      protected                            = seox_true
      private                              = seox_true
*      SUPPRESS_CORR                        = SEOX_FALSE
      return_generated_sections_only       = seox_true
*      TYPEINFO                             =
*      LINE_SIZE                            = 255
*      SUPPRESS_INDEX_UPDATE                = SEOX_FALSE
    IMPORTING
      pubsec_source                        = pubsec_source
      prosec_source                        = prosec_source
      prisec_source                        = prisec_source
    EXCEPTIONS
      not_existing                         = 1
      model_only                           = 2
      public_sec_not_generated             = 3
      protected_sec_not_generated          = 4
      private_sec_not_generated            = 5
      public_sec_not_initialised           = 6
      protected_sec_not_initialised        = 7
      private_sec_not_initialised          = 8
      _internal_class_not_existing         = 9
      OTHERS                               = 10.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_GENERATE_SECTIONS'
                                                subrc = sy-subrc
                                            classname = 'ZAPLINK_CX_CONNECTOR' ).
    application_log->add_symsg( ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  IF pubsec_source = pub_source.
    CLEAR pubsec_source.
  ELSE.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE w002 WITH text-pub classkey. ENDIF.
    SET EXTENDED CHECK ON.
    CALL METHOD application_log->add_warning
      EXPORTING
*            id_msgid     =
        id_msgno     = '002'
        id_msgv1     = 'Public section'(pub)
        id_msgv2     = classkey.
*            id_msgv3     =
*            id_msgv4     =
*            id_detlevel  =
*            id_probclass =
  ENDIF.

  IF prosec_source = pro_source.
    CLEAR prosec_source.
  ELSE.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE w002 WITH text-pro classkey. ENDIF.
    SET EXTENDED CHECK ON.
    CALL METHOD application_log->add_warning
      EXPORTING
*        id_msgid     =
        id_msgno     = '002'
        id_msgv1     = 'Protected section'(pro)
        id_msgv2     = classkey.
*        id_msgv3     =
*        id_msgv4     =
*        id_detlevel  =
*        id_probclass =
  ENDIF.

  IF prisec_source = pri_source.
    CLEAR prisec_source.
  ELSE.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE w002 WITH text-pri classkey. ENDIF.
    SET EXTENDED CHECK ON.
    CALL METHOD application_log->add_warning
      EXPORTING
*        id_msgid     =
        id_msgno     = '002'
        id_msgv1     = 'Private section'(pri)
        id_msgv2     = classkey.
*        id_msgv3     =
*        id_msgv4     =
*        id_detlevel  =
*        id_probclass =
  ENDIF.
  IF NOT pubsec_source IS INITIAL OR NOT prosec_source IS INITIAL OR NOT prisec_source IS INITIAL.
    LOG-POINT ID zaplink
              FIELDS pubsec_source prosec_source prisec_source.       " Store information
    CALL FUNCTION 'SEO_CLASS_GENERATE_SECTIONS'
      EXPORTING
        clskey                               = classkey
        public                               = seox_true
        protected                            = seox_true
        private                              = seox_true
*         SUPPRESS_CORR                        = SEOX_FALSE
*         return_generated_sections_only       = seox_false
*         TYPEINFO                             =
*         LINE_SIZE                            = 255
*         SUPPRESS_INDEX_UPDATE                = SEOX_FALSE
*        IMPORTING
*          pubsec_source                        = pubsec_source
*          prosec_source                        = prosec_source
*          prisec_source                        = prisec_source
      EXCEPTIONS
        not_existing                         = 1
        model_only                           = 2
        public_sec_not_generated             = 3
        protected_sec_not_generated          = 4
        private_sec_not_generated            = 5
        public_sec_not_initialised           = 6
        protected_sec_not_initialised        = 7
        private_sec_not_initialised          = 8
        _internal_class_not_existing         = 9
        OTHERS                               = 10.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLASS_GENERATE_SECTIONS'
                                                  subrc = sy-subrc
                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
      application_log->add_symsg( ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
* Already a commit inside
*    CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
*      EXPORTING
*        cifkey                              = classkey
**       NO_SECTIONS                         = SEOX_FALSE
*        SECTIONS_ONLY                       = SEOX_true
**       SUPPRESS_CORR                       = SEOX_FALSE
**       SUPPRESS_REFACTORING_ENTRIES        = SEOX_FALSE
**       SUPPRESS_METHOD_GENERATION          = SEOX_FALSE
**       SUPPRESS_PUBSEC_GENERATION          = SEOX_FALSE
**       SUPPRESS_PROSEC_GENERATION          = SEOX_FALSE
**       SUPPRESS_PRISEC_GENERATION          = SEOX_FALSE
**       SUPPRESS_DOCU_DELETE                = SEOX_FALSE
**       SUPPRESS_MODIFICATION_SUPPORT       = SEOX_FALSE
**       DISABLE_MODIFICATION_SUPPORT        = SEOX_FALSE
**       GENERATE_IF_METHODS_INITIAL         = SEOX_FALSE
**       LINE_SIZE                           = 255
**       SUPPRESS_COMMIT                     = SEOX_FALSE
**       NOTEASS_MODE                        = SEOX_FALSE
**       SUPPRESS_INDEX_UPDATE               = SEOX_FALSE
**     IMPORTING
**       ERROR_OCCURRED                      =
**       PUBLIC_SAVED                        =
**       PRIVATE_SAVED                       =
**       PROTECTED_SAVED                     =
**     CHANGING
**       CORRNR                              =
**       DEVCLASS                            =
**       GENFLAG                             =
*      EXCEPTIONS
*        NOT_EXISTING                        = 1
*        NOTHING_TO_DO                       = 2
*        ACCESS_ERROR                        = 3
*        DB_ERROR                            = 4
*        ERROR_IN_CODE_GENERATION            = 5
*        OTHERS                              = 6.
*    IF sy-subrc <> 0.
*      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLIF_SAVE_ALL'
*                                                  subrc = sy-subrc
*                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
*      application_log->add_symsg( ).
*      RAISE EXCEPTION o_mycx.
*    ENDIF.
  ENDIF.
  endmethod.


  method _CLEANUP_CLASS.
  DATA f_has_error  TYPE abap_bool.
  DATA t_types      TYPE STANDARD TABLE OF seotypepls.
  DATA s_key        TYPE seotpukey.
  DATA d_classkey   TYPE seoclskey.
  DATA s_data       TYPE vseocdefer.
  FIELD-SYMBOLS <t> LIKE LINE OF t_types.

  TRY.
    IF generate_class( object_data-class-clsname ) IS INITIAL.    f_has_error = abap_true.    ENDIF.

* From SEO_CLIF_SAVE_ALL
** Clean-up unused implicite type pool usages (MS 20060921)
*      DATA type_pool_usage_cleaner TYPE REF TO lcl_type_pool_usage_cleaner.
*      TRY.
*          type_pool_usage_cleaner = lcl_type_pool_usage_cleaner=>create( classkey ).
*          type_pool_usage_cleaner->clean_up( ).
*        CATCH lcx_internal_error.                       "#EC NO_HANDLER
**     No handler needed here => default behavior
*      ENDTRY.

*      DELETE OBJECT_DATA-typeusages WHERE version <> OBJECT_DATA-class-version.   " Remove type usage of the wrong version
* Type-pools declaration might be present within method's declaration section (auto added by SAP when needed).
* Have to detect it, add type pool to type pool list and delete type pool declaration
    IF cleanup_typeusage( include = object_data-includes-pubsec
                             text = 'Public section'(pub) ) = abap_true.
      has_change = abap_true.
    ENDIF.
    IF cleanup_typeusage( include = object_data-includes-prosec
                             text = 'Protected section'(pro) ) = abap_true.
      has_change = abap_true.
    ENDIF.
    IF cleanup_typeusage( include = object_data-includes-prisec
                             text = 'Private section'(pri) ) = abap_true.
      has_change = abap_true.
    ENDIF.
    IF NOT has_change IS INITIAL.
      d_classkey-clsname = object_data-class-clsname.
      CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
        EXPORTING
          cifkey                   = d_classkey
        EXCEPTIONS
          not_existing             = 1
          nothing_to_do            = 2
          access_error             = 3
          db_error                 = 4
          error_in_code_generation = 5
          OTHERS                   = 6.
      IF sy-subrc <> 0.
        o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLIF_SAVE_ALL'
                                                    subrc = sy-subrc
                                                classname = 'ZAPLINK_CX_CONNECTOR' ).
        application_log->add_symsg( ).
        RAISE EXCEPTION o_mycx.
      ENDIF.
    ENDIF.

    IF f_has_error IS INITIAL.

* try to remove implicit type declaration : often useless
      SELECT * INTO TABLE t_types
        FROM seotypepls
        WHERE clsname = object_data-class-clsname
          AND tputype <> 0    " no type-pool
          AND explicit = abap_false
          AND implicit = abap_true.

      LOOP AT t_types ASSIGNING <t>.
        CLEAR: s_key.    s_key-clsname = <t>-clsname.      s_key-typegroup = <t>-typegroup.
        CASE <t>-tputype.
          WHEN 1.
            CALL FUNCTION 'SEO_CLSDEFERRD_DELETE_W_DEPS'
              EXPORTING
                cdfkey        = s_key
                save          = seox_true
              EXCEPTIONS
                not_existing  = 1
                is_intdeferrd = 2
                is_typepusage = 3
                not_deleted   = 4
                db_error      = 5
                OTHERS        = 6.
            IF sy-subrc <> 0.
              o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLSDEFERRD_DELETE_W_DEPS'
                                                          subrc = sy-subrc
                                                      classname = 'ZAPLINK_CX_CONNECTOR' ).
              application_log->add_symsg( ).
              RAISE EXCEPTION o_mycx.
            ENDIF.
          WHEN 2.     " interface*
            CALL FUNCTION 'SEO_INTDEFERRD_DELETE_W_DEPS'
              EXPORTING
                idfkey        = s_key
                save          = seox_true
              EXCEPTIONS
                not_existing  = 1
                is_typepusage = 2
                is_clsdeferrd = 3
                not_deleted   = 4
                db_error      = 5
                OTHERS        = 6.
            IF sy-subrc <> 0.
              o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTDEFERRD_DELETE_W_DEPS'
                                                          subrc = sy-subrc
                                                      classname = 'ZAPLINK_CX_CONNECTOR' ).
              application_log->add_symsg( ).
              RAISE EXCEPTION o_mycx.
            ENDIF.
          WHEN OTHERS.
            ASSERT <t>-tputype >= 1 AND <t>-tputype <= 2.
            CONTINUE.
        ENDCASE.

        IF NOT generate_class( s_key-clsname ) IS INITIAL.
          has_change = abap_true.
        ELSE.
          MOVE-CORRESPONDING <t> TO s_data.
* undo deletion
          CASE <t>-tputype.
            WHEN 1.
              CALL FUNCTION 'SEO_CLSDEFERRD_CREATE_F_DATA'
                EXPORTING
                  save                    = seox_true
*             NOT_AFFECT_PUBLIC       = SEOX_FALSE
                CHANGING
                  clsdeferrd              = s_data
                EXCEPTIONS
                  existing                = 1
                  is_intdeferrd           = 2
                  is_typepusage           = 3
                  not_created             = 4
                  db_error                = 5
                  OTHERS                  = 6.
              IF sy-subrc <> 0.
                o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_CLSDEFERRD_CREATE_F_DATA'
                                                            subrc = sy-subrc
                                                        classname = 'ZAPLINK_CX_CONNECTOR' ).
                application_log->add_symsg( ).
                RAISE EXCEPTION o_mycx.
              ENDIF.
            WHEN 2.     " interface
              CALL FUNCTION 'SEO_INTDEFERRD_CREATE_F_DATA'
                EXPORTING
                  save                    = seox_true
*                 NOT_AFFECT_PUBLIC       = SEOX_FALSE
                CHANGING
                  intdeferrd              = s_data
                EXCEPTIONS
                  existing                = 1
                  is_typepusage           = 2
                  is_clsdeferrd           = 3
                  not_created             = 4
                  db_error                = 5
                  OTHERS                  = 6.
              IF sy-subrc <> 0.
                o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'SEO_INTDEFERRD_CREATE_F_DATA'
                                                            subrc = sy-subrc
                                                        classname = 'ZAPLINK_CX_CONNECTOR' ).
                application_log->add_symsg( ).
                RAISE EXCEPTION o_mycx.
              ENDIF.
            WHEN OTHERS.
              ASSERT <t>-tputype >= 1 AND <t>-tputype <= 2.
              CONTINUE.
          ENDCASE.

        ENDIF.

      ENDLOOP.

    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
