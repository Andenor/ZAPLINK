class ZAPLINK_BADI definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.
  type-pools SEEX .

  constants ST_BADI_DEF type TD_COMPTYPE value 'SXSD' ##NO_TEXT.
  constants ST_BADI_IMPL type TD_COMPTYPE value 'SXCI' ##NO_TEXT.
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_BADI' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
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
private section.

  types:
    BEGIN OF ts_doc_key,                                    " equiv DOKU_OBJ = CHAR 60
      simg(4)    TYPE c,     "SIMG
      name       TYPE rsexscrn-exit_name,                   " CHAR 30
    END OF ts_doc_key .
  types:
    BEGIN OF ts_clas_attr.
  INCLUDE TYPE ts_base_attributs AS base.
  TYPES:
    END OF ts_clas_attr .
  types TO_BADI_DEF type ref to ZAPLINK_SXSD_DATA .
  types TO_BADI_IMPL type ref to ZAPLINK_SXCI_DATA .

  class-data R_DOC_ID_DEFINITION type TR_DOCID .
  constants T_BADI_ITF type TD_COMPTYPE value 'INTF' ##NO_TEXT.
  constants T_BADI_IMPL type TD_COMPTYPE value 'CLAS' ##NO_TEXT.
  class-data R_DOC_ID_IMPLEMENTATION type TR_DOCID .
  constants _UUID type TD_CONNUUID value '5DAA0A4B07A5A05AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.

  class-methods CREATE_DOC_KEY
    returning
      value(DOC_KEY) type TS_DOC_KEY .
  class-methods DO_EXISTS_IMPL
    importing
      !NAME type RSEXSCRN-IMP_NAME
    returning
      value(_EXISTS) type TD_COMPEXISTS .
  methods EXPORT_FROM_SAP_DEF
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(O_DATA) type TO_BADI_DEF
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_FROM_SAP_IMPL
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(O_DATA) type TO_BADI_IMPL
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TO_SAP_DEF
    importing
      !O_DATA type TO_BADI_DEF
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TO_SAP_IMPL
    importing
      !O_DATA type TO_BADI_IMPL
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
ENDCLASS.



CLASS ZAPLINK_BADI IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*BI       Business Add-Ins: Implementations
  DATA _id LIKE LINE OF r_doc_id_definition.

  _id-sign = 'I'.
  _id-option = 'EQ'.
  _id-low = 'HY'. APPEND _id TO r_doc_id_definition.

  _id-low = 'BI'. APPEND _id TO r_doc_id_implementation.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = st_badi_def. INSERT type INTO TABLE supported_types.
  type-type = st_badi_impl. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method CREATE_DOC_KEY.
* SELECT WHERE "ID" = 'HY' AND "OBJECT" = 'SIMGZAPLINK_ACTION' AND "LANGU" = 'E' AND "TYP" = 'E' AND "DOKVERSION" = 0001 ORDER BY
  doc_key-simg = 'SIMG'.
  endmethod.


  method DO_EXISTS_IMPL.
  _exists = zaplink_badi=>exists-not_exists.
  CALL FUNCTION 'SXV_IMP_EXISTS'
    EXPORTING
      imp_name           = name
    EXCEPTIONS
      not_existing       = 1
      data_inconsistency = 2
      OTHERS             = 3.
  IF sy-subrc = 0.
    _exists =  zaplink_badi=>exists-exists.
  ENDIF.
  endmethod.


  method EXPORT_FROM_SAP_DEF.
  DATA obj_mask    TYPE doku_obj.
  DATA docu_key    TYPE ts_doc_key.
  DATA _d          TYPE zaplink_sxsd_data=>t_fm_data.
  DATA exit_name   TYPE rsexscrn-exit_name.
  DATA o_conn      TYPE to_connector.
  DATA _list       TYPE to_list.
  DATA _comp       TYPE to_component.
  DATA _name       TYPE td_compname.
  DATA f_subcomp  TYPE td_with_subcomp.

  TRY.
    CHECK component->get_type( ) = st_badi_def.
    f_subcomp = component->get_with_subcomp( ).

    exit_name = component->get_name( ).
    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name                = exit_name
*     MAINT_LANGU              = SY-LANGU
      IMPORTING
        badi                     = _d-definition
        mast_langu               = _d-mast_langu
        ext_clname               = _d-ext_clname
        filter_obj               = _d-filter_obj
      TABLES
        fcodes                   = _d-fcodes
        cocos                    = _d-cocos
        intas                    = _d-intas
        scrns                    = _d-scrns
        methods                  = _d-methods
        inactive_tabstrips       = _d-inactive_tabstrips
      EXCEPTIONS
        read_failure             = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

* Documentations
    docu_key = create_doc_key( ).
    docu_key-name = component->get_name( ).
    obj_mask = docu_key.

    CREATE OBJECT o_data.
    o_data->from_data( _d ).

* Load interface
    IF NOT o_data->a0_maindata-inter_name IS INITIAL AND f_subcomp >= sub_component-with_required.
      o_conn = zaplink_connectors=>create_connector( type = t_badi_itf ).
      IF o_conn IS BOUND.
        CREATE OBJECT _comp.
        _comp->set_type( t_badi_itf ).
        _name = o_data->a0_maindata-inter_name.
        _comp->set_name( _name ).
        o_data->interface = o_conn->read_from_sap( _comp ).
        IF o_data->interface IS BOUND.
          CLEAR o_data->a0_maindata-inter_name.
          CREATE OBJECT _list.
          _list->add( _comp ).
          component->set_subcomponents( _list ).
        ENDIF.
      ENDIF.
    ENDIF.

    o_data->documentation = zaplink_documentation=>get( ids = r_doc_id_definition
                                                     object = obj_mask ).
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_FROM_SAP_IMPL.
  DATA obj_mask    TYPE doku_obj.
  DATA _d          TYPE zaplink_badi_data=>t_fm_data.
  DATA imp_name    TYPE rsexscrn-imp_name.
  DATA exit_name   TYPE rsexscrn-exit_name.
  DATA inter_name  TYPE rsexscrn-inter_name.
  DATA filters     TYPE seex_filter_table.
  DATA o_conn      TYPE to_connector.
  DATA _comp       TYPE to_component.
  DATA badi_info   TYPE badi_data.
  DATA filter_obj  TYPE REF TO cl_badi_flt_struct.
  DATA t_fcodes    TYPE seex_fcode_table.
  DATA t_cocos     TYPE seex_coco_table.
  DATA t_intas     TYPE seex_table_table.
  DATA t_scrns     TYPE seex_screen_table.
  DATA methods     TYPE seex_mtd_table.
  DATA _name       TYPE td_compname.
  DATA f_subcomp  TYPE td_with_subcomp.

  TRY.

    CHECK component->get_type( ) = st_badi_impl.
    f_subcomp = component->get_with_subcomp( ).

*  _objtype = COMPONENT-type.
    imp_name = component->get_name( ).
    IF do_exists_impl( imp_name ) <> exists.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name           = imp_name
      IMPORTING
        exit_name          = exit_name
      TABLES
        filters            = filters
      EXCEPTIONS
        data_inconsistency = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SXV_EXIT_FOR_IMP' sy-subrc.
    ENDIF.

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name                = exit_name
*       MAINT_LANGU              = SY-LANGU
      IMPORTING
        badi                     = badi_info
*       MAST_LANGU               =
*       EXT_CLNAME               =
        filter_obj               = filter_obj
      TABLES
        fcodes                   = t_fcodes
        cocos                    = t_cocos
        intas                    = t_intas
        scrns                    = t_scrns
        methods                  = methods
*       INACTIVE_TABSTRIPS       =
      EXCEPTIONS
        read_failure             = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SXO_BADI_READ' sy-subrc.
    ENDIF.

    inter_name = badi_info-inter_name.
    IF filter_obj IS BOUND.
      _d-impl_w_filter = badi_info-flt_ext.
      _d-filter_datatype = filter_obj->flt_type.
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_FOR_BADI_READ'
      EXPORTING
        imp_name                = imp_name
        exit_name               = exit_name
*       MAINT_LANGU             = SY-LANGU
        inter_name              = inter_name
        filter_obj              = filter_obj
      IMPORTING
        impl                    = _d-implementation
        mast_langu              = _d-mast_langu
*      filter_values_obj       = _d-filter_values
      TABLES
        fcodes                  = _d-fcodes
        cocos                   = _d-cocos
        intas                   = _d-intas
        scrns                   = _d-scrns
      CHANGING
        methods                 = methods
      EXCEPTIONS
        read_failure            = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SXO_IMPL_FOR_BADI_READ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name                 = imp_name
*   IMPORTING
*     EXIT_NAME                =
      TABLES
        filters                  = _d-filter_values
      EXCEPTIONS
        data_inconsistency       = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SXV_EXIT_FOR_IMP' sy-subrc.
    ENDIF.


    CREATE OBJECT o_data.
    o_data->from_data( _d ).

* Load Object
    IF NOT o_data->a0_maindata-imp_class IS INITIAL AND f_subcomp >= sub_component-with_required.
      o_conn = zaplink_connectors=>create_connector( type = t_badi_impl ).
      IF o_conn IS BOUND.
        CREATE OBJECT _comp.
        _comp->set_type( t_badi_impl ).
        _name = o_data->a0_maindata-imp_class.
        _comp->set_name( _name ).
        o_data->implementation = o_conn->read_from_sap( _comp ).
        IF o_data->implementation IS BOUND.
          CLEAR o_data->a0_maindata-imp_class.
        ENDIF.
      ENDIF.
    ENDIF.

* Documentations
    CONCATENATE _name '*' INTO obj_mask.
    o_data->documentation = zaplink_documentation=>get( ids = r_doc_id_implementation
                                                     object = obj_mask ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TO_SAP_DEF.
  DATA _d               TYPE zaplink_sxsd_data=>t_fm_data.
  DATA i_korrnr         TYPE trkorr.
  DATA i_devclass       TYPE devclass.
  DATA _component       TYPE to_component.
  DATA mast_langu       TYPE sy-langu.
  DATA o_conn           TYPE to_connector.
  DATA transport_key    TYPE trkey.

  TRY.
* Load interface
    IF o_data->interface IS BOUND.
      o_conn = zaplink_connectors=>create_connector( type = t_badi_itf ).
      IF o_conn IS BOUND.
        o_data->interface->type = t_badi_itf.
        components = o_conn->write_to_sap( o_data->interface ).
        IF components IS BOUND.
          components->init_iter( ).
          _component = components->get_next( ).
          WHILE _component IS BOUND.
            IF _component->get_type( ) = t_badi_itf.
              o_data->a0_maindata-inter_name = _component->get_name( ).
              EXIT.
            ENDIF.
            _component = components->get_next( ).
          ENDWHILE.
        ENDIF.
      ENDIF.
    ENDIF.

    _d = o_data->to_data( ).

    CALL FUNCTION 'SXO_BADI_SAVE'
      EXPORTING
        badi                   = _d-definition
*     MAINT_LANGU            = SY-LANGU
*     GENFLAG                = SEEX_FALSE
*     NO_DIALOG              = SEEX_FALSE
*     WITH_MIG               = SEEX_FALSE
      IMPORTING
        mast_langu             = mast_langu
      TABLES
        fcodes_to_insert       = _d-fcodes
        cocos_to_insert        = _d-cocos
        intas_to_insert        = _d-intas
        sscrs_to_insert        = _d-scrns
      CHANGING
        korrnum                = i_korrnr
        devclass               = i_devclass
      EXCEPTIONS
        save_failure           = 1
        action_canceled        = 2
        OTHERS                 = 3.
    CASE sy-subrc.
      WHEN '0'.
** i guess if we made it this far, we will assume success
** successful install
*    WHEN '1'.
*      RAISE EXCEPTION TYPE zaplink_cx_connector
*        EXPORTING textid = zaplink_cx_connector=>existing.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING textid = zaplink_cx_connector=>system_error.
    ENDCASE.
    IF mast_langu <> o_data->a0_maindata-mast_langu.
      transport_key-obj_name = o_data->a0_maindata-exit_name.
      CALL FUNCTION 'SXO_BADI_MAST_LANG_UPDATE'
        EXPORTING
          new_master_language = o_data->a0_maindata-mast_langu
          old_master_language = mast_langu
          transport_key       = transport_key
        EXCEPTIONS
          object_not_found    = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.

    zaplink_documentation=>set( o_data->documentation ).

    CREATE OBJECT _component.
    _component->set_type( st_badi_def ).
    _component->set_name( o_data->a0_maindata-exit_name ).
    IF NOT components IS BOUND. CREATE OBJECT components. ENDIF.
    components->add( _component ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TO_SAP_IMPL.
  DATA _d               TYPE zaplink_badi_data=>t_fm_data.
  DATA i_korrnr         TYPE trkorr.
  DATA i_devclass       TYPE devclass.
  DATA _component       TYPE to_component.
  DATA mast_langu       TYPE sy-langu.
  DATA o_conn           TYPE to_connector.
  DATA transport_key    TYPE trkey.
  DATA o_filter         TYPE REF TO cl_badi_flt_struct.
  DATA o_filter_val     TYPE REF TO cl_badi_flt_values_alv.
  DATA badi_data        TYPE badi_data.

  TRY.
* Load interface
    IF o_data->implementation IS BOUND.
      o_conn = zaplink_connectors=>create_connector( type = t_badi_impl ).
      IF o_conn IS BOUND.
        o_data->implementation->type = t_badi_impl.
        components = o_conn->write_to_sap( o_data->implementation ).
        IF components IS BOUND.
          components->init_iter( ).
          _component = components->get_next( ).
          WHILE _component IS BOUND.
            IF _component->get_type( ) = t_badi_impl.
              o_data->a0_maindata-imp_class = _component->get_name( ).
              EXIT.
            ENDIF.
            _component = components->get_next( ).
          ENDWHILE.
        ENDIF.
      ENDIF.
    ENDIF.

    _d = o_data->to_data( ).

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name                = _d-implementation-exit_name
*     MAINT_LANGU              = SY-LANGU
      IMPORTING
        badi                     = badi_data
*     MAST_LANGU               =
*     EXT_CLNAME               =
*     FILTER_OBJ               =
*   TABLES
*     FCODES                   =
*     COCOS                    =
*     INTAS                    =
*     SCRNS                    =
*     METHODS                  =
*     INACTIVE_TABSTRIPS       =
      EXCEPTIONS
        read_failure             = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SXO_BADI_READ' sy-subrc.
    ENDIF.


    CREATE OBJECT o_filter
      EXPORTING
        filter_structure               = _d-filter_datatype
        extend                         = badi_data-flt_ext
*      extend                         = _d-impl_w_filter
      EXCEPTIONS
        ddic_object_not_active         = 1
        no_data_element                = 2
        domain_not_of_char             = 3
        domain_too_long                = 4
        filtertype_inadmissible        = 5
        shlp_inadmissible              = 6
        invalid_key_comb_of_val_table  = 7
        invalid_key_comb_of_text_table = 8
        wrong_table_class              = 9
        invalid_text_table             = 10
        no_value_table                 = 11
        no_text_table                  = 12
        checktable_existing            = 13
        no_domain                      = 14
        OTHERS                         = 15.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT o_filter_val
      EXPORTING
        filter_object = o_filter
        filter_values = _d-filter_values
*      for_flt_val_creation = seex_false
*      for_overview = seex_false
*      filter_values_for_over =
*      fieldcatalog =
        .

    CALL FUNCTION 'SXO_IMPL_SAVE'
      EXPORTING
        impl                   = _d-implementation
        flt_ext                = _d-impl_w_filter
        flt_type               = _d-filter_datatype
        maint_langu            = _d-mast_langu
        filter_val_obj         = o_filter_val
*     GENFLAG                = SEEX_FALSE
*     NO_DIALOG              = SEEX_FALSE
      IMPORTING
        mast_langu             = mast_langu
      TABLES
        fcodes_to_insert       = _d-fcodes
        cocos_to_insert        = _d-cocos
        intas_to_insert        = _d-intas
        sscrs_to_insert        = _d-scrns
      CHANGING
        korrnum                = i_korrnr
        devclass               = i_devclass
      EXCEPTIONS
        save_failure           = 1
        action_canceled        = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'SXO_IMPL_SAVE' sy-subrc.
    ENDIF.
    IF mast_langu <> o_data->a0_maindata-mast_langu.
      transport_key-obj_name = o_data->a0_maindata-exit_name.
      CALL FUNCTION 'SXO_IMPL_MAST_LANG_UPDATE'
        EXPORTING
          new_master_language = o_data->a0_maindata-mast_langu
          old_master_language = mast_langu
          transport_key       = transport_key
        EXCEPTIONS
          object_not_found    = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.
        mac_add_mf_and_raise 'SXO_IMPL_MAST_LANG_UPDATE' sy-subrc.
      ENDIF.
    ENDIF.

    zaplink_documentation=>set( o_data->documentation ).

* Activate BADI
    IF NOT _d-implementation-active IS INITIAL.
      CALL FUNCTION 'SXO_IMPL_ACTIVE'
        EXPORTING
          imp_name                        = _d-implementation-imp_name
          no_dialog                       = seex_true
*     CALL_BY_CUSTOMIZING             = SEEX_FALSE
*   CHANGING
*     PROTOCOL                        =
        EXCEPTIONS
          badi_not_existing               = 1
          imp_not_existing                = 2
          already_active                  = 3
          data_inconsistency              = 4
          activation_not_admissable       = 5
          action_canceled                 = 6
          access_failure                  = 7
          OTHERS                          = 8.
      IF sy-subrc <> 0.
        mac_add_mf_and_raise 'SXO_IMPL_ACTIVE' sy-subrc.
      ENDIF.
    ENDIF.

    CREATE OBJECT _component.
    _component->set_type( st_badi_impl ).
    _component->set_name( o_data->a0_maindata-exit_name ).
    IF NOT components IS BOUND. CREATE OBJECT components. ENDIF.
    components->add( _component ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_bdef TYPE to_badi_def.
  DATA o_impl TYPE to_badi_impl.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN st_badi_def.
      o_bdef ?= object->raw.
      o_bdef->anonymize( ).
    WHEN st_badi_impl.
      o_impl ?= object->raw.
      o_impl->anonymize( ).
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_conn   TYPE to_connector.
  DATA o_sxsd   TYPE to_badi_def.
  DATA o_sxci   TYPE to_badi_impl.
  DATA _type    TYPE td_comptype.

  CREATE OBJECT object.
  CASE type.
    WHEN st_badi_def.
      CREATE OBJECT o_sxsd.
* Create empty interface object
      o_conn = zaplink_connectors=>create_connector( type = t_badi_itf ).
      IF o_conn IS BOUND.
        _type = t_badi_itf.
        o_sxsd->interface = o_conn->create_new_raw( _type ).
      ENDIF.
      object->raw = o_sxsd.
*      create object object type TO_CLASS.
    WHEN st_badi_impl.
      CREATE OBJECT o_sxci.
* Crete empty interface object
      o_conn = zaplink_connectors=>create_connector( type = t_badi_impl ).
      IF o_conn IS BOUND.
        _type = t_badi_impl.
        o_sxci->implementation = o_conn->create_new_raw( _type ).
      ENDIF.
      object->raw = o_sxci.
*      create object object type TO_CLASS.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA exit_name  TYPE rsexscrn-exit_name.
  DATA d_type     TYPE td_comptype.
  DATA d_name     TYPE td_compname.

  TRY.
    d_type = component->get_type( ).
    d_name = component->get_name( ).
    CASE d_type.
      WHEN st_badi_def.
        exit_name = d_name.
        CALL FUNCTION 'SXO_BADI_DELETE'
          EXPORTING
            exit_name                = exit_name
            no_dialog                = seex_true
*        CHANGING
*          KORR_NUM                 =
          EXCEPTIONS
            badi_not_existing        = 1
            action_canceled          = 2
            access_failure           = 3
            data_inconsistency       = 4
            OTHERS                   = 5
                  .
        IF sy-subrc <> 0.
          CASE sy-subrc.
*          WHEN 1.
*            RAISE EXCEPTION TYPE zaplink_cx_connector
*              EXPORTING textid = zaplink_cx_connector=>not_found.
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>system_error.
          ENDCASE.
        ENDIF.
      WHEN st_badi_impl.
        exit_name = d_name.
        CALL FUNCTION 'SXO_IMPL_DELETE'
          EXPORTING
            imp_name                 = exit_name
*         NO_DIALOG                = SEEX_FALSE
*       CHANGING
*         KORR_NUM                 =
          EXCEPTIONS
            imp_not_existing         = 1
            action_canceled          = 2
            access_failure           = 3
            data_inconsistency       = 4
            OTHERS                   = 5.
        IF sy-subrc <> 0.
          CASE sy-subrc.
*          WHEN 1.
*            RAISE EXCEPTION TYPE zaplink_cx_connector
*              EXPORTING textid = zaplink_cx_connector=>not_found.
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
  DATA exit_name  TYPE rsexscrn-exit_name.
  DATA imp_name   TYPE rsexscrn-imp_name.
  DATA d_type     TYPE td_comptype.

  TRY.
      exists = me->exists-not_exists.
      d_type = component->get_type( ).
      CASE d_type.
        WHEN st_badi_def.
          exit_name = component->get_name( ).
          CALL FUNCTION 'SXV_EXIT_EXISTS'
            EXPORTING
              exit_name                   = exit_name
            EXCEPTIONS
              not_existing                = 1
              exit_interface_not_existing = 2
              category_conflict           = 3
              no_sxs_inter_entry          = 4
              OTHERS                      = 5.
          IF sy-subrc = 0.
            exists = me->exists-exists.
          ENDIF.
        WHEN st_badi_impl.
          imp_name = component->get_name( ).
          exists = do_exists_impl( imp_name ).
        WHEN OTHERS.
          mac_raise_type_not_supported me->class_name d_type.
      ENDCASE.
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.    result = abap_true.   endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA o_bdef TYPE to_badi_def.
  DATA o_impl TYPE to_badi_impl.
  DATA d_type TYPE td_comptype.

  TRY.
    CREATE OBJECT object.
    object->set_component( component ).
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_badi_def.
        o_bdef = export_from_sap_def( component ).
        object->raw = o_bdef.
      WHEN st_badi_impl.
        o_impl = export_from_sap_impl( component ).
        object->raw = o_impl.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA _o_def         TYPE to_badi_def.
  DATA _o_impl        TYPE to_badi_impl.
  DATA d_type         TYPE td_comptype.
  DATA o_comp         TYPE to_component.              " Issue 92

  TRY.
    d_type = object->type.
    CASE d_type.
      WHEN st_badi_def.
        _o_def ?= object->raw.
        _o_def->a0_maindata-exit_name = object->name.
        components = import_to_sap_def( _o_def ).
      WHEN st_badi_impl.
        _o_impl ?= object->raw.
        _o_impl->a0_maindata-imp_name = object->name.
        components = import_to_sap_impl( _o_impl ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    check_component_list( EXPORTING     object = object
                           CHANGING components = components ).    " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
