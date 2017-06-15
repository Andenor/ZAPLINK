class ZAPLINK_DICTIONARY definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public

  global friends ZAPLINK_XINX_DATA
                 ZAPLINK_XINX_RAW .

public section.

  types TD_TYPEGROUP type TYPEGROUP .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_DICTIONARY' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  class-methods GET_TYPEGROUP_SIGNATURE
    importing
      !TYPE_GROUP type TD_TYPEGROUP
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

  types TO_TYPEGROUP type ref to ZAPLINK_TYPE_DATA .
  types TO_DOMAIN type ref to ZAPLINK_DOMA_DATA .
  types TO_DATA_ELEMENT type ref to ZAPLINK_DTEL_DATA .
  types TO_VIEW type ref to ZAPLINK_VIEW_DATA .
  types TO_TABLE type ref to ZAPLINK_TABL_DATA .
  types TO_TABLE_TYPE type ref to ZAPLINK_TTYP_DATA .
  types TO_SEARCH_HELP type ref to ZAPLINK_SHLP_DATA .
  types TO_LOCK_OBJECT type ref to ZAPLINK_ENQU_DATA .
  types TO_INDEX type ref to ZAPLINK_XINX_DATA .
  types TS_IDX_KEY type ZAPLINK_XINX_DATA=>TS_IDX_KEY .
  types TO_CONTENT type ref to ZAPLINK_TABU_DATA .

  constants:
    BEGIN OF supportedtypes,
      type_group        TYPE td_comptype VALUE 'TYPE',    "#EC NOTEXT
      domain            TYPE td_comptype VALUE 'DOMA',    "#EC NOTEXT
      data_element      TYPE td_comptype VALUE 'DTEL',    "#EC NOTEXT
      table_type        TYPE td_comptype VALUE 'TTYP',    "#EC NOTEXT
      search_help       TYPE td_comptype VALUE 'SHLP',    "#EC NOTEXT
      mactchcode_id     TYPE td_comptype VALUE 'MCID',    "#EC NOTEXT
      lock_object       TYPE td_comptype VALUE 'ENQU',    "#EC NOTEXT
      matchcode_obj     TYPE td_comptype VALUE 'MCOB',    "#EC NOTEXT
      table_index       TYPE td_comptype VALUE 'INDX',    "#EC NOTEXT : N  Table Index
      ext_index         TYPE td_comptype VALUE 'XINX',    "#EC NOTEXT : O  Extention Index MF INTERN_TRANSL_EUTYPE
      table             TYPE td_comptype VALUE 'TABL',    "#EC NOTEXT
      table_content     TYPE td_comptype VALUE 'TABU',    "#EC NOTEXT
      view              TYPE td_comptype VALUE 'VIEW',    "#EC NOTEXT
      t_cluster_pool    TYPE td_comptype VALUE 'SQLT',    "#EC NOTEXT
    END OF supportedtypes .
  constants:
    BEGIN OF dd_eu_type,          " Domain DDEUTYPE
      table_type        TYPE ddeutype VALUE 'A',     "#EC NOTEXT : A  Table Type
      domain            TYPE ddeutype VALUE 'D',     "#EC NOTEXT : D  Domain
      data_element      TYPE ddeutype VALUE 'E',     "#EC NOTEXT : E  Data Element
      table_field       TYPE ddeutype VALUE 'F',     "#EC NOTEXT : F  Table Field
      type_group        TYPE ddeutype VALUE 'G',     "#EC NOTEXT : G  Type Group
      search_help       TYPE ddeutype VALUE 'H',     "#EC NOTEXT : H  Search Help
      mactchcode_id     TYPE ddeutype VALUE 'I',     "#EC NOTEXT : I  Matchcode ID
      lock_object       TYPE ddeutype VALUE 'L',     "#EC NOTEXT : L  Lock Object
      matchcode_obj     TYPE ddeutype VALUE 'M',     "#EC NOTEXT : M  Matchcode Object
      table_index       TYPE ddeutype VALUE 'N',     "#EC NOTEXT : N  Table Index
      ext_index         TYPE ddeutype VALUE 'N',     "#EC NOTEXT : O  Extention Index MF INTERN_TRANSL_EUTYPE
      internal_struct   TYPE ddeutype VALUE 'S',     "#EC NOTEXT : S  Internal Structure
      transp_table      TYPE ddeutype VALUE 'T',     "#EC NOTEXT : T  Transparent Table
      structure         TYPE ddeutype VALUE 'U',     "#EC NOTEXT : U  Table/Structure
      view              TYPE ddeutype VALUE 'V',     "#EC NOTEXT : V  View
      table_pool        TYPE ddeutype VALUE 'X',     "#EC NOTEXT : X  Table Pool
      table_cluster     TYPE ddeutype VALUE 'Y',     "#EC NOTEXT : Y  Table Cluster
      t_cluster_pool    TYPE ddeutype VALUE 'Z',     "#EC NOTEXT : Z  Table cluster/pool
    END OF dd_eu_type .

  class-methods GET_TYPE_GROUP
    importing
      !TYPE_GROUP type TD_TYPEGROUP
    returning
      value(RESULT) type ref to CL_WB_SOURCE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_TYPE_GROUP
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_TYPEGROUP
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TYPE_GROUP
    importing
      !O_DATA type TO_TYPEGROUP
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_DOMAIN
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_DOMAIN
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_DOMAIN
    importing
      !O_DATA type TO_DOMAIN
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_DATA_ELEMENT
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_DATA_ELEMENT
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_DATA_ELEMENT
    importing
      !O_DATA type TO_DATA_ELEMENT
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_VIEW
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_VIEW
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_VIEW
    importing
      !O_DATA type TO_VIEW
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_TABLE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_TABLE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TABLE
    importing
      !O_DATA type TO_TABLE
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_TABLE_TYPE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_TABLE_TYPE
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_TABLE_TYPE
    importing
      !O_DATA type TO_TABLE_TYPE
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_SEARCH_HELP
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_SEARCH_HELP
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_SEARCH_HELP
    importing
      !O_DATA type TO_SEARCH_HELP
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_LOCK_OBJECT
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_LOCK_OBJECT
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_LOCK_OBJECT
    importing
      !O_DATA type TO_LOCK_OBJECT
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_INDEX
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_INDEX
    raising
      ZAPLINK_CX_CONNECTOR .
  methods IMPORT_INDEX
    importing
      !O_DATA type TO_INDEX
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods EXPORT_TABLE_CONTENT
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_CONTENT
    raising
      ZAPLINK_CX_CONNECTOR .
private section.

  types TO_RAW_INDEX type ZAPLINK_TABL_DATA=>TO_INDEX .

  constants _UUID type TD_CONNUUID value '5079624B82531D63E1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  class-data C_TYPEGROUP type TS_COMPTYPE .
ENDCLASS.



CLASS ZAPLINK_DICTIONARY IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  c_typegroup-type = 'TYPE'.    c_typegroup-kind = zaplink_connectors=>get_typekind( c_typegroup-type ).
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.    application_log->msgid = 'ZAPLINK_DICTIONARY'.
  type-type = supportedtypes-type_group.    INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-domain.        INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-data_element.  INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-view.          INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-table.         INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-table_content. INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-table_type.    INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-search_help.   INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-lock_object.   INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-ext_index.     INSERT type INTO TABLE supported_types. " Issue 55
  type-type = supportedtypes-table_index.   INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method EXPORT_DATA_ELEMENT.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_dtel_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd04t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name                = d_name
        state               = 'M'
        langu               = ' '
      IMPORTING
*        GOTSTATE            =
        dd04v_wa            = s_fm_data-header
        tpara_wa            = s_fm_data-param
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_DTEL_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd04t
      WHERE rollname = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_dtel_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_DOMAIN.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_doma_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd01t WITH DEFAULT KEY.
  DATA t_vtexts   TYPE STANDARD TABLE OF dd07t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name                = d_name
        state               = 'M'
*       LANGU               = ' '
      IMPORTING
*       GOTSTATE            =
        dd01v_wa            = s_fm_data-header
      TABLES
        dd07v_tab           = s_fm_data-values
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_DOMA_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd01t
      WHERE domname = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    SELECT *
      INTO TABLE t_vtexts
      FROM dd07t
      WHERE domname = d_name
        AND ddtext <> space.
    SORT t_vtexts BY ddlanguage as4local valpos.            " Issue 96
    DELETE ADJACENT DUPLICATES FROM t_vtexts COMPARING ddlanguage valpos. " Issue 96
    s_fm_data-values_texts = t_vtexts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_doma_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_INDEX.
  DATA d_name     TYPE td_compname.
  DATA s_index    TYPE ts_idx_key.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_xinx_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd12t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    s_index = zaplink_xinx_data=>name_2_key( d_name ).
    CALL FUNCTION 'DDIF_INDX_GET'
      EXPORTING
        name                = s_index-table
        id                  = s_index-index
        state               = 'M'
*       LANGU               = ' '
      IMPORTING
*       GOTSTATE            =
        dd12v_wa            = s_fm_data-header
      TABLES
        dd17v_tab           = s_fm_data-fields
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_INDX_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd12t
      WHERE sqltab = s_index-table
        AND indexname = s_index-index
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    d_mask = s_index.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_xinx_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_LOCK_OBJECT.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_enqu_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd25t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name                = d_name
*       STATE               = 'A'
*       LANGU               = ' '
      IMPORTING
*       GOTSTATE            =
        dd25v_wa            = s_fm_data-header
      TABLES
        dd26e_tab           = s_fm_data-tables
        dd27p_tab           = s_fm_data-fields
*       DDENA_TAB           =
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_ENQU_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd25t
      WHERE viewname = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_enqu_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_SEARCH_HELP.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_shlp_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd30t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name                = d_name
        state               = space
*       LANGU               = ' '
      IMPORTING
*       GOTSTATE            =
        dd30v_wa            = s_fm_data-header
      TABLES
        dd31v_tab           = s_fm_data-sub_sh
        dd32p_tab           = s_fm_data-fields
        dd33v_tab           = s_fm_data-ssh_params
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_SHLP_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd30t
      WHERE shlpname = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_shlp_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_TABLE.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_tabl_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA f_subcomp  TYPE td_with_subcomp.
  DATA t_texts    TYPE STANDARD TABLE OF dd02t WITH DEFAULT KEY.
  DATA t_ftexts   TYPE STANDARD TABLE OF dd03t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    f_subcomp = component->get_with_subcomp( ).
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name                = d_name
        state               = 'M'
*       LANGU               = ' '
      IMPORTING
*       GOTSTATE            =
        dd02v_wa            = s_fm_data-header
        dd09l_wa            = s_fm_data-tech
      TABLES
        dd03p_tab           = s_fm_data-fields
        dd05m_tab           = s_fm_data-fk_fields
        dd08v_tab           = s_fm_data-forein_keys
        dd12v_tab           = s_fm_data-indexes
        dd17v_tab           = s_fm_data-idx_fields
        dd35v_tab           = s_fm_data-search_helps
        dd36m_tab           = s_fm_data-sh_fields
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_TABL_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd02t
      WHERE tabname = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    SELECT *
      INTO TABLE t_ftexts
      FROM dd03t
      WHERE tabname = d_name
        AND ddtext <> space.
    SORT t_ftexts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_ftexts COMPARING ddlanguage.
    s_fm_data-fields_texts = t_ftexts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_tabl_data=>r_doc_ids
                                              object = d_mask ).
    IF f_subcomp < sub_component-with_mine.
* Table Only
      CLEAR: s_fm_data-indexes, s_fm_data-idx_fields.
    ELSEIF f_subcomp >= sub_component-with_all
        OR f_subcomp >= sub_component-with_required AND s_fm_data-header-contflag CO 'CG'.
* C	Customizing table, maintenance only by cust., not SAP import
* G	Customizing table, protected against SAP Upd., only INS all.
      s_fm_data-with_content = abap_true.
    ENDIF.

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_TABLE_CONTENT.
  DATA d_table    TYPE zaplink_tabu_data=>td_table_name.

  TRY.
    d_table = component->get_name( ).

    CREATE OBJECT object.
    object->from_data( d_table ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_TABLE_TYPE.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_ttyp_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd40t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name                = d_name
        STATE               = space
*       LANGU               = ' '
      IMPORTING
*       GOTSTATE            =
        DD40V_WA            = s_fm_data-header
      TABLES
        DD42V_TAB           = s_fm_data-keys
      EXCEPTIONS
        ILLEGAL_INPUT       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_TTYP_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd40t
      WHERE typename = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_ttyp_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_TYPE_GROUP.
  DATA d_type     TYPE td_typegroup.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA t_source   TYPE tt_abaprawsource.
  DATA o_src      TYPE REF TO cl_wb_source.

  TRY.
    d_type = component->get_name( ).
*  PERFORM tygr_exists IN PROGRAM saplsd31
*          USING    type
*          CHANGING subrc.
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
    o_src = get_type_group( d_type ).
    CHECK o_src IS BOUND.
    o_src->get_source_old( IMPORTING old_source = t_source ).

    CREATE OBJECT object.
    object->a0_maindata-name = d_type.
    object->a0_maindata-uccheck = o_src->l_progdir_entry-uccheck.
    SELECT SINGLE masterlang FROM tadir
                             INTO object->a0_maindata-lang
         WHERE  pgmid       = c_typegroup-kind
         AND    object      = c_typegroup-type
         AND    obj_name    = object->a0_maindata-name.
* Kurzbeschreibung lesen
    SELECT SINGLE ddtext FROM ddtypet INTO object->a0_maindata-text
           WHERE typegroup  = object->a0_maindata-name
             AND ddlanguage = object->a0_maindata-lang.

    object->set_source( t_source ).
    object->_code_signature = get_typegroup_signature( d_type ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method EXPORT_VIEW.
  DATA d_name     TYPE ddobjname.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA s_fm_data  TYPE zaplink_view_data=>ts_fm_data.
  DATA d_mask     TYPE doku_obj.
  DATA t_texts    TYPE STANDARD TABLE OF dd25t WITH DEFAULT KEY.

  TRY.
    d_name = component->get_name( ).
    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name                = d_name
        state               = 'M'
        langu               = ' '
      IMPORTING
*       GOTSTATE            =
        dd25v_wa            = s_fm_data-header
        dd09l_wa            = s_fm_data-tech
      TABLES
        dd26v_tab           = s_fm_data-tables
        dd27p_tab           = s_fm_data-fields
        dd28j_tab           = s_fm_data-joins
        dd28v_tab           = s_fm_data-wheres
      EXCEPTIONS
        illegal_input       = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_VIEW_GET' sy-subrc.
    ENDIF.

    SELECT *
      INTO TABLE t_texts
      FROM dd25t
      WHERE viewname = d_name
        AND ddtext <> space.
    SORT t_texts BY ddlanguage as4local.
    DELETE ADJACENT DUPLICATES FROM t_texts COMPARING ddlanguage.
    s_fm_data-texts = t_texts.

    d_mask = d_name.
    s_fm_data-docs = zaplink_documentation=>get( ids = zaplink_view_data=>r_doc_ids
                                              object = d_mask ).

    CREATE OBJECT object.
    object->from_data( s_fm_data ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method GET_TYPEGROUP_SIGNATURE.
  DATA t_src    TYPE tt_abaprawsource.
  DATA s_src    TYPE td_abapsource.
  DATA o_src    TYPE REF TO cl_wb_source.

  o_src = get_type_group( type_group ).
  CHECK o_src IS BOUND.
  o_src->get_source_old( IMPORTING old_source = t_src ).
  zaplink_tools=>pretty_printer( CHANGING table = t_src ).
  zaplink_tools=>condense_abap_source( CHANGING table = t_src ).
  s_src = zaplink_tools=>table_2_string( t_src ).
  result = zaplink_tools=>calculate_md5_hash( s_src ).
  endmethod.


  method GET_TYPE_GROUP.
  DATA d_type     TYPE td_typegroup.
  DATA str        TYPE string.
  DATA subrc      TYPE sy-subrc.
  DATA t_source   TYPE TABLE OF string.
  FIELD-SYMBOLS:
    <o_s> TYPE REF TO cl_wb_source.

*  PERFORM tygr_exists IN PROGRAM saplsd31
*          USING    type
*          CHANGING subrc.
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
  PERFORM xx_first_get IN PROGRAM saplsd31
          USING    type_group space dd_eu_type-type_group 'S' space 'X'
          CHANGING subrc.
* source_instance  ABAP_EDITOR DF
* uccheck = source_instance->l_progdir_entry-uccheck
*  df-ddxx-as4user = source_instance->l_progdir_entry-unam.
*  df-ddxx-as4date = source_instance->l_progdir_entry-udat.
** unicode flag
*  df-ddxx-uccheck = source_instance->l_progdir_entry-uccheck.
** Mastersprache in TADIR lesen
*  SELECT SINGLE masterlang FROM tadir
*                           INTO rsdxx-malangu
*       WHERE  pgmid       = c_typegroup-kind
*       AND    object      = c_typegroup-type
*       AND    obj_name    = df-ddxx-typegroup.
** Kurzbeschreibung lesen
*  SELECT SINGLE ddtext FROM ddtypet INTO df-ddxx-ddtext
*         WHERE typegroup  = df-ddxx-typegroup AND
*               ddlanguage = rsdxx-malangu.

  str = '(SAPLSD31)SOURCE_INSTANCE'.
  ASSIGN (str) TO <o_s>.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  result = <o_s>.
  endmethod.


  method IMPORT_DATA_ELEMENT.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_dtel_data=>ts_fm_data.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-rollname.
    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask                     = abap_true
        objname                    = d_name
        objtype                    = dd_eu_type-data_element
*     CHANGING
*       CORRNUM                    = ' '
      EXCEPTIONS
        not_executed               = 1
        object_not_found           = 0
        object_not_specified       = 3
        permission_failure         = 4
        dialog_needed              = 0      " Object is used : Issue 96
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = d_name
        dd04v_wa          = s_fm_data-header
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_DTEL_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd04t WHERE rollname = s_fm_data-header-rollname.
    MODIFY dd04t FROM TABLE s_fm_data-texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_DOMAIN.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_doma_data=>ts_fm_data.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-domname.
    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask                     = abap_true
        objname                    = d_name
        objtype                    = dd_eu_type-domain
*     CHANGING
*       CORRNUM                    = ' '
      EXCEPTIONS
        not_executed               = 1
        object_not_found           = 0
        object_not_specified       = 3
        permission_failure         = 4
        dialog_needed              = 0      " Object is used : Issue 96
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = d_name
        dd01v_wa          = s_fm_data-header
      TABLES
        dd07v_tab         = s_fm_data-values
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_DOMA_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd01t WHERE domname = s_fm_data-header-domname.
    MODIFY dd01t FROM TABLE s_fm_data-texts.

    DELETE FROM dd07t WHERE domname = s_fm_data-header-domname.
    MODIFY dd07t FROM TABLE s_fm_data-values_texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_INDEX.
  DATA s_index        TYPE ts_idx_key.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_xinx_data=>ts_fm_data.
  DATA object_name   TYPE  ddsym10tab-name.

  TRY.
    s_fm_data = o_data->to_data( ).
    s_index-table = s_fm_data-header-sqltab.   s_index-index = s_fm_data-header-indexname.
    object_name = zaplink_xinx_data=>key_2_name( s_index ).
*    CALL FUNCTION 'DD_OBJ_DEL'
*      EXPORTING
*        object_name             = object_name
*        object_type             = dd_eu_type-ext_index
*        del_state               = 'M'
**       PRID                    = 0
*     EXCEPTIONS
*       wrong_del_state         = 1
*       wrong_object_type       = 2
*       OTHERS                  = 3.
*    IF sy-subrc <> 0.
*      mac_add_mf_and_raise 'DD_OBJ_DEL' sy-subrc.
*    ENDIF.

* Provide message :-(
    CALL FUNCTION 'RS_DD_INDX_DELETE'
      EXPORTING
        objname              = s_index-table
        indexname            = s_index-index
        extension            = s_fm_data-header-isextind
      EXCEPTIONS
        object_not_found     = 0
        object_not_specified = 2
        permission_failure   = 3
        action_cancelled     = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_INDX_DELETE' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_INDX_PUT'
      EXPORTING
        name              = s_index-table
        id                = s_index-index
        dd12v_wa          = s_fm_data-header
      TABLES
        dd17v_tab         = s_fm_data-fields
      EXCEPTIONS
        indx_not_found    = 1
        name_inconsistent = 2
        indx_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_INDX_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd12t
           WHERE sqltab = s_index-table
          AND indexname = s_index-table.
    MODIFY dd12t FROM TABLE s_fm_data-texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_LOCK_OBJECT.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_enqu_data=>ts_fm_data.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-viewname.
    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask                     = abap_true
        objname                    = d_name
        objtype                    = dd_eu_type-lock_object
*     CHANGING
*       CORRNUM                    = ' '
      EXCEPTIONS
        not_executed               = 1
        object_not_found           = 0
        object_not_specified       = 3
        permission_failure         = 4
        dialog_needed              = 0      " Object is used : Issue 96
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = d_name
        dd25v_wa          = s_fm_data-header
      TABLES
        dd26e_tab         = s_fm_data-tables
        dd27p_tab         = s_fm_data-fields
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_ENQU_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd25t WHERE viewname = s_fm_data-header-viewname.
    MODIFY dd25t FROM TABLE s_fm_data-texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_SEARCH_HELP.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_shlp_data=>ts_fm_data.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-shlpname.
    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask                     = abap_true
        objname                    = d_name
        objtype                    = dd_eu_type-search_help
*     CHANGING
*       CORRNUM                    = ' '
      EXCEPTIONS
        not_executed               = 1
        object_not_found           = 0
        object_not_specified       = 3
        permission_failure         = 4
        dialog_needed              = 0      " Object is used : Issue 96
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_SHLP_PUT'
      EXPORTING
        name              = d_name
        dd30v_wa          = s_fm_data-header
      TABLES
        dd31v_tab         = s_fm_data-sub_sh
        dd32p_tab         = s_fm_data-fields
        dd33v_tab         = s_fm_data-ssh_params
      EXCEPTIONS
        shlp_not_found    = 1
        name_inconsistent = 2
        shlp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_SHLP_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd30t WHERE shlpname = s_fm_data-header-shlpname.
    MODIFY dd30t FROM TABLE s_fm_data-texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TABLE.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_tabl_data=>ts_fm_data.
  DATA t_idx_flds     TYPE zaplink_tabl_data=>tt_dd17v.
  DATA d_cname        TYPE td_compname.
  FIELD-SYMBOLS: <i> LIKE LINE OF s_fm_data-indexes.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-tabname.
* Issue 85
*    CALL FUNCTION 'RS_DD_DELETE_OBJ'
*      EXPORTING
*        no_ask                     = abap_true
*        objname                    = d_name
*        objtype                    = dd_eu_type-transp_table
**     CHANGING
**       CORRNUM                    = ' '
*      EXCEPTIONS
*        not_executed               = 1
*        object_not_found           = 0
*        object_not_specified       = 3
*        permission_failure         = 4
*        dialog_needed              = 0      " Object is used : Issue 96
*        OTHERS                     = 6.
*    IF sy-subrc <> 0.
*      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
*    ENDIF.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name                    = d_name
        dd02v_wa                = s_fm_data-header
        dd09l_wa                = s_fm_data-tech
      TABLES
          dd03p_tab           = s_fm_data-fields
          dd05m_tab           = s_fm_data-fk_fields
          dd08v_tab           = s_fm_data-forein_keys
*        dd12v_tab           = s_fm_data-indexes
*        dd17v_tab           = s_fm_data-idx_fields
          dd35v_tab           = s_fm_data-search_helps
          dd36m_tab           = s_fm_data-sh_fields
      EXCEPTIONS
        tabl_not_found          = 1
        name_inconsistent       = 2
        tabl_inconsistent       = 3
        put_failure             = 4
        put_refused             = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_TABL_PUT' sy-subrc.
    ENDIF.

    LOOP AT s_fm_data-indexes ASSIGNING <i>.
      t_idx_flds = s_fm_data-idx_fields.    DELETE t_idx_flds WHERE indexname <> <i>-indexname.
      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = d_name
          id                = <i>-indexname
          dd12v_wa          = <i>
        TABLES
          dd17v_tab         = t_idx_flds
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        mac_add_mf_and_raise 'DDIF_INDX_PUT' sy-subrc.
      ENDIF.
    ENDLOOP.

    DELETE FROM dd02t WHERE tabname = s_fm_data-header-tabname.
    MODIFY dd02t FROM TABLE s_fm_data-texts.

    DELETE FROM dd03t WHERE tabname = s_fm_data-header-tabname.
    MODIFY dd03t FROM TABLE s_fm_data-fields_texts.

    zaplink_documentation=>set( s_fm_data-docs ).

* Handle Content
    IF s_fm_data-with_content = abap_true.
      d_cname = s_fm_data-header-tabname.
*      COMMIT WORK AND WAIT.
      IF NOT active_component( type = supportedtypes-table
                               name = d_cname ) = abap_true.
* Can't insert data into table '&1', because table don't exists as active
        SET EXTENDED CHECK OFF.
        IF 1 = 2.   MESSAGE i000 WITH s_fm_data-header-tabname.   ENDIF.
        SET EXTENDED CHECK ON.
        CALL METHOD application_log->add_error
          EXPORTING
*        id_msgid     =
            id_msgno     = '000'
*        id_msgv4     =
            id_msgv1     = s_fm_data-header-tabname.
      ELSEIF NOT o_data->content IS INITIAL.
        DATA content TYPE REF TO data.
        DATA o_node TYPE REF TO if_ixml_element.
        DATA ez_xml    TYPE REF TO zaplink_easyxml.
        FIELD-SYMBOLS:
          <n>  TYPE ANY,
          <tc> TYPE STANDARD TABLE.

        CREATE OBJECT ez_xml.
* Table
        CREATE DATA content TYPE STANDARD TABLE OF (s_fm_data-header-tabname).
        ASSIGN content->* TO <tc>.
* Node
        ASSIGN o_data->content->* TO <n>.
        o_node = <n>.
* Convert
        ez_xml->read_any( EXPORTING xml_node = o_node
                           CHANGING      any = <tc> ).
        INSERT (s_fm_data-header-tabname) FROM TABLE <tc>.
* Ignoring errors : previous existing entries don't want to be overwrited
      ENDIF.
    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TABLE_TYPE.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_ttyp_data=>ts_fm_data.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-typename.
    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask                     = abap_true
        objname                    = d_name
        objtype                    = dd_eu_type-table_type
*     CHANGING
*       CORRNUM                    = ' '
      EXCEPTIONS
        not_executed               = 1
        object_not_found           = 0
        object_not_specified       = 3
        permission_failure         = 4
        dialog_needed              = 0      " Object is used : Issue 96
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = d_name
        dd40v_wa          = s_fm_data-header
      TABLES
        dd42v_tab         = s_fm_data-keys
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_TTYP_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd40t WHERE typename = s_fm_data-header-typename.
    MODIFY dd40t FROM TABLE s_fm_data-texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_TYPE_GROUP.
  DATA d_typegroup    TYPE td_typegroup.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA t_source       TYPE zaplink_type_data=>tt_string.
  DATA d_program      TYPE progdir.
  DATA str            TYPE string.
  FIELD-SYMBOLS:
    <prog> TYPE sy-repid.          "Typgruppeninclude

  TRY.
    d_typegroup = o_data->a0_maindata-name.
* Check existence
    PERFORM tygr_exists IN PROGRAM saplsd31
          USING    d_typegroup
          CHANGING d_subrc.

    IF d_subrc = 0.
      d_name = d_typegroup.
      CALL FUNCTION 'RS_DD_DELETE_OBJ'
        EXPORTING
*       NO_ASK                     = ' '
          objname                    = d_name
          objtype                    = dd_eu_type-type_group
*     CHANGING
*       CORRNUM                    = ' '
        EXCEPTIONS
          not_executed               = 1
          object_not_found           = 0
          object_not_specified       = 3
          permission_failure         = 4
          dialog_needed              = 0      " Object is used : Issue 96
          OTHERS                     = 6.
      IF sy-subrc <> 0.
        mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
      ENDIF.
    ENDIF.

    t_source = o_data->get_source( ).
    CALL FUNCTION 'RS_DD_TYGR_INSERT_SOURCES'
      EXPORTING
        typegroupname        = o_data->a0_maindata-name
        ddtext               = o_data->a0_maindata-text
        corrnum              = space
        devclass             = space
      TABLES
        SOURCE               = t_source
      EXCEPTIONS
        already_exists       = 1
        not_executed         = 2
        permission_failure   = 3
        object_not_specified = 4
        illegal_name         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_TYGR_INSERT_SOURCES' sy-subrc.
    ENDIF.

    PERFORM tygr_exists IN PROGRAM saplsd31
            USING    d_typegroup
            CHANGING d_subrc.
    IF d_subrc <> 0.
      EXIT.
    ENDIF.

    str = '(SAPLSD31)PROGNAME'.
    ASSIGN (str) TO <prog>.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname          = <prog>
*     I_STATE             = ' '
      IMPORTING
        e_progdir           = d_program
*     E_PROGDIR_TAB       =
      EXCEPTIONS
        not_exists          = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'READ_PROGDIR' sy-subrc.
    ENDIF.

    IF d_program-uccheck <> o_data->a0_maindata-uccheck.
      d_program-uccheck = o_data->a0_maindata-uccheck.
      CALL FUNCTION 'UPDATE_PROGDIR'
        EXPORTING
          i_progdir    = d_program
          i_progname   = <prog>
          i_state      = d_program-state
        EXCEPTIONS
          not_executed = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        mac_add_mf_and_raise 'UPDATE_PROGDIR' sy-subrc.
      ENDIF.
    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method IMPORT_VIEW.
  DATA d_name         TYPE rsedd0-ddobjname.
  DATA d_subrc        TYPE sy-subrc.
  DATA s_fm_data      TYPE zaplink_view_data=>ts_fm_data.

  TRY.
    s_fm_data = o_data->to_data( ).
    d_name = s_fm_data-header-viewname.
    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask                     = abap_true
        objname                    = d_name
        objtype                    = dd_eu_type-view
*     CHANGING
*       CORRNUM                    = ' '
      EXCEPTIONS
        not_executed               = 1
        object_not_found           = 0
        object_not_specified       = 3
        permission_failure         = 4
        dialog_needed              = 0      " Object is used : Issue 96
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
    ENDIF.

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = d_name
        dd25v_wa          = s_fm_data-header
        dd09l_wa          = s_fm_data-tech
      TABLES
        dd26v_tab         = s_fm_data-tables
        dd27p_tab         = s_fm_data-fields
        dd28j_tab         = s_fm_data-joins
        dd28v_tab         = s_fm_data-wheres
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'DDIF_VIEW_PUT' sy-subrc.
    ENDIF.

    DELETE FROM dd25t WHERE viewname = s_fm_data-header-viewname.
    MODIFY dd25t FROM TABLE s_fm_data-texts.

    zaplink_documentation=>set( s_fm_data-docs ).

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  mac_obj_data.

  DEFINE lmac_do_obj.
    &1 ?= object->raw.
    &1->anonymize( ).
  END-OF-DEFINITION.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN supportedtypes-type_group.     lmac_do_obj o_typegroup.
    WHEN supportedtypes-domain.         lmac_do_obj o_domain.
    WHEN supportedtypes-data_element.   lmac_do_obj o_data_e.
    WHEN supportedtypes-view.           lmac_do_obj o_view.
    WHEN supportedtypes-table.          lmac_do_obj o_table.        " Issue 86
    WHEN supportedtypes-ext_index.      lmac_do_obj o_index.        " Issue 55
    WHEN supportedtypes-table_type.     lmac_do_obj o_tab_type.     " Issue 86
    WHEN supportedtypes-search_help.    lmac_do_obj o_s_help.       " Issue 86
    WHEN supportedtypes-lock_object.    lmac_do_obj o_lock_obj.     " Issue 86
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.      " Issue 86
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
  DATA d_type   TYPE td_comptype.
  DATA d_tgrp   TYPE td_typegroup.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN supportedtypes-type_group.
        d_tgrp = component->get_name( ).
        result = get_typegroup_signature( d_tgrp ).
      WHEN supportedtypes-domain OR supportedtypes-data_element OR supportedtypes-table_type OR supportedtypes-search_help OR supportedtypes-mactchcode_id OR
           supportedtypes-lock_object OR supportedtypes-matchcode_obj OR supportedtypes-table_index OR supportedtypes-ext_index OR supportedtypes-table  OR supportedtypes-view.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  mac_obj_data.
  DATA o_raw_idx TYPE to_raw_index.

  DEFINE lmac_obj_create.
    create object &1.
    object->raw = &1.
  END-OF-DEFINITION.

  CREATE OBJECT object.
  CASE type.
    WHEN supportedtypes-type_group.     lmac_obj_create o_typegroup.
    WHEN supportedtypes-domain.         lmac_obj_create o_domain.
    WHEN supportedtypes-data_element.   lmac_obj_create o_data_e.
    WHEN supportedtypes-view.           lmac_obj_create o_view.
    WHEN supportedtypes-table.          lmac_obj_create o_table.
    WHEN supportedtypes-table_content.  lmac_obj_create o_content.
    WHEN supportedtypes-ext_index OR supportedtypes-table_index.
      CREATE OBJECT o_raw_idx.
      object = o_raw_idx.
      lmac_obj_create o_index.                              " Issue 55
    WHEN supportedtypes-table_type.     lmac_obj_create o_tab_type.
    WHEN supportedtypes-search_help.    lmac_obj_create o_s_help.
    WHEN supportedtypes-lock_object.    lmac_obj_create o_lock_obj.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA d_name    TYPE td_compname.
  DATA d_objname TYPE rsedd0-ddobjname.
  DATA d_type    TYPE rsedd0-ddobjtype.
  DATA type      TYPE td_comptype.
  DATA s_index   TYPE ts_idx_key.

  TRY.

    type = component->get_type( ).
    d_name = component->get_name( ).
    CASE type.
      WHEN supportedtypes-type_group.
        d_type = dd_eu_type-type_group.
      WHEN supportedtypes-domain.
        d_type = dd_eu_type-domain.
      WHEN supportedtypes-data_element.
        d_type = dd_eu_type-data_element.
      WHEN supportedtypes-table_type.
        d_type = dd_eu_type-table_type.
      WHEN supportedtypes-search_help.
        d_type = dd_eu_type-search_help.
      WHEN supportedtypes-mactchcode_id.
        d_type = dd_eu_type-mactchcode_id.
      WHEN supportedtypes-lock_object.
        d_type = dd_eu_type-lock_object.
      WHEN supportedtypes-matchcode_obj.
        d_type = dd_eu_type-matchcode_obj.
*      WHEN supportedtypes-table_index.
*        d_type = dd_eu_type-table_index.
      WHEN supportedtypes-ext_index OR supportedtypes-table_index.                        " Issue 55
        IF type = supportedtypes-table_index.   d_type = dd_eu_type-table_index.    ELSE.   d_type = dd_eu_type-ext_index.    ENDIF.
        s_index = zaplink_xinx_data=>name_2_key( d_name ).
        CALL FUNCTION 'RS_DD_INDX_DELETE'
          EXPORTING
            objname              = s_index-table
            indexname            = s_index-index
            extension            = 'X'
          EXCEPTIONS
            object_not_found     = 1
            object_not_specified = 2
            permission_failure   = 3
            action_cancelled     = 4
            OTHERS               = 5.
        IF sy-subrc <> 0.
          CASE sy-subrc.
            WHEN 1.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING
                  textid = zaplink_cx_connector=>not_found.
            WHEN OTHERS.
              mac_add_mf_and_raise 'RS_DD_INDX_DELETE' sy-subrc.
          ENDCASE.
        ENDIF.
* Issue 137 => Remove from Working Area
        zaplink_tools=>remove_comp_from_working_area( type = type
                                                      name = d_name ).
        EXIT.
      WHEN supportedtypes-table_content.
*        DELETE
        EXIT.
      WHEN supportedtypes-table.
        d_type = dd_eu_type-transp_table.
      WHEN supportedtypes-view.
        d_type = dd_eu_type-view.
*        WHEN supportedtypes-t_cluster_pool.
*          d_type = dd_eu_type-t_cluster_pool.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    IF NOT d_type IS INITIAL.
      d_objname = d_name.
      CALL FUNCTION 'RS_DD_DELETE_OBJ'
        EXPORTING
          no_ask                     = 'X'
          objname                    = d_objname
          objtype                    = d_type
*         CHANGING
*           CORRNUM                    = ' '
        EXCEPTIONS
          not_executed               = 1
          object_not_found           = 2
          object_not_specified       = 3
          permission_failure         = 4
          dialog_needed              = 5
          OTHERS                     = 6.
      IF sy-subrc <> 0. " SAP NameSpace
        CASE sy-subrc.
          WHEN 2.
            RAISE EXCEPTION TYPE zaplink_cx_connector
              EXPORTING
                textid = zaplink_cx_connector=>not_found.
          WHEN OTHERS.
            mac_add_mf_and_raise 'RS_DD_DELETE_OBJ' sy-subrc.
        ENDCASE.
      ENDIF.
    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  result = abap_true.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = type
                                                name = d_name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA d_name      TYPE td_compname.
  DATA d_ddname    TYPE rsedd0-ddobjname.
  DATA d_type      TYPE rsedd0-ddobjtype.
  DATA type        TYPE td_comptype.
  DATA s_idxkey    TYPE ts_idx_key.
  DATA d_secname   TYPE rsedd0-indexid.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    d_name = component->get_name( ).
    CASE type.
      WHEN supportedtypes-type_group.
        d_type = dd_eu_type-type_group.
      WHEN supportedtypes-domain.
        d_type = dd_eu_type-domain.
      WHEN supportedtypes-data_element.
        d_type = dd_eu_type-data_element.
      WHEN supportedtypes-table_type.
        d_type = dd_eu_type-table_type.
      WHEN supportedtypes-search_help.
        d_type = dd_eu_type-search_help.
      WHEN supportedtypes-mactchcode_id.
        d_type = dd_eu_type-mactchcode_id.
      WHEN supportedtypes-lock_object.
        d_type = dd_eu_type-lock_object.
      WHEN supportedtypes-matchcode_obj.
        d_type = dd_eu_type-matchcode_obj.
      WHEN supportedtypes-ext_index.
        d_type = dd_eu_type-ext_index.
        s_idxkey = zaplink_xinx_data=>name_2_key( d_name ).
        d_secname = s_idxkey-index.
        d_name = s_idxkey-table.
      WHEN supportedtypes-table_index.
        d_type = dd_eu_type-table_index.
        s_idxkey = zaplink_xinx_data=>name_2_key( d_name ).
        d_secname = s_idxkey-index.
        d_name = s_idxkey-table.
      WHEN supportedtypes-table OR supportedtypes-table_content OR supportedtypes-t_cluster_pool.
        d_type = dd_eu_type-transp_table.
      WHEN supportedtypes-view.
        d_type = dd_eu_type-view.
*        WHEN supportedtypes-t_cluster_pool.
*          d_type = dd_eu_type-t_cluster_pool.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.
    d_ddname = d_name.

    IF NOT d_type IS INITIAL AND NOT d_name IS INITIAL.
      CALL FUNCTION 'INTERN_DD_CHECK_EXIST'
        EXPORTING
          objname                     = d_ddname
          objtype                     = d_type
          err_message                 = abap_false
*           OBJSTATE                    = 'M'
          exists                      = abap_true
          secname                     = d_secname
*         IMPORTING
*           MASTERLANGU                 =
*           OTHER_CLASS                 =
*           PROXY_TYPE                  =
        EXCEPTIONS
          object_not_found            = 1
          other_object_class          = 2
          invalid_parameter           = 3
          object_already_exists       = 4
          OTHERS                      = 5.
      IF sy-subrc = 0. " SAP NameSpace
        exists = me->exists-exists.
      ELSE.
        CASE sy-subrc.
          WHEN 1  OR 2.
          WHEN OTHERS.
            mac_add_mf_and_raise 'INTERN_DD_CHECK_EXIST' sy-subrc.
        ENDCASE.
      ENDIF.

    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.    result = abap_true.   endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  mac_obj_data.
  DATA d_type       TYPE td_comptype.

  DEFINE mac_catch_local_cx.
    catch zaplink_cx_connector into o_mycx.
      if not o_mycx->messages is bound.
        o_mycx->messages = application_log.
        o_mycx->update( ).
      endif.
      raise exception o_mycx.
    END-OF-DEFINITION.

    TRY.
      d_type = component->get_type( ).
      CREATE OBJECT object.
      object->set_component( component ).

      CASE d_type.
        WHEN supportedtypes-type_group.
          TRY.
            o_typegroup = export_type_group( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_typegroup->a0_maindata-name.
          object->raw = o_typegroup.
        WHEN supportedtypes-domain.
          TRY.
            o_domain = export_domain( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_domain->a0_maindata-domname.
          object->raw = o_domain.
        WHEN supportedtypes-data_element.
          TRY.
            o_data_e = export_data_element( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_data_e->a0_maindata-rollname.
          object->raw = o_data_e.
        WHEN supportedtypes-view.
          TRY.
            o_view = export_view( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_view->a0_maindata-viewname.
          object->raw = o_view.
        WHEN supportedtypes-table.
          TRY.
            o_table = export_table( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_table->a0_maindata-tabname.
          object->raw = o_table.
        WHEN supportedtypes-table_content.
          TRY.
            o_content = export_table_content( component ).
            mac_catch_local_cx.
          ENDTRY.
*          CLEAR o_content->a0_maindata-tabname.
          object->raw = o_content.
        WHEN supportedtypes-ext_index or supportedtypes-table_index.                      " Issue 55
          TRY.
            o_index = export_index( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR: o_index->a0_maindata-sqltab,  o_index->a0_maindata-indexname.
          object->raw = o_index.
        WHEN supportedtypes-table_type.
          TRY.
            o_tab_type = export_table_type( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_tab_type->a0_maindata-typename.
          object->raw = o_tab_type.
        WHEN supportedtypes-search_help.
          TRY.
            o_s_help = export_search_help( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_s_help->a0_maindata-shlpname.
          object->raw = o_s_help.
        WHEN supportedtypes-lock_object.
          TRY.
            o_lock_obj = export_lock_object( component ).
            mac_catch_local_cx.
          ENDTRY.
          CLEAR o_lock_obj->a0_maindata-viewname.
          object->raw = o_lock_obj.
        WHEN OTHERS.
          CLEAR object.
          mac_raise_type_not_supported me->class_name d_type.
      ENDCASE.
      object->update_connector_data( me ).                  " Issue 66
      mac_def_catch zaplink_cx_connector.
    ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  mac_obj_data.
  DATA o_comp       TYPE to_component.
  DATA s_index      TYPE ts_idx_key.

  DEFINE lmac_catch_local_cx.
    catch zaplink_cx_connector into o_mycx.
      if not o_mycx->messages is bound.
        o_mycx->messages = application_log.
        o_mycx->update( ).
      endif.
      raise exception o_mycx.
  END-OF-DEFINITION.

  TRY.
    CASE object->type.
      WHEN supportedtypes-type_group.
        o_typegroup ?= object->raw.
        o_typegroup->a0_maindata-name = object->name.
        TRY.
          components = import_type_group( o_typegroup ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-domain.
        o_domain ?= object->raw.
        o_domain->a0_maindata-domname = object->name.
        TRY.
          components = import_domain( o_domain ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-data_element.
        o_data_e ?= object->raw.
        o_data_e->a0_maindata-rollname = object->name.
        TRY.
          components = import_data_element( o_data_e ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-view.
        o_view ?= object->raw.
        o_view->a0_maindata-viewname = object->name.
        TRY.
          components = import_view( o_view ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-table.
        o_table ?= object->raw.
        o_table->a0_maindata-tabname = object->name.
        TRY.
          components = import_table( o_table ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-table_content.
        o_content ?= object->raw.
*            o_content->a0_maindata-tabname = object->name.
        TRY.
*              components = import_table_content( o_content ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-ext_index OR supportedtypes-table_index. " Issue 55
        o_index ?= object->raw.
        s_index = zaplink_xinx_data=>name_2_key( object->name ).
        o_index->a0_maindata-sqltab = s_index-table.
        o_index->a0_maindata-indexname = s_index-index.
        TRY.
          components = import_index( o_index ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-table_type.
        o_tab_type ?= object->raw.
        o_tab_type->a0_maindata-typename = object->name.
        TRY.
          components = import_table_type( o_tab_type ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-search_help.
        o_s_help ?= object->raw.
        o_s_help->a0_maindata-shlpname = object->name.
        TRY.
          components = import_search_help( o_s_help ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN supportedtypes-lock_object.
        o_lock_obj ?= object->raw.
        o_lock_obj->a0_maindata-viewname = object->name.
        TRY.
          components = import_lock_object( o_lock_obj ).
          lmac_catch_local_cx.
        ENDTRY.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name object->type.
    ENDCASE.

    check_component_list( EXPORTING     object = object
                           CHANGING components = components ). " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
