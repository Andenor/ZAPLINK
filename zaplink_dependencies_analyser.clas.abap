class ZAPLINK_DEPENDENCIES_ANALYSER definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools SANA .
  type-pools SEOP .

  interfaces ZAPLINK_DATATYPES .
  interfaces ZAPLINK_KERNEL_TYPES .

  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TD_SOFTCOMP
    for ZAPLINK_DATATYPES~TD_SOFTCOMP .
  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .

  types TD_ORIGIN type TD_PROGNAME .
  types TD_CLASSTYPE type CHAR01 .
  types TD_CLASSCAT type SEOCATEGRY .
  types TD_CONTEXT type STRING .
  types:
    tr_packages TYPE RANGE OF tdevc-devclass .
  types:
    tr_dlvunit TYPE RANGE OF tdevc-dlvunit .
  types TD_NAME type SEOCLSNAME .
  types:
    BEGIN OF ts_source,
            name  TYPE td_name,
            type  TYPE td_classtype,    " 0 Class 1 interface
            def   TYPE tt_abaprawsource,
            macro TYPE tt_abaprawsource,
            impl  TYPE tt_abaprawsource,
          END OF ts_source .
  types:
    tt_sources TYPE SORTED TABLE OF ts_source WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_list,
            name      TYPE td_name,
          END OF ts_list .
  types:
    BEGIN OF ts_req_object,
            ori_name  TYPE td_name,
            name      TYPE td_name,
            type      TYPE td_classtype,    " 0 Class 1 interface
            full_def  TYPE flag,          " Full definition required
            source    TYPE td_context,
          END OF ts_req_object .
  types:
    tt_req_objects TYPE STANDARD TABLE OF ts_req_object WITH NON-UNIQUE KEY name .
  types:
    BEGIN OF ts_order.
    INCLUDE TYPE ts_list.
    TYPES:
      type    TYPE td_classtype,    " 0 Class 1 interface
      defered TYPE flag,          " 'X' if defered load required
    END OF ts_order .
  types:
    tt_list TYPE STANDARD TABLE OF ts_order WITH DEFAULT KEY .
  types:
    BEGIN OF ts_context,
              context   TYPE td_context,
              type      TYPE td_classtype,    " 0 Class 1 interface
              full_def  TYPE flag,          " Full definition required
            END OF ts_context .
  types:
    tt_context TYPE STANDARD TABLE OF ts_context WITH DEFAULT KEY .
  types:
    tt_order TYPE STANDARD TABLE OF ts_order WITH NON-UNIQUE KEY name .
  types:
    BEGIN OF ts_typegroup,
                  name TYPE seotpuname,
                END OF ts_typegroup .
  types:
    tt_typegroups TYPE STANDARD TABLE OF ts_typegroup WITH DEFAULT KEY .
  types:
    BEGIN OF ts_comp.
    INCLUDE TYPE ts_compkey AS hdr.
    TYPES:
        kind     TYPE td_transport_kind,
        devclass TYPE td_devclass,
        softcomp TYPE td_softcomp,
      END OF ts_comp .
  types:
    tt_comps TYPE SORTED TABLE OF ts_comp WITH UNIQUE KEY type name .
  types TD_MESSAGE type SEOCLASSDF-MSG_ID .
  types:
    BEGIN OF ts_message,
                  name TYPE td_message,
                END OF ts_message .
  types:
    tt_message_ids TYPE STANDARD TABLE OF ts_message WITH DEFAULT KEY .

  data CLASSES type TT_SOURCES read-only .
  data ORDER type TT_ORDER read-only .
  constants:
    BEGIN OF typetype,
                like    TYPE seotyptype VALUE '0', "#EC NOTEXT      Attribute reference (LIKE)
                type    TYPE seotyptype VALUE '1', "#EC NOTEXT      Type reference (TYPE)
                otype   TYPE seotyptype VALUE '2', "#EC NOTEXT      Object (TYPE)
                ref     TYPE seotyptype VALUE '3', "#EC NOTEXT      Object reference (TYPE REF TO)
                code    TYPE seotyptype VALUE '4', "#EC NOTEXT      See coding
              END OF typetype .
  constants:
    BEGIN OF objtype,
                class             TYPE td_classtype VALUE '0', "#EC NOTEXT      Object Type : Class
                exception_class   TYPE td_classtype VALUE '5', "#EC NOTEXT      Local Object type : Exception_Class
                interface         TYPE td_classtype VALUE '1', "#EC NOTEXT      Object Type : Interface
              END OF objtype .
  constants:
    BEGIN OF objcat,
*00	General Object Type
*01	Exit Class
*10	Persistent Class
*11	Factory for Persistent Class
*12	Status Class for Persistent Class
*20	View Class
*30	Proxy Class for Remote Interface
*40	Exception Class
*50	Business Class
*51	Business Class Interface for Static Components
*52	Business Class Interface for Instance Dependent Components
*60	BSP Application Class
*70	Basis Class for BSP Element Handlers
*80	Web Dynpro Runtime Object
*90	ESI: Provider Interface (generated)
*45	Area Class (Shared Objects)
*05	Test Class (ABAP Unit)
        class       TYPE td_classcat VALUE '00',            "#EC NOTEXT
        exit        TYPE td_classcat VALUE '01',            "#EC NOTEXT
        test        TYPE td_classcat VALUE '05',            "#EC NOTEXT
        persistent  TYPE td_classcat VALUE '10',            "#EC NOTEXT
        factory     TYPE td_classcat VALUE '11',            "#EC NOTEXT
        proxy       TYPE td_classcat VALUE '30',            "#EC NOTEXT
        exception   TYPE td_classcat VALUE '40',            "#EC NOTEXT
        bsp         TYPE td_classcat VALUE '60',            "#EC NOTEXT
        END OF objcat .
  data TYPEGROUPS type TT_TYPEGROUPS .
  class-data SOFTCOMPONENTS type TR_DLVUNIT read-only .
  data INCLUDES type TT_ABAPRAWSOURCE .
  data PACKAGES type TR_PACKAGES .
  data MESSAGES type TT_MESSAGE_IDS read-only .
  data NEW_ORDER type TT_ORDER .
  data REMAIN_CLASSES type TT_SOURCES .
  data ALL_COMPONENTS type TT_COMPS read-only .
  data TEXTSPOOL type TT_TXTP_TEXTPOOLS read-only .

  methods ADD_OBJECT
    importing
      !NAME type TD_NAME
    raising
      ZAPLINK_CX .
  methods ADD_SOURCE
    importing
      !CODE type TT_ABAPRAWSOURCE
      !ORIGIN type TD_ORIGIN optional
    raising
      ZAPLINK_CX .
  methods ADD_WITHOUT_REQUIREMENTS
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_SOURCE
    importing
      !NAME type TD_NAME
    returning
      value(CODES) type TS_SOURCE
    raising
      ZAPLINK_CX .
  methods RESOLVE .
  methods SET_TEXTPOOL
    importing
      !NAME type TD_PROGNAME
    raising
      ZAPLINK_CX .
  class-methods SOURCE_2_STRING
    importing
      !DATA type TT_ABAPRAWSOURCE
    returning
      value(RESULT) type STRING .
protected section.

  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TO_COMPONENT
    for ZAPLINK_KERNEL_TYPES~TO_COMPONENT .
  aliases TO_RAW
    for ZAPLINK_KERNEL_TYPES~TO_RAW .
  aliases TS_COMPTYPE
    for ZAPLINK_DATATYPES~TS_COMPTYPE .

  types:
    tt_classlist TYPE SORTED TABLE OF ts_list WITH UNIQUE KEY name .
  types TO_CLASS type ref to ZAPLINK_CLAS_DATA .
  types TO_OBJECT type ref to ZAPLINK_OBJECT .
  types TO_INTERFACE type ref to ZAPLINK_INTF_DATA .
  types TO_EXCEPTION type ref to ZAPLINK_CX .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .

  data DEFERRED_DECLARATION type TT_CLASSLIST .

  methods ADD_ATTRIBUTS
    importing
      !ATTRIBUTS type ZAPLINK_OBJECT_DATA=>TT_ATTRIBUTS
      !ORIGIN type TD_ORIGIN
    raising
      ZAPLINK_CX .
  methods ADD_CLASS_DEP
    importing
      !NAME type TD_NAME
      !FULL type ABAP_BOOL default ABAP_FALSE
    raising
      ZAPLINK_CX .
  methods ADD_DEFERRED_CODE
    importing
      !TYPE type TD_CLASSTYPE
      !NAME type TD_NAME
    changing
      !ABAPCODE type TT_ABAPRAWSOURCE .
  methods ADD_DEFERRED_OK
    returning
      value(RESULT) type ABAP_BOOL .
  methods ADD_INCLUDE
    importing
      !NAME type TD_PROGNAME .
  methods ADD_INTERFACES
    importing
      !INTERFACES type ZAPLINK_OBJECT_DATA=>TT_INTERFACES
    raising
      ZAPLINK_CX .
  methods ADD_INTERFACE_DEP
    importing
      !NAME type TD_NAME
      !FULL type ABAP_BOOL default ABAP_FALSE
    raising
      ZAPLINK_CX .
  methods ADD_METHODS
    importing
      !METHODS type ZAPLINK_OBJECT_DATA=>TT_METHODS
      !ORIGIN type TD_ORIGIN optional
    raising
      ZAPLINK_CX .
  methods ADD_METHOD_SOURCE
    importing
      !CODE type TD_ABAPSOURCE
      !ORIGIN type TD_ORIGIN
    raising
      ZAPLINK_CX .
  methods ADD_OBJECT_TO_LIST
    importing
      !NAME type TD_NAME
      !FULL type ABAP_BOOL default ABAP_FALSE
      !POSITION type TD_CONTEXT
    raising
      ZAPLINK_CX .
  methods ADD_PROG_TEXTPOOL
    importing
      !NAME type TD_PROGNAME .
  methods ADD_TYPES
    importing
      !TYPES type ZAPLINK_OBJECT_DATA=>TT_TYPES
      !ORIGIN type TD_ORIGIN
    raising
      ZAPLINK_CX .
  methods ANALYSE_SOURCE
    importing
      !CODE type TD_ABAPSOURCE
      !ORIGIN type TD_ORIGIN
    returning
      value(OBJECTS_LIST) type TT_ABAPRAWSOURCE .
  methods APPEND_MESSAGE
    importing
      !DATA type TD_MESSAGE .
  methods APPEND_TYPEGROUPS
    importing
      !TYPEGROUPS type ZAPLINK_OBJECT_DATA=>TT_TYPEGROUPS .
  class-methods CONVERT_LINE
    importing
      !SOURCE type STRING
    returning
      value(TARGET) type STRING .
  class-methods CONVERT_SOURCE_UPCASE
    importing
      !SOURCECODE type TT_ABAPRAWSOURCE
    returning
      value(TARGETCODE) type TT_ABAPRAWSOURCE .
  class-methods EXTRACT_STRINGS
    importing
      !FORWARD type FLAG optional
      !SEARCH_TERM type STRING
      !CODE type STRING
    preferred parameter FORWARD
    returning
      value(RESULTS) type TT_ABAPRAWSOURCE .
  class-methods GET_CLASS_SOURCE
    importing
      !NAME type TD_NAME
    returning
      value(CODES) type TS_SOURCE .
  class-methods GET_INTERFACE_SOURCE
    importing
      !NAME type TD_NAME
    returning
      value(CODES) type TS_SOURCE .
  methods HANDLE_CONTEXT
    raising
      ZAPLINK_CX .
  class-methods INVERSE_ORDER
    changing
      !ORDER type STANDARD TABLE .
  methods IS_EXCLUDED_CLASS
    importing
      !NAME type TD_NAME
      !TYPE type TADIR-OBJECT optional
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_EXCLUDED_INCLUDE
    importing
      !NAME type TD_NAME
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX .
  class-methods IS_PREDEFTYPE
    importing
      !NAME type TD_NAME
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX .
  methods PROCESS_TYPE
    importing
      !TYPE type SEOTYPTYPE
      !ORIGIN type TD_ORIGIN optional
      !NAME type RS38L_TYP
      !SOURCE type STRING optional
    raising
      ZAPLINK_CX .
  class-methods REMOVE_DEF_LOAD
    changing
      !CODES type TS_SOURCE .
  methods SEARCH_FOR_INCLUDE
    changing
      !CODES type TS_SOURCE
    raising
      ZAPLINK_CX .
  methods STACK
    importing
      !CONTEXT type ANY
      !TYPE type TD_CLASSTYPE optional
      !FULL type ABAP_BOOL default ABAP_FALSE .
  methods UNSTACK .
  methods _CLEAN_UP .
private section.

  types:
    tt_predeftypes TYPE HASHED TABLE OF sana_type_struct WITH UNIQUE KEY name .

  class-data O_OBJECT type TO_OBJECT .
  data CONTEXT type TT_CONTEXT .
  data CIRCULAR_REF_ON type TD_NAME .
  data CIRCULAR_CONTEXT type TT_CONTEXT .
  data REQUIRED_OBJECTS type TT_REQ_OBJECTS .
  data CURRENT_COMP type TD_NAME .
  data EXCLUDED_INCLUDES type TT_CLASSLIST .
  data EXCLUDED_CLASSES type TT_CLASSLIST .
  data REQUIREMENTS type TT_REQ_OBJECTS .
  data S_COMP type TS_COMP .
  class-data C_CLASS type TS_COMPTYPE .
  class-data C_INTERFACE type TS_COMPTYPE .
  class-data C_PROGRAM type TS_COMPTYPE .
  class-data C_MSG_CLASS type TS_COMPTYPE .
  class-data C_TYPEGROUP type TS_COMPTYPE .
  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
  class-data PREDEFTYPES type TT_PREDEFTYPES .
  data CURRENT_ORIGIN type TD_ORIGIN .
  data REMAIN_REQUIREMENTS type TT_REQ_OBJECTS .

  methods ADD_SOURCE_TO_LIST
    importing
      !CODE type TD_ABAPSOURCE
      !ORIGIN type TD_ORIGIN
      !POSITION type TD_CONTEXT
      !FULL type ABAP_BOOL default ABAP_FALSE
    raising
      ZAPLINK_CX .
  methods DO_CLASS
    importing
      !NAME type TD_NAME
    raising
      ZAPLINK_CX .
  methods DO_OBJECT_LIST
    importing
      !NAME type TD_NAME
    raising
      ZAPLINK_CX .
  methods INSERT_ORDER
    importing
      !S_ORDER type TS_ORDER
      value(CONTEXT) type TT_CONTEXT .
ENDCLASS.



CLASS ZAPLINK_DEPENDENCIES_ANALYSER IMPLEMENTATION.


  method ADD_ATTRIBUTS.
  DATA _name TYPE string.
  FIELD-SYMBOLS <a> LIKE LINE OF attributs.

  LOOP AT attributs ASSIGNING <a>.
    CONCATENATE 'Attributs->' <a>-cmpname INTO _name.       "#EC NOTEXT
    stack( _name ).
    process_type( type = <a>-typtype
                origin = origin
                  name = <a>-type
                source = <a>-_ ).
    unstack( ).
  ENDLOOP.
  endmethod.


  method ADD_CLASS_DEP.
  DATA _codes      LIKE LINE OF classes.
  DATA o_raw       TYPE to_raw.
  DATA o_data      TYPE to_class.
  DATA _component  TYPE to_component.
  DATA _key        TYPE td_name.
  DATA d_softcomp  TYPE tdevc-dlvunit.
  DATA d_devclass  TYPE tdevc-devclass.
  DATA _order      LIKE LINE OF order.
  DATA _name       TYPE td_compname.
  DATA f_before    TYPE xfeld.
  DATA d_tabix     TYPE sy-tabix.
  DATA _class      TYPE vseoclass.
  DATA s_key       TYPE seoclskey.
  DATA d_classname TYPE seoclsname.
  DATA d_prog      TYPE td_origin.
  DATA o_cx_childs TYPE REF TO cl_oo_class_relations.
  FIELD-SYMBOLS:
    <cx> LIKE LINE OF o_cx_childs->subclasses,
    <c> LIKE LINE OF circular_context,
    <f> LIKE LINE OF o_data->friends.

  CHECK is_excluded_class( type = c_class-type  name = name ) = abap_false.
  READ TABLE order TRANSPORTING NO FIELDS
       WITH KEY name = name
*                type = _order-type   " for perf reason
             defered = abap_false.
  IF sy-subrc = 0.    EXIT.   ENDIF.    " already processed => nothing to do

  s_key-clsname = name.
  CALL FUNCTION 'SEO_CLIF_GET'
    EXPORTING
      cifkey             = s_key
*     VERSION            = SEOC_VERSION_INACTIVE
*     STATE              = '0'
    IMPORTING
*      clstype            = o_type
      class              = _class
*     INTERFACE          =
    EXCEPTIONS
      not_existing       = 1
      deleted            = 2
      model_only         = 0
      OTHERS             = 4.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX'
                                          funcname = 'SEO_CLIF_GET' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  _order-name = name.
  IF _class-category =  objcat-exception.    _order-type = objtype-exception_class.   ELSE.   _order-type = objtype-class.   ENDIF.

  READ TABLE context TRANSPORTING NO FIELDS
       WITH KEY context = name
                   type = _order-type.
  IF sy-subrc = 0.    " Circular reference => Use defered
    stack( context = name   type = _order-type    full = full ).
    d_tabix = sy-tabix + 1.   circular_context = context.   DELETE circular_context FROM d_tabix.
    current_comp = name.    handle_context( ).        " CLEAR:  circular_ref_on, circular_context.
    unstack( ).
    EXIT.
  ENDIF.

  READ TABLE classes TRANSPORTING NO FIELDS WITH TABLE KEY name = name.
  IF sy-subrc = 0.    EXIT.   ENDIF.    " already processed => nothing to do

  d_classname = name.
  d_prog = current_origin = cl_oo_classname_service=>get_classpool_name( d_classname ).
  stack( context = name   type = _order-type    full = full ).
  do_class( name ).

  CREATE OBJECT _component.
  _component->set_type( c_class-type ).   _name = name.   _component->set_name( _name ).
  o_raw = o_object->zaplink_connector~read_from_sap( _component ).    o_data ?= o_raw->raw.

  append_typegroups( o_data->typegroups ).
  append_message( o_data->a0_maindata-msg_id ).

  IF NOT o_data->a0_maindata-inheritance-refclsname IS INITIAL.
    _key = o_data->a0_maindata-inheritance-refclsname.
    add_object_to_list( name = _key
                        full = abap_true
                    position = '=>inheritance-refclsname' ).
  ENDIF.

  add_types( types = o_data->types    origin = d_prog ).

  add_interfaces( o_data->interfaces ).

  add_attributs( attributs = o_data->attributs    origin = d_prog ).

  add_source_to_list( code = o_data->local-types-_   origin = d_prog    position = _name ).
  add_source_to_list( code = o_data->local-implementations-_   origin = d_prog    position = _name ).
  add_source_to_list( code = o_data->local-macros-_  origin = d_prog    position = _name ).

  add_methods( methods = o_data->methods    origin = d_prog ).

  LOOP AT o_data->friends ASSIGNING <f>.
    _key = <f>-refclsname.    CALL METHOD add_object_to_list( name = _key   position = '=>friends' ).
  ENDLOOP.

  do_object_list( name = name ).

  unstack( ).
  insert_order( s_order = _order   context = context ).

* Issue 134 : Unstack & Insert first because subclasses are not required by class, it's the opposit
  IF _order-type = objtype-exception_class.
* Issue 134 : Some exception class are missing
* When exception class load of childrens also
    CREATE OBJECT o_cx_childs
      EXPORTING
        clsname      = name
        w_subclasses = seox_true
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        OTHERS       = 3.
    ASSERT sy-subrc = 0.
    LOOP AT o_cx_childs->subclasses ASSIGNING <cx>.
      add_class_dep( <cx>-clsname ).
    ENDLOOP.
  ENDIF.
  endmethod.


  method ADD_DEFERRED_CODE.
  DATA _obj      LIKE LINE OF deferred_declaration.
  DATA _line     LIKE LINE OF abapcode.

  READ TABLE deferred_declaration TRANSPORTING NO FIELDS
       WITH TABLE KEY name = name.
  IF sy-subrc = 0.    EXIT.   ENDIF.
* DEFINE M_124563245. CLASS &1 DEFINITION DEFERRED. END-OF-DEFINITION.
  CASE type.
    WHEN objtype-class OR objtype-exception_class.
      CONCATENATE 'CLASS' name 'DEFINITION DEFERRED.' INTO _line SEPARATED BY space.
      APPEND _line TO abapcode.
      _obj-name = name.
      INSERT _obj INTO TABLE deferred_declaration.
    WHEN objtype-interface.
      CONCATENATE 'INTERFACE' name 'DEFERRED.' INTO _line SEPARATED BY space.
      APPEND _line TO abapcode.
      _obj-name = name.
      INSERT _obj INTO TABLE deferred_declaration.
  ENDCASE.
  endmethod.


  method ADD_DEFERRED_OK.
  TYPES:
    BEGIN OF ts_dep,
      ori_name  TYPE td_name,
      type      TYPE td_classtype,    " 0 Class 1 interface
      full_def  TYPE flag,          " Full definition required
      count     TYPE i,
    END OF ts_dep.
  DATA t_tmp     TYPE STANDARD TABLE OF ts_dep WITH DEFAULT KEY.
  DATA s_dep     LIKE LINE OF t_tmp.
  DATA t_deps    TYPE SORTED TABLE OF ts_dep WITH NON-UNIQUE KEY ori_name.
  DATA s_order   LIKE LINE OF new_order.
  FIELD-SYMBOLS:
    <r> LIKE LINE OF remain_requirements,
    <c> LIKE LINE OF remain_classes.

  LOOP AT remain_requirements ASSIGNING <r>.
    CLEAR s_dep.    MOVE-CORRESPONDING <r> TO s_dep.    s_dep-count = 1.
    COLLECT s_dep INTO t_tmp.
  ENDLOOP.
  DELETE t_tmp WHERE type <> objtype-exception_class AND full_def <> abap_true. " Issue 91
  t_deps = t_tmp.
* intf/class with only defered dependences any dependances
  LOOP AT remain_classes ASSIGNING <c>.
*    LOOP AT t_deps INTO s_dep                       " Issue 91 : Optimization
*         WHERE ori_name = <c>-name
*           AND (   type = objtype-exception_class   " Exception are full required
*            OR full_def = abap_true ).
*      EXIT.
*    ENDLOOP.
    READ TABLE t_deps TRANSPORTING NO FIELDS WITH TABLE KEY ori_name = <c>-name.
    IF sy-subrc <> 0.
      CLEAR s_order.    s_order-name = <c>-name.    s_order-type = <c>-type.    APPEND s_order TO new_order.    DELETE remain_requirements WHERE name = <c>-name.    result = abap_true.   DELETE remain_classes.
    ENDIF.
  ENDLOOP.
  add_without_requirements( ).
  endmethod.


  method ADD_INCLUDE.
  APPEND name TO includes.
  add_prog_textpool( name ).
  endmethod.


  method ADD_INTERFACES.
  DATA _key        TYPE td_name.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF interfaces.

  LOOP AT interfaces ASSIGNING <i>.
    _key = <i>-refclsname.
    add_object_to_list( name = _key
                        full = abap_true
                    position = '=>interface' ).
  ENDLOOP.
  endmethod.


  method ADD_INTERFACE_DEP.
  DATA _codes      LIKE LINE OF classes.
  DATA o_raw       TYPE to_raw.
  DATA o_data      TYPE to_interface.
  DATA _component  TYPE zaplink_connector=>to_component.
  DATA d_softcomp  TYPE tdevc-dlvunit.
  DATA d_devclass  TYPE tdevc-devclass.
  DATA _order      LIKE LINE OF order.
  DATA _name       TYPE zaplink_connector=>td_compname.
  DATA d_tabix     TYPE sy-tabix.
  DATA d_classname TYPE seoclsname.
  DATA d_prog      TYPE td_origin.

  CHECK is_excluded_class( type = c_interface-type  name = name ) = abap_false.
  _order-name = name.   _order-type = objtype-interface.
  READ TABLE order TRANSPORTING NO FIELDS
       WITH KEY name = _order-name
                type = _order-type
             defered = abap_false.
  IF sy-subrc = 0.    EXIT.   ENDIF.    " already processed => nothing to do

  READ TABLE context TRANSPORTING NO FIELDS
       WITH KEY context = name
                   type = _order-type.
  IF sy-subrc = 0.      " Circular reference => Use defered
    stack( context = name   type = _order-type    full = full ).
    d_tabix = sy-tabix + 1.   circular_context = context.   DELETE circular_context FROM d_tabix.
    current_comp = name.    handle_context( ).        " CLEAR:  circular_ref_on, circular_context.
    unstack( ).
    EXIT.
  ENDIF.

  READ TABLE classes TRANSPORTING NO FIELDS WITH TABLE KEY name = name.
  IF sy-subrc = 0.    EXIT.   ENDIF.    " already processed => nothing to do

  d_classname = name.
  d_prog = current_origin = cl_oo_classname_service=>get_classpool_name( d_classname ).
  stack( context = name   type = _order-type    full = full ).
  do_class( name ).

  CREATE OBJECT _component.
  _component->set_type( c_interface-type ).   _name = name.   _component->set_name( _name ).
  o_raw = o_object->zaplink_connector~read_from_sap( _component ).
  o_data ?= o_raw->raw.

  append_typegroups( o_data->typegroups ).
  add_types( types = o_data->types    origin = d_prog ).
  add_interfaces( o_data->interfaces ).
  add_attributs( attributs = o_data->attributs    origin = d_prog ).
  add_methods( o_data->methods ).
  do_object_list( name = name ).

  unstack( ).

  insert_order( s_order = _order   context = context ).
  endmethod.


  method ADD_METHODS.
  DATA _name       TYPE string.
  DATA t_objs      TYPE tt_abaprawsource.
  DATA _key        TYPE td_name.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF t_objs,
    <m> LIKE LINE OF methods,
    <p> LIKE LINE OF <m>-parameters,
    <e> LIKE LINE OF <m>-exceptions.

  LOOP AT methods ASSIGNING <m>.
    CONCATENATE 'Method->' <m>-cmpname INTO _name.          "#EC NOTEXT
    stack( _name ).
* Check parameters
    LOOP AT <m>-parameters ASSIGNING <p>.
      process_type( type = <p>-typtype
                    name = <p>-type ).
    ENDLOOP.

    IF NOT <m>-mtdnewexc IS INITIAL.
* Check exceptions class
      LOOP AT <m>-exceptions ASSIGNING <e>.
        _key = <e>-sconame.   add_object_to_list( name = _key   position = _name ).
      ENDLOOP.
    ENDIF.

    IF not <m>-source-_ is INITIAL.   add_source_to_list( code = <m>-source-_   origin = origin    position = _name ).    endif.

    unstack( ).
  ENDLOOP.
  endmethod.


  method ADD_METHOD_SOURCE.
  DATA ref_objs     TYPE tt_abaprawsource.
  DATA _key         TYPE td_name.
  FIELD-SYMBOLS <o> LIKE LINE OF ref_objs.

  CHECK NOT code IS INITIAL.
  ref_objs = analyse_source( code = code   origin = origin ).
  LOOP AT ref_objs ASSIGNING <o>.
    _key = <o>.
    add_object( _key ).
  ENDLOOP.
  endmethod.


  method ADD_OBJECT.
  DATA _key   TYPE seoclskey.
  DATA o_type TYPE seoclstype.
  DATA o_desc TYPE REF TO cl_abap_typedescr.
  DATA _name  TYPE td_name.
  DATA _class TYPE vseoclass.

  _name = name. TRANSLATE _name TO UPPER CASE.            "#EC SYNTCHAR
  CHECK is_predeftype( _name ) = abap_false.
  CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
    EXPORTING
      clsname                   = _name
    EXCEPTIONS
      reserved                  = 1
      space_not_allowed         = 2
      small_letters_not_allowed = 3
      symbols_not_allowed       = 4
      first_character_no_number = 5
      namespace_error           = 6
      OTHERS                    = 7.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX'
                                          funcname = 'SEO_CLIF_CHECK_NAME' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  CALL METHOD cl_abap_typedescr=>describe_by_name
    EXPORTING
      p_name         = _name
    RECEIVING
      p_descr_ref    = o_desc
    EXCEPTIONS
      type_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'cl_abap_typedescr'
                                                     method = 'describe_by_name'
                                                      subrc = sy-subrc
                                               cx_classname = 'ZAPLINK_CX').
    RAISE EXCEPTION o_mycx.
  ENDIF.

  CHECK o_desc->kind = cl_abap_typedescr=>kind_class
     OR o_desc->kind = cl_abap_typedescr=>kind_intf.

  _key-clsname = _name.
  CALL FUNCTION 'SEO_CLIF_GET'
    EXPORTING
      cifkey             = _key
*     VERSION            = SEOC_VERSION_INACTIVE
*     STATE              = '0'
    IMPORTING
      clstype            = o_type
      class              = _class
*     INTERFACE          =
    EXCEPTIONS
      not_existing       = 1
      deleted            = 2
      model_only         = 0
      OTHERS             = 4.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX'
                                          funcname = 'SEO_CLIF_GET' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  ASSERT o_type = objtype-class OR o_type = objtype-interface.

  IF o_type = objtype-class.
    add_class_dep( _name ).
  ELSE.
    add_interface_dep( _name ).
  ENDIF.

  IF context IS INITIAL.
* Main call
    CALL METHOD _clean_up( ).
  ENDIF.
  endmethod.


  method ADD_OBJECT_TO_LIST.
  DATA s_obj       LIKE LINE OF required_objects.
  DATA s_key       TYPE seoclskey.
  DATA o_type      TYPE seoclstype.
  DATA s_class     TYPE vseoclass.

  CHECK NOT name IS INITIAL. CHECK is_predeftype( name ) = abap_false.
  s_key-clsname = name.     TRANSLATE s_key-clsname TO UPPER CASE.    "#EC SYNTCHAR
  s_obj-name = s_key-clsname.
  CHECK is_excluded_class( name = s_key-clsname ) = abap_false.
  CALL FUNCTION 'SEO_CLIF_GET'
    EXPORTING
      cifkey             = s_key
*     VERSION            = SEOC_VERSION_INACTIVE
*     STATE              = '0'
    IMPORTING
      clstype            = o_type
      class              = s_class
*     INTERFACE          =
    EXCEPTIONS
      not_existing       = 1
      deleted            = 2
      model_only         = 0
      OTHERS             = 4.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX'
                                          funcname = 'SEO_CLIF_GET' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  s_obj-type = o_type.    s_obj-full_def = full.    s_obj-source = position.
  IF s_class-category = '40'.    s_obj-type = objtype-exception_class.   ENDIF.
  APPEND s_obj TO required_objects.
  endmethod.


  method ADD_PROG_TEXTPOOL.
  DATA t_texts TYPE tt_txtp_textpools.
  t_texts = zaplink_program_4dep_analyser=>get_prog_textpool( name ).               " Issue 110
  textspool = zaplink_program_4dep_analyser=>fusion_textpool( source = textspool
                                                          completion = t_texts ).   " Issue 110
  endmethod.


  method ADD_SOURCE.
  DATA _code TYPE string.
  FIELD-SYMBOLS <f> LIKE LINE OF code.
  LOOP AT code ASSIGNING <f>.   CONCATENATE _code <f> line_separator INTO _code.    ENDLOOP.
  add_method_source( code = _code    origin = origin ).
  endmethod.


  method ADD_SOURCE_TO_LIST.
  DATA t_objs       TYPE tt_abaprawsource.
  DATA _key         TYPE td_name.
  FIELD-SYMBOLS <o> LIKE LINE OF t_objs.

  t_objs = analyse_source( code = code    origin = origin ).
  LOOP AT t_objs ASSIGNING <o>.
    _key = <o>.   add_object_to_list( name = _key   full = full   position = position ).
  ENDLOOP.
  endmethod.


  method ADD_TYPES.
  DATA _name TYPE string.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF types.

  LOOP AT types ASSIGNING <t>.
    CONCATENATE 'Type=>' <t>-cmpname INTO _name.            "#EC NOTEXT
    stack( _name ).
    process_type( type = <t>-typtype
                origin = origin
                  name = <t>-type
                source = <t>-_ ).
    unstack( ).
  ENDLOOP.
  endmethod.


  method ADD_WITHOUT_REQUIREMENTS.
  DATA s_order   LIKE LINE OF new_order.
  FIELD-SYMBOLS <c> LIKE LINE OF remain_classes.

* intf/class without any dependances
  LOOP AT remain_classes ASSIGNING <c>.
    READ TABLE remain_requirements TRANSPORTING NO FIELDS
         WITH KEY ori_name = <c>-name.
    IF sy-subrc <> 0.   CLEAR s_order.    s_order-name = <c>-name.    s_order-type = <c>-type.    APPEND s_order TO new_order.    DELETE remain_requirements WHERE name = <c>-name.    result = abap_true.   DELETE remain_classes.    ENDIF.
  ENDLOOP.
  endmethod.


  method ANALYSE_SOURCE.
  CONSTANTS c_symbols_ascii TYPE string VALUE '"$%&()=?\{[]}^+*~#-''.:,;<>|@`!'. "#EC NOTEXT

*  DATA static_objs  TYPE tt_abaprawsource.
  DATA t_tokens     TYPE STANDARD TABLE OF stokes.
  DATA t_statments  TYPE STANDARD TABLE OF sstmnt.
*  DATA _code        TYPE string.
  DATA t_string     TYPE STANDARD TABLE OF string.
  DATA d_offset     TYPE i.
  DATA s_obj        LIKE LINE OF objects_list.
  DATA d_tabix      TYPE sy-tabix.
  DATA d_prog       TYPE sy-repid.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF objects_list,
    <t> LIKE LINE OF t_tokens,
    <c> LIKE LINE OF t_tokens.

  CHECK NOT code IS INITIAL.
  SPLIT code AT cl_abap_char_utilities=>newline INTO TABLE t_string.
  SCAN ABAP-SOURCE t_string
           TOKENS INTO t_tokens
       STATEMENTS INTO t_statments.

  LOOP AT t_tokens ASSIGNING <t> WHERE str CP '*=>*'.
    FIND FIRST OCCURRENCE OF '=>' IN <t>-str MATCH OFFSET d_offset.
    s_obj = <t>-str(d_offset). TRANSLATE s_obj TO UPPER CASE. "#EC SYNTCHAR
    CHECK NOT s_obj CA c_symbols_ascii.   " FROM SEO_CLIF_CHECK_NAME
    APPEND s_obj TO objects_list.
  ENDLOOP.

  LOOP AT t_tokens ASSIGNING <t> WHERE str = 'TYPE'.
    d_tabix = sy-tabix + 1.   READ TABLE t_tokens ASSIGNING <c> INDEX d_tabix.
    CHECK sy-subrc = 0.       CHECK <c>-str = 'REF'.
    ADD 1 TO d_tabix.         READ TABLE t_tokens ASSIGNING <c> INDEX d_tabix.
    CHECK sy-subrc = 0.       CHECK <c>-str = 'TO'.
    ADD 1 TO d_tabix.         READ TABLE t_tokens ASSIGNING <c> INDEX d_tabix.
    s_obj = <c>-str. TRANSLATE s_obj TO UPPER CASE.       "#EC SYNTCHAR
    CHECK NOT s_obj CA c_symbols_ascii.   " FROM SEO_CLIF_CHECK_NAME
    APPEND s_obj TO objects_list.
  ENDLOOP.
*  _code = convert_line( code ).
*
*  objects_list = extract_strings(
*      forward     = 'X'
*      search_term = 'TYPE REF TO '
*      code        = _code
*      ).
*
*  static_objs = extract_strings(
**      forward     =
*      search_term = '=>'
*      code        = _code
*      ).
*
*  APPEND LINES OF static_objs TO objects_list.
  SORT objects_list.      DELETE ADJACENT DUPLICATES FROM objects_list.

  IF origin IS NOT INITIAL.
* Local class filtering
* From analyse of abap naviguation : MF RS_NAVIGATION_PREPARE
    d_prog = origin.
    LOOP AT objects_list ASSIGNING <o>.
* FORM search_object => use 'WB_TREE_GET_INCLUDE' to determine local object definitions
      CALL FUNCTION 'WB_TREE_GET_INCLUDE'
        EXPORTING
          objectname             = <o>
          objecttype             = sana_tok_control_def   " in form search_object
          program                = d_prog
*     IMPORTING
*       INCLUDE_NAME           =
*     TABLES
*       INCL_TAB               =
        EXCEPTIONS
          index_not_found        = 1
          object_not_found       = 2
          OTHERS                 = 3.
      IF sy-subrc <> 0.
        CALL FUNCTION 'WB_TREE_GET_INCLUDE'
          EXPORTING
            objectname             = <o>
            objecttype             = sana_tok_common_def   " in form search_object
            program                = d_prog
*     IMPORTING
*       INCLUDE_NAME           =
*     TABLES
*       INCL_TAB               =
          EXCEPTIONS
            index_not_found        = 1
            object_not_found       = 2
            OTHERS                 = 3.
      ENDIF.
      IF sy-subrc = 0.    DELETE objects_list.    ENDIF.
    ENDLOOP.
  ENDIF.
  endmethod.


  method APPEND_MESSAGE.
  DATA s_msg LIKE LINE OF messages.
  CHECK NOT data IS INITIAL.
  s_msg-name = data.    APPEND s_msg TO messages.
  endmethod.


  method APPEND_TYPEGROUPS.
  DATA _tg LIKE LINE OF me->typegroups.
  FIELD-SYMBOLS <t> LIKE LINE OF typegroups.
  LOOP AT typegroups ASSIGNING <t>.
    _tg-name = <t>-typegroup.     APPEND _tg TO me->typegroups.
  ENDLOOP.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _softcomp LIKE LINE OF softcomponents.
  DATA t_types   TYPE sana_type_list.
  DATA s_type    LIKE LINE OF t_types.

  _softcomp-sign = 'I'. _softcomp-option = 'EQ'.
  _softcomp-low = space. APPEND _softcomp TO softcomponents.
  _softcomp-low = 'HOME'. APPEND _softcomp TO softcomponents.
  _softcomp-low = 'LOCAL'. APPEND _softcomp TO softcomponents.

  CREATE OBJECT o_object.

  c_class-type = 'CLAS'.    c_class-kind = zaplink_connectors=>get_typekind( c_class-type ).
  c_interface-type = 'INTF'.    c_interface-kind = zaplink_connectors=>get_typekind( c_interface-type ).
  c_program-type = 'PROG'.    c_program-kind = zaplink_connectors=>get_typekind( c_program-type ).
  c_typegroup-type = 'TYPE'.    c_typegroup-kind = zaplink_connectors=>get_typekind( c_typegroup-type ).
  c_msg_class-type = 'MSAG'.    c_msg_class-kind = zaplink_connectors=>get_typekind( c_msg_class-type ).

  CALL FUNCTION 'RS_GET_PREDEFINED_TYPES'
    IMPORTING
      type_list      = t_types
    EXCEPTIONS
      internal_error = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  s_type-name = 'SY'.   APPEND s_type TO t_types.       " Add system name
  s_type-name = 'SYST'.   APPEND s_type TO t_types.

  SORT t_types BY name.   DELETE ADJACENT DUPLICATES FROM t_types.
  predeftypes = t_types.
  endmethod.


  method CONVERT_LINE.
  target = zaplink_tools=>conv_abap_line( source ).
  endmethod.


  method CONVERT_SOURCE_UPCASE.
  FIELD-SYMBOLS <l> LIKE LINE OF targetcode.
  targetcode = sourcecode.
  LOOP AT targetcode ASSIGNING <l>.
    TRANSLATE <l> TO UPPER CASE.  "#EC SYNTCHAR
  ENDLOOP.
  endmethod.


  method DO_CLASS.
  DATA _codes      LIKE LINE OF classes.
  DATA d_prog      TYPE td_progname.
  DATA d_class     TYPE seoclsname.
  DATA d_includes  TYPE zaplink_object_data=>ts_includes.
*  TRY.
  _codes = get_source( name ).
*    CATCH cx_root.
*      RAISE failed.
*  ENDTRY.

  search_for_include( CHANGING codes = _codes ).

  INSERT _codes INTO TABLE classes.
  d_class = name.
  d_includes = zaplink_object=>get_includes( d_class ).
  d_prog = d_includes-classpool-name.
  add_prog_textpool( d_prog ).
  endmethod.


  method DO_OBJECT_LIST.
  CONSTANTS c_intftype TYPE td_classtype VALUE '9'.
  DATA t_objects TYPE tt_req_objects.
  DATA s_object  LIKE LINE OF required_objects.
  FIELD-SYMBOLS <o> LIKE LINE OF t_objects.

  SORT required_objects BY name full_def DESCENDING.   DELETE ADJACENT DUPLICATES FROM required_objects COMPARING name.
  s_object-type = c_intftype.    MODIFY required_objects FROM s_object TRANSPORTING type WHERE type = objtype-interface.   SORT required_objects BY full_def DESCENDING type DESCENDING name.
  t_objects = required_objects.    CLEAR required_objects.   " memorize object list for recursive calls
  LOOP AT t_objects ASSIGNING <o>.
    stack( context = <o>-source ).    <o>-ori_name = name.    APPEND <o> TO requirements.
    IF <o>-type = c_intftype.   add_interface_dep( name = <o>-name  full = <o>-full_def ).    ELSE.   add_class_dep( name = <o>-name  full = <o>-full_def ).    ENDIF.
    unstack( ).
  ENDLOOP.
  endmethod.


  method EXTRACT_STRINGS.
  CONSTANTS:
    c_crlf TYPE string VALUE line_separator,
    c_commentedline TYPE string VALUE '*',
    c_commentscheme TYPE string VALUE '*"*',
    c_commentchar TYPE string VALUE '"'.

  DATA _objects    TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DATA _max        TYPE sy-tabix.
  DATA _pos        TYPE sy-tabix.
  DATA _idx        TYPE sy-tabix.
  DATA _lines      TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DATA _line       TYPE string.
  DATA _tmp        TYPE string.
  FIELD-SYMBOLS:
    <l> LIKE LINE OF _objects,
    <l2> LIKE LINE OF _objects.

  DEFINE mac_split.
    split _line at &1 into table _lines. " split line into words
    check not _lines is initial.
    read table _lines into _line index 1.
  END-OF-DEFINITION.

  CHECK search_term <> c_crlf.

  IF forward IS INITIAL.
    SPLIT code AT search_term INTO TABLE _objects.
    CHECK NOT _objects IS INITIAL.
    _pos = LINES( _objects ).
    DELETE _objects INDEX _pos.  " remove last line (unwanted) because backward search
* Table _ojects contains at the end of each line the searched string
    LOOP AT _objects ASSIGNING <l>.
      SPLIT <l> AT c_crlf INTO TABLE _lines. " split string into lines
* Table _lines contains all lines, searched string is in the last line at the end
      WHILE NOT _lines IS INITIAL.
        _pos = LINES( _lines ).
        READ TABLE _lines INTO _line INDEX _pos.
        _tmp = _line.
        CONDENSE _tmp NO-GAPS.
        IF NOT _tmp IS INITIAL.
          EXIT.
        ENDIF.
        DELETE _lines INDEX _pos.  " empty line
      ENDWHILE.
* _Line contains the code line with searched string at the end
      CHECK _line(1) <> c_commentedline.      " code line is a comment
      CHECK NOT _line CP c_commentscheme.     " Check line for line comment
      SPLIT _line AT space INTO TABLE _lines. " split line into words
      _pos = LINES( _lines ).
      READ TABLE _lines INTO _line INDEX _pos.
      APPEND _line TO results.
    ENDLOOP.
  ELSE.
*  X TYPE REF TO
*     YYYY.
*  X TYPE REF TO
*  * Old type
*     YYYY.
*  X
*  TYPE REF TO
*  * Old type
*     YYYY.
*  X TYPE REF TO Z
*  *X tYPE REF TO K
*  *X tYPE REF TO
*     " X TYPE REF TO K
*     " X TYPE REF TO K
    SPLIT code AT search_term INTO TABLE _objects.
    CHECK NOT _objects IS INITIAL.
* Table _ojects contains at the begining of each line the searched string
    _max = LINES( _objects ) - 1.
    LOOP AT _objects ASSIGNING <l> TO _max.
* <l> is the text before the recherched text to check for comment
      _idx = sy-tabix.
      SPLIT <l> AT c_crlf INTO TABLE _lines. " split string into lines
* Table _lines contains all lines, searched string is in the last line at the end
      _pos = LINES( _lines ).
      READ TABLE _lines INTO _line INDEX _pos.
      IF NOT _line IS INITIAL.
* process line where search_term was found. Check that line is valid
        CHECK _line(1) <> c_commentedline.        " code line is a comment => ignore
        CHECK NOT _line CP c_commentscheme.       " Check line for line comment
      ENDIF.
* Line is not a commented line
      ADD 1 TO _idx.
      READ TABLE _objects ASSIGNING <l2> INDEX _idx.
* NOW <l2> is the text just after the recherched text => so the text to be extracted
      SPLIT <l2> AT c_crlf INTO TABLE _lines. " split string into lines
* remove blank and commented lines
      DELETE _lines WHERE table_line IS INITIAL.
      LOOP AT _lines ASSIGNING <l2>.
        IF <l2>(1) = c_commentedline.
          DELETE _lines.
        ENDIF.
      ENDLOOP.
* Searche for the "good" line
      LOOP AT _lines INTO _line.
        CLEAR _pos.
        FIND FIRST OCCURRENCE OF c_commentchar IN _line MATCH OFFSET _pos.
        IF NOT _pos IS INITIAL.
          _line = _line(_pos).      " remove comment
        ENDIF.
        _tmp = _line.
        CONDENSE _tmp NO-GAPS.
        IF NOT _tmp IS INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
* _Line contains the code line with searched string at the end
      CONDENSE _line.
      CHECK NOT _line IS INITIAL.
      mac_split space. mac_split ','. mac_split '.'.
      APPEND _line TO results.
    ENDLOOP.

  ENDIF.

  SORT results.
  DELETE ADJACENT DUPLICATES FROM results.
  DELETE results WHERE table_line IS INITIAL.
  endmethod.


  method GET_CLASS_SOURCE.
*  DATA o_source    TYPE REF TO cl_oo_source.
  DATA _clskey     TYPE seoclskey.
  DATA _pos        TYPE sy-tabix.
  DATA t_code      TYPE tt_abaprawsource.
  DATA t_upcode    TYPE tt_abaprawsource.
*  DATA t_lines     TYPE tt_abaprawsource.
  DATA _line       LIKE LINE OF t_upcode.
  FIELD-SYMBOLS:
    <l> LIKE LINE OF t_upcode.

  _clskey = name.

  t_code = zaplink_object=>get_class_globalsource( _clskey ).
*  CREATE OBJECT o_source
*    EXPORTING
*      clskey             = _clskey
*    EXCEPTIONS
*      class_not_existing = 1
*      OTHERS             = 2.
*  IF sy-subrc <> 0.
  zaplink_tools=>remove_comment_on_source( CHANGING table = t_code ).
  ASSERT NOT t_code IS INITIAL.
*  t_code = convert_source( o_source->source ).
  t_upcode = convert_source_upcase( t_code ).
*  CLEAR o_source.

*class-pool MESSAGE-ID ZAPLINK_EXTRACTOR.
*CLASS lcl_data DEFINITION DEFERRED.
*CLASS zaplink_synchronizer DEFINITION LOCAL FRIENDS lcl_data.
*CLASS lcl_data DEFINITION
*
* Must became :
*CLASS lcl_data DEFINITION DEFERRED.
*CLASS zaplink_synchronizer DEFINITION LOCAL FRIENDS lcl_data.
*CLASS lcl_data DEFINITION
  READ TABLE t_upcode ASSIGNING <l> INDEX 1.
  IF <l> CP '*CLASS-POOL*'.   DELETE t_upcode INDEX 1.    DELETE t_code INDEX 1.    ENDIF.
* Transform CLASS Zxxx DEFINITION
  CONCATENATE 'CLASS' _clskey 'DEFINITION' INTO _line SEPARATED BY space.
  READ TABLE t_upcode TRANSPORTING NO FIELDS FROM _line.
  IF sy-subrc = 0.
    _pos = sy-tabix + 1.
*class ZAPLINK_MESSAGE_COLLECTOR definition
*  public   " to remove when local class
*  create public .
    READ TABLE t_upcode INDEX _pos INTO _line.
    CONDENSE _line NO-GAPS.
    IF _line = 'PUBLIC'.
      DELETE t_code INDEX _pos.       DELETE t_upcode INDEX _pos.
    ENDIF.
  ENDIF.
**Remove text before CLASS Zxxx DEFINITION
*  CONCATENATE 'CLASS' _clskey 'DEFINITION' INTO _line SEPARATED BY space.
*  READ TABLE t_upcode TRANSPORTING NO FIELDS
*       FROM _line.
*  IF sy-subrc = 0.
*    _pos = sy-tabix.
*    APPEND LINES OF t_code FROM _pos TO t_lines.
**class ZAPLINK_MESSAGE_COLLECTOR definition
**  public
**  create public .
*    SUBTRACT 1 FROM _pos.
*    IF _pos >= 1. DELETE t_upcode TO _pos. ENDIF.
*    READ TABLE t_upcode INDEX 2 INTO _line.
*    CONDENSE _line NO-GAPS.
*    IF _line = 'PUBLIC'.
*      DELETE t_lines INDEX 2.
*      DELETE t_upcode INDEX 2.
*    ENDIF.
*  ELSE.
** anormal case
*    t_lines = t_code.
*  ENDIF.

* Get implementation
* class XXXXX implementation.
  CONCATENATE 'CLASS' _clskey 'IMPLEMENTATION.' INTO _line SEPARATED BY space.
  READ TABLE t_upcode TRANSPORTING NO FIELDS FROM _line.
  IF sy-subrc = 0.
* Class
    _pos = sy-tabix.
    APPEND LINES OF t_code  FROM _pos TO codes-impl.
    DELETE t_code FROM _pos.    DELETE t_upcode FROM _pos.
* endclass. "ZAPLINK_MESSAGE_COLLECTOR definition
    CONCATENATE 'ENDCLASS. "' _clskey ' DEFINITION' INTO _line.
    READ TABLE t_upcode TRANSPORTING NO FIELDS FROM _line.
    ASSERT sy-subrc = 0.
    _pos = sy-tabix.
    APPEND LINES OF t_code TO _pos TO codes-def.
    DELETE t_code TO _pos.    DELETE t_upcode TO _pos.
    codes-macro = t_code.
  ELSE.
* normal case for interfaces
    codes-def = t_code.
  ENDIF.

*  zaplink_tools=>condense_abap_source( CHANGING codes-macro ).
*  zaplink_tools=>condense_abap_source( CHANGING codes-impl ).
*  zaplink_tools=>condense_abap_source( CHANGING codes-def ).

* CONVERT GLOBAL FRIENDS => FRIENDS
  t_upcode = convert_source_upcase( codes-def ).
  LOOP AT t_upcode ASSIGNING <l>
     WHERE table_line CP '*GLOBAL FRIENDS*'.
    REPLACE FIRST OCCURRENCE OF 'GLOBAL FRIENDS' IN <l> WITH 'FRIENDS'.
    MODIFY codes-def FROM <l> INDEX sy-tabix.
  ENDLOOP.

  remove_def_load( CHANGING codes = codes ).

  codes-name = name.
  endmethod.


  method GET_INTERFACE_SOURCE.
  DATA _clskey     TYPE seoclskey.
*  DATA _key    TYPE seoclsname.
*  DATA _prog   TYPE program.
  DATA _line   LIKE LINE OF codes-def.
  DATA _pos        TYPE sy-tabix.
  DATA t_upcode    TYPE tt_abaprawsource.

  _clskey = name.
  codes-def = zaplink_object=>get_interface_globalsource( _clskey ).
*  _key = name.
*  _prog = cl_oo_classname_service=>get_intfsec_name( _key ).
*
*  READ REPORT _prog INTO codes-def.
*  codes-def = convert_source( codes-def ).
  t_upcode = convert_source_upcase( codes-def ).

*Remove text public after : interface xxxx
  CONCATENATE 'INTERFACE' _clskey INTO _line SEPARATED BY space.
  READ TABLE t_upcode TRANSPORTING NO FIELDS
       FROM _line.
  ASSERT sy-subrc = 0.
  _pos = sy-tabix.
  CONCATENATE _line '.' INTO _line.
  MODIFY codes-def FROM _line INDEX _pos.
  ADD 1 TO _pos.
  READ TABLE t_upcode INTO _line INDEX _pos.
  CONDENSE _line NO-GAPS.
  IF _line = 'PUBLIC.'.
    DELETE codes-def INDEX _pos.
  ENDIF.

  remove_def_load( CHANGING codes = codes ).

  codes-name = name.
  endmethod.


  method GET_SOURCE.
  DATA _key   TYPE seoclskey.
  DATA o_type TYPE seoclstype.
  DATA _class      TYPE vseoclass.

  _key-clsname = name.
  CALL FUNCTION 'SEO_CLIF_GET'
    EXPORTING
      cifkey             = _key
*     VERSION            = SEOC_VERSION_INACTIVE
*     STATE              = '0'
    IMPORTING
      clstype            = o_type
      class              = _class
*     INTERFACE          =
    EXCEPTIONS
      not_existing       = 1
      deleted            = 2
      model_only         = 0
      OTHERS             = 4.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX'
                                          funcname = 'SEO_CLIF_GET' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.

  ASSERT o_type = objtype-class OR o_type = objtype-interface.

  IF o_type = objtype-class.
    IF _class-category = '40'.    o_type = objtype-exception_class.   ENDIF.
    codes = get_class_source( name ).
  ELSE.
    codes = get_interface_source( name ).
  ENDIF.
  codes-type = o_type.
  endmethod.


  method HANDLE_CONTEXT.
  TYPES:
    BEGIN OF ts_object,
      name  TYPE td_name,
      type  TYPE seoclstype,
      rank  TYPE i,
      tabix TYPE sy-tabix,
      full  TYPE abap_bool,
    END OF ts_object.
  DATA t_objects TYPE STANDARD TABLE OF ts_object WITH DEFAULT KEY.
  DATA s_object  LIKE LINE OF t_objects.
  DATA s_key     TYPE seoclskey.
  DATA d_type    TYPE seoclstype.
  DATA s_class   TYPE vseoclass.
  DATA s_order   LIKE LINE OF order.
  DATA d_tabix   TYPE sy-tabix.
  DATA s_ctx     LIKE LINE OF circular_context.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF t_objects,
    <c> LIKE LINE OF circular_context.

  DELETE circular_context WHERE type IS INITIAL.
  LOOP AT circular_context ASSIGNING <c>.
*    CLEAR s_object.   s_object-tabix = sy-tabix.
*    s_key-clsname = <c>-context.    CHECK s_key-clsname = <c>-context.    " field might be too long
*    CALL FUNCTION 'SEO_CLIF_GET'
*      EXPORTING
*        cifkey             = s_key
**     VERSION            = SEOC_VERSION_INACTIVE
**     STATE              = '0'
*      IMPORTING
*        clstype            = d_type
*        class              = s_class
**     INTERFACE          =
*      EXCEPTIONS
*        not_existing       = 1
*        deleted            = 2
*        model_only         = 0
*        OTHERS             = 4.
*    IF sy-subrc <> 0.   CONTINUE.   ENDIF.
*    s_object-name = s_key-clsname.    s_object-type = d_type.
*    IF d_type = objtype-interface.    s_object-rank = 1.    ELSEIF s_class-category = '40'.   s_object-rank = 2.  s_object-type = objtype-exception_class.    ELSE.   s_object-rank = 3.    ENDIF.
    CLEAR s_object.   s_object-tabix = sy-tabix.    s_object-name = <c>-context.    s_object-type = <c>-type.   s_object-full = <c>-full_def.
    IF s_object-type = objtype-interface.    s_object-rank = 1.    ELSEIF s_object-type = objtype-exception_class.   s_object-rank = 2.  s_object-type = objtype-exception_class.    ELSE.   s_object-rank = 3.    ENDIF.
    IF s_object-full IS INITIAL.
      ADD 5 TO s_object-rank.
    ENDIF.
    APPEND s_object TO t_objects.
  ENDLOOP.

  SORT t_objects BY name tabix.   DELETE ADJACENT DUPLICATES FROM t_objects COMPARING name.
  IF LINES( t_objects ) = 1.    DELETE t_objects WHERE name = current_comp.   ENDIF.
  CHECK NOT t_objects IS INITIAL.   SORT t_objects BY rank tabix.   d_tabix = LINES( order ) + 1.   REFRESH circular_context.
  LOOP AT t_objects ASSIGNING <o>.    CLEAR s_ctx.    s_ctx-context = <o>-name.   s_ctx-type = <o>-type.    s_ctx-full_def = <o>-full.    APPEND s_ctx TO circular_context.   ENDLOOP.
  LOOP AT me->context ASSIGNING <c> WHERE NOT type IS INITIAL.
    READ TABLE t_objects TRANSPORTING NO FIELDS WITH KEY name = <c>-context.
    IF sy-subrc <> 0.
      APPEND <c> TO circular_context.
    ENDIF.
  ENDLOOP.
  LOOP AT t_objects ASSIGNING <o>.
*    CLEAR s_object.   s_object-tabix = sy-tabix.
*    READ TABLE order TRANSPORTING NO FIELDS
*         WITH KEY name = <o>-name
*                  type = <o>-type
*               defered = abap_false.
*    IF sy-subrc = 0.    d_tabix = s_object-tabix.   ENDIF.    " already processed : circular ref
    do_class( <o>-name ) .
    CLEAR s_order.  s_order-name = <o>-name.    s_order-type = <o>-type.
    insert_order( s_order = s_order   context = circular_context ).   DELETE circular_context WHERE context = <o>-name.   " <=> should delete index 1.
*    insert s_order inTO order index d_tabix.
  ENDLOOP.
  CLEAR: circular_context, circular_ref_on.
  endmethod.


  method INSERT_ORDER.
  DATA d_tabix TYPE sy-tabix.
  FIELD-SYMBOLS <c> LIKE LINE OF context.

  DELETE context WHERE type IS INITIAL.   DELETE context WHERE context = s_order-name.
  READ TABLE order TRANSPORTING NO FIELDS
       WITH KEY name = s_order-name
                type = s_order-type
             defered = abap_false.
  IF sy-subrc = 0.    EXIT.   ENDIF.    " already processed : circular ref
  IF context IS INITIAL.    APPEND s_order TO me->order.    EXIT.   ENDIF.
  inverse_order( CHANGING order = context ).
  LOOP AT context ASSIGNING <c> WHERE full_def = abap_true.
    READ TABLE order TRANSPORTING NO FIELDS
         WITH KEY name = <c>-context
                  type = <c>-type
               defered = abap_false.
    IF sy-subrc = 0 AND d_tabix > sy-tabix.
      d_tabix = sy-tabix.
    ENDIF.
  ENDLOOP.
  IF d_tabix IS INITIAL.
    d_tabix = LINES( order ) + 1.
    LOOP AT context ASSIGNING <c> WHERE full_def = abap_false.
      READ TABLE order TRANSPORTING NO FIELDS
           WITH KEY name = <c>-context
                    type = <c>-type
                 defered = abap_false.
      IF sy-subrc = 0 AND d_tabix > sy-tabix.
        d_tabix = sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.
  INSERT s_order INTO me->order INDEX d_tabix.
  READ TABLE order TRANSPORTING NO FIELDS   " for security
       WITH KEY name = s_order-name
                type = s_order-type
             defered = abap_false.
  IF sy-subrc <> 0.
    APPEND s_order TO me->order.
  ENDIF.
  endmethod.


  method INVERSE_ORDER.
  zaplink_tools=>inverse_table_order( CHANGING my_table = order ).
  endmethod.


  method IS_EXCLUDED_CLASS.
  DATA s_excl_c    LIKE LINE OF excluded_classes.

  CLEAR s_comp.   result = abap_true.   READ TABLE excluded_classes TRANSPORTING NO FIELDS WITH TABLE KEY name = name.    CHECK sy-subrc <> 0.
  s_comp-type = type.   s_comp-name = name.
  IF NOT s_comp-type IS INITIAL.
    s_comp-kind = zaplink_connectors=>get_typekind( type ).
    mac_read_tadir s_comp-kind s_comp-type s_comp-name s_comp-devclass s_comp-softcomp.
  ELSE.
    MOVE-CORRESPONDING c_class TO s_comp.
    mac_read_tadir s_comp-kind s_comp-type s_comp-name s_comp-devclass s_comp-softcomp.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING c_interface TO s_comp.
      mac_read_tadir s_comp-kind s_comp-type s_comp-name s_comp-devclass s_comp-softcomp.
    ENDIF.
  ENDIF.
* customer components
  IF s_comp-devclass IN packages AND s_comp-softcomp IN softcomponents.    result = abap_false.    ELSE.   s_excl_c-name = name.   INSERT s_excl_c INTO TABLE excluded_classes.    ENDIF.
  endmethod.


  method IS_EXCLUDED_INCLUDE.
  DATA s_excl_c     LIKE LINE OF excluded_includes.
  DATA d_fugr_group TYPE rs38l-area.
  DATA d_class_name TYPE seoclsname.
  DATA d_prog       TYPE rs38l-include.

  CLEAR s_comp.
  result = abap_true.   READ TABLE excluded_includes TRANSPORTING NO FIELDS WITH TABLE KEY name = name.    CHECK sy-subrc <> 0.
  d_prog = name.
  CALL FUNCTION 'RS_PROGNAME_SPLIT'
    EXPORTING
      progname_with_namespace           = d_prog
    IMPORTING
*     NAMESPACE                         =
*     PROGNAME_WITHOUT_NAMESPACE        =
*     FUGR_IS_NAME                      =
*     FUGR_IS_RESERVED_NAME             =
*     FUGR_IS_FUNCTIONPOOL_NAME         =
*     FUGR_IS_INCLUDE_NAME              =
*     FUGR_IS_FUNCTIONMODULE_NAME       =
*     FUGR_IS_HIDDEN_NAME               =
      fugr_group                        = d_fugr_group
*     FUGR_INCLUDE_NUMBER               =
*     FUGR_SUFFIX                       =
*     FUGR_IS_RESERVED_EXIT_NAME        =
*     SLDB_IS_RESERVED_NAME             =
*     SLDB_LOGDB_NAME                   =
*     MST_IS_RESERVED_NAME              =
*     TYPE_IS_RESERVED_NAME             =
*     TYPE_NAME                         =
*     MENU_IS_RESERVED_NAME             =
*     MENU_NAME                         =
*     CLASS_IS_RESERVED_NAME            =
*     CLASS_IS_NAME                     =
      class_name                        = d_class_name
*     CLASS_IS_METHOD_NAME              =
*     CLASS_METHOD_NAME                 =
*     CNTX_IS_RESERVED_NAME             =
    EXCEPTIONS
      delimiter_error                   = 1
      OTHERS                            = 2.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX'
                                          funcname = 'RS_PROGNAME_SPLIT' ).
    RAISE EXCEPTION o_mycx.
  ENDIF.
  IF NOT d_fugr_group IS INITIAL.
    s_comp-type = 'FUGR'.
    s_comp-name = d_fugr_group.
  ELSEIF NOT d_class_name IS INITIAL.
    result = is_excluded_class( name = d_class_name ).
    RETURN.
  ELSE.
    s_comp-type = 'PROG'.
    s_comp-name = name.
  ENDIF.
  s_comp-kind = zaplink_connectors=>get_typekind( s_comp-type ).
  mac_read_tadir s_comp-kind s_comp-type s_comp-name s_comp-devclass s_comp-softcomp.
* customer components
  IF s_comp-devclass IN packages AND s_comp-softcomp IN softcomponents.    result = abap_false.    ELSE.   s_excl_c-name = name.   INSERT s_excl_c INTO TABLE excluded_includes.    ENDIF.
  endmethod.


  method IS_PREDEFTYPE.
  CHECK NOT name IS INITIAL.
  READ TABLE predeftypes TRANSPORTING NO FIELDS WITH TABLE KEY name = name.
  IF sy-subrc = 0.    result = abap_true.     ENDIF.
  endmethod.


  method PROCESS_TYPE.
  DATA _key        TYPE td_name.
  DATA _obj        TYPE string.
  DATA _type       TYPE string.
  DATA _up_src     TYPE string.
  DATA ctx         LIKE LINE OF context.
  DATA full        TYPE abap_bool.

  ASSERT type <> typetype-code OR NOT origin IS INITIAL.
  _up_src = source.   TRANSLATE _up_src TO UPPER CASE.

  READ TABLE context INTO ctx INDEX 1.
  CASE type.
    WHEN typetype-like OR typetype-type.
      SPLIT name AT '=>' INTO _obj _type.
      CHECK _type <> ''.
      _key = _obj.  full = abap_true.
    WHEN typetype-otype.
      _key = name.
    WHEN typetype-ref.
      _key = name.
    WHEN typetype-code.
      add_source_to_list( code = _up_src   full = abap_true   origin = origin   position = ctx-context ).
      CLEAR _key.   EXIT.
  ENDCASE.
  CHECK NOT _key IS INITIAL.
  add_object_to_list( name = _key   full = full   position = ctx-context ).
  endmethod.


  method REMOVE_DEF_LOAD.
  DATA t_upcode    TYPE tt_abaprawsource.
  FIELD-SYMBOLS:
    <l> LIKE LINE OF t_upcode,
    <m> LIKE LINE OF codes-def.

  t_upcode = convert_source_upcase( codes-def ).
* Comment CLASS xxx DEFINITION LOAD.
  LOOP AT t_upcode ASSIGNING <l>
     WHERE table_line CP '*CLASS * DEFINITION LOAD*'.
    CHECK <l>(1) <> '*'.
    CHECK NOT <l> CP '*"*CLASS * DEFINITION LOAD*'.
    READ TABLE codes-def ASSIGNING <m> index sy-tabix.
    CONCATENATE '*' <m> INTO <m>.
  ENDLOOP.

* Comment INTERFACE xxx LOAD .
  LOOP AT t_upcode ASSIGNING <l>
     WHERE table_line CP '*INTERFACE * LOAD*'.
    CHECK <l>(1) <> '*'.
    CHECK NOT <l> CP '*"*INTERFACE * LOAD*'.
    READ TABLE codes-def ASSIGNING <m> index sy-tabix.
    CONCATENATE '*' <m> INTO <m>.
  ENDLOOP.

  t_upcode = convert_source_upcase( codes-macro ).
* Comment CLASS xxx DEFINITION LOAD.
  LOOP AT t_upcode ASSIGNING <l>
     WHERE table_line CP '*CLASS * DEFINITION LOAD*'.
    CHECK <l>(1) <> '*'.
    CHECK NOT <l> CP '*"*CLASS * DEFINITION LOAD*'.
    READ TABLE codes-macro ASSIGNING <m> index sy-tabix.
    CONCATENATE '*' <m> INTO <m>.
  ENDLOOP.

* Comment INTERFACE xxx LOAD .
  LOOP AT t_upcode ASSIGNING <l>
     WHERE table_line CP '*INTERFACE * LOAD*'.
    CHECK <l>(1) <> '*'.
    CHECK NOT <l> CP '*"*INTERFACE * LOAD*'.
    READ TABLE codes-macro ASSIGNING <m> index sy-tabix.
    CONCATENATE '*' <m> INTO <m>.
  ENDLOOP.
  endmethod.


  method RESOLVE.
  DATA s_order   LIKE LINE OF new_order.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF new_order,
    <c> LIKE LINE OF classes.

  _clean_up( ).
* Reset Part :
  REFRESH: deferred_declaration.  " staring new program no declaration have been made
  REFRESH: new_order.             " rebuild order list
  remain_classes = classes.
  remain_requirements = requirements.
* intf/class without any dependances
  WHILE add_without_requirements( ) = abap_true.  ENDWHILE.
  WHILE add_deferred_ok( ) = abap_true.  ENDWHILE.

  IF remain_requirements IS INITIAL.
    LOOP AT remain_classes ASSIGNING <c>.   CLEAR s_order.    s_order-name = <c>-name.    s_order-type = <c>-type.    APPEND s_order TO new_order.    DELETE remain_requirements WHERE name = <c>-name.    DELETE remain_classes.    ENDLOOP.
    order = new_order.
  ELSE.
    LOOP AT new_order ASSIGNING <o>.    WRITE:/ <o>-name.   ENDLOOP.
  ENDIF.
  endmethod.


  method SEARCH_FOR_INCLUDE.
  DATA t_includes TYPE tt_abaprawsource.
  DATA d_softcomp TYPE tdevc-dlvunit.
  DATA d_devclass TYPE tdevc-devclass.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF t_includes,
    <l> LIKE LINE OF codes-macro.
  DATA t_tokens TYPE STANDARD TABLE OF stokes.
  DATA t_statements TYPE STANDARD TABLE OF sstmnt.
  DATA t_kw      TYPE STANDARD TABLE OF char255.
  DATA s_kw      LIKE LINE OF t_kw.
  DATA d_tabix   TYPE sy-tabix.
  DATA d_deb     TYPE int4.
  DATA d_fin     TYPE int4.
  DATA d_l_deb   TYPE int4.
  DATA d_l_fin   TYPE int4.
  DATA d_name    TYPE td_name.
  data d_prog    type TD_PROGNAME.
*  DATA d_str TYPE string.
  FIELD-SYMBOLS:
    <st> LIKE LINE OF t_statements,
    <if> LIKE LINE OF t_tokens,
    <found> LIKE LINE OF t_tokens,
    <inc> LIKE LINE OF t_tokens,
    <t_deb> LIKE LINE OF t_tokens,
    <t> LIKE LINE OF t_tokens.

  s_kw = 'PERFORM'.    APPEND s_kw TO t_kw.
  s_kw = 'INCLUDE'.       APPEND s_kw TO t_kw.
*  s_kw = 'PROGRAM'.    APPEND s_kw TO t_kw.
  SCAN ABAP-SOURCE codes-impl TOKENS INTO t_tokens
                          STATEMENTS INTO t_statements
                            KEYWORDS FROM t_kw.
  zaplink_tools=>inverse_table_order( CHANGING my_table = t_statements ).
  LOOP AT t_statements ASSIGNING <st>.
    READ TABLE t_tokens ASSIGNING <t_deb> INDEX <st>-from.    CHECK sy-subrc = 0.
    CASE <t_deb>-str.
      WHEN 'INCLUDE'.

        d_tabix = <st>-from + 1.
        READ TABLE t_tokens ASSIGNING <inc> INDEX d_tabix.    CHECK sy-subrc = 0.
        d_name = <inc>-str.
        CHECK is_excluded_include( d_name ) = abap_false.
*          APPEND d_name TO includes.
        LOOP AT codes-macro FROM <t_deb>-row TO <st>-trow ASSIGNING <l>.
          CASE sy-tabix.
            WHEN <t_deb>-row.
              d_deb = <t_deb>-col.
              IF <t_deb>-row = <st>-trow.
                d_fin = <st>-tcol - <t_deb>-col + 1.
              ELSE.
                d_fin = STRLEN( <l> ) - <t_deb>-col + 1.
              ENDIF.
            WHEN <st>-trow.
              d_deb = 0.
              d_fin = <st>-tcol.
            WHEN OTHERS.
              CLEAR <l>.
          ENDCASE.
          IF NOT <l> IS INITIAL.    REPLACE SECTION OFFSET <t_deb>-col LENGTH <st>-tcol OF <l> WITH space.    ENDIF.
        ENDLOOP.
*          zaplink_connector_=>get_prog_source( d_name ).

      WHEN 'PERFORM'.

        LOOP AT t_tokens FROM <st>-from TO <st>-to TRANSPORTING NO FIELDS
             WHERE str CP 'PROGRAM'.
          d_tabix = sy-tabix - 1.   EXIT.
        ENDLOOP.
        CHECK sy-subrc = 0.
        READ TABLE t_tokens ASSIGNING <t_deb> INDEX d_tabix.
        CHECK sy-subrc = 0.   CHECK <t_deb>-str CP 'IN'.   " IN PROGRAM
        d_tabix = d_tabix + 2.    READ TABLE t_tokens ASSIGNING <inc> INDEX d_tabix.
        CHECK sy-subrc = 0 AND NOT <inc>-str IS INITIAL.
        CHECK <inc>-str(1) <> '('.    " Dynamic include name
        d_name = <inc>-str.
        CHECK is_excluded_include( d_name ) = abap_false.
        LOOP AT t_tokens FROM d_tabix TO <st>-to TRANSPORTING NO FIELDS
             WHERE str CP 'FOUND'.
          d_tabix = sy-tabix - 1.   EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
* search for IF found addition
          READ TABLE t_tokens ASSIGNING <if> INDEX d_tabix.
          d_tabix = d_tabix + 1.  READ TABLE t_tokens ASSIGNING <found> INDEX d_tabix.
          IF <if>-str CP 'IF' AND <found>-str CP 'FOUND'.
*            ASSIGN <found> TO <inc>.   " Do not work on preform x in prog y using Z if found.
            d_l_deb = <if>-row.
            d_l_fin = <found>-row.
            LOOP AT codes-impl FROM d_l_deb TO d_l_fin ASSIGNING <l>.
              CASE sy-tabix.
                WHEN d_l_deb.
                  d_deb = <if>-col.
                  IF d_l_deb = d_l_fin.
                    d_fin = <found>-col + STRLEN( <found>-str ) - d_deb.
                  ELSE.
                    d_fin = STRLEN( <l> ) - d_deb.
                  ENDIF.
                WHEN d_l_fin.
                  d_deb = 0.
                  d_fin = <found>-col + STRLEN( <found>-str ).
                WHEN OTHERS.
                  CLEAR <l>.
              ENDCASE.
              IF NOT <l> IS INITIAL.    REPLACE SECTION OFFSET d_deb LENGTH d_fin OF <l> WITH space.    ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        d_l_deb = <t_deb>-row.
        d_l_fin = <inc>-row.

        d_prog = d_name.      add_include( d_prog ).
        LOOP AT codes-impl FROM d_l_deb TO d_l_fin ASSIGNING <l>.
          CASE sy-tabix.
            WHEN d_l_deb.
              d_deb = <t_deb>-col.
              IF d_l_deb = d_l_fin.
                d_fin = <inc>-col + STRLEN( <inc>-str ) - d_deb.
              ELSE.
                d_fin = STRLEN( <l> ) - d_deb.
              ENDIF.
            WHEN d_l_fin.
              d_deb = 0.
              d_fin = <inc>-col + STRLEN( <inc>-str ).
            WHEN OTHERS.
              CLEAR <l>.
          ENDCASE.
          IF NOT <l> IS INITIAL.    REPLACE SECTION OFFSET d_deb LENGTH d_fin OF <l> WITH space.    ENDIF.
        ENDLOOP.

    ENDCASE.
  ENDLOOP.

  SCAN ABAP-SOURCE codes-macro TOKENS INTO t_tokens
                           STATEMENTS INTO t_statements
                             KEYWORDS FROM t_kw.
  zaplink_tools=>inverse_table_order( CHANGING my_table = t_statements ).
  LOOP AT t_statements ASSIGNING <st>.
    READ TABLE t_tokens ASSIGNING <t_deb> INDEX <st>-from.    CHECK sy-subrc = 0.
    CASE <t_deb>-str.
      WHEN 'INCLUDE'.

        d_tabix = <st>-from + 1.
        READ TABLE t_tokens ASSIGNING <inc> INDEX d_tabix.    CHECK sy-subrc = 0.
        d_name = <inc>-str.
        CHECK is_excluded_include( d_name ) = abap_false.
        d_prog = d_name.      add_include( d_prog ).
        LOOP AT codes-macro FROM <t_deb>-row TO <st>-trow ASSIGNING <l>.
          CASE sy-tabix.
            WHEN <t_deb>-row.
              d_deb = <t_deb>-col.
              IF <t_deb>-row = <st>-trow.
                d_fin = <st>-tcol - <t_deb>-col + 1.
              ELSE.
                d_fin = STRLEN( <l> ) - <t_deb>-col + 1.
              ENDIF.
            WHEN <st>-trow.
              d_deb = 0.
              d_fin = <st>-tcol.
            WHEN OTHERS.
              CLEAR <l>.
          ENDCASE.
          IF NOT <l> IS INITIAL.    REPLACE SECTION OFFSET <t_deb>-col LENGTH <st>-tcol OF <l> WITH space.    ENDIF.
        ENDLOOP.

      WHEN 'PERFORM'.

*          LOOP AT codes-macro FROM <t_deb>-row TO <st>-trow ASSIGNING <l>
*               WHERE str = 'PROGRAM'.
*            EXIT.
*          ENDLOOP.
*          d_tabix = sy-tabix + 1.
*          READ TABLE t_tokens ASSIGNING <inc> INDEX d_tabix.    CHECK sy-subrc = 0.
*          d_name = <inc>-str.
*          CHECK is_excluded_include( d_name ) = abap_false.
*
*          APPEND d_name TO includes.
*
*          LOOP AT codes-macro FROM <t_deb>-row TO <st>-trow ASSIGNING <l>.
*            CASE sy-tabix.
*              WHEN <t_deb>-row.
*                d_deb = <t_deb>-col.
*                IF <t_deb>-row = <st>-trow.
*                  d_fin = <st>-tcol - <t_deb>-col + 1.
*                ELSE.
*                  d_fin = STRLEN( <l> ) - <t_deb>-col + 1.
*                ENDIF.
*              WHEN <st>-trow.
*                d_deb = 0.
*                d_fin = <st>-tcol.
*              WHEN OTHERS.
*                CLEAR <l>.
*            ENDCASE.
*            IF NOT <l> IS INITIAL.    REPLACE SECTION OFFSET <t_deb>-col LENGTH <st>-tcol OF <l> WITH space.    ENDIF.
*          ENDLOOP.

    ENDCASE.
  ENDLOOP.

*  LOOP AT codes-macro ASSIGNING <l>.
*    t_includes = extract_strings(
*        forward     = abap_true
*        search_term = 'INCLUDE '
*        code        = <l>
*        ).
*
*    IF NOT t_includes IS INITIAL.
*      LOOP AT t_includes ASSIGNING <i>.
*        SELECT SINGLE devclass dlvunit INTO (d_devclass, d_softcomp)
*          FROM v_tralan
*          WHERE pgmid = c_program-kind
*            AND object = c_program-type
*            AND obj_name = <i>.
*
*        CHECK d_devclass IN packages.
*        CHECK d_softcomp IN softcomponents. " customer components
*
*        APPEND <i> TO includes.
*      ENDLOOP.
*      DELETE codes-macro.
*    ENDIF.
*  ENDLOOP.
  endmethod.


  method SET_TEXTPOOL.
  zaplink_program_4dep_analyser=>set_prog_textpool( program = name
                                                   textpool = textspool ). " Issue 110
  endmethod.


  method SOURCE_2_STRING.
  FIELD-SYMBOLS <l> LIKE LINE OF data.
  LOOP AT data ASSIGNING <l>.   IF result IS INITIAL.   result = <l>.   ELSE.     CONCATENATE result line_separator <l> INTO result.    ENDIF.    ENDLOOP.
  endmethod.


  method STACK.
  DATA s_ctx LIKE LINE OF context.
  s_ctx-context = context.   s_ctx-type = type.    s_ctx-full_def = full.   INSERT s_ctx INTO me->context INDEX 1.
  endmethod.


  method UNSTACK.
  IF NOT context IS INITIAL. DELETE context INDEX 1. ENDIF.
  IF context IS INITIAL. CLEAR: circular_ref_on, circular_context. ENDIF.
  endmethod.


  method _CLEAN_UP.
  FIELD-SYMBOLS:
    <r> LIKE LINE OF requirements,
    <c> LIKE LINE OF classes,
    <i> LIKE LINE OF includes,
    <t> LIKE LINE OF typegroups,
    <m> LIKE LINE OF messages.

  SORT requirements BY ori_name name full_def.    DELETE ADJACENT DUPLICATES FROM requirements COMPARING ori_name name full_def.
  LOOP AT requirements ASSIGNING <r>.   CHECK <r>-ori_name = <r>-name.    DELETE requirements.    ENDLOOP.
  inverse_order( CHANGING order = order ).     DELETE ADJACENT DUPLICATES FROM order COMPARING type name.   inverse_order( CHANGING order = order ).
  SORT typegroups.    DELETE ADJACENT DUPLICATES FROM typegroups.
  SORT includes.      DELETE ADJACENT DUPLICATES FROM includes.
  SORT messages.      DELETE ADJACENT DUPLICATES FROM messages.

* Update all components list
  REFRESH all_components.
  LOOP AT classes ASSIGNING <c>.    CLEAR s_comp.   IF <c>-type = 0. MOVE-CORRESPONDING c_class TO s_comp. ELSE.   MOVE-CORRESPONDING c_interface TO s_comp.   ENDIF.   s_comp-name = <c>-name.   mac_add_comp.    ENDLOOP.
  LOOP AT typegroups ASSIGNING <t>.    CLEAR s_comp.   MOVE-CORRESPONDING c_typegroup TO s_comp.   s_comp-name = <t>-name.   mac_add_comp.    ENDLOOP.
  LOOP AT includes ASSIGNING <i>.    CLEAR s_comp.   MOVE-CORRESPONDING c_program TO s_comp.   s_comp-name = <i>.   mac_add_comp.    ENDLOOP.
  LOOP AT messages ASSIGNING <m>.    CLEAR s_comp.   MOVE-CORRESPONDING c_msg_class TO s_comp.   s_comp-name = <m>-name.   mac_add_comp.    ENDLOOP.
  endmethod.
ENDCLASS.
