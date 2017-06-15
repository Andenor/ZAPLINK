class ZAPLINK_COMPONENT definition
  public
  create public

  global friends ZAPLINK_LIST .

public section.
  type-pools ABAP .

  interfaces ZAPLINK_DATATYPES .

  aliases DEFAULT_SUB_COMPONENT_LEVEL
    for ZAPLINK_DATATYPES~DEFAULT_SUB_COMPONENT_LEVEL .
  aliases EXISTS
    for ZAPLINK_DATATYPES~EXISTS .
  aliases TD_ACTION
    for ZAPLINK_DATATYPES~TD_ACTION .
  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
  aliases TD_COMPADD_DATA
    for ZAPLINK_DATATYPES~TD_COMPADD_DATA .
  aliases TD_COMPEXISTS
    for ZAPLINK_DATATYPES~TD_COMPEXISTS .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_DATATYPES~TD_CONNCLASS .
  aliases TD_CONNEXISTS
    for ZAPLINK_DATATYPES~TD_CONNEXISTS .
  aliases TD_CONNUUID
    for ZAPLINK_DATATYPES~TD_CONNUUID .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .
  aliases TD_SRCSYSTEM
    for ZAPLINK_DATATYPES~TD_SRCSYSTEM .
  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
  aliases TD_WITH_SUBCOMP
    for ZAPLINK_DATATYPES~TD_WITH_SUBCOMP .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TS_COMPONENT
    for ZAPLINK_DATATYPES~TS_COMPONENT .

  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_CONN_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .
  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_EXCEPTION type ref to ZAPLINK_CX_COMPONENT .
  types TO_MSG_COLL type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types TO_ME type ref to ZAPLINK_COMPONENT .

  constants:
    BEGIN OF connexists,
          exists     TYPE td_connexists VALUE ZAPLINK_DATATYPES~EXISTS-exists,
          not_exists TYPE td_connexists VALUE ZAPLINK_DATATYPES~EXISTS-not_exists,
        END OF connexists .

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .
  methods GET_TYPE
    returning
      value(RESULT) type TD_COMPTYPE .
  methods SET_TYPE
    importing
      !DATA type TD_COMPTYPE
    raising
      ZAPLINK_CX .
  methods GET_WITH_SUBCOMP
    returning
      value(RESULT) type TD_WITH_SUBCOMP .
  methods SET_WITH_SUBCOMP
    importing
      !DATA type TD_WITH_SUBCOMP .
  methods GET_DEVCLASS
    returning
      value(RESULT) type TD_DEVCLASS .
  methods SET_DEVCLASS
    importing
      !DATA type TD_DEVCLASS .
  methods GET_NAME
    returning
      value(RESULT) type TD_COMPNAME .
  methods SET_NAME
    importing
      !DATA type ANY
    raising
      ZAPLINK_CX .
  methods DO_EXISTS
    returning
      value(RESULT) type TD_COMPEXISTS .
  methods CONNECTOR_EXISTS
    returning
      value(RESULT) type TD_CONNEXISTS .
  methods CONNECTOR
    returning
      value(RESULT) type TO_CONNECTOR .
  methods GET_CONNUUID
    returning
      value(RESULT) type TD_CONNUUID .
  methods SET_CONNUUID
    importing
      !DATA type TD_CONNUUID
    raising
      ZAPLINK_CX .
  methods GET_KEY
    returning
      value(RESULT) type TS_COMPONENT
    raising
      ZAPLINK_CX .
  methods GET_EXCEPTION
    returning
      value(RESULT) type TO_ROOT_EXCEPTION .
  methods SET_EXCEPTION
    importing
      !DATA type TO_ROOT_EXCEPTION
    raising
      ZAPLINK_CX .
  methods GET_MSG_COLL
    returning
      value(RESULT) type TO_MSG_COLL .
  methods SET_MSG_COLL
    importing
      !DATA type TO_MSG_COLL
    raising
      ZAPLINK_CX .
  methods GET_SUBCOMPONENTS
    returning
      value(RESULT) type TO_LIST .
  methods SET_SUBCOMPONENTS
    importing
      !DATA type TO_LIST
    raising
      ZAPLINK_CX .
  methods GET_CHECKSUM
    returning
      value(RESULT) type TD_CHECKSUM .
  methods SET_CHECKSUM
    importing
      !DATA type TD_CHECKSUM .
  methods GET_CODE_SIGNATURE
    returning
      value(RESULT) type TD_CHECKSUM .
  methods SET_CODE_SIGNATURE
    importing
      !DATA type TD_CHECKSUM .
  methods COPY_FROM
    importing
      !COMPONENT type TO_ME .
  methods CLONE
    returning
      value(RESULT) type TO_ME .
  methods REFRESH .
  methods GET_ACTION
    returning
      value(RESULT) type TD_ACTION .
  methods SET_ACTION
    importing
      !DATA type TD_ACTION .
  methods GET_ADD_DATA
    returning
      value(RESULT) type TD_COMPADD_DATA .
  methods SET_ADD_DATA
    importing
      !DATA type TD_COMPADD_DATA .
  methods GET_ADD_OBJECT
    returning
      value(RESULT) type ref to OBJECT .
  methods SET_ADD_OBJECT
    importing
      !DATA type ref to OBJECT .
  methods GET_SRC_SYS
    returning
      value(RESULT) type TD_SRCSYSTEM .
  methods SET_SRC_SYS
    importing
      !DATA type TD_SRCSYSTEM .
  methods GET_TYPEKIND
    returning
      value(RESULT) type TD_TRANSPORT_KIND .
protected section.

  aliases TS_CONN_CLASS
    for ZAPLINK_DATATYPES~TS_CONN_CLASS .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  types:
    BEGIN OF ts_conndata,
          uuid      type td_connuuid,
          exists    TYPE td_connexists,
*          classname TYPE td_connclass,
          o_conn    TYPE to_connector,
        END OF ts_conndata .
  types:
    BEGIN OF ts_class.
INCLUDE TYPE ts_conn_class.
TYPES:
    types TYPE tt_types,   " supported_types
  END OF ts_class .
  types:
    tt_classes TYPE SORTED TABLE OF ts_class WITH UNIQUE KEY classname .
  types:
    tt_saptypes TYPE HASHED TABLE OF ko100 WITH UNIQUE KEY object .

  constants LOCAL_DEVCLASS type TD_DEVCLASS value '$LOCAL$' ##NO_TEXT.
  class-data _CLASSES type TT_CLASSES .
  data _DEVCLASS type TD_DEVCLASS .
  data _EXCEPTION type TO_CONN_EXCEPTION .
  data _EXISTS type TD_COMPEXISTS .
  data _KIND type TD_TRANSPORT_KIND .
  data _MSG_COLL type TO_MSG_COLL .
  data _OBJECT type ref to OBJECT .
  data _SRCSYSTEM type TD_SRCSYSTEM .
  data _SUBCOMPONENTS type TO_LIST .
  data _DATA type TD_COMPADD_DATA .
  data _CONNECTOR type TS_CONNDATA .
  data _CODE_SIGNATURE type TD_CHECKSUM .
  data _CHECKSUM type TD_CHECKSUM .
  data _ACTION type TD_ACTION .
  data WITH_SUBCOMP type TD_WITH_SUBCOMP value ABAP_TRUE ##NO_TEXT.
  data TYPE type TD_COMPTYPE .
  data NAME type TD_COMPNAME .

  methods _GET_KEY
    returning
      value(RESULT) type TS_COMPKEY .
  class-methods CREATE_NEW
    importing
      !KEY type TS_COMPKEY
    returning
      value(RESULT) type TO_ME
    raising
      ZAPLINK_CX .
  methods _SET_KEY
    importing
      value(DATA) type TS_COMPKEY
    raising
      ZAPLINK_CX .
private section.

  class-data O_CX type TO_ROOT_EXCEPTION .
  class-data O_MYCX type TO_EXCEPTION .
  class-data MY_ATTRIBS type ABAP_ATTRDESCR_TAB .

  methods ON_CHANGE_CLEAR .
  methods ON_CHANGE_UPDATE
    raising
      ZAPLINK_CX .
ENDCLASS.



CLASS ZAPLINK_COMPONENT IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  endmethod.


  method CLONE.
  CREATE OBJECT result.
  result->copy_from( me ).
  endmethod.


  method CONNECTOR.
  result = _connector-o_conn.
  endmethod.


  method CONNECTOR_EXISTS.
  result = _connector-exists.
  endmethod.


  method CONSTRUCTOR.
  DATA o_obj TYPE REF TO cl_abap_objectdescr.
  IF my_attribs IS INITIAL.         " can't be in class constructor because me not yet exists
    o_obj ?= cl_abap_typedescr=>describe_by_object_ref( me ).
    my_attribs = o_obj->attributes.
    DELETE my_attribs WHERE is_class = abap_true    " remove class attributes
                      OR is_constant = abap_true.   " remove constants.
  ENDIF.
  set_with_subcomp( default_sub_component_level ).
  endmethod.


  method COPY_FROM.
  FIELD-SYMBOLS:
    <s> TYPE ANY,
    <t> TYPE ANY,
    <a> LIKE LINE OF my_attribs.

  me->type = component->type.
  me->name = component->name.
  LOOP AT my_attribs ASSIGNING <a>.
    ASSIGN me->(<a>-name) TO <t>.   CHECK sy-subrc = 0.
    ASSIGN COMPONENT->(<a>-name) TO <s>.   CHECK sy-subrc = 0.
    <t> = <s>.
  ENDLOOP.
  endmethod.


  method CREATE_NEW.
  DATA _cx TYPE REF TO zaplink_cx_component.
*  TRY.
      CREATE OBJECT result.
      result->_set_key( key ).
*    CATCH zaplink_cx_component INTO _cx.
*      RAISE EXCEPTION _cx.
*  ENDTRY.
  endmethod.


  method DO_EXISTS.
  result = _exists.
  endmethod.


  method GET_ACTION.
  result = _action.
  endmethod.


  method GET_ADD_DATA.
  result = _data.
  endmethod.


  method GET_ADD_OBJECT.
  result = _object.
  endmethod.


  method GET_CHECKSUM.
  result = _checksum.
  endmethod.


  method GET_CODE_SIGNATURE.
  result = _code_signature.
  endmethod.


  method GET_CONNUUID.
  result = _connector-uuid.
  endmethod.


  method GET_DEVCLASS.
  result = _devclass.
  endmethod.


  method GET_EXCEPTION.
  result = _exception.
  endmethod.


  method GET_KEY.
  IF type IS INITIAL OR name IS INITIAL.
    RAISE EXCEPTION TYPE zaplink_cx_component
               EXPORTING textid = zaplink_cx_component=>system_error.
  ENDIF.
  result-type = type.
  result-name = name.
  endmethod.


  method GET_MSG_COLL.
  result = _msg_coll.
  endmethod.


  method GET_NAME.
  result = name.
  endmethod.


  method GET_SRC_SYS.
  result = _srcsystem.
  endmethod.


  method GET_SUBCOMPONENTS.
  result = _subcomponents.
  endmethod.


  method GET_TYPE.
  result = type.
  endmethod.


  method GET_TYPEKIND.
  result = _kind.
  endmethod.


  method GET_WITH_SUBCOMP.
  result = with_subcomp.
  endmethod.


  method ON_CHANGE_CLEAR.
  CLEAR: _exists, _connector-uuid, _code_signature, _checksum.
  endmethod.


  method ON_CHANGE_UPDATE.
  DATA o_cx_cnx TYPE REF TO zaplink_cx_connector.
  IF _connector-o_conn IS BOUND AND NOT name IS INITIAL.
*    TRY.
        _exists = _connector-o_conn->do_exists( me ).
*      CATCH zaplink_cx_connector INTO o_cx_cnx.
*        mac_cascade_raise o_mycx o_cx_cnx.
*    ENDTRY.
    _connector-uuid = _connector-o_conn->uuid.

    CHECK NOT type IS INITIAL.
    _kind = zaplink_connectors=>get_typekind( type ).

    SELECT SINGLE devclass srcsystem INTO (_devclass, _srcsystem)
      FROM tadir
      WHERE pgmid = _kind
       AND object = type
       AND obj_name = name.
  ENDIF.
  endmethod.


  method REFRESH.
  TRY.
      on_change_update( ).                                  " Issue 31
    CATCH ZAPLINK_CX.   "_COMPONENT INTO o_mycx.
*      RAISE EXCEPTION o_mycx.    " Exception is ignored
  ENDTRY.
  endmethod.


  method SET_ACTION.
  _action = data.
  endmethod.


  method SET_ADD_DATA.
  _data = data.
  endmethod.


  method SET_ADD_OBJECT.
  _object = data.
  endmethod.


  method SET_CHECKSUM.
  _checksum = data.
  endmethod.


  method SET_CODE_SIGNATURE.
  _code_signature = data.
  endmethod.


  method SET_CONNUUID.
  DATA _cx    TYPE REF TO zaplink_cx.
  DATA o_conn TYPE to_connector.

  CHECK data <> _connector-uuid.

*  TRY.
      o_conn = zaplink_connectors=>create_connector( type = type
                                                 connuuid = data ).

      _connector-o_conn = o_conn.
      on_change_clear( ).
      _connector-uuid = data.
      IF o_conn IS BOUND.
        _connector-exists = connexists-exists.
      ELSE.
        _connector-exists = connexists-not_exists.
      ENDIF.
      on_change_update( ).

*    CATCH zaplink_cx INTO o_cx.
*      mac_cascade_raise o_mycx o_cx.
*  ENDTRY.
  endmethod.


  method SET_DEVCLASS.
  CHECK data <> _devclass.
  _devclass = data.
  endmethod.


  method SET_EXCEPTION.
  DATA cx TYPE REF TO cx_root.
  DATA _cx TYPE REF TO zaplink_cx_component.
  DATA _desc TYPE REF TO cl_abap_typedescr.

  IF data IS BOUND.
    TRY.
        _exception ?= data.
      CATCH cx_root INTO cx.
        _desc = cl_abap_typedescr=>describe_by_object_ref( data ).
* Can't set Exception to the provided object type : '&OBJTYPE&'
        CREATE OBJECT _cx
          EXPORTING
            textid   = zaplink_cx_component=>invalid_exception
            objtype  = _desc->absolute_name
            previous = cx.
        _cx->update( ).
        RAISE EXCEPTION _cx.
    ENDTRY.
  ELSE.
    CLEAR _exception.
  ENDIF.
  endmethod.


  method SET_MSG_COLL.
  DATA cx TYPE REF TO cx_root.
  DATA _cx TYPE REF TO zaplink_cx_component.
  DATA _desc TYPE REF TO cl_abap_typedescr.

  IF data IS BOUND.
    TRY.
        _msg_coll ?= data.
      CATCH cx_root INTO cx.
        _desc = cl_abap_typedescr=>describe_by_object_ref( data ).
* Can't set Message Collector to the provided object type : '&OBJTYPE&'
        CREATE OBJECT _cx
          EXPORTING
            textid   = zaplink_cx_component=>invalid_msg_coll
            objtype  = _desc->absolute_name
            previous = cx.
        _cx->update( ).
        RAISE EXCEPTION _cx.
    ENDTRY.
  ELSE.
    CLEAR _msg_coll.
  ENDIF.
  endmethod.


  method SET_NAME.
  CHECK data <> name.
  name = data.
  on_change_clear( ).
  on_change_update( ).
  endmethod.


  method SET_SRC_SYS.
  _srcsystem = data.
  endmethod.


  method SET_SUBCOMPONENTS.
  DATA cx TYPE REF TO cx_root.
  DATA _cx TYPE REF TO zaplink_cx_component.
  DATA _desc TYPE REF TO cl_abap_typedescr.

  IF data IS BOUND.
    TRY.
        _subcomponents ?= data.
      CATCH cx_root INTO cx.
        _desc = cl_abap_typedescr=>describe_by_object_ref( data ).
* Can't set Message Collector to the provided object type : '&OBJTYPE&'
        CREATE OBJECT _cx
          EXPORTING
            textid   = zaplink_cx_component=>invalid_subcomponents
            objtype  = _desc->absolute_name
            previous = cx.
        _cx->update( ).
        RAISE EXCEPTION _cx.
    ENDTRY.
  ELSE.
    CLEAR _msg_coll.
  ENDIF.
  endmethod.


  method SET_TYPE.
  DATA o_conn   TYPE to_connector.
  DATA o_cx_cnx TYPE REF TO zaplink_cx_connector.

  CHECK data <> type.

*  TRY.
  TRY.
      o_conn = zaplink_connectors=>create_connector( type = data ).
    CATCH zaplink_cx_connector INTO o_cx_cnx.
      IF o_cx_cnx->is_exception_text( zaplink_cx_connector=>invalid_type ) IS INITIAL.    RAISE EXCEPTION o_cx_cnx.   ENDIF.
  ENDTRY.

  type = data.    CLEAR _connector.
  on_change_clear( ).
  _connector-o_conn = o_conn.
  IF o_conn IS BOUND.   _connector-exists = connexists-exists.    ELSE.     _connector-exists = connexists-not_exists.    ENDIF.
  on_change_update( ).

*    CATCH zaplink_cx INTO o_cx.
*      mac_cascade_raise o_mycx o_cx.
*  ENDTRY.
  endmethod.


  method SET_WITH_SUBCOMP.
  CHECK data <> with_subcomp.
  with_subcomp = data.
  endmethod.


  method _GET_KEY.
  result-type = type.
  result-name = name.
  endmethod.


  method _SET_KEY.
  DATA _cx TYPE REF TO zaplink_cx_component.
*  TRY.
      set_type( data-type ).
      set_name( data-name ).
*    CATCH zaplink_cx_component INTO _cx.
*      RAISE EXCEPTION _cx.
*  ENDTRY.
  endmethod.
ENDCLASS.
