class ZAPLINK_CONNECTORS definition
  public
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_COMPEXISTS
    for ZAPLINK_DATATYPES~TD_COMPEXISTS .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTEXT
    for ZAPLINK_DATATYPES~TD_COMPTEXT .
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
  aliases TD_FILENAME
    for ZAPLINK_DATATYPES~TD_FILENAME .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .
  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
  aliases TD_TYPEORDER
    for ZAPLINK_DATATYPES~TD_TYPEORDER .
  aliases TD_TYPE_AS_TEXT
    for ZAPLINK_DATATYPES~TD_TYPE_AS_TEXT .
  aliases TO_XML
    for ZAPLINK_DATATYPES~TO_XML .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  types TO_RAW_BASE type ref to ZAPLINK_RAW_BASE .
  types TO_MSG_COLL type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types:
    BEGIN OF ts_component,
          type TYPE td_comptype,
          name TYPE td_compname,
        END OF ts_component .

  constants:
    BEGIN OF exists,
          exists     TYPE td_connexists VALUE ZAPLINK_DATATYPES~EXISTS-exists,
          not_exists TYPE td_connexists VALUE ZAPLINK_DATATYPES~EXISTS-not_exists,
        END OF exists .
  class-data SUPPORTED_TYPES type TT_TYPES read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods ADD_LOCAL_CONNECTOR
    importing
      !LOCAL_CLASS type TD_CONNCLASS
    raising
      ZAPLINK_CX .
  class-methods DO_EXISTS
    importing
      !TYPE type TD_COMPTYPE
    returning
      value(RESULT) type TD_CONNEXISTS
    raising
      ZAPLINK_CX .
  class-methods CREATE_CONNECTOR
    importing
      !TYPE type TD_COMPTYPE optional
      !CONNUUID type TD_CONNUUID optional
    returning
      value(RESULT) type TO_CONNECTOR
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods GET_VALUES
    changing
      !TYPE type TD_COMPTYPE
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods NODE_2_RAW
    importing
      !NODE type ref to IF_IXML_NODE
    returning
      value(RESULT) type TO_RAW_BASE
    raising
      ZAPLINK_CX .
  class-methods GET_TYPEORDER
    importing
      !DATA type TD_COMPTYPE
    returning
      value(RESULT) type TD_TYPEORDER .
  class-methods TYPE2TEXT
    importing
      !DATA type TD_COMPTYPE
    returning
      value(RESULT) type TD_TYPE_AS_TEXT .
  class-methods GET_CLASSNAME
    importing
      !O_CONN type TO_CONNECTOR
    returning
      value(RESULT) type TD_CONNCLASS .
  class-methods CLEAR_CONNECTORS .
  class-methods RESET_CONNECTORS
    raising
      ZAPLINK_CX .
  class-methods GET_TYPEKIND
    importing
      !DATA type TD_COMPTYPE
    returning
      value(RESULT) type TD_TRANSPORT_KIND .
  class-methods RAW_2_STRING
    importing
      !DATA type TO_RAW_BASE
    returning
      value(RESULT) type STRING
    raising
      ZAPLINK_CX .
  class-methods XML_2_STRING
    importing
      !XML type TO_XML
    returning
      value(RESULT) type STRING .
  class-methods STRING_2_XML
    importing
      !DATA type STRING
    returning
      value(RESULT) type TO_XML .
  class-methods RAW_2_XML
    importing
      !DATA type TO_RAW_BASE
    returning
      value(RESULT) type TO_XML
    raising
      ZAPLINK_CX .
protected section.

  aliases COMP_NODENAME
    for ZAPLINK_DATATYPES~COMP_NODENAME .
  aliases TS_COMPTYPE
    for ZAPLINK_DATATYPES~TS_COMPTYPE .
  aliases TS_CONN_CLASS
    for ZAPLINK_DATATYPES~TS_CONN_CLASS .
  aliases TS_CONN_DEF
    for ZAPLINK_DATATYPES~TS_CONN_DEF .
  aliases TT_TYPELIST
    for ZAPLINK_DATATYPES~TT_TYPELIST .

  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .
  types:
    BEGIN OF ts_conndata,
          exists TYPE td_connexists,
          classname TYPE td_connclass,
          o_conn TYPE to_connector,
        END OF ts_conndata .
  types:
    BEGIN OF ts_class.
  INCLUDE TYPE ts_conn_class.
  TYPES:
      types TYPE tt_types,   " supported_types
    END OF ts_class .
  types:
    tt_classes TYPE SORTED TABLE OF ts_class WITH UNIQUE KEY uuid .
  types:
    BEGIN OF ts_saptypes.
  INCLUDE TYPE ko100 AS hdr.
  TYPES:
      order TYPE td_typeorder,     " order of prefered load
    END OF ts_saptypes .
  types:
    tt_saptypes TYPE HASHED TABLE OF ts_saptypes WITH UNIQUE KEY object .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .

  class-data TYPE_LIST type TT_TYPELIST .
  class-data SAPTYPES type TT_SAPTYPES .
  class-data _CLASSES type TT_CLASSES .
  constants LOCAL_DEVCLASS type TD_DEVCLASS value '$LOCAL$' ##NO_TEXT.

  class-methods _SEARCH_CONNECTOR
    importing
      !TYPE type TD_COMPTYPE
    returning
      value(RESULT) type TS_CONN_DEF
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods GET_CONNECTORLIST
    returning
      value(RESULT) type TO_LIST
    raising
      ZAPLINK_CX .
  class-methods _ADD_CONNECTOR
    importing
      !CLASS type TD_CONNCLASS
      !DEVCLASS type TD_DEVCLASS optional
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods _UPDATE_DEF_CLASS .
private section.

  types:
    BEGIN OF ts_value,
           object TYPE td_comptype,
           text   TYPE td_comptext,
         END OF ts_value .
  types:
    tt_values TYPE STANDARD TABLE OF ts_value WITH DEFAULT KEY .

  class-data O_MYCX type TO_EXCEPTION .
  class-data VALUE_TABLE type TT_VALUES .
  class-data XMLENGINE type ref to IF_IXML .
  constants MAX_ORDER type TD_TYPEORDER value '999999' ##NO_TEXT.
  class-data O_CX type TO_ROOT_EXCEPTION .
  class-data C_CLASS type TS_COMPTYPE .
ENDCLASS.



CLASS ZAPLINK_CONNECTORS IMPLEMENTATION.


  method ADD_LOCAL_CONNECTOR.

*  TRY.

      _add_connector( class = local_class
                   devclass = local_devclass ).
      _update_def_class( ).

*    CATCH zaplink_cx_connector INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA t_types    TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY.
  DATA s_saptypes LIKE LINE OF saptypes.
  DATA t_saptypes TYPE STANDARD TABLE OF ts_saptypes.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF t_types.

* Create XML Engine
  xmlengine = cl_ixml=>create( ).

  CALL FUNCTION 'TR_OBJECT_TABLE'
*   IMPORTING
*     WE_TABLE_LINES       =
    TABLES
      wt_object_text       = t_types.

  SORT t_types BY object.
  DELETE ADJACENT DUPLICATES FROM t_types COMPARING object.

  LOOP AT t_types ASSIGNING <t>.
    CLEAR s_saptypes. s_saptypes-hdr = <t>.
    s_saptypes-order = max_order.
    APPEND s_saptypes TO t_saptypes.
  ENDLOOP.

  saptypes = t_saptypes.

  c_class-type = 'CLAS'.    c_class-kind = zaplink_connectors=>get_typekind( c_class-type ).

  s_saptypes-order = 50.  MODIFY saptypes FROM s_saptypes TRANSPORTING order WHERE object = 'DEVC'.
  s_saptypes-order = 100. MODIFY saptypes FROM s_saptypes TRANSPORTING order WHERE object = 'INTF'.
  s_saptypes-order = 150. MODIFY saptypes FROM s_saptypes TRANSPORTING order WHERE object = c_class-type.

  TRY.
      reset_connectors( ).
    CATCH zaplink_cx INTO o_cx.
      IF o_cx->IS_EXCEPTION_TEXT( zaplink_cx_connector=>twice_uuid ) = abap_true OR
         o_cx->IS_EXCEPTION_TEXT( zaplink_cx_connector=>twice_class ) = abap_true.
        RAISE EXCEPTION o_cx.   "  Major failure that will cause Short Dump
      ENDIF.
  ENDTRY.
  endmethod.


  method CLEAR_CONNECTORS.
  clear type_list.
  endmethod.


  method CREATE_CONNECTOR.
  DATA _cx        TYPE REF TO cx_root.
  DATA _conn      LIKE LINE OF type_list.
  DATA _classname TYPE td_connclass.
  FIELD-SYMBOLS:
    <cl> LIKE LINE OF _classes,
    <c> LIKE LINE OF _conn-classes.

* Check same class already loaded
  IF NOT connuuid IS INITIAL.
    READ TABLE _classes ASSIGNING <cl>
               WITH TABLE KEY uuid = connuuid.
    IF sy-subrc <> 0.
* Class '&CONNUUID&' for type '&TYPE&' is not a valid connector. Check attribute SUPPORTED_TYPES of Class '&CONNCLASS&'
      RAISE EXCEPTION TYPE zaplink_cx_connector
                 EXPORTING textid = zaplink_cx_connector=>invalid_uuid
                             type = type
                         connuuid = connuuid.
    ELSE.
      IF NOT type IS INITIAL.
        READ TABLE <cl>-types TRANSPORTING NO FIELDS
             WITH TABLE KEY type = type.
        IF sy-subrc <> 0.
* Class '&CONNUUID&' for type '&TYPE&' is not a valid connector. Check attribute SUPPORTED_TYPES of Class '&CONNCLASS&'
          RAISE EXCEPTION TYPE zaplink_cx_connector
                     EXPORTING textid = zaplink_cx_connector=>type_not_supported
                                 type = type
                            connclass = <cl>-classname
                             connuuid = connuuid.
        ENDIF.
      ENDIF.
    ENDIF.
    _classname = <cl>-classname.
  ELSEIF NOT type IS INITIAL.
    _conn = _search_connector( type ).
    IF NOT _conn-classes IS INITIAL AND _conn-default_class IS INITIAL.
      READ TABLE _conn-classes ASSIGNING <c> INDEX 1.
      IF sy-subrc = 0.
        _conn-default_class = <c>-classname.
      ENDIF.
    ENDIF.
    _classname = _conn-default_class.
  ELSE.
    EXIT.
** Class '&CONNUUID&' for type '&TYPE&' is not a valid connector. Check attribute SUPPORTED_TYPES of Class '&CONNCLASS&'
*    RAISE EXCEPTION TYPE zaplink_cx_connector
*               EXPORTING textid = zaplink_cx_connector=>invalid_uuid
*                           type = type
*                       connuuid = connuuid.
  ENDIF.

  IF NOT _classname IS INITIAL.
    TRY.
        CREATE OBJECT result TYPE (_classname).
      CATCH cx_root INTO _cx.
        RAISE EXCEPTION TYPE zaplink_cx_connector
                   EXPORTING textid = zaplink_cx=>system_error
                           previous = _cx.
    ENDTRY.
  ENDIF.
  endmethod.


  method DO_EXISTS.
  DATA _conn LIKE LINE OF type_list.

*  TRY.
      result = exists-not_exists.
      _conn = _search_connector( type ).

      IF NOT _conn-classes IS INITIAL.
        result = exists-exists.
      ENDIF.

*    CATCH zaplink_cx INTO o_cx.
*      mac_cascade_raise o_mycx o_cx.
*  ENDTRY.
  endmethod.


  method GET_CLASSNAME.
  DATA o_typedesc TYPE REF TO cl_abap_objectdescr.

  CHECK o_conn IS BOUND.
  o_typedesc ?= cl_abap_objectdescr=>describe_by_object_ref( o_conn ).
  result = o_typedesc->get_relative_name( ).
  endmethod.


  method GET_CONNECTORLIST.
  DATA o_comp TYPE to_component.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF _classes.

  CREATE OBJECT result.
  LOOP AT _classes ASSIGNING <c>
       WHERE NOT types IS INITIAL
         AND NOT uuid IS INITIAL.
*    TRY.
        CREATE OBJECT o_comp.
        o_comp->set_type( c_class-type ).
        o_comp->set_name( <c>-classname ).
        result->add( o_comp ).
*      CATCH zaplink_cx.
*    ENDTRY.
  ENDLOOP.
  endmethod.


  method GET_TYPEKIND.
  FIELD-SYMBOLS <t> LIKE LINE OF saptypes.

  READ TABLE saptypes ASSIGNING <t>
       WITH TABLE KEY object = data.
  IF sy-subrc = 0.    result = <t>-pgmid.   ENDIF.
  endmethod.


  method GET_TYPEORDER.
  FIELD-SYMBOLS <t> LIKE LINE OF saptypes.

  READ TABLE saptypes ASSIGNING <t>
       WITH TABLE KEY object = data.
  IF sy-subrc = 0.
    result = <t>-order.
  ELSE.
    result = max_order.
  ENDIF.
  endmethod.


  method GET_VALUES.
  DATA lt_return TYPE STANDARD TABLE OF ddshretval.
  DATA d_value   TYPE help_info-fldvalue.

  d_value = type.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield               = 'OBJECT'
*     PVALKEY                = ' '
*     DYNPPROG               = ' '
*     DYNPNR                 = ' '
*     DYNPROFIELD            = ' '
*     STEPL                  = 0
      window_title           = 'Installed connectors'(sh1)
      value                  = d_value
      value_org              = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY                = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     MARK_TAB               =
*   IMPORTING
*     USER_RESET             =
    TABLES
      value_tab              = value_table
*     FIELD_TAB              =
      return_tab             = lt_return
*     DYNPFLD_MAPPING        =
    EXCEPTIONS
      parameter_error        = 1
      no_values_found        = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'F4IF_INT_TABLE_VALUE_REQUEST'
                                                subrc = sy-subrc
                                            classname = 'ZAPLINK_CX_CONNECTOR' ).
    o_mycx->update( ).
    RAISE EXCEPTION o_mycx.
  ELSE.
    type = d_value.
  ENDIF.
  endmethod.


  method NODE_2_RAW.
  DATA ez_xml    TYPE REF TO zaplink_easyxml.
  DATA _xml_comp TYPE REF TO if_ixml_document.
  DATA _clone    TYPE REF TO if_ixml_node.
  DATA _cx       TYPE REF TO zaplink_cx.

  _xml_comp = xmlengine->create_document( ).
  _clone = node->clone( ).
  _xml_comp->if_ixml_node~append_child( _clone ).
  CREATE OBJECT result.
  CREATE OBJECT ez_xml.
  TRY.
      CALL METHOD ez_xml->xml2any
        EXPORTING
          xmldoc = _xml_comp
        CHANGING
          any    = result.
    CATCH zaplink_cx INTO _cx.
      CREATE OBJECT o_mycx
        EXPORTING
          textid   = zaplink_cx=>system_error
          previous = _cx.
      o_mycx->update( ).
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method RAW_2_STRING.
  DATA o_xml  TYPE to_xml.

  CHECK data IS BOUND.
*  TRY.
      o_xml = raw_2_xml( data ).
      result = xml_2_string( o_xml ).
*    CATCH zaplink_cx INTO o_cx.
*      CREATE OBJECT o_mycx
*        EXPORTING
*          textid   = zaplink_cx=>system_error
*          previous = o_cx.
*      o_mycx->update( ).
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method RAW_2_XML.
  DATA ez_xml    TYPE REF TO zaplink_easyxml.

  CHECK data IS BOUND.

  CREATE OBJECT ez_xml.
*  TRY.
      result = ez_xml->any2xml( any       = data
                                type      = comp_nodename ).
*    CATCH zaplink_cx INTO o_cx.
*      mac_cascade_raise o_mycx o_cx.
*  ENDTRY.
  endmethod.


  method RESET_CONNECTORS.
  DATA o_conn      TYPE REF TO zaplink_connector.
  DATA _connectors TYPE REF TO cl_oo_if_relations.
  DATA o_desc      TYPE REF TO cl_abap_typedescr.
  FIELD-SYMBOLS:
    <im_cl> LIKE LINE OF _connectors->implementing_classes,
    <s_cl> LIKE LINE OF _connectors->subclasses.

  CLEAR type_list.
*Issue 3
* Check interface type (local or global).
  o_desc = cl_abap_structdescr=>describe_by_name( 'ZAPLINK_CONNECTOR' ).
* when Local : \PROGRAM=ZAPLINK_INSTALLER\INTERFACE=ZAPLINK_CONNECTOR
* when Global : \INTERFACE=ZAPLINK_CONNECTOR
  IF o_desc->absolute_name CP '\INTERFACE=*'.
* Global Interface : check use
    CREATE OBJECT _connectors
      EXPORTING
        clsname      = 'ZAPLINK_CONNECTOR'
*      w_references = seox_true
*      w_implementings = seox_true
*      w_comprisings = seox_true
*      w_subclasses = seox_true
      EXCEPTIONS
        not_existing = 0    " For installer ignore not existing class
        is_class     = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'cl_oo_if_relations'
                                                       method = 'constructor'
                                                        subrc = sy-subrc
                                                 cx_classname = 'ZAPLINK_CX_CONNECTOR').
      RAISE EXCEPTION o_mycx.
    ENDIF.
    IF _connectors IS BOUND.
      LOOP AT _connectors->implementing_classes ASSIGNING <im_cl>
              WHERE exposure <> '0'   " Private
                AND version = '1'     " Active
                AND state <> '0'.     " Only modeled

        TRY.

            _add_connector( class = <im_cl>-clsname ).

          CATCH zaplink_cx INTO o_cx.
            IF o_cx->is_exception_text( zaplink_cx_connector=>twice_uuid ) = abap_true OR
               o_cx->is_exception_text( zaplink_cx_connector=>twice_class ) = abap_true.
              RAISE EXCEPTION o_cx.
            ENDIF.
        ENDTRY.

      ENDLOOP.

      LOOP AT _connectors->subclasses ASSIGNING <s_cl>
              WHERE state <> '0'      " Only modeled
                AND version = '1'.    " Active

        TRY.

            _add_connector( class = <s_cl>-clsname ).

          CATCH zaplink_cx INTO o_cx.
            IF o_cx->is_exception_text( zaplink_cx_connector=>twice_uuid ) = abap_true OR
               o_cx->is_exception_text( zaplink_cx_connector=>twice_class ) = abap_true.
              RAISE EXCEPTION o_cx.
            ENDIF.
        ENDTRY.

      ENDLOOP.

      _update_def_class( ).
    ENDIF.
  ENDIF.
  endmethod.


  method STRING_2_XML.
  DATA o_streamfactory  TYPE REF TO if_ixml_stream_factory.
  DATA o_stream         TYPE REF TO if_ixml_istream.
  DATA o_parser         TYPE REF TO if_ixml_parser.
  DATA _tempstring      TYPE string.
  DATA _rc              TYPE sysubrc.
  DATA o_doc            TYPE to_xml.

  CHECK NOT data IS INITIAL.

  TRY.
      o_streamfactory = xmlengine->create_stream_factory( ).
      o_doc = xmlengine->create_document( ).
      o_stream = o_streamfactory->create_istream_string( data ).
      o_parser = xmlengine->create_parser(  stream_factory = o_streamfactory
                                            istream        = o_stream
                                            document       = o_doc ).
      o_parser->parse( ).
    CATCH cx_root.
  ENDTRY.

  IF o_doc IS BOUND.  result = o_doc.   ENDIF.
  endmethod.


  method TYPE2TEXT.
  FIELD-SYMBOLS <t> LIKE LINE OF saptypes.

  READ TABLE saptypes ASSIGNING <t>
       WITH TABLE KEY object = data.
  IF sy-subrc = 0.
    result = <t>-text.
  ENDIF.
  endmethod.


  method XML_2_STRING.
  DATA o_xmlengine    TYPE REF TO if_ixml.
  DATA _streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA _outputstream TYPE REF TO if_ixml_ostream.
  DATA _renderer TYPE REF TO if_ixml_renderer.
  DATA _tempstring TYPE string.
  DATA _printxmldoc TYPE REF TO cl_xml_document.
  DATA _rc      TYPE sysubrc.

  CHECK xml IS BOUND.
  o_xmlengine = cl_ixml=>create( ).
  _streamfactory = o_xmlengine->create_stream_factory( ).
  _outputstream = _streamfactory->create_ostream_cstring( _tempstring ).
  _renderer = xmlengine->create_renderer( document = xml
                                           ostream = _outputstream ).
  _renderer->set_normalizing( ).
  _rc = _renderer->render( ).
  CREATE OBJECT _printxmldoc.
  _rc = _printxmldoc->parse_string( _tempstring ).

  WHILE _tempstring(1) <> '<'.    SHIFT _tempstring LEFT BY 1 PLACES.   ENDWHILE.

  result = _tempstring.
  endmethod.


  method _ADD_CONNECTOR.
  DATA connector TYPE REF TO zaplink_connector.
  DATA o_cx_root TYPE REF TO cx_root.
  DATA o_cx_create TYPE REF TO cx_sy_create_object_error.
  DATA cx_comp   TYPE REF TO zaplink_cx_component.
  DATA tabletypeline TYPE ko105.
  DATA _conn     LIKE LINE OF type_list.
  DATA _class    LIKE LINE OF _conn-classes.
  DATA the_class LIKE LINE OF _classes.
  DATA _value    LIKE LINE OF value_table.

  FIELD-SYMBOLS:
    <cl> LIKE LINE OF _classes,
    <c>  LIKE LINE OF type_list,
    <st> LIKE LINE OF saptypes,
    <t>  LIKE LINE OF connector->supported_types.

* try to build class
  TRY.

      CREATE OBJECT connector TYPE (class).

    CATCH cx_sy_create_object_error INTO o_cx_create.
      CASE o_cx_create->textid.
        WHEN cx_sy_create_object_error=>abstract_class
          OR cx_sy_create_object_error=>private_constructor
          OR cx_sy_create_object_error=>protected_constructor.
* Ignore these exceptions
        WHEN OTHERS.
          mac_cascade_raise o_mycx o_cx_create.
      ENDCASE.
    CATCH cx_root INTO o_cx_root.
      mac_cascade_raise o_mycx o_cx_root.
  ENDTRY.

  CHECK connector IS BOUND.

  IF NOT connector->uuid IS INITIAL               AND
     NOT connector->supported_types IS INITIAL.
    the_class-uuid = connector->uuid.
* Check same class already loaded
    READ TABLE _classes ASSIGNING <cl>
               WITH TABLE KEY uuid = the_class-uuid.
    IF sy-subrc = 0.
      IF <cl>-classname <> class.                           " Issue 25
        RAISE EXCEPTION TYPE zaplink_cx_connector EXPORTING
                        textid = zaplink_cx_connector=>twice_uuid
                     connclass = class
                    otherclass = <cl>-classname
                      connuuid = <cl>-uuid.
      ELSEIF NOT ( <cl>-devclass <> local_devclass AND devclass = local_devclass ).
        RAISE EXCEPTION TYPE zaplink_cx_connector EXPORTING
                        textid = zaplink_cx_connector=>twice_class
                     connclass = class.
      ELSE.
* OK : Overide general class with local class
        LOOP AT <cl>-types ASSIGNING <t>.
          READ TABLE type_list ASSIGNING <c>
               WITH TABLE KEY object = <t>-type.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zaplink_cx_connector EXPORTING
                            textid = zaplink_cx_connector=>system_error
                           cx_name = 'TYPE NOT FOUND'.
          ELSE.
            DELETE <c>-classes WHERE classname = class.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    the_class-classname = class.
    IF NOT devclass IS INITIAL.
      the_class-devclass = devclass.
    ELSE.
      SELECT SINGLE devclass INTO the_class-devclass
        FROM tadir
        WHERE pgmid = c_class-kind
          AND object = c_class-type
          AND obj_name = the_class-classname.
    ENDIF.
    the_class-types = connector->supported_types.
    INSERT the_class INTO TABLE _classes.

    LOOP AT connector->supported_types ASSIGNING <t>.
      READ TABLE type_list ASSIGNING <c>
           WITH TABLE KEY object = <t>-type.
      IF sy-subrc <> 0.
        _conn-object = <t>-type.
        READ TABLE saptypes ASSIGNING <st>
                   WITH TABLE KEY object = _conn-object.
        IF sy-subrc = 0.
          _conn-text = <st>-text.
        ENDIF.
        INSERT _conn INTO TABLE type_list.
        READ TABLE type_list ASSIGNING <c>
             WITH TABLE KEY object = <t>-type.
      ENDIF.
      CLEAR _class.
      MOVE-CORRESPONDING the_class TO _class.
      INSERT _class INTO TABLE <c>-classes.
    ENDLOOP.
  ENDIF.

  CLEAR value_table.
  LOOP AT type_list ASSIGNING <c>.
    _value-object = <c>-object.
    _value-text = <c>-text.
    APPEND _value TO value_table.
  ENDLOOP.
  endmethod.


  method _SEARCH_CONNECTOR.

  READ TABLE type_list INTO result
             WITH TABLE KEY object = type.
  IF sy-subrc <> 0.
    READ TABLE saptypes TRANSPORTING NO FIELDS
               WITH TABLE KEY object = type.
    IF sy-subrc <> 0.
* Type '&1' is not assigned to a ZAPLINK connector and is not a known SAPobject type (transaction SE01).
      RAISE EXCEPTION TYPE zaplink_cx_connector
                 EXPORTING textid = zaplink_cx_connector=>invalid_type
                             type = type.
    ENDIF.
  ENDIF.
  endmethod.


  method _UPDATE_DEF_CLASS.
  TYPES:
    BEGIN OF t_classlist,
      class TYPE seoclsname,
    END OF t_classlist.

  DATA classlist TYPE STANDARD TABLE OF t_classlist WITH DEFAULT KEY.
  DATA tabletypeline TYPE ko105.
  DATA _conn LIKE LINE OF type_list.
  DATA _class LIKE LINE OF _conn-classes.
  DATA _types TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY.
  DATA s_type LIKE LINE OF supported_types.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF type_list.

  REFRESH supported_types.
* Default class
  LOOP AT type_list ASSIGNING <c>.
    CLEAR: s_type.    s_type-type = <c>-object.   INSERT s_type INTO TABLE supported_types.
    CHECK NOT <c>-classes IS INITIAL.
    LOOP AT <c>-classes INTO _class   WHERE devclass = local_devclass.
      EXIT.
    ENDLOOP.
    LOOP AT <c>-classes INTO _class   WHERE devclass = 'ZAPLINK-CONNECTORS-MAIN'.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0 OR _class-classname IS INITIAL.
      LOOP AT <c>-classes INTO _class WHERE devclass CP 'ZAPLINK-CONNECTORS*'.
        EXIT.
      ENDLOOP.
    ENDIF.
    IF sy-subrc <> 0 OR _class-classname IS INITIAL.
      LOOP AT <c>-classes INTO _class WHERE devclass CP 'ZAPLINK*'.
        EXIT.
      ENDLOOP.
    ENDIF.
    IF sy-subrc <> 0 OR _class-classname IS INITIAL.
      READ TABLE <c>-classes INTO _class INDEX 1.
    ENDIF.
    <c>-default_class = _class-classname.
  ENDLOOP.
  endmethod.
ENDCLASS.
