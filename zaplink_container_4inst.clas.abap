class ZAPLINK_CONTAINER_4INST definition
  public
  create public .

public section.
  type-pools ABAP .

  interfaces ZAPLINK_DATATYPES .

  aliases BALLOG_OBJECT
    for ZAPLINK_DATATYPES~BALLOG_OBJECT .
  aliases COMP_NODENAME
    for ZAPLINK_DATATYPES~COMP_NODENAME .
  aliases EXISTS
    for ZAPLINK_DATATYPES~EXISTS .
  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TEXTPOOL_KINDS
    for ZAPLINK_DATATYPES~TEXTPOOL_KINDS .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_ACTION
    for ZAPLINK_DATATYPES~TD_ACTION .
  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
  aliases TD_CLASSNAME
    for ZAPLINK_DATATYPES~TD_CLASSNAME .
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
  aliases TD_CONNVER
    for ZAPLINK_DATATYPES~TD_CONNVER .
  aliases TD_CONTNAME
    for ZAPLINK_DATATYPES~TD_CONTNAME .
  aliases TD_CONTVER
    for ZAPLINK_DATATYPES~TD_CONTVER .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TD_EXISTS
    for ZAPLINK_DATATYPES~TD_EXISTS .
  aliases TD_FILENAME
    for ZAPLINK_DATATYPES~TD_FILENAME .
  aliases TD_FILETYPE
    for ZAPLINK_DATATYPES~TD_FILETYPE .
  aliases TD_LANG
    for ZAPLINK_DATATYPES~TD_LANG .
  aliases TD_LIGHT
    for ZAPLINK_DATATYPES~TD_LIGHT .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .
  aliases TD_ORDERKIND
    for ZAPLINK_DATATYPES~TD_ORDERKIND .
  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TD_SUBSTITUTIONKIND
    for ZAPLINK_DATATYPES~TD_SUBSTITUTIONKIND .
  aliases TD_TRANSPORT_REQUEST
    for ZAPLINK_DATATYPES~TD_TRANSPORT_REQUEST .
  aliases TD_TXTP_ID
    for ZAPLINK_DATATYPES~TD_TXTP_ID .
  aliases TD_TXTP_KIND
    for ZAPLINK_DATATYPES~TD_TXTP_KIND .
  aliases TD_TXTP_LEN
    for ZAPLINK_DATATYPES~TD_TXTP_LEN .
  aliases TD_TXTP_TEXT
    for ZAPLINK_DATATYPES~TD_TXTP_TEXT .
  aliases TD_TYPEORDER
    for ZAPLINK_DATATYPES~TD_TYPEORDER .
  aliases TD_TYPE_AS_TEXT
    for ZAPLINK_DATATYPES~TD_TYPE_AS_TEXT .
  aliases TO_XML
    for ZAPLINK_DATATYPES~TO_XML .
  aliases TR_DLVUNIT
    for ZAPLINK_DATATYPES~TR_DLVUNIT .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TR_TR
    for ZAPLINK_DATATYPES~TR_TR .
  aliases TS_BASE_ATTRIBUTS
    for ZAPLINK_DATATYPES~TS_BASE_ATTRIBUTS .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TS_COMPONENT
    for ZAPLINK_DATATYPES~TS_COMPONENT .
  aliases TS_CONNDATA
    for ZAPLINK_DATATYPES~TS_CONNDATA .
  aliases TS_CONN_CLASS
    for ZAPLINK_DATATYPES~TS_CONN_CLASS .
  aliases TS_CONN_DEF
    for ZAPLINK_DATATYPES~TS_CONN_DEF .
  aliases TS_CONTDATA
    for ZAPLINK_DATATYPES~TS_CONTDATA .
  aliases TS_DIRECTORY
    for ZAPLINK_DATATYPES~TS_DIRECTORY .
  aliases TS_DOC
    for ZAPLINK_DATATYPES~TS_DOC .
  aliases TS_HEAD
    for ZAPLINK_DATATYPES~TS_HEAD .
  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .
  aliases TS_TXTP_TEXT
    for ZAPLINK_DATATYPES~TS_TXTP_TEXT .
  aliases TS_TXTP_TEXTPOOL
    for ZAPLINK_DATATYPES~TS_TXTP_TEXTPOOL .
  aliases TS_TYPE
    for ZAPLINK_DATATYPES~TS_TYPE .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .
  aliases TT_CONN_CLASSES
    for ZAPLINK_DATATYPES~TT_CONN_CLASSES .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_RAWTEXT
    for ZAPLINK_DATATYPES~TT_RAWTEXT .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
  aliases TT_TXTP_TEXTS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTS .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  types TO_RAW_BASE type ref to ZAPLINK_RAW_BASE .
  types TO_EXCEPTION type ref to ZAPLINK_CX_CONTAINER .
  types TO_OPTIONS type ref to ZAPLINK_OPTIONS .
  types TO_RAW type ref to ZAPLINK_RAW .
  types TO_LOG type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .
  types TO_FILE type ref to ZAPLINK_FILE .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .

  constants TN_CONTAINER_ROOT type STRING value 'CONTAINER' ##NO_TEXT.
  data FILE type TO_FILE .
  data LOG type TO_LOG read-only .
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CONTAINER' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !O_FILE type TO_FILE .
  methods LOAD_FROM_FILE
    importing
      !O_FILE type TO_FILE optional
    raising
      ZAPLINK_CX_CONTAINER .
  methods LOAD_FROM_XML
    importing
      !XML type STRING
    raising
      ZAPLINK_CX_CONTAINER .
  methods TRANSPORT_TO_SAP
    importing
      !OBJECT_LIST type TO_LIST
    raising
      ZAPLINK_CX_CONTAINER .
  methods GET_CONTENT
    returning
      value(RESULT) type TO_LIST
    raising
      ZAPLINK_CX_CONTAINER .
  methods GET_OPTIONS
    returning
      value(RESULT) type TO_OPTIONS
    raising
      ZAPLINK_CX_CONTAINER .
  methods UNINSTALL_FROM_SAP
    importing
      !OBJECT_LIST type TO_LIST
    raising
      ZAPLINK_CX_CONTAINER .
  methods SET_OPTIONS
    importing
      !DATA type TO_OPTIONS
    raising
      ZAPLINK_CX_CONTAINER .
protected section.

  types:
    BEGIN OF ts_nodeindex,
      key   TYPE ref to zaplink_component,
      node  TYPE REF TO if_ixml_node,
    END OF ts_nodeindex .
  types:
    tt_nodeindex type SORTED TABLE OF ts_nodeindex with UNIQUE key key .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .

  data XMLDATA type ref to IF_IXML_DOCUMENT .
  data _OPTIONS type TO_OPTIONS .
  class-data XMLENGINE type ref to IF_IXML .
  data _COMPLIST type TO_LIST .
  data _NODEINDEX type TT_NODEINDEX .

  methods _FIND_COMPONENT_NODE
    importing
      !COMPONENT type TO_COMPONENT
      !SUB_COMP type TO_COMPONENT optional
    returning
      value(NODE) type ref to IF_IXML_NODE
    raising
      ZAPLINK_CX_CONTAINER .
  methods _UPDATE_COMPONENTSLIST
    raising
      ZAPLINK_CX_CONTAINER .
  methods GET_ROOT
    returning
      value(ROOT) type ref to IF_IXML_NODE .
  methods _IS_SUBCOMPONENT
    importing
      !NODE type ref to IF_IXML_NODE
    returning
      value(RESULT) type ABAP_BOOL .
  methods _SEARCH_COMPONENTS
    importing
      !NODE type ref to IF_IXML_NODE
    raising
      ZAPLINK_CX_CONTAINER .
  methods _ADD_TO_COMPONENTSLIST
    importing
      !NODE type ref to IF_IXML_NODE
    raising
      ZAPLINK_CX_CONTAINER .
  methods _UNINSTALL_COMPONENT
    importing
      !COMPONENT type TO_COMPONENT
    raising
      ZAPLINK_CX_CONTAINER .
private section.

  class-data O_MYCX type TO_EXCEPTION .
  data O_CURSOR type ref to IF_IXML_NODE .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_CONTAINER_4INST IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  xmlengine = cl_ixml=>create( ).
  endmethod.


  method CONSTRUCTOR.
  CREATE OBJECT _complist.
  CREATE OBJECT _options.
  mac_create_log log ballog_subobject space.
  log->msgid = 'ZAPLINK'.
  xmldata = xmlengine->create_document( ).
  file = o_file.
  endmethod.


  method GET_CONTENT.
  TRY.
      result = _complist->clone( ).
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method GET_OPTIONS.
result = _options.
  endmethod.


  method GET_ROOT.
  root = xmldata->get_first_child( ).
  check root is bound.
*  IF root->get_name( ) <> tn_container_root.
*    CLEAR root.
*  ENDIF.
  endmethod.


  method LOAD_FROM_FILE.
  DATA _xml   TYPE string.
  DATA _file  TYPE to_file.

  IF o_file IS BOUND.
    _file = o_file.
  ELSE.
    _file = file.
  ENDIF.

  TRY.
      _file->load( ).
      _xml = _file->get_filecontent( ).
    CATCH zaplink_cx_file.
      RETURN.
  ENDTRY.
  file = _file.

  load_from_xml( _xml ).
  endmethod.


  method LOAD_FROM_XML.
  DATA _streamfactory   TYPE REF TO if_ixml_stream_factory.
  DATA _stream          TYPE REF TO if_ixml_istream.
  DATA _parser          TYPE REF TO if_ixml_parser.
  DATA _doc             TYPE REF TO	if_ixml_document.

  _streamfactory = xmlengine->create_stream_factory( ).
  _doc = xmlengine->create_document( ).
  _stream = _streamfactory->create_istream_string( xml ).
  _parser = xmlengine->create_parser(  stream_factory = _streamfactory
                                       istream        = _stream
                                       document       = _doc ).
  _parser->parse( ).
  IF _doc IS BOUND.
    xmldata = _doc.
  ENDIF.

  _update_componentslist( ).
  endmethod.


  method SET_OPTIONS.
check data <> _options.
  _options = data.
  endmethod.


  method TRANSPORT_TO_SAP.
  DATA o_connector  TYPE to_connector.
  DATA o_list_comp  TYPE to_component.
  DATA o_comp       TYPE to_component.
  DATA o_comp_n     TYPE to_component.
  DATA o_comp_s     TYPE to_component.
  DATA o_listsub    TYPE to_list.
  DATA o_subcomp    TYPE to_component.
*  DATA object_off        TYPE to_raw.
  DATA _components  TYPE to_list.
  DATA _xml_comp    TYPE REF TO if_ixml_document.
  DATA _clone       TYPE REF TO	if_ixml_node.
  DATA _cx_connector TYPE REF TO zaplink_cx_connector.
  DATA o_raw        TYPE to_raw_base.
  DATA _directory   TYPE ts_directory.
  DATA o_log        TYPE to_log.
* Issue 47 : handling dependencies  --> Start
  DATA o_todo       TYPE to_list.
  DATA o_remain     TYPE to_list.
  DATA o_skiplist   TYPE to_list.
  DATA o_deplist    TYPE to_list.
* Issue 47 : handling dependencies  <-- END
* Issue 124 : Check dependencies --> Start
  DATA o_chk_exist  TYPE to_component.
  DATA d_chk_name   TYPE td_compname.
  DATA d_chk_type   TYPE td_comptype.
  DATA f_dep_ok     TYPE abap_bool.
* Issue 124 : Check dependencies <-- End
  DATA d_name       TYPE td_compname.
  DATA d_type       TYPE td_comptype.
  DATA f_ok         TYPE abap_bool.
  DATA d_msgno      TYPE symsgno.
  DATA d_msgty      TYPE symsgty.

  CHECK object_list IS BOUND.
  IF zaplink_tools=>import_allowed <> abap_true.
    RAISE EXCEPTION TYPE zaplink_cx_import_not_allowed.
  ENDIF.
  TRY.
      object_list->keep_commun_list( _complist ).
* Issue 47 : handling dependencies  --> Start
      o_todo = object_list->clone( ).
      DO.
        CREATE OBJECT o_skiplist.
        o_remain = o_todo->clone( ).
* Issue 47 : handling dependencies  <-- END
        o_todo->display_progress = abap_true.
        o_todo->order_kind = zaplink_list=>order_kinds-install.
        o_todo->init_sel_iter( ).
        o_comp_n = o_todo->get_next( ).
        WHILE o_comp_n IS BOUND.
          o_comp = o_comp_n.    o_comp_n = o_todo->get_next( ).    " to allow continue
          o_comp_s = _complist->search( o_comp ).
          ASSERT ID zaplink CONDITION o_comp_s IS BOUND.
          o_list_comp = object_list->search( o_comp ).
          ASSERT ID zaplink CONDITION o_list_comp IS BOUND.
          o_connector = o_comp_s->connector( ).
          o_connector->options = _options.
*         o_log = o_comp_s->get_msg_coll( ).
          o_log = o_list_comp->get_msg_coll( ).
          IF NOT o_log IS BOUND.
            CREATE OBJECT o_log.
*            o_comp_s
            o_list_comp->set_msg_coll( o_log ).
            CALL METHOD o_log->init
              EXPORTING
                id_object      = ballog_object
                id_subobject   = ballog_subobject
*          id_extnumber   =
                auto_upd_custo = abap_true
*          id_activity    = ACT_CREATE
              EXCEPTIONS
                error          = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
              log->add_symsg( ).
            ENDIF.
          ENDIF.

          d_name = o_comp_s->get_name( ).     d_type = o_comp_s->get_type( ).
* Processing (&1) &2
          SET EXTENDED CHECK OFF.
          IF 1 = 2.   MESSAGE i000 WITH space space.    ENDIF.
          SET EXTENDED CHECK ON.
          CALL METHOD log->add_info
            EXPORTING
*              id_detlevel  =
*              id_probclass =
*              id_msgid     =
              id_msgno     = '000'
*              id_msgv3     =
*              id_msgv4     =
              id_msgv1     = d_type
              id_msgv2     = d_name.

          _xml_comp = xmlengine->create_document( ).
          _clone = _find_component_node( o_comp_s ).
          _clone = _clone->clone( ).
          _xml_comp->if_ixml_node~append_child( _clone ).

          TRY.
* Make directory entry
              o_raw = zaplink_connectors=>node_2_raw( _clone ).
* Issue 47 : handling dependencies  --> Start
              CREATE OBJECT o_deplist.
              o_deplist->add_keys( o_raw->dependencies ).
              IF o_remain->has_any( o_deplist ) = abap_true.
                o_skiplist->add( o_comp ). " because dependcies will be processed first
                CONTINUE.
              ENDIF.
* Issue 47 : handling dependencies  <-- END
* Issue 124 : Check dependencies --> Start
              o_deplist->display_progress = abap_false.
              o_deplist->init_iter( ).
              o_chk_exist = o_deplist->get_next( ).
              f_dep_ok = abap_true.     " by default OK : Hot fix
              WHILE o_chk_exist IS BOUND.
                f_dep_ok = o_chk_exist->do_exists( ).
                IF f_dep_ok = abap_false.
                  d_chk_name = o_chk_exist->get_name( ).     d_chk_type = o_chk_exist->get_type( ).
* Failed to import (&1) &2 : Dependencie (&3) &4 is missing
                  SET EXTENDED CHECK OFF.
                  IF 1 = 2.   MESSAGE i008 WITH space space.    ENDIF.
                  SET EXTENDED CHECK ON.
                  CALL METHOD o_log->add
                    EXPORTING
*                      is_message   =
                      id_msgty     = 'E'
                      id_msgid     = 'ZAPLINK'
                      id_msgno     = 008
                      id_msgv1     = d_type
                      id_msgv2     = d_name
                      id_msgv3     = d_chk_type
                      id_msgv4     = d_chk_name.
                  EXIT.
                ENDIF.
                o_chk_exist = o_deplist->get_next( ).
              ENDWHILE.
              IF f_dep_ok = abap_false.
                CONTINUE.                     " Skip object
              ENDIF.
* Issue 124 : Check dependencies <-- End
              TRY.      " Issue 99 : Error while setting directory mustn't stop importing process
                  _directory = _options->directory->set_directory( o_raw ).
                CATCH zaplink_cx INTO o_cx.
                  o_log->add_exception( o_cx ).
              ENDTRY.
              o_log->add_from_instance( _options->directory->application_log ).
* Sub components
              o_listsub = o_comp_s->get_subcomponents( ).
              IF o_listsub IS BOUND.
                o_listsub->display_progress = abap_false.
                o_listsub->init_iter( ).
                o_subcomp = o_listsub->get_next( ).
                WHILE o_subcomp IS BOUND.
                  _clone = _find_component_node( component = o_comp_s
                                                  sub_comp = o_subcomp ).
                  o_raw = zaplink_connectors=>node_2_raw( _clone ).
                  TRY.      " Issue 99 : Error while setting directory mustn't stop importing process
                      _options->directory->set_directory( o_raw ).
                    CATCH zaplink_cx INTO o_cx.
                      o_log->add_exception( o_cx ).
                  ENDTRY.
                  o_log->add_from_instance( _options->directory->application_log ).
                  o_subcomp = o_listsub->get_next( ).
                ENDWHILE.
              ENDIF.

              _components = o_connector->import_to_sap( _xml_comp ).
              IF _components IS NOT BOUND.   CREATE OBJECT _components.   ELSE.   f_ok = abap_true. ENDIF.
              o_list_comp->set_subcomponents( _components ).
              o_list_comp->set_msg_coll( o_connector->application_log ).
              IF o_connector->application_log IS BOUND.   IF o_connector->application_log->has_messages_of_msgt('W') = abap_true.      f_ok = abap_false. log->add_from_instance( o_connector->application_log ).   ENDIF.   ENDIF.
              IF _components->search( o_comp ) IS BOUND.    _components->remove( o_comp ).    ELSE.     f_ok = abap_false.     ENDIF.
              o_remain->remove( o_comp ).                   " Issue 47
              IF f_ok = abap_true.
                d_msgty = 'S'.    d_msgno = 004.
* Succesfully imported (&1) &2
                SET EXTENDED CHECK OFF.
                IF 1 = 2.   MESSAGE i004 WITH space space.    ENDIF.
                SET EXTENDED CHECK ON.
              ELSE.
                d_msgty = 'E'.    d_msgno = 005.
* Failed to import (&1) &2
                SET EXTENDED CHECK OFF.
                IF 1 = 2.   MESSAGE i005 WITH space space.    ENDIF.
                SET EXTENDED CHECK ON.
              ENDIF.
              CALL METHOD log->add
                EXPORTING
*                  is_message   =
                  id_msgty     = d_msgty
                  id_msgid     = 'ZAPLINK'
                  id_msgno     = d_msgno
*                  id_msgv3     =
*                  id_msgv4     =
                  id_msgv1     = d_type
                  id_msgv2     = d_name.
            CATCH zaplink_cx_connector INTO _cx_connector.
              o_list_comp->set_exception( _cx_connector ).
              CALL METHOD log->add_exception
                EXPORTING
                  exception = _cx_connector.
              mac_add_log o_connector->application_log o_log 'E'.   " append application log if error is present
          ENDTRY.

        ENDWHILE.
* Issue 47 : handling dependencies  --> Start
        IF o_skiplist->is_empty( ) = abap_true.   EXIT.   ENDIF.
        IF o_todo->get_count( ) = o_skiplist->get_count( ).
* error : circular dependencies
          RAISE EXCEPTION TYPE zaplink_cx_container EXPORTING textid = zaplink_cx_container=>circular_dependencies.
        ENDIF.
        o_todo = o_skiplist->clone( ).    o_todo->select_all( ).
      ENDDO.
* Issue 47 : handling dependencies  <-- END
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method UNINSTALL_FROM_SAP.
  DATA o_connector  TYPE to_connector.
  DATA o_comp       TYPE to_component.
  DATA o_scomp      TYPE to_component.
*  DATA o_ncomp      TYPE to_component.
  DATA object       TYPE to_raw.
  DATA _done        TYPE abap_bool.
  DATA s_key        TYPE ts_component.
*  DATA _cx_connector TYPE REF TO zaplink_cx_connector.
  DATA o_log        TYPE to_log.
  DATA s_lock       TYPE tlock_int.                         " Issue 101
  DATA d_flag       TYPE trpari-s_lockflag.                 " Issue 101
  DATA o_raw        TYPE to_raw_base.                       " Issue 101
  DATA _directory   TYPE ts_directory.                      " Issue 101
  DATA o_list       TYPE to_list.                           " Issue 104

  CHECK object_list IS BOUND.
  TRY.
*  object_list->keep_commun_list( _complist ).
      object_list->display_progress = abap_true.
      object_list->order_kind = zaplink_list=>order_kinds-uninstall.

      object_list->init_sel_iter( ).
      o_comp = object_list->get_next( ).
      WHILE o_comp IS BOUND.
        o_scomp = _complist->search( o_comp ).
        IF o_scomp IS BOUND.
* Delete sub object : Issue 104
          o_list = o_scomp->get_subcomponents( ).
          IF o_list IS BOUND.
            o_list->display_progress = abap_false.
            o_list->order_kind = zaplink_list=>order_kinds-uninstall.
            o_list->select_all( ).
            o_list->init_sel_iter( ).
            o_scomp = o_list->get_next( ).
            WHILE o_scomp IS BOUND.
              _uninstall_component( o_scomp ).
              o_log = o_scomp->get_msg_coll( ).
              log->add_from_instance( io_msglist        = o_log
                                      if_add_as_subnode = abap_true ).
              o_scomp = o_list->get_next( ).
            ENDWHILE.
          ENDIF.
        ENDIF.
* Delete sub object : end
        _uninstall_component( o_comp ).
        o_log = o_comp->get_msg_coll( ).
        log->add_from_instance( io_msglist        = o_log
                                if_add_as_subnode = abap_true ).
        o_comp = object_list->get_next( ).
      ENDWHILE.
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method _ADD_TO_COMPONENTSLIST.
  DATA o_raw       TYPE to_raw_base.
  DATA o_comp      TYPE to_component.
  DATA o_comp_p    TYPE to_component.
*  DATA o_cx_comp   TYPE REF TO zaplink_cx_component.
  DATA o_conn      TYPE to_connector.
  DATA o_list      TYPE to_list.
  DATA _index      LIKE LINE OF _nodeindex.
  DATA str         TYPE string.
  DATA d_sub       TYPE abap_bool.
*  DATA d_uuid_init TYPE td_connuuid.

  CHECK node IS BOUND.
  str = node->get_name( ).
  CHECK str = comp_nodename. " Security
  _index-node = node.          " save current node
  d_sub = _is_subcomponent( _index-node ).      " Issue 102

  TRY.
      o_raw = zaplink_connectors=>node_2_raw( _index-node ).
      IF o_raw->name IS INITIAL.
*        CALL METHOD log->add_error
        EXIT.
      ENDIF.
      TRY.
          o_comp = o_raw->get_component( ).
          o_conn = o_comp->connector( ).
        CATCH zaplink_cx_container INTO o_mycx.
          log->add_exception( exception = o_mycx ).
          o_comp = o_raw->get_component( abap_true ).
      ENDTRY.
*      TRY.
*          d_uuid_init = o_raw->connector.
*          o_comp->set_connuuid( d_uuid_init ).
*          o_conn = o_comp->connector( ).
*        CATCH zaplink_cx_component INTO o_cx_comp.
**        CALL METHOD log->add_warning
*          CLEAR o_raw->connector.
*          o_comp->set_connuuid( d_uuid_init ).
*          o_conn = o_comp->connector( ).
*      ENDTRY.

      IF NOT o_conn IS BOUND.
*        CALL METHOD log->add_warning
      ELSE.
        IF o_conn->is_supported_version( o_raw->version ) = abap_false.
*        CALL METHOD log->add_warning
        ENDIF.
      ENDIF.

      IF d_sub = abap_false.      " Issue 102 : Root comp
        o_comp_p = _complist->search( o_comp ).
        IF o_comp_p IS BOUND.   o_comp = o_comp_p.    ELSE.   _complist->add( o_comp ).     ENDIF.
      ELSE.
* Not working : node are the same but it's a new object. :(
*      READ TABLE _nodeindex INTO _index
*           WITH KEY node = o_cursor.
        o_raw = zaplink_connectors=>node_2_raw( o_cursor ).   " Read Parent
        o_comp_p = o_raw->get_component( ).
        o_comp_p = _complist->search( o_comp_p ).
        o_list = o_comp_p->get_subcomponents( ).
        IF NOT o_list IS BOUND.
          CREATE OBJECT o_list.
          o_comp_p->set_subcomponents( o_list ).
        ENDIF.
        IF NOT o_list->search( o_comp ) IS BOUND.   o_list->add( o_comp ).    ENDIF.      " Issue 102
      ENDIF.

* Issue 10 : update node list
      _index-key = o_comp.
      MODIFY TABLE _nodeindex FROM _index.
      IF sy-subrc <> 0.   INSERT _index INTO TABLE _nodeindex.    ENDIF.

    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method _FIND_COMPONENT_NODE.
  DATA o_comp   TYPE to_component.
  DATA o_list   TYPE to_list.
  DATA _cx_list TYPE REF TO zaplink_cx_list.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _nodeindex.

  TRY.
    TRY.
        o_comp = _complist->search( component ).
      CATCH zaplink_cx_list INTO _cx_list.
        IF _cx_list->is_exception_text( zaplink_cx_list=>not_found ) = abap_true.
          EXIT.
        ELSE.
          CREATE OBJECT o_mycx
            EXPORTING
              textid   = zaplink_cx=>system_error
              previous = _cx_list.
          o_mycx->update( ).
          RAISE EXCEPTION o_mycx.
        ENDIF.
    ENDTRY.

* Issue 10 : sub comp lookup
    CHECK o_comp IS BOUND.
    o_list = o_comp->get_subcomponents( ).
    IF sub_comp IS BOUND AND o_list IS BOUND.
      o_comp = o_list->search( sub_comp ).
    ENDIF.
* Issue 10 : end

    READ TABLE _nodeindex ASSIGNING <i>
         WITH TABLE KEY key = o_comp.
    IF sy-subrc = 0.
      node = <i>-node.
    ENDIF.

*  DATA _type     TYPE td_comptype.
*  DATA _name     TYPE td_compname.
*  DATA _cx_comp  TYPE REF TO zaplink_cx_component.
*  DATA f_type   TYPE REF TO if_ixml_node_filter.
*  DATA f_name   TYPE REF TO if_ixml_node_filter.
*  DATA o_f_node TYPE REF TO if_ixml_node_filter.
*  DATA filter   TYPE REF TO if_ixml_node_filter.
*  DATA filter_f TYPE REF TO if_ixml_node_filter.
*  DATA iterator TYPE REF TO if_ixml_node_iterator.
*  DATA _root    TYPE REF TO if_ixml_node.
*  DATA string TYPE string.
*
*  TRY.
*      _type = component->get_type( ).
*      _name = component->get_name( ).
*    CATCH zaplink_cx_component INTO _cx_comp.
*  ENDTRY.
*
*  _root = xmldata->get_root_element( ).
*  o_f_node = xmldata->create_filter_name( name = comp_nodename ).
*  string = _type.
*  f_type = xmldata->create_filter_attribute( name = 'TYPE'
*                                            value = string ).
*  string = _name.
*  f_name = _root->create_filter_attribute( name = 'NAME'
*                                          value = string ).
*  filter = _root->create_filter_and( filter1 = f_type
*                                     filter2 = f_name ).
*  filter_f = _root->create_filter_and( filter1 = filter
*                                       filter2 = filter_f ).
*  iterator = _root->create_iterator_filtered( depth = 1
*                                             filter = filter ).
*  node = iterator->get_next( ).
*  check node is bound.
*  WHILE _is_subcomponent( node ) = abap_true.
*    node = iterator->get_next( ).
*    IF NOT node IS BOUND.
*      EXIT.
*    ENDIF.
*  ENDWHILE.
    mac_def_catch zaplink_cx_container.
  ENDTRY.
  endmethod.


  method _IS_SUBCOMPONENT.
  data _cursor type ref to IF_IXML_NODE.

  result = abap_false.
  clear o_cursor.
  _cursor = node->get_parent( ).
  WHILE _cursor IS BOUND.
    IF _cursor->get_name( ) = comp_nodename.
      result = abap_true.
      o_cursor = _cursor.
*      EXIT.  new the root node
    ENDIF.
    _cursor = _cursor->get_parent( ).
  ENDWHILE.
  endmethod.


  method _SEARCH_COMPONENTS.
  DATA o_iterator  TYPE REF TO if_ixml_node_iterator.
  DATA o_filt_name TYPE REF TO if_ixml_node_filter.
  DATA o_node      TYPE REF TO  if_ixml_node.
  DATA str         TYPE string.

  CHECK node IS BOUND.
  str = node->get_name( ).
  CHECK str <> '#document'.
* Issue 102 : Search also for sub component
*  IF str = comp_nodename.
*    TRY.
*        _add_to_componentslist( node ).
*      CATCH zaplink_cx INTO o_cx.
*        mac_cascade_raise o_mycx o_cx.
*    ENDTRY.
*  ELSE.
* Search parent & childrens
    o_filt_name = node->create_filter_name( name = comp_nodename ).
    o_iterator = node->create_iterator_filtered( filter = o_filt_name ).
    o_node = o_iterator->get_next( ).
    WHILE o_node IS BOUND.
      TRY.
          _add_to_componentslist( o_node ).
        CATCH zaplink_cx INTO o_cx.
          mac_cascade_raise o_mycx o_cx.
      ENDTRY.
      o_node = o_iterator->get_next( ).

    ENDWHILE.
*  ENDIF.
  endmethod.


  method _UNINSTALL_COMPONENT.
  DATA o_connector  TYPE to_connector.
  DATA o_comp       TYPE to_component.
  DATA object       TYPE to_raw.
  DATA _done        TYPE abap_bool.
  DATA s_key        TYPE ts_component.
  DATA _cx_connector TYPE REF TO zaplink_cx_connector.
  DATA o_log        TYPE to_log.
  DATA s_lock       TYPE tlock_int.                         " Issue 101
  DATA d_flag       TYPE trpari-s_lockflag.                 " Issue 101
  DATA o_raw        TYPE to_raw_base.                       " Issue 101
  DATA _directory   TYPE ts_directory.                      " Issue 101
*  DATA o_list       TYPE to_list.                           " Issue 104

  CHECK component IS BOUND.
  o_comp = component.
  o_connector = o_comp->connector( ).
*  o_list = o_comp->get_subcomponents( ).
*  IF o_list IS BOUND.
*    uninstall_from_sap( o_list ).
*  ENDIF.

  o_log = o_comp->get_msg_coll( ).
  IF NOT o_log IS BOUND.
    CREATE OBJECT o_log.
    o_comp->set_msg_coll( o_log ).
    CALL METHOD o_log->init
      EXPORTING
        id_object      = ballog_object
        id_subobject   = ballog_subobject
*          id_extnumber   =
        auto_upd_custo = abap_true
*          id_activity    = ACT_CREATE
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      log->add_symsg( ).
    ENDIF.
  ENDIF.

  TRY.
      s_key = o_comp->get_key( ).
* Processing (&1) &2
      IF 1 = 2.   MESSAGE i000 WITH space space.    ENDIF.
      CALL METHOD log->add_info
        EXPORTING
*              id_detlevel  =
*              id_probclass =
*              id_msgid     =
          id_msgno     = '000'
*              id_msgv3     =
*              id_msgv4     =
          id_msgv1     = s_key-type
          id_msgv2     = s_key-name.

      CALL FUNCTION 'TRINT_CHECK_LOCKS'
        EXPORTING
          wi_lock_key = s_lock
        IMPORTING
          we_lockflag = d_flag
        EXCEPTIONS
          empty_key   = 1
          OTHERS      = 2.
      IF sy-subrc = 0 AND d_flag IS INITIAL.
        CREATE OBJECT o_raw.
        o_raw->set_component( o_comp ).
        _directory = _options->directory->set_directory( o_raw ).
      ENDIF.

      _done = o_connector->delete_from_sap( o_comp ).
      IF _done = abap_true.
* Succesfully deleted &1 &2
        SET EXTENDED CHECK OFF.
        IF 1 = 2. MESSAGE s006 WITH space space. ENDIF.
        SET EXTENDED CHECK ON.
        CALL METHOD log->add_success
          EXPORTING
*                  id_msgid     = 'ZAPLINK'
            id_msgno     = '006'
            id_msgv1     = s_key-type
            id_msgv2     = s_key-name
*                  id_msgv3     =
*                  id_msgv4     =
           .
      ELSE.
* Failed to delete &1 &2
        SET EXTENDED CHECK OFF.
        IF 1 = 2. MESSAGE e007 WITH space space. ENDIF.
        SET EXTENDED CHECK ON.
        CALL METHOD log->add_error
          EXPORTING
*                  id_msgid     = 'ZAPLINK'
            id_msgno     = '007'
            id_msgv1     = s_key-type
            id_msgv2     = s_key-name
**              id_msgv3     =
**              id_msgv4     =
            .
      ENDIF.
      mac_add_log o_connector->application_log o_log 'W'.
    CATCH zaplink_cx_connector INTO _cx_connector.
      o_log->add_exception( exception = _cx_connector ).
  ENDTRY.
  endmethod.


  method _UPDATE_COMPONENTSLIST.
*  CALL METHOD SUPER->_UPDATE_COMPONENTSLIST.
  DATA o_iterator  TYPE REF TO if_ixml_node_iterator.
  DATA o_node      TYPE REF TO if_ixml_node.
*  DATA str         TYPE string.

*<X>
*<ZL_OBJECT CONNECTOR="29AD0A4B07A5A05AE1000000AC120173" NAME="ZAPLINK" TYPE="DEVC" VERSION="1.0">
* <DIRECTORY SRCSYSTEM="OD4" AUTHOR="TBENSIAL" MASTERLANG="E"/>
* <RAW>
*  <DATA DEVCLASS="ZAPLINK" />
* </RAW>
*</ZL_OBJECT>
*</X>
*<ZL_OBJECT CONNECTOR="29AD0A4B07A5A05AE1000000AC120173" NAME="ZAPLINK-CONNECTORS" TYPE="DEVC" VERSION="1.0">
* <DIRECTORY SRCSYSTEM="OD4" AUTHOR="TBENSIAL" MASTERLANG="E"/>
* <RAW>
*  <DATA DEVCLASS="ZAPLINK-CONNECTORS"/>
* </RAW>
*</ZL_OBJECT>
* Nodes are only :
*#document
*X
*ZL_OBJECT
*DIRECTORY
*RAW
*DATA
*the next ZL_OBJECT his unaccessible.
*and ignored : If XML tag are not closed no problem.

  _complist->clear( ).    REFRESH _nodeindex.     " Issue 102
  o_iterator = xmldata->create_iterator( depth = 1 ).   " Do only root level
  o_node = o_iterator->get_next( ).
  WHILE o_node IS BOUND.
*    str = o_node->get_name( ).
    TRY.
        _search_components( o_node ).
      CATCH zaplink_cx INTO o_cx.
        mac_cascade_raise o_mycx o_cx.
    ENDTRY.
    o_node = o_iterator->get_next( ).
  ENDWHILE.
  endmethod.
ENDCLASS.
