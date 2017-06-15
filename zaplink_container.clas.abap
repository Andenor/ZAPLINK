class ZAPLINK_CONTAINER definition
  public
  inheriting from ZAPLINK_CONTAINER_4INST
  create public .

public section.

  types T_XML type STRING .

  constants VERSION type TD_CONTVER value '1.0' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  class-methods CREATE_NEW
    importing
      !O_FILE type TO_FILE
      !DATA type TS_CONTDATA
    returning
      value(RESULT) type ref to ZAPLINK_CONTAINER
    raising
      ZAPLINK_CX_CONTAINER .
  methods SAVE_TO_FILE
    importing
      value(O_FILE) type TO_FILE optional
    raising
      ZAPLINK_CX_CONTAINER .
  methods ADD_COMPONENTS
    importing
      !OBJECT_LIST type TO_LIST
    raising
      ZAPLINK_CX_CONTAINER .
  methods REMOVE_COMPONENTS
    importing
      !OBJECT_LIST type TO_LIST
    raising
      ZAPLINK_CX_CONTAINER .
  methods COMPACT
    raising
      ZAPLINK_CX_CONTAINER .
protected section.

  types:
    BEGIN OF ts_contdata_full.
  INCLUDE TYPE ts_contdata AS public.
  TYPES:
      version  TYPE td_contver,
      class    TYPE td_connclass,
    END OF ts_contdata_full .

  data DATA type TS_CONTDATA_FULL .

  methods _ADD_COMPONENT
    importing
      !COMPONENT type TO_COMPONENT
    raising
      ZAPLINK_CX_CONTAINER .
  methods _REMOVE_COMPONENT
    importing
      !COMPONENT type TO_COMPONENT
    raising
      ZAPLINK_CX_CONTAINER .
  methods _UPDATE_HEADER
    raising
      ZAPLINK_CX_CONTAINER .
private section.

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_CONTAINER IMPLEMENTATION.


  method ADD_COMPONENTS.
  DATA o_comp    TYPE zaplink_list=>to_component.
  DATA msg       TYPE string.
  DATA _msg_col  TYPE to_log.

  CHECK object_list IS BOUND.
  TRY.
* Initialize Interator
    object_list->order_kind = zaplink_list=>order_kinds-no_order.
    object_list->init_sel_iter( ).
    object_list->display_progress = abap_true.
    o_comp = object_list->get_next( ).

    WHILE o_comp IS BOUND.
      TRY.
          _add_component( component = o_comp ).
        CATCH zaplink_cx_container INTO o_mycx.
          CALL METHOD log->add_exception
            EXPORTING
              exception    = o_mycx
*                id_detlevel  =
              id_probclass = log->probcl-medium.
          _msg_col = o_comp->get_msg_coll( ).
          IF _msg_col IS BOUND.
            CALL METHOD log->add_from_instance
              EXPORTING
                io_msglist        = _msg_col
                if_add_as_subnode = abap_true
*                if_cumulate       =
                .
          ENDIF.
      ENDTRY.

      o_comp = object_list->get_next( ).
    ENDWHILE.

    _complist->refresh( ).    " Issue 102
    compact( ).
    _update_componentslist( ).

    mac_def_catch zaplink_cx_container.
  ENDTRY.
  endmethod.


  method CLASS_CONSTRUCTOR.
xmlengine = cl_ixml=>create( ).
  endmethod.


  method COMPACT.
  DATA o_list TYPE to_list.
  DATA o_comp TYPE to_component.
  DATA o_subcomps TYPE to_list.

  TRY.
      _complist->init_iter( ).
      _complist->display_progress = abap_false.
      o_comp = _complist->get_next( ).
      WHILE o_comp IS BOUND.
        o_list = _complist->clone( ).
        o_list->remove( o_comp ).
        o_subcomps = o_comp->get_subcomponents( ).
        IF o_subcomps IS NOT BOUND.
          CREATE OBJECT o_subcomps.
        else.
          o_subcomps = o_subcomps->clone( ).    " Issue 49 : DO not use list itself because generate nested loop
        ENDIF.
        o_subcomps->add( o_comp ).
        IF o_list->is_included( o_subcomps ) = abap_true.
* components o_comp and it's subcomponents do exists as sub components of an other components (like DEVC)
          _remove_component( o_comp ).
        ENDIF.
        o_comp = _complist->get_next( ).
      ENDWHILE.
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method CREATE_NEW.
  CHECK NOT data-name IS INITIAL.
  CREATE OBJECT result
    EXPORTING
      o_file = o_file.
  result->data-public = data.
  result->data-version = version.
  result->data-class = zaplink_tools=>get_clas_name( result ).
  result->_update_header( ).
  endmethod.


  method REMOVE_COMPONENTS.
  DATA o_comp    TYPE zaplink_list=>to_component.

  CHECK object_list IS BOUND.
  TRY.
      object_list->order_kind = zaplink_list=>order_kinds-no_order.
      object_list->init_sel_iter( ).
      object_list->display_progress = abap_true.

      o_comp = object_list->get_next( ).
      WHILE o_comp IS BOUND.
        TRY.
            _remove_component( o_comp ).
          CATCH zaplink_cx_connector.
        ENDTRY.

        o_comp = object_list->get_next( ).
      ENDWHILE.
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method SAVE_TO_FILE.
  DATA _streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA _outputstream  TYPE REF TO if_ixml_ostream.
  DATA _renderer      TYPE REF TO if_ixml_renderer.
  DATA _tempstring    TYPE string.
  DATA _printxmldoc   TYPE REF TO cl_xml_document.
  DATA _rc            TYPE sysubrc.
  DATA _file          TYPE to_file.
  DATA go_encoding    TYPE REF TO if_ixml_encoding.

  IF NOT xmldata IS BOUND.
    EXIT.
  ENDIF.

  _streamfactory = xmlengine->create_stream_factory( ).
  _outputstream = _streamfactory->create_ostream_cstring( _tempstring ).
  go_encoding = _outputstream->get_encoding( ).
  go_encoding->set_character_set( 'utf-8' ).    " issue 136
  _update_header( ).
  _renderer = xmlengine->create_renderer( document = xmldata
                                           ostream = _outputstream ).
  _renderer->set_normalizing( ).
  _rc = _renderer->render( ).
  CREATE OBJECT _printxmldoc.
  _rc = _printxmldoc->parse_string( _tempstring ).

  WHILE _tempstring(1) <> '<'.
    SHIFT _tempstring LEFT BY 1 PLACES.
  ENDWHILE.

  IF o_file IS BOUND.
    _file = o_file.
  ELSE.
    _file = file.
  ENDIF.
  TRY.
      _file->set_filecontent( _tempstring ).
      _file->save( ).
    CATCH zaplink_cx_file.
      RETURN.
  ENDTRY.
  file = _file.
  endmethod.


  method _ADD_COMPONENT.
  DATA o_connector TYPE zaplink_component=>to_connector.
  DATA root      TYPE REF TO if_ixml_element.
  DATA _base     TYPE REF TO if_ixml_node.
  DATA rval      TYPE i.
  DATA _comp     TYPE zaplink_component=>ts_component.
  DATA object    TYPE to_xml.
  DATA _index    LIKE LINE OF _nodeindex.
  DATA o_cx_cnx  TYPE REF TO zaplink_cx_connector.
*  DATA o_cx_comp TYPE REF TO zaplink_cx_component.
  DATA o_cx_list TYPE REF TO zaplink_cx_list.

  CHECK component IS BOUND.
  TRY.
      _comp = component->get_key( ).
      o_connector = component->connector( ).
    CATCH zaplink_cx INTO o_cx.
      TRY.
          component->set_exception( o_cx ).
        CATCH zaplink_cx INTO o_cx.
          mac_cascade_raise o_mycx o_cx.
      ENDTRY.
* Failed to add component '&NAME&' (&TYPE&). Look at exception & message collector in component to get the root cause.
      RAISE EXCEPTION TYPE zaplink_cx_container
                 EXPORTING textid = zaplink_cx_container=>add_comp_failed
                             type = _comp-type
                             name = _comp-name.
  ENDTRY.

  ASSERT o_connector IS BOUND.

* Extract Component
  TRY.
      o_connector->options = _options.
      object = o_connector->export_from_sap( component ).
      component->set_msg_coll( o_connector->application_log ).
      mac_add_log o_connector->application_log log 'W'.
    CATCH zaplink_cx_connector INTO o_cx_cnx.
      TRY.
          component->set_exception( o_cx_cnx ).
          component->set_msg_coll( o_connector->application_log ).
        CATCH zaplink_cx INTO o_cx.
          mac_cascade_raise o_mycx o_cx.
      ENDTRY.
* Failed to add component '&NAME&' (&TYPE&). Look at exception & message collector in component to get the root cause.
      RAISE EXCEPTION TYPE zaplink_cx_container
                 EXPORTING textid = zaplink_cx_container=>add_comp_failed
                         previous = o_cx_cnx
                             type = _comp-type
                             name = _comp-name.
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.

  IF NOT object IS BOUND.
* Failed to add component '&NAME&' (&TYPE&). Look at exception & message collector in component to get the root cause.
    RAISE EXCEPTION TYPE zaplink_cx_container
               EXPORTING textid = zaplink_cx_container=>add_comp_failed
                           type = _comp-type
                           name = _comp-name.

  ENDIF.

  TRY.
      root = object->get_root_element( ).
      _base = get_root( ). " xmldata

      CHECK root IS BOUND.
      _remove_component( component ).
      rval = _base->append_child( root ).
      IF rval <> 0.
* error handling
        EXIT.
      ENDIF.

* Issue 102 : Replaced code
*      TRY.
*          _complist->add( component ).
*        CATCH zaplink_cx_list INTO o_cx_list.
*          TRY.
*              component->set_exception( o_cx_list ).
*            CATCH zaplink_cx INTO o_cx.
*              mac_cascade_raise o_mycx o_cx.
*          ENDTRY.
** Failed to add component '&NAME&' (&TYPE&). Look at exception & message collector in component to get the root cause.
*          RAISE EXCEPTION TYPE zaplink_cx_container
*                     EXPORTING textid = zaplink_cx_container=>add_comp_failed
*                                 type = _comp-type
*                                 name = _comp-name.
*      ENDTRY.
*
*      _index-key = component.
**      _index-node = _find_component_node( component ).       " Find_component is based on node_index it can't be used to get node.
*      _index-node = root.
*      MODIFY TABLE _nodeindex FROM _index.
*      IF sy-subrc <> 0.
*        INSERT _index INTO TABLE _nodeindex.
*      ENDIF.

      _search_components( root ).     " Issue 102 : to keep comp list up to date

    CATCH zaplink_cx_container INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method _REMOVE_COMPONENT.
  DATA node   TYPE REF TO if_ixml_node.
  DATA o_comp TYPE to_component.

  TRY.
    node = _find_component_node( component ).
    WHILE node IS BOUND.
      node->remove_node( ).
      IF sy-subrc = 0.
        DELETE _nodeindex WHERE node = node.
        o_comp = _complist->search( component ).
        IF o_comp IS BOUND.
          DELETE _nodeindex WHERE key = o_comp.
        ELSE. " for security
          DELETE _nodeindex WHERE key = component.
        ENDIF.
        _complist->remove( component ).
      ENDIF.
      node = _find_component_node( component ).
    ENDWHILE.

    mac_def_catch zaplink_cx_container.
  ENDTRY.
  endmethod.


  method _UPDATE_HEADER.
  DATA _root TYPE REF TO if_ixml_element.
  DATA _ezxml TYPE REF TO zaplink_easyxml.

  _root = xmldata->get_root_element( ).

  IF NOT _root IS BOUND.
    _root = xmldata->create_element( name = tn_container_root ).
    xmldata->if_ixml_node~append_child( _root ).
  ENDIF.

  CREATE OBJECT _ezxml.
  _ezxml->add_any( EXPORTING xml_node = _root
                                  any = data
*                                  name = tn_container_root
                               xmldoc = xmldata ).
  endmethod.
ENDCLASS.
