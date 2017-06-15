class ZAPLINK_RAW_BASE definition
  public
  create public

  global friends ZAPLINK_CONTAINER_4INST
                 ZAPLINK_EASYXML .

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
  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TD_SUBSTITUTIONKIND
    for ZAPLINK_DATATYPES~TD_SUBSTITUTIONKIND .
  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
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

  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types TO_EZ_XML type ref to ZAPLINK_EASYXML .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .

  data TYPE type TD_COMPTYPE .
  data NAME type TD_COMPNAME .
  data CONNECTOR type TD_CONNUUID .
  data VERSION type TD_CONNVER .
  data DIRECTORY type TS_DIRECTORY read-only .
  data DEPENDENCIES type TT_COMPKEYS .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !O_COMP type TO_COMPONENT optional .
  methods UPDATE_CONNECTOR_DATA
    importing
      !O_CONNECTOR type TO_CONNECTOR
    raising
      ZAPLINK_CX .
  methods SET_COMPONENT
    importing
      !O_COMP type TO_COMPONENT
    raising
      ZAPLINK_CX_CONTAINER .
  methods GET_COMPONENT
    importing
      !WITHOUT_CX type ABAP_BOOL optional
    returning
      value(O_COMP) type TO_COMPONENT
    raising
      ZAPLINK_CX .
  methods GET_TYPE
    returning
      value(RESULT) type TD_COMPTYPE .
  methods GET_NAME
    returning
      value(RESULT) type TD_COMPNAME .
  methods GET_CHECKSUM
    returning
      value(RESULT) type TD_CHECKSUM .
  methods SET_CHECKSUM
    raising
      ZAPLINK_CX .
  methods ANONYMIZE
    raising
      ZAPLINK_CX .
  methods UNANONYMIZE .
  methods GET_CODE_SIGNATURE
    returning
      value(RESULT) type TD_CHECKSUM .
  methods GET_TYPEKIND
    returning
      value(RESULT) type TD_TRANSPORT_KIND .
protected section.

  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_EXCEPTION type ref to ZAPLINK_CX_CONTAINER .

  data CHECKSUM type TD_CHECKSUM .
  data CODE_SIGNATURE type TD_CHECKSUM .

  methods CONV_TO_XML
    returning
      value(RESULT) type TO_XML
    raising
      ZAPLINK_CX .
  class-methods XML_2_STRING
    importing
      !O_XML type TO_XML
    returning
      value(RESULT) type STRING .
private section.

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
  class-data XMLENGINE type ref to IF_IXML .

  methods REFRESH_DIRECTORY
    raising
      ZAPLINK_CX_CONTAINER .
ENDCLASS.



CLASS ZAPLINK_RAW_BASE IMPLEMENTATION.


  method ANONYMIZE.
* Have to call backward parent at the end due to checksum recalc.
  clear:
    me->DIRECTORY-korrnum,
    me->DIRECTORY-srcsystem,
    me->DIRECTORY-author,
*    me->DIRECTORY-srcdep,      " Data to keep
*    me->DIRECTORY-devclass,    " Data to keep
*    me->DIRECTORY-genflag,     " Data to keep
*    me->DIRECTORY-edtflag,     " Data to keep
*    me->DIRECTORY-CPROJECT,
*    me->DIRECTORY-masterlang,  " Data to keep
*    me->DIRECTORY-VERSID,
*    me->DIRECTORY-paknocheck,  " Data to keep
*    me->DIRECTORY-objstablty,  " Data to keep
*    me->DIRECTORY-component,   " Data to keep
*    me->DIRECTORY-crelease,    " Data to keep
*    me->DIRECTORY-delflag,     " Data to keep
*    me->DIRECTORY-translttxt,  " Data to keep
    me->DIRECTORY-trkorr.
  set_checksum( ).
  endmethod.


  method CLASS_CONSTRUCTOR.
  xmlengine = cl_ixml=>create( ).
  endmethod.


  method CONSTRUCTOR.
  TRY.
      IF o_comp IS BOUND.
        set_component( o_comp ).
      ENDIF.
    CATCH zaplink_cx.
  ENDTRY.
  endmethod.


  method CONV_TO_XML.
  DATA ez_xml          TYPE to_ez_xml.
  DATA o_xml           TYPE to_xml.

  CREATE OBJECT ez_xml.
  CLEAR checksum.  " Issue 21 : clear previous checksum
  o_xml = ez_xml->any2xml( any       = me
                           type      = comp_nodename ).

  result = o_xml.
  endmethod.


  method GET_CHECKSUM.
* result = type.  " Issue 21
  result = checksum.
  endmethod.


  method GET_CODE_SIGNATURE.
  result = code_signature.
  endmethod.


  method GET_COMPONENT.
  TRY.
      CREATE OBJECT o_comp.

      o_comp->set_type( type ).
      o_comp->set_name( name ).
      o_comp->set_devclass( directory-devclass ).
      o_comp->set_checksum( checksum ).
      o_comp->set_code_signature( code_signature ).
      TRY.
          o_comp->set_connuuid( connector ).
        CATCH zaplink_cx INTO o_cx.
* Ignore if Connector do not exists on this system
          IF o_cx->is_exception_text( zaplink_cx_connector=>invalid_uuid ) IS INITIAL.     RAISE EXCEPTION o_cx.    ENDIF.
      ENDTRY.
    CATCH zaplink_cx INTO o_cx.
      IF without_cx IS INITIAL.  RAISE EXCEPTION o_cx.    ENDIF.
  ENDTRY.
  endmethod.


  method GET_NAME.
  result = name.
  endmethod.


  method GET_TYPE.
  result = type.
  endmethod.


  method GET_TYPEKIND.
  CHECK NOT type IS INITIAL.
  result = zaplink_connectors=>get_typekind( type ).      " Issue 87
  endmethod.


  method REFRESH_DIRECTORY.
  DATA d_kind  TYPE td_transport_kind.
  d_kind = zaplink_connectors=>get_typekind( type ).
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF directory
    FROM tadir
    WHERE pgmid = d_kind
      AND object = type
      AND obj_name = name.
*  IF NOT o_comp->get_devclass( ) IS INITIAL.
*    directory-devclass = o_comp->get_devclass( ).
*  ENDIF.
  IF type = 'DEVC'. CLEAR directory-devclass. ENDIF.
*  devclass = directory-devclass.
  endmethod.


  method SET_CHECKSUM.
  DATA o_xml           TYPE to_xml.
  DATA _tempstring     TYPE string.
  DATA _len            TYPE i.
  DATA _key            TYPE string.

  o_xml = conv_to_xml( ).
  ASSERT o_xml IS BOUND.
  _tempstring = xml_2_string( o_xml ).
*  _regex = '<\?.*\?>'.
*  replace FIRST OCCURRENCE OF REGEX _regex IN _tempstring with ``.
  REPLACE FIRST OCCURRENCE OF REGEX '<\?.*\?>' IN _tempstring WITH ``.
  checksum = zaplink_tools=>calculate_md5_hash( _tempstring ).
  CONCATENATE type '/' name INTO _key.
  LOG-POINT ID zaplink  SUBKEY _key FIELDS checksum _tempstring.
  endmethod.


  method SET_COMPONENT.
  CHECK o_comp IS BOUND.

  TRY.

    type = o_comp->get_type( ).
    name = o_comp->get_name( ).
    connector = o_comp->get_connuuid( ).
    refresh_directory( ).

    mac_def_catch zaplink_cx_container.
  ENDTRY.
  endmethod.


  method UNANONYMIZE.
*     me->DIRECTORY-korrnum,
  IF me->directory-srcsystem IS INITIAL. me->directory-srcsystem = sy-sysid. ENDIF.
  IF me->directory-author IS INITIAL. me->directory-author = sy-uname. ENDIF.
*    me->DIRECTORY-trkorr.
  endmethod.


  method UPDATE_CONNECTOR_DATA.
  me->connector = o_connector->uuid.
  me->version = o_connector->version.
  endmethod.


  method XML_2_STRING.
  DATA _streamfactory  TYPE REF TO if_ixml_stream_factory.
  DATA _outputstream   TYPE REF TO if_ixml_ostream.
  DATA _renderer       TYPE REF TO if_ixml_renderer.
  DATA _tempstring     TYPE string.
  DATA _printxmldoc    TYPE REF TO cl_xml_document.
  DATA _rc             TYPE sysubrc.

  check o_xml IS BOUND.

  _streamfactory = xmlengine->create_stream_factory( ).
  _outputstream = _streamfactory->create_ostream_cstring( _tempstring ).
  _renderer = xmlengine->create_renderer( document = o_xml
                                           ostream = _outputstream ).
  _renderer->set_normalizing( ).
  _rc = _renderer->render( ).
  CREATE OBJECT _printxmldoc.
  _rc = _printxmldoc->parse_string( _tempstring ).

  WHILE _tempstring(1) <> '<'.
    SHIFT _tempstring LEFT BY 1 PLACES.
  ENDWHILE.

  result = _tempstring.
  endmethod.
ENDCLASS.
