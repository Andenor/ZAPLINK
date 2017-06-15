class ZAPLINK_EASYXML definition
  public
  create public .

public section.
  type-pools ABAP .

  types TO_LOG type ref to ZAPLINK_MESSAGE_COLLECTOR .

  data APPLICATION_LOG type TO_LOG read-only .

  methods ADD_SELF
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !NAME type STRING optional
      !ANY type ANY .
  methods ADD_ANY
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !NAME type STRING optional
      !ANY type ANY
      !XMLDOC type ref to IF_IXML_DOCUMENT
      !TYPEDESC type ref to CL_ABAP_TYPEDESCR optional .
  methods ANY2XML
    importing
      !ANY type ANY
      !TYPE type STRING
      !ATTRIBUTS type ANY optional
    returning
      value(XMLDOC) type ref to IF_IXML_DOCUMENT
    raising
      ZAPLINK_CX .
  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  methods GET_ATTRIBUTS
    importing
      !XMLDOC type ref to IF_IXML_DOCUMENT
    changing
      !ATTRIBUTS type ANY
    raising
      ZAPLINK_CX .
  class-methods GET_REF_TYPE
    importing
      !TYPEDESC type ref to CL_ABAP_TYPEDESCR
    returning
      value(REF_TYPE) type ref to CL_ABAP_TYPEDESCR .
  class-methods NODE2STRING
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
    returning
      value(STRING) type STRING .
  methods RESET .
  methods XML2ANY
    importing
      !XMLDOC type ref to IF_IXML_DOCUMENT
    changing
      !ANY type ANY
      !ATTRIBUTS type ANY optional
    raising
      ZAPLINK_CX .
  methods _XML2ANY
    importing
      !XML type ref to IF_IXML_NODE
    changing
      !ANY type ANY
    raising
      ZAPLINK_CX .
  methods READ_ANY
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !TYPEDESC type ref to CL_ABAP_TYPEDESCR optional
    changing
      !ANY type ANY
    raising
      ZAPLINK_CX .
protected section.

  types TO_EXCEPTION type ref to ZAPLINK_CX .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .

  data IXML type ref to IF_IXML .

  methods ADD_OBJECT
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !NAME type STRING optional
      !OBJECT type ref to OBJECT
      !XMLDOC type ref to IF_IXML_DOCUMENT
      !TYPEDESC type ref to CL_ABAP_TYPEDESCR optional .
  methods ADD_STRUCTURE
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !NAME type STRING optional
      !STRUCTURE type DATA
      !XMLDOC type ref to IF_IXML_DOCUMENT .
  methods ADD_TABLE
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !NAME type STRING optional
      !TABLE type ANY TABLE
      !XMLDOC type ref to IF_IXML_DOCUMENT .
  methods ELEM_2_STRING
    importing
      !ANY type ANY
      !TYPE_DESC type ref to CL_ABAP_TYPEDESCR optional
    returning
      value(STRING) type STRING .
  class-methods IS_SIMPLE_TYPE_KIND
    importing
      !TYPE_KIND type ABAP_TYPEKIND
    returning
      value(RESULT) type ABAP_BOOL .
  methods READ_OBJECT
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
      !TYPEDESC type ref to CL_ABAP_TYPEDESCR optional
    changing
      !OBJECT type ref to OBJECT
    raising
      ZAPLINK_CX .
  methods READ_STRUCTURE
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
    changing
      !STRUCTURE type DATA
    raising
      ZAPLINK_CX .
  methods READ_TABLE
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
    changing
      !TABLE type ANY TABLE
    raising
      ZAPLINK_CX .
  methods STACK
    importing
      !ITEM type STRING .
  methods STACK_DESCR
    importing
      !DESC type ref to CL_ABAP_TYPEDESCR .
  methods STRING_2_ELEM
    importing
      !TYPE_DESC type ref to CL_ABAP_TYPEDESCR optional
      value(STRING) type STRING
    changing
      !ANY type ANY .
  methods UNSTACK .
  methods GET_NODE_TEXT
    importing
      !XML_NODE type ref to IF_IXML_ELEMENT
    returning
      value(RESULT) type STRING .
  class-methods NODE_IS_NOT_TYPE
    importing
      !NODE type ref to IF_IXML_NODE
      !TYPE type I
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods NODE_IS_TYPE
    importing
      !NODE type ref to IF_IXML_NODE
      !TYPE type I
    returning
      value(RESULT) type ABAP_BOOL .
private section.

  types:
    BEGIN OF ts_ele,
    select    TYPE flag,
    elem_type	TYPE trobjtype,
    elem_key  TYPE elemgenkey,
    td_pack  	TYPE devclass,
    elem_pack	TYPE devclass,
  END OF ts_ele .
  types:
    tt_ele TYPE STANDARD TABLE OF ts_ele WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_itf_el,
    interface	TYPE REF TO if_package_interface,
    elements  TYPE STANDARD TABLE OF scomeldtln WITH NON-UNIQUE DEFAULT KEY, "SCOMELDATA,
    acls      TYPE STANDARD TABLE OF scomaclstr WITH NON-UNIQUE DEFAULT KEY, "SCOMACLTAB,
  END OF ts_itf_el .
  types:
    tt_itf_el TYPE STANDARD TABLE OF ts_itf_el WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_context TYPE STANDARD TABLE OF string WITH DEFAULT KEY .

  constants EN_LINE type STRING value 'ITEM' ##NO_TEXT.
  constants MSGID type SYMSGID value 'ZAPLINK_EASYXML' ##NO_TEXT.
  data O_MYCX type TO_EXCEPTION .
  data BAL_LEVEL type BALLEVEL .
  data RC type I .
  data CRC type STRING .
  constants SELF_FIELDNAME type STRING value '_' ##NO_TEXT.
  data CONTEXT type TT_CONTEXT .
  constants ISSUE46_PREFIX type ABAP_CHAR1 value '_' ##NO_TEXT.
  class-data ISSUE46_REGEX type STRING .
  constants PROTECTION_CHAR type ABAP_CHAR1 value '`' ##NO_TEXT.
  class-data:
    char_to_protect TYPE RANGE OF ABAP_CHAR1 .

  class-methods PROTECT_STRING
    importing
      !STRING type STRING
    returning
      value(RESULT) type STRING .
  class-methods UNPROTECT_STRING
    importing
      !STRING type STRING
    returning
      value(RESULT) type STRING .
ENDCLASS.



CLASS ZAPLINK_EASYXML IMPLEMENTATION.


  method ADD_ANY.
  DATA _desc     TYPE REF TO cl_abap_typedescr.
  DATA svalue    TYPE string.
  FIELD-SYMBOLS: <any> TYPE ANY.

  CHECK any IS NOT INITIAL.         " prevent from do anything for an empty field
  IF typedesc IS BOUND.
    _desc = typedesc.
  ELSE.
    _desc = cl_abap_typedescr=>describe_by_data( p_data = any ).
  ENDIF.

  _desc = get_ref_type( _desc ).
  stack_descr( _desc ).
  CASE _desc->kind.
    WHEN cl_abap_datadescr=>kind_ref.
      IF _desc->type_kind = cl_abap_refdescr=>typekind_dref.   " Data Ref
        ASSIGN any->* TO <any>.
        IF sy-subrc = 0.        " Ignore dref {A:INITIAL}
          _desc = cl_abap_typedescr=>describe_by_data( p_data = <any> ).
          add_any(   xmldoc = xmldoc
                   xml_node = xml_node
                       name = name
                        any = <any>
                   typedesc = _desc ).
        ENDIF.
      ELSE.
* Unexpected => Ignored
        mac_syserr_log -1 'cl_abap_datadescr=>kind_ref' 'ZAPLINK_EASYXML->ADD_ANY'.
      ENDIF.
    WHEN cl_abap_datadescr=>kind_elem.
      svalue = elem_2_string( any = any
                        type_desc = _desc ).
      IF NOT svalue IS INITIAL.
        IF name IS INITIAL.
          rc = xml_node->set_value( svalue ).
          mac_syserr_log rc 'set_value' 'ZAPLINK_EASYXML->ADD_ANY'.
        ELSE.
          rc = xml_node->set_attribute( name = name
                                       value = svalue ).
          mac_syserr_log rc 'set_attribute' 'ZAPLINK_EASYXML->ADD_ANY'. "#EC NOTEXT
        ENDIF.
      ENDIF.
    WHEN cl_abap_datadescr=>kind_struct.
      add_structure(   xmldoc = xmldoc
                     xml_node = xml_node
                         name = name
                    structure = any ).
    WHEN cl_abap_datadescr=>kind_table.
      add_table(   xmldoc = xmldoc
                 xml_node = xml_node
                     name = name
                    table = any ).
    WHEN cl_abap_datadescr=>kind_class.
      add_object(   xmldoc = xmldoc
                  xml_node = xml_node
                      name = name
                    object = any
                  typedesc = _desc ).
    WHEN cl_abap_datadescr=>kind_intf.
* To be ignored
      svalue = _desc->get_relative_name( ).
*      IF NOT application_log IS BOUND. MESSAGE ID MSGID TYPE 'E' NUMBER 001 WITH abap_true abap_true. ENDIF.
* Assuming Application_log is bound
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE ID msgid TYPE 'E' NUMBER 001 WITH name svalue. ENDIF.
      SET EXTENDED CHECK ON.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '001'
          id_msgv1     = name
          id_msgv2     = svalue
*          id_msgv3     = &3
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
  ENDCASE.
  unstack( ).
  endmethod.


  method ADD_OBJECT.
  DATA node      TYPE REF TO if_ixml_element.
  DATA o_obj     TYPE REF TO cl_abap_classdescr.
  DATA typedescr TYPE REF TO cl_abap_typedescr.
  DATA svalue    TYPE string.
  DATA sname     TYPE string.
  DATA attr_name TYPE string.
  DATA self_done TYPE flag.
  FIELD-SYMBOLS:
    <d> TYPE ANY,
    <any> TYPE ANY,
    <a> LIKE LINE OF o_obj->attributes.

  IF NOT object IS BOUND. EXIT. ENDIF.

* Issue 26 : describe_by_object_ref all the time
*  IF typedesc IS BOUND.
*    o_obj ?= typedesc.
*  ELSE.
*    o_obj ?= cl_abap_classdescr=>describe_by_object_ref( p_object_ref = object ).
*  ENDIF.
*  IF o_obj->absolute_name = '\CLASS=OBJECT'.
  o_obj ?= cl_abap_classdescr=>describe_by_object_ref( p_object_ref = object ).
* Replace in stack '\CLASS=YYYY' => '\CLASS=XXXX'
  unstack( ).
  attr_name = o_obj->absolute_name.
  stack( attr_name ).
*  ENDIF.

  IF name IS INITIAL.
    node = xml_node.
  ELSE.
    node = xmldoc->create_element( name ).
  ENDIF.

* set value before adding node or they will be lost
  READ TABLE o_obj->attributes TRANSPORTING NO FIELDS
       WITH KEY name = self_fieldname.
  IF sy-subrc = 0.
    ASSIGN object->(self_fieldname) TO <d>.
    IF sy-subrc <> 0.
*Error handling
      mac_syserr_log sy-subrc 'ASSIGN COMPONENT <c>-name OF STRUCTURE structure' 'ZAPLINK_EASYXML->ADD_OBJECT'. "#EC NOTEXT
    ELSE.
      add_self( xml_node = node
                    name = name
                    any = <d> ).
      self_done = abap_true.
    ENDIF.
  ENDIF.

  LOOP AT o_obj->attributes ASSIGNING <a>
    WHERE ( visibility = cl_abap_classdescr=>public OR
            visibility = cl_abap_classdescr=>protected )  " May be friend of the class
      AND is_constant IS INITIAL  " Do not handle constants
      AND is_class    IS INITIAL  " Do not handle class attribute
      AND alias_for   IS INITIAL
      AND is_virtual  IS INITIAL.

    ASSIGN object->(<a>-name) TO <d>.
    IF sy-subrc <> 0.
* error handling
      mac_syserr_log sy-subrc 'ASSIGN object->(<a>-name)' 'ZAPLINK_EASYXML->ADD_OBJECT'. "#EC NOTEXT
      CONTINUE.
    ENDIF.
    IF <a>-name = self_fieldname AND self_done = abap_true.     CONTINUE.   ENDIF.
    attr_name = <a>-name.
    stack( attr_name ).
    add_any(   xmldoc = xmldoc
             xml_node = node
                 name = attr_name
                  any = <d> ).
    unstack( ).
  ENDLOOP.

  IF node IS BOUND AND NOT name IS INITIAL.
    rc = xml_node->append_child( node ).
    mac_syserr_log rc 'append_child' 'ZAPLINK_EASYXML->ADD_OBJECT'. "#EC NOTEXT
  ENDIF.
  endmethod.


  method ADD_SELF.
  DATA svalue    TYPE string.
  DATA typedescr TYPE REF TO cl_abap_typedescr.
  FIELD-SYMBOLS: <any> TYPE ANY.

  typedescr = cl_abap_typedescr=>describe_by_data( any ).
  typedescr = get_ref_type( typedescr ).
  IF typedescr->kind = cl_abap_datadescr=>kind_ref.
    ASSIGN any->* TO <any>.
    typedescr = cl_abap_typedescr=>describe_by_data( p_data = <any> ).
  ENDIF.
  CASE typedescr->kind.
    WHEN cl_abap_datadescr=>kind_elem.
      svalue = elem_2_string( any = any
                        type_desc = typedescr ).
      IF svalue IS NOT INITIAL.
        svalue = protect_string( svalue ).                  " Issue 61
        rc = xml_node->set_value( svalue ).
        mac_syserr_log rc 'set_value' 'ZAPLINK_EASYXML->ADD_SELF'. "#EC NOTEXT
      ENDIF.
    WHEN OTHERS.
      svalue = typedescr->get_relative_name( ).
* Self field '&1' of &2 (&3) must be simple data type, not '&4'
      IF NOT application_log IS BOUND. MESSAGE E004 WITH self_fieldname name svalue typedescr->kind. ENDIF.
      CALL METHOD application_log->add_warning
        EXPORTING
*              id_msgid     = MSGID
          id_msgno     = 004
          id_msgv1     = self_fieldname
          id_msgv2     = name
          id_msgv3     = svalue
          id_msgv4     = typedescr->kind
*              id_detlevel  =
*              id_probclass =
          .
  ENDCASE.
  endmethod.


  method ADD_STRUCTURE.
  DATA node         TYPE REF TO if_ixml_element.
  DATA sname        TYPE string.
  DATA svalue       TYPE string.
  DATA structdescr  TYPE REF TO cl_abap_structdescr.
  DATA typedescr    TYPE REF TO cl_abap_typedescr.
  DATA self_done    TYPE flag.
  FIELD-SYMBOLS:
    <fieldvalue> TYPE ANY,
    <fld> TYPE ANY,
    <any> TYPE ANY,
    <c> LIKE LINE OF structdescr->components.

  IF structure IS INITIAL. EXIT. ENDIF.

  IF name IS INITIAL.
    node = xml_node.
  ELSE.
    node = xmldoc->create_element( name ).
  ENDIF.

  structdescr ?= cl_abap_structdescr=>describe_by_data( structure ).
* set value before adding node or they will be lost
  READ TABLE structdescr->components TRANSPORTING NO FIELDS
       WITH KEY name = self_fieldname.
  IF sy-subrc = 0.
    ASSIGN COMPONENT self_fieldname OF STRUCTURE structure TO <fieldvalue>.
    IF sy-subrc <> 0.
*Error handling
      mac_syserr_log sy-subrc 'ASSIGN COMPONENT <c>-name OF STRUCTURE structure' 'ZAPLINK_EASYXML->ADD_STRUCTURE'. "#EC NOTEXT
    ELSE.
      add_self( xml_node = node
                    name = name
                     any = <fieldvalue> ).
      self_done = abap_true.
    ENDIF.
  ENDIF.
  LOOP AT structdescr->components ASSIGNING <c>.
    ASSIGN COMPONENT <c>-name OF STRUCTURE structure TO <fieldvalue>.
    IF sy-subrc <> 0.
*Error handling
      mac_syserr_log sy-subrc 'ASSIGN COMPONENT <c>-name OF STRUCTURE structure' 'ZAPLINK_EASYXML->ADD_STRUCTURE'. "#EC NOTEXT
      CONTINUE.
    ENDIF.
    IF <c>-name = self_fieldname AND self_done = abap_true.     CONTINUE.   ENDIF.

    typedescr = cl_abap_typedescr=>describe_by_data( <fieldvalue> ).
    typedescr = get_ref_type( typedescr ).
    sname = <c>-name.

* Issue 46 : Field names strating with numbre in attributes
    IF sname(1) CO '0123456789' AND typedescr->kind = cl_abap_datadescr=>kind_elem.
      CONCATENATE issue46_prefix sname INTO sname. UNASSIGN <fld>. ASSIGN COMPONENT sname OF STRUCTURE structure TO <fld>.
      WHILE <fld> IS ASSIGNED. CONCATENATE issue46_prefix sname INTO sname. UNASSIGN <fld>. ASSIGN COMPONENT sname OF STRUCTURE structure TO <fld>. ENDWHILE.
    ENDIF.

    stack( sname ).
    add_any( xmldoc = xmldoc
           xml_node = node
               name = sname
                any = <fieldvalue>
           typedesc = typedescr ).
    unstack( ).
  ENDLOOP.

  IF node IS BOUND AND NOT name IS INITIAL.
    rc = xml_node->append_child( node ).
    mac_syserr_log rc 'append_child' 'ZAPLINK_EASYXML->ADD_STRUCTURE'. "#EC NOTEXT
  ENDIF.
  endmethod.


  method ADD_TABLE.
  DATA t_node        TYPE REF TO if_ixml_element.
  DATA i_node        TYPE REF TO if_ixml_element.
  FIELD-SYMBOLS:
    <l> TYPE ANY.

  IF table IS INITIAL. EXIT. ENDIF.

  IF name IS INITIAL.
    t_node = xml_node.
  ELSE.
    t_node = xmldoc->create_element( name ).
  ENDIF.
  ASSERT t_node IS BOUND.

  LOOP AT table ASSIGNING <l>.
    i_node = xmldoc->create_element( en_line ).
    add_any(   xmldoc = xmldoc
             xml_node = i_node
*                 name = EN_LINE
                  any = <l> ).
    t_node->append_child( i_node ).
  ENDLOOP.

  IF NOT name IS INITIAL.
    rc = xml_node->append_child( t_node ).
    mac_syserr_log rc 'append_child' 'ZAPLINK_EASYXML->ADD_TABLE'.
  ENDIF.
  endmethod.


  method ANY2XML.
  DATA xml_node  TYPE REF TO if_ixml_element.
  DATA _desc     TYPE REF TO cl_abap_typedescr.
  DATA string    TYPE string.

  xmldoc = ixml->create_document( ).
  xml_node = xmldoc->create_element( type ). stack( type ).

  IF attributs IS SUPPLIED.
    _desc = cl_abap_typedescr=>describe_by_data( p_data = attributs ).
    _desc = get_ref_type( _desc ).
    stack_descr( _desc ).
    CASE _desc->kind.
      WHEN cl_abap_datadescr=>kind_ref.
* Unexpected => Ignored
        mac_syserr_log -1 'cl_abap_datadescr=>kind_ref' 'ZAPLINK_EASYXML->ANY2XML'.
      WHEN cl_abap_datadescr=>kind_struct.
        add_structure(   xmldoc = xmldoc
                       xml_node = xml_node
*                         name = name
                      structure = attributs ).
        " cl_abap_datadescr=>kind_elem.
        " cl_abap_datadescr=>kind_table.
        " cl_abap_datadescr=>kind_class.
      WHEN OTHERS. " cl_abap_datadescr=>kind_intf.
* To be ignored
        string = _desc->get_relative_name( ).
        MESSAGE ID MSGID TYPE 'X' NUMBER 001 WITH string 'ATTRIBUTES'.
    ENDCASE.
    unstack( ).
  ENDIF.
*  IF NOT name IS INITIAL. xml_node->set_attribute( name = tn_name value = name ). ENDIF.
*  IF NOT version IS INITIAL.
*    rc = xml_node->set_attribute( name = tn_version value = version ).
*    mac_syserr_log rc 'set_attribute' 'ZAPLINK_EASYXML->ANY2XML'.
*    IF rc <> 0.
*      EXIT.
*    ENDIF.
*  ENDIF.

  add_any( xmldoc = xmldoc
         xml_node = xml_node
*             name =
              any = any ).

  rc = xmldoc->append_child( xml_node ).
  mac_syserr_log rc 'append_child' 'ZAPLINK_EASYXML->ANY2XML'.

  IF application_log->has_messages_of_msgt( id_msgty     = 'E' ) IS INITIAL.
* Successfull conversion data to XML.
    IF NOT application_log IS BOUND. MESSAGE ID MSGID TYPE 'S' NUMBER 006. ENDIF.
    CALL METHOD application_log->add_success
      EXPORTING
*          id_msgid     = MSGID
        id_msgno     = '006'
*          id_msgv1     =
*          id_msgv2     =
*          id_msgv3     =
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
        .
  ENDIF.
  unstack( ).
  endmethod.


  method CLASS_CONSTRUCTOR.
*  DATA: obj TYPE balobj.
*  DATA: sobj TYPE balsub.
* Issue 61 : Workaround
  DATA s_char LIKE LINE OF char_to_protect.
  s_char-sign = 'I'.    s_char-option = 'EQ'.
  s_char-low = space.                                 APPEND s_char TO char_to_protect.
  s_char-low = CL_ABAP_CHAR_UTILITIES=>NEWLINE.       APPEND s_char TO char_to_protect.
  s_char-low = CL_ABAP_CHAR_UTILITIES=>CR_LF.         APPEND s_char TO char_to_protect.
  s_char-low = CL_ABAP_CHAR_UTILITIES=>FORM_FEED.     APPEND s_char TO char_to_protect.

* Issue 46 : '^[_]*'
  CONCATENATE '^[' issue46_prefix ']*' INTO issue46_regex.

* Issue 56 : BAL Log object are set in ZAPLINK_MESSAGE_COLLECTOR=>CHECK_CUSTO( )
*<?xml version="1.0" encoding="utf-16"?>
* <TABU TABNAME="BALOBJ">
*  <DataRow OBJECT="ZAPLINK"/>
* </TABU>
* <TABU TABNAME="BALOBJT">
*  <DataRow SPRAS="D" OBJECT="ZAPLINK" OBJTXT="SAP Link - Logs"/>
*  <DataRow SPRAS="E" OBJECT="ZAPLINK" OBJTXT="SAP Link - Logs"/>
*  <DataRow SPRAS="F" OBJECT="ZAPLINK" OBJTXT="SAP Link - Logs"/>
* </TABU>
* <TABU TABNAME="BALSUB">
*  <DataRow OBJECT="ZAPLINK" SUBOBJECT="DEVC"/>
* </TABU>
* <TABU TABNAME="BALSUBT">
*  <DataRow SPRAS="D" OBJECT="ZAPLINK" SUBOBJECT="DEVC" SUBOBJTXT="Devlopment class"/>
*  <DataRow SPRAS="E" OBJECT="ZAPLINK" SUBOBJECT="DEVC" SUBOBJTXT="Devlopment class"/>
*  <DataRow SPRAS="F" OBJECT="ZAPLINK" SUBOBJECT="DEVC" SUBOBJTXT="Devlopment class"/>
* </TABU>
** Check BALOBJ
*  obj-object = zaplink_datatypes=>ballog_object.
*  SELECT SINGLE * INTO obj
*    FROM balobj
*    WHERE object = obj-object.
*  IF sy-subrc <> 0.
*    INSERT balobj FROM obj.
*  ENDIF.
*
** Check BALSUB
*  sobj-object = obj-object.
*  sobj-subobject = 'EASYXML'.
*  SELECT SINGLE * INTO sobj
*    FROM balsub
*    WHERE    object = sobj-object
*      AND subobject = sobj-subobject.
*  IF sy-subrc <> 0.
*    INSERT balsub FROM sobj.
*  ENDIF.
*  IF sy-subrc <> 0.
*    COMMIT WORK AND WAIT.
*  ENDIF.
  endmethod.


  method CONSTRUCTOR.
  ixml = cl_ixml=>create( ).
  reset( ).
  endmethod.


  method ELEM_2_STRING.
  DATA _desc     TYPE REF TO cl_abap_typedescr.
  DATA exception TYPE REF TO cx_root.
  DATA l_convin  TYPE REF TO cl_abap_conv_in_ce.

  IF any IS INITIAL.
    EXIT.
  ENDIF.

  IF type_desc IS BOUND.
    _desc = type_desc.
  ELSE.
    _desc = cl_abap_typedescr=>describe_by_data( p_data = any ).
  ENDIF.

  IF _desc->kind <> cl_abap_datadescr=>kind_elem. EXIT. ENDIF.

  CASE _desc->type_kind.
    WHEN cl_abap_typedescr=>typekind_xstring
      OR cl_abap_typedescr=>typekind_xsequence.
      TRY.
*          CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
          l_convin = cl_abap_conv_in_ce=>create( encoding = 'UTF-8'
                                                    input = any ).
          CALL METHOD l_convin->read
            IMPORTING
              data = string.
        CATCH cx_root INTO exception.
          application_log->add_exception( exception = exception ).
      ENDTRY.
    WHEN cl_abap_typedescr=>typekind_char
      OR cl_abap_typedescr=>typekind_clike
      OR cl_abap_typedescr=>typekind_date
      OR cl_abap_typedescr=>typekind_time
      OR cl_abap_typedescr=>typekind_string
      OR cl_abap_typedescr=>typekind_csequence
      OR cl_abap_typedescr=>typekind_num
      OR cl_abap_typedescr=>typekind_numeric
      OR cl_abap_typedescr=>typekind_int
      OR cl_abap_typedescr=>typekind_int1
      OR cl_abap_typedescr=>typekind_int2
      OR cl_abap_typedescr=>typekind_float
      OR cl_abap_typedescr=>typekind_packed
      OR cl_abap_typedescr=>typekind_simple
      OR cl_abap_typedescr=>typekind_hex        " Convert Hex to String works
        .
      TRY.
          string = any.
        CATCH cx_root INTO exception.
          application_log->add_exception( exception ).
      ENDTRY.
    WHEN OTHERS.
* Unexpected => Dump
      mac_syserr_log -1 'WHEN OTHERS' 'ZAPLINK_EASYXML->ELEM_2_STRING'.
*    WHEN cl_abap_typedescr=>typekind_class
*      OR cl_abap_typedescr=>typekind_struct1
*      OR cl_abap_typedescr=>typekind_struct2
*      OR cl_abap_typedescr=>typekind_table
*      .
*
*    WHEN cl_abap_typedescr=>typekind_any.
*    WHEN cl_abap_typedescr=>typekind_data.
*    WHEN cl_abap_typedescr=>typekind_dref.
*    WHEN cl_abap_typedescr=>typekind_intf.
*    WHEN cl_abap_typedescr=>typekind_iref.
*    WHEN cl_abap_typedescr=>typekind_oref.
*    WHEN cl_abap_typedescr=>typekind_w.
  ENDCASE.
  endmethod.


  method GET_ATTRIBUTS.
  DATA root_node   TYPE REF TO if_ixml_element.
  DATA _desc     TYPE REF TO cl_abap_typedescr.
  DATA string    TYPE string.

  root_node = xmldoc->get_root_element( ).

  IF root_node IS BOUND.

    _desc = cl_abap_typedescr=>describe_by_data( p_data = attributs ).
    _desc = get_ref_type( _desc ).
    stack_descr( _desc ).
    CASE _desc->kind.
      WHEN cl_abap_datadescr=>kind_ref.
* Unexpected => Ignored
        mac_syserr_log -1 'cl_abap_datadescr=>kind_ref' 'ZAPLINK_EASYXML->ANY2XML'.
      WHEN cl_abap_datadescr=>kind_struct.
        read_structure( EXPORTING
                         xml_node = root_node
                         CHANGING
                        structure = attributs ).
        " cl_abap_datadescr=>kind_elem.
        " cl_abap_datadescr=>kind_table.
        " cl_abap_datadescr=>kind_class.
      WHEN OTHERS. " cl_abap_datadescr=>kind_intf.
* To be ignored
        string = _desc->get_relative_name( ).
        MESSAGE ID msgid TYPE 'X' NUMBER 001 WITH string 'ATTRIBUTES'.
    ENDCASE.
    unstack( ).
  ENDIF.
  endmethod.


  method GET_NODE_TEXT.
  DATA o_filter   TYPE REF TO if_ixml_node_filter.
  DATA o_iterator TYPE REF TO if_ixml_node_iterator.
  DATA o_text     TYPE REF TO if_ixml_node.

* XML Value
  check xml_node->contains( if_ixml_node=>co_node_text ) = abap_true.    " value returned by get_value is the right one.
  o_filter = xml_node->create_filter_node_type( if_ixml_node=>co_node_text ).
  o_iterator = xml_node->create_iterator_filtered( depth = 1
                                                  filter = o_filter ).
  o_text = o_iterator->get_next( ).   result = o_text->get_value( ).    IF result = '#'. clear result. endif.
  endmethod.


  method GET_REF_TYPE.
  DATA o_ref      TYPE REF TO cl_abap_refdescr.
  FIELD-SYMBOLS <a> TYPE ANY.

  ref_type = typedesc.
  IF typedesc->kind = cl_abap_datadescr=>kind_ref.
    o_ref ?= typedesc.
    IF o_ref->type_kind = cl_abap_refdescr=>typekind_oref.    " Object
      ref_type = o_ref->get_referenced_type( ).
    ENDIF.
  ENDIF.
*  DATA o_ref      TYPE REF TO cl_abap_refdescr.
*  DATA _desc      TYPE REF TO cl_abap_typedescr.
*  FIELD-SYMBOLS <a> TYPE ANY.
*
*  IF typedesc IS BOUND.
*    _desc = typedesc.
*  ELSE.
*    _desc = cl_abap_typedescr=>describe_by_data( p_data = data ).
*  ENDIF.
*
*  IF _desc->kind = cl_abap_datadescr=>kind_ref.
*    o_ref ?= _desc.
*    IF o_ref->type_kind = cl_abap_refdescr=>typekind_oref.    " Object
*      ref_type = o_ref->get_referenced_type( ).
*    ELSEIF o_ref->type_kind = cl_abap_refdescr=>typekind_dref.   " Data Ref
*      ASSIGN data->* TO <a>.
*      ref_type = cl_abap_typedescr=>describe_by_data( <a> ).
*    ELSE.
*      ASSERT 0 = 1.
*    ENDIF.
*  ELSE.
*    ref_type = typedesc.
*  ENDIF.
  endmethod.


  method IS_SIMPLE_TYPE_KIND.
  result = '?'.  " unknow

  CASE TYPE_KIND.
*          WHEN cl_abap_typedescr=>typekind_clike.
*          WHEN cl_abap_typedescr=>typekind_date.
*          WHEN cl_abap_typedescr=>typekind_time.
*          WHEN cl_abap_typedescr=>typekind_string.

*          WHEN cl_abap_typedescr=>typekind_int.
*          WHEN cl_abap_typedescr=>typekind_int1.
*          WHEN cl_abap_typedescr=>typekind_int2.

*          WHEN cl_abap_typedescr=>typekind_num.
*          WHEN cl_abap_typedescr=>typekind_numeric.

*          WHEN cl_abap_typedescr=>typekind_float.
*          WHEN cl_abap_typedescr=>typekind_packed.
*          WHEN cl_abap_typedescr=>typekind_simple.

    WHEN cl_abap_typedescr=>typekind_char
      OR cl_abap_typedescr=>typekind_clike
      OR cl_abap_typedescr=>typekind_date
      OR cl_abap_typedescr=>typekind_time
      OR cl_abap_typedescr=>typekind_string
      OR cl_abap_typedescr=>typekind_xstring
      OR cl_abap_typedescr=>typekind_num
      OR cl_abap_typedescr=>typekind_numeric
      OR cl_abap_typedescr=>typekind_int
      OR cl_abap_typedescr=>typekind_int1
      OR cl_abap_typedescr=>typekind_int2
      OR cl_abap_typedescr=>typekind_float
      OR cl_abap_typedescr=>typekind_packed
      OR cl_abap_typedescr=>typekind_simple
        .
     result = abap_true.
    WHEN cl_abap_typedescr=>typekind_class
      or cl_abap_typedescr=>typekind_struct1
      or cl_abap_typedescr=>typekind_struct2
      or cl_abap_typedescr=>typekind_table
      .
     result = abap_false.

    WHEN cl_abap_typedescr=>typekind_any.
    WHEN cl_abap_typedescr=>typekind_csequence.
    WHEN cl_abap_typedescr=>typekind_data.
    WHEN cl_abap_typedescr=>typekind_dref.
    WHEN cl_abap_typedescr=>typekind_hex.
    WHEN cl_abap_typedescr=>typekind_intf.
    WHEN cl_abap_typedescr=>typekind_iref.
    WHEN cl_abap_typedescr=>typekind_oref.
    WHEN cl_abap_typedescr=>typekind_w.
    WHEN cl_abap_typedescr=>typekind_xsequence.
  ENDCASE.
  endmethod.


  method NODE2STRING.
  DATA _ixml TYPE REF TO if_ixml.
  DATA _streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA _outputstream TYPE REF TO if_ixml_ostream.
  DATA _printxmldoc TYPE REF TO cl_xml_document.

  _ixml = cl_ixml=>create( ).
  _streamfactory = _ixml->create_stream_factory( ).
  _outputstream = _streamfactory->create_ostream_cstring( string ).
  CALL METHOD xml_node->render
    EXPORTING
      ostream   = _outputstream.
  CREATE OBJECT _printxmldoc.
  _printxmldoc->parse_string( string ).
  endmethod.


  method NODE_IS_NOT_TYPE.
  DATA d_mask(8)  TYPE x.
  DATA d_txt_m(8) TYPE x.

  check node is bound.
  d_mask = node->get_type( ).   d_txt_m = type.
  d_mask = d_mask BIT-AND d_txt_m.
  if d_mask = 00000000.   result = abap_true.   endif.
  endmethod.


  method NODE_IS_TYPE.
  DATA d_mask(8)  TYPE x.
  DATA d_txt_m(8) TYPE x.

  check node is bound.
  d_mask = node->get_type( ).   d_txt_m = type.
  d_mask = d_mask BIT-AND d_txt_m.
  if d_mask = d_txt_m.    result = abap_true.   endif.
  endmethod.


  method PROTECT_STRING.
  DATA d_last TYPE i.
  CHECK NOT string IS INITIAL.
  d_last = strlen( string ) - 1.
  IF NOT string(1) IN char_to_protect.
    result = string.
  ELSE.
    CONCATENATE protection_char string INTO result.
  ENDIF.
  IF string+d_last(1) IN char_to_protect.   CONCATENATE result protection_char INTO result.   ENDIF.    " Issue 61 & 68
  endmethod.


  method READ_ANY.
  DATA _desc     TYPE REF TO cl_abap_typedescr.
  DATA string    TYPE string.
  DATA name      TYPE string.
  DATA object    TYPE REF TO object.
  FIELD-SYMBOLS: <any> TYPE ANY.

  IF typedesc IS BOUND.
    _desc = typedesc.
  ELSE.
    _desc = cl_abap_typedescr=>describe_by_data( p_data = any ).
  ENDIF.

  _desc = get_ref_type( _desc ).
  stack_descr( _desc ).
  CASE _desc->kind.
    WHEN cl_abap_datadescr=>kind_ref.
      IF _desc->type_kind = cl_abap_refdescr=>typekind_dref.   " Data Ref
        ASSIGN any->* TO <any>.
        IF sy-subrc = 0.        " already assigned
* Unexpected => Ignored
          mac_syserr_log -1 'cl_abap_datadescr=>kind_ref' 'ZAPLINK_EASYXML->READ_ANY'.
        ELSE.
*          TRY.
          CREATE DATA any TYPE REF TO if_ixml_element.
*            CATCH cx_sy_create_data_error.
*              mac_syserr_log -1 'cl_abap_datadescr=>kind_ref' 'ZAPLINK_EASYXML->READ_ANY'.
*          ENDTRY.
          ASSIGN any->* TO <any>.
          ASSERT sy-subrc = 0.
          <any> = xml_node.
        ENDIF.
      ELSE.
* Unexpected => Ignored
        mac_syserr_log -1 'cl_abap_datadescr=>kind_ref' 'ZAPLINK_EASYXML->READ_ANY'.
      ENDIF.
    WHEN cl_abap_datadescr=>kind_elem.
*      string = xml_node->get_attribute( name = tn_value ).
      string = xml_node->get_value( ).
      string_2_elem( EXPORTING
                        string = string
                    type_desc = _desc
                     CHANGING
                          any = any ).
    WHEN cl_abap_datadescr=>kind_struct.
      read_structure( EXPORTING
                       xml_node = xml_node
                       CHANGING
                      structure = any ).
    WHEN cl_abap_datadescr=>kind_table.
      read_table( EXPORTING
                   xml_node = xml_node
                   CHANGING
                      table = any ).
    WHEN cl_abap_datadescr=>kind_class.
      object = any.
      read_object( EXPORTING
                    xml_node = xml_node
                    typedesc = _desc
                    CHANGING
                      object = object ).
      IF any <> object.   any ?= object.    ENDIF.
    WHEN cl_abap_datadescr=>kind_intf.
* To be ignored
      string = _desc->get_relative_name( ).
      name = xml_node->get_name( ).
      IF NOT application_log IS BOUND. MESSAGE ID msgid TYPE 'E' NUMBER 001 WITH space space. ENDIF.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '001'
          id_msgv1     = name
          id_msgv2     = string
*          id_msgv3     = &3
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
  ENDCASE.
  unstack( ).
  endmethod.


  method READ_OBJECT.
*  DATA o_ref      TYPE REF TO cl_abap_refdescr.
  DATA elem       TYPE REF TO if_ixml_element.
  DATA next_node  TYPE REF TO if_ixml_node.
  DATA node       TYPE REF TO if_ixml_node.
  DATA o_obj      TYPE REF TO cl_abap_classdescr.
  DATA iterator   TYPE REF TO if_ixml_node_iterator.
  DATA childs     TYPE REF TO if_ixml_node_list.
  DATA attrs      TYPE REF TO if_ixml_named_node_map.
  DATA data_type  TYPE string.
  DATA attr_name  TYPE string.
  DATA name       TYPE string.
  DATA svalue     TYPE string.
  DATA typedescr  TYPE REF TO cl_abap_typedescr.
  DATA _cx        TYPE REF TO cx_root.
  FIELD-SYMBOLS <d> TYPE ANY.

  IF NOT object IS BOUND AND NOT typedesc IS BOUND. EXIT. ENDIF.
  IF NOT object IS BOUND.
    data_type = typedesc->get_relative_name( ).
    TRY.
        CREATE OBJECT object TYPE (data_type).
      CATCH cx_root INTO _cx.
        CALL METHOD application_log->add_exception
          EXPORTING
            exception    = _cx
*          id_detlevel  =
*          id_probclass =
            .
        EXIT.
    ENDTRY.
  ENDIF.

* Issue 26 : describe_by_object_ref all the time
*  IF typedesc IS BOUND.
*    o_obj ?= typedesc.
*  ELSE.
*    o_ref ?= cl_abap_classdescr=>describe_by_data( p_data = object ).
*    o_obj ?= get_ref_type( o_ref ).
*  ENDIF.
*  IF o_obj->absolute_name = '\CLASS=OBJECT'.
  o_obj ?= cl_abap_classdescr=>describe_by_object_ref( p_object_ref = object ).
* Replace in stack '\CLASS=YYYY' => '\CLASS=XXXX'
  unstack( ).
  attr_name = o_obj->absolute_name.
  stack( attr_name ).
*  ENDIF.

* XML Value
  svalue = get_node_text( xml_node ).                       " Issue 38
  IF NOT svalue IS INITIAL.
    svalue = unprotect_string( svalue ).                    " Issue 61
    ASSIGN object->(self_fieldname) TO <d>.
    IF sy-subrc <> 0.
* Error to raise
      mac_syserr_log sy-subrc 'ASSIGN object->(self_fieldname) TO <d>' 'ZAPLINK_EASYXML->READ_OBJECT'. "#EC NOTEXT
    ELSE.
* attribute assigned
      typedescr = cl_abap_typedescr=>describe_by_data( <d> ).
      typedescr = get_ref_type( typedescr ).
      CASE typedescr->kind.
        WHEN cl_abap_datadescr=>kind_ref.
* Exception ref of ref ?
          mac_syserr_log -1 'cl_abap_datadescr=>kind_ref (Value)' 'ZAPLINK_EASYXML->READ_OBJECT'. "#EC NOTEXT
        WHEN cl_abap_datadescr=>kind_elem.
          string_2_elem( EXPORTING
                            string = svalue
                        type_desc = typedescr
                         CHANGING
                              any = <d> ).
* cl_abap_datadescr=>kind_struct cl_abap_datadescr=>kind_table cl_abap_datadescr=>kind_class cl_abap_datadescr=>kind_intf.
        WHEN OTHERS.
          mac_syserr_log -1 'NOT cl_abap_datadescr=>kind_elem (Value)' 'ZAPLINK_EASYXML->READ_OBJECT'. "#EC NOTEXT
      ENDCASE.
    ENDIF.
  ENDIF.

* XML Attributs
  attrs = xml_node->get_attributes( ).
  iterator = attrs->create_iterator( ).
  next_node = iterator->get_next( ).
  WHILE next_node IS BOUND.
    node = next_node.    next_node ?= iterator->get_next( ).
    attr_name = node->get_name( ).
    READ TABLE o_obj->attributes TRANSPORTING NO FIELDS WITH KEY name = attr_name.
    IF sy-subrc <> 0.
* Component not found
      data_type = o_obj->get_relative_name( ).
      name = xml_node->get_name( ).
      IF NOT application_log IS BOUND. MESSAGE ID msgid TYPE 'E' NUMBER 003 WITH abap_true abap_true abap_true. ENDIF.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '003'
          id_msgv1     = attr_name
          id_msgv2     = name
          id_msgv3     = data_type
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
    ELSE.
      ASSIGN object->(attr_name) TO <d>.
      IF sy-subrc <> 0.
* Error to raise
        mac_syserr_log sy-subrc 'ASSIGN object->(attr_name) TO <d> (Attrs)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
      ELSE.
* attribute assigned
        typedescr = cl_abap_typedescr=>describe_by_data( <d> ).
        typedescr = get_ref_type( typedescr ).
        CASE typedescr->kind.
          WHEN cl_abap_datadescr=>kind_ref.
* Exception ref of ref ?
            mac_syserr_log -1 'cl_abap_datadescr=>kind_ref (Attrs)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
          WHEN cl_abap_datadescr=>kind_elem.
*            svalue = xml_node->get_attribute( name = attr_name ). " equivalent to =>
            svalue = node->get_value( ).
            string_2_elem( EXPORTING
                              string = svalue
                          type_desc = typedescr
                           CHANGING
                                any = <d> ).
* cl_abap_datadescr=>kind_struct cl_abap_datadescr=>kind_table cl_abap_datadescr=>kind_class cl_abap_datadescr=>kind_intf.
          WHEN OTHERS.
            TRY.
                elem ?= node.
                stack( attr_name ).
                read_any( EXPORTING
                           xml_node = elem
                           typedesc = typedescr
                           CHANGING
                                any = <d> ).
                unstack( ).
              CATCH cx_root INTO _cx.
                application_log->add_exception( exception = _cx ).
            ENDTRY.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDWHILE.

* XML Childs
  childs = xml_node->get_children( ).
  iterator = childs->create_iterator( ).
  next_node = iterator->get_next( ).
  WHILE next_node IS BOUND.
    node = next_node.    next_node ?= iterator->get_next( ).

    attr_name = node->get_name( ).
    CHECK node_is_not_type( node = node type = if_ixml_node=>co_node_text ) = abap_true. " not text node

    READ TABLE o_obj->attributes TRANSPORTING NO FIELDS WITH KEY name = attr_name.
    IF sy-subrc <> 0.
* Attributes not found
      data_type = o_obj->get_relative_name( ).
      name = xml_node->get_name( ).
      IF NOT application_log IS BOUND. MESSAGE ID msgid TYPE 'E' NUMBER 003 WITH abap_true abap_true abap_true. ENDIF.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '003'
          id_msgv1     = attr_name
          id_msgv2     = name
          id_msgv3     = data_type
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
    ELSE.
      ASSIGN object->(attr_name) TO <d>.
      IF sy-subrc <> 0.
* Error to raise
        mac_syserr_log sy-subrc 'ASSIGN object->(attr_name)' 'ZAPLINK_EASYXML->READ_OBJECT'. "#EC NOTEXT
      ELSE.
* attribute assigned
        typedescr = cl_abap_typedescr=>describe_by_data( <d> ).
        typedescr = get_ref_type( typedescr ).
        CASE typedescr->kind.
*          WHEN cl_abap_datadescr=>kind_ref.
** Exception ref of ref ?
*            mac_syserr_log -1 'cl_abap_datadescr=>kind_ref (Attrs)' 'ZAPLINK_EASYXML->READ_OBJECT'.
          WHEN cl_abap_datadescr=>kind_elem.
*            svalue = xml_node->get_attribute( name = attr_name ). " equivalent to =>
            svalue = node->get_value( ).
            string_2_elem( EXPORTING
                              string = svalue
                          type_desc = typedescr
                           CHANGING
                                any = <d> ).
* cl_abap_datadescr=>kind_struct cl_abap_datadescr=>kind_table cl_abap_datadescr=>kind_class cl_abap_datadescr=>kind_intf.
          WHEN OTHERS.
            TRY.
                elem ?= node.
                stack( attr_name ).
                read_any( EXPORTING
                           xml_node = elem
                           typedesc = typedescr
                           CHANGING
                                any = <d> ).
                unstack( ).
              CATCH cx_root INTO _cx.
                application_log->add_exception( exception = _cx ).
            ENDTRY.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDWHILE.
  endmethod.


  method READ_STRUCTURE.
  DATA elem        TYPE REF TO if_ixml_element.
  DATA node        TYPE REF TO if_ixml_node.
  DATA next_node   TYPE REF TO if_ixml_node.
  DATA structdescr TYPE REF TO cl_abap_structdescr.
  DATA typedescr   TYPE REF TO cl_abap_typedescr.
  DATA svalue      TYPE string.
  FIELD-SYMBOLS:
    <d> TYPE ANY,
    <c> LIKE LINE OF structdescr->components.
  DATA attrs      TYPE REF TO if_ixml_named_node_map.
  DATA iterator   TYPE REF TO if_ixml_node_iterator.
  DATA childs     TYPE REF TO if_ixml_node_list.
  DATA attr_name  TYPE string.
  DATA name       TYPE string.
  DATA data_type  TYPE string.
  DATA exception  TYPE REF TO cx_root.
*  DATA o_filter   TYPE REF TO if_ixml_node_filter.
*  DATA o_iterator TYPE REF TO if_ixml_node_iterator.
*  DATA o_text     TYPE REF TO if_ixml_node.

* XML Value
  svalue = get_node_text( xml_node ).                       " Issue 38
  IF NOT svalue IS INITIAL.
    svalue = unprotect_string( svalue ).                    " Issue 61
    ASSIGN COMPONENT self_fieldname OF STRUCTURE structure TO <d>.
    IF sy-subrc <> 0.
* Error to raise
      mac_syserr_log sy-subrc 'ASSIGN COMPONENT self_fieldname OF STRUCTURE structure (Value)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
    ELSE.
* attribute assigned
      typedescr = cl_abap_typedescr=>describe_by_data( <d> ).
      typedescr = get_ref_type( typedescr ).
      CASE typedescr->kind.
        WHEN cl_abap_datadescr=>kind_ref.
* Exception ref of ref ?
          mac_syserr_log -1 'cl_abap_datadescr=>kind_ref (Value)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
        WHEN cl_abap_datadescr=>kind_elem.
          string_2_elem( EXPORTING
                            string = svalue
                        type_desc = typedescr
                         CHANGING
                              any = <d> ).
* cl_abap_datadescr=>kind_struct cl_abap_datadescr=>kind_table cl_abap_datadescr=>kind_class cl_abap_datadescr=>kind_intf.
        WHEN OTHERS.
          mac_syserr_log -1 'NOT cl_abap_datadescr=>kind_elem (Value)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
      ENDCASE.
    ENDIF.
  ENDIF.

* XML Attributs
  structdescr ?= cl_abap_structdescr=>describe_by_data( structure ).
  attrs = xml_node->get_attributes( ).
  iterator = attrs->create_iterator( ).
  node = iterator->get_next( ).
  WHILE node IS BOUND.
    attr_name = node->get_name( ).    UNASSIGN <c>.
    READ TABLE structdescr->components ASSIGNING <c> WITH KEY name = attr_name.
    IF sy-subrc <> 0.
      REPLACE FIRST OCCURRENCE OF REGEX issue46_regex IN attr_name WITH ``.
      IF sy-subrc = 0 AND STRLEN( attr_name ) > 0.
        IF attr_name(1) CO '0123456789'.    " Issue 46 : Field names strating with numbre in attributes
          READ TABLE structdescr->components ASSIGNING <c> WITH KEY name = attr_name.
        ENDIF.
      ENDIF.
      IF NOT <c> IS ASSIGNED.   attr_name = node->get_name( ).    endif.
    ENDIF.
    IF NOT <c> IS ASSIGNED.
* Component not found
      data_type = structdescr->get_relative_name( ).
      name = xml_node->get_name( ).
      IF NOT application_log IS BOUND. MESSAGE ID msgid TYPE 'E' NUMBER 003 WITH abap_true abap_true abap_true. ENDIF.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '003'
          id_msgv1     = attr_name
          id_msgv2     = name
          id_msgv3     = data_type
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
    ELSE.
      ASSIGN COMPONENT attr_name OF STRUCTURE structure TO <d>.
      IF sy-subrc <> 0.
* Error to raise
        mac_syserr_log sy-subrc 'ASSIGN COMPONENT attr_name OF STRUCTURE structure (Attrs)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
      ELSE.
* attribute assigned
        typedescr = cl_abap_typedescr=>describe_by_data( <d> ).
        typedescr = get_ref_type( typedescr ).
        CASE typedescr->kind.
          WHEN cl_abap_datadescr=>kind_ref.
* Exception ref of ref ?
            mac_syserr_log -1 'cl_abap_datadescr=>kind_ref (Attrs)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
          WHEN cl_abap_datadescr=>kind_elem.
*            svalue = xml_node->get_attribute( name = attr_name ). " equivalent to =>
            svalue = node->get_value( ).
            string_2_elem( EXPORTING
                              string = svalue
                          type_desc = typedescr
                           CHANGING
                                any = <d> ).
* cl_abap_datadescr=>kind_struct cl_abap_datadescr=>kind_table cl_abap_datadescr=>kind_class cl_abap_datadescr=>kind_intf.
          WHEN OTHERS.
            TRY.
                elem ?= node.
                stack( attr_name ).
                read_any( EXPORTING
                           xml_node = elem
                           typedesc = typedescr
                           CHANGING
                                any = <d> ).
                unstack( ).
              CATCH cx_root INTO exception.
                application_log->add_exception( exception = exception ).
            ENDTRY.
        ENDCASE.
      ENDIF.
    ENDIF.
    node ?= iterator->get_next( ).
  ENDWHILE.

* XML Childs
  childs = xml_node->get_children( ).
  iterator = childs->create_iterator( ).
  next_node ?= iterator->get_next( ).
  WHILE next_node IS BOUND.
    node = next_node.    next_node ?= iterator->get_next( ).
    attr_name = node->get_name( ).
    CHECK node_is_not_type( node = node type = if_ixml_node=>co_node_text ) = abap_true. " not text node
    READ TABLE structdescr->components ASSIGNING <c> WITH KEY name = attr_name.
    IF sy-subrc <> 0.
* Component not found
      data_type = structdescr->get_relative_name( ).
      name = xml_node->get_name( ).
      IF NOT application_log IS BOUND. MESSAGE ID msgid TYPE 'E' NUMBER 003 WITH abap_true abap_true abap_true. ENDIF.
      CALL METHOD application_log->add_warning
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '003'
          id_msgv1     = attr_name
          id_msgv2     = name
          id_msgv3     = data_type
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
    ELSE.
      ASSIGN COMPONENT attr_name OF STRUCTURE structure TO <d>.
      IF sy-subrc <> 0.
* Error to raise
        mac_syserr_log sy-subrc 'ASSIGN COMPONENT attr_name OF STRUCTURE structure (Childs)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
      ELSE.
* attribute assigned
        typedescr = cl_abap_typedescr=>describe_by_data( <d> ).
        typedescr = get_ref_type( typedescr ).
        CASE typedescr->kind.
          WHEN cl_abap_datadescr=>kind_ref.
* Exception ref of ref ?
            mac_syserr_log -1 'cl_abap_datadescr=>kind_ref (Childs)' 'ZAPLINK_EASYXML->READ_STRUCTURE'. "#EC NOTEXT
          WHEN cl_abap_datadescr=>kind_elem.
            svalue = node->get_value( ).
            string_2_elem( EXPORTING
                              string = svalue
                          type_desc = typedescr
                           CHANGING
                                any = <d> ).
* cl_abap_datadescr=>kind_struct cl_abap_datadescr=>kind_table cl_abap_datadescr=>kind_class cl_abap_datadescr=>kind_intf.
          WHEN OTHERS.
            elem ?= node.
            stack( attr_name ).
            read_any( EXPORTING
                       xml_node = elem
                       typedesc = typedescr
                       CHANGING
                            any = <d> ).
            unstack( ).
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDWHILE.
  endmethod.


  method READ_TABLE.
*  DATA o_node     TYPE REF TO if_ixml_node.
  DATA o_n_elem   TYPE REF TO if_ixml_element.
  DATA o_elem     TYPE REF TO if_ixml_element.
  DATA o_iterator TYPE REF TO if_ixml_node_iterator.
  DATA o_childs   TYPE REF TO if_ixml_node_list.
  DATA o_filter   TYPE REF TO if_ixml_node_filter.
  DATA dref       TYPE REF TO data.
  DATA d_name     TYPE string.
  DATA d_context  LIKE LINE OF context.
  DATA _cx        TYPE REF TO cx_root.
  DATA _count     TYPE string.
  FIELD-SYMBOLS:
    <d> TYPE ANY.

  o_childs = xml_node->get_children( ).
  o_filter = xml_node->create_filter_node_type( if_ixml_node=>co_node_element ).
  o_iterator = o_childs->create_iterator_filtered( filter = o_filter ).
  o_n_elem ?= o_iterator->get_next( ).
  WHILE o_n_elem IS BOUND.
    ADD 1 TO _count.    stack( _count ).
    o_elem = o_n_elem.    o_n_elem ?= o_iterator->get_next( ).
    d_name = o_elem->get_name( ).
    IF en_line <> d_name.
* Unexpected node name : Node name of table line should be '&1' but is '&2' within &3
      IF NOT context IS INITIAL. READ TABLE context INTO d_context INDEX 1. ENDIF.
      CALL METHOD application_log->add_error
        EXPORTING
*             id_msgid     = MSGID
          id_msgno     = '007'
          id_msgv1     = en_line
          id_msgv2     = d_name
          id_msgv3     = d_context
*             id_msgv4     =
*             id_detlevel  =
*             id_probclass =
          .
    ELSE.
      TRY.
          CREATE DATA dref LIKE LINE OF table.
        CATCH cx_root INTO _cx.
          application_log->add_exception( exception = _cx ).
          CONTINUE.
      ENDTRY.
      ASSIGN dref->* TO <d>.
      IF sy-subrc <> 0.
* Assign error
        mac_syserr_log sy-subrc 'ASSIGN dref->*' 'ZAPLINK_EASYXML->READ_TABLE'. "#EC NOTEXT
      ELSE.
        read_any( EXPORTING
                   xml_node = o_elem
                   CHANGING
                        any = <d> ).
        INSERT <d> INTO TABLE table.
        mac_syserr_log sy-subrc 'INSERT <d> INTO TABLE table.' 'ZAPLINK_EASYXML->READ_TABLE'. "#EC NOTEXT
      ENDIF.
    ENDIF.
    unstack( ).
  ENDWHILE.
  endmethod.


  method RESET.
  DATA object	TYPE balobj_d.
  DATA subobject  TYPE balsubobj.
  DATA ext_id	TYPE balnrext.

  IF application_log IS BOUND.
    application_log->clear( ).
    application_log->free( ).
  ELSE.
    CREATE OBJECT application_log.
    application_log->msgid = msgid.

    object = 'ZAPLINK'.
    subobject	= 'EASYXML'.
*  CONCATENATE sy-uname sy-datum sy-uzeit INTO ext_id SEPARATED BY space.
*01    Create
*02    Change
*03    Display
*04    Modify (Direct Input: Create/Change)
*06    Delete
*11    Create defaults
*12    Change defaults
*13    Display defaults
    CALL METHOD application_log->init
      EXPORTING
        id_object      = object
        id_subobject   = subobject
        id_extnumber   = ext_id
        auto_upd_custo = abap_true
        id_activity    = zaplink_message_collector=>actions-create
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* ignore
    ELSE.
      bal_level = 1.
    ENDIF.
  ENDIF.
  endmethod.


  method STACK.
  IF NOT item IS INITIAL.
    INSERT item INTO context INDEX 1.
  ENDIF.
  endmethod.


  method STACK_DESCR.
  DATA name TYPE string.

  name = desc->absolute_name.

  stack( name ).
  endmethod.


  method STRING_2_ELEM.
  DATA _desc     TYPE REF TO cl_abap_typedescr.
  DATA exception TYPE REF TO cx_root.

  IF string IS INITIAL. EXIT. ENDIF.

  IF type_desc IS BOUND.
    _desc = type_desc.
  ELSE.
    _desc = cl_abap_typedescr=>describe_by_data( p_data = any ).
  ENDIF.

  IF _desc->kind <> cl_abap_datadescr=>kind_elem. EXIT. ENDIF.

  CASE _desc->type_kind.
    WHEN cl_abap_typedescr=>typekind_xstring
      OR cl_abap_typedescr=>typekind_xsequence.
      TRY.
          CALL FUNCTION 'ECATT_CONV_STRING_TO_XSTRING'
            EXPORTING
              im_string         = string
*             IM_ENCODING       = 'UTF-8'
            IMPORTING
              ex_xstring        = any
*             EX_LEN            =
                    .
        CATCH cx_root INTO exception.
          application_log->add_exception( exception = exception ).
      ENDTRY.
    WHEN cl_abap_typedescr=>typekind_char
      OR cl_abap_typedescr=>typekind_clike
      OR cl_abap_typedescr=>typekind_date
      OR cl_abap_typedescr=>typekind_time
      OR cl_abap_typedescr=>typekind_string
      OR cl_abap_typedescr=>typekind_csequence
      OR cl_abap_typedescr=>typekind_num
      OR cl_abap_typedescr=>typekind_numeric
      OR cl_abap_typedescr=>typekind_int
      OR cl_abap_typedescr=>typekind_int1
      OR cl_abap_typedescr=>typekind_int2
      OR cl_abap_typedescr=>typekind_float
      OR cl_abap_typedescr=>typekind_packed
      OR cl_abap_typedescr=>typekind_simple
      OR cl_abap_typedescr=>typekind_hex        " Convert Hex to String works
        .
      TRY.
          any = string.
        CATCH cx_root INTO exception.
          application_log->add_exception( exception = exception ).
      ENDTRY.
    WHEN OTHERS.
* Unexpected => Dump
      mac_syserr_log -1 'WHEN OTHERS' 'ZAPLINK_EASYXML->STRING_2_ELEM'.

*    WHEN cl_abap_typedescr=>typekind_class
*      OR cl_abap_typedescr=>typekind_struct1
*      OR cl_abap_typedescr=>typekind_struct2
*      OR cl_abap_typedescr=>typekind_table
*      .
*
*    WHEN cl_abap_typedescr=>typekind_any.
*    WHEN cl_abap_typedescr=>typekind_data.
*    WHEN cl_abap_typedescr=>typekind_dref.
*    WHEN cl_abap_typedescr=>typekind_hex.
*    WHEN cl_abap_typedescr=>typekind_intf.
*    WHEN cl_abap_typedescr=>typekind_iref.
*    WHEN cl_abap_typedescr=>typekind_oref.
*    WHEN cl_abap_typedescr=>typekind_w.
  ENDCASE.
  endmethod.


  method UNPROTECT_STRING.
  DATA d_last TYPE i.
  CHECK NOT string IS INITIAL.
  d_last = STRLEN( string ) - 1.    result = string.
  IF string(1) = protection_char AND d_last >= 1.
    IF string+1(1) IN char_to_protect.
      SHIFT result BY 1 PLACES LEFT.
    ENDIF.
  ENDIF.
  IF string+d_last(1) = protection_char AND d_last >= 2.      " Issue 61 & 68
    SUBTRACT 1 FROM d_last.
    IF string+d_last(1) IN char_to_protect.
      SHIFT result BY 1 PLACES RIGHT CIRCULAR.    SHIFT result BY 1 PLACES LEFT.
    ENDIF.
  ENDIF.
  endmethod.


  method UNSTACK.
  IF NOT context IS INITIAL.
    DELETE context INDEX 1.
  ENDIF.
  endmethod.


  method XML2ANY.
  DATA root_node   TYPE REF TO if_ixml_element.

  root_node = xmldoc->get_root_element( ).

  IF root_node IS BOUND.
    IF attributs IS SUPPLIED.
      CALL METHOD me->get_attributs
        EXPORTING
          xmldoc    = xmldoc
        CHANGING
          attributs = attributs.
    ENDIF.

    read_any( EXPORTING
               xml_node = root_node
               CHANGING
                    any = any ).
    IF application_log->has_messages_of_msgt( id_msgty     = 'E' ) IS INITIAL.
* Successfull conversion XML to data.
      IF NOT application_log IS BOUND. MESSAGE ID msgid TYPE 'S' NUMBER 005. ENDIF.
      CALL METHOD application_log->add_success
        EXPORTING
*          id_msgid     = MSGID
          id_msgno     = '005'
*          id_msgv1     =
*          id_msgv2     =
*          id_msgv3     =
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
          .
    ENDIF.
  ENDIF.
  endmethod.


  method _XML2ANY.
  DATA root_node   TYPE REF TO if_ixml_element.
  DATA _doc        TYPE REF TO if_ixml_document.

  TRY.
      _doc ?= xml.
      root_node = _doc->get_root_element( ).
    CATCH cx_sy_move_cast_error.
      root_node ?= xml.
  ENDTRY.

  IF root_node IS BOUND.
    read_any( EXPORTING
               xml_node = root_node
               CHANGING
                    any = any ).
  ENDIF.
  endmethod.
ENDCLASS.
