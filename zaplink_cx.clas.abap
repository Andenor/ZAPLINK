class ZAPLINK_CX definition
  public
  inheriting from CX_STATIC_CHECK
  create public

  global friends ZAPLINK_CX .

public section.
  type-pools ABAP .
  type-pools ICON .
  interface IF_T100_MESSAGE load .

  interfaces IF_T100_MESSAGE .

  types TD_EXCEPTION type SCX_T100KEY .
  types TO_EXCEPTION type ref to ZAPLINK_CX .
  types TO_ROOT_EXCEPTION type ref to CX_ROOT .
  types:
    BEGIN OF ts_cx,
        class TYPE seoclsname,
      END OF ts_cx .
  types:
    tt_cx_list TYPE HASHED TABLE OF ts_cx WITH UNIQUE KEY class .
  types:
    BEGIN OF ts_source_def,
          type_txt TYPE euobjt-singular,
          name TYPE	seu_objkey,
          str TYPE string,
        END OF ts_source_def .
  types O_MSG_COLL type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TS_SOURCE_POS type ABAP_CALLSTACK_LINE .

  constants:
    begin of BAL_LOG,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '103',
      attr1 type scx_attrname value 'OBJECT',
      attr2 type scx_attrname value 'SUBOBJECT',
      attr3 type scx_attrname value 'EXT_ID',
      attr4 type scx_attrname value '',
    end of BAL_LOG .
  constants:
    begin of ZAPLINK_CX,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '105',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX .
  constants:
    begin of SYSTEM_ERROR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '104',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SYSTEM_ERROR .
  data MESSAGES type O_MSG_COLL .
  data OBJECT type BALOBJ_D .
  data SUBOBJECT type BALSUBOBJ .
  data EXT_ID type BALNREXT .
  data CX_NAME type STRING .
  class-data _CX_LIST type TT_CX_LIST .
  data SYSTEMSTACK type ABAP_CALLSTACK read-only .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MESSAGES type O_MSG_COLL optional
      !OBJECT type BALOBJ_D optional
      !SUBOBJECT type BALSUBOBJ optional
      !EXT_ID type BALNREXT optional
      !CX_NAME type STRING optional
      !_CX_LIST type TT_CX_LIST optional
      !SYSTEMSTACK type ABAP_CALLSTACK optional .
  methods UPDATE .
  class-methods CREATE_FROM_APPLICATION_LOG
    importing
      !CX_NAME type STRING optional
      !MESSAGE_COLLECTOR type O_MSG_COLL
      !TEXTID type TD_EXCEPTION default BAL_LOG
      !PREV_CX type ref to ZAPLINK_CX optional
    returning
      value(RESULT) type ref to ZAPLINK_CX
    raising
      ZAPLINK_CX .
  methods WRITE .
  methods GET_SOURCE_POSITION_AS_STRING
    returning
      value(STR) type STRING .
  class-methods SOURCE_POSITION_AS_STRING
    importing
      !EXCEPTION type ref to CX_ROOT
    returning
      value(STR) type STRING .
  class-methods CREATE_FROM_MF_CX
    importing
      !FUNCNAME type RS38L-NAME
      value(SUBRC) type SY-SUBRC
      !CLASSNAME type STRING optional
      !TEXTID type TD_EXCEPTION default SYSTEM_ERROR
    returning
      value(RESULT) type ref to ZAPLINK_CX .
  class-methods CREATE_FROM_SYMSG
    importing
      !CLASSNAME type STRING optional
      !TEXTID type TD_EXCEPTION default SYSTEM_ERROR
    returning
      value(RESULT) type ref to ZAPLINK_CX
    raising
      ZAPLINK_CX .
  class-methods CREATE_FROM_METHOD_CX
    importing
      !CLASS type ref to OBJECT optional
      !CLASS_NAME type STRING optional
      value(METHOD) type STRING
      value(SUBRC) type SY-SUBRC
      !CX_CLASSNAME type STRING optional
      !TEXTID type TD_EXCEPTION default SYSTEM_ERROR
    returning
      value(RESULT) type ref to ZAPLINK_CX .
  methods LOAD_SYSTEMSTACK
    importing
      !FORCE_RELOAD type ABAP_BOOL optional .
  methods SYSTEMSTACK_UP
    importing
      !FORCE_RELOAD type ABAP_BOOL optional .
  methods IS_EXCEPTION_TEXT
    importing
      !TEXTID type TD_EXCEPTION
    returning
      value(RESULT) type ABAP_BOOL .
protected section.

  aliases TEXTID_MSG
    for IF_T100_MESSAGE~T100KEY .

  class-methods CONV_PROGNAME
    importing
      !OBJECT_NAME type SEU_OBJKEY
      !CONTEXT type SEU_OBJKEY optional
    returning
      value(SOURCE) type TS_SOURCE_DEF .
  class-methods STACK_POSITION_AS_STRING
    importing
      !SOURCE type TS_SOURCE_POS
    returning
      value(STR) type STRING .
  class-methods _CREATE_CX
    importing
      !CX_NAME type STRING optional
      !TEXTID type TD_EXCEPTION
    returning
      value(RESULT) type ref to ZAPLINK_CX .
private section.

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_CX IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  DATA _exceptions TYPE REF TO CL_OO_CLASS_RELATIONS.
  DATA s_list      LIKE LINE OF _cx_list.
  DATA t_list      LIKE STANDARD TABLE OF s_list.
  FIELD-SYMBOLS:
*    <im_cl> LIKE LINE OF _exceptions->implementing_classes,
    <s_cl> LIKE LINE OF _exceptions->subclasses.

  CLEAR _cx_list.
* Interface use
  CREATE OBJECT _exceptions
    EXPORTING
      clsname      = 'ZAPLINK_CX'
*      w_superclasses = seox_true
*      w_subclasses = seox_true
*      w_references = seox_true
*      w_redefinitions = seox_true
*      w_eventhandler = seox_true
*      w_implementings = seox_true
     EXCEPTIONS
       is_interface = 1
       not_existing = 0  " For installer ignore not existing class
       others       = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  check _exceptions is bound.

*  LOOP AT _exceptions->implementing_classes ASSIGNING <im_cl>
*          WHERE exposure <> '0'   " Private
*            AND version = '1'     " Active
*            AND state <> '0'      " Only modeled
*            .
*    CLEAR s_list. s_list-class = <im_cl>-clsname. APPEND s_list TO t_list.
*  ENDLOOP.

  LOOP AT _exceptions->subclasses ASSIGNING <s_cl>
          WHERE state <> '0'      " Only modeled
            AND version = '1'     " Active
            .
    CLEAR s_list. s_list-class = <s_cl>-clsname. APPEND s_list TO t_list.
  ENDLOOP.

  SORT t_list BY class.
  DELETE ADJACENT DUPLICATES FROM t_list.
  _cx_list = t_list.
  endmethod.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MESSAGES = MESSAGES .
me->OBJECT = OBJECT .
me->SUBOBJECT = SUBOBJECT .
me->EXT_ID = EXT_ID .
me->CX_NAME = CX_NAME .
me->_CX_LIST = _CX_LIST .
me->SYSTEMSTACK = SYSTEMSTACK .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method CONV_PROGNAME.
  DATA p_wb_request	TYPE REF TO cl_wb_request.
  DATA p_ex_request	TYPE REF TO cl_wb_request.
  DATA p_wb_source_req_dispatcher TYPE REF TO cl_wb_source_req_dispatcher.
  DATA kind TYPE  seoclsname.

  CREATE OBJECT p_wb_source_req_dispatcher.

  CREATE OBJECT p_wb_request
    EXPORTING
      p_object_type                = 'P'
      p_object_name                = object_name
      p_operation                  = 'DISPLAY'
*      p_in_new_window              = wn_same_window
*      p_object_state               =
*      p_lazy_start                 = to_immediate_start
    EXCEPTIONS
      illegal_object_type          = 1
      illegal_operation            = 2
      illegal_new_window_parameter = 3
      OTHERS                       = 4.
  IF sy-subrc <> 0.
    source-str = source-name = object_name.
    EXIT.
  ENDIF.

  CALL METHOD p_wb_source_req_dispatcher->if_wb_decision_class~get_tool_for_request
    EXPORTING
      p_wb_request            = p_wb_request
    IMPORTING
      p_execution_request     = p_ex_request
      p_toolname              = kind
    EXCEPTIONS
      no_tool_found           = 1
      object_not_found        = 2
      operation_not_supported = 3
      OTHERS                  = 4.
  IF sy-subrc <> 0.
    source-str = source-name = object_name.
    EXIT.
  ENDIF.

  IF NOT p_ex_request IS BOUND.
    p_ex_request = p_wb_request.
  ENDIF.

  source-name = p_ex_request->object_name.
  IF NOT context IS INITIAL.
    REPLACE FIRST OCCURRENCE OF context IN source-name WITH ` `.
    CONDENSE source-name.
  ENDIF.

  SELECT SINGLE singular INTO source-type_txt
    FROM euobjt
    WHERE spras = sy-langu
      AND type = p_ex_request->object_type.
  IF sy-subrc <> 0.
    SELECT SINGLE singular INTO source-type_txt
      FROM euobjt
      WHERE spras = 'E'
        AND type = p_ex_request->object_type.
    IF sy-subrc <> 0.
      SELECT SINGLE singular INTO source-type_txt
        FROM euobjt
        WHERE type = p_ex_request->object_type.
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    CONCATENATE source-type_txt source-name INTO source-str SEPARATED BY space.
  ELSE.
    source-str = source-name.
  ENDIF.
  endmethod.


  method CREATE_FROM_APPLICATION_LOG.
  result = _create_cx( cx_name = cx_name
                        textid = textid ).
  result->systemstack_up( ).  " remove this method from stack

  result->messages = message_collector.
  result->previous = prev_cx.

  result->update( ).
  endmethod.


  method CREATE_FROM_METHOD_CX.
  DATA o_desc   TYPE REF TO cl_abap_objectdescr.
  DATA t_excep  TYPE abap_excpdescr_tab.
  FIELD-SYMBOLS:
    <m> LIKE LINE OF o_desc->methods,
    <e> LIKE LINE OF <m>-exceptions.

  result = _create_cx( cx_name = cx_classname
                        textid = textid ).
  result->systemstack_up( ).  " remove this method from stack

  CHECK class IS BOUND OR NOT class_name IS INITIAL.

  IF class IS BOUND.
    o_desc ?= cl_abap_objectdescr=>describe_by_object_ref( class ).
  ELSE.
    o_desc ?= cl_abap_objectdescr=>describe_by_name( class_name ).
  ENDIF.

  TRANSLATE method TO UPPER CASE.
  READ TABLE o_desc->methods ASSIGNING <m>
       WITH TABLE KEY name = method.
  ASSERT sy-subrc = 0.
  ASSERT subrc >= 1 AND subrc <= LINES( <m>-exceptions ).
  t_excep = <m>-exceptions.   SORT t_excep.
  READ TABLE t_excep ASSIGNING <e> INDEX subrc.
  ASSERT sy-subrc = 0.
  result->cx_name = <e>-name.
  endmethod.


  method CREATE_FROM_MF_CX.
  CONSTANTS c_cx_kind TYPE funct-kind VALUE 'X'. " Exception

  DATA _funcname      TYPE rs38l-name.
  DATA lt_ptfdir      TYPE STANDARD TABLE OF tfdir WITH DEFAULT KEY.
  DATA lt_ptftit      TYPE STANDARD TABLE OF tftit WITH DEFAULT KEY.
  DATA lt_pfunct      TYPE STANDARD TABLE OF funct WITH DEFAULT KEY.
  DATA lt_penlfdir    TYPE STANDARD TABLE OF enlfdir WITH DEFAULT KEY.
  DATA lt_ptrdir      TYPE STANDARD TABLE OF trdir WITH DEFAULT KEY.
  DATA lt_pfupararef  TYPE STANDARD TABLE OF sfupararef WITH DEFAULT KEY.
  DATA lt_uincl       TYPE STANDARD TABLE OF abaptxt255 WITH DEFAULT KEY.
  DATA _cx            TYPE REF TO zaplink_cx.
  DATA _subrc         TYPE sy-subrc.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF lt_pfunct,
    <p> LIKE LINE OF lt_pfupararef.

  _funcname = funcname.
  TRANSLATE _funcname TO UPPER CASE.                      "#EC SYNTCHAR

  result = _create_cx( cx_name = classname
                        textid = textid ).
  result->systemstack_up( ).  " remove this method from stack

  CALL FUNCTION 'FUNC_GET_OBJECT'
    EXPORTING
      funcname                 = _funcname
*     R3STATE                  = 'A'
    TABLES
      ptfdir                   = lt_ptfdir
      ptftit                   = lt_ptftit
      pfunct                   = lt_pfunct
      penlfdir                 = lt_penlfdir
      ptrdir                   = lt_ptrdir
      pfupararef               = lt_pfupararef
      uincl                    = lt_uincl
*     VSMODISRC                =
*     VSMODILOG                =
    EXCEPTIONS
      function_not_exist       = 1
      version_not_found        = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    _cx = create_from_mf_cx( funcname = 'FUNC_GET_OBJECT'
                                subrc = sy-subrc ).
    result->previous = _cx.
    result->textid_msg = zaplink_cx=>system_error.
    EXIT.
  ENDIF.

  DELETE lt_pfunct WHERE kind <> c_cx_kind.
  DELETE lt_pfupararef WHERE paramtype <> c_cx_kind.

  READ TABLE lt_pfupararef ASSIGNING <p>
       WITH KEY pposition = subrc.
  IF sy-subrc <> 0.
    _subrc = subrc - 1.
    READ TABLE lt_pfupararef ASSIGNING <p>
         WITH KEY pposition = _subrc.
    IF sy-subrc = 0.
      result->cx_name = 'OTHERS'.   " #EC NOTEXT
      EXIT.
    ENDIF.
  ENDIF.
  ASSERT ID ZAPLINK CONDITION <p> IS ASSIGNED.     CHECK <p> IS ASSIGNED.
*  IF NOT <p> IS ASSIGNED.
*    RAISE EXCEPTION TYPE zaplink_cx
*          EXPORTING textid = zaplink_cx=>system_error.
*  ENDIF.

  result->cx_name = <p>-parameter.
  READ TABLE lt_pfunct ASSIGNING <t>
       WITH KEY parameter = <p>-parameter
                    spras = sy-langu.
  IF <t> IS ASSIGNED. IF <t>-stext IS INITIAL.     UNASSIGN <t>.      ENDIF. ENDIF.
  IF NOT <t> IS ASSIGNED.
    READ TABLE lt_pfunct ASSIGNING <t>
         WITH KEY parameter = <p>-parameter
                      spras = 'E'.
    IF <t> IS ASSIGNED. IF <t>-stext IS INITIAL.     UNASSIGN <t>.      ENDIF. ENDIF.
  ENDIF.
  IF NOT <t> IS ASSIGNED.
    READ TABLE lt_pfunct ASSIGNING <t>
         WITH KEY parameter = <p>-parameter.
    IF <t> IS ASSIGNED. IF <t>-stext IS INITIAL.     UNASSIGN <t>.      ENDIF. ENDIF.
  ENDIF.
  IF <t> IS ASSIGNED.
    CONCATENATE result->cx_name '(' <t>-stext ')' INTO result->cx_name.
  ENDIF.
  endmethod.


  method CREATE_FROM_SYMSG.

  result = _create_cx( cx_name = classname
                        textid = textid ).
  result->systemstack_up( ).  " remove this method from stack

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO result->cx_name.
  endmethod.


  method GET_SOURCE_POSITION_AS_STRING.
  str = source_position_as_string( me ).
  endmethod.


  method IS_EXCEPTION_TEXT.
  If textid_msg = textid.     result = abap_true.   endif.
  endmethod.


  method LOAD_SYSTEMSTACK.
  IF systemstack IS INITIAL OR force_reload = abap_true.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
*     EXPORTING
*       MAX_LEVEL          = 0
      IMPORTING
*       ET_CALLSTACK       =
        callstack          = systemstack.
    systemstack_up( ).   " Remove load systemstack
  ENDIF.
  endmethod.


  method SOURCE_POSITION_AS_STRING.
  DATA _pos TYPE ts_source_pos.
*  DATA _key TYPE seu_objkey.
*  DATA _prog TYPE ts_source_def.
*  DATA _include TYPE ts_source_def.

  CALL METHOD exception->get_source_position
    IMPORTING
      program_name = _pos-mainprogram
      include_name = _pos-include
      source_line  = _pos-line.

  str = STACK_POSITION_AS_STRING( _pos ).
*  _key = _pos-mainprogram. _prog = conv_progname( _key ).
*  IF _pos-mainprogram = _pos-include.
*    MESSAGE i002 WITH _prog-str _pos-line INTO str.
*  ELSE.
*    _key = _pos-include.     _include = conv_progname( object_name = _key   context = _prog-name ).
*    MESSAGE i000 WITH _prog-str _include-str _pos-line INTO str.
*  ENDIF.
  endmethod.


  method STACK_POSITION_AS_STRING.
  DATA _key TYPE seu_objkey.
  DATA _prog TYPE ts_source_def.
  DATA _include TYPE ts_source_def.

  _key = source-mainprogram. _prog = conv_progname( _key ).
  IF source-mainprogram = source-include.
    MESSAGE i102 WITH _prog-str source-line INTO str.
  ELSE.
    _key = source-include.     _include = conv_progname( object_name = _key   context = _prog-name ).
    MESSAGE i100 WITH _prog-str _include-str source-line INTO str.
  ENDIF.
  endmethod.


  method SYSTEMSTACK_UP.
  CHECK systemstack IS NOT INITIAL.   DELETE systemstack INDEX 1.
  endmethod.


  method UPDATE.
  DATA desc TYPE REF TO cl_abap_objectdescr.

* BAl_log
  IF messages IS BOUND.
    object = messages->object.
    subobject = messages->subobject.
    ext_id = messages->ext_id.
    messages->store( ).
  ENDIF.

* cx_name
  IF cx_name IS INITIAL AND previous IS BOUND.
    desc ?= cl_abap_objectdescr=>describe_by_object_ref( previous ).
    cx_name = desc->absolute_name.
  ENDIF.
  endmethod.


  method WRITE.
  DATA _exceptions  TYPE STANDARD TABLE OF string.
  DATA _msg         TYPE string.
  DATA _pos         TYPE string.
  DATA _cx          TYPE REF TO cx_root.
  DATA _order       TYPE STANDARD TABLE OF string.
  DATA _offset      TYPE string.
  DATA _messages    TYPE bapirettab.
  DATA _light       TYPE icon_d.
  DATA t_stack      TYPE abap_callstack.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF t_stack,
    <e> LIKE LINE OF _exceptions.

  _cx = me.
  WHILE _cx IS BOUND.
    _msg = _cx->get_text( ).
*    if not _cx->systemstack is INITIAL.   t_stack = _cx->systemstack.   endif.
    _pos = zaplink_cx=>source_position_as_string( _cx ).
    CONCATENATE _msg _pos INTO _msg SEPARATED BY space.
    APPEND _msg TO _exceptions.
    _cx = _cx->previous.
  ENDWHILE.

  LOOP AT _exceptions ASSIGNING <e>.
    INSERT <e> INTO _order INDEX 1.
  ENDLOOP.
  FREE _exceptions.
*ICON_ARROW_RIGHT
*ICON_WF_LINK
*ICON_PDIR_FOREWARD
*ICON_PDIR_FOREWARD_SWITCH
*ICON_CONNECT
*ICON_TREND_UNCHANGED
*ICON_PAGE_RIGHT
  LOOP AT _order ASSIGNING <e>.
    IF sy-tabix = 1.
      WRITE:/ icon_message_critical_small AS ICON, <e>.
    ELSE.
      CONCATENATE ` ` _offset INTO _offset.
      WRITE:/ _offset, icon_arrow_right AS ICON, <e>.
    ENDIF.
  ENDLOOP.
  CONCATENATE ` ` _offset INTO _offset.
  LOOP AT t_stack ASSIGNING <s>.
    _msg = stack_position_as_string( <s> ).
    WRITE:/ _offset, icon_stack AS ICON, _msg.
  ENDLOOP.
  IF me->messages IS BOUND.   messages->write( _offset ).   ENDIF.
  endmethod.


  method _CREATE_CX.
  DATA f_exists TYPE abap_bool.

  IF NOT cx_name IS INITIAL.
    READ TABLE _cx_list TRANSPORTING NO FIELDS
         WITH TABLE KEY class = cx_name.
    IF sy-subrc = 0.
      f_exists = abap_true.
    ELSE.
      f_exists = abap_false.
    ENDIF.
    TRY.
        CREATE OBJECT result
          TYPE
            (cx_name)
          EXPORTING
            textid    = textid.
      CATCH cx_root INTO o_cx.
        ASSERT f_exists = abap_false.
*        IF f_exists = abap_true.
*          mac_cascade_raise o_mycx o_cx.
*        ENDIF.
    ENDTRY.
  ENDIF.

  IF result IS NOT BOUND.
    CREATE OBJECT result
      EXPORTING
        textid = textid.
  ENDIF.
  result->load_systemstack( ).
  result->systemstack_up( ).  " remove this method from stack
  endmethod.
ENDCLASS.
