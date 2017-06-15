class ZAPLINK_CX_LIST definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_ACTION
    for ZAPLINK_DATATYPES~TD_ACTION .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_DATATYPES~TD_CONNCLASS .

  constants:
    begin of INVALID_ACTION,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '301',
      attr1 type scx_attrname value 'SEL_ACTION',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_ACTION .
  constants:
    begin of ITERATOR_NOT_INITIALIZE,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '302',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ITERATOR_NOT_INITIALIZE .
  constants:
    begin of NOT_FOUND,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '303',
      attr1 type scx_attrname value 'TYPE',
      attr2 type scx_attrname value 'NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_FOUND .
  constants:
    begin of ZAPLINK_CX_LIST,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '110',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_LIST .
  data NAME type TD_COMPNAME .
  data TYPE type TD_COMPTYPE .
  data CONNCLASS type TD_CONNCLASS .
  data SEL_ACTION type TD_ACTION .

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
      !SYSTEMSTACK type ABAP_CALLSTACK optional
      !NAME type TD_COMPNAME optional
      !TYPE type TD_COMPTYPE optional
      !CONNCLASS type TD_CONNCLASS optional
      !SEL_ACTION type TD_ACTION optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_LIST IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MESSAGES = MESSAGES
OBJECT = OBJECT
SUBOBJECT = SUBOBJECT
EXT_ID = EXT_ID
CX_NAME = CX_NAME
_CX_LIST = _CX_LIST
SYSTEMSTACK = SYSTEMSTACK
.
me->NAME = NAME .
me->TYPE = TYPE .
me->CONNCLASS = CONNCLASS .
me->SEL_ACTION = SEL_ACTION .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_LIST .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
