class ZAPLINK_CX_COMPONENT definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.
  type-pools ABAP .

  interfaces ZAPLINK_DATATYPES .

  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_DATATYPES~TD_CONNCLASS .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .

  constants:
    begin of INVALID_EXCEPTION,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '068',
      attr1 type scx_attrname value 'OBJTYPE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_EXCEPTION .
  constants:
    begin of INVALID_MSG_COLL,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '069',
      attr1 type scx_attrname value 'OBJTYPE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_MSG_COLL .
  constants:
    begin of INVALID_SUBCOMPONENTS,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '070',
      attr1 type scx_attrname value 'OBJTYPE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_SUBCOMPONENTS .
  constants:
    begin of INVALID_TYPE,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '060',
      attr1 type scx_attrname value 'TYPE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_TYPE .
  constants:
    begin of ZAPLINK_CX_COMPONENT,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '106',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_COMPONENT .
  data NAME type TD_COMPNAME .
  data TYPE type TD_COMPTYPE .
  data CONNCLASS type TD_CONNCLASS .
  data OBJTYPE type TD_OBJ_TYPE .

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
      !OBJTYPE type TD_OBJ_TYPE optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_COMPONENT IMPLEMENTATION.


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
me->OBJTYPE = OBJTYPE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_COMPONENT .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
