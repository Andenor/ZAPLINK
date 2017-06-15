class ZAPLINK_CX_CONTAINER definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  constants:
    begin of ADD_COMP_FAILED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '150',
      attr1 type scx_attrname value 'TYPE',
      attr2 type scx_attrname value 'NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ADD_COMP_FAILED .
  constants:
    begin of ADD_FAILED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '151',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ADD_FAILED .
  constants:
    begin of CIRCULAR_DEPENDENCIES,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '152',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CIRCULAR_DEPENDENCIES .
  constants:
    begin of ZAPLINK_CX_CONTAINER,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '107',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_CONTAINER .
  data NAME type ZAPLINK_CX_COMPONENT=>TD_COMPNAME .
  data TYPE type ZAPLINK_CX_COMPONENT=>TD_COMPTYPE .

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
      !NAME type ZAPLINK_CX_COMPONENT=>TD_COMPNAME optional
      !TYPE type ZAPLINK_CX_COMPONENT=>TD_COMPTYPE optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_CONTAINER IMPLEMENTATION.


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
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_CONTAINER .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
