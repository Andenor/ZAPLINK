class ZLCX_COMP_ALREADY_EXISTS definition
  public
  inheriting from ZAPLINK_CX_LIST
  create public .

public section.

  constants:
    begin of ZLCX_COMP_ALREADY_EXISTS,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '300',
      attr1 type scx_attrname value 'TYPE',
      attr2 type scx_attrname value 'NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZLCX_COMP_ALREADY_EXISTS .

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



CLASS ZLCX_COMP_ALREADY_EXISTS IMPLEMENTATION.


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
NAME = NAME
TYPE = TYPE
CONNCLASS = CONNCLASS
SEL_ACTION = SEL_ACTION
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZLCX_COMP_ALREADY_EXISTS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
