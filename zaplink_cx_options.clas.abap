class ZAPLINK_CX_OPTIONS definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  constants:
    begin of DIALOG_CANCELED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '253',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DIALOG_CANCELED .
  constants:
    begin of ZAPLINK_CX_OPTIONS,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '111',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_OPTIONS .

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
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_OPTIONS IMPLEMENTATION.


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
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_OPTIONS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
