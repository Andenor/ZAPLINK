class ZAPLINK_CX_OPT_DEVCLASS definition
  public
  inheriting from ZAPLINK_CX_OPTIONS
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_SUBSTITUTIONKIND
    for ZAPLINK_DATATYPES~TD_SUBSTITUTIONKIND .

  constants:
    begin of INVALID_SUBSTITUTIONKIND,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '254',
      attr1 type scx_attrname value 'SUBSTKIND',
      attr2 type scx_attrname value 'OPT_CAT',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_SUBSTITUTIONKIND .
  constants:
    begin of ZAPLINK_CX_OPT_DEVCLASS,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '111',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_OPT_DEVCLASS .
  constants:
    begin of MISSING_DEVCLASS,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '255',
      attr1 type scx_attrname value 'SUBSTKIND',
      attr2 type scx_attrname value 'OPT_CAT',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_DEVCLASS .
  data SUBSTKIND type TD_SUBSTITUTIONKIND .
  data DEVCLASS type TD_DEVCLASS .
  class-data OPT_CAT type STRING .

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
      !SYSTEMSTACK type ABAP_CALLSTACK optional
      !SUBSTKIND type TD_SUBSTITUTIONKIND optional
      !DEVCLASS type TD_DEVCLASS optional
      !OPT_CAT type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_OPT_DEVCLASS IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  OPT_CAT = 'Package'(cat).
  endmethod.


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
me->SUBSTKIND = SUBSTKIND .
me->DEVCLASS = DEVCLASS .
me->OPT_CAT = OPT_CAT .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_OPT_DEVCLASS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
