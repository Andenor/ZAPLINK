class ZAPLINK_CX_GUI definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .

  constants:
    begin of MISSING_FILENAME,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '204',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_FILENAME .
  constants:
    begin of MISSING_NAME,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '250',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_NAME .
  constants:
    begin of MISSING_OBJTYPE,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '251',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_OBJTYPE .
  constants:
    begin of ZAPLINK_CX_GUI,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '109',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_GUI .
  constants:
    begin of MISSING_CONTAINER_NAME,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '252',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MISSING_CONTAINER_NAME .
  data TYPE type TD_COMPTYPE .

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
      !TYPE type TD_COMPTYPE optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_GUI IMPLEMENTATION.


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
me->TYPE = TYPE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_GUI .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
