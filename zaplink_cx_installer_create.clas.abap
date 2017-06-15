class ZAPLINK_CX_INSTALLER_CREATE definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .

  constants:
    begin of ACTIVATION_ERROR,
      msgid type symsgid value 'ZAPLINK_DEVTOOLS',
      msgno type symsgno value '503',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ACTIVATION_ERROR .
  constants:
    begin of NOT_EXISTS,
      msgid type symsgid value 'ZAPLINK_DEVTOOLS',
      msgno type symsgno value '500',
      attr1 type scx_attrname value 'MASTER_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_EXISTS .
  constants:
    begin of NO_INCLUDE,
      msgid type symsgid value 'ZAPLINK_DEVTOOLS',
      msgno type symsgno value '502',
      attr1 type scx_attrname value 'INCLUDE_NAME',
      attr2 type scx_attrname value 'MASTER_NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_INCLUDE .
  constants:
    begin of NO_OVERWRITE,
      msgid type symsgid value 'ZAPLINK_DEVTOOLS',
      msgno type symsgno value '501',
      attr1 type scx_attrname value 'INSTALLER_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_OVERWRITE .
  constants:
    begin of ZAPLINK_CX_INSTALLER_CREATE,
      msgid type symsgid value 'ZAPLINK_DEVTOOLS',
      msgno type symsgno value '499',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_INSTALLER_CREATE .
  data INCLUDE_NAME type PROGRAMM read-only .
  data INSTALLER_NAME type PROGRAMM read-only .
  data MASTER_NAME type PROGRAMM read-only .

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
      !INCLUDE_NAME type PROGRAMM optional
      !INSTALLER_NAME type PROGRAMM optional
      !MASTER_NAME type PROGRAMM optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_INSTALLER_CREATE IMPLEMENTATION.


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
me->INCLUDE_NAME = INCLUDE_NAME .
me->INSTALLER_NAME = INSTALLER_NAME .
me->MASTER_NAME = MASTER_NAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_INSTALLER_CREATE .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
