class ZAPLINK_CX_CONNECTOR definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_DATATYPES~TD_CONNCLASS .
  aliases TD_CONNUUID
    for ZAPLINK_DATATYPES~TD_CONNUUID .
  aliases TD_CONNVER
    for ZAPLINK_DATATYPES~TD_CONNVER .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .

  constants:
    begin of NOT_AUTHORIZED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '056',
      attr1 type scx_attrname value 'NAME',
      attr2 type scx_attrname value 'TYPE',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_AUTHORIZED .
  constants:
    begin of NOT_FOUND,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '057',
      attr1 type scx_attrname value 'NAME',
      attr2 type scx_attrname value 'TYPE',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NOT_FOUND .
  constants:
    begin of OBJECT_LOCKED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '058',
      attr1 type scx_attrname value 'NAME',
      attr2 type scx_attrname value 'TYPE',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBJECT_LOCKED .
  constants:
    begin of ZAPLINK_CX_CONNECTOR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '055',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_CONNECTOR .
  constants:
    begin of TWICE_CLASS,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '059',
      attr1 type scx_attrname value 'CONNCLASS',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TWICE_CLASS .
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
    begin of INVALID_CONNECTOR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '061',
      attr1 type scx_attrname value 'CONNCLASS',
      attr2 type scx_attrname value 'TYPE',
      attr3 type scx_attrname value 'CX_NAME',
      attr4 type scx_attrname value '',
    end of INVALID_CONNECTOR .
  constants:
    begin of INVALID_UUID,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '062',
      attr1 type scx_attrname value 'CONNUUID',
      attr2 type scx_attrname value 'TYPE',
      attr3 type scx_attrname value 'CONNCLASS',
      attr4 type scx_attrname value '',
    end of INVALID_UUID .
  constants:
    begin of INVALID_XML,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '063',
      attr1 type scx_attrname value 'NODE_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_XML .
  constants:
    begin of UNSUPORTED_VERSION,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '064',
      attr1 type scx_attrname value 'CONN_VER',
      attr2 type scx_attrname value 'CONNUUID',
      attr3 type scx_attrname value 'CONNCLASS',
      attr4 type scx_attrname value '',
    end of UNSUPORTED_VERSION .
  constants:
    begin of TYPE_NOT_SUPPORTED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '065',
      attr1 type scx_attrname value 'TYPE',
      attr2 type scx_attrname value 'CONNCLASS',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TYPE_NOT_SUPPORTED .
  constants:
    begin of TWICE_UUID,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '066',
      attr1 type scx_attrname value 'CONNCLASS',
      attr2 type scx_attrname value 'OTHERCLASS',
      attr3 type scx_attrname value 'CONNUUID',
      attr4 type scx_attrname value '',
    end of TWICE_UUID .
  constants:
    begin of XML_ERROR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '067',
      attr1 type scx_attrname value 'TYPE',
      attr2 type scx_attrname value 'NAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of XML_ERROR .
  data NAME type TD_COMPNAME value '[NAME]' ##NO_TEXT.
  data TYPE type TD_COMPTYPE value '[TY]' ##NO_TEXT.
  data DEVCLASS type TD_DEVCLASS value '[DEVCLASS]' ##NO_TEXT.
  data CONNCLASS type TD_CONNCLASS value '[CONNCLASS]' ##NO_TEXT.
  data CONNUUID type TD_CONNUUID value '00000000000000000000000000000000' ##NO_TEXT.
  data NODE_NAME type STRING value '[NODE_NAME]' ##NO_TEXT.
  data CONN_VER type TD_CONNVER value '[CONN_VER]' ##NO_TEXT.
  data OTHERCLASS type TD_CONNCLASS value '[OTHERCLASS]' ##NO_TEXT.

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
      !NAME type TD_COMPNAME default '[NAME]'
      !TYPE type TD_COMPTYPE default '[TY]'
      !DEVCLASS type TD_DEVCLASS default '[DEVCLASS]'
      !CONNCLASS type TD_CONNCLASS default '[CONNCLASS]'
      !CONNUUID type TD_CONNUUID default '00000000000000000000000000000000'
      !NODE_NAME type STRING default '[NODE_NAME]'
      !CONN_VER type TD_CONNVER default '[CONN_VER]'
      !OTHERCLASS type TD_CONNCLASS default '[OTHERCLASS]' .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_CONNECTOR IMPLEMENTATION.


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
me->DEVCLASS = DEVCLASS .
me->CONNCLASS = CONNCLASS .
me->CONNUUID = CONNUUID .
me->NODE_NAME = NODE_NAME .
me->CONN_VER = CONN_VER .
me->OTHERCLASS = OTHERCLASS .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_CONNECTOR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
