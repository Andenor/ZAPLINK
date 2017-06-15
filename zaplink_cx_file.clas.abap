class ZAPLINK_CX_FILE definition
  public
  inheriting from ZAPLINK_CX
  create public .

public section.

  types T_FILENAME type STRING .
  types T_FILETYPE type CHAR1 .

  constants:
    begin of DIALOG_CANCELED,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '209',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DIALOG_CANCELED .
  constants:
    begin of INVALID_FILEKIND,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '208',
      attr1 type scx_attrname value 'FILEKIND',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_FILEKIND .
  constants:
    begin of INVALID_FILENAME,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '207',
      attr1 type scx_attrname value 'FILENAME',
      attr2 type scx_attrname value 'FILETYPE',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_FILENAME .
  constants:
    begin of INVALID_FILETYPE,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '206',
      attr1 type scx_attrname value 'FILETYPE',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_FILETYPE .
  constants:
    begin of LOAD_ERROR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '205',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value 'FILENAME',
      attr3 type scx_attrname value 'FILETYPE',
      attr4 type scx_attrname value '',
    end of LOAD_ERROR .
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
    begin of SAVE_ERROR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '203',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value 'FILENAME',
      attr3 type scx_attrname value 'FILETYPE',
      attr4 type scx_attrname value '',
    end of SAVE_ERROR .
  constants:
    begin of SEARCH_ERROR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '202',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value 'FILENAME',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEARCH_ERROR .
  constants:
    begin of ZAPLINK_CX_FILE,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '108',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZAPLINK_CX_FILE .
  constants:
    begin of DELETE_ERROR,
      msgid type symsgid value 'ZAPLINK',
      msgno type symsgno value '210',
      attr1 type scx_attrname value 'CX_NAME',
      attr2 type scx_attrname value 'FILENAME',
      attr3 type scx_attrname value 'FILETYPE',
      attr4 type scx_attrname value '',
    end of DELETE_ERROR .
  data FILENAME type T_FILENAME .
  data FILETYPE type T_FILETYPE .
  data FILEKIND type T_FILETYPE .

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
      !FILENAME type T_FILENAME optional
      !FILETYPE type T_FILETYPE optional
      !FILEKIND type T_FILETYPE optional .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_CX_FILE IMPLEMENTATION.


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
me->FILENAME = FILENAME .
me->FILETYPE = FILETYPE .
me->FILEKIND = FILEKIND .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZAPLINK_CX_FILE .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
