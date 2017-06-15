class ZAPLINK_PINF_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_PACKAGE .

public section.

  types:
    TT_ELEMENTS TYPE STANDARD TABLE OF scomeldtln WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_acls     TYPE STANDARD TABLE OF scomaclstr WITH NON-UNIQUE DEFAULT KEY .

  data A0_MAINDATA type SCOMPIDTLN .
  data ACLS type TT_ACLS .
  data ELEMENTS type TT_ELEMENTS .
  data IF type ref to IF_PACKAGE_INTERFACE .

  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_PINF_DATA IMPLEMENTATION.


  method ANONYMIZE.
  CLEAR:
*    me->a0_maindata-author,         " Issue 88 : Person responsible : Must be clear because user might not exists
    me->a0_maindata-created_by,
    me->a0_maindata-created_on,
    me->a0_maindata-changed_by,
    me->a0_maindata-changed_on.
  endmethod.


  method UNANONYMIZE.
  If me->a0_maindata-created_by is INITIAL. me->a0_maindata-created_by = sy-uname. endif.
  If me->a0_maindata-created_on is INITIAL. me->a0_maindata-created_on = sy-datum. endif.
*    me->a0_maindata-changed_by,
*    me->a0_maindata-changed_on.
  endmethod.
ENDCLASS.
