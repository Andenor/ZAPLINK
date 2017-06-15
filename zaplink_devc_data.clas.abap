class ZAPLINK_DEVC_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_PACKAGE .

public section.

  types TO_PINF_RAW type ref to ZAPLINK_PINF_RAW .
  types:
    tt_acls TYPE STANDARD TABLE OF pkgpermdat WITH NON-UNIQUE DEFAULT KEY .
  types TO_DEVC_RAW type ref to ZAPLINK_DEVC_RAW .
  types:
    BEGIN OF ts_interface,
        zl_object TYPE to_pinf_raw,
      END OF ts_interface .
  types:
    tt_interfaces TYPE STANDARD TABLE OF ts_interface WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ts_package,
        zl_object TYPE to_devc_raw,
      END OF ts_package .
  types:
    tt_packages TYPE STANDARD TABLE OF ts_package WITH NON-UNIQUE DEFAULT KEY .

  data A0_MAINDATA type SCOMPKDTLN .
  data INTERFACES type TT_INTERFACES .
  data PACKAGES type TT_PACKAGES .
  data IF type ref to IF_PACKAGE .
  data ACLS type TT_ACLS .

  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.

  types TO_INTERFACE type ref to ZAPLINK_PINF_DATA .
private section.

  types TO_MYKIND type ref to ZAPLINK_DEVC_DATA .
ENDCLASS.



CLASS ZAPLINK_DEVC_DATA IMPLEMENTATION.


  method ANONYMIZE.
  DATA o_raw_i     LIKE LINE OF me->interfaces.
  DATA o_interface TYPE to_interface.
  DATA o_raw_p     LIKE LINE OF me->packages.
  DATA o_package   TYPE to_mykind.

  CLEAR:
    me->a0_maindata-created_by,
    me->a0_maindata-created_on,
    me->a0_maindata-changed_by,
*    me->a0_maindata-as4user,    " Issue 88 : Person responsible : Must be clear because user might not exists
    me->a0_maindata-changed_on.

  LOOP AT me->interfaces INTO o_raw_i.
    o_interface ?= o_raw_i-zl_object->raw.
    o_interface->anonymize( ).
  ENDLOOP.

  LOOP AT me->packages INTO o_raw_p.
    o_package ?= o_raw_p-zl_object->raw.
    o_package->anonymize( ).
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  DATA o_raw_i     LIKE LINE OF me->interfaces.
  DATA o_interface TYPE to_interface.
  DATA o_raw_p     LIKE LINE OF me->packages.
  DATA o_package   TYPE to_mykind.

  If me->a0_maindata-created_by is INITIAL. me->a0_maindata-created_by = sy-uname. endif.
  If me->a0_maindata-created_on is INITIAL. me->a0_maindata-created_on = sy-datum. endif.
  If me->a0_maindata-as4user is INITIAL. me->a0_maindata-as4user = sy-uname. endif. " Person responsible

  LOOP AT me->interfaces INTO o_raw_i.
    o_interface ?= o_raw_i-zl_object->raw.
    o_interface->unanonymize( ).
  ENDLOOP.

  LOOP AT me->packages INTO o_raw_p.
    o_package ?= o_raw_p-zl_object->raw.
    o_package->unanonymize( ).
  ENDLOOP.
  endmethod.
ENDCLASS.
