class ZAPLINK_PINF_RAW definition
  public
  inheriting from ZAPLINK_RAW
  create public

  global friends ZAPLINK_EASYXML .

public section.

  methods CONSTRUCTOR
    importing
      !O_COMP type TO_COMPONENT optional .

  methods ANONYMIZE
    redefinition .
protected section.
private section.

  types TO_MYDATA type ref to ZAPLINK_PINF_DATA .

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_PINF_RAW IMPLEMENTATION.


  method ANONYMIZE.
  DATA o_raw   TYPE to_mydata.

  IF raw IS BOUND.    o_raw ?= raw.   o_raw->anonymize( ).    ENDIF.
*  TRY.
  super->anonymize( ).
*    CATCH zaplink_cx_container INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method CONSTRUCTOR.
  DATA o_raw TYPE to_mydata.
  CALL METHOD super->constructor
    EXPORTING
      o_comp = o_comp.
  CREATE OBJECT o_raw.
  raw ?= o_raw.
  endmethod.
ENDCLASS.
