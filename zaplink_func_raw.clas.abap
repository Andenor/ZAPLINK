class ZAPLINK_FUNC_RAW definition
  public
  inheriting from ZAPLINK_RAW
  create public

  global friends ZAPLINK_EASYXML .

public section.

  methods CONSTRUCTOR
    importing
      !FM_DATA type ZAPLINK_FUNC_DATA=>TS_FM_DATA optional
    raising
      ZAPLINK_CX .
  methods TO_DATA
    returning
      value(FM_DATA) type ZAPLINK_FUNC_DATA=>TS_FM_DATA .

  methods ANONYMIZE
    redefinition .
protected section.
private section.

  types TO_MYDATA type ref to ZAPLINK_FUNC_DATA .

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_FUNC_RAW IMPLEMENTATION.


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
  DATA o_comp  TYPE to_component.
  DATA o_raw   TYPE TO_MYDATA.
  DATA o_conn  TYPE to_connector.

  IF fm_data IS INITIAL.    " create from ZL_EZXML
    CREATE OBJECT o_comp.
  ELSE.
    CREATE OBJECT o_comp.
    o_comp->set_type( zaplink_function_group=>supportedtypes-function ).   o_comp->set_name( fm_data-header-name ).
  ENDIF.

  CALL METHOD super->constructor
    EXPORTING
      o_comp = o_comp.

  CREATE OBJECT o_raw.
  raw ?= o_raw.
  IF NOT fm_data IS INITIAL.    o_raw->from_data( fm_data ).      CLEAR: o_raw->a0_maindata-name.     ENDIF.

  CREATE OBJECT o_conn TYPE zaplink_function_group.
  update_connector_data( o_conn ).
  endmethod.


  method TO_DATA.
  DATA o_raw   TYPE TO_MYDATA.

  o_raw ?= raw.
  o_raw->a0_maindata-name = me->name.
  fm_data = o_raw->to_data( ).
  endmethod.
ENDCLASS.
