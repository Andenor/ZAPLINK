class ZAPLINK_VARI_RAW definition
  public
  inheriting from ZAPLINK_RAW
  create public

  global friends ZAPLINK_EASYXML .

public section.

  types TS_FM_DATA type ZAPLINK_VARI_DATA=>TS_FM_DATA .

  methods CONSTRUCTOR
    importing
      !FM_DATA type TS_FM_DATA optional
    raising
      ZAPLINK_CX .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .

  methods ANONYMIZE
    redefinition .
protected section.
private section.

  types TO_MYDATA type ref to ZAPLINK_VARI_DATA .

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_VARI_RAW IMPLEMENTATION.


  method ANONYMIZE.
  DATA o_raw   TYPE to_mydata.

  IF raw IS BOUND.    o_raw ?= raw.   o_raw->anonymize( ).    ENDIF.
*  TRY.
  CALL METHOD super->anonymize.
*    CATCH zaplink_cx_container INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method CONSTRUCTOR.
  DATA o_comp     TYPE to_component.
  DATA o_raw      TYPE to_mydata.
  DATA o_conn     TYPE to_connector.
  DATA s_variant  TYPE zaplink_vari_data=>ts_variant_key.
  DATA d_name     TYPE td_compname.

  IF fm_data IS INITIAL.    " create from ZL_EZXML
    CREATE OBJECT o_comp.
  ELSE.
    CREATE OBJECT o_comp.
    IF fm_data-header-variant CP zaplink_vari_data=>variant_prefix-system OR fm_data-header-variant CP zaplink_vari_data=>variant_prefix-customer.
      o_comp->set_type( zaplink_program=>supportedtypes-sys_variant ).
    ELSE.
      o_comp->set_type( zaplink_program=>supportedtypes-appl_variant ).
    ENDIF.
    s_variant-program = fm_data-header-report.
    s_variant-variant = fm_data-header-variant.
    d_name = zaplink_vari_data=>key_2_name( s_variant ).
    o_comp->set_name( d_name ).
  ENDIF.

  CALL METHOD super->constructor
    EXPORTING
      o_comp = o_comp.

  CREATE OBJECT o_raw.
  raw ?= o_raw.
  IF NOT fm_data IS INITIAL.    o_raw->from_data( fm_data ).      CLEAR: o_raw->a0_maindata-report, o_raw->a0_maindata-variant.     ENDIF.

  CREATE OBJECT o_conn TYPE zaplink_program.
  update_connector_data( o_conn ).
  endmethod.


  method TO_DATA.
  DATA o_raw      TYPE to_mydata.
  DATA s_variant  TYPE zaplink_vari_data=>ts_variant_key.
  DATA d_name     TYPE td_compname.

  s_variant = zaplink_vari_data=>name_2_key( me->name ).
  o_raw ?= raw.
  o_raw->a0_maindata-report = s_variant-program.
  o_raw->a0_maindata-variant = s_variant-variant.
  fm_data = o_raw->to_data( ).
  endmethod.
ENDCLASS.
