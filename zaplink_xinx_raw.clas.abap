class ZAPLINK_XINX_RAW definition
  public
  inheriting from ZAPLINK_RAW
  create public

  global friends ZAPLINK_EASYXML .

public section.

  methods CONSTRUCTOR
    importing
      !FM_DATA type ZAPLINK_XINX_DATA=>TS_FM_DATA optional
    raising
      ZAPLINK_CX .
  methods TO_DATA
    returning
      value(FM_DATA) type ZAPLINK_XINX_DATA=>TS_FM_DATA .

  methods ANONYMIZE
    redefinition .
protected section.
private section.

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_XINX_RAW IMPLEMENTATION.


  method ANONYMIZE.
  DATA o_raw   TYPE REF TO zaplink_xinx_data.

  if raw IS BOUND.    o_raw ?= raw.   o_raw->anonymize( ).    endif.
*  TRY.
      CALL METHOD super->anonymize.
*    CATCH zaplink_cx_container INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method CONSTRUCTOR.
  DATA s_index TYPE zaplink_xinx_data=>ts_idx_key.
  DATA o_comp  TYPE to_component.
  DATA o_raw   TYPE REF TO zaplink_xinx_data.
  DATA s_key   TYPE ts_compkey.
  DATA o_conn  TYPE to_connector.

  IF fm_data IS INITIAL.    " create from ZL_EZXML
    CREATE OBJECT o_comp.
  ELSE.
    s_index-table = fm_data-header-sqltab.
    s_index-index = fm_data-header-indexname.
    s_key-name = zaplink_xinx_data=>key_2_name( s_index ).
    IF fm_data-header-isextind IS INITIAL.
      s_key-type = zaplink_dictionary=>supportedtypes-table_index.
    ELSE.
      s_key-type = zaplink_dictionary=>supportedtypes-ext_index.
    ENDIF.
    CREATE OBJECT o_comp.
    o_comp->set_type( s_key-type ).   o_comp->set_name( s_key-name ).
  ENDIF.

  CALL METHOD super->constructor
    EXPORTING
      o_comp = o_comp.

  CREATE OBJECT o_raw.
  raw ?= o_raw.
  IF NOT fm_data IS INITIAL.    o_raw->from_data( fm_data ).    CLEAR: o_raw->a0_maindata-sqltab, o_raw->a0_maindata-indexname, o_raw->a0_maindata-isextind.    ENDIF.

  CREATE OBJECT o_conn TYPE zaplink_dictionary.
  update_connector_data( o_conn ).
  endmethod.


  method TO_DATA.
  DATA s_index TYPE zaplink_xinx_data=>ts_idx_key.
  DATA o_raw   TYPE REF TO zaplink_xinx_data.

  o_raw ?= raw.
  s_index = zaplink_xinx_data=>name_2_key( name ).
  o_raw->a0_maindata-sqltab = s_index-table.
  o_raw->a0_maindata-indexname = s_index-index.
  IF type = zaplink_dictionary=>supportedtypes-ext_index.   o_raw->a0_maindata-isextind = abap_true.    ENDIF.

  fm_data = o_raw->to_data( ).
  endmethod.
ENDCLASS.
