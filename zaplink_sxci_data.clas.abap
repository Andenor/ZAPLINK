class ZAPLINK_SXCI_DATA definition
  public
  inheriting from ZAPLINK_BADI_DATA
  create public

  global friends ZAPLINK_BADI
                 ZAPLINK_EASYXML .

public section.
  type-pools SEEX .

  types:
    BEGIN OF ts_maindata.
  INCLUDE TYPE impl_data AS main.
  TYPES:
      mast_langu  TYPE  sy-langu,
    END OF ts_maindata .
  types:
    BEGIN OF ts_filter,
      enhanceable TYPE rsexscrn-flt_ext,
      datatype TYPE	rsexscrn-flt_type,
*      Object   TYPE REF TO CL_BADI_FLT_VALUES_ALV,
*  data FILTER_VALUES type SEEX_FILTER_TABLE .
      values TYPE seex_filter_table,
    END OF ts_filter .

  data A0_MAINDATA type TS_MAINDATA .
  data FILTER type TS_FILTER .
  data IMPLEMENTATION type TO_RAW .

  methods ANONYMIZE
    redefinition .
  methods FROM_DATA
    redefinition .
  methods TO_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_SXCI_DATA IMPLEMENTATION.


  method ANONYMIZE.
* Issue 81
  DATA o_conn    TYPE to_connector.
  DATA o_cleaner TYPE REF TO zaplink_cnx_ext_cleaner.
  CLEAR: a0_maindata-uname, a0_maindata-udate, a0_maindata-utime,a0_maindata-aname, a0_maindata-adate, a0_maindata-atime.
  IF implementation IS BOUND.
    o_conn = zaplink_connectors=>create_connector( type = implementation->type
                                               connuuid = implementation->connector ).
    TRY.
        o_cleaner ?= o_conn.
        o_cleaner->anonymize( implementation ).
      CATCH cx_root.
    ENDTRY.
  ENDIF.
*  TRY.
  super->anonymize( ).
*    CATCH zaplink_cx_connector INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method FROM_DATA.
  def_name = fm_data-implementation-exit_name.

  CALL METHOD super->from_data
    EXPORTING
      fm_data = fm_data.

  a0_maindata-main = fm_data-implementation.
  a0_maindata-mast_langu = fm_data-mast_langu.
  filter-enhanceable = fm_data-impl_w_filter.
  filter-datatype = fm_data-filter_datatype.
  filter-values = fm_data-filter_values.
  endmethod.


  method TO_DATA.
def_name = a0_maindata-exit_name.

* Warning recieving (will reset FM_DATA)
  CALL METHOD super->to_data
    RECEIVING
      fm_data = fm_data.

  fm_data-implementation = a0_maindata-main.
  fm_data-mast_langu = a0_maindata-mast_langu.
*  READ TABLE a0_maindata-texts INTO _t
*       WITH TABLE KEY langu = a0_maindata-langu.
*  IF sy-subrc = 0.
*    fm_data-implementation-descript = _t-descript.
*  ENDIF.

*CLASS_DESC  TYPE tt_SEOCLASSTX,
*  fm_data-class_desc = a0_maindata-texts.
*  _t-clsname = a0_maindata-clsname.
*  MODIFY fm_data-class_desc FROM _t TRANSPORTING clsname WHERE clsname <> _t-clsname.

  fm_data-impl_w_filter = filter-enhanceable.
  fm_data-filter_datatype = filter-datatype.
  fm_data-filter_values = filter-values.
  endmethod.
ENDCLASS.
