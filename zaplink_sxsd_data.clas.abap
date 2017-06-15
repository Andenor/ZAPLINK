class ZAPLINK_SXSD_DATA definition
  public
  inheriting from ZAPLINK_BADI_DATA
  create public

  global friends ZAPLINK_BADI
                 ZAPLINK_EASYXML .

public section.
  type-pools SEEX .

  types:
    BEGIN OF ts_maindata.
  INCLUDE TYPE badi_data AS main.
  TYPES:
      mast_langu  TYPE  sy-langu,
      ext_clname  TYPE  seoclsname,
    END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
  data INTERFACE type TO_RAW .

  methods CONSTRUCTOR .

  methods ANONYMIZE
    redefinition .
  methods FROM_DATA
    redefinition .
  methods TO_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_SXSD_DATA IMPLEMENTATION.


  method ANONYMIZE.
* Issue 81
  DATA o_conn    TYPE to_connector.
  DATA o_cleaner TYPE REF TO zaplink_cnx_ext_cleaner.
  CLEAR: a0_maindata-uname, a0_maindata-udate, a0_maindata-utime.
  IF interface IS BOUND.
    o_conn = zaplink_connectors=>create_connector( type = interface->type
                                               connuuid = interface->connector ).
    TRY.
        o_cleaner ?= o_conn.
        o_cleaner->anonymize( interface ).
      CATCH cx_root.
    ENDTRY.
  ENDIF.
  TRY.
      super->anonymize( ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method CONSTRUCTOR.
  super->constructor( ).
*  DATA o_conn   TYPE to_connector.
*  DATA o_sxsd   TYPE to_badi_def.
*  DATA o_sxci   TYPE to_badi_impl.
*  DATA _type    TYPE td_comptype.
*
*  CREATE OBJECT object.
*  CASE type.
*    WHEN st_badi_def.
*      CREATE OBJECT o_sxsd.
** Create empty interface object
*      o_conn = zaplink_connectors=>create_connector( type = t_badi_itf ).
*      IF o_conn IS BOUND.
*        _type = t_badi_itf.
*        o_sxsd->interface = o_conn->create_new_raw( _type ).
*      ENDIF.
*      object->raw = o_sxsd.
**      create object object type TO_CLASS.
  endmethod.


  method FROM_DATA.
  DATA _fc    LIKE LINE OF functions_codes.
  DATA _cc    LIKE LINE OF controls_composites.
  DATA _scr   LIKE LINE OF subscreens.
  DATA _t     LIKE LINE OF tables.
*  DATA o_conn TYPE to_connector.
*  CONSTANTS c_type TYPE td_comptype VALUE 'INTF'.
*  DATA _comp TYPE to_component.
  FIELD-SYMBOLS:
    <fc>  LIKE LINE OF fm_data-fcodes,
    <cc>  LIKE LINE OF fm_data-cocos,
    <in>  LIKE LINE OF fm_data-intas,
    <scr> LIKE LINE OF fm_data-scrns,
    <me>  LIKE LINE OF fm_data-methods,
    <it>  LIKE LINE OF fm_data-inactive_tabstrips.

  a0_maindata-main = fm_data-definition.
  a0_maindata-mast_langu = fm_data-mast_langu.
  a0_maindata-ext_clname = fm_data-ext_clname.

  LOOP AT fm_data-intas ASSIGNING <in>
    WHERE exit_name = a0_maindata-exit_name.
    CLEAR _t.
    _t = <in>.

    CLEAR: _t-exit_name. " VERSION
    INSERT _t INTO TABLE tables.
  ENDLOOP.

  LOOP AT fm_data-cocos ASSIGNING <cc>
    WHERE exit_name = a0_maindata-exit_name.
    CLEAR _cc.
    _cc = <cc>.

    CLEAR: _cc-exit_name. " VERSION
    INSERT _cc INTO TABLE controls_composites.
  ENDLOOP.

  LOOP AT fm_data-fcodes ASSIGNING <fc>
    WHERE exit_name = a0_maindata-exit_name.
    CLEAR _fc.
    _fc = <fc>.

    CLEAR: _fc-exit_name. " VERSION
    INSERT _fc INTO TABLE functions_codes.
  ENDLOOP.

  LOOP AT fm_data-scrns ASSIGNING <scr>
    WHERE exit_name = a0_maindata-exit_name.
    CLEAR _scr.
    _scr = <scr>.

    CLEAR: _scr-exit_name. " VERSION
    INSERT _scr INTO TABLE subscreens.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA _in  LIKE LINE OF fm_data-intas.
  DATA _cc  LIKE LINE OF fm_data-cocos.
  DATA _fc  LIKE LINE OF fm_data-fcodes.
  DATA _scr LIKE LINE OF fm_data-scrns.
  FIELD-SYMBOLS:
    <t>   LIKE LINE OF tables,
    <scr> LIKE LINE OF subscreens,
    <fc>  LIKE LINE OF functions_codes,
    <cc>  LIKE LINE OF controls_composites.

*CLASS  TYPE  VSEOCLASS
  fm_data-definition = a0_maindata-main.
  fm_data-mast_langu = a0_maindata-mast_langu.
  fm_data-ext_clname = a0_maindata-ext_clname.

  LOOP AT tables ASSIGNING <t>.
    CLEAR _in.
    _in = <t>.

    _in-exit_name = a0_maindata-exit_name.
    INSERT _in INTO TABLE fm_data-intas.
  ENDLOOP.

  LOOP AT controls_composites ASSIGNING <cc>.
    CLEAR _cc.
    _cc = <cc>.

    _cc-exit_name = a0_maindata-exit_name.
    INSERT _cc INTO TABLE fm_data-cocos.
  ENDLOOP.

  LOOP AT functions_codes ASSIGNING <fc>.
    CLEAR _fc.
    _fc = <fc>.

    _fc-exit_name = a0_maindata-exit_name.
    INSERT _fc INTO TABLE fm_data-fcodes.
  ENDLOOP.

  LOOP AT subscreens ASSIGNING <scr>.
    CLEAR _scr.
    _scr = <scr>.

    _scr-exit_name = a0_maindata-exit_name.
    INSERT _scr INTO TABLE fm_data-scrns.
  ENDLOOP.
  endmethod.
ENDCLASS.
