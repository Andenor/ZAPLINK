class ZAPLINK_BADI_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create protected

  global friends ZAPLINK_BADI
                 ZAPLINK_EASYXML .

public section.
  type-pools SEEX .

  interfaces ZAPLINK_DATATYPES .

  aliases EXISTS
    for ZAPLINK_DATATYPES~EXISTS .
  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TEXTPOOL_KINDS
    for ZAPLINK_DATATYPES~TEXTPOOL_KINDS .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_CLASSNAME
    for ZAPLINK_DATATYPES~TD_CLASSNAME .
  aliases TD_COMPEXISTS
    for ZAPLINK_DATATYPES~TD_COMPEXISTS .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_DATATYPES~TD_CONNCLASS .
  aliases TD_CONNEXISTS
    for ZAPLINK_DATATYPES~TD_CONNEXISTS .
  aliases TD_CONNVER
    for ZAPLINK_DATATYPES~TD_CONNVER .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_EXISTS
    for ZAPLINK_DATATYPES~TD_EXISTS .
  aliases TD_LANG
    for ZAPLINK_DATATYPES~TD_LANG .
  aliases TD_LIGHT
    for ZAPLINK_DATATYPES~TD_LIGHT .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .
  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TD_TXTP_ID
    for ZAPLINK_DATATYPES~TD_TXTP_ID .
  aliases TD_TXTP_KIND
    for ZAPLINK_DATATYPES~TD_TXTP_KIND .
  aliases TD_TXTP_LEN
    for ZAPLINK_DATATYPES~TD_TXTP_LEN .
  aliases TD_TXTP_TEXT
    for ZAPLINK_DATATYPES~TD_TXTP_TEXT .
  aliases TO_XML
    for ZAPLINK_DATATYPES~TO_XML .
  aliases TR_DLVUNIT
    for ZAPLINK_DATATYPES~TR_DLVUNIT .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TR_TR
    for ZAPLINK_DATATYPES~TR_TR .
  aliases TS_BASE_ATTRIBUTS
    for ZAPLINK_DATATYPES~TS_BASE_ATTRIBUTS .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TS_CONN_CLASS
    for ZAPLINK_DATATYPES~TS_CONN_CLASS .
  aliases TS_CONN_DEF
    for ZAPLINK_DATATYPES~TS_CONN_DEF .
  aliases TS_TXTP_TEXT
    for ZAPLINK_DATATYPES~TS_TXTP_TEXT .
  aliases TS_TXTP_TEXTPOOL
    for ZAPLINK_DATATYPES~TS_TXTP_TEXTPOOL .
  aliases TS_TYPE
    for ZAPLINK_DATATYPES~TS_TYPE .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .
  aliases TT_CONN_CLASSES
    for ZAPLINK_DATATYPES~TT_CONN_CLASSES .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
  aliases TT_TXTP_TEXTS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTS .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  types:
    tt_functions TYPE SORTED TABLE OF seex_fcode_struct WITH UNIQUE KEY gui_prog gui_code .
  types:
    tt_controls TYPE SORTED TABLE OF seex_coco_struct WITH UNIQUE KEY coco_outer node_id .
  types:
    tt_screens TYPE SORTED TABLE OF seex_screen_struct WITH UNIQUE KEY scr_a_prog scr_a_num scr_a_sscr .
  types:
    tt_tables TYPE SORTED TABLE OF seex_table_struct WITH UNIQUE KEY tabname .
  types:
    BEGIN OF t_fm_data,
* Commun
          mast_langu      TYPE  sy-langu,
          fcodes          TYPE  seex_fcode_table,
          cocos           TYPE  seex_coco_table,
          intas           TYPE  seex_table_table,
          scrns           TYPE  seex_screen_table,
*        methods         TYPE  seex_mtd_table,        not used on save
* Definition Specific
          definition      TYPE  badi_data,
          inactive_tabstrips  TYPE  seex_inactive_tabstrips,
          ext_clname      TYPE  seoclsname,
          filter_obj      TYPE REF TO cl_badi_flt_struct,
          methods     TYPE  seex_mtd_table,
* Implementation Specific
          implementation  TYPE impl_data,
          impl_w_filter   TYPE flt_ext,  " Filter active
          filter_datatype TYPE rollname,
          filter_values   TYPE SEEX_FILTER_TABLE,    "REF TO cl_badi_flt_values_alv,
        END OF t_fm_data .

  data FUNCTIONS_CODES type TT_FUNCTIONS .
  data CONTROLS_COMPOSITES type TT_CONTROLS .
  data SUBSCREENS type TT_SCREENS .
  data TABLES type TT_TABLES .
  data DOCUMENTATION type TT_DOCS .

  methods FROM_DATA
    importing
      !FM_DATA type T_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type T_FM_DATA .
  methods ANONYMIZE
    raising
      ZAPLINK_CX .
  methods UNANONYMIZE .
protected section.

  types TO_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .
  types TO_RAW type ref to ZAPLINK_RAW .
  types TO_CONNECTOR type ZAPLINK_CONNECTORS=>TO_CONNECTOR .

  data DEF_NAME type EXIT_DEF .
  class-data O_MYCX type TO_EXCEPTION .
private section.
ENDCLASS.



CLASS ZAPLINK_BADI_DATA IMPLEMENTATION.


  method ANONYMIZE.
  endmethod.


  method FROM_DATA.
* Handle Commun data
  DATA _fc    LIKE LINE OF functions_codes.
  DATA _cc    LIKE LINE OF controls_composites.
  DATA _scr   LIKE LINE OF subscreens.
*  DATA _me    LIKE LINE OF methods.
  DATA _t     LIKE LINE OF tables.
*  DATA o_conn TYPE td_connector.
*  CONSTANTS c_type TYPE td_comptype VALUE 'INTF'.
*  DATA _comp TYPE ts_component.
  FIELD-SYMBOLS:
    <fc>  LIKE LINE OF fm_data-fcodes,
    <cc>  LIKE LINE OF fm_data-cocos,
    <in>  LIKE LINE OF fm_data-intas,
    <scr> LIKE LINE OF fm_data-scrns,
*    <me>  LIKE LINE OF fm_data-methods,
    <it>  LIKE LINE OF fm_data-inactive_tabstrips.

* Commun
*        mast_langu      TYPE  sy-langu,
*        fcodes          TYPE  seex_fcode_table,
*        cocos           TYPE  seex_coco_table,
*        intas           TYPE  seex_table_table,
*        scrns           TYPE  seex_screen_table,
*        methods         TYPE  seex_mtd_table,

  LOOP AT fm_data-fcodes ASSIGNING <fc>
    WHERE exit_name = def_name.
    CLEAR _fc.
    _fc = <fc>.

    CLEAR: _fc-exit_name. " VERSION
    INSERT _fc INTO TABLE functions_codes.
  ENDLOOP.

  LOOP AT fm_data-cocos ASSIGNING <cc>
    WHERE exit_name = def_name.
    CLEAR _cc.
    _cc = <cc>.

    CLEAR: _cc-exit_name. " VERSION
    INSERT _cc INTO TABLE controls_composites.
  ENDLOOP.

  LOOP AT fm_data-intas ASSIGNING <in>
    WHERE exit_name = def_name.
    CLEAR _t.
    _t = <in>.

    CLEAR: _t-exit_name. " VERSION
    INSERT _t INTO TABLE tables.
  ENDLOOP.

  LOOP AT fm_data-scrns ASSIGNING <scr>
    WHERE exit_name = def_name.
    CLEAR _scr.
    _scr = <scr>.

    CLEAR: _scr-exit_name. " VERSION
    INSERT _scr INTO TABLE subscreens.
  ENDLOOP.

*  LOOP AT fm_data-methods ASSIGNING <me>
*    WHERE exit_name = def_name.
*    CLEAR _me.
*    _me = <me>.
*
*    CLEAR: _me-exit_name. " VERSION
*    INSERT _me INTO TABLE methods.
*  ENDLOOP.
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

* Commun
*        mast_langu      TYPE  sy-langu,
*        fcodes          TYPE  seex_fcode_table,
*        cocos           TYPE  seex_coco_table,
*        intas           TYPE  seex_table_table,
*        scrns           TYPE  seex_screen_table,
*        methods         TYPE  seex_mtd_table,

  LOOP AT functions_codes ASSIGNING <fc>.
    CLEAR _fc.
    _fc = <fc>.

    _fc-exit_name = def_name.
    INSERT _fc INTO TABLE fm_data-fcodes.
  ENDLOOP.

  LOOP AT controls_composites ASSIGNING <cc>.
    CLEAR _cc.
    _cc = <cc>.

    _cc-exit_name = def_name.
    INSERT _cc INTO TABLE fm_data-cocos.
  ENDLOOP.

  LOOP AT tables ASSIGNING <t>.
    CLEAR _in.
    _in = <t>.

    _in-exit_name = def_name.
    INSERT _in INTO TABLE fm_data-intas.
  ENDLOOP.

  LOOP AT subscreens ASSIGNING <scr>.
    CLEAR _scr.
    _scr = <scr>.

    _scr-exit_name = def_name.
    INSERT _scr INTO TABLE fm_data-scrns.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
