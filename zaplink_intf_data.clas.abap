class ZAPLINK_INTF_DATA definition
  public
  inheriting from ZAPLINK_OBJECT_DATA
  final
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_OBJECT .

public section.
  type-pools SEOF .
  type-pools SEOK .
  type-pools SEOO .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .

  types:
    BEGIN OF ts_main_data.
    INCLUDE TYPE vseointerf AS hdr.
    TYPES:
        texts       TYPE tt_ltexts,   " tt_seoclasstx_s,
        textpool    TYPE tt_txtp_textpools,
      END OF ts_main_data .

  data A0_MAINDATA type TS_MAIN_DATA .

  methods ANONYMIZE
    redefinition .
  methods COMPLETE_DATA
    redefinition .
  methods FROM_DATA
    redefinition .
  methods TO_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_INTF_DATA IMPLEMENTATION.


  method ANONYMIZE.
  super->anonymize( ).
  CLEAR:
    a0_maindata-author,
    a0_maindata-createdon,
    a0_maindata-changedby,
    a0_maindata-changedon,
    a0_maindata-chgdanyby,
    a0_maindata-chgdanyon,
    a0_maindata-r3release.
  endmethod.


  method COMPLETE_DATA.
* Issue 23 : start
  obj_name = fm_data-interface-clsname.
  super->complete_data( CHANGING fm_data = fm_data ).
* Issue 23 : end
  endmethod.


  method FROM_DATA.
  DATA _t   LIKE LINE OF a0_maindata-texts.
  DATA _in  LIKE LINE OF interfaces.
  FIELD-SYMBOLS:
    <co>  LIKE LINE OF fm_data-comprisings,
    <ec>  LIKE LINE OF fm_data-explore_comprisings.
* added for documentation move
  FIELD-SYMBOLS:
    <desc> LIKE LINE OF fm_data-class_desc,
    <doc>  LIKE LINE OF fm_data-documentation,
    <dt>   LIKE LINE OF <doc>-texts.
  DATA s_txt  LIKE LINE OF a0_maindata-texts.
  DATA s_key  TYPE ts_doc_key.

  a0_maindata-hdr = fm_data-interface.
  obj_name = a0_maindata-clsname.
  obj_version = a0_maindata-version.
  obj_langu = a0_maindata-langu.
  obj_type = object_types-interface.
  CLEAR a0_maindata-uuid.                                   " Issue 59

*CLASS_DESC  TYPE tt_SEOCLASSTX,
* Issue 33 : Documentation connected to text
  a0_maindata-texts = fd_text( descriptions  = fm_data-class_desc
                                   classname = obj_name
                                          id = doc_ids-interface-header
                               documentation = fm_data-documentation ).
  IF NOT a0_maindata-texts IS INITIAL. CLEAR a0_maindata-descript. ENDIF.

* Get all common attributes between Class and Interface
  CALL METHOD super->from_data
    EXPORTING
      fm_data = fm_data.

*COMPRISINGS TYPE  SEOR_COMPRISINGS_R
  LOOP AT fm_data-comprisings ASSIGNING <co>
    WHERE clsname = a0_maindata-clsname.
    CLEAR _in.
    MOVE-CORRESPONDING <co> TO _in.

    CLEAR _in-clsname.
    INSERT _in INTO TABLE interfaces.
  ENDLOOP.
  clear_tags( ).
  endmethod.


  method TO_DATA.
  DATA _t   LIKE LINE OF a0_maindata-texts.
  DATA _ct  LIKE LINE OF fm_data-class_desc.
  DATA _co  LIKE LINE OF fm_data-comprisings.
  DATA _ec  LIKE LINE OF fm_data-explore_comprisings.
  DATA _d   LIKE LINE OF documentation.
  DATA _dt  LIKE LINE OF _d-texts.
  FIELD-SYMBOLS:
    <t>   LIKE LINE OF a0_maindata-texts,
    <in>  LIKE LINE OF interfaces.

  obj_name = a0_maindata-clsname.
  obj_version = a0_maindata-version.
  obj_langu = a0_maindata-langu.
  obj_type = object_types-interface.

* Warning recieving (will reset FM_DATA)
  CALL METHOD super->to_data
    RECEIVING
      fm_data = fm_data.

  fm_data-interface = a0_maindata-hdr.
  READ TABLE a0_maindata-texts INTO _t
       WITH TABLE KEY lang = a0_maindata-langu.
  IF sy-subrc = 0.    fm_data-interface-descript = _t-text.    ENDIF.

*CLASS_DESC  TYPE tt_SEOCLASSTX,
* Issue 33 : Documentation connected to text
  CALL METHOD td_text
    EXPORTING
      classname     = a0_maindata-clsname
      texts         = a0_maindata-texts
      id            = doc_ids-interface-header
    CHANGING
      documentation = fm_data-documentation
      descriptions  = fm_data-class_desc.

*COMPRISINGS TYPE  SEOR_COMPRISINGS_R
  LOOP AT interfaces ASSIGNING <in>.
    CLEAR: _co.
    MOVE-CORRESPONDING <in> TO _co.
    _co-clsname = a0_maindata-clsname.
    INSERT _co INTO TABLE fm_data-comprisings.
  ENDLOOP.
  clear_tags( ).
  endmethod.
ENDCLASS.
