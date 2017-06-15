class ZAPLINK_DOCV_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_DOCUMENTATION .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TS_HEAD
    for ZAPLINK_DATATYPES~TS_HEAD .

  types:
    tt_text TYPE STANDARD TABLE OF tline WITH DEFAULT KEY .
  types:
    begin of ts_text,
        Langu type sy-langu,
        _     type string,      " Text
      end of ts_text .
  types:
    tt_texts type SORTED TABLE OF ts_text with UNIQUE key langu .
  types:
    tt_rawtext TYPE STANDARD TABLE OF tline WITH DEFAULT KEY .

  data A0_MAINDATA type TS_HEAD .
  data TEXTS type TT_TEXTS .
protected section.

  aliases TS_DOC
    for ZAPLINK_DATATYPES~TS_DOC .

  methods FROM_DATA
    importing
      !FM_DATA type TS_DOC .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_DOC .
private section.
ENDCLASS.



CLASS ZAPLINK_DOCV_DATA IMPLEMENTATION.


  method FROM_DATA.
  DATA s_text LIKE LINE OF texts.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF fm_data-texts.

  a0_maindata = fm_data-hdr.    a0_maindata-masterlang = a0_maindata-langu.   CLEAR a0_maindata-langu.
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_text.   s_text-langu = <t>-tdspras.   s_text-_ = <t>-_.   INSERT s_text INTO TABLE texts.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_text LIKE LINE OF fm_data-texts.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF texts.

  fm_data-hdr = a0_maindata.    fm_data-hdr-langu = a0_maindata-masterlang. fm_data-hdr-masterlang = abap_true.
  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.   s_text-tdspras = <t>-langu.   s_text-_ = <t>-_.   INSERT s_text INTO TABLE fm_data-texts.
  ENDLOOP.
  endmethod.
ENDCLASS.
