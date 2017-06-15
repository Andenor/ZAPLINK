class ZAPLINK_TYPE_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  final
  create public

  global friends ZAPLINK_DICTIONARY
                 ZAPLINK_EASYXML .

public section.

  types:
    BEGIN OF ts_data,
        name      TYPE typegroup,
        lang      TYPE langu,
        text      TYPE ddtext,
        devclass  TYPE devclass,
        uccheck   TYPE uccheck,
      END OF ts_data .
  types:
    tt_abapsource type standard table of ABAPSOURCE with DEFAULT KEY .
  types:
    tt_string type standard table of ABAPSOURCE with DEFAULT KEY .

  data A0_MAINDATA type TS_DATA .
  data SOURCE type STRING read-only .

  methods SET_SOURCE
    importing
      !SOURCE type STANDARD TABLE .
  methods GET_SOURCE
    returning
      value(SOURCE) type TT_ABAPSOURCE .
  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_TYPE_DATA IMPLEMENTATION.


  method ANONYMIZE.
* Nothing to do
  endmethod.


  method GET_SOURCE.
SPLIT me->source AT cl_abap_char_utilities=>newline
        INTO TABLE source.
  endmethod.


  method SET_SOURCE.
  me->source = zaplink_tools=>table_2_string( source ).
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
