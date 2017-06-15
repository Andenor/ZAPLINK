class ZAPLINK_EXCC_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_OS_COMMAND .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .

  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE SXPGCOLIST.
    TYPES:
      END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_EXCC_DATA IMPLEMENTATION.
ENDCLASS.
