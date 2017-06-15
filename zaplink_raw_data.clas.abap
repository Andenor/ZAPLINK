class ZAPLINK_RAW_DATA definition
  public
  abstract
  create public .

public section.

  data _DEPENDENCIES type ZAPLINK_DATATYPES=>TT_COMPKEYS .
  data _CODE_SIGNATURE type ZAPLINK_DATATYPES=>TD_CHECKSUM .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_RAW_DATA IMPLEMENTATION.
ENDCLASS.
