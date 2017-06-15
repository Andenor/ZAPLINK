interface ZAPLINK_CNX_EXT_CODE_SIGNATURE
  public .

  type-pools ABAP .

  interfaces ZAPLINK_DATATYPES .

  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .

  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .

  methods GET_SIGNATURE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(RESULT) type TD_CHECKSUM
    raising
      ZAPLINK_CX .
endinterface.
