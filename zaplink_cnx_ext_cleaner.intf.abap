interface ZAPLINK_CNX_EXT_CLEANER
  public .

  type-pools ABAP .

  types TO_RAW type ref to ZAPLINK_RAW .

  methods ANONYMIZE
    importing
      !OBJECT type TO_RAW
    raising
      ZAPLINK_CX .
  methods UNANONYMIZE
    importing
      !OBJECT type TO_RAW
    raising
      ZAPLINK_CX .
endinterface.
