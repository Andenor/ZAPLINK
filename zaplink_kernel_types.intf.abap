interface ZAPLINK_KERNEL_TYPES
  public .


  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .
  types TO_CONTAINER type ref to ZAPLINK_CONTAINER .
  types TO_FILE type ref to ZAPLINK_FILE .
  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_MSG_COLL type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TO_OPTIONS type ref to ZAPLINK_OPTIONS .
  types TO_OPT_DEVCLASS type ref to ZAPLINK_OPT_DEVCLASS .
  types TO_OPT_DIRECTORY type ref to ZAPLINK_OPT_DIRECTORY .
  types TO_OPT_TRANSPORT_REQUEST type ref to ZAPLINK_OPT_TRANSPORT_REQUEST .
  types TO_RAW type ref to ZAPLINK_RAW .
  types TO_RAW_BASE type ref to ZAPLINK_RAW_BASE .

  constants BALLOG_OBJECT type BALOBJ_D value 'ZAPLINK' ##NO_TEXT.
endinterface.
