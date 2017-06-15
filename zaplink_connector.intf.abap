interface ZAPLINK_CONNECTOR
  public .

  type-pools ABAP .

  interfaces ZAPLINK_DATATYPES .

  aliases BALLOG_OBJECT
    for ZAPLINK_DATATYPES~BALLOG_OBJECT .
  aliases COMP_NODENAME
    for ZAPLINK_DATATYPES~COMP_NODENAME .
  aliases EXISTS
    for ZAPLINK_DATATYPES~EXISTS .
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
  aliases TD_CONNUUID
    for ZAPLINK_DATATYPES~TD_CONNUUID .
  aliases TD_CONNVER
    for ZAPLINK_DATATYPES~TD_CONNVER .
  aliases TO_XML
    for ZAPLINK_DATATYPES~TO_XML .
  aliases TS_BASE_ATTRIBUTS
    for ZAPLINK_DATATYPES~TS_BASE_ATTRIBUTS .
  aliases TS_CONNDATA
    for ZAPLINK_DATATYPES~TS_CONNDATA .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  types TI_CLEANNING type ref to ZAPLINK_CNX_EXT_CLEANER .
  types TI_CLEANNING_4DATA type ref to ZAPLINK_CNX_EXT_CLEANER_4DATA .
  types TO_EZ_XML type ref to ZAPLINK_EASYXML_4CNX .
  types TO_OPTIONS type ref to ZAPLINK_OPTIONS .
  types TO_RAW type ref to ZAPLINK_RAW .
  types TO_RAW_BASE type ref to ZAPLINK_RAW_BASE .
  types TO_RAW_DATA type ref to ZAPLINK_RAW_DATA .
  types TO_LIST type ref to ZAPLINK_LIST .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types TO_MSG_COLL type ref to ZAPLINK_MESSAGE_COLLECTOR .

  data SUPPORTED_TYPES type TT_TYPES read-only .
  data APPLICATION_LOG type TO_MSG_COLL read-only .
  data VERSION type TD_CONNVER read-only .
  data OPTIONS type TO_OPTIONS .
  data UUID type TD_CONNUUID read-only .

  methods DO_EXISTS
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(EXISTS) type TD_COMPEXISTS
    raising
      ZAPLINK_CX .
  methods GET_VALUES
    importing
      !TYPE type TD_COMPTYPE
    changing
      !NAME type TD_COMPNAME
    raising
      ZAPLINK_CX .
  methods IMPORT_TO_SAP
    importing
      !OBJECT type TO_XML
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX .
  methods EXPORT_FROM_SAP
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_XML
    raising
      ZAPLINK_CX .
  methods DELETE_FROM_SAP
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX .
  methods WRITE_TO_SAP
    importing
      !OBJECT type TO_RAW
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX .
  methods READ_FROM_SAP
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_RAW
    raising
      ZAPLINK_CX .
  methods CREATE_NEW_RAW
    importing
      !TYPE type TD_COMPTYPE
    returning
      value(OBJECT) type TO_RAW
    raising
      ZAPLINK_CX .
  methods IS_SUPPORTED_VERSION
    importing
      !VERSION type TD_CONNVER
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX .
endinterface.
