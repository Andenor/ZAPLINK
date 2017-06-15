class ZAPLINK_RAW definition
  public
  inheriting from ZAPLINK_RAW_BASE
  create public

  global friends ZAPLINK_CONNECTOR
                 ZAPLINK_CONTAINER_4INST
                 ZAPLINK_EASYXML
                 ZAPLINK_RAW .

public section.

  types TO_ME type ref to ZAPLINK_RAW .
  types TO_RAW_DATA type ref to ZAPLINK_RAW_DATA .

  data RAW type TO_RAW_DATA .

  methods CONSTRUCTOR
    importing
      !O_COMP type TO_COMPONENT optional .
  methods COPY_AS
    importing
      !RAW type TO_ME .

  methods UPDATE_CONNECTOR_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_RAW IMPLEMENTATION.


  method CONSTRUCTOR.
  super->constructor( o_comp ).
  endmethod.


  method COPY_AS.
  me->type = raw->type.
  me->name = raw->name.
  me->connector = raw->connector.
  me->version = raw->version.
  me->directory = raw->directory.
  me->checksum = raw->checksum.
  me->dependencies = raw->dependencies.
  me->code_signature = raw->code_signature.
  me->raw = raw->raw.
  endmethod.


  method UPDATE_CONNECTOR_DATA.
  DATA o_raw_data      TYPE to_raw_data.

  o_raw_data = me->raw.
  IF NOT o_raw_data->_dependencies IS INITIAL.    me->dependencies = o_raw_data->_dependencies.     CLEAR o_raw_data->_dependencies.    ENDIF.  " Issue 47 : Introducing dependencies
  IF NOT o_raw_data->_code_signature IS INITIAL.  me->code_signature = o_raw_data->_code_signature. CLEAR o_raw_data->_code_signature.  ENDIF.  " Issue 65 : Source Code signature
  super->update_connector_data( o_connector = o_connector ).
  me->set_checksum( ).
  endmethod.
ENDCLASS.
