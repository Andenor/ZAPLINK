class ZAPLINK_EASYXML_4CNX definition
  public
  inheriting from ZAPLINK_EASYXML
  create public .

public section.
protected section.

  methods READ_OBJECT
    redefinition .
private section.

  types TO_RAW_BASE type ref to ZAPLINK_RAW_BASE .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types TO_RAW type ref to ZAPLINK_RAW .
ENDCLASS.



CLASS ZAPLINK_EASYXML_4CNX IMPLEMENTATION.


  method READ_OBJECT.
  DATA o_raw_base TYPE to_raw_base.
  DATA o_as_raw   TYPE to_raw.
  DATA data_type  TYPE string.
  DATA o_conn     TYPE to_connector.
  DATA _cx        TYPE REF TO cx_root.
  DATA o_cx_cnx   TYPE REF TO zaplink_cx_connector.
  DATA f_newobj   TYPE abap_bool.
  DATA o_raw      TYPE to_raw.

  CHECK object IS BOUND OR typedesc IS BOUND.

  IF NOT object IS BOUND.

    data_type = typedesc->get_relative_name( ).     f_newobj = abap_true.
    TRY.
        CREATE OBJECT object TYPE (data_type).
      CATCH cx_root INTO _cx.
        CALL METHOD application_log->add_exception
          EXPORTING
            exception = _cx.
        EXIT.
    ENDTRY.

*  ELSE.
  ENDIF.    " for ZL_OBJECT in ZL_TRANSACTION where ZL_OBJECT could be program, transaction or Class
* In such case object is not bound and typedesc is bound.

  TRY.
      o_as_raw ?= object.
    CATCH cx_root.
  ENDTRY.

  IF o_as_raw IS BOUND. " object inherited from o_raw_base
    TRY.                  " => Create real data type
        o_raw_base = zaplink_connectors=>node_2_raw( xml_node ).
        o_conn = zaplink_connectors=>create_connector( connuuid = o_raw_base->connector ).
        o_raw = o_conn->create_new_raw( type = o_raw_base->type ).
* Warning : object type should be compatible because will mapped in calling method READ_ANY using CAST that might failed with downard cast.
        IF NOT f_newobj IS INITIAL.
          object = o_raw.                   " Object was not created when calling this method
        ELSE.
          o_as_raw->copy_as( o_raw ).     " Map each fields
        ENDIF.
      CATCH zaplink_cx_connector INTO o_cx_cnx.
        CALL METHOD application_log->add_exception
          EXPORTING
            exception = o_cx_cnx.
        EXIT.
    ENDTRY.
  ENDIF.

*  ENDIF.

  CALL METHOD super->read_object
    EXPORTING
      xml_node = xml_node
      typedesc = typedesc
    CHANGING
      object   = object.
  endmethod.
ENDCLASS.
