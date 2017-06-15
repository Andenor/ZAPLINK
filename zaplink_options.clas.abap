class ZAPLINK_OPTIONS definition
  public
  create public .

public section.

  types TO_DEVCLASS type ref to ZAPLINK_OPT_DEVCLASS .
  types TO_DIRECTORY type ref to ZAPLINK_OPT_DIRECTORY .

  data DEVCLASS type TO_DEVCLASS read-only .
  data DIRECTORY type TO_DIRECTORY read-only .

  methods CONSTRUCTOR .
  methods GET_DIRECTORY
    returning
      value(RESULT) type TO_DIRECTORY
    raising
      ZAPLINK_CX_OPTIONS .
  methods SET_DIRECTORY
    importing
      !DATA type TO_DIRECTORY
    raising
      ZAPLINK_CX_OPTIONS .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_OPTIONS IMPLEMENTATION.


  method CONSTRUCTOR.
  create OBJECT directory.
  create OBJECT devclass.
  endmethod.


  method GET_DIRECTORY.
  result = directory.
  endmethod.


  method SET_DIRECTORY.
  CHECK data <> directory.
  directory = data.
  endmethod.
ENDCLASS.
