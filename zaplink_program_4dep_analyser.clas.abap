class ZAPLINK_PROGRAM_4DEP_ANALYSER definition
  public
  inheriting from ZAPLINK_PROGRAM
  final
  create public

  global friends ZAPLINK_DEPENDENCIES_ANALYSER .

public section.

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_PROGRAM_4DEP_ANALYSER IMPLEMENTATION.


  method CONSTRUCTOR.
  super->constructor( ).
  clear: supported_types, uuid, version.     " Prevent having 2 connectors for the same UUID
  endmethod.
ENDCLASS.
