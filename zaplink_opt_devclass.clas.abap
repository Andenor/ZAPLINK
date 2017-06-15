class ZAPLINK_OPT_DEVCLASS definition
  public
  inheriting from ZAPLINK_OPT_BASE
  create public .

public section.

  constants:
    BEGIN OF substitutionkinds,
        use_container  TYPE td_substitutionkind VALUE 'C',    " Keep dev class in XML file
        no_overwrite   TYPE td_substitutionkind VALUE 'N',    " No overwrite : Use existing devclass, otherwise use container devclass otherwise use local otherwise let user choose
        keep_existing  TYPE td_substitutionkind VALUE 'K',    " Use devclass define in component
        local          TYPE td_substitutionkind VALUE 'L',    " Devclass = $TMP
        fixed          TYPE td_substitutionkind VALUE 'F',    " Assign allways the same specified devclass
      END OF substitutionkinds .

  methods CONSTRUCTOR .
  methods GET_SUBSTITUTIONKIND
    returning
      value(RESULT) type TD_SUBSTITUTIONKIND
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods SET_SUBSTITUTIONKIND
    importing
      !DATA type TD_SUBSTITUTIONKIND
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods GET_DEVCLASS
    returning
      value(RESULT) type TD_DEVCLASS
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods SET_DEVCLASS
    importing
      !DATA type TD_DEVCLASS
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods SUBSTITUTE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(RESULT) type TD_DEVCLASS
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
protected section.

  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_EXCEPTION type ref to ZAPLINK_CX_OPT_DEVCLASS .

  data _SUBSTITUTION type TD_SUBSTITUTIONKIND .
  data _DEVCLASS type TD_DEVCLASS .

  methods GET_EXISTING
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(RESULT) type TD_DEVCLASS
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods DO_EXISTS
    importing
      !DATA type TD_DEVCLASS
    returning
      value(RESULT) type TD_EXISTS
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
private section.

  constants LOCAL_DEVCLASS type TD_DEVCLASS value '$TMP' ##NO_TEXT.
  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_OPT_DEVCLASS IMPLEMENTATION.


  method CONSTRUCTOR.
  super->constructor( ).
  _substitution = substitutionkinds-keep_existing.
  endmethod.


  method DO_EXISTS.

  result = abap_false.
  CHECK NOT data IS INITIAL.

  CALL FUNCTION 'TRINT_DEVCLASS_GET'
    EXPORTING
      iv_devclass              = data
*      IV_LANGU                 = SY-LANGU
*      IV_WITH_LAYER            = 'X'
*      IV_WITH_TEXT             = ' '
*      IV_USE_MEMORY            = ' '
*    IMPORTING
*      ES_TDEVC                 =
    EXCEPTIONS
      devclass_not_found       = 1
      OTHERS                   = 2.
  IF sy-subrc = 0.
    result = abap_true.
  ELSEIF sy-subrc <> 1.
    RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
          EXPORTING textid = zaplink_cx_opt_devclass=>missing_devclass
                  devclass = data.
  ENDIF.
  endmethod.


  method GET_DEVCLASS.
result = _devclass.
  endmethod.


  method GET_EXISTING.
  DATA _name        TYPE tadir-obj_name.
  DATA _type        TYPE tadir-object.
  DATA _exists      TYPE saus_dpara-tadirexist.
  DATA _kind        TYPE tadir-pgmid.

  CHECK component IS BOUND.

  TRY.
      _type = component->get_type( ).
      _name = component->get_name( ).
      _kind = component->get_typekind( ).
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.

  CALL FUNCTION 'TRINT_TADIR_QUERY'
    EXPORTING
      iv_pgmid           = _kind
      iv_object          = _type
      iv_obj_name        = _name
    IMPORTING
*      EV_OBJECT          =
*      EV_SRCSYSTEM       =
      ev_devclass        = result
*      EV_AUTHOR          =
*      EV_GENFLAG         =
      ev_exist           = _exists.
  IF _exists IS INITIAL.
    CLEAR result.
  ENDIF.
  endmethod.


  method GET_SUBSTITUTIONKIND.
  result = _substitution.
  endmethod.


  method SET_DEVCLASS.
  CHECK data <> _devclass.

  IF _devclass IS INITIAL AND _substitution = substitutionkinds-fixed.
    RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
          EXPORTING textid = zaplink_cx_opt_devclass=>missing_devclass
                 substkind = _substitution.
  ENDIF.
  _devclass = data.
  endmethod.


  method SET_SUBSTITUTIONKIND.
  CHECK data <> _substitution.

  CASE data.
    WHEN substitutionkinds-keep_existing OR
         substitutionkinds-use_container OR
         substitutionkinds-no_overwrite  OR
         substitutionkinds-local.
    WHEN substitutionkinds-fixed.
      IF _devclass IS INITIAL.
        RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
              EXPORTING textid = zaplink_cx_opt_devclass=>missing_devclass
                     substkind = data.
      ENDIF.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
            EXPORTING textid = zaplink_cx_opt_devclass=>invalid_substitutionkind
                   substkind = data.
  ENDCASE.
  _substitution = data.
  endmethod.


  method SUBSTITUTE.
  DATA comp_devc TYPE td_devclass.

  CHECK component IS BOUND.
  comp_devc = component->directory-devclass.

  TRY.

    IF component->get_type( ) = 'DEVC'.
      result = component->get_name( ).
    ELSE.
      CASE _substitution.
        WHEN substitutionkinds-no_overwrite.
          result = get_existing( component ).
          IF NOT result IS INITIAL.
            EXIT.
          ENDIF.
          IF do_exists( comp_devc ) = abap_true.
            result = comp_devc.
            EXIT.
          ENDIF.
          result = me->_devclass.
        WHEN substitutionkinds-keep_existing.
          result = get_existing( component ).
          IF result IS INITIAL.
            result = local_devclass.
          ENDIF.
        WHEN substitutionkinds-use_container.
          IF do_exists( comp_devc ) = abap_true.
            result = comp_devc.
          ELSE.
            result = local_devclass.
          ENDIF.
        WHEN substitutionkinds-local.
          result = local_devclass.
        WHEN substitutionkinds-fixed.
          result = me->_devclass.
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
                EXPORTING textid = zaplink_cx_opt_devclass=>invalid_substitutionkind
                       substkind = _substitution.
      ENDCASE.
    ENDIF.

    mac_def_catch zaplink_cx_opt_devclass.
  ENDTRY.

  IF result IS INITIAL.   result = local_devclass.    ENDIF.
  endmethod.
ENDCLASS.
