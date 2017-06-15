class ZAPLINK_OPT_TRANSPORT_REQUEST definition
  public
  inheriting from ZAPLINK_OPT_BASE
  create public .

public section.

  constants:
    BEGIN OF substitutionkinds,
        fixed             TYPE td_substitutionkind VALUE 'F',    " Assign allways the same specified devclass
        same_as_container TYPE td_substitutionkind VALUE 'K',    " Keep the same scheme as in container
        user              TYPE td_substitutionkind VALUE 'U',    " let user select a transport request for all component
        input             TYPE td_substitutionkind VALUE 'I',    " let user select a transport request for each component
        custom            TYPE td_substitutionkind VALUE 'X',    " user define
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
  methods GET_TRANSPORT_REQUEST
    returning
      value(RESULT) type TD_TRANSPORT_REQUEST
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods SET_TRANSPORT_REQUEST
    importing
      !DATA type TD_TRANSPORT_REQUEST
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
  methods SUBSTITUTE
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(RESULT) type TD_TRANSPORT_REQUEST
    raising
      ZAPLINK_CX_OPT_DEVCLASS .
protected section.

  data _SUBSTITUTION type TD_SUBSTITUTIONKIND .
  data _TRANSPORT_REQUEST type TD_TRANSPORT_REQUEST .
private section.

  types:
    BEGIN OF ts_tr_substit,
     xml_tr TYPE td_transport_request,
     sap_tr TYPE td_transport_request,
   END OF ts_tr_substit .
  types:
    tt_tr_substits TYPE SORTED TABLE OF ts_tr_substit WITH UNIQUE KEY xml_tr .

  data _TR_MAPPING type TT_TR_SUBSTITS .
  data _USER_INPUT type TD_TRANSPORT_REQUEST .
ENDCLASS.



CLASS ZAPLINK_OPT_TRANSPORT_REQUEST IMPLEMENTATION.


  method CONSTRUCTOR.
  super->constructor( ).
  _substitution = substitutionkinds-input.
  endmethod.


  method GET_SUBSTITUTIONKIND.
result = _substitution.
  endmethod.


  method GET_TRANSPORT_REQUEST.
  result = _transport_request.
  endmethod.


  method SET_SUBSTITUTIONKIND.
  CHECK data <> _substitution.

  CLEAR _user_input.
  CASE data.
    WHEN substitutionkinds-same_as_container OR
         substitutionkinds-user OR
         substitutionkinds-custom OR
         substitutionkinds-input.
    WHEN substitutionkinds-fixed.
      IF _transport_request IS INITIAL.
        RAISE EXCEPTION TYPE zaplink_cx_opt_transport_req
              EXPORTING textid = zaplink_cx_opt_transport_req=>missing_transport_request
                     substkind = data.
      ENDIF.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zaplink_cx_opt_transport_req
            EXPORTING textid = zaplink_cx_opt_transport_req=>invalid_substitutionkind
                   substkind = data.
  ENDCASE.
  _substitution = data.
  endmethod.


  method SET_TRANSPORT_REQUEST.
CHECK data <> _transport_request.

  IF _transport_request IS INITIAL AND _substitution = substitutionkinds-fixed.
    RAISE EXCEPTION TYPE zaplink_cx_opt_transport_req
          EXPORTING textid = zaplink_cx_opt_transport_req=>missing_transport_request
                 substkind = _substitution.
  ENDIF.
  _transport_request = data.
  endmethod.


  method SUBSTITUTE.
  DATA t_e071   TYPE STANDARD TABLE OF e071.
  DATA t_e071k  TYPE STANDARD TABLE OF e071k.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF _tr_mapping.

  CHECK component IS BOUND.
  CASE _substitution.
    WHEN substitutionkinds-input.
      CLEAR result.
      EXIT. " all ways empty to let use choose the right transport request
    WHEN substitutionkinds-fixed.
      result = _transport_request.
    WHEN substitutionkinds-user.
* to develop using MF TRINT_ORDER_CHOICE
      IF _user_input IS INITIAL.
        CALL FUNCTION 'TRINT_ORDER_CHOICE'
          EXPORTING
*         WI_SIMULATION                = ' '
            wi_order_type                = 'K'
            wi_task_type                 = 'S'
*         WI_CATEGORY                  = 'SYST'
*         WI_CLIENT                    = SY-MANDT
*         IV_TARSYSTEM                 = ' '
*         WI_ORDER                     = ' '
*         WI_E070                      =
*         WI_SUPPRESS_DIALOG           = ' '
*         WI_CLI_DEP                   = ' '
*         WI_REMOVE_LOCKS              = 'X'
*         WI_DISPLAY_BUTTON            = ' '
*         WI_APPEND_TO_ORDER           = ' '
*         WI_APPEND_TO_FO              = ' '
*         IV_CURRENT_PROJECT           = ' '
*         IV_WARN_NOSTAND_TAR          = ' '
          IMPORTING
*         WE_ORDER                     =
            we_task                      = _user_input
          TABLES
            wt_e071                      = t_e071
            wt_e071k                     = t_e071k
          EXCEPTIONS
            no_correction_selected       = 1
            display_mode                 = 2
            object_append_error          = 3
            recursive_call               = 4
            wrong_order_type             = 5
            OTHERS                       = 6.
        IF sy-subrc <> 0.
          CLEAR _user_input.
        ELSE.
          result = _user_input.
        ENDIF.
      ENDIF.
      IF NOT _user_input IS INITIAL.
        result = _user_input.
      ELSE.
        CLEAR result.
      ENDIF.
    WHEN substitutionkinds-custom.
* to develop using event ?
    WHEN substitutionkinds-same_as_container.
      READ TABLE _tr_mapping ASSIGNING <s>
           WITH TABLE KEY xml_tr = component->directory-trkorr.
      IF sy-subrc = 0.
        result = <s>-sap_tr.
      ELSEIF NOT component->directory-trkorr IS INITIAL.
* to develop using MF TRINT_ORDER_CHOICE
      ENDIF.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zaplink_cx_opt_transport_req
            EXPORTING textid = zaplink_cx_opt_transport_req=>invalid_substitutionkind
                   substkind = _substitution.
  ENDCASE.
  endmethod.
ENDCLASS.
