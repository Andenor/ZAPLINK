class ZAPLINK_MESSAGE_COLLECTOR definition
  public
  create public .

public section.
  type-pools ABAP .

  types:
    t_msg TYPE STANDARD TABLE OF bal_s_msg WITH NON-UNIQUE DEFAULT KEY .

  data OBJECT type BALOBJ_D read-only .
  data MD_EXTNUMBER type BALNREXT read-only .
  data MD_LEVEL type BALLEVEL read-only .
  data SUBOBJECT type BALSUBOBJ read-only .
  data EXT_ID type BALNREXT read-only .
  constants:
    begin of ACTIONS,
      CREATE    type BU_AKTYP value '01', "#EC NOTEXT
      CHANGE    type BU_AKTYP value '02', "#EC NOTEXT
      DISPLAY   type BU_AKTYP value '03', "#EC NOTEXT
    end of ACTIONS .
  data MSGID type SYMSGID .
  constants:
    BEGIN OF probcl,
        very_imp    TYPE balprobcl VALUE '1',               "#EC NOTEXT
        important   TYPE balprobcl VALUE '2',               "#EC NOTEXT
        medium      TYPE balprobcl VALUE '3',               "#EC NOTEXT
        for_info    TYPE balprobcl VALUE '4',               "#EC NOTEXT
        other       TYPE balprobcl VALUE space,
      END OF probcl .
  data MD_HANDLE type BALLOGHNDL read-only .

  methods ADD
    importing
      !IS_MESSAGE type BAL_S_MSG optional
      !ID_MSGTY type SYMSGTY optional
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO optional
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_MSGD1 type SYDATUM optional
      !ID_MSGD2 type SYDATUM optional
      !ID_MSGD3 type SYDATUM optional
      !ID_MSGD4 type SYDATUM optional
      !ID_DETLEVEL type BALLEVEL optional
      !IF_CUMULATE type ABAP_BOOL optional
      !ID_PROBCLASS type BALPROBCL optional
      !ID_INDEX type NUMERIC optional
      !ID_SUBLOG type BALLOGHNDL optional
    exporting
      value(ES_MESSAGE) type BAL_S_MSG .
  methods ADD_ABEND
    importing
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional .
  methods ADD_ERROR
    importing
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional .
  methods ADD_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional .
  methods ADD_FROM_BAPI
    importing
      !IT_BAPIRET type BAPIRETTAB optional
      !IS_BAPIRET type BAPIRET2 optional
      !IF_CUMULATE type ABAP_BOOL optional
    exporting
      !EF_ADD_ERROR type ABAP_BOOL
      !EF_ADD_WARNING type ABAP_BOOL .
  methods ADD_FROM_INSTANCE
    importing
      !IO_MSGLIST type ref to ZAPLINK_MESSAGE_COLLECTOR
      !IF_ADD_AS_SUBNODE type ABAP_BOOL default ABAP_FALSE
      !IF_CUMULATE type ABAP_BOOL optional .
  methods ADD_INFO
    importing
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional .
  methods ADD_SUCCESS
    importing
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional .
  methods ADD_SYMSG
    importing
      !ID_MSGTY type SYMSGTY default SY-MSGTY
      !ID_DETLEVEL type BALLEVEL optional
      !IF_CUMULATE type ABAP_BOOL optional
      !ID_PROBCLASS type BALPROBCL optional
      !ID_INDEX type NUMERIC optional
    exporting
      value(ES_MESSAGE) type BAL_S_MSG .
  methods ADD_WARNING
    importing
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional .
  class-methods AS_CHAR
    importing
      !ID_DATE type SYDATUM
    returning
      value(RD_DATE) type CHAR10 .
  methods CHANGE_MSG_TYPE
    importing
      !ID_MSGTY_SRC type SYMSGTY optional
      !ID_MSGTY_TRG type SYMSGTY
    returning
      value(RD_CHANGED) type I .
  methods CHECK_CUSTO
    importing
      !ID_OBJECT type BALOBJ_D
      !ID_SUBOBJECT type BALSUBOBJ
    exceptions
      ERROR .
  methods CLEAR .
  class-methods CONVERT_DATE_TO_STRING
    importing
      !ID_DATE type SYDATUM
    exporting
      !ED_DATE_STRING type CSEQUENCE .
  methods COUNT
    returning
      value(RD_COUNT) type I .
  methods DELETE_MESSAGE
    importing
      !ID_MSGNUMBER type BALMNR
    exceptions
      NOT_FOUND .
  methods FREE .
  methods GET_FIRST_MESSAGE
    importing
      !ID_MSGTY type SYMSGTY default 'I'
      !IF_OR_HIGHER type ABAP_BOOL default ABAP_TRUE
    exporting
      !ES_MESSAGE type BAL_S_MSG
    exceptions
      NOT_FOUND .
  methods GET_HANDLE
    returning
      value(RD_HANDLE) type BALLOGHNDL .
  methods GET_LAST_MESSAGE
    importing
      !ID_MSGTY type SYMSGTY default 'I'
      !IF_OR_HIGHER type ABAP_BOOL default ABAP_TRUE
    exporting
      !ES_MESSAGE type BAL_S_MSG
    exceptions
      NOT_FOUND .
  methods GET_LIST
    returning
      value(ET_LIST) type T_MSG .
  methods GET_LIST_AS_BAPIRET
    returning
      value(ET_LIST) type BAPIRETTAB .
  class-methods GET_REF_TYPE
    importing
      !TYPEDESC type ref to CL_ABAP_TYPEDESCR
    returning
      value(REF_TYPE) type ref to CL_ABAP_TYPEDESCR .
  methods GET_STATISTICS
    returning
      value(RS_STATISTICS) type BAL_S_SCNT .
  methods GET_STATISTICS_AS_TEXT
    importing
      !IF_OUTPUT_E_MESSAGES type ABAP_BOOL default ABAP_TRUE
      !IF_OUTPUT_W_MESSAGES type ABAP_BOOL default ABAP_TRUE
      !IF_OUTPUT_I_MESSAGES type ABAP_BOOL default ABAP_FALSE
      !IF_OUTPUT_HIGHEST_ONLY type ABAP_BOOL default ABAP_TRUE
    exporting
      value(ED_TEXT) type CSEQUENCE .
  methods HAS_MESSAGES_OF_MSGT
    importing
      !ID_MSGTY type SYMSGTY
      !IF_OR_HIGHER type ABAP_BOOL default ABAP_TRUE
    returning
      value(RF_EXISTS) type ABAP_BOOL .
  methods INIT
    importing
      !ID_OBJECT type BALOBJ_D
      !ID_SUBOBJECT type BALSUBOBJ
      !ID_EXTNUMBER type BALNREXT optional
      !AUTO_UPD_CUSTO type FLAG default ABAP_FALSE
      !ID_ACTIVITY type BU_AKTYP default ACTIONS-CREATE
    exceptions
      ERROR .
  methods INIT_BY_HANDLE
    importing
      !ID_HANDLE type BALLOGHNDL
    exceptions
      ERROR .
  methods INSERT
    importing
      !ID_INSERT_INDEX type I
      !IF_SET_FOLLOWING_AS_SUBNODE type ABAP_BOOL default ABAP_FALSE
      !IS_MESSAGE type BAL_S_MSG optional
      !ID_MSGTY type SYMSGTY optional
      !ID_MSGID type SYMSGID optional
      !ID_MSGNO type SYMSGNO optional
      !ID_MSGV1 type CLIKE optional
      !ID_MSGV2 type CLIKE optional
      !ID_MSGV3 type CLIKE optional
      !ID_MSGV4 type CLIKE optional
      !ID_MSGD1 type SYDATUM optional
      !ID_MSGD2 type SYDATUM optional
      !ID_MSGD3 type SYDATUM optional
      !ID_MSGD4 type SYDATUM optional
      !ID_DETLEVEL type BALLEVEL optional
      !ID_PROBCLASS type BALPROBCL optional
      !ID_INDEX type NUMERIC optional
    exporting
      value(ES_MESSAGE) type BAL_S_MSG .
  class-methods IS_DATE_INITIAL
    importing
      !ID_DATE type SY-DATUM
    returning
      value(RF_INITIAL) type ABAP_BOOL .
  methods IS_EMPTY
    returning
      value(RF_EMPTY) type ABAP_BOOL .
  class-methods NEW
    returning
      value(OBJ) type ref to ZAPLINK_MESSAGE_COLLECTOR .
  methods RAISE_FIRST_MESSAGE
    importing
      !ID_MSGTY type SYMSGTY default 'E'
      !IF_OR_HIGHER type ABAP_BOOL default ABAP_TRUE
    exceptions
      FIRST_MESSAGE .
  methods RAISE_ON_ERROR
    raising
      ZAPLINK_CX .
  methods SET_DETAIL_LEVEL
    importing
      !ID_ABSOLUTE type BALLEVEL optional
      !ID_RELATIVE type INT4 optional .
  methods SET_EXTNUMBER
    importing
      !ID_EXTNUMBER type BALNREXT .
  methods STORE
    importing
      !IF_IN_UPDATE_TASK type ABAP_BOOL default ABAP_FALSE
    exceptions
      ERROR .
  methods WRITE
    importing
      !_OFFSET type STRING optional .
protected section.

  types:
    BEGIN OF msg_x .
    INCLUDE TYPE bal_s_msg AS msg.
TYPES:
    msgnumber	TYPE balmnr,
  END OF msg_x .
  types:
    t_msg_x TYPE STANDARD TABLE OF msg_x WITH NON-UNIQUE DEFAULT KEY .

  constants MC_MIN_LEVEL type N value '1' ##NO_TEXT.
  constants MC_MAX_LEVEL type N value '9' ##NO_TEXT.
  data MS_LOG type BAL_S_LOG .
  data MD_SUP_COUNT type SYTABIX .
  data MT_SUP_MSG type T_MSG_X .
  data MF_LOG_NOT_STORED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.

  methods GET_LIST_X
    importing
      !ID_MSGTY type SYMSGTY default 'I'
    exporting
      !ET_LIST_X type T_MSG_X .
  methods GET_MSG_FILTER
    importing
      value(ID_MSGTY) type SYMSGTY default 'I'
      value(IF_OR_HIGHER) type ABAP_BOOL default ABAP_TRUE
    exporting
      !ES_MSG_FILTER type BAL_S_MFIL .
  methods _ADD_ZL_CX
    importing
      !EXCEPTION type ref to ZAPLINK_CX
      !ID_DETLEVEL type BALLEVEL
      !ID_PROBCLASS type BALPROBCL .
  methods _ADD_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT
      !ID_DETLEVEL type BALLEVEL
      !ID_PROBCLASS type BALPROBCL .
private section.
ENDCLASS.



CLASS ZAPLINK_MESSAGE_COLLECTOR IMPLEMENTATION.


  method ADD.
  DATA:
    ls_message    TYPE bal_s_msg,
*    ls_msgcontext TYPE recamsgcontext,
    ls_par        LIKE LINE OF ls_message-params-t_par,
    ld_parcontext TYPE balpval.

*  DEFINE mac_append_par.
*    if &2 is not initial.
*      ls_par-parname  = &1.
*      ls_par-parvalue = &2.
*      append ls_par to ls_message-params-t_par.
*    endif.
*  END-OF-DEFINITION.

* INIT RESULTS
  CLEAR es_message.

* BODY
  IF is_message IS NOT INITIAL.

*   set message data from structure
    ls_message = is_message.

  ELSE.

*   set message data from single field values
*   (special handling of message v1..v4 for type date)
    ls_message-msgty = id_msgty.
    IF id_msgid IS INITIAL.
      ls_message-msgid = msgid.
    ELSE.
      ls_message-msgid = id_msgid.
    ENDIF.
    ls_message-msgno = id_msgno.

    IF id_msgv1 IS SUPPLIED.
      ls_message-msgv1 = id_msgv1.
    ELSEIF id_msgd1 IS SUPPLIED.
      CALL METHOD convert_date_to_string
        EXPORTING
          id_date        = id_msgd1
        IMPORTING
          ed_date_string = ls_message-msgv1.
    ENDIF.

    IF id_msgv2 IS SUPPLIED.
      ls_message-msgv2 = id_msgv2.
    ELSEIF id_msgd2 IS SUPPLIED.
      CALL METHOD convert_date_to_string
        EXPORTING
          id_date        = id_msgd2
        IMPORTING
          ed_date_string = ls_message-msgv2.
    ENDIF.

    IF id_msgv3 IS SUPPLIED.
      ls_message-msgv3 = id_msgv3.
    ELSEIF id_msgd3 IS SUPPLIED.
      CALL METHOD convert_date_to_string
        EXPORTING
          id_date        = id_msgd3
        IMPORTING
          ed_date_string = ls_message-msgv3.
    ENDIF.

    IF id_msgv4 IS SUPPLIED.
      ls_message-msgv4 = id_msgv4.
    ELSEIF id_msgd4 IS SUPPLIED.
      CALL METHOD convert_date_to_string
        EXPORTING
          id_date        = id_msgd4
        IMPORTING
          ed_date_string = ls_message-msgv4.
    ENDIF.

    ls_message-detlevel  = id_detlevel.
    ls_message-probclass = id_probclass.

  ENDIF.

* check message data (T100)
  IF ( ls_message-msgty NA 'ISWEAX' ) OR
     ( ls_message-msgid IS INITIAL  ).
    MESSAGE ID ls_message-msgid TYPE ls_message-msgty NUMBER ls_message-msgno
            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
  ENDIF.

* modify detail level for message hierarchy
  IF ls_message-detlevel NOT BETWEEN mc_min_level AND mc_max_level.
    ls_message-detlevel = md_level.
  ENDIF.

* modify problem class
  IF ls_message-probclass IS INITIAL.
    CASE ls_message-msgty.
      WHEN 'I' OR 'S'.       ls_message-probclass = '4'.
      WHEN 'W'.              ls_message-probclass = '3'.
      WHEN 'E'.              ls_message-probclass = '2'.
      WHEN 'A' OR 'X'.       ls_message-probclass = '1'.
    ENDCASE.
  ENDIF.

** set additional message data
*  IF ( id_tabname   IS NOT INITIAL ) OR
*     ( id_fieldname IS NOT INITIAL ) OR
*     ( id_value     IS NOT INITIAL ) OR
*     ( id_index     IS NOT INITIAL ) OR
*     ( id_intreno   IS NOT INITIAL ) OR
*     ( id_custact   IS NOT INITIAL ) OR
*     ( id_sublog    IS NOT INITIAL ) OR
*     ( id_context   IS NOT INITIAL ).
*
***   set callback header
**    IF ( id_tabname IS NOT INITIAL OR id_fieldname IS NOT INITIAL ) OR
**       ( id_intreno IS NOT INITIAL                                ) OR
**       ( id_custact IS NOT INITIAL                                ) OR
**       ( id_sublog  IS NOT INITIAL                                ).
**      ls_message-params-callback-userexitp = space.
**      ls_message-params-callback-userexitf = mc_func_callback.
**      ls_message-params-callback-userexitt = 'F'.
**    ELSE.
*      ls_message-params-altext = '-'.
**    ENDIF.
*
**   set message callback parameter
*    mac_append_par 'TABNAME'   id_tabname.
*    mac_append_par 'FIELDNAME' id_fieldname.
*    mac_append_par 'VALUE'     id_value.
*    mac_append_par 'INDEX'     id_index.
*    mac_append_par 'INTRENO'   id_intreno.
*    mac_append_par 'CUSTACT'   id_custact.
*    mac_append_par 'SUBLOG'    id_sublog.
*
**    IF id_context IS NOT INITIAL.
***     (unicode-like assignment)
**      CALL METHOD cl_abap_container_utilities=>fill_container_c
**        EXPORTING
**          im_value     = id_context
**        IMPORTING
**          ex_container = ld_parcontext.
**    ENDIF.
**
**    mac_append_par 'CONTEXT'   ld_parcontext.
**
****   set message context (only used to display in support mode)
***    ls_msgcontext-tabname      = id_tabname.
***    ls_msgcontext-fieldname    = id_fieldname.
***    ls_msgcontext-tabindex     = id_index.
***    ls_msgcontext-intreno      = id_intreno.
***
***    ls_message-context-tabname = 'RECAMSGCONTEXT'.
**    ls_message-context-tabname = id_tabname.
**
***   (unicode-like assignment)
**    CALL METHOD cl_abap_container_utilities=>fill_container_c
**      EXPORTING
**        im_value     = id_context
**      IMPORTING
**        ex_container = ls_message-context-value.
*
*  ENDIF.

** trace
*  CASE ls_message-msgty.
*    WHEN 'E' OR 'A'. mac_trace 'MSG_E_ADD'.
*    WHEN 'W'.        mac_trace 'MSG_W_ADD'.
*  ENDCASE.
*  mac_trace 'MSG_ADD'.

* add message to application log

  IF if_cumulate = abap_true.
*   add cumulated messages (see documentation of funcion module BAL_LOG_MSG_CUMULATE)
    CALL FUNCTION 'BAL_LOG_MSG_CUMULATE'
      EXPORTING
        i_log_handle         = md_handle
        i_s_msg              = ls_message
        i_compare_attributes = abap_true       " compare attributes
        i_compare_context    = abap_true       " and context
        i_compare_parameters = abap_false
      EXCEPTIONS
        log_not_found        = 1
        msg_inconsistent     = 2
        log_is_full          = 3
        OTHERS               = 4.
  ELSE.
*   just separately add this message
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = md_handle
        i_s_msg          = ls_message
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
  ENDIF.
  IF sy-subrc <> 0.
    mac_symsg_send_as_type 'A'.
  ELSE.
    es_message = ls_message.
  ENDIF.

** also refresh support message list
*  mac_refresh_support_list.
  endmethod.


  method ADD_ABEND.
  DATA: _detlevel	TYPE ballevel.

  IF id_detlevel IS INITIAL.
    _detlevel = md_level.
  ELSE.
    _detlevel = id_detlevel.
  ENDIF.

  CALL METHOD add
    EXPORTING
*      is_message   =
      id_msgty     = 'A'
      id_msgid     = id_msgid
      id_msgno     = id_msgno
      id_msgv1     = id_msgv1
      id_msgv2     = id_msgv2
      id_msgv3     = id_msgv3
      id_msgv4     = id_msgv4
*      id_msgd1     =
*      id_msgd2     =
*      id_msgd3     =
*      id_msgd4     =
      id_detlevel  = _detlevel
*      if_cumulate  =
      id_probclass = id_probclass
*      id_index     =
*      id_sublog    =
*    IMPORTING
*      es_message   =
      .
  endmethod.


  method ADD_ERROR.
  DATA: _detlevel	TYPE ballevel.

  IF id_detlevel IS INITIAL.
    _detlevel = md_level.
  ELSE.
    _detlevel = id_detlevel.
  ENDIF.

  CALL METHOD add
    EXPORTING
*      is_message   =
      id_msgty     = 'E'
      id_msgid     = id_msgid
      id_msgno     = id_msgno
      id_msgv1     = id_msgv1
      id_msgv2     = id_msgv2
      id_msgv3     = id_msgv3
      id_msgv4     = id_msgv4
*      id_msgd1     =
*      id_msgd2     =
*      id_msgd3     =
*      id_msgd4     =
      id_detlevel  = _detlevel
*      if_cumulate  =
      id_probclass = id_probclass
*      id_index     =
*      id_sublog    =
*    IMPORTING
*      es_message   =
      .
  endmethod.


  method ADD_EXCEPTION.
  DATA _detlevel  TYPE ballevel.
  DATA o_zl_cx    TYPE REF TO zaplink_cx.

  IF id_detlevel IS INITIAL.    _detlevel = md_level.   ELSE.   _detlevel = id_detlevel.    ENDIF.
  TRY.
      o_zl_cx ?= exception.
    CATCH cx_root.
  ENDTRY.
  IF o_zl_cx IS BOUND.
    CALL METHOD _add_zl_cx
      EXPORTING
        exception    = o_zl_cx
        id_detlevel  = _detlevel
        id_probclass = id_probclass.
  ELSE.
    CALL METHOD _add_exception
      EXPORTING
        exception    = exception
        id_detlevel  = _detlevel
        id_probclass = id_probclass.
  ENDIF.
  endmethod.


  method ADD_FROM_BAPI.
  FIELD-SYMBOLS:
      <ls_bapiret> LIKE is_bapiret.

* INIT RESULTS
  ef_add_error   = abap_false.
  ef_add_warning = abap_false.

* BODY
* evaluate bapi return table
  LOOP AT it_bapiret ASSIGNING <ls_bapiret>.
    CALL METHOD add
      EXPORTING
        id_msgty    = <ls_bapiret>-type
        id_msgid    = <ls_bapiret>-id
        id_msgno    = <ls_bapiret>-number
        id_msgv1    = <ls_bapiret>-message_v1
        id_msgv2    = <ls_bapiret>-message_v2
        id_msgv3    = <ls_bapiret>-message_v3
        id_msgv4    = <ls_bapiret>-message_v4
        if_cumulate = if_cumulate.
*       id_tabname   = <ls_bapiret>-parameter
*       id_fieldname = <ls_bapiret>-field
*       id_index     = <ls_bapiret>-row

    IF ( <ls_bapiret>-type = 'E' ) OR
       ( <ls_bapiret>-type = 'A' ).
      ef_add_error   = abap_true.
    ELSEIF ( <ls_bapiret>-type = 'W' ).
      ef_add_warning = abap_true.
    ENDIF.
  ENDLOOP.

* evaluate bapi return structure
  IF is_bapiret IS NOT INITIAL.
    CALL METHOD add
      EXPORTING
        id_msgty    = is_bapiret-type
        id_msgid    = is_bapiret-id
        id_msgno    = is_bapiret-number
        id_msgv1    = is_bapiret-message_v1
        id_msgv2    = is_bapiret-message_v2
        id_msgv3    = is_bapiret-message_v3
        id_msgv4    = is_bapiret-message_v4
        if_cumulate = if_cumulate.
*       id_tabname   = is_bapiret-parameter
*       id_fieldname = is_bapiret-field
*       id_index     = is_bapiret-row

    IF ( is_bapiret-type = 'E' ) OR
       ( is_bapiret-type = 'A' ).
      ef_add_error   = abap_true.
    ELSEIF ( is_bapiret-type = 'W' ).
      ef_add_warning = abap_true.
    ENDIF.
  ENDIF.
  endmethod.


  method ADD_FROM_INSTANCE.
  DATA:
    lt_msglist            TYPE t_msg,
    ls_msglist            TYPE bal_s_msg,
    ld_num_max_level      TYPE int1 VALUE mc_max_level,
    ld_num_detlevel_diff  TYPE int1,
    ld_num_detlevel_other TYPE int1,
    lf_cumulate           TYPE abap_bool.

  FIELD-SYMBOLS:
    <ls_msglist>          LIKE LINE OF lt_msglist.

* PRECONDITION
  CHECK io_msglist IS BOUND.

* BODY
  lt_msglist = io_msglist->get_list( ).

  CHECK lt_msglist IS NOT INITIAL.

* cumulation of messages
  lf_cumulate = if_cumulate.

* increment detail level?
  IF if_add_as_subnode = abap_true.
    set_detail_level( id_relative = 1 ).
    lf_cumulate = abap_false.
  ENDIF.

  LOOP AT lt_msglist ASSIGNING <ls_msglist>.

    MOVE-CORRESPONDING <ls_msglist> TO ls_msglist.

*   clear some fields as messages are "new"
    CLEAR:
      ls_msglist-time_stmp,
      ls_msglist-msg_count.

*   set detail level
    IF ls_msglist-detlevel NOT BETWEEN mc_min_level AND mc_max_level.
*     adopt always current detail level
      ls_msglist-detlevel = md_level.
    ELSEIF md_level = mc_min_level.
*     nothing to do: adopt the detail level from other
    ELSE.
*     adopt the message with a "deeper" detail level
      ld_num_detlevel_diff  = md_level - 1.
      ld_num_detlevel_other = ls_msglist-detlevel.
      ADD ld_num_detlevel_diff TO ld_num_detlevel_other.

      IF ld_num_detlevel_other <= ld_num_max_level.
        ls_msglist-detlevel = ld_num_detlevel_other.
      ELSE.
        ls_msglist-detlevel = mc_max_level.
      ENDIF.
    ENDIF.

*   add message
    add( is_message  = ls_msglist
         if_cumulate = lf_cumulate ).

  ENDLOOP.

* decrement detail level?
  IF if_add_as_subnode = abap_true.
    set_detail_level( id_relative = -1 ).
  ENDIF.
  endmethod.


  method ADD_INFO.
  DATA _detlevel  TYPE ballevel.

  IF id_detlevel IS INITIAL.
    _detlevel = md_level.
  ELSE.
    _detlevel = id_detlevel.
  ENDIF.

  CALL METHOD add
    EXPORTING
*      is_message   =
      id_msgty     = 'I'
      id_msgid     = ID_MSGID
      id_msgno     = id_msgno
      id_msgv1     = id_msgv1
      id_msgv2     = id_msgv2
      id_msgv3     = id_msgv3
      id_msgv4     = id_msgv4
*      id_msgd1     =
*      id_msgd2     =
*      id_msgd3     =
*      id_msgd4     =
      id_detlevel  = _detlevel
*      if_cumulate  =
      id_probclass = id_probclass
*      id_index     =
*      id_sublog    =
*    IMPORTING
*      es_message   =
      .
  endmethod.


  method ADD_SUCCESS.
  DATA: _detlevel	TYPE ballevel.

  IF id_detlevel IS INITIAL.
    _detlevel = md_level.
  ELSE.
    _detlevel = id_detlevel.
  ENDIF.

  CALL METHOD add
    EXPORTING
*      is_message   =
      id_msgty     = 'S'
      id_msgid     = id_msgid
      id_msgno     = id_msgno
      id_msgv1     = id_msgv1
      id_msgv2     = id_msgv2
      id_msgv3     = id_msgv3
      id_msgv4     = id_msgv4
*      id_msgd1     =
*      id_msgd2     =
*      id_msgd3     =
*      id_msgd4     =
      id_detlevel  = _detlevel
*      if_cumulate  =
      id_probclass = id_probclass
*      id_index     =
*      id_sublog    =
*    IMPORTING
*      es_message   =
      .
  endmethod.


  method ADD_SYMSG.
  DATA ls_symsg TYPE symsg.

  CHECK NOT sy-msgid IS INITIAL AND
*        not id_msgty is INITIAL AND
        NOT sy-msgno IS INITIAL.
* BODY
* pass local copies of current sy-msg*-values
  ls_symsg-msgty = id_msgty.
  ls_symsg-msgid = sy-msgid.
  ls_symsg-msgno = sy-msgno.
  ls_symsg-msgv1 = sy-msgv1.
  ls_symsg-msgv2 = sy-msgv2.
  ls_symsg-msgv3 = sy-msgv3.
  ls_symsg-msgv4 = sy-msgv4.

* add message
  CALL METHOD add
    EXPORTING
      id_msgty     = ls_symsg-msgty
      id_msgid     = ls_symsg-msgid
      id_msgno     = ls_symsg-msgno
      id_msgv1     = ls_symsg-msgv1
      id_msgv2     = ls_symsg-msgv2
      id_msgv3     = ls_symsg-msgv3
      id_msgv4     = ls_symsg-msgv4
      if_cumulate  = if_cumulate
      id_detlevel  = id_detlevel
      id_probclass = id_probclass
*      id_tabname   = id_tabname
*      id_fieldname = id_fieldname
*      id_value     = id_value
      id_index     = id_index
*      id_intreno   = id_intreno
*      id_custact   = id_custact
*      id_context   = id_context
    IMPORTING
      es_message   = es_message.
  endmethod.


  method ADD_WARNING.
  DATA: _detlevel	TYPE ballevel.

  IF id_detlevel IS INITIAL.
    _detlevel = md_level.
  ELSE.
    _detlevel = id_detlevel.
  ENDIF.

  CALL METHOD add
    EXPORTING
*      is_message   =
      id_msgty     = 'W'
      id_msgid     = id_msgid
      id_msgno     = id_msgno
      id_msgv1     = id_msgv1
      id_msgv2     = id_msgv2
      id_msgv3     = id_msgv3
      id_msgv4     = id_msgv4
*      id_msgd1     =
*      id_msgd2     =
*      id_msgd3     =
*      id_msgd4     =
      id_detlevel  = _detlevel
*      if_cumulate  =
      id_probclass = id_probclass
*      id_index     =
*      id_sublog    =
*    IMPORTING
*      es_message   =
      .
  endmethod.


  method AS_CHAR.
* BODY
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = id_date
    IMPORTING
      date_external            = rd_date
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    CLEAR rd_date.
  ENDIF.
  endmethod.


  method CHANGE_MSG_TYPE.
  DATA:
    lt_log_handle   TYPE bal_t_logh,
    lt_msg_handle   TYPE bal_t_msgh,
    ls_msg          TYPE bal_s_msg.

  FIELD-SYMBOLS:
    <ls_msg_handle> LIKE LINE OF lt_msg_handle.

* PRECONDITION
  IF ( id_msgty_src NA ' AEISWX' ) OR
     ( id_msgty_trg NA 'AEISWX'  ).
    mac_invalid_precondition.
  ENDIF.

* BODY
* something to do?
  CHECK id_msgty_src <> id_msgty_trg.

* get handles of messages
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
    IMPORTING
      e_t_msg_handle = lt_msg_handle
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   no message found
    RETURN.
  ENDIF.

* get message data and change
  LOOP AT lt_msg_handle ASSIGNING <ls_msg_handle>.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = <ls_msg_handle>
      IMPORTING
        e_s_msg        = ls_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.

    IF sy-subrc = 0.

      IF ( id_msgty_src IS INITIAL     ) OR
         ( id_msgty_src = ls_msg-msgty ).

        ls_msg-msgty = id_msgty_trg.
        CALL FUNCTION 'BAL_LOG_MSG_CHANGE'
          EXPORTING
            i_s_msg_handle = <ls_msg_handle>
            i_s_msg        = ls_msg
          EXCEPTIONS
            OTHERS         = 1.
        IF sy-subrc = 0.
          ADD 1 TO rd_changed.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

** also refresh support message list
*  mac_refresh_support_list.
  endmethod.


  method CHECK_CUSTO.
  DATA _commit  TYPE flag.
  DATA obj      TYPE balobj.
  DATA obj_t    TYPE balobjt.
  DATA sobj     TYPE balsub.
  DATA sobj_t   TYPE balsubt.
  DATA str      TYPE string.
  FIELD-SYMBOLS:
    <t_balobj> TYPE ANY,
    <s_balobj> TYPE ANY,
    <t_balsub> TYPE ANY,
    <s_balsub> TYPE ANY.

* Check BALOBJ
  obj-object = id_object.
  CALL FUNCTION 'BAL_OBJECT_SELECT'
    EXPORTING
      i_object                      = id_object
*   IMPORTING
*     E_BALOBJ                      =
*     E_T_BALSUB                    =
*     E_OBJECT_HAS_SUBOBJECTS       =
   EXCEPTIONS
     object_not_found              = 1
     OTHERS                        = 2
            .
  IF sy-subrc <> 0.
*  SELECT SINGLE * INTO obj
*    FROM balobj
*    WHERE object = obj-object.
*  IF sy-subrc <> 0.
    INSERT balobj FROM obj.
    obj_t-spras = sy-langu.
    obj_t-object = obj-object.
    obj_t-objtxt = 'Entry auto created by ZAPLINK_MESSAGE_COLLECTOR'(blt).
    INSERT balobjt FROM obj_t.
    _commit = abap_true.
*  ENDIF.
  ENDIF.

* Check BALSUB
  sobj-object = obj-object.
  sobj-subobject = id_subobject.
  SELECT SINGLE * INTO sobj
    FROM balsub
    WHERE    object = sobj-object
      AND subobject = sobj-subobject.
  IF sy-subrc <> 0.
    INSERT balsub FROM sobj.
    sobj_t-spras = sy-langu.
    sobj_t-object = sobj-object.
    sobj_t-subobject = sobj-subobject.
    sobj_t-subobjtxt = 'Entry auto created by ZAPLINK_MESSAGE_COLLECTOR'(blt).
    INSERT balsubt FROM sobj_t.
    _commit = abap_true.
  ENDIF.

  IF NOT _commit IS INITIAL.
* Clear cached tables
    str = '(SAPLSBAL_SERVICE)g_balobj[]'.  ASSIGN (str) TO <t_balobj>.
    IF sy-subrc = 0. CLEAR <t_balobj>.  UNASSIGN <t_balobj>.  ENDIF.
    str = '(SAPLSBAL_SERVICE)g_balobj'.    ASSIGN (str) TO <s_balobj>.
    IF sy-subrc = 0. CLEAR <s_balobj>.  UNASSIGN <s_balobj>.  ENDIF.
    str = '(SAPLSBAL_SERVICE)g_balsub[]'.  ASSIGN (str) TO <t_balsub>.
    IF sy-subrc = 0. CLEAR <t_balsub>.  UNASSIGN <t_balsub>.  ENDIF.
    str = '(SAPLSBAL_SERVICE)g_balsub'.    ASSIGN (str) TO <s_balsub>.
    IF sy-subrc = 0. CLEAR <s_balsub>.  UNASSIGN <s_balsub>.  ENDIF.
* include LSBAL_SERVICETOP
*  g_balobj TYPE SORTED TABLE OF balobj WITH HEADER LINE
*           WITH NON-UNIQUE DEFAULT KEY INITIAL SIZE 20,
*  g_balsub TYPE SORTED TABLE OF balsub WITH HEADER LINE
*           WITH NON-UNIQUE DEFAULT KEY INITIAL SIZE 60,
    COMMIT WORK AND WAIT.
* Reload BAL cache
    CALL FUNCTION 'BAL_OBJECT_SELECT'
      EXPORTING
        i_object                      = id_object
*     IMPORTING
*       E_BALOBJ                      =
*       E_T_BALSUB                    =
*       E_OBJECT_HAS_SUBOBJECTS       =
     EXCEPTIONS
       object_not_found              = 1
       OTHERS                        = 2
              .
    IF sy-subrc <> 0.
* ignore error
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
  endmethod.


  method CLEAR.
* BODY
* delete all messages
  CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
    EXPORTING
      i_log_handle = md_handle
    EXCEPTIONS
      OTHERS       = 0.

* initialize detail level
  md_level = mc_min_level.

* also delete support mode tables
  CLEAR:
    md_sup_count,
    mt_sup_msg.
  endmethod.


  method CONVERT_DATE_TO_STRING.
* local data
  DATA:
    ld_date     TYPE sydatum,
    ld_char(10) TYPE c.

* eliminate NULL dates
  ld_date = id_date.
  mac_clear_date_if_initial ld_date.

* convert date
  ld_char = as_char( ld_date ).

* set "correct" date "00.00.0000" for initial dates
  IF ld_char IS INITIAL.
    ld_date = '11111111'.
    ld_char = as_char( ld_date ).
    REPLACE '1' WITH '0' INTO ld_char.
  ENDIF.

  ed_date_string = ld_char.
  endmethod.


  method COUNT.
  DATA:
    lt_log_handle TYPE bal_t_logh,
    lt_msg_handle TYPE bal_t_msgh.

* BODY
* get handles of messages
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
    IMPORTING
      e_t_msg_handle = lt_msg_handle
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    rd_count = lines( lt_msg_handle ).
  ELSE.
*   no message found
    rd_count = 0.
  ENDIF.
  endmethod.


  method DELETE_MESSAGE.
  DATA:
    ls_msg_handle TYPE balmsghndl.

* BODY
  ls_msg_handle-log_handle = md_handle.
  ls_msg_handle-msgnumber  = id_msgnumber.

  CALL FUNCTION 'BAL_LOG_MSG_DELETE'
    EXPORTING
      i_s_msg_handle = ls_msg_handle
    EXCEPTIONS
      msg_not_found  = 1
      log_not_found  = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    mac_symsg_raise not_found.
  ENDIF.

** also refresh support message list
*  mac_refresh_support_list.
  endmethod.


  method FREE.
* BODY
  IF mf_log_not_stored = abap_false.
    CALL FUNCTION 'BAL_LOG_REFRESH'
    EXPORTING
      i_log_handle = md_handle
    EXCEPTIONS
      OTHERS       = 0.
  ENDIF.

* clear instance attributes
  CLEAR md_handle.
  CLEAR ms_log.
  endmethod.


  method GET_FIRST_MESSAGE.
  DATA:
    lt_log_handle TYPE bal_t_logh,
    lt_msg_handle TYPE bal_t_msgh,
    ls_msg_filter TYPE bal_s_mfil,
    ld_msg_handle TYPE balmsghndl,
    ls_msg        TYPE bal_s_msg.

* INIT RESULTS
  CLEAR es_message.

* PRECONDITION
  IF id_msgty NA 'IWEA'.
    mac_invalid_precondition.
  ENDIF.

* BODY
* get message filter by message type
  CALL METHOD get_msg_filter
    EXPORTING
      id_msgty      = id_msgty
      if_or_higher  = if_or_higher
    IMPORTING
      es_msg_filter = ls_msg_filter.

* get handles of messages
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
      i_s_msg_filter = ls_msg_filter
    IMPORTING
      e_t_msg_handle = lt_msg_handle
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   no message found
    mac_symsg_raise not_found.
  ENDIF.

* get data of first message
  READ TABLE lt_msg_handle INDEX 1 INTO ld_msg_handle.
  CALL FUNCTION 'BAL_LOG_MSG_READ'
    EXPORTING
      i_s_msg_handle = ld_msg_handle
    IMPORTING
      e_s_msg        = ls_msg
    EXCEPTIONS
      log_not_found  = 1
      msg_not_found  = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    mac_symsg_raise not_found.
  ENDIF.

  es_message = ls_msg.
  endmethod.


  method GET_HANDLE.
  rd_handle = md_handle.
  endmethod.


  method GET_LAST_MESSAGE.
  DATA:
    lt_log_handle TYPE bal_t_logh,
    lt_msg_handle TYPE bal_t_msgh,
    ls_msg_filter TYPE bal_s_mfil,
    ld_msg_handle TYPE balmsghndl,
    ls_msg        TYPE bal_s_msg,
    ld_count      TYPE i.

* INIT RESULTS
  CLEAR es_message.

* PRECONDITION
  IF id_msgty NA 'IWEA'.
    mac_invalid_precondition.
  ENDIF.

* BODY
* get message filter by message type
  CALL METHOD get_msg_filter
    EXPORTING
      id_msgty      = id_msgty
      if_or_higher  = if_or_higher
    IMPORTING
      es_msg_filter = ls_msg_filter.

* get handles of messages
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
      i_s_msg_filter = ls_msg_filter
    IMPORTING
      e_t_msg_handle = lt_msg_handle
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   no message found
    mac_symsg_raise not_found.
  ENDIF.

* get data of last message
  ld_count = lines( lt_msg_handle ).
  READ TABLE lt_msg_handle INDEX ld_count INTO ld_msg_handle.
  CALL FUNCTION 'BAL_LOG_MSG_READ'
    EXPORTING
      i_s_msg_handle = ld_msg_handle
    IMPORTING
      e_s_msg        = ls_msg
    EXCEPTIONS
      log_not_found  = 1
      msg_not_found  = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    mac_symsg_raise not_found.
  ENDIF.

  es_message = ls_msg.
  endmethod.


  method GET_LIST.
  DATA lt_msglist_x   TYPE t_msg_x.

  FIELD-SYMBOLS <ls_msglist_x> LIKE LINE OF lt_msglist_x.

* INIT RESULTS
  CLEAR et_list.

* BODY
  CALL METHOD get_list_x
    IMPORTING
      et_list_x = lt_msglist_x.

  LOOP AT lt_msglist_x ASSIGNING <ls_msglist_x>.
    APPEND <ls_msglist_x>-msg TO et_list.
  ENDLOOP.
  endmethod.


  method GET_LIST_AS_BAPIRET.
  DATA:
    lt_msglist_x  TYPE t_msg_x,
    ls_list       LIKE LINE OF et_list,
    ls_par        TYPE bal_s_par,
    ld_bapi_par   TYPE bapiret2-parameter,
    ld_bapi_field TYPE bapiret2-field,
    ld_bapi_row   TYPE bapiret2-row.

  FIELD-SYMBOLS <ls_msglist_x> LIKE LINE OF lt_msglist_x.

* INIT RESULTS
  CLEAR et_list.

* BODY
* get messages
  CALL METHOD get_list_x
    IMPORTING
      et_list_x = lt_msglist_x.

  LOOP AT lt_msglist_x ASSIGNING <ls_msglist_x>.
*   also try to get parameters
*   1. TABLE name
    READ TABLE <ls_msglist_x>-params-t_par INTO ls_par
      WITH KEY parname = 'TABNAME'.
    IF sy-subrc = 0.
      ld_bapi_par = ls_par-parvalue.
    ELSE.
      CLEAR ld_bapi_par.
    ENDIF.

*   2. FIELD name
    READ TABLE <ls_msglist_x>-params-t_par INTO ls_par
      WITH KEY parname = 'FIELDNAME'.
    IF sy-subrc = 0.
      ld_bapi_field = ls_par-parvalue.
    ELSE.
      CLEAR ld_bapi_field.
    ENDIF.

*   3. INDEX
    READ TABLE <ls_msglist_x>-params-t_par INTO ls_par
      WITH KEY parname = 'INDEX'.
    IF sy-subrc = 0.
      ld_bapi_row = ls_par-parvalue.
    ELSE.
      ld_bapi_row = 0.
    ENDIF.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type       = <ls_msglist_x>-msgty
        cl         = <ls_msglist_x>-msgid
        number     = <ls_msglist_x>-msgno
        par1       = <ls_msglist_x>-msgv1
        par2       = <ls_msglist_x>-msgv2
        par3       = <ls_msglist_x>-msgv3
        par4       = <ls_msglist_x>-msgv4
*       LOG_NO     = ' '
*       LOG_MSG_NO = ' '
        parameter  = ld_bapi_par
        row        = ld_bapi_row
        field      = ld_bapi_field
      IMPORTING
        return     = ls_list.
    APPEND ls_list TO et_list.
  ENDLOOP.
  endmethod.


  method GET_LIST_X.
  DATA:
    lt_log_handle   TYPE bal_t_logh,
    lt_msg_handle   TYPE bal_t_msgh,
    ls_msg_filter   TYPE bal_s_mfil,
    ls_msg          TYPE bal_s_msg,
    ls_list_x       LIKE LINE OF et_list_x.

  FIELD-SYMBOLS <ls_msg_handle> LIKE LINE OF lt_msg_handle.

* INIT RESULTS
  CLEAR et_list_x.

* PRECONDITION
  IF id_msgty NA 'IWEA'.
    mac_invalid_precondition.
  ENDIF.

* BODY
* get message filter by message type
  CALL METHOD get_msg_filter
    EXPORTING
      id_msgty      = id_msgty
      if_or_higher  = abap_true
    IMPORTING
      es_msg_filter = ls_msg_filter.

* get handles of messages
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
      i_s_msg_filter = ls_msg_filter
    IMPORTING
      e_t_msg_handle = lt_msg_handle
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   no message found
    RETURN.
  ENDIF.

* get message data
  LOOP AT lt_msg_handle ASSIGNING <ls_msg_handle>.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = <ls_msg_handle>
      IMPORTING
        e_s_msg        = ls_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      ls_list_x-msg       = ls_msg.
      ls_list_x-msgnumber = <ls_msg_handle>-msgnumber.
      APPEND ls_list_x TO et_list_x.
    ENDIF.

  ENDLOOP.
  endmethod.


  method GET_MSG_FILTER.
  DATA ls_so_msgty LIKE LINE OF es_msg_filter-msgty.

  DEFINE mac_add_msgty.
    clear ls_so_msgty.
    ls_so_msgty-sign   = 'I'.
    ls_so_msgty-option = 'EQ'.
    ls_so_msgty-low    = &1.
    append ls_so_msgty to es_msg_filter-msgty.
  END-OF-DEFINITION.

* INIT RESULTS
  CLEAR es_msg_filter.

* PRECONDITION
  IF id_msgty NA 'AEISWX'.
    mac_invalid_precondition.
  ENDIF.

* BODY
* something to do?
  IF ( id_msgty     CA 'IS'     ) AND
     ( if_or_higher = abap_true ).
    RETURN.
  ENDIF.

* set filter by message type
  mac_add_msgty id_msgty.

* add message type with same level
  CASE id_msgty.
    WHEN 'I'.     mac_add_msgty 'S'.
    WHEN 'S'.     mac_add_msgty 'I'.
    WHEN 'A'.     mac_add_msgty 'X'.
    WHEN 'X'.     mac_add_msgty 'A'.
  ENDCASE.

* add message types with higher level
  IF if_or_higher = abap_true.
    CASE id_msgty.
      WHEN 'I' OR 'S'.
        mac_add_msgty 'W'.
        mac_add_msgty 'E'.
        mac_add_msgty 'A'.
        mac_add_msgty 'X'.

      WHEN 'W'.
        mac_add_msgty 'E'.
        mac_add_msgty 'A'.
        mac_add_msgty 'X'.

      WHEN 'E'.
        mac_add_msgty 'A'.
        mac_add_msgty 'X'.

    ENDCASE.
  ENDIF.
  endmethod.


  method GET_REF_TYPE.
  DATA o_ref      TYPE REF TO cl_abap_refdescr.

  IF typedesc->kind = cl_abap_datadescr=>kind_ref.
    o_ref ?= typedesc.
    ref_type = o_ref->get_referenced_type( ).
  ELSE.
    ref_type = typedesc.
  ENDIF.
  endmethod.


  method GET_STATISTICS.
* clear result
  CLEAR rs_statistics.

* call log
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle = md_handle
    IMPORTING
      e_statistics = rs_statistics
    EXCEPTIONS
      OTHERS       = 0.
  endmethod.


  method GET_STATISTICS_AS_TEXT.
  DATA:
    ls_statistics   TYPE bal_s_scnt,
    ld_result       TYPE string,
    ld_result_cnt   TYPE string,
    ld_result_cnt_e TYPE string,
    ld_result_cnt_w TYPE string,
    ld_result_cnt_i TYPE string.

* INIT RESULTS
  CLEAR ed_text.

* BODY
  ls_statistics = get_statistics( ).

* evaluate E- / W- / I-messages only
  ADD ls_statistics-msg_cnt_a TO ls_statistics-msg_cnt_e.
  ADD ls_statistics-msg_cnt_s TO ls_statistics-msg_cnt_i.

* build text for each message type only
  IF if_output_e_messages = abap_true.
    IF ls_statistics-msg_cnt_e > 1.
      ld_result_cnt = ls_statistics-msg_cnt_e.
      ld_result_cnt_e = text-se0.
      REPLACE '&1' WITH ld_result_cnt INTO ld_result_cnt_e.
      CONDENSE ld_result_cnt_e.
    ELSEIF ls_statistics-msg_cnt_e = 1.
      ld_result_cnt_e = text-se1.
    ENDIF.
  ENDIF.

  IF if_output_w_messages = abap_true.
    IF ls_statistics-msg_cnt_w > 1.
      ld_result_cnt = ls_statistics-msg_cnt_w.
      ld_result_cnt_w = text-sw0.
      REPLACE '&1' IN ld_result_cnt_w WITH ld_result_cnt.
      CONDENSE ld_result_cnt_w.
    ELSEIF ls_statistics-msg_cnt_w = 1.
      ld_result_cnt_w = text-sw1.
    ENDIF.
  ENDIF.

  IF if_output_i_messages = abap_true.
    IF ls_statistics-msg_cnt_i > 1.
      ld_result_cnt = ls_statistics-msg_cnt_i.
      ld_result_cnt_i = text-si0.
      REPLACE '&1' WITH ld_result_cnt INTO ld_result_cnt_i.
      CONDENSE ld_result_cnt_i.
    ELSEIF ls_statistics-msg_cnt_i = 1.
      ld_result_cnt_i = text-si1.
    ENDIF.
  ENDIF.

* build resulting text
  IF ld_result_cnt_e IS NOT INITIAL.
    ld_result = ld_result_cnt_e.
  ENDIF.

  IF ld_result_cnt_w IS NOT INITIAL.
    IF ld_result IS INITIAL.
      ld_result = ld_result_cnt_w.
    ELSEIF if_output_highest_only = abap_false.
      CONCATENATE ld_result ', &1'
             INTO ld_result.
      REPLACE '&1' IN ld_result WITH ld_result_cnt_w.
    ENDIF.
  ENDIF.

  IF ld_result_cnt_i IS NOT INITIAL.
    IF ld_result IS INITIAL.
      ld_result = ld_result_cnt_i.
    ELSEIF if_output_highest_only = abap_false.
      CONCATENATE ld_result ', &1'
             INTO ld_result.
      REPLACE '&1' IN ld_result WITH ld_result_cnt_i.
    ENDIF.
  ENDIF.

* SET RESULT
  ed_text = ld_result.
  endmethod.


  method HAS_MESSAGES_OF_MSGT.
  DATA:
    ls_statistics TYPE bal_s_scnt,
    ld_msg_count  TYPE i.

* PRECONDITION
  IF id_msgty NA 'IWEA'.
    mac_invalid_precondition.
  ENDIF.

* BODY
  IF ( id_msgty     = 'I'       ) AND
     ( if_or_higher = abap_true ).

*   corresponds to "all messages"
    IF is_empty( ) = abap_false.
      rf_exists = abap_true.
    ENDIF.

  ELSE.

*   get statistics about messages
    ls_statistics = get_statistics( ).
    ld_msg_count  = 0.

    CASE id_msgty.
      WHEN 'I'.
        ld_msg_count = ls_statistics-msg_cnt_i +
                       ls_statistics-msg_cnt_s.

      WHEN 'W'.
        ld_msg_count = ls_statistics-msg_cnt_w.
        IF ( if_or_higher = abap_true ) AND ( ld_msg_count = 0 ).
          ld_msg_count = ls_statistics-msg_cnt_e +
                         ls_statistics-msg_cnt_a.
        ENDIF.

      WHEN 'E'.
        ld_msg_count = ls_statistics-msg_cnt_e.
        IF ( if_or_higher = abap_true ) AND ( ld_msg_count = 0 ).
          ld_msg_count = ls_statistics-msg_cnt_a.
        ENDIF.

      WHEN 'A'.
        ld_msg_count = ls_statistics-msg_cnt_a.

    ENDCASE.

*   check message counter
    IF ld_msg_count > 0.
      rf_exists = abap_true.
    ENDIF.

  ENDIF.
  endmethod.


  method INIT.
  DATA:
    lt_log_header TYPE balhdr_t,
    ls_log_header TYPE balhdr,
    ls_log_filter TYPE bal_s_lfil,
    ld_guid       TYPE guid_32.
*TYPE BU_AKTYP
*01    Create
*02    Change
*03    Display
*04    Modify (Direct Input: Create/Change)
*06    Delete
*11    Create defaults
*12    Change defaults
*13    Display defaults

  IF NOT auto_upd_custo IS INITIAL.
    check_custo( id_object = id_object
              id_subobject = id_subobject ).
  ENDIF.

* BODY
  IF id_activity = actions-create.                    "create

*   initialize application log data
    ms_log-object     = id_object.
    ms_log-subobject  = id_subobject.
    ms_log-aluser     = sy-uname.
    ms_log-alprog     = sy-cprog.
    ms_log-aldate_del = sy-datum + 30.      "expiration in 30 days

*   set external number
    IF id_extnumber IS NOT INITIAL.
      ms_log-extnumber = id_extnumber.
    ELSE.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = ld_guid.
      ms_log-extnumber = ld_guid.
    ENDIF.

*   create application log
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ms_log
      IMPORTING
        e_log_handle            = md_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.

*   find log header
    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING
        i_object       = id_object
        i_subobject    = id_subobject
        i_extnumber    = id_extnumber
      IMPORTING
        e_s_log_filter = ls_log_filter.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter     = ls_log_filter
      IMPORTING
        e_t_log_header     = lt_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      mac_symsg_raise error.
    ENDIF.

*    IF LINES( lt_log_header ) > 1.
*      MESSAGE e105(recabc)
*              RAISING error.
*    ENDIF.

*   load log
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header                = lt_log_header
*       I_DO_NOT_LOAD_MESSAGES        = ' '
*       I_EXCEPTION_IF_ALREADY_LOADED =
*     IMPORTING
*       E_T_LOG_HANDLE                =
*       E_T_MSG_HANDLE                =
      EXCEPTIONS
        no_logs_specified             = 1
        log_not_found                 = 2
        log_already_loaded            = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      mac_symsg_raise error.
    ENDIF.

*   initialize attributes
    READ TABLE lt_log_header INDEX 1 INTO ls_log_header.
    MOVE-CORRESPONDING ls_log_header TO ms_log.
    md_handle = ls_log_header-log_handle.

  ENDIF.

* set interface attribute
  object = id_object.
  subobject = id_subobject.
  ext_id = md_extnumber = ms_log-extnumber.

* initialize detail level
  md_level = mc_min_level.
  endmethod.


  method INIT_BY_HANDLE.
  DATA:
    lt_log_handle TYPE bal_t_logh,
    lf_load_error TYPE abap_bool.

* BODY
* load log
  INSERT id_handle INTO TABLE lt_log_handle.

* read log from DB
  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_handle     = lt_log_handle
    EXCEPTIONS
      no_logs_specified  = 1
      log_not_found      = 2
      log_already_loaded = 3
      OTHERS             = 4.
* ignore error (maybe the log is not yet stored)
  IF sy-subrc <> 0.
    lf_load_error = abap_true.
  ENDIF.

* initialize attributes
  CALL FUNCTION 'BAL_LOG_HDR_READ'
    EXPORTING
      i_log_handle = id_handle
    IMPORTING
      e_s_log      = ms_log
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    mac_symsg_raise error.
  ENDIF.

* if load error => do not delete log ( see method free)
  IF lf_load_error = abap_true.
    mf_log_not_stored = abap_true.
  ENDIF.

* initialize attributes
  md_handle    = id_handle.
  ext_id = md_extnumber = ms_log-extnumber.
  md_level     = mc_min_level.
  endmethod.


  method INSERT.
  DATA:
    lt_msglist_x              TYPE t_msg_x,
    ld_insert_index           TYPE i,
    ld_delete_index           TYPE i,
    ld_num_detlevel_new       TYPE i,
    ld_num_detlevel_next      TYPE i,
    ld_num_detlevel_diff      TYPE i,
    ld_num_detlevel_following TYPE i.

  FIELD-SYMBOLS <ls_msglist_x>            LIKE LINE OF lt_msglist_x.

* BODY
* adopt index as inserting position
* (index = 0 or index = 1 inserts the new message as the 1st message)
  IF id_insert_index > 0.
    ld_insert_index = id_insert_index.
  ELSE.
    ld_insert_index = 1.
  ENDIF.

* get all current messages
  CALL METHOD get_list_x
    IMPORTING
      et_list_x = lt_msglist_x.

* insert new message
  IF ld_insert_index > lines( lt_msglist_x ).

*   simply append as last message
    CALL METHOD add
      EXPORTING
        is_message   = is_message
        id_msgty     = id_msgty
        id_msgid     = id_msgid
        id_msgno     = id_msgno
        id_msgv1     = id_msgv1
        id_msgv2     = id_msgv2
        id_msgv3     = id_msgv3
        id_msgv4     = id_msgv4
        id_msgd1     = id_msgd1
        id_msgd2     = id_msgd2
        id_msgd3     = id_msgd3
        id_msgd4     = id_msgd4
        id_detlevel  = id_detlevel
        id_probclass = id_probclass
*        id_tabname   = id_tabname
*        id_fieldname = id_fieldname
*        id_value     = id_value
        id_index     = id_index
*        id_intreno   = id_intreno
*        id_context   = id_context
      IMPORTING
        es_message   = es_message.

  ELSE.

*   insert message with specified index

*   delete all following messages from message list and
*   hold only these messages in lt_msglist_x
    IF ld_insert_index > 1.
      ld_delete_index = ld_insert_index - 1.
      DELETE lt_msglist_x TO ld_delete_index.

      LOOP AT lt_msglist_x ASSIGNING <ls_msglist_x>.
        CALL METHOD delete_message
          EXPORTING
            id_msgnumber = <ls_msglist_x>-msgnumber
          EXCEPTIONS
            OTHERS       = 0.
      ENDLOOP.
    ELSE.
      clear( ).
    ENDIF.

*   append new message to temporarily reduced list
    CALL METHOD add
      EXPORTING
        is_message   = is_message
        id_msgty     = id_msgty
        id_msgid     = id_msgid
        id_msgno     = id_msgno
        id_msgv1     = id_msgv1
        id_msgv2     = id_msgv2
        id_msgv3     = id_msgv3
        id_msgv4     = id_msgv4
        id_msgd1     = id_msgd1
        id_msgd2     = id_msgd2
        id_msgd3     = id_msgd3
        id_msgd4     = id_msgd4
        id_detlevel  = id_detlevel
        id_probclass = id_probclass
*        id_tabname   = id_tabname
*        id_fieldname = id_fieldname
*        id_value     = id_value
        id_index     = id_index
*        id_intreno   = id_intreno
*        id_context   = id_context
      IMPORTING
        es_message   = es_message.

*   determine detail level for subnode
    IF if_set_following_as_subnode = abap_true.
      READ TABLE lt_msglist_x INDEX 1 ASSIGNING <ls_msglist_x>.
      ld_num_detlevel_next = <ls_msglist_x>-detlevel.
      ld_num_detlevel_new  = es_message-detlevel.
      ld_num_detlevel_diff = ld_num_detlevel_new  -
                             ld_num_detlevel_next + 1.
    ENDIF.

*   append deleted messages
    LOOP AT lt_msglist_x ASSIGNING <ls_msglist_x>.

*     clear some fields as messages are "new"
      CLEAR:
        <ls_msglist_x>-time_stmp,
        <ls_msglist_x>-msg_count.

*     adopt detail level
      IF if_set_following_as_subnode = abap_true.
        ld_num_detlevel_following = <ls_msglist_x>-detlevel +
                                    ld_num_detlevel_diff.
        IF ld_num_detlevel_following < 0.
          <ls_msglist_x>-detlevel = 0.
        ELSEIF ld_num_detlevel_following > mc_max_level.
          <ls_msglist_x>-detlevel = mc_max_level.
        ELSE.
          <ls_msglist_x>-detlevel = ld_num_detlevel_following.
        ENDIF.
      ENDIF.

*     add message
      CALL METHOD add( is_message = <ls_msglist_x>-msg ).

    ENDLOOP.

  ENDIF.
  endmethod.


  method IS_DATE_INITIAL.
* PRECONDITION
  IF ( id_date(1) >= '1' ).
    rf_initial = abap_false.
    RETURN.
  ENDIF.

* BODY
  IF ( id_date IS INITIAL    ) OR
     ( id_date  = space      ) OR
     ( id_date  = '00000000' ).
    rf_initial = abap_true.
  ENDIF.
  endmethod.


  method IS_EMPTY.
  DATA lt_log_handle TYPE bal_t_logh.

* BODY
* try to get handles of messages
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   no message found
    rf_empty = abap_true.
  ENDIF.
  endmethod.


  method NEW.
  CREATE OBJECT obj.
  endmethod.


  method RAISE_FIRST_MESSAGE.
  DATA ls_msg TYPE BAL_S_MSG.

* BODY
  CALL METHOD get_first_message
    EXPORTING
      id_msgty     = id_msgty
      if_or_higher = if_or_higher
    IMPORTING
      es_message   = ls_msg
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc = 0.
    MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
            WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
            RAISING first_message.
  ENDIF.
  endmethod.


  method RAISE_ON_ERROR.
  DATA o_exception TYPE REF TO zaplink_cx.

  IF NOT has_messages_of_msgt( id_msgty = 'E' ) IS INITIAL.
* Fatal Errors occurs, check ba log (SLG1) : '&1' '&2' '&3'
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE e012(zaplink_package) WITH space space space. ENDIF.
    SET EXTENDED CHECK ON.
    add_abend( id_msgid = 'ZAPLINK_PACKAGE'
               id_msgno = '012'
               id_msgv1 = object
               id_msgv2 = subobject
               id_msgv3 = ext_id
*             id_msgv4 = id_msgv4
*               id_detlevel = '1'
*               id_probclass = '1'
                ).

    o_exception = zaplink_cx=>create_from_application_log( message_collector = me ).

    RAISE EXCEPTION o_exception.
  ENDIF.
  endmethod.


  method SET_DETAIL_LEVEL.
  DATA:
    ld_num_min_level    LIKE id_relative,
    ld_num_max_level    LIKE id_relative,
    ld_num_level_signed LIKE id_relative.

* PRECONDITION
  IF ( id_absolute IS NOT INITIAL ) AND
     ( id_relative IS NOT INITIAL ).
    mac_invalid_precondition.
  ENDIF.

* BODY
  IF id_absolute IS NOT INITIAL.

*   set level as absolute number
    IF id_absolute BETWEEN mc_min_level AND mc_max_level.
      md_level = id_absolute.
    ENDIF.

  ELSEIF id_relative IS NOT INITIAL .

*   set level relative to the previous one
    ld_num_min_level = mc_min_level.
    ld_num_max_level = mc_max_level.

    ld_num_level_signed = md_level.
    ADD id_relative TO ld_num_level_signed.

    IF ld_num_level_signed < ld_num_min_level.
      md_level = mc_min_level.
    ELSEIF ld_num_level_signed > ld_num_max_level.
      md_level = mc_max_level.
    ELSE.
      md_level = ld_num_level_signed.
    ENDIF.

  ENDIF.
  endmethod.


  method SET_EXTNUMBER.
  DATA ls_log_temp LIKE ms_log.

* PRECONDITION
  CHECK id_extnumber IS NOT INITIAL.

* BODY
  ls_log_temp           = ms_log.
  ls_log_temp-extnumber = id_extnumber.

  CALL FUNCTION 'BAL_LOG_HDR_CHANGE'
    EXPORTING
      i_log_handle            = md_handle
      i_s_log                 = ls_log_temp
    EXCEPTIONS
      log_not_found           = 1
      log_header_inconsistent = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
*   ignore failure
    RETURN.
  ENDIF.

  ms_log-extnumber = id_extnumber.
  ext_id = md_extnumber     = id_extnumber.
  endmethod.


  method STORE.
  DATA lt_log_handle TYPE bal_t_logh.

* BODY
  INSERT md_handle INTO TABLE lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
*     I_CLIENT         = SY-MANDT
      i_in_update_task = if_in_update_task
*     I_SAVE_ALL       = ' '
      i_t_log_handle   = lt_log_handle
*   IMPORTING
*     E_NEW_LOGNUMBERS =
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    mac_symsg_raise error.
  ENDIF.
  endmethod.


  method WRITE.
  DATA lt_messages  TYPE t_msg_x.
  DATA _msg TYPE string.
  DATA _light TYPE icon_d.
  FIELD-SYMBOLS <m> LIKE LINE OF lt_messages.

* get messages
  CALL METHOD get_list_x
    IMPORTING
      et_list_x = lt_messages.

  LOOP AT lt_messages ASSIGNING <m>.
    CASE <m>-msgty.
      WHEN 'X'. _light = icon_red_xcircle.
      WHEN 'A'. _light = icon_message_critical_small.
      WHEN 'E'. _light = icon_red_light.
      WHEN 'W'. _light = icon_yellow_light.
      WHEN 'I'. _light = icon_light_out.
      WHEN 'S'. _light = icon_green_light.
      WHEN OTHERS. _light = icon_light_out.
    ENDCASE.
    MESSAGE ID <m>-msgid TYPE 'I' NUMBER <m>-msgno WITH <m>-msgv1 <m>-msgv2 <m>-msgv3 <m>-msgv4 INTO _msg.
    WRITE:/ _offset, _light AS ICON, _msg.
  ENDLOOP.
  endmethod.


  method _ADD_EXCEPTION.
  DATA _detlevel  TYPE ballevel.
  DATA cx_text    TYPE string.
  DATA o_ref      TYPE REF TO cl_abap_refdescr.
  DATA o_obj      TYPE REF TO cl_abap_classdescr.
*  DATA prog       TYPE syrepid.
*  DATA include    TYPE syrepid.
*  DATA line       TYPE i.
  DATA cx_src_pos TYPE string.
  DATA f_toolong  TYPE abap_bool.
  DATA:
    BEGIN OF s_msg,
      msgv1	TYPE symsgv,
      msgv2	TYPE symsgv,
      msgv3	TYPE symsgv,
      msgv4	TYPE symsgv,
    END OF s_msg.

  IF exception->previous IS BOUND.
    _detlevel = id_detlevel + 1.
    CALL METHOD add_exception
      EXPORTING
        exception    = exception->previous
        id_detlevel  = _detlevel
        id_probclass = id_probclass.
  ENDIF.

  o_ref ?= cl_abap_classdescr=>describe_by_data( p_data = exception ).
  o_obj ?= get_ref_type( o_ref ).

  cx_src_pos = zaplink_cx=>source_position_as_string( exception ).
*  CALL METHOD exception->get_source_position
*    IMPORTING
*      program_name = prog
*      include_name = include
*      source_line  = line.
*  cx_src_pos = line.
*  IF include = prog.    CLEAR include.    ENDIF.
*  IF include IS INITIAL.
*    CONCATENATE prog ' @' cx_src_pos INTO cx_src_pos.
*  ELSE.
*    CONCATENATE include '(' prog ') @' cx_src_pos INTO cx_src_pos.
*  ENDIF.

  s_msg = o_obj->absolute_name.
  IF NOT s_msg-msgv2 IS INITIAL OR NOT s_msg-msgv3 IS INITIAL OR NOT s_msg-msgv4 IS INITIAL.    f_toolong = abap_true.    ENDIF.
  s_msg = cx_src_pos.
  IF NOT s_msg-msgv2 IS INITIAL OR NOT s_msg-msgv3 IS INITIAL OR NOT s_msg-msgv4 IS INITIAL.    f_toolong = abap_true.    ENDIF.
  s_msg = cx_text = exception->if_message~get_text( ).
  IF NOT s_msg-msgv3 IS INITIAL OR NOT s_msg-msgv4 IS INITIAL.    f_toolong = abap_true.    ENDIF.
  IF f_toolong IS INITIAL.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE e002(zaplink_easyxml) WITH abap_true abap_true abap_true abap_true. ENDIF.
    SET EXTENDED CHECK ON.
    CALL METHOD add_error
      EXPORTING
        id_msgid     = 'ZAPLINK_EASYXML'
        id_msgno     = '002'
        id_msgv1     = o_obj->absolute_name
        id_msgv2     = s_msg-msgv1
        id_msgv3     = s_msg-msgv2
        id_msgv4     = cx_src_pos
        id_detlevel  = id_detlevel
        id_probclass = id_probclass.
  ELSE.
    s_msg = o_obj->absolute_name.   s_msg+100 = cx_src_pos.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE e012(zaplink_easyxml) WITH abap_true abap_true abap_true abap_true. ENDIF.
    SET EXTENDED CHECK ON.
    CALL METHOD add_error
      EXPORTING
        id_msgid     = 'ZAPLINK_EASYXML'
        id_msgno     = '012'
        id_msgv1     = s_msg-msgv1
        id_msgv2     = s_msg-msgv2
        id_msgv3     = s_msg-msgv3
        id_msgv4     = s_msg-msgv4
        id_detlevel  = id_detlevel
        id_probclass = id_probclass.
    WHILE NOT cx_text IS INITIAL.
      s_msg = cx_text.
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e011(zaplink_easyxml) WITH abap_true abap_true abap_true abap_true. ENDIF.
      SET EXTENDED CHECK ON.
      CALL METHOD add_info
        EXPORTING
          id_msgid     = 'ZAPLINK_EASYXML'
          id_msgno     = '011'
          id_msgv1     = s_msg-msgv1
          id_msgv2     = s_msg-msgv2
          id_msgv3     = s_msg-msgv3
          id_msgv4     = s_msg-msgv4
          id_detlevel  = id_detlevel
          id_probclass = id_probclass.
      IF STRLEN( cx_text ) < 200.   CLEAR cx_text.    ELSE.   SHIFT cx_text BY 200 PLACES LEFT.    ENDIF.
    ENDWHILE.
  ENDIF.
  endmethod.


  method _ADD_ZL_CX.
  DATA _detlevel  TYPE ballevel.

  IF exception->previous IS BOUND.
    _detlevel = id_detlevel + 1.
    CALL METHOD add_exception
      EXPORTING
        exception    = exception->previous
        id_detlevel  = _detlevel
        id_probclass = id_probclass.
  ENDIF.

  CALL METHOD _add_exception
    EXPORTING
      exception    = exception
      id_detlevel  = id_detlevel
      id_probclass = id_probclass.

  IF exception->messages IS BOUND.
    CALL METHOD add_from_instance
      EXPORTING
        io_msglist        = exception->messages
*      if_cumulate       =
        if_add_as_subnode = abap_true.
  ENDIF.
  endmethod.
ENDCLASS.
