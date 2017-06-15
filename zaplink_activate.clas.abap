class ZAPLINK_ACTIVATE definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools SEWOR .
  type-pools SWBM .

  interfaces ZAPLINK_DATATYPES .

  aliases TD_CHECKBOX
    for ZAPLINK_DATATYPES~TD_CHECKBOX .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TR_AUTHOR
    for ZAPLINK_DATATYPES~TR_AUTHOR .
  aliases TR_COMPNAME
    for ZAPLINK_DATATYPES~TR_COMPNAME .
  aliases TR_COMPTYPE
    for ZAPLINK_DATATYPES~TR_COMPTYPE .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TR_SRCSYSTEM
    for ZAPLINK_DATATYPES~TR_SRCSYSTEM .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .

  types TO_LIST type ref to ZAPLINK_LIST .
  types:
    TT_OBJLIST type STANDARD TABLE OF E071 with NON-UNIQUE DEFAULT KEY .
  types:
    tt_GENSET  type STANDARD TABLE OF GENSETC with NON-UNIQUE DEFAULT KEY .
  types:
    tt_det_obj TYPE STANDARD TABLE OF dwinactiv WITH NON-UNIQUE DEFAULT KEY .

  data ACTIVATION_REQUESTED type TD_CHECKBOX read-only .
  data OBJECT_COUNT type I read-only .
  data FORCE_ACTIVATION type TD_CHECKBOX read-only .

  methods ADD_FROM_TADIR
    importing
      !SRCSYSTEM type TR_SRCSYSTEM
      !AUTHOR type TR_AUTHOR
      !TYPE type TR_COMPTYPE
      !NAME type TR_COMPNAME
      !PACKAGES type TR_PACKAGES
      !EXT type ABAP_BOOL
    raising
      ZAPLINK_CX .
  methods RESET .
  methods ADD_KEYS
    importing
      !OBJ_LIST type TT_COMPKEYS .
  methods ADD_LIST
    importing
      !O_LIST type TO_LIST .
  methods ASK_FOR_ACTIVATION
    returning
      value(RESULT) type TT_COMPKEYS
    raising
      ZAPLINK_CX .
  methods ACTIVATE
    importing
      !FORCE_ACTIVATION type TD_CHECKBOX optional
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods GET_INACTIVE_OBJECTS
    importing
      !OBJ_LIST type TT_OBJLIST
    returning
      value(INACT) type TT_DET_OBJ .
  class-methods CHECK_INACTIVE_OBJECTS
    changing
      !OBJ_LIST type TT_OBJLIST .
  methods GENERATE
    returning
      value(RESULT) type TT_COMPKEYS .
protected section.

  types:
    BEGIN OF t_mainprog,
      master TYPE d010inc-master,
      include TYPE d010inc-include,
      object TYPE e071-object,
      error(1) TYPE c,
      switch_state(1) TYPE c,
    END OF t_mainprog .
  types:
    tt_mainprogs TYPE STANDARD TABLE OF t_mainprog WITH NON-UNIQUE DEFAULT KEY .
  types:
    tt_items TYPE STANDARD TABLE OF sewor_working_area WITH NON-UNIQUE DEFAULT KEY .

  data IT_ITEMS type TT_ITEMS .
  data IT_OBJ_LIST type TT_OBJLIST .
  data COMPONENT_LIST type TT_COMPKEYS .

  class-methods SPLIT_LIST
    importing
      !OBJ_LIST type TT_OBJLIST
    exporting
      !DDIC type TT_OBJLIST
      !PROG_LIST type TT_OBJLIST .
  class-methods ACTIVATE_OBJECTS
    importing
      !PROGRAMS type TT_MAINPROGS
      !ITEMS type TT_ITEMS .
  class-methods ACTIVATE_DDIC_V2
    importing
      !ONE_BY_ONE type XFLAG optional
    changing
      !IT_LIST type TT_OBJLIST .
  class-methods ACTIVATE_NONDDIC
    importing
      !ONE_BY_ONE type TD_CHECKBOX optional
      !FORCE_ACTIVATION type TD_CHECKBOX optional
    changing
      !IT_LIST type TT_OBJLIST .
  class-methods ACTIVATE_DDIC
    changing
      !IT_LIST type TT_OBJLIST .
  methods ADD_OBJ_TO_WA .
  class-methods CONV_LIST
    importing
      !OBJ_LIST type TT_OBJLIST
    returning
      value(ITEM_LIST) type TT_ITEMS .
  class-methods COMPKEYS_2_OBJLIST
    importing
      !KEYS type TT_COMPKEYS
    returning
      value(RESULT) type TT_OBJLIST .
  class-methods OBJLIST_2_COMPKEYS
    importing
      !KEYS type TT_OBJLIST
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods COMPKEYS_2_GENSET
    importing
      !KEYS type TT_COMPKEYS
    returning
      value(RESULT) type TT_GENSET .
  class-methods GENSET_2_COMPKEYS
    importing
      !KEYS type TT_GENSET
    returning
      value(RESULT) type TT_COMPKEYS .
private section.
ENDCLASS.



CLASS ZAPLINK_ACTIVATE IMPLEMENTATION.


  method ACTIVATE.
  DATA:
    lb_actdone TYPE xflag,
    lc_before  TYPE i,
    lc_after   TYPE i,
    it_items   TYPE tt_items,
    it_progs   TYPE tt_objlist,
    it_mainp   TYPE tt_mainprogs,
    _mp        LIKE LINE OF it_mainp,
    dummy_tab  TYPE tt_items,
    it_objlist TYPE tt_objlist,
    it_ddic    TYPE tt_objlist,
    it_det_obj TYPE tt_det_obj.
*  FIELD-SYMBOLS:
*    <mainp> LIKE LINE OF it_mainp,
*    <item>  LIKE LINE OF it_items,
*    <obj>   LIKE LINE OF it_obj_list.

  it_obj_list = compkeys_2_objlist( component_list ).
  SORT it_obj_list BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM it_obj_list COMPARING object obj_name.

  CALL METHOD split_list
    EXPORTING
      obj_list  = it_obj_list
    IMPORTING
      ddic      = it_ddic
      prog_list = it_progs.

* Try mass activation first (usefull for non DDIC objects)
  activate_ddic( CHANGING it_list = it_ddic ).
  activate_nonddic( CHANGING it_list = it_progs ).
  COMMIT WORK AND WAIT.

* do the loop
  DO.
    CLEAR lb_actdone.

*Active DDIC
    IF NOT it_ddic IS INITIAL.
      lc_before = LINES( it_ddic ).
      activate_ddic( CHANGING it_list = it_ddic ).
      lc_after = LINES( it_ddic ).
      IF lc_after < lc_before.
        lb_actdone = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

*Active NON DDIC
    IF NOT it_progs IS INITIAL.
      lc_before = LINES( it_progs ).
      activate_nonddic( EXPORTING one_by_one = 'X'
                         CHANGING    it_list = it_progs ).
      lc_after = LINES( it_progs ).
      IF lc_after >= lc_before.
        activate_nonddic( CHANGING    it_list = it_progs ).    " Try mass activation
        lc_after = LINES( it_progs ).
      ENDIF.
      IF lc_after < lc_before.
        lb_actdone = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

    IF lb_actdone IS INITIAL. EXIT. ENDIF.
  ENDDO.

  IF force_activation IS NOT INITIAL.
    activate_nonddic( EXPORTING force_activation = force_activation
                                CHANGING it_list = it_progs ).
  ENDIF.

  APPEND LINES OF it_ddic TO it_objlist.
  APPEND LINES OF it_progs TO it_objlist.
  result = objlist_2_compkeys( it_objlist ).
** Switch into activation and generation
*  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
*    EXPORTING
**     SUPPRESS_SYNTAX_CHECK        =
*      suppress_generation          = 'X'
**     P_WB_MANAGER                 =
**     SUPPRESS_INSERT              =
*     activate_ddic_objects        = 'X'
**     WITH_POPUP                   = ' '
*    TABLES
*      OBJECTS                      = it_det_obj
*    EXCEPTIONS
*      excecution_error             = 1
*      cancelled                    = 2
*      insert_into_corr_error       = 3
*      OTHERS                       = 4
*            .
*  IF sy-subrc <> 0.
**    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  it_det_obj = get_inactive_objects( it_progs ).
*
*  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
*    EXPORTING
**     SUPPRESS_SYNTAX_CHECK        =
*      suppress_generation          = 'X'
**     P_WB_MANAGER                 =
*      SUPPRESS_INSERT              = 'X'
**     ACTIVATE_DDIC_OBJECTS        = ' '
**     WITH_POPUP                   = ' '
*    TABLES
*      OBJECTS                      = it_det_obj
*    EXCEPTIONS
*      excecution_error             = 1
*      cancelled                    = 2
*      insert_into_corr_error       = 3
*      OTHERS                       = 4
*            .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  it_items = conv_list( it_progs ).
*
** FROM LSEWORKINGAREAF01
**  PERFORM get_main_programs USING 'X' with_popup .
*  DATA: l_progs TYPE rsnewleng-programm,
*        l_main TYPE STANDARD TABLE OF rsnewleng-programm WITH NON-UNIQUE DEFAULT KEY,
*        l_index TYPE i.
*  FIELD-SYMBOLS:
*    <pname> LIKE LINE OF l_main.
*
** Selektierte f#r ABAP setzen
*  CALL FUNCTION 'RS_WORKING_AREA_PREPARE'
*    EXPORTING
*      suppress_init = 'X'
*    EXCEPTIONS
*      OTHERS        = 1.
*
** FROM LSEWORKINGAREAF01 (366)
*  REFRESH it_mainp.
*  CLEAR it_mainp.
*  LOOP AT it_items ASSIGNING <item>.
*
*    CASE <item>-object.
*      WHEN 'CPRI' OR 'CPRO' OR 'CPUB' OR 'CREP' OR 'METH' OR 'INTF'
*           OR 'CINC' OR 'CLAS'.
**       Geht schneller die Klasse zu bestimmen
*        _mp-include = <item>-obj_name.
*        <item>-obj_name = <item>-obj_name(30).
*        TRANSLATE <item>-obj_name USING '= '.
*        _mp-master = <item>-obj_name.
*        _mp-object = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*      WHEN 'WAPD' OR 'WAPA'.
**       BSP-Applikation
*        _mp-include = <item>-obj_name.
*        _mp-master  = <item>-obj_name.
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*      WHEN 'WAPP'.
**       BSP-Seite
*        _mp-include = <item>-obj_name+30.
*        _mp-master  = <item>-obj_name(30).
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*      WHEN 'WDYN'.
**       Web Dynpro-Applikation
*        _mp-include = <item>-obj_name.
*        _mp-master  = <item>-obj_name.
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*      WHEN 'WDYD' OR 'WDYV' OR 'WDYC'.
**       Web Dynpro
*        _mp-include = <item>-obj_name+30.
*        _mp-master  = <item>-obj_name(30).
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*
*      WHEN 'ENHO' OR 'ENHC' OR 'ENHS' OR 'ENSC'.
**       Enhancement Objects
*        _mp-include = <item>-obj_name.
*        _mp-master  = <item>-obj_name.
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*
*      WHEN 'SFBF' OR 'SFSW' OR 'SFBS' OR 'SF01'.
**       Switch Framework
*        _mp-include = <item>-obj_name.
*        _mp-master  = <item>-obj_name.
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*
*      WHEN 'WEBI' OR 'WEBS'.
**       Webservices
*        _mp-include = <item>-obj_name.
*        _mp-master  = <item>-obj_name.
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*      WHEN 'SFPF' OR 'SFPI'.
**       Formulare
*        _mp-include = <item>-obj_name.
*        _mp-master  = <item>-obj_name.
*        _mp-object  = <item>-object.
*        APPEND _mp TO it_mainp.
*        CONTINUE.
*      WHEN OTHERS.
*        PERFORM convert_object_for_abap IN PROGRAM saplseworkingarea
*                                          TABLES dummy_tab
*                                        CHANGING <item>.
*    ENDCASE.
*    CHECK <item>-object = 'REPS'.
*    CHECK <item>-obj_name(1) <> '<' AND <item>-obj_name(1) <> '>'.
*    l_progs = <item>-obj_name.
**   Type-Pools
*    IF <item>-obj_name(3) = '%_C'.
*      _mp-include = <item>-obj_name.
*      _mp-master = <item>-obj_name.
*      _mp-object = <item>-object.
*      APPEND _mp TO it_mainp.
*      CONTINUE.
*    ENDIF.
**   PG030801 wenn Rahmenprogramm zu REPS schon bekannt, kein Dialog
*    IF <item>-object = 'REPS'
*    AND <item>-main_item_object = 'PROG'
*    AND <item>-main_item NE space.
*      _mp-include = <item>-obj_name.
*      _mp-master = <item>-main_item.
*      _mp-object = <item>-object.
*      APPEND _mp TO it_mainp.
*    ELSE.
**     Rahmenprogramme per Dialog bestimmen
*      CALL FUNCTION 'RS_GET_MAINPROGRAMS'
*         EXPORTING
*              fulltab      = 'X'
**             dialog_required = 'X'
*              dialog       = ' ' "with_dialog  "  'X'
*              name         = l_progs
*         IMPORTING
*              tindex       = l_index
**              number_of_mainprograms = l_number
*         TABLES
*              mainprograms = l_main
*         EXCEPTIONS
*              cancelled    = 1
*              OTHERS       = 2.
*      IF sy-subrc = 1.
**        g_cancel_flag = 'X'.
**        work_items[] = l_items[].
**        working_area[] = l_work[].
**        EXIT.
*      ENDIF.
*      IF l_index <> 0.
*        READ TABLE l_main INDEX l_index ASSIGNING <pname>.
*        IF sy-subrc = 0.
*          _mp-include = <item>-obj_name.
*          _mp-master = <pname>.
*          _mp-object = <item>-object.
*          APPEND _mp TO it_mainp.
*        ENDIF.
*      ELSE.
*        LOOP AT l_main ASSIGNING <pname>.
*          _mp-include = <item>-obj_name.
*          _mp-master = <pname>.
*          _mp-object = <item>-object.
*          APPEND _mp TO it_mainp.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  CALL METHOD activate_objects
*    EXPORTING
*      programs = it_mainp
*      items    = it_items.
  endmethod.


  method ACTIVATE_DDIC.
  DATA:
    it_debug TYPE STANDARD TABLE OF ddmsactrc WITH NON-UNIQUE DEFAULT KEY.
  FIELD-SYMBOLS:
    <wa> LIKE LINE OF it_debug.

*Active DDIC
  CALL FUNCTION 'SEO_BUFFER_REFRESH'
   EXPORTING
*     CIFKEY        =
*     VERSION       = SEOC_VERSION_INACTIVE
      force         = 'X'.
  CALL FUNCTION 'SEO_BUFFER_REFRESH'
   EXPORTING
*     CIFKEY        =
      version       = 1
      force         = 'X'.

*  CALL FUNCTION 'RS_DD_WORKLIST_ACTIVATE' in UPDATE TASK
*    EXPORTING
*      p_generate_active_obj = 'X'
*      i_suppress_dialog     = 'X'
*    TABLES
*      t_worklist            = it_list
*      t_act_obj             = it_debug.
  CALL FUNCTION 'RS_DD_WORKLIST_ACTIVATE'
    EXPORTING
      p_generate_active_obj = 'X'
      i_suppress_dialog     = 'X'
    TABLES
      t_worklist            = it_list
      t_act_obj             = it_debug
    EXCEPTIONS
      not_all_activated     = 1
      error_occured         = 2
      locked                = 3
      OTHERS                = 4.
  IF sy-subrc <= 1.
    LOOP AT it_debug ASSIGNING <wa>.
      DELETE it_list
          WHERE pgmid = <wa>-pgmid
            AND object = <wa>-type
            AND obj_name = <wa>-name.
    ENDLOOP.
  ENDIF.
  COMMIT WORK AND WAIT.
  endmethod.


  method ACTIVATE_DDIC_V2.
  DATA:
    _wb_manager TYPE REF TO  if_wb_manager,   "#EC NEEDED
    it_obj TYPE tt_det_obj,
    it_det_obj TYPE tt_det_obj.
*  FIELD-SYMBOLS:
*    <disable_popup> TYPE data,        " DATA: xcode LIKE rsnewleng-tcode,
*    <obj> LIKE LINE OF it_det_obj.

*Active NON DDIC
*  ASSIGN ('(SAPLSEWORKINGAREA)xcode') TO <disable_popup>.
*  IF sy-subrc = 0.
*    <disable_popup> = 'CHEC'. " LSEWORKINGAREAFCK from CHECKLIST_EVALUATE line 2134
*  ENDIF.
*  it_det_obj = get_inactive_objects( it_list ).
*  count = LINES( it_det_obj ).
*  IF one_by_one IS INITIAL.
**    it_det_obj = conv_list( it_list ).
  it_det_obj = get_inactive_objects( it_list ).
  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
    EXPORTING
*     SUPPRESS_SYNTAX_CHECK        =
*     SUPPRESS_GENERATION          =
*     P_WB_MANAGER                 =
*     SUPPRESS_INSERT              =
      activate_ddic_objects        = 'X'
*     WITH_POPUP                   = ' '
*     CWB_MODE                     =
    TABLES
      OBJECTS                      = it_det_obj
    EXCEPTIONS
      excecution_error             = 1
      cancelled                    = 2
      insert_into_corr_error       = 3
      OTHERS                       = 4
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'RS_MASS_ACTIVATION'
    EXPORTING
      p_wb_manager = _wb_manager
    TABLES
      OBJECTS      = it_det_obj.
*  ELSE.
*    it_det_obj = conv_list( it_list ).
*    LOOP AT it_det_obj ASSIGNING <obj>.
*      CLEAR it_obj.
*      APPEND <obj> TO it_obj.
*      CALL FUNCTION 'RS_MASS_ACTIVATION'
*        EXPORTING
*          p_wb_manager = _wb_manager
*        TABLES
*          OBJECTS      = it_obj.
*    ENDLOOP.
*  ENDIF.
  check_inactive_objects( CHANGING obj_list = it_list ).
  endmethod.


  method ACTIVATE_NONDDIC.
  DATA:
    _wb_manager TYPE REF TO  if_wb_manager,                 "#EC NEEDED
    it_obj TYPE tt_det_obj,
    it_det_obj TYPE tt_det_obj.
  FIELD-SYMBOLS:
    <disable_popup> TYPE data,        " DATA: xcode LIKE rsnewleng-tcode,
    <obj> LIKE LINE OF it_det_obj.

*Active NON DDIC
  ASSIGN ('(SAPLSEWORKINGAREA)xcode') TO <disable_popup>.
  IF sy-subrc = 0.
    <disable_popup> = 'CHEC'. " LSEWORKINGAREAFCK from CHECKLIST_EVALUATE line 2134
  ENDIF.
*  it_det_obj = get_inactive_objects( it_list ).
*  count = LINES( it_det_obj ).
  IF one_by_one IS INITIAL.
    it_det_obj = conv_list( it_list ).
*    it_det_obj = get_inactive_objects( it_list ).
    CALL FUNCTION 'RS_MASS_ACTIVATION'
      EXPORTING
        p_wb_manager = _wb_manager
      TABLES
        OBJECTS      = it_det_obj.
    IF force_activation IS NOT INITIAL.
      PERFORM activate_object IN PROGRAM saplseworkingarea.
    ENDIF.
  ELSE.
    it_det_obj = conv_list( it_list ).
    LOOP AT it_det_obj ASSIGNING <obj>.
      CLEAR it_obj.
      APPEND <obj> TO it_obj.
      CALL FUNCTION 'RS_MASS_ACTIVATION'
        EXPORTING
          p_wb_manager = _wb_manager
        TABLES
          OBJECTS      = it_obj.
    ENDLOOP.
  ENDIF.
  check_inactive_objects( CHANGING obj_list = it_list ).
  endmethod.


  method ACTIVATE_OBJECTS.
  FIELD-SYMBOLS:
    <it> LIKE LINE OF it_items,
    <mp> LIKE LINE OF programs.

* Code from LSEWORKINGAREAFGE (form generate_active_object)
  DATA: l_w_area TYPE tt_items,
        l_w_items TYPE tt_items,
        l_master TYPE rsnewleng-programm,
        BEGIN OF l_dynp_id,
          prog TYPE progname,
          dnum TYPE dynpronr,
        END OF l_dynp_id,
        message(240) TYPE c,
        l_page TYPE seu_objkey,
        l_imp_name TYPE exit_imp,
        l_navigate TYPE sy-calld,
        l_subrc TYPE sy-subrc,
        l_clskey TYPE seoclskey,
        l_wapd_class TYPE seoclskey-clsname
                   VALUE 'CL_O2_API_APPLICATION',
        l_sfpf_class TYPE seoclskey-clsname
                   VALUE 'CL_FP_WB_HELPER',
        l_appl TYPE o2applname,
        l_obj_name TYPE e071-obj_name,
        l_exception TYPE REF TO cx_root,
        l_message_text TYPE string.

  LOOP AT programs ASSIGNING <mp>.
    IF <mp>-master <> l_master.
      l_master = <mp>-master.
      CHECK <mp>-error IS INITIAL.
      CHECK <mp>-include(1) <> '<' AND
            <mp>-include(1) <> '>'.
      CASE <mp>-object.
        WHEN 'REPS'.
*          data: l_with_dialog type c.
*          if g_no_dialog is initial.
*            l_with_dialog = 'X'.
*          endif.
          CALL FUNCTION 'REPS_OBJECT_GENERATE'
            EXPORTING
              i_program        = l_master
*              i_with_dialog    = l_with_dialog
            IMPORTING
              o_gen_message    = message
              o_gen_subrc      = l_subrc
              o_navigate       = l_navigate
*              o_cancel         = g_cancel_flag
            EXCEPTIONS
              syntax_error     = 1
              generation_error = 2
              OTHERS           = 3.
          IF l_subrc <> 0 OR sy-subrc <> 0.
*            PERFORM dequeue_objects.
*            IF l_subrc >= 8.
** Hier hat ein Roll-Back stattgefunden und hoffentlich nur hier!!!***JR
*              CALL FUNCTION 'RS_WORKING_AREA_INIT'
*                EXPORTING
*                  force_initialisation = 'X'.
*            ENDIF.
*            IF NOT l_navigate IS INITIAL.
*              PERFORM navigate_to_object.
*            ENDIF.
*            IF l_subrc >= 8.
*              MESSAGE e826(eu) RAISING execution_error.
*            ENDIF.
          ENDIF.
        WHEN 'CREP' OR 'CPUB' OR 'CPRI' OR 'CPRO' OR 'METH'
             OR 'CINC' OR 'CLAS'.
          l_clskey = l_master.
          CALL FUNCTION 'SEO_CLASS_GENERATE_LOAD'
            EXPORTING
              clskey             = l_clskey
            EXCEPTIONS
              syntax_error       = 1
              generation_failure = 2
              OTHERS             = 3.
          IF sy-subrc <> 0.
*            IF NOT sy-msgid IS INITIAL.
*              g_cancel_flag = '3'.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
          ENDIF.
        WHEN 'INTF'.
          l_clskey = l_master.
          CALL FUNCTION 'SEO_INTERFACE_GENERATE_LOAD'
            EXPORTING
              intkey             = l_clskey
            EXCEPTIONS
              syntax_error       = 1
              generation_failure = 2
              OTHERS             = 3.
          IF sy-subrc <> 0.
*            IF NOT sy-msgid IS INITIAL.
*              g_cancel_flag = '3'.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
          ENDIF.
        WHEN 'WAPD' OR 'WAPA'.
** O2-HTML-Seiten
          l_appl = <it>-obj_name.
          CALL METHOD (l_wapd_class)=>generate_application
            EXPORTING
              p_application = l_appl
            EXCEPTIONS
              OTHERS        = 1.
          IF sy-subrc <> 0.
*            IF NOT sy-msgid IS INITIAL.
*              g_cancel_flag = '3'.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
          ENDIF.
        WHEN 'WDYN' OR 'WDYD' OR 'WDYV' OR 'WDYC'.
          l_obj_name = l_master.
          CALL FUNCTION 'WDY_WB_GENERATE'
            EXPORTING
              p_object_name        = l_obj_name
              p_suppress_index_upd = ' '
            EXCEPTIONS
              not_existing         = 1
              error_occured        = 2
              syntax_error         = 3
              OTHERS               = 4.
          IF sy-subrc <> 0.
*            PERFORM dequeue_objects.
*            IF NOT sy-msgid IS INITIAL.
*              g_cancel_flag = '3'.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
          ENDIF.

** Enhancement Objects:
        WHEN 'ENHO' OR 'ENHC' OR 'ENHS' OR'ENSC'.
          l_obj_name = l_master.
          CALL FUNCTION 'ENH_WB_GENERATE'
            EXPORTING
              p_object_name        = l_obj_name
              p_obj_type           = <mp>-object
              p_suppress_index_upd = ' '
            EXCEPTIONS
              not_existing         = 1
              error_occured        = 2
              syntax_error         = 3
              OTHERS               = 4.
          IF sy-subrc <> 0.
*            PERFORM dequeue_objects.
*            IF NOT sy-msgid IS INITIAL.
*              g_cancel_flag = '3'.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
          ENDIF.


** Switch Framework:
        WHEN 'SFBF' OR 'SFSW' OR 'SFBS' OR 'SF01'.
          l_obj_name = l_master.
          CALL FUNCTION 'SFW_WB_GENERATE'
            EXPORTING
              p_object_name        = l_obj_name
              p_obj_type           = <mp>-object
              p_suppress_index_upd = ' '
            EXCEPTIONS
              not_existing         = 1
              error_occured        = 2
              syntax_error         = 3
              OTHERS               = 4.
          IF sy-subrc <> 0.
*            PERFORM dequeue_objects.
*            IF NOT sy-msgid IS INITIAL.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
          ENDIF.



        WHEN 'SFPI'.
** Formularinterface
          l_obj_name = <it>-obj_name.
          TRY.
              CALL METHOD (l_sfpf_class)=>interface_generate
                EXPORTING
                  i_name = l_obj_name.
            CATCH cx_fp_api_usage
                  cx_fp_api_repository
                  cx_fp_api_internal INTO l_exception.
              CLEAR l_message_text.
              l_message_text = l_exception->get_text( ).
*              IF NOT l_message_text IS INITIAL.
*                g_cancel_flag = '3'.
*                MESSAGE l_message_text TYPE 'E'.
*              ENDIF.
          ENDTRY.
        WHEN 'SFPF'.
** Formulare
          l_obj_name = <it>-obj_name.
          TRY.
              CALL METHOD (l_sfpf_class)=>form_generate
                EXPORTING
                  i_name = l_obj_name.
            CATCH cx_fp_api_usage
                  cx_fp_api_repository
                  cx_fp_api_internal INTO l_exception.
              CLEAR l_message_text.
              l_message_text = l_exception->get_text( ).
*              IF NOT l_message_text IS INITIAL.
*                g_cancel_flag = '3'.
*                MESSAGE l_message_text TYPE 'E'.
*              ENDIF.
          ENDTRY.
      ENDCASE.
    ENDIF.
  ENDLOOP.
  LOOP AT items ASSIGNING <it>.
    CASE <it>-object.
      WHEN 'DYNP'.
        l_dynp_id = <it>-obj_name.
        CALL FUNCTION 'RS_SCRP_SERVICE'
             EXPORTING
                  dynnr           = l_dynp_id-dnum
                  function        = swbm_c_op_activate
                  progname        = l_dynp_id-prog
                  suppress_checks = ' '
*               p_wb_checklist  = l_checklist
             IMPORTING
                  error_subrc     = l_subrc
             EXCEPTIONS
                  illegal_value   = 1
                  not_exists      = 2
                  cancelled       = 3
                  OTHERS          = 4.
        IF sy-subrc <> 0 OR l_subrc <> 0 .
*          g_cancel_flag = 'X'.
*          IF NOT sy-msgid IS INITIAL.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                    RAISING execution_error.
*          ENDIF.
        ENDIF.
      WHEN 'CUAD'.
        l_obj_name = <it>-obj_name.
        CALL FUNCTION 'CUAD_OBJECT_GENERATE'
          EXPORTING
            object_name       = l_obj_name
          EXCEPTIONS
            generation_failed = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                  RAISING execution_error.
        ENDIF.
      WHEN 'WAPP'.
        l_page = <it>-obj_name.
        CALL FUNCTION 'O2_GENERATE_PAGE'
          EXPORTING
            p_composite_name = l_page
          EXCEPTIONS
            not_found        = 1
            error_occured    = 2
            OTHERS           = 3.
        IF sy-subrc <> 0.
*          g_cancel_flag = '3'.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      WHEN 'SXCI'.
        l_imp_name =  <it>-obj_name.
        CALL FUNCTION 'SXO_IMPL_ACTIVE'
          EXPORTING
            imp_name                  = l_imp_name
          EXCEPTIONS
            badi_not_existing         = 1
            imp_not_existing          = 2
            already_active            = 3
            data_inconsistency        = 4
            activation_not_admissable = 5
            action_canceled           = 6
            access_failure            = 7
            OTHERS                    = 8.
        IF sy-subrc <> 0.
*          g_cancel_flag = '3'.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  endmethod.


  method ADD_FROM_TADIR.
  DATA t_keys      TYPE tt_compkeys.

  t_keys = zaplink_tools=>get_keys_from_tadir( srcsystem = srcsystem
                                                   type  = type
                                                    name = name
                                                packages = packages
                                                     ext = ext
                                                  author = author ).
  CHECK NOT t_keys IS INITIAL.
  add_keys( t_keys ).
  endmethod.


  method ADD_KEYS.
  DATA t_obj_list TYPE tt_objlist.
  DATA local_list TYPE tt_compkeys.
  DATA flag       TYPE abap_bool.
  FIELD-SYMBOLS <wa> LIKE LINE OF local_list.

* Issue 137 : Remove unvalid (that is not supposed to be activated) object types
  local_list = obj_list.
  SORT local_list.
  LOOP AT local_list ASSIGNING <wa>.
    AT NEW type.
      CALL FUNCTION 'RS_OBJ_SUPPORTS_INACTIVE'
        EXPORTING
          obj_type          = space
          sub_type          = <wa>-type
        IMPORTING
          supports_inactive = flag.
      IF flag IS INITIAL.   " If Not activable => Remove
        DELETE local_list WHERE type = <wa>-type.
      ENDIF.
    ENDAT.
  ENDLOOP.

  APPEND LINES OF local_list TO component_list.
  SORT component_list BY type name.   DELETE ADJACENT DUPLICATES FROM component_list COMPARING type name.
  t_obj_list = compkeys_2_objlist( obj_list ).
  APPEND LINES OF t_obj_list TO it_obj_list.
  SORT it_obj_list BY object obj_name.    DELETE ADJACENT DUPLICATES FROM it_obj_list COMPARING object obj_name.
  object_count = lines( it_obj_list ).
  endmethod.


  method ADD_LIST.
  DATA t_comps      TYPE tt_compkeys.
  CHECK o_list IS BOUND.
  t_comps = o_list->get_keys( ).
  add_keys( t_comps ).
  endmethod.


  method ADD_OBJ_TO_WA.
  FIELD-SYMBOLS <wa>   LIKE LINE OF it_obj_list.
  DATA name TYPE td_compname.
  LOOP AT it_obj_list ASSIGNING <wa>.
    name = <wa>-obj_name.
* Issue 137 => Use commun way to add into Working Area
*    IF zaplink_tools=>is_working_areable( <wa>-object ) = abap_true.
*      CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
*        EXPORTING
*          object                  = <wa>-object
*          obj_name                = <wa>-obj_name
**       DELETED_FLAG            = ' '
**     IMPORTING
**       OBJECT_INSERTED         =
*        EXCEPTIONS
*          wrong_object_name       = 1
*          OTHERS                  = 2
*                .
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*    ENDIF.
    zaplink_tools=>add_comp_to_working_area( type = <wa>-object
                                             name = name ).
  ENDLOOP.
  endmethod.


  method ASK_FOR_ACTIVATION.
  DATA d_count    TYPE i.
  DATA d_answer   TYPE c.
  DATA d_msg      TYPE string.
  DATA o_cx       TYPE REF TO zaplink_cx.

  CLEAR activation_requested.
  d_count = LINES( it_obj_list ).   d_msg = d_count.  CONDENSE d_msg.
  CONCATENATE 'Would you like to activate the objects ('(t01) d_msg ') that have been installed ?'(t02) INTO d_msg. "#EC NOTEXT
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Activate imported objects'(ptb)
      text_question         = d_msg
      text_button_1         = 'Yes'(yes)
      icon_button_1         = 'ICON_ACTIVATE'               "#EC NOTEXT
      text_button_2         = 'No'(no_)
      icon_button_2         = 'ICON_SKIP'                   "#EC NOTEXT
      default_button        = '1'                           "#EC NOTEXT
      display_cancel_button = abap_false
      iv_quickinfo_button_1 = 'All object will be activate as far as possible'(qi1) "#EC NOTEXT
      iv_quickinfo_button_2 = 'No object activation at all'(qi2) "#EC NOTEXT
    IMPORTING
      answer                = d_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    o_cx = zaplink_cx=>create_from_mf_cx( funcname = 'POPUP_TO_CONFIRM'
                                             subrc = sy-subrc ) .
    RAISE EXCEPTION o_cx.
  ENDIF.
  IF d_answer CO 'YJ1'.
    activation_requested = abap_true.
    result = activate( ).
  ELSE.
    result = objlist_2_compkeys( it_obj_list ).
  ENDIF.
  endmethod.


  method CHECK_INACTIVE_OBJECTS.
  DATA my_obj LIKE obj_list.
  DATA inact TYPE tt_items.
  FIELD-SYMBOLS <obj> LIKE LINE OF obj_list.

  LOOP AT obj_list ASSIGNING <obj>.
    REFRESH: my_obj, inact.   APPEND <obj> TO my_obj.
* Get all inactive Objects of List
    CALL FUNCTION 'RS_INACTIVE_OBJECTS_WARNING'
      EXPORTING
        suppress_protocol               = 'X'
*     WITH_PROGRAM_INCLUDES           = ' '
*     SUPPRESS_DICTIONARY_CHECK       = ' '
      TABLES
        p_e071                          = my_obj
*     P_XMSG                          =
        p_dwinactiv                     = inact.
*     USERNAME                        =
    IF inact IS INITIAL.    DELETE obj_list.    ENDIF.
  ENDLOOP.
  endmethod.


  method COMPKEYS_2_GENSET.
  DATA          s_obj LIKE LINE OF result.
  FIELD-SYMBOLS <k>   LIKE LINE OF keys.

  LOOP AT keys ASSIGNING <k>.
    CLEAR s_obj.
    s_obj-object = <k>-type.
    s_obj-objname = <k>-name.
    s_obj-pgmid = zaplink_connectors=>get_typekind( <k>-type ).
*    s_obj-component
    APPEND s_obj TO result.
  ENDLOOP.
  SORT result BY object objname.
* determine the program names on the basis of the TADIR keys *
* Form is not suppose to get only :
*    CASE lv_tadirobject.
**----------------------------programs--------------------------------
*      WHEN 'PROG'.
**----------------------------function groups ------------------------
*      WHEN 'FUGR'.
**-----------------function groups with customer include ----------------
** if FUGX exists then also FUGS exists
** Since the prognames of FUGX and FUGS are identical, FUGX needn't be
** considered here.
*      WHEN 'FUGS'.
**---------------------------logical database, 2 programs---------------
*      WHEN 'LDBA'.
**-----------------------classes and interfaces--------------------------
*      WHEN 'CLAS' OR 'INTF'.
**------------------------------type-pool--------------------------------
*      WHEN 'TYPE'.
*    ENDCASE.
* It generate wrong program name.
  PERFORM tadir_2_trdir IN PROGRAM rsparagenlod
          CHANGING result.
  endmethod.


  method COMPKEYS_2_OBJLIST.
  DATA          s_obj LIKE LINE OF result.
  FIELD-SYMBOLS <k>   LIKE LINE OF keys.
  LOOP AT keys ASSIGNING <k>.   CLEAR s_obj.  s_obj-object = <k>-type.    s_obj-obj_name = <k>-name.  APPEND s_obj TO result.   ENDLOOP.
  endmethod.


  method CONV_LIST.
  DATA _item LIKE LINE OF item_list.
  FIELD-SYMBOLS <obj>   LIKE LINE OF obj_list.
  LOOP AT obj_list ASSIGNING <obj>.   MOVE-CORRESPONDING <obj> TO _item.    APPEND _item TO item_list.    ENDLOOP.
  endmethod.


  method GENERATE.
  CONSTANTS gc_gen_types(8) TYPE c VALUE '1CFJKMST'.
  DATA t_genset   TYPE tt_genset.
  DATA t_trdir    TYPE HASHED TABLE OF trdir WITH UNIQUE KEY name.
  FIELD-SYMBOLS:
    <dir> LIKE LINE OF t_trdir,
    <obj> LIKE LINE OF t_genset.

  t_genset = compkeys_2_genset( component_list ).

* Clean list
  SORT t_genset BY name.
  DELETE ADJACENT DUPLICATES FROM t_genset COMPARING name.
* Remove component type that can't be generated
* FROM RSPARAGENLOD(3496) FORM get_component_objects
  DELETE t_genset WHERE name IS INITIAL
                    OR NOT ( object = 'PROG' OR
                             object = 'FUGR' OR
                             object = 'FUGS' OR
                             object = 'CLAS' OR
                             object = 'INTF' OR
                             object = 'TYPE' OR
                             object = 'LDBA' )
                   OR NOT pgmid = 'R3TR'.

* Remove programs that can't be generated : Include, ....
*      PERFORM trdir_check IN PROGRAM rsparagenlod
  SELECT * INTO TABLE t_trdir
    FROM trdir
    FOR ALL ENTRIES IN t_genset
    WHERE name = t_genset-name
    %_HINTS ORACLE '&max_blocking_factor 250&'.

  LOOP AT t_genset ASSIGNING <obj>.
    READ TABLE t_trdir ASSIGNING <dir>
         WITH TABLE KEY name = <obj>-name.
    CHECK sy-subrc = 0.
    If not <dir>-subc CA gc_gen_types.    Clear <obj>-name.     endif.
  ENDLOOP.
  DELETE t_genset WHERE name IS INITIAL.

  CALL FUNCTION 'SUBST_GENER8_ABAP_LOADS'
    EXPORTING
      countmax   = 100    " commit every 100 obejcts
    TABLES
      it_gensetc = t_genset.

* Generation Status (X = generated, E/S = error, I = initial)
  DELETE t_genset WHERE NOT genstatus = 'E' AND NOT genstatus = 'S'.

  result = genset_2_compkeys( t_genset ).
  endmethod.


  method GENSET_2_COMPKEYS.
  DATA          s_obj LIKE LINE OF result.
  FIELD-SYMBOLS <k>   LIKE LINE OF keys.
  LOOP AT keys ASSIGNING <k>.   CLEAR s_obj.  s_obj-type = <k>-object.    s_obj-name = <k>-objname.    APPEND s_obj TO result.   ENDLOOP.
  endmethod.


  method GET_INACTIVE_OBJECTS.
* Get all inactive Objects of List
  CALL FUNCTION 'RS_INACTIVE_OBJECTS_WARNING'
    EXPORTING
      SUPPRESS_PROTOCOL               = 'X'
*     WITH_PROGRAM_INCLUDES           = ' '
*     SUPPRESS_DICTIONARY_CHECK       = ' '
    TABLES
      P_E071                          = OBJ_LIST
*     P_XMSG                          =
      P_DWINACTIV                     = INACT.
*     USERNAME                        =
  endmethod.


  method OBJLIST_2_COMPKEYS.
  DATA          s_obj LIKE LINE OF result.
  FIELD-SYMBOLS <k>   LIKE LINE OF keys.
  LOOP AT keys ASSIGNING <k>.   CLEAR s_obj.  s_obj-type = <k>-object.    s_obj-name = <k>-obj_name.    APPEND s_obj TO result.   ENDLOOP.
  endmethod.


  method RESET.
  CLEAR: it_items, it_obj_list, component_list, activation_requested, object_count, force_activation.
  endmethod.


  method SPLIT_LIST.
  FIELD-SYMBOLS <obj>   LIKE LINE OF obj_list.
  LOOP AT obj_list ASSIGNING <obj>.

    IF <obj>-object = 'DTEL' OR
       <obj>-object = 'VIEW' OR
       <obj>-object = 'DOMA' OR
       <obj>-object = 'TTYP' OR
       <obj>-object = 'INDX' OR
       <obj>-object = 'XINX' OR
       <obj>-object = 'SHLP' OR
       <obj>-object = 'ENQU' OR
       <obj>-object = 'MCOB' OR
       <obj>-object = 'TABL' OR
       <obj>-object = 'SQLT' OR
       <obj>-object = 'STRU'.
      APPEND <obj> TO ddic.
    ELSE.
      APPEND <obj> TO prog_list.
    ENDIF.
  ENDLOOP.
  endmethod.
ENDCLASS.
