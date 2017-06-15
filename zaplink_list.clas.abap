class ZAPLINK_LIST definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools SLIS .

  interfaces ZAPLINK_DATATYPES .

  aliases SEL_ACTIONS
    for ZAPLINK_DATATYPES~ACTIONS .
  aliases TD_ORDERKIND
    for ZAPLINK_DATATYPES~TD_ORDERKIND .
  aliases TD_SEL_ACTION
    for ZAPLINK_DATATYPES~TD_ACTION .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .

  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_COMPONENT type ref to ZAPLINK_COMPONENT .
  types TO_ME type ref to ZAPLINK_LIST .

  data ORDER_KIND type TD_ORDERKIND .
  data DISPLAY_PROGRESS type ABAP_BOOL .
  constants:
    begin of ORDER_KINDS,
      no_order  type td_orderkind value space,
      install   type td_orderkind value 'B',  " Bottom up
      uninstall type td_orderkind value 'T',  " Top down
    end of ORDER_KINDS .
  data IS_CONFIRMED type ABAP_BOOL .
  class-data CALLBACK_PROGRAM type SY-REPID value 'ZAPLINK_LIST' ##NO_TEXT.

  methods ADD
    importing
      !COMP type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods ADD_KEYS
    importing
      !DATA type TT_COMPKEYS
    raising
      ZAPLINK_CX .
  methods ADD_LIST
    importing
      !DATA type TO_ME
    raising
      ZAPLINK_CX_LIST .
  methods CHANGE_SELECTION
    raising
      ZAPLINK_CX_LIST .
  class-methods CLASS_CONSTRUCTOR .
  methods CLEAR .
  methods CLONE
    returning
      value(RESULT) type TO_ME
    raising
      ZAPLINK_CX_LIST .
  methods DISPLAY
    raising
      ZAPLINK_CX_LIST .
  methods GET_COUNT
    importing
      !IS_SELECTED type ABAP_BOOL optional
    returning
      value(RESULT) type I .
  methods GET_NEXT
    returning
      value(RESULT) type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods GET_KEYS
    returning
      value(RESULT) type TT_COMPKEYS .
  methods HAS_ANY
    importing
      !LIST type TO_ME
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX_LIST .
  methods INIT_ITER
    raising
      ZAPLINK_CX_LIST .
  methods INIT_SEL_ITER .
  methods IS_EMPTY
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX_LIST .
  methods IS_INCLUDED
    importing
      !LIST type TO_ME
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX_LIST .
  methods IS_SELECTED
    importing
      !COMP type TO_COMPONENT
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX_LIST .
  methods KEEP_COMMUN_LIST
    importing
      !DATA type TO_ME .
  methods REMOVE
    importing
      !COMP type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods REMOVE_LIST
    importing
      !DATA type TO_ME
    raising
      ZAPLINK_CX_LIST .
  methods SEARCH
    importing
      !COMP type TO_COMPONENT
    returning
      value(RESULT) type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods SELECT
    importing
      !COMP type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods SELECT_ALL
    raising
      ZAPLINK_CX_LIST .
  methods SELECT_DEFAULT
    importing
      !SEL_ACTION type TD_SEL_ACTION
    raising
      ZAPLINK_CX_LIST .
  methods UNSELECT
    importing
      !COMP type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods UNSELECT_ALL
    raising
      ZAPLINK_CX_LIST .
  methods REVERSE_SELECTION
    raising
      ZAPLINK_CX_LIST .
  methods SELECTION_AS_LIST
    returning
      value(RESULT) type TO_ME
    raising
      ZAPLINK_CX_LIST .
  methods SELECT_BY_ACTION
    raising
      ZAPLINK_CX_LIST .
  methods REFRESH
    returning
      value(RESULT) type TO_ME
    raising
      ZAPLINK_CX_LIST .
protected section.

  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_LIGHT
    for ZAPLINK_DATATYPES~TD_LIGHT .
  aliases TD_TYPE_AS_TEXT
    for ZAPLINK_DATATYPES~TD_TYPE_AS_TEXT .

  types:
    BEGIN OF ts_cursor,
    key      TYPE ts_compkey,
    o_comp   TYPE to_component,
    order    TYPE i,
  END OF ts_cursor .
  types:
    tt_cursors TYPE STANDARD TABLE OF ts_cursor WITH DEFAULT KEY .
  types:
    BEGIN OF ts_complist,
    key      TYPE ts_compkey,
    o_comp   TYPE to_component,
    selected TYPE abap_bool,
  END OF ts_complist .
  types:
    tt_complist TYPE SORTED TABLE OF ts_complist WITH UNIQUE KEY KEY .
  types:
    BEGIN OF ts_allcomplist,
    key      TYPE ts_compkey,
    o_comp   TYPE to_component,
    as_sub   TYPE abap_bool,
  END OF ts_allcomplist .
  types:
    tt_allcomplist TYPE SORTED TABLE OF ts_allcomplist WITH NON-UNIQUE KEY KEY .
  types TD_ALVID type GUID_16 .
  types:
    BEGIN OF ts_alv_conf,
        fields TYPE slis_t_fieldcat_alv,
        layout TYPE slis_layout_alv,
      END OF ts_alv_conf .
  types:
    BEGIN OF ts_alv_confs,
      simple TYPE ts_alv_conf,
      hierar TYPE ts_alv_conf,
      END OF ts_alv_confs .
  types:
    BEGIN OF ts_alv,
      action     TYPE td_light, "type pools ICON
      type       TYPE td_comptype,
      typ_txt    TYPE td_type_as_text,
      name       TYPE td_compname,
      connector  TYPE td_light, "type pools ICON
      status     TYPE td_light, "type pools ICON
      msg        TYPE string,
      devclass   TYPE tadir-devclass,
      srcsystem  TYPE tadir-srcsystem,
      checksum   TYPE td_checksum,
      code_signature  TYPE td_checksum,
* Not displayed fields
      select     TYPE flag,
      id         TYPE td_alvid,
      expand     TYPE flag,
  END OF ts_alv .
  types:
    tt_alv TYPE STANDARD TABLE OF ts_alv WITH DEFAULT KEY .
  types:
    BEGIN OF td_alv,
      headers TYPE tt_alv,
      items   TYPE tt_alv,
    END OF td_alv .

  constants:
    BEGIN OF lights,
      no            TYPE td_light VALUE icon_light_out,
      red           TYPE td_light VALUE icon_red_light,
      yellow        TYPE td_light VALUE icon_yellow_light,
      green         TYPE td_light VALUE icon_green_light,
      export        TYPE td_light VALUE icon_arrow_left,
      import        TYPE td_light VALUE icon_arrow_right,
      uninst        TYPE td_light VALUE icon_delete,
      active        TYPE td_light VALUE icon_activate,
      inactive      TYPE td_light VALUE icon_deactivate,
      rep_error     TYPE td_light VALUE icon_message_error_small,   " ICON_STATUS_CRITICAL ICON_ALERT ICON_MESSAGE_CRITICAL_SMALL
      delete_file   type td_light value ICON_BOOKING_STOP,    " ICON_BEN_TERMINATION ICON_TERMINATED_ORG_UNIT ICON_TERMINATED_TASK ICON_MESSAGE_ORPHANED " ICON_CLOSED_FOLDER_ORPHANED doesn't exist on all SAP Systems :-(
    END OF lights .
  class-data ALV_CONF type TS_ALV_CONFS .
  data _ALLCOMP type TT_ALLCOMPLIST .
  data _INDEX type TT_COMPLIST .
  constants PF_STATUS type SLIS_FORMNAME value 'PF_STATUS_SET ' ##NO_TEXT.
  constants PF_STATUS_NO_ACTIONS type SLIS_FORMNAME value 'PF_STATUS_NO_ACTION ' ##NO_TEXT.
  constants USER_COMMAND type SLIS_FORMNAME value 'USER_COMMAND_USER' ##NO_TEXT.

  methods COMP_TO_ALV
    importing
      !O_COMP type TO_COMPONENT
      !SELECTED type ABAP_BOOL
    returning
      value(RESULT) type TS_ALV
    raising
      ZAPLINK_CX_LIST .
  methods CONV_TO_ALV
    importing
      !W_ITEM type ABAP_BOOL optional
    returning
      value(RESULT) type TD_ALV
    raising
      ZAPLINK_CX_LIST .
  methods SEARCH_INDEX
    importing
      !COMP type TO_COMPONENT
    returning
      value(RESULT) type TS_COMPLIST .
  methods SELECT_EXPORT
    raising
      ZAPLINK_CX_LIST .
  methods SELECT_IMPORT
    raising
      ZAPLINK_CX_LIST .
  methods UPDATE_FROM_ALV
    importing
      !DATA type TD_ALV
    raising
      ZAPLINK_CX_LIST .
  methods _ADD_SUBCOMP
    importing
      !DATA type TO_COMPONENT
    raising
      ZAPLINK_CX_LIST .
  methods _SELECT
    importing
      !COMP type TO_COMPONENT
      !VALUE type ABAP_BOOL
    raising
      ZAPLINK_CX_LIST .
  methods SELECT_UNINSTALL
    raising
      ZAPLINK_CX_LIST .
private section.

  types TD_ITERATOR_TYPE type CHAR1 .
  types TO_EXCEPTION type ref to ZAPLINK_CX_LIST .

  constants:
    BEGIN OF iterator_types,
      all       TYPE td_iterator_type VALUE 'A',
      selected  TYPE td_iterator_type VALUE 'S',
    END OF iterator_types .
  data O_EXCEPTION type TO_EXCEPTION .
  data _ITERATOR_COUNT type SY-TABIX .
  data _ITERATOR_TABIX type SY-TABIX .
  data _ITERATOR_TYPE type TD_ITERATOR_TYPE .
  constants:
    BEGIN OF alv_h_tables,
      header    TYPE slis_tabname VALUE 'HDR',
      item      TYPE slis_tabname VALUE 'ITM',
    END OF alv_h_tables .
  data T_CURSORS type TT_CURSORS .
  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_LIST IMPLEMENTATION.


  method ADD.
  DATA _idx  LIKE LINE OF _index.
  DATA _comp LIKE LINE OF _allcomp.

  CHECK comp IS BOUND.
  _idx = search_index( comp ).
  IF _idx-o_comp IS BOUND.
* Component '&name&' (&type&) already in the collection
    RAISE EXCEPTION TYPE zlcx_comp_already_exists
               EXPORTING type = _idx-key-type
                         name = _idx-key-name.
  ENDIF.

  _idx-o_comp = comp.
  INSERT _idx INTO TABLE _index.
  MOVE-CORRESPONDING _idx TO _comp. INSERT _comp INTO TABLE _allcomp.
  _add_subcomp( comp ).
  endmethod.


  method ADD_KEYS.
  DATA o_comp   TYPE to_component.
  DATA o_cx_comp TYPE REF TO zaplink_cx_component.
  DATA _list    TYPE tt_compkeys.
  FIELD-SYMBOLS:
    <k> LIKE LINE OF _list.

  TRY.
      _list = data.
      SORT _list.
      DELETE ADJACENT DUPLICATES FROM _list.
      DELETE _list WHERE table_line IS INITIAL.

      LOOP AT _list ASSIGNING <k>.
        TRY.
            o_comp = zaplink_component=>create_new( <k> ).
          CATCH zaplink_cx_component INTO o_cx_comp.
* Action failed due to exception '&CX_NAME&'
            CREATE OBJECT o_mycx
              EXPORTING
                textid   = zaplink_cx_list=>zaplink_cx_list
                previous = o_cx_comp.
            o_mycx->update( ).
            RAISE EXCEPTION o_mycx.
        ENDTRY.
        add( o_comp ).
      ENDLOOP.
    CATCH zaplink_cx_list INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ADD_LIST.
  DATA t_allcomp   TYPE	tt_allcomplist.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _index.

  LOOP AT data->_index ASSIGNING <i>.
    READ TABLE _index TRANSPORTING NO FIELDS
               WITH TABLE KEY key = <i>-key.
    IF sy-subrc = 0.
* Already exist here
      CONTINUE.
    ENDIF.
    t_allcomp = data->_allcomp.
    DELETE t_allcomp WHERE o_comp <> <i>-o_comp.
    INSERT <i> INTO TABLE _index.
    INSERT LINES OF t_allcomp INTO TABLE _allcomp.
  ENDLOOP.
  endmethod.


  method CHANGE_SELECTION.
  DATA alv TYPE td_alv.

  CHECK sy-batch IS INITIAL.

  alv = conv_to_alv( ).

  PERFORM set_list IN PROGRAM zaplink_list USING me.
*  PERFORM show_list IN PROGRAM zaplink_list using alv_conf-simple-layout alv_conf-simple-fields alv-headers.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = callback_program
      i_callback_pf_status_set = pf_status
      i_callback_user_command  = user_command
      i_grid_title             = 'Select objects'(lst)
      it_fieldcat              = alv_conf-simple-fields
      is_layout                = alv_conf-simple-layout
    TABLES
      t_outtab                 = alv-headers
    EXCEPTIONS
      OTHERS                   = 0.

  PERFORM clear_list IN PROGRAM zaplink_list.
  IF is_confirmed IS INITIAL.
    unselect_all( ).
  ELSE.
    update_from_alv( alv ).
  ENDIF.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA field_s   LIKE LINE OF alv_conf-simple-fields.
  DATA field_h   LIKE LINE OF alv_conf-hierar-fields.
  DATA i_pos     TYPE i VALUE 0. " position d'affichage des champs
  DATA t_desc    TYPE REF TO cl_abap_typedescr.
  DATA st_desc   TYPE REF TO cl_abap_structdescr.
  DATA o_elem    TYPE REF TO cl_abap_elemdescr.
  DATA list      TYPE tt_alv.
  DATA line      LIKE LINE OF list.
  DATA text_name TYPE string.
  DATA i_len     TYPE i.
  DATA d_hex     TYPE x.                                    " Issue 76
  DATA d_char(2) TYPE c.
  CONSTANTS fn_sel TYPE string VALUE 'SELECT'.
  FIELD-SYMBOLS:
    <t> TYPE ANY,
    <c> LIKE LINE OF st_desc->components.

* Macro definition
  DEFINE m_fieldcat_s.
    clear field_s.
    field_s-fieldname = &1.
    field_s-seltext_m = field_s-seltext_l = &2.
    append field_s to alv_conf-simple-fields.
  END-OF-DEFINITION.

  DEFINE m_fieldcat_h.
    clear field_h.
    field_h-row_pos   = 1.
    field_h-tabname   = &1.
    field_h-col_pos   = i_pos.
    field_h-fieldname = &2.
    field_h-seltext_m = field_h-seltext_l = &3.
    field_h-intlen  = field_h-outputlen = field_h-ddic_outputlen = &4.
    field_h-datatype = 'CHAR'.
    case field_h-fieldname.
      when 'NAME'.
        field_h-key = abap_true.
      when 'TYPE'.
        field_h-key = abap_true.
      when 'SELECT'.
        field_h-input = abap_true.
        field_h-checkbox = abap_true.
      when 'MSG'.
        field_h-row_pos   = 2.
        field_h-col_pos   = 1.
    endcase.
    append field_h to alv_conf-hierar-fields.
  END-OF-DEFINITION.

*==============================================
*==========     ALV : Simple    ===============
*==============================================

* Not working :-(
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name     = 'ZAPLINK_LIST==================CP'
*      i_internal_tabname = 'LIST'
*      i_inclname         = 'ZAPLINK_LIST==================CM005'
*      i_bypassing_buffer = 'X'
*    CHANGING
*      ct_fieldcat        = alv_conf-simple-fields.

  t_desc = cl_abap_structdescr=>describe_by_data( line ).

  CHECK t_desc->kind = cl_abap_structdescr=>kind_struct.
  st_desc ?= t_desc.

  LOOP AT st_desc->components ASSIGNING <c>.
* Issue 76 : Start
*    text_name = i_pos = sy-tabix.
*    CONDENSE text_name NO-GAPS.
    i_pos = sy-tabix.
    d_hex = i_pos.
    WRITE d_hex TO d_char.
    text_name = d_char+1.
* Issue 76 : end
    CONCATENATE 'TEXT-NF' text_name INTO text_name.
    ASSIGN (text_name) TO <t>.
    IF sy-subrc = 0.
      text_name = <t>.
    ELSE.
      text_name = <c>-name.
    ENDIF.

    CLEAR i_len.
    ASSIGN COMPONENT <c>-name OF STRUCTURE line TO <t>.
    IF sy-subrc = 0.
      t_desc = cl_abap_structdescr=>describe_by_data( <t> ).
      IF t_desc->kind = cl_abap_structdescr=>kind_elem.
        o_elem ?= t_desc.
        IF o_elem->output_length = 0.
          CASE <c>-name.
            WHEN 'NAME'.
              i_len = 30.
            WHEN 'MSG'.
              i_len = 80.
            WHEN OTHERS.
              i_len = 50.
          ENDCASE.
        ELSE.
          i_len = o_elem->output_length.
        ENDIF.
      ENDIF.
    ENDIF.
    IF i_len IS INITIAL. i_len = <c>-length. ENDIF.

    m_fieldcat_s <c>-name text_name.
    m_fieldcat_h alv_h_tables-header <c>-name text_name i_len.
    m_fieldcat_h alv_h_tables-item <c>-name text_name i_len.

  ENDLOOP.

  alv_conf-simple-layout-box_fieldname     = fn_sel.
  alv_conf-simple-layout-f2code            = 'MYPICK' .
  alv_conf-simple-layout-colwidth_optimize = 'X'.
*  alv_conf-layout-lights_fieldname  = 'STATUS'.

  DELETE alv_conf-simple-fields WHERE fieldname = alv_conf-simple-layout-box_fieldname
                                   OR fieldname = 'ID'
                                   OR fieldname = 'EXPAND'.

*  alv_conf-hierar-layout-zebra = 'X'.
  alv_conf-hierar-layout-expand_fieldname = 'EXPAND'.
  alv_conf-hierar-layout-box_fieldname     = fn_sel.
  alv_conf-hierar-layout-allow_switch_to_list = abap_true.
  DELETE alv_conf-hierar-fields WHERE fieldname = 'ID'
                                   OR ( fieldname = fn_sel AND tabname = alv_h_tables-item )
                                   OR fieldname = alv_conf-hierar-layout-expand_fieldname.
  endmethod.


  method CLEAR.
  CLEAR:
      display_progress,
      _allcomp,
      _index,
      o_exception,
      t_cursors,
      _iterator_count,
      _iterator_tabix,
      _iterator_type.
  endmethod.


  method CLONE.
  DATA o_comp TYPE to_component.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _index.

  TRY.
      CREATE OBJECT result.

      LOOP AT _index ASSIGNING <i>.
        o_comp = <i>-o_comp->clone( ).
        result->add( o_comp ).
        If <i>-selected = abap_true.    result->select( o_comp ).    endif.
      ENDLOOP.
    CATCH zaplink_cx.
      CLEAR result.
  ENDTRY.
  endmethod.


  method COMP_TO_ALV.
  DATA _key     TYPE ts_compkey.
  DATA d_action TYPE td_sel_action.
  DATA o_cx     TYPE REF TO cx_root.

  _key = o_comp->_get_key( ).
  result-type = _key-type.
  result-name = _key-name.

  d_action = o_comp->get_action( ).
  CASE d_action.
    WHEN sel_actions-delete_file.
      result-action = lights-delete_file.
    WHEN sel_actions-import.
      result-action = lights-import.
    WHEN sel_actions-export.
      result-action = lights-export.
    WHEN sel_actions-uninstall.
      result-action = lights-uninst.
    WHEN sel_actions-activated.
      result-action = lights-active.
    WHEN sel_actions-not_active.
      result-action = lights-inactive.
    WHEN sel_actions-unreplicable.
      result-action = lights-rep_error.
  ENDCASE.

  result-typ_txt = zaplink_connectors=>type2text( result-type ).

  IF o_comp->_connector-exists = zaplink_component=>connexists-not_exists.
    result-connector = lights-red.
    result-status = lights-no.
* There is no ZAPLink connector installed for object type '&1'
    MESSAGE e050 WITH result-type INTO result-msg.
  ELSE.
    result-connector = lights-green.
    result-checksum = o_comp->get_checksum( ).
    result-code_signature = o_comp->get_code_signature( ).
    result-devclass = o_comp->get_devclass( ).
    result-srcsystem = o_comp->get_src_sys( ).
    o_cx = o_comp->get_exception( ).

    result-status = lights-green.

    CASE d_action.
      WHEN sel_actions-import.
        IF o_comp->_exists = zaplink_connector=>exists. " AND selected = abap_true.
          result-status = lights-yellow.
          CLEAR result-select.
* Object '&2' (&1) already exists. Select it to overwrite
          MESSAGE e051 WITH result-type result-name INTO result-msg.
        ELSEIF o_cx IS BOUND.
          result-status = lights-yellow.
          result-msg = o_cx->get_text( ).
        ENDIF.
      WHEN sel_actions-export.
        IF o_comp->_exists <> zaplink_connector=>exists. " AND selected = abap_true.
          CLEAR result-select.
          result-status = lights-red.
* Object '&2' (&1) does not exists.
          MESSAGE e052 WITH result-type result-name INTO result-msg.
        ELSEIF o_cx IS BOUND.
          result-status = lights-yellow.
          result-msg = o_cx->get_text( ).
        ENDIF.
      WHEN sel_actions-uninstall.
        IF o_comp->_exists <> zaplink_connector=>exists. " AND selected = abap_true.
          CLEAR result-select.
          result-status = lights-red.
* Object '&2' (&1) does not exists.
          MESSAGE e052 WITH result-type result-name INTO result-msg.
        ELSEIF o_cx IS BOUND.
          result-status = lights-yellow.
          result-msg = o_cx->get_text( ).
        ENDIF.
      WHEN sel_actions-unreplicable.
        result-status = lights-red.
* Changes made on both (file&SAP) for Object '&2' (&1) => Manual ajustment
        MESSAGE e053 WITH result-type result-name INTO result-msg.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
  endmethod.


  method CONV_TO_ALV.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.
  DATA _line  LIKE LINE OF result-headers.
  DATA s_item LIKE LINE OF result-items.
  DATA o_list TYPE to_me.
  DATA o_comp TYPE to_component.

  TRY.

    LOOP AT _index ASSIGNING <i>.
      _line = comp_to_alv( o_comp = <i>-o_comp
                         selected = <i>-selected ).
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = _line-id.

      _line-select = <i>-selected.
      IF NOT w_item IS INITIAL.
        o_list = <i>-o_comp->get_subcomponents( ).
        IF o_list IS BOUND.
          _line-expand = abap_true.
        ENDIF.
      ENDIF.

      APPEND _line TO result-headers.

      IF NOT w_item IS INITIAL.
        CHECK o_list IS BOUND.
        o_list->display_progress = abap_false.
        o_list->init_iter( ).
        o_comp = o_list->get_next( ).
        WHILE o_comp IS BOUND.
          s_item = comp_to_alv( o_comp = o_comp
                              selected = <i>-selected ).
          o_comp = o_list->get_next( ).
          s_item-id = _line-id.
          s_item-select = _line-expand.
          APPEND s_item TO result-items.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

    mac_def_catch zaplink_cx_list.
  ENDTRY.
  endmethod.


  method DISPLAY.
  DATA alv        TYPE td_alv.
  DATA s_keyinfo  TYPE slis_keyinfo_alv.

  s_keyinfo-header01 = s_keyinfo-item01 = 'ID'.

  alv = conv_to_alv( abap_true ).

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK              = ' '
      i_callback_program             = callback_program
      i_callback_pf_status_set       = pf_status_no_actions
      i_callback_user_command        = user_command
      is_layout                      = alv_conf-hierar-layout
      it_fieldcat                    = alv_conf-hierar-fields
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*     IT_SORT                        =
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*     I_DEFAULT                      = 'X'
*     I_SAVE                         = ' '
*     IS_VARIANT                     =
*     IT_EVENTS                      =
*     IT_EVENT_EXIT                  =
      i_tabname_header               = alv_h_tables-header
      i_tabname_item                 = alv_h_tables-item
*     I_STRUCTURE_NAME_HEADER        =
*     I_STRUCTURE_NAME_ITEM          =
      is_keyinfo                     = s_keyinfo
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                =
*     IR_SALV_HIERSEQ_ADAPTER        =
*     IT_EXCEPT_QINFO                =
*     I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab_header                = alv-headers
      t_outtab_item                  = alv-items
    EXCEPTIONS
      program_error                  = 1
      OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  endmethod.


  method GET_COUNT.
  IF is_selected IS INITIAL.
    result = LINES( _index ).
  ELSE.
    LOOP AT _index TRANSPORTING NO FIELDS WHERE selected = abap_true. ADD 1 TO result. ENDLOOP.
  ENDIF.
  endmethod.


  method GET_KEYS.
  FIELD-SYMBOLS:
    <k> LIKE LINE OF _index.

  LOOP AT _index ASSIGNING <k>.
    APPEND <k>-key TO result.
  ENDLOOP.
  SORT result.    DELETE ADJACENT DUPLICATES FROM result.
  endmethod.


  method GET_NEXT.
  DATA component TYPE ts_compkey.
  DATA msg       TYPE string.
  DATA _pos      TYPE i.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF t_cursors.

*  CHECK _iterator_end = abap_false.
  CHECK _iterator_tabix < _iterator_count.

  CASE _iterator_type.
    WHEN iterator_types-selected.
    WHEN iterator_types-all.
    WHEN OTHERS.
* GET_NEXT has been called without initialize a iterator.
      RAISE EXCEPTION TYPE zaplink_cx_list
                 EXPORTING textid = zaplink_cx_list=>iterator_not_initialize.
  ENDCASE.

  ADD 1 TO _iterator_tabix.
  READ TABLE t_cursors INDEX _iterator_tabix ASSIGNING <i>.
  ASSERT sy-subrc = 0.
*  IF sy-subrc <> 0.
** should not happend
*    RAISE EXCEPTION TYPE zaplink_cx_list
*               EXPORTING textid = zaplink_cx_list=>system_error.
*  ENDIF.

  result = <i>-o_comp.

* Display progression
  IF display_progress = abap_true.
    component = result->_get_key( ).
    IF sy-batch = abap_true.
* Processing object : &2 (&1)
      MESSAGE i000 WITH component-type component-name.
    ELSE.
* Processing object : &2 (&1)
      MESSAGE i000 WITH component-type component-name INTO msg.
      IF _iterator_count > 0.
        _pos = 100 * _iterator_tabix / _iterator_count.
      ELSE.
        _pos = _iterator_tabix.
      ENDIF.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = _pos
          text       = msg.
    ENDIF.
  ENDIF.
  endmethod.


  method HAS_ANY.
  DATA t_allcomp TYPE tt_allcomplist.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF _allcomp.

  t_allcomp = list->_allcomp.

  DELETE ADJACENT DUPLICATES FROM t_allcomp COMPARING key.

  result = abap_false.
  LOOP AT t_allcomp ASSIGNING <c>.
    READ TABLE _allcomp TRANSPORTING NO FIELDS
         WITH TABLE KEY key = <c>-key.
    IF sy-subrc = 0.
      result = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
  endmethod.


  method INIT_ITER.
  DATA s_cursor LIKE LINE OF t_cursors.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _index.

  CLEAR: _iterator_tabix, _iterator_count, t_cursors.    " Issue 30
  _iterator_type = iterator_types-all.
  LOOP AT _index ASSIGNING <i>.
    MOVE-CORRESPONDING <i> TO s_cursor.
    IF order_kind <> order_kinds-no_order.
      s_cursor-order = zaplink_connectors=>get_typeorder( <i>-key-type ).
    ENDIF.
    APPEND s_cursor TO t_cursors.
  ENDLOOP.
  CASE order_kind.
    WHEN order_kinds-install.
      SORT t_cursors BY order key.
    WHEN order_kinds-uninstall.
      SORT t_cursors BY order DESCENDING key.
    WHEN OTHERS.
      SORT t_cursors BY key.
  ENDCASE.
  DESCRIBE TABLE t_cursors LINES _iterator_count.
  endmethod.


  method INIT_SEL_ITER.
  DATA s_cursor LIKE LINE OF t_cursors.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _index.

  CLEAR: _iterator_tabix, _iterator_count, t_cursors.    " Issue 30
  _iterator_type = iterator_types-selected.
  LOOP AT _index ASSIGNING <i>
       WHERE selected = abap_true.
    MOVE-CORRESPONDING <i> TO s_cursor.
    IF order_kind <> order_kinds-no_order.
      s_cursor-order = zaplink_connectors=>get_typeorder( <i>-key-type ).
    ENDIF.
    APPEND s_cursor TO t_cursors.
  ENDLOOP.
  CASE order_kind.
    WHEN order_kinds-install.
      SORT t_cursors BY order key.
    WHEN order_kinds-uninstall.
      SORT t_cursors BY order DESCENDING key.
    WHEN OTHERS.
      SORT t_cursors BY key.
  ENDCASE.
  DESCRIBE TABLE t_cursors LINES _iterator_count.
  endmethod.


  method IS_EMPTY.
  IF _index IS INITIAL.   result = abap_true.   ENDIF.
  endmethod.


  method IS_INCLUDED.
  DATA t_allcomp TYPE tt_allcomplist.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF _allcomp.

  t_allcomp = list->_allcomp.

  DELETE ADJACENT DUPLICATES FROM t_allcomp COMPARING key.

  result = abap_true.
  LOOP AT t_allcomp ASSIGNING <c>.
    READ TABLE _allcomp TRANSPORTING NO FIELDS
         WITH TABLE KEY key = <c>-key.
    IF sy-subrc <> 0.
      result = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.
  endmethod.


  method IS_SELECTED.
  DATA _idx LIKE LINE OF _index.

  _idx = search_index( comp ).
  IF _idx-selected IS NOT INITIAL.
    result = abap_true.
  ELSE.
    result = abap_false.
  ENDIF.
  endmethod.


  method KEEP_COMMUN_LIST.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _index.

  LOOP AT _index ASSIGNING <i>.
    READ TABLE data->_index TRANSPORTING NO FIELDS
               WITH TABLE KEY key = <i>-key.
    IF sy-subrc <> 0.
      DELETE _index.
    ENDIF.
  ENDLOOP.
  endmethod.


  method REFRESH.
  DATA t_list TYPE tt_complist.
  FIELD-SYMBOLS <i> LIKE LINE OF t_list.

  t_list = _index.
  clear( ).
  LOOP AT t_list ASSIGNING <i>.
    add( <i>-o_comp ).
    IF <i>-selected = abap_true.    select( <i>-o_comp ).    ENDIF.
  ENDLOOP.
  endmethod.


  method REMOVE.
  DATA _idx LIKE LINE OF _index.

  TRY.
      _idx = search_index( comp ).
    CATCH zaplink_cx_list INTO o_exception.
      RAISE EXCEPTION o_exception.
  ENDTRY.

  DELETE _allcomp WHERE o_comp = _idx-o_comp.
  DELETE TABLE _index FROM _idx.
  endmethod.


  method REMOVE_LIST.
  FIELD-SYMBOLS:
    <i> LIKE LINE OF _index.

  LOOP AT data->_index ASSIGNING <i>.
    TRY.
        remove( <i>-o_comp ).
      CATCH zaplink_cx_list INTO o_exception.
    ENDTRY.
  ENDLOOP.
  endmethod.


  method REVERSE_SELECTION.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.
  LOOP AT _index ASSIGNING <i>. IF <i>-selected is INITIAL.   <i>-selected = abap_true.   else.   clear <i>-selected.   endif.  ENDLOOP.
  endmethod.


  method SEARCH.
  DATA _idx LIKE LINE OF _index.

  _idx = search_index( comp ).
  IF NOT _idx-o_comp IS BOUND.
* not a real exception.
* Component '&NAME&' (&TYPE&) not found in the collection
*    RAISE EXCEPTION TYPE zaplink_cx_list
*               EXPORTING textid = zaplink_cx_list=>not_found
*                           type = _idx-key-type
*                           name = _idx-key-name.
    exit.
  ENDIF.

  result = _idx-o_comp.
  endmethod.


  method SEARCH_INDEX.
  DATA _key TYPE ts_compkey.

  CHECK comp IS BOUND.
  _key = comp->_get_key( ).

  READ TABLE _index INTO result
             WITH TABLE KEY key = _key.
  IF sy-subrc <> 0.
    result-key = _key.
  ENDIF.
  endmethod.


  method SELECT.

  TRY.
      _select( comp = comp
              value = abap_true ).
    CATCH zaplink_cx_list INTO o_exception.
      RAISE EXCEPTION o_exception.
  ENDTRY.
  endmethod.


  method SELECTION_AS_LIST.
  DATA o_comp TYPE to_component.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.

  TRY.
      CREATE OBJECT result.

      LOOP AT _index ASSIGNING <i> WHERE selected = abap_true.

        o_comp = <i>-o_comp->clone( ).
        result->add( o_comp ).
*        If <i>-selected = abap_true.    result->select( o_comp ).    endif.
      ENDLOOP.
    CATCH zaplink_cx.
      CLEAR result.
  ENDTRY.
  endmethod.


  method SELECT_ALL.
  DATA s_idx LIKE LINE OF _index.

  s_idx-selected = abap_true.   MODIFY _index FROM s_idx TRANSPORTING selected WHERE selected <> s_idx-selected.
  endmethod.


  method SELECT_BY_ACTION.
  DATA action TYPE td_sel_action.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.

  LOOP AT _index ASSIGNING <i>.
    CLEAR <i>-selected.
    action = <i>-o_comp->get_action( ).
    CASE action.
      WHEN sel_actions-export OR sel_actions-import OR sel_actions-uninstall OR sel_actions-delete_file.
        <i>-selected = abap_true.
    ENDCASE.
  ENDLOOP.
  endmethod.


  method SELECT_DEFAULT.
  TRY.
      CASE sel_action.
        WHEN sel_actions-export.
          select_export( ).
        WHEN sel_actions-import.
          select_import( ).
        WHEN sel_actions-uninstall.
          select_uninstall( ).
        WHEN OTHERS.
          RAISE EXCEPTION TYPE zaplink_cx_list
                     EXPORTING textid = zaplink_cx_list=>invalid_action
                           sel_action = sel_action.
      ENDCASE.
    CATCH zaplink_cx_list INTO o_exception.
      RAISE EXCEPTION o_exception.
  ENDTRY.
  endmethod.


  method SELECT_EXPORT.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.

  LOOP AT _index ASSIGNING <i>.
    CLEAR <i>-selected.
    IF <i>-o_comp->_connector-exists = zaplink_component=>connexists-exists.
      <i>-o_comp->set_action( sel_actions-export ).
      IF <i>-o_comp->_exists = zaplink_connector=>exists.    <i>-selected = abap_true.   ENDIF.
    ENDIF.
  ENDLOOP.
  endmethod.


  method SELECT_IMPORT.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.

  LOOP AT _index ASSIGNING <i>.
    CLEAR <i>-selected.
    IF <i>-o_comp->_connector-exists = zaplink_component=>connexists-exists.
      <i>-o_comp->set_action( sel_actions-import ).
      IF <i>-o_comp->_exists = zaplink_connector=>exists-not_exists.    <i>-selected = abap_true.   ENDIF.
    ENDIF.
  ENDLOOP.
  endmethod.


  method SELECT_UNINSTALL.
  FIELD-SYMBOLS <i> LIKE LINE OF _index.

  LOOP AT _index ASSIGNING <i>.
    CLEAR <i>-selected.
    IF <i>-o_comp->_connector-exists = zaplink_component=>connexists-exists.
      <i>-o_comp->set_action( sel_actions-uninstall ).
      IF <i>-o_comp->_exists = zaplink_connector=>exists.     <i>-selected = abap_true.     ENDIF.
    ENDIF.
  ENDLOOP.
  endmethod.


  method UNSELECT.

  TRY.
      _select( comp = comp
              value = abap_false ).
    CATCH zaplink_cx_list INTO o_exception.
      RAISE EXCEPTION o_exception.
  ENDTRY.
  endmethod.


  method UNSELECT_ALL.
  DATA s_idx LIKE LINE OF _index.

  s_idx-selected = abap_false.   MODIFY _index FROM s_idx TRANSPORTING selected WHERE selected <> s_idx-selected.
  endmethod.


  method UPDATE_FROM_ALV.
  FIELD-SYMBOLS:
    <a> LIKE LINE OF data-headers,
    <i> LIKE LINE OF _index.
  DATA _key TYPE ts_compkey.

  LOOP AT data-headers ASSIGNING <a>.
    _key-type = <a>-type.
    _key-name = <a>-name.
    READ TABLE _index ASSIGNING <i>
         WITH KEY key = _key.
    IF sy-subrc = 0.
      IF NOT <a>-select IS INITIAL.
        <i>-selected = abap_true.
      ELSE.
        <i>-selected = abap_false.
      ENDIF.
    ENDIF.
  ENDLOOP.
  endmethod.


  method _ADD_SUBCOMP.
  DATA o_list TYPE to_me.
  DATA o_sub  TYPE to_me.
  DATA _comp LIKE LINE OF _allcomp.

  CHECK data IS BOUND.
  TRY.
      o_list = data->get_subcomponents( ).
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  CHECK o_list IS BOUND.

  LOOP AT o_list->_allcomp INTO _comp.
    _comp-as_sub = abap_true.
    _comp-o_comp = data.
    INSERT _comp INTO TABLE _allcomp.
  ENDLOOP.

  LOOP AT o_list->_allcomp INTO _comp.    " Issue 102
    _add_subcomp( _comp-o_comp ).
  ENDLOOP.
  endmethod.


  method _SELECT.
  DATA _idx LIKE LINE OF _index.

  _idx = search_index( comp ).
  IF NOT _idx-o_comp IS BOUND.
* Component '&NAME&' (&TYPE&) not found in the collection
    RAISE EXCEPTION TYPE zaplink_cx_list
               EXPORTING textid = zaplink_cx_list=>not_found
                           type = _idx-key-type
                           name = _idx-key-name.
  ENDIF.

  _idx-selected = value.
  MODIFY TABLE _index FROM _idx TRANSPORTING selected.

  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_list
               EXPORTING textid = zaplink_cx_list=>system_error
                           type = _idx-key-type
                           name = _idx-key-name.
  ENDIF.
  endmethod.
ENDCLASS.
