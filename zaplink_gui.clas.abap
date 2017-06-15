class ZAPLINK_GUI definition
  public
  inheriting from ZAPLINK_TOOLS
  create public .

public section.
  type-pools ABAP .
  type-pools ICON .
  type-pools SLIS .

  interfaces ZAPLINK_KERNEL_TYPES .
  interfaces ZAPLINK_TYPES .

  aliases BALLOG_OBJECT
    for ZAPLINK_KERNEL_TYPES~BALLOG_OBJECT .
  aliases TO_COMPONENT
    for ZAPLINK_TYPES~TO_COMPONENT .
  aliases TO_CONTAINER
    for ZAPLINK_TYPES~TO_CONTAINER .
  aliases TO_FILE
    for ZAPLINK_TYPES~TO_FILE .
  aliases TO_GUI
    for ZAPLINK_TYPES~TO_GUI .
  aliases TO_LIST
    for ZAPLINK_TYPES~TO_LIST .
  aliases TO_MSG_COLL
    for ZAPLINK_TYPES~TO_MSG_COLL .
  aliases TO_OPTIONS
    for ZAPLINK_TYPES~TO_OPTIONS .
  aliases TO_OPT_DEVCLASS
    for ZAPLINK_TYPES~TO_OPT_DEVCLASS .
  aliases TO_RAW
    for ZAPLINK_TYPES~TO_RAW .

  types TO_EXCEPTION type ref to ZAPLINK_CX_GUI .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .

  class-data APPLICATION_LOG type TO_MSG_COLL .
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'GUI' ##NO_TEXT.
  class-data O_OPTIONS type TO_OPTIONS .

  class-methods CLASS_CONSTRUCTOR .
  class-methods EXPORT
    importing
      !FILE type TO_FILE
      !WITH_SUBCOMP type TD_WITH_SUBCOMP default ABAP_TRUE
    changing
      !OBJLIST type TO_LIST
    raising
      ZAPLINK_CX_GUI .
  class-methods IMPORT
    importing
      !FILE type TO_FILE
      !WITH_SUBCOMP type TD_WITH_SUBCOMP default ABAP_TRUE
      !ACTIVATE type ABAP_BOOL default ABAP_FALSE
      !ACTIVATE_ANYWAY type ABAP_BOOL default ABAP_FALSE
    exporting
      !NAME type TD_CONTNAME
      !LIST type TO_LIST
    raising
      ZAPLINK_CX_GUI .
  class-methods CREATE
    importing
      !NAME type TD_CONTNAME
      !FILE type TO_FILE optional
    raising
      ZAPLINK_CX_GUI .
  class-methods ADD_OBJECT
    importing
      !OBJTYPE type TD_COMPTYPE
      !NAME type TD_COMPNAME
      !FILE type TO_FILE
      !WITH_SUBCOMP type TD_WITH_SUBCOMP default ABAP_TRUE
    raising
      ZAPLINK_CX_GUI .
  class-methods UPDATE_SUBOBJ
    importing
      !O_LIST type TO_LIST
      !F_SUB type TD_WITH_SUBCOMP
    raising
      ZAPLINK_CX .
  class-methods GET_OBJLIST
    importing
      !FILE type TO_FILE
    returning
      value(LIST) type TO_LIST
    raising
      ZAPLINK_CX_GUI .
  class-methods UNINSTALL
    importing
      !FILE type TO_FILE
      !WITH_SUBCOMP type TD_WITH_SUBCOMP default ABAP_TRUE
    exporting
      !NAME type TD_CONTNAME
    changing
      !LIST type TO_LIST
    raising
      ZAPLINK_CX_GUI .
  class-methods CHANGE_SELECTION
    importing
      !ACTION type TD_ACTION
      !LIST type TO_LIST
    raising
      ZAPLINK_CX_GUI .
  class-methods CONTAINERFILE_SELECT
    changing
      !FILENAME type TD_FILENAME .
  class-methods GET_VALUES
    importing
      !TYPE type TD_COMPTYPE
    changing
      !VALUE type TD_COMPNAME
    raising
      ZAPLINK_CX .
  class-methods REFRESH
    importing
      !FILE type TO_FILE
      !WITH_SUBCOMP type TD_WITH_SUBCOMP default ABAP_TRUE
    returning
      value(OBJLIST) type TO_LIST
    raising
      ZAPLINK_CX_GUI .
protected section.

  types:
    BEGIN OF t_alv,
      fields TYPE slis_t_fieldcat_alv,
      layout TYPE slis_layout_alv,
    END OF t_alv .
  types TO_CONNECTOR type ref to ZAPLINK_CONNECTOR .
  types TO_ACTIVATE type ref to ZAPLINK_ACTIVATE .

  class-methods _SAVE
    importing
      !FILE type TO_FILE
    exporting
      !CONTAINER type TO_CONTAINER
    raising
      ZAPLINK_CX_GUI .
  class-methods _LOAD
    importing
      !FILE type TO_FILE
    exporting
      !CONTAINER type TO_CONTAINER
      !CONT_NAME type TD_CONTNAME
    raising
      ZAPLINK_CX_GUI .
  class-methods _GET_OBJLIST
    importing
      !FILE type TO_FILE
    exporting
      !LIST type TO_LIST
      !CONTAINER type TO_CONTAINER
      !CONT_NAME type TD_CONTNAME
    raising
      ZAPLINK_CX_GUI .
  class-methods CREATE_FILE
    importing
      !FILENAME type TD_FILENAME
      !FILETYPE type TD_FILETYPE default ZAPLINK_FILE=>C_FT_LOCAL
    returning
      value(O_FILE) type TO_FILE
    raising
      ZAPLINK_CX_GUI .
  class-methods _UPDATE_SUBOBJ
    importing
      !O_LIST type TO_LIST
      !F_SUB type TD_WITH_SUBCOMP
    raising
      ZAPLINK_CX .
  class-methods _UPDATE_SELECTION
    importing
      !O_LIST type TO_LIST
    raising
      ZAPLINK_CX .
private section.

  constants:
    BEGIN OF c_light,
*        no_light TYPE t_light VALUE 0,
*        red      TYPE t_light VALUE 1,
*        yellow   TYPE t_light VALUE 2,
*        green    TYPE t_light VALUE 3,
        no       TYPE td_light VALUE ICON_LIGHT_OUT,
        red      TYPE td_light VALUE ICON_RED_LIGHT,
        yellow   TYPE td_light VALUE ICON_YELLOW_LIGHT,
        green    TYPE td_light VALUE ICON_GREEN_LIGHT,
      END OF c_light .
  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_GUI IMPLEMENTATION.


  method ADD_OBJECT.
  DATA o_cont   TYPE to_container.
  DATA obj_list TYPE to_list.
  DATA obj      TYPE to_component.
  DATA _objname TYPE string.
  DATA _cx      TYPE REF TO zaplink_cx.

  IF objtype IS INITIAL.
    RAISE EXCEPTION TYPE zaplink_cx_gui EXPORTING textid = zaplink_cx_gui=>missing_objtype.
  ELSEIF name IS INITIAL.
    RAISE EXCEPTION TYPE zaplink_cx_gui EXPORTING textid = zaplink_cx_gui=>missing_name.
  ENDIF.

  TRY.
    CALL METHOD _load
      EXPORTING
        file      = file
      IMPORTING
        container = o_cont.

    CREATE OBJECT obj.
    obj->set_type( objtype ).
    _objname = name.
    obj->set_name( _objname ).
    obj->set_with_subcomp( with_subcomp ).

    IF obj->connector_exists( ) = zaplink_component=>connexists-not_exists. " OR NOT obj->connector( ) IS BOUND.
      RAISE EXCEPTION TYPE zaplink_cx_connector EXPORTING
            textid = zaplink_cx_connector=>invalid_type
              type = objtype.
    ENDIF.

    CREATE OBJECT obj_list.
    obj_list->add( obj ).
    obj_list->select( obj ).

    IF o_options IS BOUND.
      o_cont->set_options( o_options ).
    ENDIF.

    o_cont->add_components( obj_list ).

    CALL METHOD _save
      EXPORTING
        file      = file
      IMPORTING
        container = o_cont.

    IF o_cont->log->has_messages_of_msgt( id_msgty = 'E' ) = abap_true.
      o_mycx ?= zaplink_cx=>create_from_application_log( cx_name = 'ZAPLINK_CX_GUI'
                                               message_collector = o_cont->log ).
      RAISE EXCEPTION o_mycx.
    ENDIF.

    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method CHANGE_SELECTION.
  TRY.
      IF NOT action IS INITIAL.
        list->select_default( action ).
      ENDIF.
      list->change_selection( ).
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method CLASS_CONSTRUCTOR.
  mac_create_log application_log ballog_subobject space.
  endmethod.


  method CONTAINERFILE_SELECT.
  DATA o_file    TYPE to_file.
  DATA o_cx_file TYPE REF TO zaplink_cx_file.
  DATA d_msg     TYPE string.

  TRY.
      CREATE OBJECT o_file.
      o_file->set_filetype( zaplink_file=>filetypes-local ).
      o_file->load_dialog( filename ).
      filename = o_file->get_filename( ).
    CATCH zaplink_cx_file INTO o_cx_file.
      CHECK o_cx_file->is_exception_text( zaplink_cx_file=>dialog_canceled ) IS INITIAL.
      d_msg = o_cx_file->get_text( ).
      MESSAGE d_msg TYPE 'W'.
  ENDTRY.
  endmethod.


  method CREATE.
  DATA s_info    TYPE ts_contdata.
  DATA container TYPE to_container.
  DATA def_fn    TYPE td_filename.
*  DATA o_file    TYPE to_file.

  IF name IS INITIAL.
    RAISE EXCEPTION TYPE zaplink_cx_gui EXPORTING textid = zaplink_cx_gui=>missing_container_name.
  ENDIF.

  TRY.
*      CONCATENATE name ext_sep file_ext INTO def_fn.
*      IF filename IS INITIAL.
*        o_file = create_file( filename = def_fn ).
*      ELSE.
*        o_file = create_file( filename = filename ).
*      ENDIF.
*      o_file->save_dialog( def_fn ).

    s_info-name = name.
    container = zaplink_container=>create_new( o_file = file
                                                 data = s_info ).
    container->save_to_file( ).

    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method CREATE_FILE.
  DATA _cx TYPE REF TO zaplink_cx_file.
  TRY.
      CREATE OBJECT o_file.
      o_file->set_filetype( filetype ).
      o_file->set_filename( filename ).
    CATCH zaplink_cx_file INTO _cx.
      mac_cascade_raise o_mycx _cx.
  ENDTRY.
  endmethod.


  method EXPORT.
  DATA o_cont     TYPE to_container.

  TRY.
    CALL METHOD _get_objlist
      EXPORTING
        file      = file
      IMPORTING
*          list      = o_list
        container = o_cont.

    IF o_options IS BOUND.
      o_cont->set_options( o_options ).
    ENDIF.

    _update_subobj( o_list = objlist
                     f_sub = with_subcomp ).
    o_cont->add_components( objlist ).

    CALL METHOD _save
      EXPORTING
        file      = file
      IMPORTING
        container = o_cont.

    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method GET_OBJLIST.
  TRY.
      CALL METHOD _get_objlist
        EXPORTING
          file         = file
         IMPORTING
           list        = list.
    CATCH zaplink_cx_gui INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method GET_VALUES.
  DATA o_conn TYPE to_connector.

  CHECK NOT type IS INITIAL.
  TRY.
      o_conn = zaplink_connectors=>create_connector( type = type ).
    CATCH zaplink_cx INTO o_cx.
      IF o_cx->is_exception_text( zaplink_cx_connector=>invalid_type ) = abap_true.
* There is no ZAPLink connector installed for object type '&1'
        MESSAGE w050 WITH type.
        EXIT.
      ELSE.
        RAISE EXCEPTION o_cx.
      ENDIF.
  ENDTRY.

  TRY.
      CALL METHOD o_conn->get_values
        EXPORTING
          type = type
        CHANGING
          name = value.
    CATCH zaplink_cx_connector INTO o_cx.
      RAISE EXCEPTION o_cx.
  ENDTRY.
  endmethod.


  method IMPORT.
  DATA o_cont    TYPE to_container.
  DATA _cx       TYPE REF TO zaplink_cx.
  DATA o_activ   TYPE to_activate.
  DATA o_list    TYPE to_list.
  DATA t_keys    TYPE tt_compkeys.
  DATA t_keys_ko TYPE tt_compkeys.
  DATA o_comp    TYPE to_component.
  DATA s_key     LIKE LINE OF t_keys_ko.

  TRY.
      CALL METHOD _get_objlist
        EXPORTING
          file      = file
        IMPORTING
          list      = list
          container = o_cont
          cont_name = name.

*  IF my_badi IS BOUND.
*    TRY.
*        CALL METHOD my_badi->cont_imp_at_load
*          EXPORTING
*            file = file
*          CHANGING
*            list   = list.
*      CATCH zaplink_cx INTO o_mycx.
*        RAISE EXCEPTION o_mycx.
*    ENDTRY.
*  ENDIF.

      _update_subobj( o_list = list
                       f_sub = with_subcomp ).
      list->select_default( zaplink_list=>sel_actions-import ).
      list->change_selection( ).
      _update_selection( list ).

*  IF my_badi IS BOUND.
*    TRY.
*        CALL METHOD my_badi->cont_imp_before
*          EXPORTING
*            file = file
*          CHANGING
*            list   = list.
*      CATCH zaplink_cx INTO o_mycx.
*        RAISE EXCEPTION o_mycx.
*    ENDTRY.
*  ENDIF.

      IF o_options IS BOUND.
        o_cont->set_options( o_options ).
      ENDIF.
      o_cont->transport_to_sap( list ).

*  IF my_badi IS BOUND.
*    TRY.
*        CALL METHOD my_badi->cont_imp_after
*          EXPORTING
*            file = file
*          CHANGING
*            list   = list.
*      CATCH zaplink_cx INTO o_mycx.
*        RAISE EXCEPTION o_mycx.
*    ENDTRY.
*  ENDIF.
*  raise event after_file_import
*    exporting
*      file     =
*      filename =
*      list       =
*      .

      IF activate = abap_true.
        o_list = list->selection_as_list( ).
        t_keys = o_list->get_keys( ).
        CREATE OBJECT o_activ.
        o_activ->add_keys( t_keys ).
        t_keys_ko = o_activ->activate( force_activation = activate_anyway ).
        SORT t_keys_ko BY type name.
        list->init_sel_iter( ).
        list->display_progress = abap_false.
        o_comp = list->get_next( ).
        WHILE o_comp IS BOUND.
          s_key-type = o_comp->get_type( ).   s_key-name = o_comp->get_name( ).
          READ TABLE t_keys_ko TRANSPORTING NO FIELDS
               WITH KEY type = s_key-type
                        name = s_key-name.
          IF sy-subrc = 0.    o_comp->set_action( zaplink_list=>sel_actions-not_active ).   ELSE.   o_comp->set_action( zaplink_list=>sel_actions-activated ).   ENDIF.
          o_comp = list->get_next( ).
        ENDWHILE.
      ENDIF.

    CATCH zaplink_cx INTO _cx.
      CREATE OBJECT o_mycx
        EXPORTING
          textid   = zaplink_cx_gui=>system_error
          previous = _cx.
      o_mycx->update( ).
      RAISE EXCEPTION o_mycx.

  ENDTRY.
  endmethod.


  method REFRESH.
  DATA o_cont     TYPE to_container.

  TRY.
    CALL METHOD _get_objlist
      EXPORTING
        file      = file
      IMPORTING
        list      = objlist
        container = o_cont.

    IF o_options IS BOUND.
      o_cont->set_options( o_options ).
    ENDIF.

    _update_subobj( o_list = objlist
                     f_sub = with_subcomp ).
    objlist->select_default( zaplink_list=>sel_actions-export ).
    objlist->change_selection( ).
    o_cont->add_components( objlist ).

    CALL METHOD _save
      EXPORTING
        file      = file
      IMPORTING
        container = o_cont.

    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method UNINSTALL.
  DATA o_list TYPE to_list.
  DATA o_cont TYPE to_container.

  TRY.
    CALL METHOD _get_objlist
      EXPORTING
        file      = file
      IMPORTING
        list      = o_list
        container = o_cont
        cont_name = name.

    IF list IS BOUND.
      o_list->keep_commun_list( list ).
    ENDIF.

    _update_subobj( o_list = o_list
                     f_sub = with_subcomp ).
    o_list->select_default( zaplink_list=>sel_actions-uninstall ).
    o_list->change_selection( ).

    o_cont->uninstall_from_sap( o_list ).

    list = o_list.
    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method UPDATE_SUBOBJ.
  _update_subobj( o_list = o_list
                   f_sub = f_sub ).
  endmethod.


  method _GET_OBJLIST.

  TRY.
    CALL METHOD _load
      EXPORTING
        file      = file
      IMPORTING
        container = container
        cont_name = cont_name.

    CHECK container IS BOUND.
    list = container->get_content( ).

    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method _LOAD.
*  DATA o_file TYPE to_file.
  DATA d_filename TYPE td_filename.

  IF NOT file IS BOUND.
    RAISE EXCEPTION TYPE zaplink_cx_gui EXPORTING textid = zaplink_cx_gui=>missing_filename.
  ENDIF.

  TRY.
      d_filename = file->get_filename( ).
      IF file->get_filetype( ) = zaplink_file=>filetypes-local.
* Openning file '&1' on local machine
        IF 1 = 2. MESSAGE i002 WITH d_filename. ENDIF.
        CALL METHOD application_log->add
          EXPORTING
*      is_message   =
             id_msgty     = 'I'
             id_msgid     = 'ZAPLINK_GUI'
             id_msgno     = '002'
             id_msgv1     = d_filename
*      id_msgv2     =
*      id_msgv3     =
*      id_msgv4     =
            .
      ELSEIF file->get_filetype( ) = zaplink_file=>filetypes-server.
* Openning file '&1' on ABAP Server
        IF 1 = 2. MESSAGE i004 WITH d_filename. ENDIF.
        CALL METHOD application_log->add
          EXPORTING
*      is_message   =
             id_msgty     = 'I'
             id_msgid     = 'ZAPLINK_GUI'
             id_msgno     = '004'
             id_msgv1     = d_filename
*      id_msgv2     =
*      id_msgv3     =
*      id_msgv4     =
            .
      ENDIF.
*      CREATE OBJECT o_file.
*      o_file->set_filetype( zaplink_file=>c_ft_local ).
*      o_file->set_filename( file ).
**  o_file->set_filekind( zaplink_file_zip=>filekinds-zipped ).
      CREATE OBJECT container
        EXPORTING
          o_file = file.

      container->load_from_file( ).

    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method _SAVE.
*  DATA o_file TYPE to_file.
  DATA _fn    TYPE td_filename.

  TRY.
*      CREATE OBJECT o_file.
*      o_file->set_filetype( zaplink_file=>c_ft_local ).
    _fn = file->get_filename( ).
    IF _fn IS INITIAL.
      CALL METHOD file->save_dialog
        EXPORTING
          filename = _fn.
    ENDIF.
    file->set_filename( _fn ).

    container->save_to_file( file ).

    mac_def_catch zaplink_cx_gui.
  ENDTRY.
  endmethod.


  method _UPDATE_SELECTION.
  DATA o_comp   TYPE to_component.
  DATA o_slist  TYPE to_list.

  o_list->init_sel_iter( ).
  o_list->display_progress = abap_false.
  o_comp = o_list->get_next( ).
  WHILE o_comp IS BOUND.
    CLEAR o_slist.    o_slist = o_comp->get_subcomponents( ).
    IF o_slist IS BOUND.    o_slist->select_all( ).   ENDIF.    " for display list
    o_comp = o_list->get_next( ).
  ENDWHILE.
  endmethod.


  method _UPDATE_SUBOBJ.
  DATA o_comp   TYPE to_component.

  o_list->init_iter( ).
  o_list->display_progress = abap_false.
  o_comp = o_list->get_next( ).
  WHILE o_comp IS BOUND.
    o_comp->set_with_subcomp( f_sub ).
    o_comp = o_list->get_next( ).
  ENDWHILE.
  endmethod.
ENDCLASS.
