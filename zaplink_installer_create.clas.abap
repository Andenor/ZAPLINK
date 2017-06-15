class ZAPLINK_INSTALLER_CREATE definition
  public
  create public .

public section.
  type-pools ABAP .

  interfaces ZAPLINK_DATATYPES .

  aliases TD_FILENAME
    for ZAPLINK_DATATYPES~TD_FILENAME .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TR_TR
    for ZAPLINK_DATATYPES~TR_TR .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .

  types TT_ABAPRAWSOURCE type ZAPLINK_STANDALONE_GENERATOR=>TT_ABAPRAWSOURCE .
  types:
    BEGIN OF ts_files,
        container      TYPE td_filename,
        installer      TYPE td_filename,
      END OF ts_files .
  types:
    BEGIN OF ts_selection,
        request     TYPE tr_tr,
        packages    TYPE tr_packages,
        keys        type tt_compkeys,
      END OF ts_selection .
  types:
    BEGIN OF ts_programs,
        master  TYPE programm,
        include TYPE programm,
        program TYPE programm,
        PAckage type devclass,
      END OF ts_programs .
  types TT_ORDER type ZAPLINK_STANDALONE_GENERATOR=>TT_ORDER .

  data O_OBJECTS type ref to ZAPLINK_LIST read-only .
  data T_ORDER type TT_ORDER read-only .

  methods CREATE_INSTALLER
    raising
      ZAPLINK_CX .
  methods DISPLAY_CONTAINER_CONTENT
    raising
      ZAPLINK_CX .
  methods DISPLAY_OBJECT_ORDER .
  methods SET_PARAMETERS
    importing
      !FILES type TS_FILES
      !OVERWRITE type ABAP_BOOL
      !PROGRAMS type TS_PROGRAMS
      !SELECTION type TS_SELECTION .
protected section.

  data OVERWRITE type ABAP_BOOL .
  data O_GENERATOR type ref to ZAPLINK_STANDALONE_GENERATOR .
  data S_FILES type TS_FILES .
  data S_PROGRAMS type TS_PROGRAMS .
  data S_SELECTION type TS_SELECTION .
  data T_LOADS type TT_ABAPRAWSOURCE .

  methods BUILD_CLASS_POOL
    importing
      !KEYS type TT_COMPKEYS .
  methods CHECK_PROG_INCLUDE
    importing
      !PROGRAMS type TS_PROGRAMS
    returning
      value(_INDEX) type I .
  class-methods CHECK_PROG_POS
    importing
      !CODE type TT_ABAPRAWSOURCE
      !SEARCH type STRING
    returning
      value(POSITION) type I .
  methods GETOBJ_LIST
    importing
      !SELECTION type TS_SELECTION
    returning
      value(RESULT) type TT_COMPKEYS .
private section.

  data MASTER_SOURCECODE type TT_ABAPRAWSOURCE .
  data MASTER_UP type TT_ABAPRAWSOURCE .
ENDCLASS.



CLASS ZAPLINK_INSTALLER_CREATE IMPLEMENTATION.


  method BUILD_CLASS_POOL.
  DATA _line LIKE LINE OF t_loads.
  DATA _comp TYPE REF TO zaplink_component.
  DATA _classname TYPE zaplink_component=>td_connclass.
  DATA o_conn TYPE REF TO zaplink_connector.
  DATA _classes TYPE SORTED TABLE OF zaplink_component=>td_connclass WITH UNIQUE KEY table_line.
  DATA _cx         TYPE REF TO zaplink_cx.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF keys.

*  SORT keys BY classname.
*  DELETE ADJACENT DUPLICATES FROM keys COMPARING classname.
  _line = 'zaplink_connectors=>clear_connectors( ).'. APPEND _line TO t_loads. " remove ABAP class connector to add only local ones
  _line = 'DATA _cx_90335e5d44316adf4805a732f type ref to ZAPLINK_CX_COMPONENT.'. APPEND _line TO t_loads. "#EC NOTEXT
  _line = 'TRY.'. APPEND _line TO t_loads.                  "#EC NOTEXT
  LOOP AT keys ASSIGNING <c>.
    TRY.
        CREATE OBJECT _comp.
        _comp->set_type( <c>-type ).
        o_conn = _comp->connector( ).
        _classname = zaplink_connectors=>get_classname( o_conn ).
        CHECK NOT _classname IS INITIAL.
        READ TABLE _classes TRANSPORTING NO FIELDS
             WITH TABLE KEY table_line = _classname.
        IF sy-subrc <> 0.
          INSERT _classname INTO TABLE _classes.
          o_generator->add_object( _classname ).
          CLEAR _line.
          CONCATENATE 'ZAPLINK_CONNECTORS=>ADD_LOCAL_CONNECTOR(''' _classname ''').' INTO _line. "#EC NOTEXT
          APPEND _line TO t_loads.
        ENDIF.
      CATCH zaplink_cx INTO _cx.
        _cx->write( ).
    ENDTRY.
  ENDLOOP.
  _line = 'CATCH ZAPLINK_CX_COMPONENT INTO _cx_90335e5d44316adf4805a732f.'. APPEND _line TO t_loads. "#EC NOTEXT
  _line = '_cx_90335e5d44316adf4805a732f->write( ).'. APPEND _line TO t_loads. "#EC NOTEXT
  _line = 'BREAK-POINT.'. APPEND _line TO t_loads.          "#EC NOTEXT
  _line = 'RAISE EXCEPTION _cx_90335e5d44316adf4805a732f.'. APPEND _line TO t_loads. "#EC NOTEXT
  _line = 'ENDTRY.'. APPEND _line TO t_loads.               "#EC NOTEXT
  endmethod.


  method CHECK_PROG_INCLUDE.
  DATA _search  TYPE string.
  DATA tmp      TYPE string.

  CLEAR _index.     CONCATENATE '*INCLUDE*' programs-include '*' INTO _search.      TRANSLATE _search TO UPPER CASE.
  LOOP AT master_up INTO tmp.
    CHECK tmp CP _search.
    CHECK tmp(1) <> '*'. " comment line
    _index = sy-tabix.
    EXIT.
  ENDLOOP.
  endmethod.


  method CHECK_PROG_POS.
  DATA tmp      TYPE string.
  DATA p_text   TYPE string.

  p_text = search.    TRANSLATE p_text TO UPPER CASE.
  LOOP AT code INTO tmp.
    TRANSLATE tmp TO UPPER CASE.
    CHECK tmp CP p_text.
    CHECK tmp(1) <> '*'. " comment line
    position = sy-tabix.
    EXIT.
  ENDLOOP.
  endmethod.


  method CREATE_INSTALLER.
  DATA gencode     TYPE tt_abaprawsource.
  DATA finalcode   TYPE tt_abaprawsource.
*  DATA abapcode    TYPE TT_ABAPRAWSOURCE.
  DATA t_conns     TYPE tt_compkeys.
  DATA idx_init    TYPE sy-tabix.
*  DATA abapcode_up TYPE TT_ABAPRAWSOURCE.
  DATA m_index     TYPE i.
  DATA _prog       TYPE progdir.
  DATA _oname      TYPE e071-obj_name.
  DATA d_source    TYPE string.
  DATA o_file      TYPE REF TO zaplink_file.
  DATA t_data      TYPE zaplink_container=>ts_contdata.
  DATA o_cont      TYPE REF TO zaplink_container.
  DATA t_type      TYPE zaplink_component=>td_comptype.
  DATA t_name      TYPE zaplink_component=>td_compname.
*  FIELD-SYMBOLS: <l> LIKE LINE OF abapcode_up.
  DATA o_activ     TYPE REF TO zaplink_activate.
  DATA t_keys      TYPE tt_compkeys.
  DATA t_keys_ko   TYPE tt_compkeys.
  DATA s_key       LIKE LINE OF t_keys.

  IF overwrite IS INITIAL.
    SELECT SINGLE name INTO _prog
      FROM progdir
      WHERE name = s_programs-program.
    IF sy-subrc = 0.
      RAISE EXCEPTION TYPE zaplink_cx_installer_create
            EXPORTING textid = zaplink_cx_installer_create=>no_overwrite
              installer_name = s_programs-program.
    ENDIF.
  ENDIF.
  SELECT SINGLE name INTO _prog
    FROM progdir
    WHERE name = s_programs-master.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zaplink_cx_installer_create
          EXPORTING textid = zaplink_cx_installer_create=>not_exists
               master_name = s_programs-master.
  ENDIF.
  m_index = check_prog_include( s_programs ).
  IF m_index IS INITIAL.
    RAISE EXCEPTION TYPE zaplink_cx_installer_create
          EXPORTING textid = zaplink_cx_installer_create=>no_include
              include_name = s_programs-include
               master_name = s_programs-master.
  ENDIF.
  CREATE OBJECT o_generator
*    EXPORTING
*      master = s_programs-master
      .
* Add Framework
*  READ REPORT s_programs-master INTO abapcode STATE 'I'.
*  IF sy-subrc <> 0.
*    READ REPORT s_programs-master INTO abapcode STATE 'A'.
*  ENDIF.
*  abapcode_up = abapcode.
*  LOOP AT abapcode_up ASSIGNING <l>.
*    TRANSLATE <l> TO UPPER CASE.                           "#EC SYNTCHAR
*  ENDLOOP.
  o_generator->add_object( 'ZAPLINK_MESSAGE_COLLECTOR' ).
*  o_generator->add_source( abapcode ).
  o_generator->add_source( code = master_sourcecode    origin = s_programs-master ).
  t_conns = getobj_list( s_selection ).
  build_class_pool( t_conns ).

  gencode = o_generator->generate( ).
  finalcode = master_sourcecode.

  idx_init = check_prog_pos( code = master_up
                           search = '*INITIALIZATION*.*').
  IF idx_init IS INITIAL.
    idx_init = check_prog_pos( code = master_up
                             search = '*START-OF-SELECTION*.*').
  ENDIF.

  IF idx_init > m_index.
* Initialize is after include : Normal case
* Adding Connector local class load
    IF idx_init IS INITIAL.
      APPEND LINES OF t_loads TO finalcode.
    ELSE.
      ADD 1 TO idx_init.
      INSERT LINES OF t_loads INTO finalcode INDEX idx_init.
    ENDIF.
* Then Adding local classes (INCLUDE).
    DELETE finalcode INDEX m_index.
    INSERT LINES OF gencode INTO finalcode INDEX m_index.
  ELSE.
* Initialize is before include : Anormal case that should lead to compiling error
* Adding local classes (INCLUDE).
    DELETE finalcode INDEX m_index.
    INSERT LINES OF gencode INTO finalcode INDEX m_index.
* Then Adding Connector local class load
    IF idx_init IS INITIAL.
      APPEND LINES OF t_loads TO finalcode.
    ELSE.
      ADD 1 TO idx_init.
      INSERT LINES OF t_loads INTO finalcode INDEX idx_init.
    ENDIF.
  ENDIF.

  t_order = o_generator->order.

  zaplink_tools=>remove_comment_on_source( CHANGING table = finalcode ).
  IF NOT s_programs-program IS INITIAL.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object                         = s_programs-program
        object_class                   = 'ABAP'
        mode                           = 'I'    " Insert
*         GLOBAL_LOCK                    = ' '
        devclass                       = s_programs-package
*         KORRNUM                        = ' '
*         USE_KORRNUM_IMMEDIATEDLY       = ' '
*         AUTHOR                         = ' '
*         MASTER_LANGUAGE                = ' '
*         GENFLAG                        = ' '
*         PROGRAM                        = ' '
*         OBJECT_CLASS_SUPPORTS_MA       = ' '
*         EXTEND                         = ' '
*         SUPPRESS_DIALOG                = ' '
*         MOD_LANGU                      = ' '
*         ACTIVATION_CALL                = ' '
*       IMPORTING
*         DEVCLASS                       =
*         KORRNUM                        =
*         ORDERNUM                       =
*         NEW_CORR_ENTRY                 =
*         AUTHOR                         =
*         TRANSPORT_KEY                  =
*         NEW_EXTEND                     =
      EXCEPTIONS
        cancelled                      = 1
        permission_failure             = 2
        unknown_objectclass            = 3
        OTHERS                         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    _oname = s_programs-program.
    CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
      EXPORTING
        object                  = 'REPS'
        obj_name                = _oname
*         DELETED_FLAG            = ' '
*       IMPORTING
*         OBJECT_INSERTED         =
      EXCEPTIONS
        wrong_object_name       = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    _prog-name = s_programs-program.
    _prog-state = 'I'.
    _prog-edtx = 'X'.
    _prog-varcl = 'X'.
*  _prog-fixpt = 'X'.
    _prog-rstat = 'T'.
    _prog-vern = 1.
    _prog-subc = '1'.    "Executable program
    _prog-unam = _prog-cnam = sy-uname.
    _prog-sdate = _prog-idate = _prog-udat = _prog-cdat = sy-datum.
    _prog-stime = _prog-itime = sy-uzeit.
    _prog-rmand = sy-mandt.
    _prog-rload = sy-langu.
    MODIFY progdir FROM _prog.
*  INSERT progdir FROM _prog.
    INSERT REPORT _prog-name FROM finalcode STATE _prog-state.
*    _prog-state = 'A'.
*    MODIFY progdir FROM _prog.
*    INSERT REPORT _prog-name FROM finalcode STATE _prog-state.

    COMMIT WORK AND WAIT.
  ENDIF.

  REFRESH t_keys.
  s_key-type = 'PROG'.   s_key-name = s_programs-program.   APPEND s_key TO t_keys.
  CREATE OBJECT o_activ.
  o_activ->add_keys( t_keys ).
  t_keys = o_activ->activate( ).
  IF NOT t_keys_ko IS INITIAL.
    RAISE EXCEPTION TYPE zaplink_cx_installer_create
          EXPORTING textid = zaplink_cx_installer_create=>activation_error.
  ENDIF.

  CREATE OBJECT o_file.
  IF NOT s_files-installer IS INITIAL.
    d_source = zaplink_standalone_generator=>source_2_string( finalcode ).
    o_file->set_filetype( zaplink_file=>filetypes-local ).
    o_file->set_filename( s_files-installer ).
    o_file->set_filecontent( d_source ).
    o_file->save( ).
  ENDIF.

  o_file->set_filename( s_files-container ).
  CONCATENATE sy-cprog sy-uname sy-datum sy-uzeit INTO t_data-name.
  o_cont = zaplink_container=>create_new( data = t_data
                                        o_file = o_file ).
  o_cont->add_components( o_objects ).
  o_cont->save_to_file( ).
  o_objects = o_cont->get_content( ).
  endmethod.


  method DISPLAY_CONTAINER_CONTENT.
  DATA o_comp      TYPE REF TO zaplink_component.
  DATA t_type      TYPE zaplink_component=>td_comptype.
  DATA t_name      TYPE zaplink_component=>td_compname.

  o_objects->init_iter( ).
  o_objects->display_progress = abap_false.
  o_comp = o_objects->get_next( ).
  WRITE:/ 'Container''s containt :'(001).
  WHILE NOT o_comp IS INITIAL.
    t_type = o_comp->get_type( ).
    t_name = o_comp->get_name( ).
    WRITE:/ t_type, t_name.
    o_comp = o_objects->get_next( ).
  ENDWHILE.
  endmethod.


  method DISPLAY_OBJECT_ORDER.
  FIELD-SYMBOLS:  <o> LIKE LINE OF t_order.

  LOOP AT t_order ASSIGNING <o>.
    CASE <o>-type.
      WHEN zaplink_standalone_generator=>objtype-exception_class.
        WRITE :/ 'CXCL', <o>-name, <o>-defered.
      WHEN zaplink_standalone_generator=>objtype-class.
        WRITE :/ 'CLAS', <o>-name, <o>-defered.
      WHEN zaplink_standalone_generator=>objtype-interface.
        WRITE :/ 'INTF', <o>-name, <o>-defered.
      WHEN OTHERS.
        WRITE :/ <o>-type, <o>-name, <o>-defered.
    ENDCASE.
  ENDLOOP.
  endmethod.


  method GETOBJ_LIST.
  DATA so_tr   TYPE tr_tr.
  DATA _tr     LIKE LINE OF so_tr.
  DATA _count  TYPE i.
  DATA t_objs  TYPE tt_compkeys.
  DATA _cx     TYPE REF TO zaplink_cx.

  IF NOT selection-request IS INITIAL. result = zaplink_gui=>get_keys_from_tr( selection-request ).   ENDIF.

  IF NOT selection-packages[] IS INITIAL.
    t_objs = zaplink_gui=>get_keys_from_package( packages = selection-packages
                                                 ext      = abap_true ).
    APPEND LINES OF t_objs TO result.
  ENDIF.

  APPEND LINES OF selection-keys TO result.

  SORT result.
  DELETE ADJACENT DUPLICATES FROM result.

  TRY.
      CREATE OBJECT o_objects.
      o_objects->add_keys( result ).
      o_objects->select_default( zaplink_list=>sel_actions-export ).
    CATCH zaplink_cx INTO _cx.
      CLEAR result.   _cx->write( ).
  ENDTRY.
  SORT result BY type.
  DELETE ADJACENT DUPLICATES FROM result COMPARING type.
  endmethod.


  method SET_PARAMETERS.
  FIELD-SYMBOLS: <l> LIKE LINE OF master_up.

  s_programs = programs.
  s_files = files.
  s_selection = selection.
  me->overwrite = overwrite.
  READ REPORT s_programs-master INTO master_sourcecode STATE 'I'.
  IF sy-subrc <> 0.
    READ REPORT s_programs-master INTO master_sourcecode STATE 'A'.
  ENDIF.
  master_up = master_sourcecode.
  LOOP AT master_up ASSIGNING <l>.
    TRANSLATE <l> TO UPPER CASE.                          "#EC SYNTCHAR
  ENDLOOP.
  endmethod.
ENDCLASS.
