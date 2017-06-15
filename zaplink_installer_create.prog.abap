*&---------------------------------------------------------------------*
*& Report  ZAPLINK_INSTALLER_CREATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zaplink_installer_create.
TABLES: tdevc.

PARAMETERS p_incl   TYPE programm OBLIGATORY DEFAULT 'ZAPLINK_INSTALLER_CLASSES'.
PARAMETERS p_overw  TYPE flag AS CHECKBOX.
PARAMETERS p_prog   TYPE programm DEFAULT 'ZAPLINK_INSTALLER'.
PARAMETERS p_master TYPE programm OBLIGATORY DEFAULT 'ZAPLINK_INSTALLER_MASTER'.
PARAMETERS p_trkorr TYPE trkorr.
SELECT-OPTIONS:
  s_pack FOR tdevc-devclass.
PARAMETERS p_devc   TYPE devclass DEFAULT 'ZAPLINK-INSTALLER'.
PARAMETERS p_fn     TYPE zaplink_file=>td_filename LOWER CASE OBLIGATORY.
PARAMETERS p_ifn    TYPE zaplink_file=>td_filename LOWER CASE.

*CONSTANTS c_exists TYPE ZAPLINK_DATATYPES=>td_exists VALUE ZAPLINK_DATATYPES=>exists-exists.

TYPES t_obj_list TYPE zaplink_list=>tt_compkeys.
TYPES tr_pack    TYPE zaplink_gui=>tr_packages.
TYPES td_sourcecode TYPE zaplink_standalone_generator=>tt_abaprawsource.
DATA t_conns     TYPE t_obj_list.
DATA o_generator TYPE REF TO zaplink_standalone_generator.
DATA o_objects   TYPE REF TO zaplink_list.
DATA o_comp      TYPE REF TO zaplink_component.
DATA o_file      TYPE REF TO zaplink_file.
DATA o_cont      TYPE REF TO zaplink_container.
DATA t_type      TYPE zaplink_component=>td_comptype.
DATA t_name      TYPE zaplink_component=>td_compname.
DATA t_data      TYPE zaplink_container=>ts_contdata.
DATA _cx         TYPE REF TO zaplink_cx.
DATA abapcode    TYPE td_sourcecode.
DATA ac_loadlocal TYPE td_sourcecode.
DATA m_code      TYPE td_sourcecode.
DATA m_index     TYPE sy-tabix.
DATA idx_init    TYPE sy-tabix.
DATA _prog       TYPE progdir.
DATA _oname      TYPE e071-obj_name.
DATA d_source    TYPE string.
FIELD-SYMBOLS:
  <o> LIKE LINE OF o_generator->order.

INITIALIZATION.
  DATA t_pack LIKE LINE OF s_pack.

  t_pack-sign = 'I'.  t_pack-option = 'EQ'.
  t_pack-low = 'ZAPLINK'.  APPEND t_pack TO s_pack.
  t_pack-sign = 'E'.  t_pack-option = 'EQ'.
  t_pack-low = 'ZAPLINK-FRAMEWORK-DEVTOOLS'.  APPEND t_pack TO s_pack.
  t_pack-sign = 'E'.  t_pack-option = 'EQ'.
  t_pack-low = 'ZAPLINK-TUTORIALS'.  APPEND t_pack TO s_pack.
  t_pack-sign = 'E'.  t_pack-option = 'EQ'.
  t_pack-low = 'ZAPLINK-INSTALLER'.  APPEND t_pack TO s_pack.
  TRY.
      IF NOT o_file IS BOUND. CREATE OBJECT o_file. ENDIF.
      IF sy-batch IS INITIAL.
        CONCATENATE sy-uname '-' sy-datum sy-uzeit '.xml' INTO p_fn.
        o_file->set_filetype( zaplink_file=>filetypes-local ).
      ELSE.
        CONCATENATE '/tmp/' sy-uname '-' sy-datum sy-uzeit '.xml' INTO p_fn.
        o_file->set_filetype( zaplink_file=>filetypes-server ).
      ENDIF.
*      o_file->set_filekind( zaplink_file=>filekinds-zipped ).
    CATCH zaplink_cx INTO _cx.
      _cx->write( ).
  ENDTRY.

AT SELECTION-SCREEN ON p_prog.
  IF p_overw IS INITIAL.
    PERFORM check_prog_notexists USING p_prog.
  ENDIF.

AT SELECTION-SCREEN ON p_devc.
  IF NOT p_prog IS INITIAL AND p_devc IS INITIAL.
    MESSAGE 'Specify program''s devlopement class'(pdc) TYPE 'E'.
  ENDIF.

*AT SELECTION-SCREEN ON p_incl.
*  PERFORM check_prog_notexists USING p_incl.

AT SELECTION-SCREEN.
  PERFORM check_prog_include USING p_master  p_incl
                          CHANGING m_index   m_code.

AT SELECTION-SCREEN ON p_master.
  PERFORM check_prog_exists USING p_master.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fn.
  TRY.
      o_file->save_dialog( p_fn ).
      p_fn = o_file->get_filename( ).
    CATCH zaplink_cx INTO _cx.
      _cx->write( ).
  ENDTRY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ifn.
  TRY.
      o_file->save_dialog( p_ifn ).
      p_ifn = o_file->get_filename( ).
    CATCH zaplink_cx INTO _cx.
      _cx->write( ).
  ENDTRY.

START-OF-SELECTION.
  CREATE OBJECT o_generator.
* Add Framework
  READ REPORT p_master INTO abapcode STATE 'I'.
  IF sy-subrc <> 0.
    READ REPORT p_master INTO abapcode STATE 'A'.
  ENDIF.
  o_generator->add_object( 'ZAPLINK_MESSAGE_COLLECTOR' ).
  o_generator->add_source( abapcode ).
  PERFORM getobj_list USING p_trkorr
                            s_pack[]
*                   CHANGING t_objects t_conns
                   CHANGING t_conns
                     .

  PERFORM build_class_pool USING t_conns.

  abapcode = o_generator->generate( ).

  PERFORM check_prog_pos USING m_code '*INITIALIZATION*.*' CHANGING idx_init.
  IF idx_init IS INITIAL.
    PERFORM check_prog_pos USING m_code '*START-OF-SELECTION*.*' CHANGING idx_init.
  ENDIF.
  IF idx_init IS INITIAL.
    APPEND LINES OF ac_loadlocal TO abapcode.
  ELSE.
    ADD 1 TO idx_init.
    INSERT LINES OF ac_loadlocal INTO m_code INDEX idx_init.
  ENDIF.

  DELETE m_code INDEX m_index.
  INSERT LINES OF abapcode INTO m_code INDEX m_index.

  LOOP AT o_generator->order ASSIGNING <o>.
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

  IF NOT p_prog IS INITIAL.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object                         = p_prog
        object_class                   = 'ABAP'
        mode                           = 'I'    " Insert
*         GLOBAL_LOCK                    = ' '
        devclass                       = p_devc
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

    _oname = _prog.
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

    _prog-name = p_prog.
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
    INSERT REPORT _prog-name FROM m_code STATE _prog-state.
    _prog-state = 'A'.
    MODIFY progdir FROM _prog.
*  INSERT progdir FROM _prog.
    zaplink_tools=>remove_comment_on_source( CHANGING table = m_code ).
*    m_code = zaplink_standalone_generator=>clean_code( m_code ).
    INSERT REPORT _prog-name FROM m_code STATE _prog-state.

    COMMIT WORK AND WAIT.
  ENDIF.

  IF NOT p_ifn IS INITIAL.
    d_source = zaplink_standalone_generator=>source_2_string( m_code ).
    TRY.
        o_file->set_filename( p_ifn ).
        o_file->set_filecontent( d_source ).
        o_file->save( ).
      CATCH zaplink_cx INTO _cx.
        _cx->write( ).
    ENDTRY.
  ENDIF.

  WRITE sy-uline.
  TRY.
      o_file->set_filename( p_fn ).
      CONCATENATE sy-cprog sy-uname sy-datum sy-uzeit INTO t_data-name.
      o_cont = zaplink_container=>create_new( data = t_data
                                            o_file = o_file ).
      o_cont->add_components( o_objects ).
      o_objects = o_cont->get_content( ).
      o_objects->init_iter( ).
      o_objects->display_progress = abap_false.
      o_comp = o_objects->get_next( ).
      WRITE:/ 'Container containt :'(001).
      WHILE NOT o_comp IS INITIAL.
        t_type = o_comp->get_type( ).
        t_name = o_comp->get_name( ).
        WRITE:/ t_type, t_name.
        o_comp = o_objects->get_next( ).
      ENDWHILE.
      o_cont->save_to_file( ).
    CATCH zaplink_cx INTO _cx.
      _cx->write( ).
  ENDTRY.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  GETOBJ_LIST
*&---------------------------------------------------------------------*
FORM getobj_list  USING    p_trkorr  TYPE trkorr
                           s_pack    TYPE tr_pack
                  CHANGING t_connectors TYPE t_obj_list
  .

  DATA so_tr   TYPE zaplink_gui=>tr_tr.
  DATA _tr     LIKE LINE OF so_tr.
  DATA _count  TYPE i.
  DATA t_objs  TYPE t_obj_list.

  IF NOT p_trkorr IS INITIAL.
    _tr-sign = 'I'. _tr-option = 'EQ'.
    _tr-low = p_trkorr. APPEND _tr TO so_tr.

    t_connectors = zaplink_gui=>get_keys_from_tr( so_tr ).
  ENDIF.

  IF NOT s_pack[] IS INITIAL.
    t_objs = zaplink_gui=>get_keys_from_package( packages = s_pack
                                                 ext      = abap_true ).
    APPEND LINES OF t_objs TO t_connectors.
  ENDIF.

  SORT t_connectors.
  DELETE ADJACENT DUPLICATES FROM t_connectors.

  TRY.
      CREATE OBJECT o_objects.
*      t_objs = t_connectors.
*      delete t_objs where type <> 'DEVC'.
*      o_objects->add_keys( t_objs ).
*      delete  t_connectors where type = 'DEVC'.
      o_objects->add_keys( t_connectors ).
      o_objects->select_default( zaplink_list=>sel_actions-export ).

    CATCH zaplink_cx INTO _cx.
      _cx->write( ).
  ENDTRY.
  SORT t_connectors BY type.
  DELETE ADJACENT DUPLICATES FROM t_connectors COMPARING type.

ENDFORM.                    " GETOBJ_LIST
*&---------------------------------------------------------------------*
*&      Form  BUILD_CLASS_POOL
*&---------------------------------------------------------------------*
FORM build_class_pool  USING    t_connectors TYPE t_obj_list.
  DATA _line LIKE LINE OF ac_loadlocal.
  DATA _comp TYPE REF TO zaplink_component.
  DATA _classname TYPE zaplink_component=>td_connclass.
  DATA o_conn TYPE REF TO zaplink_connector.
  DATA _classes TYPE SORTED TABLE OF zaplink_component=>td_connclass WITH UNIQUE KEY table_line.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF t_connectors.

*  SORT t_connectors BY classname.
*  DELETE ADJACENT DUPLICATES FROM t_connectors COMPARING classname.
  _line = 'zaplink_connectors=>clear_connectors( ).'. APPEND _line TO ac_loadlocal. " remove ABAP class connector to add only local ones
  _line = 'DATA _cx_90335e5d44316adf4805a732f type ref to ZAPLINK_CX_COMPONENT.'. APPEND _line TO ac_loadlocal. "#EC NOTEXT
  _line = 'TRY.'. APPEND _line TO ac_loadlocal.             "#EC NOTEXT
  LOOP AT t_connectors ASSIGNING <c>.
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
          APPEND _line TO ac_loadlocal.
        ENDIF.
      CATCH zaplink_cx INTO _cx.
        _cx->write( ).
    ENDTRY.
  ENDLOOP.
  _line = 'CATCH ZAPLINK_CX_COMPONENT INTO _cx_90335e5d44316adf4805a732f.'. APPEND _line TO ac_loadlocal. "#EC NOTEXT
  _line = '_cx_90335e5d44316adf4805a732f->write( ).'. APPEND _line TO ac_loadlocal. "#EC NOTEXT
  _line = 'BREAK-POINT.'. APPEND _line TO ac_loadlocal.     "#EC NOTEXT
  _line = 'RAISE EXCEPTION _cx_90335e5d44316adf4805a732f.'. APPEND _line TO ac_loadlocal. "#EC NOTEXT
  _line = 'ENDTRY.'. APPEND _line TO ac_loadlocal.          "#EC NOTEXT

ENDFORM.                    " BUILD_CLASS_POOL
*&---------------------------------------------------------------------*
*&      Form  CHECK_PROG_NOTEXISTS
*&---------------------------------------------------------------------*
FORM check_prog_notexists  USING    p_prog.
  DATA: _name TYPE progdir-name.
  SELECT SINGLE name
    INTO _name
    FROM progdir
    WHERE name = p_prog.
  IF sy-subrc = 0.
* Program &1 already exists; specify another name
    MESSAGE e228(ez) WITH p_prog.
  ENDIF.
ENDFORM.                    " CHECK_PROG_NOTEXISTS
*&---------------------------------------------------------------------*
*&      Form  CHECK_PROG_EXISTS
*&---------------------------------------------------------------------*
FORM check_prog_include  USING  p_prog
                                value(p_incl)
                      CHANGING  _index   TYPE sy-tabix
                                abapcode TYPE td_sourcecode.
  DATA: _search TYPE string.
  DATA tmp TYPE string.

  CLEAR _index.
  TRANSLATE p_incl TO UPPER CASE.
  CONCATENATE '*INCLUDE*' p_incl '*' INTO _search.
  READ REPORT p_prog INTO abapcode STATE 'I'.
  IF sy-subrc <> 0.
    READ REPORT p_prog INTO abapcode STATE 'A'.
  ENDIF.
  LOOP AT abapcode INTO tmp.
    TRANSLATE tmp TO UPPER CASE.
    CHECK tmp CP _search.
    CHECK tmp(1) <> '*'. " comment line
    _index = sy-tabix.
    EXIT.
  ENDLOOP.
  IF _index IS INITIAL.
* Include &1 is not incorporated in program &2
    MESSAGE e012(swy_task_form) WITH p_incl p_prog.
  ENDIF.
ENDFORM.                    " CHECK_PROG_NOTEXISTS
*&---------------------------------------------------------------------*
*&      Form  check_prog_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ABAPCODE   text
*      -->P_TEXT     text
*      -->_INDEX     text
*----------------------------------------------------------------------*
FORM check_prog_pos  USING  abapcode TYPE td_sourcecode
                       value(p_text) TYPE string
                  CHANGING  _index   TYPE sy-tabix.
  DATA tmp TYPE string.

  CLEAR _index.
  TRANSLATE p_text TO UPPER CASE.
  LOOP AT abapcode INTO tmp.
    TRANSLATE tmp TO UPPER CASE.
    CHECK tmp CP p_text.
    CHECK tmp(1) <> '*'. " comment line
    _index = sy-tabix.
    EXIT.
  ENDLOOP.
ENDFORM.                    " CHECK_PROG_NOTEXISTS
*&---------------------------------------------------------------------*
*&      Form  CHECK_PROG_EXISTS
*&---------------------------------------------------------------------*
FORM check_prog_exists  USING    p_prog.
  DATA: _name TYPE progdir-name.
  SELECT SINGLE name
    INTO _name
    FROM progdir
    WHERE name = p_prog.
  IF sy-subrc <> 0.
* Program & does not exist
    MESSAGE e541(00) WITH p_prog.
  ENDIF.
ENDFORM.                    " CHECK_PROG_NOTEXISTS
