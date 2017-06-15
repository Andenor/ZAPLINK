class ZAPLINK_STANDALONE_GENERATOR definition
  public
  inheriting from ZAPLINK_DEPENDENCIES_ANALYSER
  create public .

public section.
  type-pools ABAP .
  type-pools SEOP .

  data MASTER_PROGRAM type TD_PROGNAME read-only .

  methods ADD_CONNECTORS
    raising
      ZAPLINK_CX .
  methods CONSTRUCTOR
    importing
      !MASTER type TD_PROGNAME optional
    raising
      ZAPLINK_CX .
  methods GENERATE
    returning
      value(CODE) type TT_ABAPRAWSOURCE .
  methods SAVE_REPORT
    importing
      !CODE type TT_ABAPRAWSOURCE optional
      !PACKAGE type TD_DEVCLASS default '$TMP'
      !REPORT type TD_PROGNAME
    raising
      ZAPLINK_CX .
protected section.

  data ABAP_SOURCECODE type TT_ABAPRAWSOURCE .
  data CONNECTORS_SOURCECODE type TT_ABAPRAWSOURCE .
  data MASTER_SOURCECODE type TT_ABAPRAWSOURCE .

  class-methods REMOVE_CHECKPOINTS
    changing
      !CODE type TT_ABAPRAWSOURCE .
private section.
ENDCLASS.



CLASS ZAPLINK_STANDALONE_GENERATOR IMPLEMENTATION.


  method ADD_CONNECTORS.
  DATA _line LIKE LINE OF connectors_sourcecode.
  DATA _comp TYPE REF TO zaplink_component.
  DATA _classname TYPE zaplink_component=>td_connclass.
  DATA o_conn TYPE REF TO zaplink_connector.
  DATA _classes TYPE SORTED TABLE OF zaplink_component=>td_connclass WITH UNIQUE KEY table_line.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF zaplink_connectors=>supported_types.

  REFRESH connectors_sourcecode.
*  SORT t_connectors BY classname.
*  DELETE ADJACENT DUPLICATES FROM t_connectors COMPARING classname.
  add_object( 'ZAPLINK_CONNECTORS' ).                       "#EC NOTEXT
  _line = 'ZAPLINK_CONNECTORS=>clear_connectors( ).'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT : remove ABAP class connector to add only local ones
  _line = 'DATA _cx_90335e5d44316adf4805a732f type ref to ZAPLINK_CX_COMPONENT.'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT
  _line = 'TRY.'. APPEND _line TO connectors_sourcecode.    "#EC NOTEXT
  LOOP AT zaplink_connectors=>supported_types ASSIGNING <c>.
    CREATE OBJECT _comp.
    _comp->set_type( <c>-type ).
    o_conn = _comp->connector( ).
    _classname = zaplink_connectors=>get_classname( o_conn ).
    CHECK NOT _classname IS INITIAL.
    READ TABLE _classes TRANSPORTING NO FIELDS
         WITH TABLE KEY table_line = _classname.
    IF sy-subrc <> 0.
      INSERT _classname INTO TABLE _classes.
      add_object( _classname ).
      CLEAR _line.
      CONCATENATE 'ZAPLINK_CONNECTORS=>ADD_LOCAL_CONNECTOR(''' _classname ''').' INTO _line. "#EC NOTEXT
      APPEND _line TO connectors_sourcecode.
    ENDIF.
  ENDLOOP.
  _line = 'CATCH ZAPLINK_CX_COMPONENT INTO _cx_90335e5d44316adf4805a732f.'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT
  _line = '_cx_90335e5d44316adf4805a732f->write( ).'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT
  _line = 'BREAK-POINT.'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT
  _line = 'RAISE EXCEPTION _cx_90335e5d44316adf4805a732f.'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT
  _line = 'ENDTRY.'. APPEND _line TO connectors_sourcecode. "#EC NOTEXT
  endmethod.


  method CONSTRUCTOR.
  DATA sourcecode  TYPE tt_abaprawsource.
  super->constructor( ).

  master_program = master.
  IF NOT master_program IS INITIAL.
    master_sourcecode = sourcecode = zaplink_program_4dep_analyser=>get_prog_rawsource( master_program ). " Issue 110
    add_source( code = sourcecode   origin = master_program ).
  ENDIF.
  endmethod.


  method GENERATE.
  DATA _tmp_code TYPE ts_source.
  DATA _line     LIKE LINE OF _tmp_code-def.
  DATA _tmp      TYPE tt_abaprawsource.
  DATA _pos      TYPE sy-tabix.
  DATA _order    TYPE tt_order.
  DATA d_report  TYPE programm.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF typegroups,
    <i> LIKE LINE OF includes,
    <c> LIKE LINE OF classes,
    <o> LIKE LINE OF order.
  DATA t_tokens TYPE STANDARD TABLE OF stokes.
  DATA t_statements TYPE STANDARD TABLE OF sstmnt.
  DATA t_kw      TYPE STANDARD TABLE OF char255.
  DATA s_kw      LIKE LINE OF t_kw.
  FIELD-SYMBOLS:
    <t_deb> LIKE LINE OF t_tokens,
    <st> LIKE LINE OF t_statements.

  _clean_up( ).
  resolve( ).
  LOOP AT typegroups ASSIGNING <t>.   IF _line IS INITIAL. CONCATENATE 'TYPE-POOLS: ' <t>-name INTO _line. ELSE. CONCATENATE _line ', ' <t>-name INTO _line. ENDIF.   ENDLOOP.
  IF NOT _line IS INITIAL. CONCATENATE _line '.' INTO _line. APPEND _line TO code. ENDIF.

* DEFERRED declaration first
  _order = order.
  SORT _order BY name type.
  DELETE ADJACENT DUPLICATES FROM _order COMPARING name.
  LOOP AT _order ASSIGNING <o>.
    add_deferred_code( EXPORTING     type = <o>-type
                                     name = <o>-name
                        CHANGING abapcode = code ).
  ENDLOOP.

* Prepare objects
  LOOP AT order ASSIGNING <o>.
    IF NOT <o>-defered IS INITIAL.
* Insert defered call
* use less because already done before
      add_deferred_code( EXPORTING     type = <o>-type
                                       name = <o>-name
                          CHANGING abapcode = _tmp_code-def ).
    ELSE.
      READ TABLE classes ASSIGNING <c>
           WITH TABLE KEY name = <o>-name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      APPEND LINES OF <c>-def TO _tmp_code-def.
      APPEND LINES OF <c>-macro TO _tmp_code-macro.
      APPEND LINES OF <c>-impl TO _tmp_code-impl.
** Put macro into implementation
*      IF <c>-macro IS INITIAL.
*        APPEND LINES OF <c>-impl TO _tmp_code-impl.
*      ELSE.
*        _tmp = <c>-impl.
*        LOOP AT _tmp TRANSPORTING NO FIELDS
*             WHERE table_line CP '*CLASS * IMPLEMENTATION*.*'.
*          _pos = sy-tabix + 1.
*          EXIT.
*        ENDLOOP.
*        IF sy-subrc <> 0.
*          APPEND LINES OF <c>-macro TO _tmp_code-macro.
*          APPEND LINES OF <c>-impl TO _tmp_code-impl.
*        ELSE.
*          INSERT LINES OF <c>-macro INTO _tmp INDEX _pos.
*          APPEND LINES OF _tmp TO _tmp_code-impl.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDLOOP.

* Objects definition second because might be used by includes
  APPEND LINES OF _tmp_code-def TO code.

* Include third
  s_kw = 'PROGRAM'.    APPEND s_kw TO t_kw.
  s_kw = 'REPORT'.     APPEND s_kw TO t_kw.
  LOOP AT includes ASSIGNING <i>.
    d_report = <i>.   CLEAR _tmp.
    READ REPORT d_report INTO _tmp.
    CHECK sy-subrc = 0.
    SCAN ABAP-SOURCE _tmp TOKENS INTO t_tokens
                      STATEMENTS INTO t_statements
                        KEYWORDS FROM t_kw.
    zaplink_tools=>inverse_table_order( CHANGING my_table = t_statements ).
    LOOP AT t_statements ASSIGNING <st>.
      READ TABLE t_tokens ASSIGNING <t_deb> INDEX <st>-from.    CHECK sy-subrc = 0.
      DELETE _tmp FROM <t_deb>-row TO <st>-trow.
    ENDLOOP.
*    APPEND LINES OF _tmp TO _tmp_code-macro.
    APPEND LINES OF _tmp TO code.
  ENDLOOP.

* remaining of the object then
  APPEND LINES OF _tmp_code-macro TO code.
  APPEND LINES OF _tmp_code-impl TO code.

* Insert into Master source code
  abap_sourcecode = master_sourcecode.
  IF NOT abap_sourcecode IS INITIAL.
    REFRESH t_kw.
*  s_kw = 'INITIALIZATION'.        APPEND s_kw TO t_kw.
    s_kw = 'START-OF-SELECTION'.    APPEND s_kw TO t_kw.
    s_kw = 'REPORT'.                APPEND s_kw TO t_kw.
    SCAN ABAP-SOURCE abap_sourcecode TOKENS INTO t_tokens
                                 STATEMENTS INTO t_statements
                                   KEYWORDS FROM t_kw.
    IF NOT connectors_sourcecode IS INITIAL.
      READ TABLE t_tokens ASSIGNING <t_deb> WITH KEY str = 'START-OF-SELECTION'.
      ASSERT sy-subrc = 0.  _pos = sy-tabix.
      READ TABLE t_statements ASSIGNING <st> WITH KEY FROM = _pos.
      ASSERT sy-subrc = 0.
      READ TABLE t_tokens ASSIGNING <t_deb> INDEX <st>-to.
      ASSERT sy-subrc = 0.
      _pos = <t_deb>-row + 1.
      INSERT LINES OF connectors_sourcecode INTO abap_sourcecode INDEX _pos.
    ENDIF.

    READ TABLE t_tokens ASSIGNING <t_deb> WITH KEY str = 'REPORT'.
    ASSERT sy-subrc = 0.  _pos = sy-tabix.
    READ TABLE t_statements ASSIGNING <st> WITH KEY FROM = _pos.
    ASSERT sy-subrc = 0.
    READ TABLE t_tokens ASSIGNING <t_deb> INDEX <st>-to.
    ASSERT sy-subrc = 0.
    _pos = <t_deb>-row + 1.
    INSERT LINES OF code INTO abap_sourcecode INDEX _pos.
    code = abap_sourcecode.
  ENDIF.

*  code = clean_code( code ).
  remove_checkpoints( CHANGING code = code ).           " Issue 107
  zaplink_tools=>remove_comment_on_source( CHANGING table = code ).
  abap_sourcecode = code.
  endmethod.


  method REMOVE_CHECKPOINTS.
  DATA c_log        TYPE string VALUE 'LOG-POINT'.
  DATA c_assert     TYPE string VALUE 'ASSERT'.
  DATA c_break      TYPE string VALUE 'BREAK-POINT'.
  DATA t_tokens     TYPE STANDARD TABLE OF stokes.
  DATA t_statements TYPE STANDARD TABLE OF sstmnt.
  DATA t_kw         TYPE STANDARD TABLE OF char255.
  DATA s_kw         LIKE LINE OF t_kw.
  DATA d_line       TYPE i.
  DATA d_start      TYPE i.
  DATA d_end        TYPE i.
  DATA d_sdeb       TYPE string.
  DATA d_sfin       TYPE string.
  FIELD-SYMBOLS:
    <t_deb> LIKE LINE OF t_tokens,
    <t_fin> LIKE LINE OF t_tokens,
    <l>  LIKE LINE OF code,
    <st> LIKE LINE OF t_statements.

* Include third
  s_kw = c_assert.      APPEND s_kw TO t_kw.
  s_kw = c_log.         APPEND s_kw TO t_kw.
  s_kw = c_break.       APPEND s_kw TO t_kw.
  SCAN ABAP-SOURCE code TOKENS INTO t_tokens
                    STATEMENTS INTO t_statements
                      KEYWORDS FROM t_kw.
  zaplink_tools=>inverse_table_order( CHANGING my_table = t_statements ).
  LOOP AT t_statements ASSIGNING <st>.
    READ TABLE t_tokens ASSIGNING <t_deb> INDEX <st>-from.    CHECK sy-subrc = 0.
    CASE <t_deb>-str.
      WHEN c_log.
      WHEN OTHERS.
        d_line = <st>-from + 1.   CHECK d_line <= <st>-to.
        READ TABLE t_tokens ASSIGNING <t_deb> INDEX d_line.    CHECK sy-subrc = 0.
        IF <t_deb>-str <> 'ID'.   CONTINUE.   ENDIF.
    ENDCASE.

    zaplink_tools=>comment_statement( EXPORTING statement = <st>
                                                   tokens = t_tokens
                                            CHANGING code = code ).
  ENDLOOP.
  endmethod.


  method SAVE_REPORT.
  DATA t_source    TYPE tt_abaprawsource.
  DATA _prog       TYPE progdir.
  DATA _oname      TYPE e071-obj_name.

  IF code IS INITIAL.
    t_source = abap_sourcecode.
  ELSE.
    t_source = code.
    zaplink_tools=>remove_comment_on_source( CHANGING table = t_source ).
  ENDIF.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object                         = report
      object_class                   = 'ABAP'
      mode                           = 'I'    " Insert
*         GLOBAL_LOCK                    = ' '
      devclass                       = package
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

  _oname = report.
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

  _prog-name = report.
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
*  _prog-state = 'A'.
*  MODIFY progdir FROM _prog.
  zaplink_program_4dep_analyser=>set_prog_rawsource( program = report
                                                         raw = t_source ).      " Issue 110
  set_textpool( report ).
  endmethod.
ENDCLASS.
