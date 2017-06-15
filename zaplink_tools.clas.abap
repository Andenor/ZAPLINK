class ZAPLINK_TOOLS definition
  public
  create public .

public section.
  type-pools ABAP .
  type-pools SEDI .
  type-pools SEWOR .
  type-pools SMODI .

  interfaces ZAPLINK_DATATYPES .

  aliases COMP_NODENAME
    for ZAPLINK_DATATYPES~COMP_NODENAME .
  aliases EXISTS
    for ZAPLINK_DATATYPES~EXISTS .
  aliases EXT_SEP
    for ZAPLINK_DATATYPES~EXT_SEP .
  aliases FILE_EXT
    for ZAPLINK_DATATYPES~FILE_EXT .
  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TEXTPOOL_KINDS
    for ZAPLINK_DATATYPES~TEXTPOOL_KINDS .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_ACTION
    for ZAPLINK_DATATYPES~TD_ACTION .
  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
  aliases TD_CLASSNAME
    for ZAPLINK_DATATYPES~TD_CLASSNAME .
  aliases TD_COMPEXISTS
    for ZAPLINK_DATATYPES~TD_COMPEXISTS .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .
  aliases TD_COMPTEXT
    for ZAPLINK_DATATYPES~TD_COMPTEXT .
  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_DATATYPES~TD_CONNCLASS .
  aliases TD_CONNEXISTS
    for ZAPLINK_DATATYPES~TD_CONNEXISTS .
  aliases TD_CONNUUID
    for ZAPLINK_DATATYPES~TD_CONNUUID .
  aliases TD_CONNVER
    for ZAPLINK_DATATYPES~TD_CONNVER .
  aliases TD_CONTNAME
    for ZAPLINK_DATATYPES~TD_CONTNAME .
  aliases TD_CONTVER
    for ZAPLINK_DATATYPES~TD_CONTVER .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TD_EXISTS
    for ZAPLINK_DATATYPES~TD_EXISTS .
  aliases TD_FILENAME
    for ZAPLINK_DATATYPES~TD_FILENAME .
  aliases TD_FILETYPE
    for ZAPLINK_DATATYPES~TD_FILETYPE .
  aliases TD_LANG
    for ZAPLINK_DATATYPES~TD_LANG .
  aliases TD_LIGHT
    for ZAPLINK_DATATYPES~TD_LIGHT .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .
  aliases TD_ORDERKIND
    for ZAPLINK_DATATYPES~TD_ORDERKIND .
  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TD_SUBSTITUTIONKIND
    for ZAPLINK_DATATYPES~TD_SUBSTITUTIONKIND .
  aliases TD_TRANSPORT_REQUEST
    for ZAPLINK_DATATYPES~TD_TRANSPORT_REQUEST .
  aliases TD_TXTP_ID
    for ZAPLINK_DATATYPES~TD_TXTP_ID .
  aliases TD_TXTP_KIND
    for ZAPLINK_DATATYPES~TD_TXTP_KIND .
  aliases TD_TXTP_LEN
    for ZAPLINK_DATATYPES~TD_TXTP_LEN .
  aliases TD_TXTP_TEXT
    for ZAPLINK_DATATYPES~TD_TXTP_TEXT .
  aliases TD_TYPEORDER
    for ZAPLINK_DATATYPES~TD_TYPEORDER .
  aliases TD_TYPE_AS_TEXT
    for ZAPLINK_DATATYPES~TD_TYPE_AS_TEXT .
  aliases TD_WITH_SUBCOMP
    for ZAPLINK_DATATYPES~TD_WITH_SUBCOMP .
  aliases TO_XML
    for ZAPLINK_DATATYPES~TO_XML .
  aliases TR_AUTHOR
    for ZAPLINK_DATATYPES~TR_AUTHOR .
  aliases TR_COMPNAME
    for ZAPLINK_DATATYPES~TR_COMPNAME .
  aliases TR_COMPTYPE
    for ZAPLINK_DATATYPES~TR_COMPTYPE .
  aliases TR_DLVUNIT
    for ZAPLINK_DATATYPES~TR_DLVUNIT .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TR_SRCSYSTEM
    for ZAPLINK_DATATYPES~TR_SRCSYSTEM .
  aliases TR_TR
    for ZAPLINK_DATATYPES~TR_TR .
  aliases TS_BASE_ATTRIBUTS
    for ZAPLINK_DATATYPES~TS_BASE_ATTRIBUTS .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TS_COMPONENT
    for ZAPLINK_DATATYPES~TS_COMPONENT .
  aliases TS_CONNDATA
    for ZAPLINK_DATATYPES~TS_CONNDATA .
  aliases TS_CONN_CLASS
    for ZAPLINK_DATATYPES~TS_CONN_CLASS .
  aliases TS_CONN_DEF
    for ZAPLINK_DATATYPES~TS_CONN_DEF .
  aliases TS_CONTDATA
    for ZAPLINK_DATATYPES~TS_CONTDATA .
  aliases TS_DIRECTORY
    for ZAPLINK_DATATYPES~TS_DIRECTORY .
  aliases TS_DOC
    for ZAPLINK_DATATYPES~TS_DOC .
  aliases TS_HEAD
    for ZAPLINK_DATATYPES~TS_HEAD .
  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .
  aliases TS_STATMENT
    for ZAPLINK_DATATYPES~TS_STATMENT .
  aliases TS_TXTP_TEXT
    for ZAPLINK_DATATYPES~TS_TXTP_TEXT .
  aliases TS_TXTP_TEXTPOOL
    for ZAPLINK_DATATYPES~TS_TXTP_TEXTPOOL .
  aliases TS_TYPE
    for ZAPLINK_DATATYPES~TS_TYPE .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_COMPKEYS
    for ZAPLINK_DATATYPES~TT_COMPKEYS .
  aliases TT_CONN_CLASSES
    for ZAPLINK_DATATYPES~TT_CONN_CLASSES .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_GENSETC
    for ZAPLINK_DATATYPES~TT_GENSETC .
  aliases TT_RAWTEXT
    for ZAPLINK_DATATYPES~TT_RAWTEXT .
  aliases TT_TOKENS
    for ZAPLINK_DATATYPES~TT_TOKENS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
  aliases TT_TXTP_TEXTS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTS .
  aliases TT_TYPES
    for ZAPLINK_DATATYPES~TT_TYPES .

  class-data IMPORT_ALLOWED type ABAP_BOOL read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods CALCULATE_MD5_HASH
    importing
      !STRING type STRING
    returning
      value(RESULT) type TD_CHECKSUM .
  class-methods CLEAN_ABAP_STRING
    importing
      !DATA type STRING
    returning
      value(RESULT) type STRING .
  class-methods ADD_COMP_TO_WORKING_AREA
    importing
      !NAME type TD_COMPNAME
      !TYPE type TD_COMPTYPE
    raising
      ZAPLINK_CX .
  class-methods REMOVE_COMP_FROM_WORKING_AREA
    importing
      !NAME type TD_COMPNAME
      !TYPE type TD_COMPTYPE .
  class-methods IS_WORKING_AREABLE
    importing
      !TYPE type TD_COMPTYPE
    returning
      value(RESULT) type ABAP_BOOL .
  class-methods CONDENSE_ABAP_SOURCE
    changing
      !TABLE type STANDARD TABLE .
  class-methods CONV_ABAP_LINE
    importing
      !SOURCE type STRING
    returning
      value(TARGET) type STRING .
  class-methods CONV_TABLE_2UPCASE
    changing
      !TABLE type STANDARD TABLE .
  class-methods EXTEND_PACKAGES
    importing
      !PACKAGES type TR_PACKAGES
    returning
      value(RESULT) type TR_PACKAGES .
  class-methods GET_CLAS_NAME
    importing
      !OBJECT type ref to OBJECT
    returning
      value(RESULT) type TD_CLASSNAME .
  class-methods GET_KEYS_FROM_PACKAGE
    importing
      !PACKAGES type TR_PACKAGES
      !EXT type ABAP_BOOL default ABAP_TRUE
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods GET_KEYS_FROM_TADIR
    importing
      !SRCSYSTEM type TR_SRCSYSTEM optional
      !TYPE type TR_COMPTYPE optional
      !NAME type TR_COMPNAME optional
      !PACKAGES type TR_PACKAGES optional
      !EXT type ABAP_BOOL default ABAP_TRUE
      !AUTHOR type TR_AUTHOR optional
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods GET_KEYS_FROM_TR
    importing
      !TR type TR_TR
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods INVERSE_TABLE_ORDER
    changing
      !MY_TABLE type STANDARD TABLE .
  class-methods PRETTY_PRINTER
    importing
      !INDENT type ABAP_BOOL optional
    changing
      !TABLE type STANDARD TABLE .
  class-methods REMOVE_COMMENT_ON_SOURCE
    changing
      !TABLE type STANDARD TABLE .
  class-methods TABLE_2_STRING
    importing
      !SOURCE type STANDARD TABLE
    returning
      value(RESULT) type STRING .
  class-methods UNZIP_STRING
    importing
      !DATA type XSTRING
    returning
      value(RESULT) type STRING .
  class-methods ZIP_STRING
    importing
      !DATA type STRING
    returning
      value(RESULT) type XSTRING .
  class-methods CHECK_VALID_PROGLIST
    importing
      !LIST type TT_GENSETC
    returning
      value(RESULT) type TT_GENSETC .
  class-methods CONV_COMPKEY_2_PROGLIST
    importing
      !KEYS type TT_COMPKEYS
    returning
      value(RESULT) type TT_GENSETC .
  class-methods CONV_PROGLIST_2_COMPKEY
    importing
      !LIST type TT_GENSETC
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods COMMENT_STATEMENT
    importing
      !STATEMENT type TS_STATMENT
      !TOKENS type TT_TOKENS
      !COMMENT type STRING optional
    changing
      !CODE type TT_ABAPRAWSOURCE .
protected section.

  types:
    tt_e071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY .
  types:
    tt_pack TYPE STANDARD TABLE OF tdevc-devclass WITH DEFAULT KEY .

  constants:
    gc_gen_types(8) TYPE c value '1CFJKMST' ##NO_TEXT.

  class-methods CONV_LIMU_2_R3TR
    importing
      !E071 type TT_E071
    returning
      value(RESULT) type TT_COMPKEYS .
  class-methods EXTEND_PACKAGE
    importing
      !PACKAGES type TT_PACK
    returning
      value(RESULT) type TT_PACK .
  class-methods GET_SUB_PACKAGES
    importing
      !PACKAGES type TR_PACKAGES
    returning
      value(RESULT) type TR_PACKAGES .
private section.
ENDCLASS.



CLASS ZAPLINK_TOOLS IMPLEMENTATION.


  method ADD_COMP_TO_WORKING_AREA.
    DATA wa_flag   TYPE rs38l-head.
    DATA o_cx      TYPE REF TO zaplink_cx.
    DATA f_is_wi   TYPE abap_bool.
    DATA _name     TYPE e071-obj_name.

    _name = name.
    IF is_working_areable( type ) = abap_true.
      CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
        EXPORTING
          object              = type
          obj_name            = _name
        IMPORTING
          object_is_work_item = f_is_wi.
      IF f_is_wi = abap_false.   " should be the case
        CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
          EXPORTING
            object            = type
            obj_name          = _name
          IMPORTING
            object_inserted   = wa_flag
          EXCEPTIONS
            wrong_object_name = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
          o_cx ?= zaplink_cx=>create_from_mf_cx( funcname = 'RS_INSERT_INTO_WORKING_AREA'
                                                    subrc = sy-subrc
                                                classname = 'ZAPLINK_CX' ).
          RAISE EXCEPTION o_cx.
        ENDIF.
      ENDIF.
    ELSE.   " Security Remove old eroneous entries Issue 137
      CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
        EXPORTING
          object              = type
          obj_name            = _name
        IMPORTING
          object_is_work_item = f_is_wi.
      IF f_is_wi = abap_true.   " should only be the case of Issue 137
        remove_comp_from_working_area( type = type
                                       name = name ).
      ENDIF.
    ENDIF.
  endmethod.


  method CALCULATE_MD5_HASH.
    DATA _len            TYPE i.
    DATA _md5            TYPE md5_fields-hash.

    _len = strlen( string ).
    CALL FUNCTION 'MD5_CALCULATE_HASH_FOR_CHAR'
      EXPORTING
        data           = string
        length         = _len
*       VERSION        = 1
      IMPORTING
        hash           = _md5
*   TABLES
*       DATA_TAB       =
      EXCEPTIONS
        no_data        = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc = 0.    result = _md5.    ENDIF.
  endmethod.


  method CHECK_VALID_PROGLIST.
    DATA gc_infoline_on TYPE oax.
    DATA it_gensetc_raw TYPE tt_gensetc.
    DATA t_trdir TYPE HASHED TABLE OF trdir WITH UNIQUE KEY name.
    FIELD-SYMBOLS:
      <dir> LIKE LINE OF t_trdir,
      <obj> LIKE LINE OF it_gensetc_raw.

    it_gensetc_raw = list.
* determine the program names on the basis of the TADIR keys *
    PERFORM  tadir_2_trdir IN PROGRAM rsparagenlod
             CHANGING it_gensetc_raw.
* When type doesn't correspond to a program (or equivalent) name is filled with previous value
    DELETE ADJACENT DUPLICATES FROM it_gensetc_raw COMPARING name.
* delete non existing or invalid objects from generation set
    IF lines( it_gensetc_raw ) > 1000.
      PERFORM trdir_check IN PROGRAM rsparagenlod
        USING    gc_infoline_on
                 it_gensetc_raw
      CHANGING result.
    ELSE.
      SELECT * INTO TABLE t_trdir
        FROM trdir
        FOR ALL ENTRIES IN it_gensetc_raw
        WHERE name = it_gensetc_raw-name
        %_HINTS ORACLE '&max_blocking_factor 50&'.
      LOOP AT it_gensetc_raw ASSIGNING <obj>.
        READ TABLE t_trdir ASSIGNING <dir>
             WITH TABLE KEY name = <obj>-name.
        CHECK sy-subrc = 0.
        CHECK <dir>-subc CA gc_gen_types.
        APPEND <obj> TO result.
      ENDLOOP.
    ENDIF.

    SORT result BY pgmid object objname.   DELETE ADJACENT DUPLICATES FROM result COMPARING pgmid object objname.
  endmethod.


  method CLASS_CONSTRUCTOR.
    DATA s_mandant  TYPE t000.
    SELECT SINGLE *
      INTO s_mandant
      FROM t000
      WHERE mandt EQ sy-mandt.
    ASSERT sy-subrc = 0.
    IF s_mandant-cccategory <> 'P'    " Productive
      AND s_mandant-ccnocliind CO ' 1'.
      import_allowed = abap_true.
    ENDIF.
  endmethod.


  method CLEAN_ABAP_STRING.
    CHECK NOT data IS INITIAL.    result = data.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN result WITH ``.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN result WITH ``.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN result WITH ``.
  endmethod.


  method COMMENT_STATEMENT.
    DATA d_line       TYPE i.
    DATA d_start      TYPE i.
    DATA d_end        TYPE i.
    DATA d_sdeb       TYPE string.
    DATA d_sfin       TYPE string.
    DATA d_removed    TYPE string.
    DATA f_st_full_l  TYPE abap_bool.     " statement is alone on his lines
    FIELD-SYMBOLS:
      <t_bdeb> LIKE LINE OF tokens,
      <t_afin> LIKE LINE OF tokens,
      <t_deb>  LIKE LINE OF tokens,
      <t_fin>  LIKE LINE OF tokens,
      <l>      LIKE LINE OF code.

    READ TABLE tokens ASSIGNING <t_deb> INDEX statement-from.    ASSERT sy-subrc = 0.
    READ TABLE tokens ASSIGNING <t_fin> INDEX statement-to.      ASSERT sy-subrc = 0.

    f_st_full_l = abap_true.
    d_start = statement-from - 1.     READ TABLE tokens ASSIGNING <t_bdeb> INDEX d_start.
    IF <t_bdeb> IS ASSIGNED.    IF <t_bdeb>-row >= <t_deb>-row.   CLEAR f_st_full_l.    ENDIF.    ENDIF.
    d_end = statement-to + 1.         READ TABLE tokens ASSIGNING <t_afin> INDEX d_end.
    IF <t_afin> IS ASSIGNED.    IF <t_afin>-row <= <t_fin>-row.   CLEAR f_st_full_l.    ENDIF.    ENDIF.

    IF f_st_full_l = abap_true.   " simple case statement is alone
      LOOP AT code ASSIGNING <l> FROM <t_deb>-row TO <t_fin>-row.
        CONCATENATE '*' <l> '"' comment INTO <l>.
      ENDLOOP.
    ELSE.     " Statement share lines with other statements
      d_start = <t_deb>-row + 1.    d_end = <t_fin>-row - 1.
      IF d_start <= d_end.    " lines for this statements only
        LOOP AT code ASSIGNING <l> FROM d_start TO d_end.
          CONCATENATE '*' <l> '"' comment INTO <l>.
        ENDLOOP.
      ENDIF.

      READ TABLE code ASSIGNING <l> INDEX <t_fin>-row.          ASSERT sy-subrc = 0.
      IF <t_deb>-row = <t_fin>-row.     " on the same line

        CLEAR: d_sdeb, d_sfin.
        d_end = <t_fin>-col + strlen( <t_fin>-str ).
        d_removed = <l>+<t_deb>-col(d_end).
        d_sdeb = <l>(<t_deb>-col).    d_sfin = <l>+d_end.

*      CLEAR <l>+<t_deb>-col(<t_fin>-row).
        CONCATENATE d_sdeb d_sfin '"' comment d_removed INTO <l>.
*    CONDENSE <l>.

      ELSE.
* Last line
        CLEAR: d_sdeb, d_sfin.
        d_end = <t_fin>-col + strlen( <t_fin>-str ).
        d_removed = <l>(d_end).
        d_sfin = <l>+d_end.
*      CLEAR <l>(<t_fin>-row).
        CONCATENATE d_sfin '"' comment d_removed INTO <l>.
*    CONDENSE <l>.

* First line
        READ TABLE code ASSIGNING <l> INDEX <t_deb>-row.         ASSERT sy-subrc = 0.
        CLEAR: d_sdeb, d_sfin.
        d_sdeb = <l>(<t_deb>-col).
        d_removed = <l>+<t_deb>-col.
*      CLEAR <l>(<t_fin>-row).
        <l> = d_sdeb.
        CONCATENATE d_sdeb '"' comment d_removed INTO <l>.
*    CONDENSE <l>.
      ENDIF.

    ENDIF.
  endmethod.


  method CONDENSE_ABAP_SOURCE.
    FIELD-SYMBOLS <l> TYPE any.
    LOOP AT table ASSIGNING <l>.
      IF <l> IS INITIAL. DELETE table. CONTINUE. ENDIF.  " Condense
      IF <l>(1) = '*'.   DELETE table. CONTINUE. ENDIF.  " Condense
      <l> = conv_abap_line( <l> ).   CONDENSE <l>.
      IF <l> IS INITIAL. CLEAR <l>. CONTINUE. ENDIF.          " Issue 131
      IF <l>(1) = '"'.   DELETE table. CONTINUE. ENDIF.  " commented line like :     " comment
    ENDLOOP.
  endmethod.


  method CONV_ABAP_LINE.
    target = source.
* Convert also constants '#document' => '#DOCUMENT'. so removed
*  TRANSLATE target TO UPPER CASE.  "#EC SYNTCHAR
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN target WITH ` `.
  endmethod.


  method CONV_COMPKEY_2_PROGLIST.
    DATA s_entry      LIKE LINE OF result.
    FIELD-SYMBOLS <k> LIKE LINE OF keys.

    LOOP AT keys ASSIGNING <k>.
      s_entry-pgmid = 'R3TR'.     s_entry-object = <k>-type.      s_entry-objname = <k>-name.   APPEND s_entry TO result.
    ENDLOOP.
    SORT result. DELETE ADJACENT DUPLICATES FROM result.
  endmethod.


  method CONV_LIMU_2_R3TR.
* From TRINT_OBJECT_TABLE
* HINT TRINT_OLE_SORT_COMPRESS
* HINT TR_SORT_OBJECT_AND_KEY_LIST
    DATA _objlist LIKE LINE OF result.
    DATA w_tadir TYPE tadir.
    FIELD-SYMBOLS <limu> LIKE LINE OF e071.

    LOOP AT e071 ASSIGNING <limu>.
      CLEAR: _objlist.
      IF <limu>-pgmid = 'R3TR'.
        _objlist-type = <limu>-object.
        _objlist-name = <limu>-obj_name.
      ELSE.
        CALL FUNCTION 'TR_CHECK_TYPE'
          EXPORTING
            wi_e071  = <limu>
*           IV_TRANSLATE_OBJNAME                 = ' '
          IMPORTING
*           PE_RESULT                            =
*           WE_E071  =
*           WE_LOCK_KEY                          =
*           WE_COMPLETE_ONLY                     =
*           WE_CATEGORY                          =
*           WE_CLI_DEP                           =
*           WE_TABLE_TYP                         =
*           WE_TABLE_CONTFLAG                    =
*           PE_NAME_TOO_LONG                     =
*           EV_NO_OBJECT_NAME                    =
*           EV_R3TRTEXT_SYNTAXERROR              =
*           EV_LANGTYP                           =
*           EV_LANGTYPA                          =
*           EV_LOGO_OBJ                          =
*           EV_CURR_SETTING                      =
*           EV_NAME_TOO_LONG_FOR_OLD             =
*           EV_NAME_WITH_NONSTANDARD_CHARS       =
*           EV_NAMESPACE                         =
*           EV_INVALID_PREFIX_SYNTAX             =
*           EV_PREFIX_NOT_ALLOWED                =
*           EV_CASE_SENSITIVE                    =
*           EV_OBJLEN                            =
*           EV_INVALID_LANGUAGE_FIELD            =
            we_tadir = w_tadir.
        _objlist-type = w_tadir-object.
        _objlist-name = w_tadir-obj_name.
      ENDIF.
      IF _objlist IS NOT INITIAL. APPEND _objlist TO result. ENDIF.
    ENDLOOP.

    SORT result BY type name.
    DELETE ADJACENT DUPLICATES FROM result COMPARING type name.
  endmethod.


  method CONV_PROGLIST_2_COMPKEY.
    DATA s_entry      LIKE LINE OF result.
    FIELD-SYMBOLS <k> LIKE LINE OF list.

    LOOP AT list ASSIGNING <k>.
      CHECK NOT <k>-name IS INITIAL.
      s_entry-type = 'PROG'.      s_entry-name = <k>-name.   APPEND s_entry TO result.
    ENDLOOP.
    SORT result. DELETE ADJACENT DUPLICATES FROM result.
  endmethod.


  method CONV_TABLE_2UPCASE.
    FIELD-SYMBOLS <l> TYPE any.
    LOOP AT table ASSIGNING <l>.    TRANSLATE <l> TO UPPER CASE.  ENDLOOP. "#EC SYNTCHAR
  endmethod.


  method EXTEND_PACKAGE.
    DATA r_exclu TYPE tr_packages.
    DATA t_level TYPE tt_pack.

    t_level = result = packages .
    WHILE NOT t_level[] IS INITIAL.
      SORT t_level.
      DELETE ADJACENT DUPLICATES FROM t_level.

      SELECT devclass
          FROM tdevc
          INTO TABLE t_level
             FOR ALL ENTRIES IN t_level
          WHERE parentcl = t_level-table_line.

      APPEND LINES OF t_level TO result.
    ENDWHILE.

    SORT result.
    DELETE ADJACENT DUPLICATES FROM result.
  endmethod.


  method EXTEND_PACKAGES.
    DATA r_exclu TYPE tr_packages.
    DATA t_level TYPE tr_packages.
    DATA s_pack LIKE LINE OF result.

    CHECK NOT packages IS INITIAL.
    r_exclu = packages.   DELETE r_exclu WHERE sign <> 'E'. " Keep only Devcclass to remove

* Package selection
    result = get_sub_packages( packages ).

    IF NOT r_exclu IS INITIAL.
      s_pack-sign = 'I'.   MODIFY r_exclu FROM s_pack TRANSPORTING sign WHERE sign <> s_pack-sign.
      r_exclu = get_sub_packages( r_exclu ).
      DELETE result WHERE low IN r_exclu.
    ENDIF.

    SORT result.    DELETE ADJACENT DUPLICATES FROM result COMPARING low.
    s_pack-sign = 'I'.  s_pack-option = 'EQ'.   MODIFY result FROM s_pack TRANSPORTING sign option WHERE NOT low IS INITIAL.
  endmethod.


  method GET_CLAS_NAME.
    DATA o_desc TYPE REF TO cl_abap_objectdescr.
    DATA d_pos TYPE i.
    DATA d_str  TYPE string.

    CHECK object IS BOUND.
* Get class name
    o_desc ?= cl_abap_objectdescr=>describe_by_object_ref( object ).
    d_str = o_desc->absolute_name.
    FIND ALL OCCURRENCES OF '=' IN d_str MATCH OFFSET d_pos.
    IF NOT d_pos IS INITIAL.
      ADD 1 TO d_pos.
      IF strlen( d_str ) > d_pos.
        d_str = d_str+d_pos.
      ENDIF.
    ENDIF.

    result = d_str.
  endmethod.


  method GET_KEYS_FROM_PACKAGE.
    result = get_keys_from_tadir( packages = packages   ext = ext ).
  endmethod.


  method GET_KEYS_FROM_TADIR.
    TYPES tt_pack TYPE STANDARD TABLE OF tdevc-devclass.
    DATA r_exclu TYPE tr_packages.
    DATA t_devc  TYPE tt_pack.
    DATA t_exclu TYPE tt_pack.
    FIELD-SYMBOLS: <p> LIKE LINE OF t_exclu.

    IF packages IS INITIAL.
      CHECK srcsystem IS NOT INITIAL OR type IS NOT INITIAL OR name IS NOT INITIAL OR author IS NOT INITIAL. " to prevent extracting all tadir
      SELECT object obj_name
        INTO TABLE result
        FROM tadir
        WHERE srcsystem IN srcsystem
          AND object IN type
          AND obj_name IN name
          AND author IN author.
      EXIT.
    ENDIF.

    r_exclu = packages.
    DELETE r_exclu WHERE sign <> 'E'. " Keep only Devcclass to remove

* Package selection
    SELECT devclass
        FROM tdevc
        INTO TABLE t_devc
        WHERE devclass IN packages.

    IF NOT ext IS INITIAL.
      t_devc = extend_package( t_devc ).
    ENDIF.

* Package selection
    IF NOT r_exclu IS INITIAL.
      SELECT devclass
          FROM tdevc
          INTO TABLE t_exclu
          WHERE NOT devclass IN r_exclu.

      t_exclu = extend_package( t_exclu ).
      LOOP AT t_exclu ASSIGNING <p>.
        DELETE TABLE t_devc FROM <p>.
      ENDLOOP.
    ENDIF.

    IF NOT t_devc IS INITIAL.
      SELECT object obj_name
        INTO TABLE result
        FROM tadir
          FOR ALL ENTRIES IN t_devc
        WHERE devclass = t_devc-table_line
          AND srcsystem IN srcsystem
          AND object IN type
          AND obj_name IN name
          AND author IN author.
    ENDIF.
  endmethod.


  method GET_KEYS_FROM_TR.
    DATA objlist TYPE tt_e071.
*  data keys    TYPE TT_COMPKEYS.
    DATA r_trs   TYPE tr_tr.

* TR
    SELECT trkorr trkorr trkorr
        FROM e070
        INTO TABLE r_trs
        WHERE trkorr IN tr.

* TR Tasks if not released
    SELECT trkorr trkorr trkorr
        FROM e070
        APPENDING TABLE r_trs
        WHERE strkorr IN tr
          AND ( trstatus = 'D'    " Modifiable
             OR trstatus = 'O'    " Release started
             OR trstatus = 'L' ). " Modifiable locked

* Alternative : direct but 2 range
*  SELECT trkorr trkorr trkorr
*    INTO TABLE r_trs
*    from V_E071EU
*    where TRKORR in tr OR (
*              STRKORR in tr
*          and ( TRSTATUS = 'D' OR " Modifiable
*                TRSTATUS = 'O' OR " Release started
*                TRSTATUS = 'L') " Modifiable locked
*          ).

    SORT r_trs BY low.
    DELETE ADJACENT DUPLICATES FROM r_trs COMPARING low.

    IF NOT r_trs IS INITIAL.
      SELECT *
        FROM  e071
        INTO TABLE objlist
        FOR ALL ENTRIES IN r_trs
        WHERE  trkorr = r_trs-low.

*    keys = CONV_LIMU_2_R3TR( objlist ).
*  create OBJECT result.
*    result->add_keys( keys  ).
      result = conv_limu_2_r3tr( objlist ).
    ENDIF.
  endmethod.


  method GET_SUB_PACKAGES.
    DATA t_level TYPE tr_packages.
    DATA s_pack LIKE LINE OF result.

    CHECK NOT packages IS INITIAL.

* Package selection
    SELECT devclass AS low
        FROM tdevc
        INTO CORRESPONDING FIELDS OF TABLE result
        WHERE devclass IN packages.

* Package extention
    t_level = result.
    WHILE NOT t_level[] IS INITIAL.
      SORT t_level.
      DELETE ADJACENT DUPLICATES FROM t_level.

      SELECT devclass AS low
          FROM tdevc
          INTO CORRESPONDING FIELDS OF TABLE t_level
             FOR ALL ENTRIES IN t_level
          WHERE parentcl = t_level-low.

      APPEND LINES OF t_level TO result.
    ENDWHILE.

    SORT result.    DELETE ADJACENT DUPLICATES FROM result COMPARING low.
    s_pack-sign = 'I'.  s_pack-option = 'EQ'.   MODIFY result FROM s_pack TRANSPORTING sign option WHERE NOT low IS INITIAL.
  endmethod.


  method INVERSE_TABLE_ORDER.
    DATA x TYPE i.
    FIELD-SYMBOLS <o> TYPE any.

    CHECK NOT my_table IS INITIAL.
    x = lines( my_table ) + 1.
    LOOP AT my_table ASSIGNING <o>.
      IF sy-tabix >= x.   EXIT.   ENDIF.
      INSERT <o> INTO my_table INDEX x.
    ENDLOOP.
    SUBTRACT 1 FROM x.    CHECK x > 0.   DELETE my_table FROM 1 TO x.
  endmethod.


  method IS_WORKING_AREABLE.
    DATA l_item TYPE sewor_working_area.

    result = abap_false.
* Issue 137 : Only type that can be activated should be added
    CHECK type <> 'CLAS' AND type <> 'PROG' AND type <> 'FUGR'.
* Exception CLAS, PROG & FUGR are not type that can be in working area despite they could be activated
    CALL FUNCTION 'RS_OBJ_SUPPORTS_INACTIVE'
      EXPORTING
        obj_type          = space
        sub_type          = type
      IMPORTING
        supports_inactive = result.
* Old way that is not working very well
*  l_item-object = type.
*  PERFORM get_devclass IN PROGRAM saplseworkingarea CHANGING l_item.
*  IF l_item-object = l_item-main_item_object.
*    result = abap_true.
*  ENDIF.
  endmethod.


  method PRETTY_PRINTER.
    FIELD-SYMBOLS <a>   TYPE any.
    DATA d_remotedata   TYPE string.
    DATA v_extend_mod   TYPE char255.
    DATA v_komprmode(1) TYPE c VALUE '0'.
    DATA v_edit_control TYPE c.
    DATA v_status_flag  TYPE char255.
    DATA v_convert_mode	TYPE rseumod.
    DATA v_source_id    TYPE sy-repid.
    DATA v_overflow_area TYPE char255.
    DATA v_content_fill TYPE sy-index.
    DATA v_msg          TYPE char255.
    DATA v_incl         TYPE char255.
    DATA v_row          TYPE char255.
    DATA v_col          TYPE char255.
    DATA v_wrd(30)      TYPE c.
    DATA v_scanned      TYPE char255.
    DATA t_mod_tab      TYPE smodi_mod_tab.
    DATA t_lineindex    TYPE STANDARD TABLE OF edlineindx.
    DATA t_tk           TYPE sedi_tk.
    DATA t_stm          TYPE STANDARD TABLE OF sstmnt.
    DATA t_dummy        TYPE STANDARD TABLE OF string.

    DEFINE mac_assign.
      d_remotedata = &1.
      ASSIGN (d_remotedata) TO <a>.
      IF sy-subrc = 0. <a> = &2. ENDIF.
    END-OF-DEFINITION.
    EXIT.
* To load Funtion Group in memory
    CALL FUNCTION 'CONVERT_EDITOR_SOURCE'
      EXPORTING
        convert_mode  = v_convert_mode
        source_id     = v_source_id
        extend_mod    = v_extend_mod
        komprmode     = v_komprmode
        edit_control  = v_edit_control
        status_flag   = v_status_flag
*       C_EDITOR      =
      TABLES
        content       = t_dummy
        tk            = t_tk
        stm           = t_stm
        lineindex     = t_lineindex
        mod_tab       = t_mod_tab
      CHANGING
        overflow_area = v_overflow_area
        content_fill  = v_content_fill
        msg           = v_msg
        incl          = v_incl
        row           = v_row
        col           = v_col
        wrd           = v_wrd
        scanned       = v_scanned.

    IF indent <> abap_true.   v_convert_mode-indent = '0'.    ENDIF.
* From FUNCTION 'CONVERT_EDITOR_SOURCE'
*  IF RSEUMOD-LOWERCASE = 'X' AND RSEUMOD-STYLE NE SPACE.
*    CASE_MODE = 'LOWER'.
*  ELSEIF RSEUMOD-LOWERCASE = 'G' AND RSEUMOD-STYLE NE SPACE.
*    CASE_MODE = 'HIKEY'.
*  ELSEIF RSEUMOD-STYLE NE SPACE.
*    CASE_MODE = 'UPPER'.
*  ELSE.
*    CASE_MODE = 'EMPTY'.
*  ENDIF.
    mac_assign '(SAPLLOCAL_EDT1)RSEUMOD-STYLE' abap_true.   " Change code to ....
    mac_assign '(SAPLLOCAL_EDT1)RSEUMOD-LOWERCASE' space.   " UPPER CASE
    mac_assign '(SAPLLOCAL_EDT1)EDIT-APP_ABAP' abap_true.   " ABAP Editor
    mac_assign '(SAPLLOCAL_EDT1)EDIT-LINE_SIZE' swbse_c_max_line_width.       " Line Size

    CALL FUNCTION 'CONVERT_EDITOR_SOURCE'
      EXPORTING
        convert_mode  = v_convert_mode
        source_id     = v_source_id
        extend_mod    = v_extend_mod
        komprmode     = v_komprmode
        edit_control  = v_edit_control
        status_flag   = v_status_flag
*       C_EDITOR      =
      TABLES
        content       = table
        tk            = t_tk
        stm           = t_stm
        lineindex     = t_lineindex
        mod_tab       = t_mod_tab
      CHANGING
        overflow_area = v_overflow_area
        content_fill  = v_content_fill
        msg           = v_msg
        incl          = v_incl
        row           = v_row
        col           = v_col
        wrd           = v_wrd
        scanned       = v_scanned.
*     ENH_INSTANCE        =

* Old way
*  DATA t_step TYPE STANDARD TABLE OF edstep.
*  DATA t_lineindex  TYPE STANDARD TABLE OF edlineindx.
*  DATA t_usercom  TYPE STANDARD TABLE OF edusercoms.
*  DATA t_page  TYPE rswsourcet.
*  DATA t_tk  TYPE sedi_tk.
*  DATA t_stm  TYPE STANDARD TABLE OF sstmnt.
*  DATA t_linenum TYPE STANDARD TABLE OF edlinenum.
*  DATA t_linecoms TYPE STANDARD TABLE OF edlinecoms.
*  DATA t_delete_tab TYPE smodi_src_tab.
*  DATA t_smodisrc_tab TYPE smodi_src_tab.
**"     VALUE(CALLBACK_USERCOM) TYPE  RSTXP-TDOBJECT DEFAULT SPACE
**"     VALUE(CALLBACK_CHECK) TYPE  RSTXP-TDOBJECT DEFAULT SPACE
**"     VALUE(CALLBACK_SAVE) TYPE  RSTXP-TDOBJECT DEFAULT SPACE
**"     VALUE(CALLBACK_PROGRAM) LIKE  TRDIR-NAME DEFAULT SPACE
**"     VALUE(CALLBACK_SET_PFKEY) TYPE  RSTXP-TDOBJECT DEFAULT SPACE
**"     VALUE(CALLBACK_BEFORE_FUNCTION) TYPE  RSTXP-TDOBJECT DEFAULT
**"       SPACE
**"     VALUE(CALLBACK_AFTER_FUNCTION) TYPE  RSTXP-TDOBJECT DEFAULT
**"       SPACE
*  DATA v_editor_mode TYPE editormode.
*  DATA v_case_mode(5) TYPE c VALUE 'UPPER'.
*  DATA v_komprmode(1) TYPE c VALUE '0'.
*  DATA v_edit_control TYPE c.
*  DATA v_cursor_i TYPE sedi_cursor.
*  DATA v_control_content TYPE cxtab_control.
*  DATA v_content_display TYPE sy-index.
*  DATA v_content_fill TYPE sy-index.
*  DATA v_content_fill_c TYPE sy-index.
*  DATA v_linecom_shift_places TYPE sy-index.
*  DATA v_fcode_i TYPE tse02-progfun VALUE 'PGPP'.
*  DATA v_flag_replconf TYPE boolean.
*  DATA v_flag_auto_scroll TYPE boolean.
*  DATA v_linecom_was_executed TYPE char255.
*  DATA v_edit TYPE s38e.
*  DATA v_new_content_display TYPE sy-index.
*  DATA v_pcmode TYPE string VALUE '2'.
*  DATA v_tdcommline TYPE  rstxp-tdcommline.
*  DATA v_tdparam TYPE  rstxp-tdparam.
*  DATA v_tdparam2 TYPE  rstxp-tdparam.
*  DATA v_tdattach TYPE  rstxp-tdattach.
*  DATA v_flag_linecom_reset TYPE char255.
*  DATA v_undo_exported TYPE char255.
*  DATA v_s_modified TYPE char255.
*  DATA v_editor_content_modified TYPE char255.
*  DATA:  BEGIN OF v_remember_i,
*           index1 LIKE sy-tabix,
*           offset1 LIKE sy-fdpos,
*           linecom1(2),
*           index2 LIKE sy-tabix,
*           offset2 LIKE sy-fdpos,
*           linecom2(2),
*           content_display1 TYPE i,        "CONTENT_DISPLAY zu INDEX1
*           content_display2 TYPE i,        "CONTENT_DISPLAY zu INDEX2
*           side,
*        END OF v_remember_i.
**  DATA v_remember_i TYPE char255.
*  DATA v_scanned TYPE char255.
*  DATA v_new_source TYPE char255.
*  DATA v_h TYPE d020s.
*  DATA v_dynpro_language_import TYPE sy-langu.
*  DATA v_dynpro_text TYPE char255.
*  DATA v_motif TYPE char255.
*  DATA v_content_app_id TYPE s38e-app_id VALUE 'PG'.
*  DATA v_status_flag TYPE char255.
*  DATA v_lines_appended TYPE sy-index.
*  DATA v_mark_flag TYPE char255.
*  DATA v_lineal_mode TYPE char255.
*  DATA v_auto_correct TYPE c.
*  DATA v_suppress_dialog_flag TYPE string VALUE abap_true.
*  DATA v_title TYPE char255.
*  DATA v_content_display_new TYPE sy-index.
*  DATA v_change_mod_flag TYPE char255.
*  DATA v_dynpro_lines TYPE sy-index.
*  DATA v_usercom_filled TYPE char255.
*  DATA v_exclude_filled TYPE char255.
*  DATA v_extend_mod TYPE char255.
*  DATA v_dynpro_mod TYPE char255.
*  DATA v_content_save_flag TYPE char255.
*  DATA v_in_linenumber_field TYPE char255.
*  DATA v_scrp_edtr_interface_flag TYPE char255.
*  DATA v_with_next TYPE char255.
*  DATA v_overflow_area TYPE char255.
*  DATA v_msg TYPE char255.
*  DATA v_incl TYPE char255.
*  DATA v_row TYPE char255.
*  DATA v_col TYPE char255.
*  DATA v_wrd(30) TYPE c.
*  DATA v_korrnum TYPE e070-trkorr.
*  DATA v_f1_flag TYPE char255.
**"     VALUE(DYNPRO_IS_GENERATED) DEFAULT SPACE
**"     VALUE(DYNPRO_IS_IN_WA) DEFAULT SPACE
**"     VALUE(DYNPRO_IS_IN_FOREIGN_WA) DEFAULT SPACE
**"     VALUE(WITH_ACTIVE_SOURCES) DEFAULT SPACE
*  DATA v_changed_max_line_neu TYPE sy-index VALUE 99999.
*  DATA v_changed_max_line_neu_c TYPE sy-index VALUE 99999.
*  DATA v_changed_min_line_neu TYPE sy-index.
*  DATA v_changed_min_line_neu_c TYPE sy-index.
*  DATA v_changed_min_line_alt TYPE sy-index.
*  DATA v_changed_min_line_alt_c TYPE sy-index.
*  DATA v_changed_max_line_alt TYPE sy-index VALUE 99999.
*  DATA v_changed_max_line_alt_c TYPE sy-index VALUE 99999.
*  DATA v_diff_altneu TYPE sy-index.
*  DATA v_diff_altneu_c TYPE sy-index.
*  DATA v_fb_name TYPE rs38l-name.
*  DATA v_enter_w TYPE tse04-linecomtxt.
**"     VALUE(TRDIR_INF) LIKE  TRDIR STRUCTURE  TRDIR DEFAULT SPACE
**"     VALUE(FIND_LEN) LIKE  SY-FDPOS OPTIONAL
**"     VALUE(REPLACE_LEN) LIKE  SY-FDPOS OPTIONAL
**"     VALUE(C_EDITOR) TYPE REF TO  OBJECT OPTIONAL
**"     VALUE(CONTAINER_NEW) TYPE REF TO  CL_GUI_CUSTOM_CONTAINER OPTIONAL
**data v_CONTAINER_NEW1) TYPE REF TO  CL_GUI_CONTAINER OPTIONAL
**"     VALUE(C1_EDITOR) TYPE REF TO  C_TEXTEDIT_CONTROL OPTIONAL
**"     VALUE(ERROR_HANDLE) TYPE  CNTL_HANDLE OPTIONAL
**"     VALUE(OLD_FCODE) LIKE  TSE02-PROGFUN OPTIONAL
**"     VALUE(IDENT_FLAG) TYPE  CHAR1 OPTIONAL
**"     VALUE(TRANSPORT_KEY) LIKE  TRKEY STRUCTURE  TRKEY OPTIONAL
**"     VALUE(P_DOCU_CONTAINER) TYPE REF TO  CL_GUI_CONTROL OPTIONAL
**"     REFERENCE(MOD_INSTANCE) TYPE REF TO  CL_CLM_TOOL_LOG OPTIONAL
**"     VALUE(CONTROL_TYPE) TYPE  C DEFAULT SPACE
**"     REFERENCE(ENH_INSTANCE) TYPE REF TO  CL_ENH_HOOK_CONTAINER
**"       OPTIONAL
**"     REFERENCE(SUBCONTAINER_TAB) TYPE  SEDI_SUB_CONT_TAB OPTIONAL
*  mac_assign '(SAPLLOCAL_EDT1)RSEUMOD-STYLE' 'X'.
*  v_edit-app_id = 'PG'.
*  v_edit-app_disp = 'A'.
*  v_edit-app_obj  = 'ABAP'.
*  v_edit-app_abap = abap_true.
**  v_edit-BUF_NAME                                      ZAPLINK_OBJECT================CM00P
**                                     BUF_CULINE                                      000001
**                                     BUF_SCLINE                                      000001
**                                     BUF_CUOFFS                                      000
**                                     BUF_CUWORD
**                                     BUF_CUSTRG
*  v_edit-line_size = 1024.
*  v_edit-app_prog = abap_true.
*
*  CALL FUNCTION 'USERCOM_INPUT'
**   EXPORTING
**     LINE_NAME                      = 'RSTXP-TDLINE'
**     FIRST_NAME                     = 'RSTXP-TDFIRST'
**     LAST_NAME                      = 'RSTXP-TDLAST'
**     TOTAL_NAME                     = 'RSTXP-TDTOTAL'
**     MARK_NAME                      = 'RSTXP-TDMARK'
**     MARK1_NAME                     = 'RSTXP-TDMARK1'
**     COMMLINE_NAME                  = 'RSTXP-TDCOMMLINE'
**     LINECOM_NAME                   = 'RSTXP-TDLINECOM'
**     SPLIT_CALL                     = ' '
**     INITIALIZE_EDIT_CONTROL        = 'X'
**     C_HANDLE                       =
*    TABLES
*      step                           = t_step
*      content                        = sourcecode
*      content_c                      = result
*      lineindex                      = t_lineindex
*      usercom                        = t_usercom
*      page                           = t_page
*      tk                             = t_tk
*      stm                            = t_stm
**     E                              =
**     F                              =
**     M                              =
**     SMODILOG_ABAP                  =
**     MOD_TAB                        =
*      linenum                        = t_linenum
*      linecoms                       = t_linecoms
**     BUFFER                         =
**     BUFFER_C                       =
**     MESSAGE_TAB_I                  =
*      delete_tab                     = t_delete_tab
*      smodisrc_tab                   = t_smodisrc_tab
*    CHANGING
**     CALLBACK_USERCOM               = ' '
**     CALLBACK_CHECK                 = ' '
**     CALLBACK_SAVE                  = ' '
**     CALLBACK_PROGRAM               = ' '
**     CALLBACK_SET_PFKEY             = ' '
**     CALLBACK_BEFORE_FUNCTION       = ' '
**     CALLBACK_AFTER_FUNCTION        = ' '
*      editor_mode                    = v_editor_mode
*      case_mode                      = v_case_mode
*      komprmode                      = v_komprmode
*      edit_control                   = v_edit_control
*      cursor_i                       = v_cursor_i
*      control_content                = v_control_content
*      content_display                = v_content_display
*      content_fill                   = v_content_fill
*      content_fill_c                 = v_content_fill_c
*      linecom_shift_places           = v_linecom_shift_places
*      fcode_i                        = v_fcode_i
*      flag_replconf                  = v_flag_replconf
*      flag_auto_scroll               = v_flag_auto_scroll
*      linecom_was_executed           = v_linecom_was_executed
*      edit                           = v_edit
*      new_content_display            = v_new_content_display
*      pcmode                         = v_pcmode
*      tdcommline                     = v_tdcommline
*      tdparam                        = v_tdparam
*      tdparam2                       = v_tdparam2
*      tdattach                       = v_tdattach
*      flag_linecom_reset             = v_flag_linecom_reset
*      undo_exported                  = v_undo_exported
*      s_modified                     = v_s_modified
*      editor_content_modified        = v_editor_content_modified
*      remember_i                     = v_remember_i
*      scanned                        = v_scanned
*      new_source                     = v_new_source
*      h                              = v_h
*      dynpro_language_import         = v_dynpro_language_import
*      dynpro_text                    = v_dynpro_text
*      motif                          = v_motif
*      content_app_id                 = v_content_app_id
*      status_flag                    = v_status_flag
*      lines_appended                 = v_lines_appended
*      mark_flag                      = v_mark_flag
*      lineal_mode                    = v_lineal_mode
*      auto_correct                   = v_auto_correct
*      suppress_dialog_flag           = v_suppress_dialog_flag
*      title                          = v_title
*      content_display_new            = v_content_display_new
*      change_mod_flag                = v_change_mod_flag
*      dynpro_lines                   = v_dynpro_lines
*      usercom_filled                 = v_usercom_filled
*      exclude_filled                 = v_exclude_filled
*      extend_mod                     = v_extend_mod
*      dynpro_mod                     = v_dynpro_mod
*      content_save_flag              = v_content_save_flag
*      in_linenumber_field            = v_in_linenumber_field
*      scrp_edtr_interface_flag       = v_scrp_edtr_interface_flag
*      with_next                      = v_with_next
*      overflow_area                  = v_overflow_area
*      msg                            = v_msg
*      incl                           = v_incl
*      row                            = v_row
*      col                            = v_col
*      wrd                            = v_wrd
*      korrnum                        = v_korrnum
*      f1_flag                        = v_f1_flag
**#####DYNPRO_IS_GENERATED############ = v_DYNPRO_IS_GENERATED
**#####DYNPRO_IS_IN_WA################ = v_DYNPRO_IS_IN_WA
**#####DYNPRO_IS_IN_FOREIGN_WA######## = v_DYNPRO_IS_IN_FOREIGN_WA
**#####WITH_ACTIVE_SOURCES############ = v_WITH_ACTIVE_SOURCES
*      changed_max_line_neu           = v_changed_max_line_neu
*      changed_max_line_neu_c         = v_changed_max_line_neu_c
*      changed_min_line_neu           = v_changed_min_line_neu
*      changed_min_line_neu_c         = v_changed_min_line_neu_c
*      changed_min_line_alt           = v_changed_min_line_alt
*      changed_min_line_alt_c         = v_changed_min_line_alt_c
*      changed_max_line_alt           = v_changed_max_line_alt
*      changed_max_line_alt_c         = v_changed_max_line_alt_c
*      diff_altneu                    = v_diff_altneu
*      diff_altneu_c                  = v_diff_altneu_c
*      fb_name                        = v_fb_name
*      enter_w                        = v_enter_w.
**     TRDIR_INF                      = ' '
**     FIND_LEN                       =
**     REPLACE_LEN                    =
**     C_EDITOR                       =
**     CONTAINER_NEW                  =
**     CONTAINER_NEW1                 =
**     C1_EDITOR                      =
**     ERROR_HANDLE                   =
**     OLD_FCODE                      =
**     IDENT_FLAG                     =
**     TRANSPORT_KEY                  =
**     P_DOCU_CONTAINER               =
**     MOD_INSTANCE                   =
**     CONTROL_TYPE                   = ' '
**     ENH_INSTANCE                   =
**     SUBCONTAINER_TAB               =

* Cleanning comments like '" my comment' but keeps comment like '"#EC'
    DATA t_tokens TYPE STANDARD TABLE OF stokes.
    DATA t_statements TYPE STANDARD TABLE OF sstmnt.
    DATA t_lines   TYPE SORTED TABLE OF sstmnt WITH UNIQUE KEY trow.
*  DATA t_kw      TYPE STANDARD TABLE OF char255.
*  DATA s_kw      LIKE LINE OF t_kw.
    DATA d_tabix   TYPE sy-tabix.
    DATA t_result  TYPE match_result_tab.
    FIELD-SYMBOLS:
      <st> LIKE LINE OF t_lines,
      <r>  LIKE LINE OF t_result.

*  s_kw = '"'.    APPEND s_kw TO t_kw.
    SCAN ABAP-SOURCE table TOKENS INTO t_tokens
                       STATEMENTS INTO t_statements.
*                       KEYWORDS FROM t_kw.
    SORT t_statements BY trow. DELETE ADJACENT DUPLICATES FROM t_statements COMPARING trow.
    DELETE t_statements WHERE trow = 0.
    t_lines = t_statements.
*  zaplink_tools=>inverse_table_order( CHANGING my_table = t_statements ).
    LOOP AT table ASSIGNING <a>.
      CHECK <a> CP '*"*' AND NOT <a> CP '*"##EC*'.
      d_tabix = sy-tabix.
      READ TABLE t_lines ASSIGNING <st>
           WITH TABLE KEY trow = d_tabix.
*    if sy-subrc = 4.    read TABLE t_lines ASSIGNING <sT> INDEX sy-tabix.     endif.
      CHECK sy-subrc = 0.
      FIND ALL OCCURRENCES OF '"' IN <a> RESULTS t_result.
      LOOP AT t_result ASSIGNING <r>.
        CHECK <r>-offset >  <st>-tcol.
        <a> = <a>(<r>-offset).    EXIT.
      ENDLOOP.
    ENDLOOP.
  endmethod.


  method REMOVE_COMMENT_ON_SOURCE.
    FIELD-SYMBOLS <l> TYPE any.
    LOOP AT table ASSIGNING <l>.
      IF <l> IS INITIAL. DELETE table. CONTINUE. ENDIF.  " Condense
      IF <l>(1) = '*'.   DELETE table. CONTINUE. ENDIF.  " Condense
      <l> = conv_abap_line( <l> ).
*    CONDENSE <l>.
*    IF <l>(1) = '"'.   DELETE table. CONTINUE. ENDIF.  " commented line like :     " comment
    ENDLOOP.
  endmethod.


  method REMOVE_COMP_FROM_WORKING_AREA.
    DATA _name     TYPE e071-obj_name.

    _name = name.
    IF type ='CLAS'.
* Issue 137 : Remove erroneous Working Area entries
* Do not works : FM DELETE transform CLAS into * NAME=% :-(
      DELETE FROM dwinactiv WHERE object = type
                            AND obj_name = _name.
    ELSE.
      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = type
          obj_name               = _name
          immediate              = abap_true
          actualize_working_area = abap_false.
    ENDIF.
  endmethod.


  method TABLE_2_STRING.
    DATA s TYPE string.

    CHECK NOT source IS INITIAL.
    LOOP AT source INTO s.
      CONCATENATE result cl_abap_char_utilities=>newline s INTO result.
    ENDLOOP.
    SHIFT result LEFT BY 1 PLACES.
  endmethod.


  method UNZIP_STRING.
    TRY.
        CALL METHOD cl_abap_gzip=>decompress_text
          EXPORTING
            gzip_in    = data
*           gzip_in_len  = -1
            conversion = 'DEFAULT'
          IMPORTING
*           text_out_len =
            text_out   = result.
      CATCH cx_parameter_invalid_range.
        result = data.
      CATCH cx_sy_buffer_overflow.
        result = data.
      CATCH cx_sy_conversion_codepage.
        result = data.
      CATCH cx_sy_compression_error.
        result = data.
    ENDTRY.
  endmethod.


  method ZIP_STRING.
    TRY.
        CALL METHOD cl_abap_gzip=>compress_text
          EXPORTING
            text_in        = data
*           text_in_len    = -1
            compress_level = 9
            conversion     = 'DEFAULT'
          IMPORTING
*           gzip_out_len   =
            gzip_out       = result.
      CATCH cx_parameter_invalid_range.
        result = data.
      CATCH cx_sy_buffer_overflow.
        result = data.
      CATCH cx_sy_conversion_codepage.
        result = data.
    ENDTRY.
  endmethod.
ENDCLASS.
