class ZAPLINK_CONNECTOR_ definition
  public
  abstract
  create public .

public section.
  type-pools ABAP .
  type-pools RPYTY .

  interfaces ZAPLINK_CNX_EXT_CLEANER .
  interfaces ZAPLINK_CNX_EXT_CODE_SIGNATURE .
  interfaces ZAPLINK_CONNECTOR .
  interfaces ZAPLINK_DATATYPES .

  aliases APPLICATION_LOG
    for ZAPLINK_CONNECTOR~APPLICATION_LOG .
  aliases BALLOG_OBJECT
    for ZAPLINK_CONNECTOR~BALLOG_OBJECT .
  aliases COMP_NODENAME
    for ZAPLINK_CONNECTOR~COMP_NODENAME .
  aliases EXISTS
    for ZAPLINK_CONNECTOR~EXISTS .
  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases OPTIONS
    for ZAPLINK_CONNECTOR~OPTIONS .
  aliases SUB_COMPONENT
    for ZAPLINK_DATATYPES~SUB_COMPONENT .
  aliases SUPPORTED_TYPES
    for ZAPLINK_CONNECTOR~SUPPORTED_TYPES .
  aliases TEXTPOOL_KINDS
    for ZAPLINK_DATATYPES~TEXTPOOL_KINDS .
  aliases UUID
    for ZAPLINK_CONNECTOR~UUID .
  aliases VERSION
    for ZAPLINK_CONNECTOR~VERSION .
  aliases ANONYMIZE
    for ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE .
  aliases CREATE_NEW_RAW
    for ZAPLINK_CONNECTOR~CREATE_NEW_RAW .
  aliases DELETE_FROM_SAP
    for ZAPLINK_CONNECTOR~DELETE_FROM_SAP .
  aliases DO_EXISTS
    for ZAPLINK_CONNECTOR~DO_EXISTS .
  aliases EXPORT_FROM_SAP
    for ZAPLINK_CONNECTOR~EXPORT_FROM_SAP .
  aliases GET_VALUES
    for ZAPLINK_CONNECTOR~GET_VALUES .
  aliases IMPORT_TO_SAP
    for ZAPLINK_CONNECTOR~IMPORT_TO_SAP .
  aliases IS_SUPPORTED_VERSION
    for ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION .
  aliases READ_FROM_SAP
    for ZAPLINK_CONNECTOR~READ_FROM_SAP .
  aliases UNANONYMIZE
    for ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE .
  aliases WRITE_TO_SAP
    for ZAPLINK_CONNECTOR~WRITE_TO_SAP .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
  aliases TD_CLASSNAME
    for ZAPLINK_DATATYPES~TD_CLASSNAME .
  aliases TD_COMPEXISTS
    for ZAPLINK_CONNECTOR~TD_COMPEXISTS .
  aliases TD_COMPNAME
    for ZAPLINK_CONNECTOR~TD_COMPNAME .
  aliases TD_COMPTYPE
    for ZAPLINK_CONNECTOR~TD_COMPTYPE .
  aliases TD_CONNCLASS
    for ZAPLINK_CONNECTOR~TD_CONNCLASS .
  aliases TD_CONNEXISTS
    for ZAPLINK_CONNECTOR~TD_CONNEXISTS .
  aliases TD_CONNUUID
    for ZAPLINK_CONNECTOR~TD_CONNUUID .
  aliases TD_CONNVER
    for ZAPLINK_CONNECTOR~TD_CONNVER .
  aliases TD_DEVCLASS
    for ZAPLINK_DATATYPES~TD_DEVCLASS .
  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TD_EXISTS
    for ZAPLINK_DATATYPES~TD_EXISTS .
  aliases TD_LANG
    for ZAPLINK_DATATYPES~TD_LANG .
  aliases TD_LIGHT
    for ZAPLINK_DATATYPES~TD_LIGHT .
  aliases TD_OBJ_TYPE
    for ZAPLINK_DATATYPES~TD_OBJ_TYPE .
  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
  aliases TD_TXTP_ID
    for ZAPLINK_DATATYPES~TD_TXTP_ID .
  aliases TD_TXTP_KIND
    for ZAPLINK_DATATYPES~TD_TXTP_KIND .
  aliases TD_TXTP_LEN
    for ZAPLINK_DATATYPES~TD_TXTP_LEN .
  aliases TD_TXTP_TEXT
    for ZAPLINK_DATATYPES~TD_TXTP_TEXT .
  aliases TD_WITH_SUBCOMP
    for ZAPLINK_DATATYPES~TD_WITH_SUBCOMP .
  aliases TI_CLEANNING
    for ZAPLINK_CONNECTOR~TI_CLEANNING .
  aliases TI_CLEANNING_4DATA
    for ZAPLINK_CONNECTOR~TI_CLEANNING_4DATA .
  aliases TO_COMPONENT
    for ZAPLINK_CONNECTOR~TO_COMPONENT .
  aliases TO_CONNECTOR
    for ZAPLINK_CONNECTOR~TO_CONNECTOR .
  aliases TO_EZ_XML
    for ZAPLINK_CONNECTOR~TO_EZ_XML .
  aliases TO_LIST
    for ZAPLINK_CONNECTOR~TO_LIST .
  aliases TO_MSG_COLL
    for ZAPLINK_CONNECTOR~TO_MSG_COLL .
  aliases TO_OPTIONS
    for ZAPLINK_CONNECTOR~TO_OPTIONS .
  aliases TO_RAW
    for ZAPLINK_CONNECTOR~TO_RAW .
  aliases TO_RAW_BASE
    for ZAPLINK_CONNECTOR~TO_RAW_BASE .
  aliases TO_RAW_DATA
    for ZAPLINK_CONNECTOR~TO_RAW_DATA .
  aliases TO_XML
    for ZAPLINK_CONNECTOR~TO_XML .
  aliases TR_DLVUNIT
    for ZAPLINK_DATATYPES~TR_DLVUNIT .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .
  aliases TR_PACKAGES
    for ZAPLINK_DATATYPES~TR_PACKAGES .
  aliases TR_TR
    for ZAPLINK_DATATYPES~TR_TR .
  aliases TS_BASE_ATTRIBUTS
    for ZAPLINK_CONNECTOR~TS_BASE_ATTRIBUTS .
  aliases TS_COMPKEY
    for ZAPLINK_DATATYPES~TS_COMPKEY .
  aliases TS_COMPTYPE
    for ZAPLINK_DATATYPES~TS_COMPTYPE .
  aliases TS_CONNDATA
    for ZAPLINK_CONNECTOR~TS_CONNDATA .
  aliases TS_CONN_CLASS
    for ZAPLINK_DATATYPES~TS_CONN_CLASS .
  aliases TS_CONN_DEF
    for ZAPLINK_DATATYPES~TS_CONN_DEF .
  aliases TS_DOC
    for ZAPLINK_DATATYPES~TS_DOC .
  aliases TS_HEAD
    for ZAPLINK_DATATYPES~TS_HEAD .
  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .
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
  aliases TT_DYNPROS
    for ZAPLINK_DATATYPES~TT_DYNPROS .
  aliases TT_RAWTEXT
    for ZAPLINK_DATATYPES~TT_RAWTEXT .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
  aliases TT_TXTP_TEXTS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTS .
  aliases TT_TYPES
    for ZAPLINK_CONNECTOR~TT_TYPES .

  types TO_MENUPAINTER type ref to ZAPLINK_MENUPAINTER_DATA .

  data CLASS_NAME type TD_CLASSNAME read-only .

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_TYPE_FROM_XML
    importing
      !OBJECT type TO_XML
    returning
      value(RESULT) type TD_COMPTYPE
    raising
      ZAPLINK_CX .
protected section.

  aliases TT_FLOW_LOGIC
    for ZAPLINK_DATATYPES~TT_FLOW_LOGIC .

  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
  class-data O_ACTIVATE type ref to ZAPLINK_ACTIVATE .

  methods SET_BASE_ATTRIBUTS
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(ATTRIBUTS) type TS_BASE_ATTRIBUTS .
  class-methods GET_PROG_SOURCE
    importing
      !PROGRAM type TD_PROGNAME
    returning
      value(RESULT) type TD_ABAPSOURCE .
  class-methods GET_PROG_RAWSOURCE
    importing
      !PROGRAM type TD_PROGNAME
    returning
      value(RESULT) type TT_ABAPRAWSOURCE .
  class-methods CONV_ABAP_RAW2SOURCE
    importing
      !RAW type TT_ABAPRAWSOURCE
    returning
      value(RESULT) type TD_ABAPSOURCE .
  class-methods CONV_ABAP_SOURCE2RAW
    importing
      !SOURCE type TD_ABAPSOURCE
    returning
      value(RESULT) type TT_ABAPRAWSOURCE .
  class-methods SET_PROG_RAWSOURCE
    importing
      !PROGRAM type TD_PROGNAME
      !RAW type TT_ABAPRAWSOURCE .
  class-methods SET_PROG_SOURCE
    importing
      !PROGRAM type TD_PROGNAME
      !SOURCE type TD_ABAPSOURCE .
  class-methods GET_PROG_TEXTPOOL
    importing
      !PROGRAM type TD_PROGNAME
    returning
      value(RESULT) type TT_TXTP_TEXTPOOLS .
  class-methods FUSION_TEXTPOOL
    importing
      !SOURCE type TT_TXTP_TEXTPOOLS
      !COMPLETION type TT_TXTP_TEXTPOOLS
    returning
      value(RESULT) type TT_TXTP_TEXTPOOLS .
  class-methods SET_PROG_TEXTPOOL
    importing
      !PROGRAM type TD_PROGNAME
      !TEXTPOOL type TT_TXTP_TEXTPOOLS .
  methods GET_DYNPROS
    importing
      !PROGRAM type TD_PROGNAME
    returning
      value(RESULT) type TT_DYNPROS .
  methods SET_DYNPROS
    importing
      !PROGRAM type TD_PROGNAME
      value(DYNPROS) type TT_DYNPROS
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods CONV_FLOWLOGIC2ABAPSOURCE
    importing
      !RAW type TT_FLOW_LOGIC
    returning
      value(RESULT) type TD_ABAPSOURCE .
  class-methods CONV_ABAPSOURCE2FLOWLOGIC
    importing
      !SOURCE type TD_ABAPSOURCE
    returning
      value(RESULT) type TT_FLOW_LOGIC .
  methods GET_MENUS
    importing
      !PROGRAM type TD_PROGNAME
    returning
      value(RESULT) type TO_MENUPAINTER
    raising
      ZAPLINK_CX .
  methods SET_MENUS
    importing
      !PROGRAM type TD_PROGNAME
      !MENUS type TO_MENUPAINTER
    raising
      ZAPLINK_CX .
  methods CHECK_COMPONENT_LIST
    importing
      !OBJECT type TO_RAW
    changing
      !COMPONENTS type TO_LIST
    raising
      ZAPLINK_CX .
  class-methods ACTIVE_COMPONENT
    importing
      !TYPE type TD_COMPTYPE
      !NAME type TD_COMPNAME
    returning
      value(RESULT) type ABAP_BOOL .
private section.

  types:
    tr_scrntype TYPE RANGE OF scrntype .

  class-data R_TEXT_FIELDS type TR_SCRNTYPE .
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_COMMUN' ##NO_TEXT.
ENDCLASS.



CLASS ZAPLINK_CONNECTOR_ IMPLEMENTATION.


  method ACTIVE_COMPONENT.
  DATA d_is_work_i    TYPE abap_bool.
  DATA d_is_inactiv   TYPE abap_bool.
  DATA d_obj_name     TYPE e071-obj_name.
  DATA d_type         TYPE e071-object.
  DATA t_comps      TYPE zaplink_list=>tt_compkeys.
  DATA s_comp       LIKE LINE OF t_comps.

  result = abap_false.
  s_comp-name = d_obj_name = name.
  s_comp-type = d_type = type.
  CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
    EXPORTING
      object                        = d_type
      obj_name                      = d_obj_name
*     GLOBAL_CHECK                  = ' '
*     MODE                          = 'S'
    IMPORTING
      object_is_work_item           = d_is_work_i     " My inactive object
      object_inactive_version       = d_is_inactiv.
  IF NOT d_is_work_i IS INITIAL OR NOT d_is_inactiv IS INITIAL.
    IF NOT o_activate IS BOUND.
      CREATE OBJECT o_activate.
    ELSE.
      o_activate->reset( ).
    ENDIF.
    APPEND s_comp TO t_comps.
    o_activate->add_keys( t_comps ).
    t_comps = o_activate->activate( ).
    result = abap_true.
    IF NOT t_comps IS INITIAL.      CLEAR result.     ENDIF.
  ENDIF.
  endmethod.


  method CHECK_COMPONENT_LIST.
* First reported by Issue 92
  DATA o_comp         TYPE to_component.

  o_comp = object->get_component( abap_true ).
  IF NOT components IS BOUND.   CREATE OBJECT components.   ENDIF.
  TRY.
      components->add( o_comp ).
    CATCH zaplink_cx_list.
  ENDTRY.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA s_type LIKE LINE OF r_text_fields.
  s_type-sign = 'I'. s_type-option = 'EQ'.
  s_type-low = rpyty_dynp_ftype-text.       APPEND s_type TO r_text_fields.
  s_type-low = rpyty_dynp_ftype-frame.      APPEND s_type TO r_text_fields.
  s_type-low = rpyty_dynp_ftype-push.       APPEND s_type TO r_text_fields.
  endmethod.


  method CONSTRUCTOR.
  class_name = zaplink_tools=>get_clas_name( me ).
  endmethod.


  method CONV_ABAPSOURCE2FLOWLOGIC.
  SPLIT source AT line_separator INTO TABLE result.
  IF NOT result IS INITIAL.       " remove first line separator
    IF source(1) = line_separator.
      DELETE result INDEX 1.
    ENDIF.
  ENDIF.
  endmethod.


  method CONV_ABAP_RAW2SOURCE.
  FIELD-SYMBOLS <c> LIKE LINE OF raw.

  LOOP AT raw ASSIGNING <c>.
    CONCATENATE result <c> INTO result SEPARATED BY line_separator. " will start with line_sep better for XML display and required because get_value remove spaces
  ENDLOOP.
  endmethod.


  method CONV_ABAP_SOURCE2RAW.
  SPLIT source AT line_separator INTO TABLE result.
  IF NOT result IS INITIAL.       " remove first line separator
    IF source(1) = line_separator.
      DELETE result INDEX 1.
    ENDIF.
  ENDIF.
  endmethod.


  method CONV_FLOWLOGIC2ABAPSOURCE.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF raw.

  LOOP AT raw ASSIGNING <c>.
    CONCATENATE result <c> INTO result SEPARATED BY line_separator. " will start with line_sep better for XML display and required because get_value remove spaces
  ENDLOOP.
  endmethod.


  method FUSION_TEXTPOOL.
  FIELD-SYMBOLS:
    <c>  LIKE LINE OF completion,
    <ct> LIKE LINE OF <c>-texts,
    <t>  LIKE LINE OF source.

  result = source.
  LOOP AT result ASSIGNING <t>.
    READ TABLE completion ASSIGNING <c>
         WITH TABLE KEY id = <t>-id
                       key = <t>-key.
    CHECK sy-subrc = 0.
    LOOP AT <c>-texts ASSIGNING <ct>.
      READ TABLE <t>-texts TRANSPORTING NO FIELDS
           WITH TABLE KEY langu = <ct>-langu.
      CHECK sy-subrc <> 0.
      INSERT <ct> INTO TABLE <t>-texts.
    ENDLOOP.
  ENDLOOP.
  LOOP AT completion ASSIGNING <c>.
    READ TABLE result ASSIGNING <t>
         WITH TABLE KEY id = <c>-id
                       key = <c>-key.
    CHECK sy-subrc <> 0.
    INSERT <c> INTO TABLE result.
  ENDLOOP.
  endmethod.


  method GET_DYNPROS.
  SET EXTENDED CHECK OFF.
  INCLUDE mseusbit. " for c_type_dynp_select_norm, c_type_dynp_select_mod, c_type_dynp_select_incl
  SET EXTENDED CHECK ON.
*  DATA:
*    BEGIN OF dynp_id,
*      prog TYPE d020s-prog,
*      dnum TYPE d020s-dnum,
*    END OF dynp_id.
  TYPES:
    BEGIN OF t_dynpro,
      prog TYPE d020s-prog,
      dnum TYPE d020s-dnum,
    END OF t_dynpro.
  DATA t_dynpros TYPE SORTED TABLE OF t_dynpro WITH UNIQUE KEY dnum.
  DATA header TYPE d020s.
  DATA t_d021t TYPE SORTED TABLE OF d021t WITH UNIQUE KEY dynr fldn lang.
  DATA t_container TYPE dycatt_tab.
*  DATA t_fields TYPE STANDARD TABLE OF d021s.
  DATA t_fields    TYPE dyfatc_tab.
  DATA t_flowlogic TYPE tt_flow_logic.
  DATA s_result LIKE LINE OF result.
  DATA s_cont   LIKE LINE OF s_result-containers.
  FIELD-SYMBOLS:
    <c> LIKE LINE OF t_container,
    <f> LIKE LINE OF t_fields,
    <t> LIKE LINE OF t_d021t,
    <r> LIKE LINE OF t_dynpros.
*    <r> LIKE LINE OF result.
*  DATA s_field  LIKE LINE OF <r>-fields.
  DATA s_field  LIKE LINE OF s_cont-fields.
  DATA s_text   LIKE LINE OF s_field-texts.
  DATA f_clear  TYPE abap_bool.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE result
  SELECT prog dnum INTO TABLE t_dynpros
    FROM d020s
    WHERE prog = program
      AND type NOT IN (c_type_dynp_select_norm, c_type_dynp_select_mod, c_type_dynp_select_incl).

  SELECT *
    INTO TABLE t_d021t
    FROM d021t
    WHERE prog = program
      AND dtxt <> space.

*  loop at result assigning <r>.
  LOOP AT t_dynpros ASSIGNING <r>.
    CLEAR s_result.

* Processing Dynpro #&2 (&1)
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE i500 WITH space space. ENDIF.
    SET EXTENDED CHECK ON.
    application_log->add_info( id_msgid = 'ZAPLINK'
                               id_msgno = '500'
                               id_msgv1 = <r>-prog
                               id_msgv2 = <r>-dnum ).

    SELECT lang dtxt
      INTO CORRESPONDING FIELDS OF TABLE s_result-texts
      FROM d020t
      WHERE prog = <r>-prog
        AND dynr = <r>-dnum
        AND dtxt <> space.

*    dynp_id-prog = <r>-prog.
*    dynp_id-dnum = <r>-dnum.
*    IMPORT DYNPRO header t_fields t_flowlogic <r>-matchcodes ID dynp_id.
*    IF sy-subrc <> 0.
*    ENDIF.
    CALL FUNCTION 'RPY_DYNPRO_READ'
      EXPORTING
        progname                    = <r>-prog
        dynnr                       = <r>-dnum
*       SUPPRESS_EXIST_CHECKS       = ' '
*       SUPPRESS_CORR_CHECKS        = ' '
      IMPORTING
        header                      = s_result-hdr
      TABLES
        containers                  = t_container
        fields_to_containers        = t_fields
        flow_logic                  = t_flowlogic
        params                      = s_result-matchcodes
      EXCEPTIONS
        cancelled                   = 1
        not_found                   = 2
        permission_error            = 3
        OTHERS                      = 4.
    IF sy-subrc <> 0.
    ENDIF.

    LOOP AT t_container ASSIGNING <c>.
      CLEAR s_cont.
      IF <c>-type <> 'SCREEN'.
* Code from SAPLSIFP form I_CONT_TO_FIELD : LSIFPF11(1479->1493)
* When importing dynpro and exporting again C_LINE_MIN or C_COLN_MIN are blank sometimes despite in source system they are set to 1
* After analyzing both read and write dynpro it apears that when writing C_LINE_MIN is not transfered to ADEZ if C_RESIZE_V is initial
* We have to set the set rule manualy to ensure that unmodified screen will be exported with the same data.
        IF <c>-c_resize_v IS INITIAL.   CLEAR <c>-c_line_min.   ENDIF.
        IF <c>-c_resize_h IS INITIAL.   CLEAR <c>-c_coln_min.   ENDIF.
      ENDIF.
      s_cont-hdr = <c>.
      LOOP AT t_fields ASSIGNING <f>
           WHERE cont_type = <c>-type
             AND cont_name = <c>-name.
        CLEAR: s_field, f_clear.
        s_field-hdr = <f>.
        CLEAR: s_field-cont_type, s_field-cont_name.
        IF <f>-type IN r_text_fields.       " field may have texts
          LOOP AT t_d021t ASSIGNING <t>
               WHERE dynr = <r>-dnum
                 AND fldn = <f>-name.
            CLEAR s_text.
            s_text-langu = <t>-lang.
            s_text-short_txt = <t>-dtxt.
            INSERT s_text INTO TABLE s_field-texts.
            IF <t>-dtxt = s_field-text. f_clear = abap_true. ENDIF.
          ENDLOOP.
          IF NOT s_field-texts IS INITIAL AND NOT s_field-text CO '_' AND f_clear = abap_true. CLEAR s_field-text. ENDIF.
        ENDIF.
        INSERT s_field INTO TABLE s_cont-fields.
      ENDLOOP.
      INSERT s_cont INTO TABLE s_result-containers.
    ENDLOOP.

    s_result-flow_logic-_ = conv_flowlogic2abapsource( t_flowlogic ).

    CLEAR s_result-program.

    INSERT s_result INTO TABLE result.
  ENDLOOP.
  endmethod.


  method GET_MENUS.
  DATA s_fm_data TYPE zaplink_menupainter_data=>ts_fm_data.
  DATA d_state   TYPE progdir-state.
  DATA rcode(5)  TYPE c.

  d_state = 'I'.      " Inactive
  PERFORM version_check IN PROGRAM saplsmpi USING program d_state CHANGING rcode.
  IF sy-subrc <> 0 OR NOT rcode IS INITIAL.   d_state = 'A'.    ENDIF.

  CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
    EXPORTING
      program                    = program
*     LANGUAGE                   =
      state                      = d_state
*     WITH_SECOND_LANGUAGE       = ' '
    IMPORTING
      adm                        = s_fm_data-header
*      LANGU                      =
*      AUTHOR                     =
*      DATE                       =
*      TIME                       =
*      CAUTHOR                    =
*      CDATE                      =
*      CTIME                      =
*      GDATE                      =
*      GTIME                      =
    TABLES
      sta                        = s_fm_data-status
      fun                        = s_fm_data-functions
      men                        = s_fm_data-menus_det
      mtx                        = s_fm_data-menus
      act                        = s_fm_data-bars_det
      but                        = s_fm_data-buttons_det
      pfk                        = s_fm_data-keyboards
      set                        = s_fm_data-stat_funcs
      doc                        = s_fm_data-attributes
      tit                        = s_fm_data-titles
      biv                        = s_fm_data-ffuncs
    EXCEPTIONS
      not_found                  = 1
      unknown_version            = 2
      OTHERS                     = 3.
  IF sy-subrc <> 0.
    mac_add_mf_and_raise 'RS_CUA_INTERNAL_FETCH' sy-subrc.
  ELSEIF s_fm_data IS INITIAL.
    EXIT.     " No Menu
  ENDIF.

  SELECT *
    INTO TABLE s_fm_data-texts
    FROM rsmptexts
    WHERE progname = program.

  CREATE OBJECT result.
  result->from_data( s_fm_data ).
  endmethod.


  method GET_PROG_RAWSOURCE.
*  DATA d_o_type TYPE  seu_objtyp.
*  DATA d_o_name TYPE  seu_objkey.
*  DATA d_object     TYPE e071-object.
  DATA d_obj_name   TYPE e071-obj_name.
  DATA d_is_work_i  TYPE abap_bool.
  DATA d_is_inactiv TYPE abap_bool.
*  DATA d_prog       TYPE progdir-name.

*  d_is_inactiv = '_'.
*  CALL FUNCTION 'RS_PROGNAME_SPLIT'
*    EXPORTING
*      progname_with_namespace           = program
**   IMPORTING
**     NAMESPACE                         =
**     PROGNAME_WITHOUT_NAMESPACE        =
**     FUGR_IS_NAME                      =
**     FUGR_IS_RESERVED_NAME             =
**     FUGR_IS_FUNCTIONPOOL_NAME         =
**     FUGR_IS_INCLUDE_NAME              =
**     FUGR_IS_FUNCTIONMODULE_NAME       =
**     FUGR_IS_HIDDEN_NAME               =
**     FUGR_GROUP                        =
**     FUGR_INCLUDE_NUMBER               =
**     FUGR_SUFFIX                       =
**     FUGR_IS_RESERVED_EXIT_NAME        =
**     SLDB_IS_RESERVED_NAME             =
**     SLDB_LOGDB_NAME                   =
**     MST_IS_RESERVED_NAME              =
**     TYPE_IS_RESERVED_NAME             =
**     TYPE_NAME                         =
**     MENU_IS_RESERVED_NAME             =
**     MENU_NAME                         =
**     CLASS_IS_RESERVED_NAME            =
**     CLASS_IS_NAME                     =
**     CLASS_NAME                        =
**     CLASS_IS_METHOD_NAME              =
**     CLASS_METHOD_NAME                 =
**     CNTX_IS_RESERVED_NAME             =
*    EXCEPTIONS
*      delimiter_error                   = 1
*      OTHERS                            = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  CALL METHOD cl_wb_source_req_dispatcher=>get_object_from_sourcename
*    EXPORTING
*      p_source_name       = program
*    IMPORTING
*      p_object_type       = d_o_type
*      p_object_name       = d_o_name
*    EXCEPTIONS
*      no_objecttype_found = 1
*      OTHERS              = 2.
*  IF sy-subrc = 0.
*CALL METHOD cl_wb_cleditor=>convert_type_and_name
*  EXPORTING
*    object_type   =
*    mtdkey        =
*    section_limu  =
*  IMPORTING
*    limu          =
*    incname       =
*    cpname        =
*    objtype       =
*    extension     =
*  EXCEPTIONS
*    not_converted = 1
*    others        = 2
*        .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*    CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
*      EXPORTING
*        object                        = d_object
*        obj_name                      = d_obj_name
**     GLOBAL_CHECK                  = ' '
**     MODE                          = 'S'
*      IMPORTING
**     OBJECT_IS_WORK_ITEM           =
*        object_inactive_version       = d_is_inactiv.
*  ENDIF.

* Issue 73 : Detecting when inactive version exists
  d_obj_name = program.
  CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
    EXPORTING
      object                        = 'REPS'
      obj_name                      = d_obj_name
*     GLOBAL_CHECK                  = ' '
*     MODE                          = 'S'
    IMPORTING
      object_is_work_item           = d_is_work_i     " My inactive object
      object_inactive_version       = d_is_inactiv.

* Backup solution not very efficient
*  IF d_is_inactiv = '_'.
*    SELECT SINGLE program INTO d_prog
*      FROM progdir
*      WHERE name = program
*       AND state = 'I'.
*    IF sy-subrc = 0.
*      d_is_inactiv = abap_true.
*    ELSE.
*      CLEAR d_is_inactiv.
*    ENDIF.
*  ENDIF.

  IF NOT d_is_work_i IS INITIAL OR NOT d_is_inactiv IS INITIAL.
    READ REPORT program INTO result STATE 'I'.
  ELSE.
    sy-subrc = 4.
  ENDIF.
  IF sy-subrc <> 0.
    READ REPORT program INTO result STATE 'A'.
  ENDIF.
  endmethod.


  method GET_PROG_SOURCE.
  DATA code TYPE tt_abaprawsource.

  code = get_prog_rawsource( program ).
  result = conv_abap_raw2source( code ).
  endmethod.


  method GET_PROG_TEXTPOOL.
  DATA t_textpool TYPE STANDARD TABLE OF textpool.
  DATA t_langlist TYPE instlang.
  TYPES:
    BEGIN OF ts_text,
      id      TYPE  textpoolid,
      key	    TYPE  textpoolky,
      langu   TYPE spras,
      entry	  TYPE textpooltx,
      length  TYPE textpoolln,
    END OF ts_text.
  DATA: t_texts TYPE STANDARD TABLE OF ts_text.
  DATA _text LIKE LINE OF t_texts.
  DATA _textpool LIKE LINE OF result.
  DATA _tpool LIKE LINE OF _textpool-texts.
  FIELD-SYMBOLS:
    <l> LIKE LINE OF t_langlist,
    <t> LIKE LINE OF t_textpool.

  CALL FUNCTION 'RS_TEXTLOG_GET_PARAMETERS'
    CHANGING
      installed_languages = t_langlist.

  LOOP AT t_langlist ASSIGNING <l>.
    READ TEXTPOOL program INTO t_textpool LANGUAGE <l>.
    CHECK sy-subrc = 0.
    _text-langu = <l>.
    LOOP AT t_textpool ASSIGNING <t>.
      _text-id = <t>-id.
      _text-key = <t>-key.
      _text-entry = <t>-entry.
      _text-length = <t>-length.
      APPEND _text TO t_texts.
    ENDLOOP.
  ENDLOOP.

  SORT t_texts BY id key langu.
  DELETE ADJACENT DUPLICATES FROM t_texts COMPARING id key langu.

  LOOP AT t_texts INTO _text.
    AT NEW key. " ID
      CLEAR _textpool.
      _textpool-id = _text-id.
      _textpool-key = _text-key.
    ENDAT.
    AT NEW langu.
      CLEAR _tpool.
      _tpool-langu = _text-langu.
    ENDAT.

    _tpool-entry = _text-entry.
    _tpool-length = _text-length.

    AT END OF langu.
      APPEND _tpool TO _textpool-texts.
    ENDAT.
    AT END OF key.
      APPEND _textpool TO result.
    ENDAT.

  ENDLOOP.
  endmethod.


  method GET_TYPE_FROM_XML.
  DATA root_node TYPE REF TO if_ixml_node.
  DATA o_raw     TYPE to_raw_base.
  DATA _name     TYPE string.

  CHECK object IS BOUND.

  root_node = object->get_root_element( ).
  IF NOT root_node IS BOUND.
    _name = '$NONE$'(001).
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
         textid    = zaplink_cx_connector=>invalid_xml
         node_name = _name.
  ENDIF.

  _name = root_node->get_name( ).
  IF _name <> comp_nodename.
    RAISE EXCEPTION TYPE zaplink_cx_connector
      EXPORTING
        textid   = zaplink_cx_connector=>invalid_xml
        node_name = _name.
  ENDIF.

*  TRY.
      o_raw = zaplink_connectors=>node_2_raw( root_node ).
      result = o_raw->type.
*    CATCH zaplink_cx_connector INTO o_mycx.
*      RAISE EXCEPTION o_mycx.
*  ENDTRY.
  endmethod.


  method SET_BASE_ATTRIBUTS.
  DATA o_desc    TYPE REF TO cl_abap_typedescr.
  DATA string    TYPE string.
  DATA _name     TYPE tadir-obj_name.
  DATA _type     TYPE tadir-object.
  DATA _kind     TYPE tadir-pgmid.

  o_desc = cl_abap_typedescr=>describe_by_object_ref( p_object_ref = me ).
  string = o_desc->get_relative_name( ).

*  TRY.
      _name = attributs-name = component->get_name( ).
      _type = component->get_type( ).
*    CATCH zaplink_cx INTO o_cx.
*      mac_cascade_raise o_mycx o_cx.
*  ENDTRY.
  attributs-class = string.
  attributs-version = me->version.

  _kind = zaplink_connectors=>get_typekind( _type ).

  CALL FUNCTION 'TRINT_TADIR_QUERY'
    EXPORTING
      iv_pgmid           = _kind
      iv_object          = _type
      iv_obj_name        = _name
    IMPORTING
*     EV_OBJECT          =
*     EV_SRCSYSTEM       =
*     EV_AUTHOR          =
*     EV_GENFLAG         =
*     EV_EXIST           =
      ev_devclass        = attributs-devclass.
  endmethod.


  method SET_DYNPROS.
  INCLUDE mseusbit. " for c_type_dynp_select_norm, c_type_dynp_select_mod, c_type_dynp_select_incl
  DATA:
    BEGIN OF dynp_id,
      prog TYPE d020s-prog,
      dnum TYPE d020s-dnum,
    END OF dynp_id.
  DATA header   TYPE rpy_dyhead.
  DATA t_flowlogic TYPE tt_flow_logic.
*  DATA t_dp_txt TYPE d020t-dtxt.
  DATA t_containers TYPE dycatt_tab.
  DATA s_cont  LIKE LINE OF t_containers.
  DATA t_fields TYPE dyfatc_tab.
  DATA s_field LIKE LINE OF t_fields.
  DATA t_descs TYPE SORTED TABLE OF d020t WITH UNIQUE KEY prog dynr.
  DATA s_desc LIKE LINE OF t_descs.
  DATA t_texts TYPE SORTED TABLE OF d021t WITH UNIQUE KEY prog dynr fldn lang.
  DATA s_text LIKE LINE OF t_texts.
  DATA f_fill TYPE abap_bool.
  FIELD-SYMBOLS:
    <r> LIKE LINE OF dynpros,
    <l> LIKE LINE OF <r>-texts,
    <c> LIKE LINE OF <r>-containers,
    <f> LIKE LINE OF <c>-fields,
    <t> LIKE LINE OF <f>-texts.

  CLEAR o_mycx.
  LOOP AT dynpros ASSIGNING <r>.
    CLEAR: header, t_containers, t_fields, t_texts, t_descs.
    header = <r>-hdr.
    header-program = program.

* Processing Dynpro #&2 (&1)
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE i500 WITH space space. ENDIF.
    SET EXTENDED CHECK ON.
    application_log->add_info( id_msgid = 'ZAPLINK'
                               id_msgno = '500'
                               id_msgv1 = header-program
                               id_msgv2 = header-screen ).

* Texts
    LOOP AT <r>-texts ASSIGNING <l>.
      CLEAR s_desc.
      MOVE-CORRESPONDING <l> TO s_desc.
      INSERT s_desc INTO TABLE t_descs.
    ENDLOOP.

    t_flowlogic = conv_abapsource2flowlogic( <r>-flow_logic-_ ).
    LOOP AT <r>-containers ASSIGNING <c>.
      CLEAR s_cont.
      s_cont = <c>-hdr.
      LOOP AT <c>-fields ASSIGNING <f>.
        s_field = <f>-hdr.
        s_field-cont_type = <c>-type.
        s_field-cont_name = <c>-name.
        IF s_field-text IS INITIAL. f_fill = abap_true. ELSE. f_fill = abap_false. ENDIF.    " protect data
        LOOP AT <f>-texts ASSIGNING <t>.
          IF f_fill = abap_true AND ( sy-tabix = 1 OR <t>-langu = sy-langu ). s_field-text = <t>-short_txt. ENDIF.
          CLEAR s_text.
          s_text-prog = header-program.
          s_text-dynr = header-screen.
          s_text-fldn = <f>-name.
          s_text-lang = <t>-langu.
          s_text-dtxt = <t>-short_txt.
          INSERT s_text INTO TABLE t_texts.
        ENDLOOP.
        INSERT s_field INTO TABLE t_fields.
      ENDLOOP.
      INSERT s_cont INTO TABLE t_containers.
    ENDLOOP.
*    CALL FUNCTION 'RS_SCRP_DYNPRO_CHECK'
*      TABLES
*        fieldlist            = <r>-fields
*        flowlogic            = t_flowlogic
*        params               = <r>-matchcodes
*      CHANGING
*        header               = header
*      EXCEPTIONS
*        damaged_but_repaired = 0
*        damaged              = 2
*        OTHERS               = 3.
*    IF sy-subrc <> 0.
*      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'RS_SCRP_DYNPRO_CHECK'
*                                                  subrc = sy-subrc
*                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
*      application_log->add_symsg( ).
*      application_log->add_exception( o_mycx ).
*      continue.
*    endif.
    SORT t_containers BY element_of name.
    CALL FUNCTION 'RPY_DYNPRO_INSERT'
      EXPORTING
*       SUPPRESS_CORR_CHECKS           = ' '
*       CORRNUM                        = ' '
        suppress_exist_checks          = abap_true
*       SUPPRESS_GENERATE              = ' '
*       SUPPRESS_DICT_SUPPORT          = ' '
*       SUPPRESS_EXTENDED_CHECKS       = ' '
        header                         = header
*       USE_CORRNUM_IMMEDIATEDLY       = ' '
*       SUPPRESS_COMMIT_WORK           = ' '
      TABLES
        containers                     = t_containers
        fields_to_containers           = t_fields
        flow_logic                     = t_flowlogic
        params                         = <r>-matchcodes
      EXCEPTIONS
        cancelled                      = 1
        already_exists                 = 2
        program_not_exists             = 3
        not_executed                   = 4
        missing_required_field         = 5
        illegal_field_value            = 6
        field_not_allowed              = 7
        not_generated                  = 8
        illegal_field_position         = 9
        OTHERS                         = 10.
    IF sy-subrc <> 0.
*      TRY.
          o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'RPY_DYNPRO_INSERT'
                                                      subrc = sy-subrc
                                                  classname = 'ZAPLINK_CX_CONNECTOR' ).
*        CATCH zaplink_cx INTO o_cx.
*          mac_cascade_raise o_mycx o_cx.
*      ENDTRY.
      application_log->add_symsg( ).
      application_log->add_exception( o_mycx ).
      CONTINUE.
    ENDIF.
    MODIFY d020t FROM TABLE t_descs.
    MODIFY d021t FROM TABLE t_texts.
*    CALL FUNCTION 'RPY_DYNPRO_INSERT_NATIVE'
*      EXPORTING
**     SUPPRESS_CORR_CHECKS           = ' '
**     CORRNUM                        = ' '
*        header                         = header
*        dynprotext                     = d_dp_txt
**     SUPPRESS_EXIST_CHECKS          = ' '
**     USE_CORRNUM_IMMEDIATEDLY       = ' '
**     SUPPRESS_COMMIT_WORK           = ' '
*      TABLES
*        fieldlist                      = <r>-fields
*        flowlogic                      = t_flowlogic
*        params                         = <r>-matchcodes
*      EXCEPTIONS
*        cancelled                      = 1
*        already_exists                 = 2
*        program_not_exists             = 3
*        not_executed                   = 4
*        OTHERS                         = 5.
*    IF sy-subrc <> 0.
*      o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'RPY_DYNPRO_INSERT_NATIVE'
*                                                  subrc = sy-subrc
*                                              classname = 'ZAPLINK_CX_CONNECTOR' ).
*      application_log->add_symsg( ).
*      application_log->add_exception( o_mycx ).
*    ENDIF.
  ENDLOOP.
  IF o_mycx IS BOUND.   RAISE EXCEPTION o_mycx.   ENDIF.
  endmethod.


  method SET_MENUS.
  DATA s_fm_data TYPE zaplink_menupainter_data=>ts_fm_data.
  DATA d_trkey   TYPE trkey.
  DATA d_lang    TYPE sy-langu.

  CHECK menus IS BOUND.
  s_fm_data = menus->to_data( program ).

  d_trkey-obj_type = 'PROG'.
  d_trkey-sub_name = d_trkey-obj_name = program.
  d_trkey-sub_type = 'CUAD'.
  d_lang = s_fm_data-header-mod_langu.
  IF d_lang IS INITIAL.   d_lang = sy-langu.    ENDIF.

  CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
    EXPORTING
      program   = program
      language  = d_lang
      tr_key    = d_trkey
      adm       = s_fm_data-header
      state     = 'I'
    TABLES
      sta       = s_fm_data-status
      fun       = s_fm_data-functions
      men       = s_fm_data-menus_det
      mtx       = s_fm_data-menus
      act       = s_fm_data-bars_det
      but       = s_fm_data-buttons_det
      pfk       = s_fm_data-keyboards
      set       = s_fm_data-stat_funcs
      doc       = s_fm_data-attributes
      tit       = s_fm_data-titles
      biv       = s_fm_data-ffuncs
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    mac_add_mf_and_raise 'RS_CUA_INTERNAL_WRITE' sy-subrc.
  ENDIF.

  DELETE FROM rsmptexts WHERE progname = program.
  MODIFY rsmptexts FROM TABLE s_fm_data-texts.
  endmethod.


  method SET_PROG_RAWSOURCE.
  INSERT REPORT program FROM raw STATE 'I'.
  IF sy-subrc = 0.
    INSERT REPORT program FROM raw STATE 'A'.
  ENDIF.
  endmethod.


  method SET_PROG_SOURCE.
DATA _raw TYPE tt_abaprawsource.

  _raw = conv_abap_source2raw( source ).
  set_prog_rawsource( program = program
                          raw = _raw ).
  endmethod.


  method SET_PROG_TEXTPOOL.
  TYPES:
    BEGIN OF ts_text,
      langu   TYPE spras,
      id      TYPE  textpoolid,
      key	    TYPE  textpoolky,
      entry	  TYPE textpooltx,
      length  TYPE textpoolln,
    END OF ts_text.
  DATA t_textpool TYPE STANDARD TABLE OF textpool.
  DATA: t_texts TYPE STANDARD TABLE OF ts_text.
  DATA _text LIKE LINE OF t_texts.
  DATA _textpool LIKE LINE OF t_textpool.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF textpool,
    <l> LIKE LINE OF <t>-texts.

  LOOP AT textpool ASSIGNING <t>.
    _text-id = <t>-id.
    _text-key = <t>-key.
    LOOP AT <t>-texts ASSIGNING <l>.
      _text-langu = <l>-langu.
      _text-entry = <l>-entry.
      _text-length = <l>-length.
      APPEND _text TO t_texts.
    ENDLOOP.
  ENDLOOP.

  SORT t_texts BY langu id key.
  DELETE ADJACENT DUPLICATES FROM t_texts COMPARING langu id key.

  LOOP AT t_texts INTO _text.
    AT NEW langu. " ID
      CLEAR: t_textpool, _textpool.
    ENDAT.

    _textpool-id = _text-id.
    _textpool-key = _text-key.
    _textpool-entry = _text-entry.
    _textpool-length = _text-length.
    APPEND _textpool TO t_textpool.

    AT END OF langu.
      INSERT textpool program FROM t_textpool LANGUAGE _text-langu STATE 'I'.   " Issue 68
      INSERT textpool program FROM t_textpool LANGUAGE _text-langu STATE 'A'.   " Issue 68
    ENDAT.

  ENDLOOP.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_intf TYPE ti_cleanning_4data.
* Must be call backward child to parent
  CHECK object IS BOUND.
  TRY.
      TRY.
          o_intf ?= object.
        CATCH cx_sy_move_cast_error.    " Auto anonymize can't be done
      ENDTRY.
      IF o_intf is bound.     o_intf->anonymize( ).     ENDIF.
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  TRY.
      object->anonymize( ).
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE.
  DATA o_intf TYPE ti_cleanning_4data.
* Must be call backward child to parent
  CHECK object IS BOUND.
  TRY.
      TRY.
          o_intf ?= object.
        CATCH cx_sy_move_cast_error.    " Auto anonymize can't be done
      ENDTRY.
      IF o_intf is bound.       o_intf->unanonymize( ).     endif.
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  TRY.
      object->unanonymize( ).
    CATCH zaplink_cx INTO o_cx.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
* Method implemented empty by default because most of SAP Component do not have source code.
* This prevent connector using inheriting from this object to have to implement an empty method.
  endmethod.


  method ZAPLINK_CONNECTOR~EXPORT_FROM_SAP.
  DATA ez_xml          TYPE to_ez_xml.
  DATA o_data          TYPE to_raw.

  o_data = read_from_sap( component ).
  IF o_data IS BOUND.
    o_data->update_connector_data( me ).
    CREATE OBJECT ez_xml.

    TRY.
        object = ez_xml->any2xml( any       = o_data
                                  type      = comp_nodename ).
      CATCH zaplink_cx INTO o_cx.
        mac_cascade_raise o_mycx o_cx.
    ENDTRY.
  ENDIF.
  endmethod.


  method ZAPLINK_CONNECTOR~GET_VALUES.
  DATA _obj_type TYPE  euobj-id.
  DATA _name TYPE td_compname.

  _obj_type = type.

  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type           = _obj_type
      object_name           = name
      suppress_selection    = abap_true
      use_alv_grid          = abap_false
      without_personal_list = abap_false
    IMPORTING
      object_name_selected  = _name
    EXCEPTIONS
      cancel                = 1.
  IF sy-subrc = 0.
    name = _name.
  ENDIF.
  endmethod.


  method ZAPLINK_CONNECTOR~IMPORT_TO_SAP.
  DATA root_node TYPE REF TO if_ixml_node.
  DATA o_raw_b   TYPE to_raw_base.
  DATA o_raw     TYPE to_raw.
  DATA _name     TYPE string.
*  DATA wa_type   TYPE e071-object.
*  DATA wa_name   TYPE e071-obj_name.
  DATA wa_flag   TYPE rs38l-head.
  DATA ez_xml    TYPE to_ez_xml.
  DATA o_cx      TYPE REF TO cx_root.
*  DATA o_zlcx    TYPE REF TO zaplink_cx.     " Issue 93
  DATA t_bapiret TYPE bapirettab.
  DATA f_is_wi TYPE abap_bool.

  CHECK object IS BOUND.

  TRY.
      root_node = object->get_root_element( ).
      IF NOT root_node IS BOUND.
        _name = '$NONE$'(001).
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
              textid    = zaplink_cx_connector=>invalid_xml
              node_name = _name.
      ENDIF.

      _name = root_node->get_name( ).
      IF _name <> comp_nodename.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
              textid    = zaplink_cx_connector=>invalid_xml
              node_name = _name.
      ENDIF.

      TRY.
          o_raw_b = zaplink_connectors=>node_2_raw( root_node ).
        CATCH zaplink_cx_connector INTO o_mycx.
          RAISE EXCEPTION o_mycx.
      ENDTRY.

* Version handling to develop
      IF is_supported_version( o_raw_b->version ) = abap_false.
* Version '&CONN_VER&' is not (or no longer) suported by this connector.
        RAISE EXCEPTION TYPE zaplink_cx_connector
          EXPORTING
            textid   = zaplink_cx_connector=>unsuported_version
            conn_ver = o_raw_b->version.
      ENDIF.

      o_raw = create_new_raw( o_raw_b->type ).
      CREATE OBJECT ez_xml.
      CALL METHOD ez_xml->xml2any
        EXPORTING
          xmldoc = object
        CHANGING
          any    = o_raw.
      IF ez_xml->application_log->has_messages_of_msgt( id_msgty = 'W') = abap_true.
*        CREATE OBJECT o_comp.   o_comp->set_type( o_raw_b->type ).    o_comp->set_name( o_raw_b->name ).
*        mac_create_log o_msgs ballog_subobject space.   o_comp->set_msg_coll( o_msgs ).
        application_log->add_from_instance( io_msglist = ez_xml->application_log
                                     if_add_as_subnode = abap_true ).
        DATA t_handle TYPE bal_t_logh.
        DATA s_profil TYPE bal_s_prof.
        DATA s_choice TYPE bal_s_excm.

        APPEND application_log->md_handle TO t_handle.
        CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
          IMPORTING
            e_s_display_profile = s_profil
          EXCEPTIONS
            OTHERS              = 0.
        s_profil-langu = sy-langu.
* Error with &1 &2 : Confirm or abort this comp.
        MESSAGE i010(zaplink_easyxml) WITH o_raw_b->type o_raw_b->name INTO s_profil-title.
        s_profil-use_grid = s_profil-no_toolbar = s_profil-show_all = s_profil-cwidth_opt = abap_true.
*        s_profil-tree_ontop = abap_false. = s_profil-pop_adjst = s_profil-tree_adjst
* XML Errors for component &1 &2
        MESSAGE i009(zaplink_easyxml) WITH o_raw_b->type o_raw_b->name INTO s_profil-grid_title-gridtitle.
        s_profil-start_col = s_profil-start_row = 5.
        s_profil-end_col = 120.
        s_profil-end_row = 40.
        s_profil-head_size = 10.

* Issue 45
        CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
          EXPORTING
            i_s_display_profile    = s_profil
            i_t_log_handle         = t_handle
*           I_T_MSG_HANDLE         =
*           I_S_LOG_FILTER         =
*           I_S_MSG_FILTER         =
*           I_T_LOG_CONTEXT_FILTER =
*           I_T_MSG_CONTEXT_FILTER =
            i_amodal               = abap_false
*           I_SRT_BY_TIMSTMP       = ' '
          IMPORTING
            e_s_exit_command       = s_choice
          EXCEPTIONS
            profile_inconsistent   = 1
            internal_error         = 2
            no_data_available      = 3
            no_authority           = 4
            OTHERS                 = 5.
        IF sy-subrc <> 0 OR s_choice-continue IS INITIAL.
          CREATE OBJECT o_mycx
            EXPORTING
              textid = zaplink_cx_connector=>xml_error
              type   = o_raw_b->type
              name   = o_raw_b->name.
          o_mycx->update( ).
          RAISE EXCEPTION o_mycx.
        ENDIF.
      ENDIF.

* Add to worklist
*      wa_type = o_raw->type.
*      wa_name = o_raw->name.
      zaplink_tools=>add_comp_to_working_area( type = o_raw->type
                                               name = o_raw->name ).
*      IF zaplink_tools=>is_working_areable( o_raw->type ) = abap_true.
*        CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
*          EXPORTING
*            object                  = wa_type
*            obj_name                = wa_name
**            DELETED_FLAG            = ' '
*          IMPORTING
*            object_inserted         = wa_flag
*          EXCEPTIONS
*            wrong_object_name       = 1
*            OTHERS                  = 2.
*        IF sy-subrc <> 0.
*          mac_add_mf_and_raise 'RS_INSERT_INTO_WORKING_AREA' sy-subrc.
*        ENDIF.
*      ELSE.   " Security Remove old eroneous entries
*        CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
*          EXPORTING
*            object                        = wa_type
*            obj_name                      = wa_name
**            GLOBAL_CHECK                  = ' '
**            MODE                          = 'S'
*          IMPORTING
*            object_is_work_item           = f_is_wi
**             OBJECT_INACTIVE_VERSION       =
*                  .
*        IF f_is_wi = abap_true.   " shouldn't be the case
*          CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
*            EXPORTING
*              object                         = wa_type
*              obj_name                       = wa_name
*              immediate                      = abap_true
**              OWN_SET_ONLY                   = ' '
*              actualize_working_area         = abap_false
**              ADJUST_OBJECT_INPUT_DATA       = 'X'
*                    .
*        ENDIF.
*      ENDIF.

      components = write_to_sap( o_raw ).
      IF components IS NOT BOUND.   CREATE OBJECT components.   ENDIF.
*      o_c_in_l = components->search( o_comp ).
*      IF o_c_in_l IS INITIAL.
*        components->add( o_comp ).
*      ELSE.
*        o_msgs_2 = o_c_in_l->get_msg_coll( ).
*        IF NOT o_msgs_2 IS INITIAL.   o_msgs->add_from_instance( io_msglist = o_msgs_2 ).   ENDIF.
*        o_c_in_l->set_msg_coll( o_msgs ).
*      ENDIF.
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
*    CATCH zaplink_cx INTO o_zlcx.      " Issue 93
*      CREATE OBJECT o_mycx
*        EXPORTING
*          textid  = zaplink_cx=>system_error
*          previous = o_zlcx.
*      o_mycx->update( ).
*      RAISE EXCEPTION o_mycx.
    CATCH cx_root INTO o_cx.
      CREATE OBJECT o_mycx
        EXPORTING
          textid   = zaplink_cx=>system_error
          previous = o_cx.
      o_mycx->update( ).
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.
ENDCLASS.
