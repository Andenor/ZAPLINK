class ZAPLINK_PROGRAM definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public

  global friends ZAPLINK_PROG_DATA
                 ZAPLINK_VARI_DATA
                 ZAPLINK_VARI_RAW .

public section.
  type-pools ABAP .
  type-pools RSDS .

  types TR_SO_VALUE type ZAPLINK_VARI_DATA=>TR_STRING .
  types TD_VARIANT type VARID-VARIANT .
  types TT_VARIANT_DATA type ZAPLINK_VARI_DATA=>TT_PARAMS .
  types T_RAW type RSWSOURCET .
  types:
    BEGIN OF ts_text,
        langu   TYPE spras,
        entry	  TYPE textpooltx,
        length  TYPE textpoolln,
      END OF ts_text .
  types:
    tt_texts TYPE SORTED TABLE OF ts_text WITH UNIQUE KEY langu .
  types:
    BEGIN OF ts_textpool,
        id    TYPE  textpoolid,
        key	  TYPE  textpoolky,
        texts TYPE  tt_texts,
      END OF ts_textpool .
  types:
    tt_textpools TYPE SORTED TABLE OF ts_textpool WITH UNIQUE KEY id KEY .
  types TS_SOURCE type STRING .
  types:
    begin of ts_so_value,
        sign    type BAPISIGN,
        option  type BAPIOPTION,
        low     type string,
        high    type string,
      END OF ts_so_value .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_PROGRAM' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .
  methods GET_PROGRAM_SIGNATURE
    importing
      !PROGRAM_NAME type TD_PROGNAME
    returning
      value(RESULT) type ZAPLINK_DATATYPES~TD_CHECKSUM .
  methods GET_VARIANT_PARAMS
    importing
      !PROGRAM type TD_PROGNAME
      !VARIANT type TD_VARIANT
    returning
      value(RESULT) type TT_VARIANT_DATA .
  methods SET_PARAM_VALUE
    returning
      value(VALUE) type STRING .
  methods SET_SELECTOPTION_VALUE
    returning
      value(VALUE) type TR_SO_VALUE .
  methods SET_VARIANT_PARAMS
    importing
      !PROGRAM type TD_PROGNAME
      !VARIANT type TD_VARIANT
      value(DATA) type TT_VARIANT_DATA
    returning
      value(RESULT) type ABAP_BOOL .
  methods GET_PARAM_VALUE
    importing
      value(VALUE) type STRING .
  methods GET_SELECTOPTION_VALUE
    importing
      !VALUE type ANY TABLE .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE
    redefinition .
  methods ZAPLINK_CONNECTOR~CREATE_NEW_RAW
    redefinition .
  methods ZAPLINK_CONNECTOR~DELETE_FROM_SAP
    redefinition .
  methods ZAPLINK_CONNECTOR~DO_EXISTS
    redefinition .
  methods ZAPLINK_CONNECTOR~GET_VALUES
    redefinition .
  methods ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION
    redefinition .
  methods ZAPLINK_CONNECTOR~READ_FROM_SAP
    redefinition .
  methods ZAPLINK_CONNECTOR~WRITE_TO_SAP
    redefinition .
protected section.

  types TO_PROGRAM type ref to ZAPLINK_PROG_DATA .
  types TO_VARIANT type ref to ZAPLINK_VARI_DATA .
  types:
    tt_variant_list TYPE STANDARD TABLE OF varid WITH DEFAULT KEY .
  types TT_VARIANTS type ZAPLINK_PROG_DATA=>TT_VARI_DATA .
  types TO_RAW_VARIANT type ZAPLINK_PROG_DATA=>TO_VARIANT .

  constants:
    BEGIN OF supportedtypes,
      program          TYPE td_comptype VALUE 'PROG',       "#EC NOTEXT
      sys_variant      TYPE td_comptype VALUE 'VARI',       "#EC NOTEXT
      appl_variant     TYPE td_comptype VALUE 'VARX',       "#EC NOTEXT
    END OF supportedtypes .

  methods READ_VARIANTS
    importing
      !VARIANTS type TT_VARIANT_LIST
    returning
      value(RESULT) type TT_VARIANTS .
  methods WRITE_VARIANTS
    importing
      !VARIANTS type TT_VARIANTS
    returning
      value(RESULT) type TT_VARIANTS .
private section.

  types:
    BEGIN OF ts_prog_attr.
  INCLUDE TYPE ts_base_attributs AS base.
  TYPES:
    END OF ts_prog_attr .
  types TS_FM_DATA type ZAPLINK_PROG_DATA=>TS_FM_DATA .
  types TS_VARIANT_KEY type ZAPLINK_VARI_DATA=>TS_VARIANT_KEY .
  types TS_FMD_VARI type ZAPLINK_VARI_DATA=>TS_FM_DATA .

  class-data R_DOC_ID type TR_DOCID .
  constants _UUID type TD_CONNUUID value 'AAAD0A4B07A5A05AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  data CURRENT_PARAM type ZAPLINK_VARI_DATA=>TS_PARAM_DATA .
  constants:
    BEGIN OF flags,       " include RSDBCOM2
      BEGIN OF varid,
        noimp TYPE x VALUE '10',
        imp TYPE x VALUE 'EF',
        allscreens TYPE x VALUE '20',
        not_all_screens TYPE x VALUE '20',
        screens TYPE x VALUE 'DF',
      END OF varid,
      BEGIN OF vari,
        nospagpa TYPE x VALUE '40',
        spagpa TYPE x VALUE 'BF',
        noint  TYPE x VALUE '80',
        obli   TYPE x VALUE '01',
        int TYPE x VALUE '7F',
        no_obli TYPE x VALUE 'FE',
      END OF vari,
    END OF flags .
  class-data REPORT type SY-REPID .
ENDCLASS.



CLASS ZAPLINK_PROGRAM IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*CF       Program function                                             Program              Function
*CM       Pull-down menu/menu bar                                      Program              Menu
*RE       Report/Function Group, Logical DB                            Program/module pool
  DATA _id    LIKE LINE OF r_doc_id.
  DATA f_ok   TYPE abap_bool.

  _id-sign = 'I'.
  _id-option = 'EQ'.
  _id-low = 'CF'. APPEND _id TO r_doc_id.
  _id-low = 'CM'. APPEND _id TO r_doc_id.
  _id-low = 'RE'. APPEND _id TO r_doc_id.

  PERFORM check_vari_form_exists IN PROGRAM (sy-repid) CHANGING f_ok IF FOUND.
  IF NOT f_ok IS INITIAL.   report = sy-repid.    RETURN.   ENDIF.
  PERFORM check_vari_form_exists IN PROGRAM zaplink_vari CHANGING f_ok IF FOUND.
  IF NOT f_ok IS INITIAL.
    report = 'ZAPLINK_VARI'.    " Dump if program name is in lower case
  ENDIF.
*      assert ID ZAPLINK CONDITION f_ok is initial.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.    application_log->msgid = 'ZAPLINK_PROGRAM'.
  type-type = supportedtypes-program. INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-sys_variant. INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-appl_variant. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method GET_PARAM_VALUE.
  current_param-value = value.
  endmethod.


  method GET_PROGRAM_SIGNATURE.
  DATA t_code TYPE tt_abaprawsource.
  DATA d_src  TYPE string.

  t_code = get_prog_rawsource( program_name ).
  zaplink_tools=>pretty_printer( CHANGING table = t_code ).
  zaplink_tools=>condense_abap_source( CHANGING table = t_code ).
  d_src = zaplink_tools=>table_2_string( t_code ).
  d_src = zaplink_tools=>clean_abap_string( d_src ).
  result = zaplink_tools=>calculate_md5_hash( d_src ).
  endmethod.


  method GET_SELECTOPTION_VALUE.
  DATA my_value LIKE LINE OF current_param-values.
*  DATA _desc    TYPE REF TO cl_abap_structdescr.
  FIELD-SYMBOLS:
*    <c> LIKE LINE OF _desc->components,
*    <sf> TYPE ANY,
*    <tf> TYPE ANY,
    <v> TYPE ANY.

  CHECK NOT value[] IS INITIAL.
*  LOOP AT value ASSIGNING <v>.   EXIT.   ENDLOOP.
*  _desc ?= cl_abap_structdescr=>describe_by_data( p_data = <v> ).
  LOOP AT value ASSIGNING <v>.
    CLEAR my_value.
    MOVE-CORRESPONDING <v> to my_value.
*    LOOP AT _desc->components ASSIGNING <c>.
*      UNASSIGN: <sf>, <tf>.
*      ASSIGN COMPONENT <c>-name OF STRUCTURE <v> TO <sf>.
*      ASSIGN COMPONENT <c>-name OF STRUCTURE my_value TO <tf>.
*      <tf> = <sf>.
*    ENDLOOP.
    APPEND my_value TO current_param-values.
  ENDLOOP.
  endmethod.


  method GET_VARIANT_PARAMS.
  DATA p_key     TYPE rsvarkey.
  DATA t_screens TYPE STANDARD TABLE OF rsscr.
  DATA t_vari    TYPE STANDARD TABLE OF rvari.
  DATA p_subrc   TYPE sy-subrc.
  DATA p_subrc_s TYPE sy-subrc.
  DATA s_subrc   TYPE string.
  DATA s_subrc_s TYPE string.
  DATA d_param   LIKE LINE OF current_param-params.
  DATA d_mandt   TYPE sy-mandt.
  TYPES tt_vdat TYPE STANDARD TABLE OF rsvarivdat.
  TYPES tt_vdyn TYPE STANDARD TABLE OF rsvaridyn.
  TYPES tt_vdd  TYPE STANDARD TABLE OF rsvdatdyn.
  FIELD-SYMBOLS:
    <vdat> TYPE tt_vdat,
    <vdyn> TYPE tt_vdyn,
    <vdd>  TYPE tt_vdd,
    <dat>  LIKE LINE OF <vdat>,
    <s> LIKE LINE OF t_screens,
    <v> LIKE LINE OF t_vari.
  DATA: BEGIN OF l_formname_0,
          prefix(7) VALUE '%_LINK_',
          suffix(8),
        END OF l_formname_0.

  DATA d_formname TYPE string.

  p_key-report = program.
  p_key-variant = variant.
  PERFORM import_var_selc IN PROGRAM saplsvar TABLES t_screens t_vari
                                               USING p_key-report p_key
                                            CHANGING p_subrc p_subrc_s.
  IF NOT p_subrc_s IS INITIAL.
    IF 1 = 2.   MESSAGE i000 WITH space space space.  ENDIF.
    s_subrc = p_subrc_s.
    CALL METHOD application_log->add_error
      EXPORTING
*        id_msgid     =
        id_msgno     = '000'
*        id_msgv4     =
        id_msgv1     = s_subrc
        id_msgv2     = p_key-report
        id_msgv3     = p_key-variant.
    RETURN.
  ELSEIF NOT p_subrc IS INITIAL.
    IF 1 = 2.   MESSAGE i001 WITH space space space.  ENDIF.
    s_subrc = p_subrc.
    CALL METHOD application_log->add_error
      EXPORTING
*        id_msgid     =
        id_msgno     = '001'
*        id_msgv4     =
        id_msgv1     = s_subrc
        id_msgv2     = p_key-report
        id_msgv3     = p_key-variant.
    RETURN.
  ENDIF.

* IN import_var_selc
*VARIVDAT LIKE RSVARIVDAT OCCURS 2
*DATA: VARIDYN LIKE RSVARIDYN OCCURS 10 WITH HEADER LINE.
*DATA: GL_VDATDYN LIKE RSVDATDYN OCCURS 0.
*  PERFORM import_variant_static(rsdbspvd) USING    p_selctab[]
*                                                   l_vari[]
*                                                   varivdat[]
*                                                   varidyn[]
*                                                   varivdat_dyn[]
*                                                   p_key
*                                                   p_imp_subrc.
  ASSIGN ('(SAPLSVAR)varivdat[]') TO <vdat>.
  ASSIGN ('(SAPLSVAR)varidyn[]') TO <vdyn>.
  ASSIGN ('(SAPLSVAR)varivdat_dyn[]') TO <vdd>.

  IF variant CP zaplink_vari_data=>variant_prefix-system OR variant CP zaplink_vari_data=>variant_prefix-customer.    d_mandt = '000'.    ELSE.   d_mandt = sy-mandt.    ENDIF.
  PERFORM %_import_vari_clnt IN PROGRAM (p_key-report)
                             USING p_key p_subrc_s d_mandt
                             CHANGING p_subrc
                             IF FOUND.
  IF NOT p_subrc IS INITIAL OR NOT p_subrc_s IS INITIAL.
    IF 1 = 2.   MESSAGE i002 WITH space space space space.  ENDIF.
    s_subrc = p_subrc.
    s_subrc_s = p_subrc_s.
    CALL METHOD application_log->add_error
      EXPORTING
*        id_msgid     =
        id_msgno     = '002'
        id_msgv1     = s_subrc_s
        id_msgv2     = s_subrc
        id_msgv3     = p_key-report
        id_msgv4     = p_key-variant.
    RETURN.
  ENDIF.

*  PERFORM set_object IN PROGRAM report USING me IF FOUND.
*  IF 1 = 2.   PERFORM set_object IN PROGRAM zaplink_vari USING me IF FOUND.  ENDIF.
  PERFORM set_object IN PROGRAM zaplink_vari USING me IF FOUND.
*perform brepi(rsdbspva) in MF : RS_VARIANT_CONTENTS
  LOOP AT t_vari ASSIGNING <v>.
    CLEAR current_param.
    MOVE-CORRESPONDING <v> TO current_param.
* FROM FORM FILL_TABLES_PS IN PROGRAM saplsvar:
*    PERFORM FILL_TABLES_PS IN PROGRAM saplsvar  tables current_param-hdr
*                                                 using <v>.
    IF <v>-xflag1 O flags-varid-noimp.      current_param-no_import = abap_true.    ENDIF.
    IF <v>-xflag1 O flags-vari-obli.        current_param-obli = abap_true.         ENDIF.
    IF <v>-xflag1 O flags-vari-noint.       current_param-noint = abap_true.        ENDIF.
    IF <v>-xflag1 Z flags-vari-nospagpa.
      READ TABLE t_screens ASSIGNING <s> WITH KEY name = <v>-name.
      IF sy-subrc = 0 AND <s>-spagpa NE space.    current_param-spagpa = abap_true.   ENDIF.
    ENDIF.
*    IF current_param-name NE space.
*      PERFORM init_text_field TABLES selctab
*                        USING current_param-name
*                        current_param-text 'S' rsvar-report.
*    ENDIF.
* end FORM FILL_TABLES_PS IN PROGRAM saplsvar
    MOVE <v>-name TO l_formname_0-suffix.
    CONCATENATE 'GET_VAL_' <v>-kind INTO d_formname.
    IF 1 = 2.                                               "#EC *
*      PERFORM get_val_s IN PROGRAM zaplink_vari IF FOUND.   "#EC *
      PERFORM get_val_p IN PROGRAM zaplink_vari USING space space space IF FOUND. "#EC *
    ENDIF.
*    PERFORM (l_formname_0) IN PROGRAM (p_key-report)
*           USING report d_formname p_subrc IF FOUND.
    PERFORM (l_formname_0) IN PROGRAM (p_key-report)
           USING 'ZAPLINK_VARI' d_formname p_subrc IF FOUND.
    LOOP AT <vdat> ASSIGNING <dat>
         WHERE selname = <v>-name.
      MOVE-CORRESPONDING <dat> TO d_param.
      APPEND d_param TO current_param-params.
    ENDLOOP.
    IF current_param-no_import = abap_true.   CLEAR: current_param-value, current_param-values, current_param-params.    ENDIF.
    INSERT current_param INTO TABLE result.
  ENDLOOP.
*  PERFORM clear_object IN PROGRAM report USING me IF FOUND.
*  IF 1 = 2.   PERFORM clear_object IN PROGRAM zaplink_vari USING me IF FOUND. ENDIF.
  PERFORM clear_object IN PROGRAM zaplink_vari USING me.
  endmethod.


  method READ_VARIANTS.
*  DATA t_varis     TYPE STANDARD TABLE OF varis WITH DEFAULT KEY.
  DATA t_dynnr     TYPE STANDARD TABLE OF rsdynnr WITH DEFAULT KEY.
  DATA t_varis     TYPE STANDARD TABLE OF rsdynnr WITH DEFAULT KEY.
  DATA t_varit     TYPE STANDARD TABLE OF varit WITH DEFAULT KEY.
  DATA s_vari      LIKE LINE OF result.
  DATA s_varis     LIKE LINE OF t_varis.
  FIELD-SYMBOLS:
    <v> LIKE LINE OF variants,
    <t> LIKE LINE OF t_varit,
    <s> LIKE LINE OF t_varis.

*  SELECT * INTO TABLE t_varis
*    FROM varis CLIENT SPECIFIED
*    FOR ALL ENTRIES IN variants
*    WHERE mandt = variants-mandt
*      AND report = variants-report
*      AND variant = variants-variant.

  SELECT * INTO TABLE t_varit
    FROM varit CLIENT SPECIFIED
    FOR ALL ENTRIES IN variants
    WHERE mandt = variants-mandt
      AND report = variants-report
      AND variant = variants-variant.

  LOOP AT variants ASSIGNING <v>.
    CLEAR s_vari.   s_vari-header = <v>.
    LOOP AT t_varit ASSIGNING <t>
         WHERE mandt = <v>-mandt
          AND report = <v>-report
         AND variant = <v>-variant.
      INSERT <t> INTO TABLE s_vari-texts.
    ENDLOOP.
    CALL FUNCTION 'RS_GET_SCREENS_4_1_VARIANT'
      EXPORTING
        program                    = <v>-report
        variant                    = <v>-variant
*     IMPORTING
*       FLAG_1000                  =
*       FLAG_ALL_SCREENS           =
      TABLES
        dynnr                      = t_dynnr
        variscreens                = t_varis
      EXCEPTIONS
        no_screens                 = 1
        variant_not_existent       = 2
        OTHERS                     = 3.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise 'RS_GET_SCREENS_4_1_VARIANT' sy-subrc.
    ENDIF.
    DELETE t_varis WHERE dynnr = '*'.   " only one screen
    MODIFY t_varis FROM s_varis TRANSPORTING kind WHERE NOT kind IS INITIAL.    " not used on import
    s_vari-screens = t_varis.
*    LOOP AT t_varis ASSIGNING <s>
*         WHERE mandt = <v>-mandt
*          AND report = <v>-report
*         AND variant = <v>-variant.
*      INSERT <s> INTO TABLE s_vari-screens.
*    ENDLOOP.
    s_vari-params = get_variant_params( program = <v>-report   variant = <v>-variant ).
    INSERT s_vari INTO TABLE result.
  ENDLOOP.
  endmethod.


  method SET_PARAM_VALUE.
  value = current_param-value.
  endmethod.


  method SET_SELECTOPTION_VALUE.
  value = current_param-values.
  endmethod.


  method SET_VARIANT_PARAMS.
  TYPES tt_vdat TYPE STANDARD TABLE OF rsvarivdat.
  TYPES tt_vdyn TYPE STANDARD TABLE OF rsvaridyn.
  TYPES tt_vdd  TYPE STANDARD TABLE OF rsvdatdyn.
  DATA p_key     TYPE rsvarkey.
  DATA t_screens TYPE STANDARD TABLE OF rsscr.
  DATA t_vari    TYPE STANDARD TABLE OF rvari.
  DATA p_subrc   TYPE sy-subrc.
  DATA p_subrc_s TYPE sy-subrc.
  DATA d_param   LIKE LINE OF current_param-params.
  DATA t_dyns_fields TYPE STANDARD TABLE OF rsdsfields WITH DEFAULT KEY.
  DATA t_varivdat TYPE tt_vdat.
  DATA t_varidyn TYPE tt_vdyn.
  DATA t_vdatdyn TYPE tt_vdd.
  DATA t_texpr   TYPE rsds_texpr.
  DATA s_vdat    LIKE LINE OF t_varivdat.
  DATA: BEGIN OF imex,
          vari,
          dyns,
        END   OF imex.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF t_vari,
    <p> LIKE LINE OF current_param-params,
    <v> LIKE LINE OF data.
  DATA: BEGIN OF l_formname_0,
          prefix(7) VALUE '%_LINK_',
          suffix(8),
        END OF l_formname_0.

  DATA d_formname TYPE string.

  p_key-report = program.
  p_key-variant = variant.
  result = abap_true.

* CALL FUNCTION 'SELTAB_2_SELOPTS'

  PERFORM import_var_selc IN PROGRAM saplsvar TABLES t_screens t_vari
                                               USING p_key-report p_key
                                            CHANGING p_subrc p_subrc_s.

  PERFORM set_object IN PROGRAM (report) USING me IF FOUND.   " for installer
  IF 1 = 2.   PERFORM set_object IN PROGRAM zaplink_vari USING me IF FOUND.  ENDIF.
*  SORT t_screens BY name.
*perform brepi(rsdbspva) in MF : RS_VARIANT_CONTENTS
  LOOP AT data INTO current_param.
    READ TABLE t_vari ASSIGNING <s> WITH KEY name = current_param-name.    " BINARY SEARCH.
    CHECK sy-subrc = 0.   " Else ignore field value
    <s>-vtype = current_param-vtype.    <s>-vname = current_param-vname.
    <s>-appendage = current_param-appendage.    <s>-invisible = current_param-invisible.    <s>-protected = current_param-protected.
* FROM MF RS_CHANGE_CREATED_VARIANT (134)
    IF current_param-spagpa = abap_true.    <s>-xflag1 = <s>-xflag1 BIT-AND flags-vari-spagpa.    ELSE.   <s>-xflag1 = <s>-xflag1 BIT-OR flags-vari-nospagpa.   ENDIF.
*    IF <v>-xflag1 O flags-vari-obli.        current_param-obli = abap_true.         ENDIF.
    IF current_param-obli = abap_true.      <s>-xflag1 = <s>-xflag1 BIT-OR flags-vari-obli.       ELSE.   <s>-xflag1 = <s>-xflag1 BIT-AND flags-vari-no_obli.   ENDIF.
*    IF <v>-xflag1 O flags-varid-noimp.      current_param-no_import = abap_true.    ENDIF.
    IF current_param-no_import = abap_true. <s>-xflag1 = <s>-xflag1 BIT-OR flags-varid-noimp.     ELSE.   <s>-xflag1 = <s>-xflag1 BIT-AND flags-varid-imp.      ENDIF.
*    IF <v>-xflag1 O flags-vari-noint.       current_param-noint = abap_true.        ENDIF.
    IF current_param-noint = abap_true.     <s>-xflag1 = <s>-xflag1 BIT-OR flags-vari-noint.      ELSE.   <s>-xflag1 = <s>-xflag1 BIT-AND flags-vari-int.       ENDIF.
* END FROM MF RS_CHANGE_CREATED_VARIANT (134)
    MOVE <s>-name TO l_formname_0-suffix.
    CONCATENATE 'SET_VAL_' <s>-kind INTO d_formname.
    IF 1 = 2.                                               "#EC *
*      PERFORM set_val_s IN PROGRAM zaplink_vari TABLES space                     "#EC *
*                USING space space space space space IF FOUND.   "#EC *
      PERFORM set_val_p IN PROGRAM zaplink_vari USING space space space IF FOUND. "#EC *
    ENDIF.
    PERFORM (l_formname_0) IN PROGRAM (p_key-report)
           USING report d_formname p_subrc IF FOUND.
    IF p_subrc <> 0.    result = abap_false.    ENDIF.
    LOOP AT current_param-params ASSIGNING <p>.
      CLEAR s_vdat.   MOVE-CORRESPONDING <p> TO s_vdat.   s_vdat-selname = current_param-name.
      APPEND s_vdat TO t_varivdat.
    ENDLOOP.
  ENDLOOP.
  PERFORM clear_object IN PROGRAM (report) USING me IF FOUND.   " for installer
  IF 1 = 2.   PERFORM clear_object IN PROGRAM zaplink_vari USING me IF FOUND.   ENDIF.

* MF RS_CREATE_VARIANT
  PERFORM export_variant_static IN PROGRAM rsdbspvd TABLES   t_dyns_fields
                                                     USING   t_vari
                                                             t_varivdat
                                                             t_varidyn
                                                             t_vdatdyn
                                                             t_texpr imex
                                                             p_key
                                                             p_subrc.
  IF p_subrc <> 0.    result = abap_false.    ENDIF.

  PERFORM export_%_vari IN PROGRAM saplsvar USING p_key-report
                                                  p_key
                                                  sy-mandt
                                                  space.    " FLAG_NOIMPORT.
  endmethod.


  method WRITE_VARIANTS.
  DATA t_varis     TYPE STANDARD TABLE OF rsdynnr WITH DEFAULT KEY.
  DATA t_varit     TYPE STANDARD TABLE OF varit WITH DEFAULT KEY.
  DATA t_params    TYPE STANDARD TABLE OF rsparams WITH DEFAULT KEY.
  DATA s_param     LIKE LINE OF t_params.
  DATA d_subrc     TYPE sy-subrc.
*  DATA t_rep       TYPE STANDARD TABLE OF string.
  DATA d_is_work_i  TYPE abap_bool.
  DATA d_is_inactiv TYPE abap_bool.
  DATA d_obj_name   TYPE e071-obj_name.
  DATA t_variants   TYPE tt_variants.
  DATA f_ok         TYPE abap_bool.
  DATA o_activate   TYPE REF TO zaplink_activate.
  DATA t_comps      TYPE zaplink_list=>tt_compkeys.
  DATA s_comp       LIKE LINE OF t_comps.
*  DATA s_vari      LIKE LINE OF result.
  FIELD-SYMBOLS:
*    <t> LIKE LINE OF t_varit,
*    <s> LIKE LINE OF t_varis,
    <v> LIKE LINE OF variants,
    <p> LIKE LINE OF <v>-params,
    <val> LIKE LINE OF <p>-values.

  t_variants = variants.    SORT t_variants.    s_comp-type = 'PROG'.
  LOOP AT t_variants ASSIGNING <v>.
    AT NEW header-report.
      f_ok = abap_true.
*    READ REPORT <v>-header-report INTO t_rep STATE 'A'.   " Check for active program existence
      d_obj_name = <v>-header-report.
      CALL FUNCTION 'RS_OBJECT_IN_WORKING_AREA'
        EXPORTING
          object                        = 'REPS'
          obj_name                      = d_obj_name
*     GLOBAL_CHECK                  = ' '
*     MODE                          = 'S'
        IMPORTING
          object_is_work_item           = d_is_work_i     " My inactive object
          object_inactive_version       = d_is_inactiv.
      IF NOT d_is_work_i IS INITIAL OR NOT d_is_inactiv IS INITIAL.
        REFRESH t_comps.    s_comp-name = <v>-header-report.    APPEND s_comp TO t_comps.
        CREATE OBJECT o_activate.
        o_activate->add_keys( t_comps ).
        t_comps = o_activate->activate( ).
        IF NOT t_comps IS INITIAL.      CLEAR f_ok.     ENDIF.
      ENDIF.
    ENDAT.

    IF f_ok IS INITIAL.
* Can't insert variant '&1' for program '&2', because do not exist as active
      SET EXTENDED CHECK OFF.
      IF 1 = 2.   MESSAGE i003 WITH <v>-header-report <v>-header-variant.   ENDIF.
      SET EXTENDED CHECK ON.
      CALL METHOD application_log->add_error
        EXPORTING
*        id_msgid     =
          id_msgno     = '003'
*        id_msgv4     =
          id_msgv1     = <v>-header-report
          id_msgv2     = <v>-header-variant.
    ENDIF.
    CHECK f_ok = abap_true.

    REFRESH t_params.
    LOOP AT <v>-params ASSIGNING <p>.
      IF NOT <p>-value IS INITIAL.
        CLEAR s_param.    s_param-kind = 'P'.       s_param-selname = <p>-name.     s_param-low = <p>-value.                  APPEND s_param TO t_params.
      ELSEIF NOT <p>-values IS INITIAL.
        LOOP AT <p>-values ASSIGNING <val>.
          CLEAR s_param.    s_param-kind = 'S'.     s_param-selname = <p>-name.     MOVE-CORRESPONDING <val> TO s_param.      APPEND s_param TO t_params.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    t_varit = <v>-texts.
    t_varis = <v>-screens.
    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report              = <v>-header-report
        variant             = <v>-header-variant
      IMPORTING
        r_c                 = d_subrc
      EXCEPTIONS
        not_authorized      = 1
        no_report           = 2
        report_not_existent = 3
        report_not_supplied = 4
        OTHERS              = 5.
    IF sy-subrc = 0 AND d_subrc = 0.
      CALL FUNCTION 'RS_VARIANT_DELETE'
        EXPORTING
          report                     = <v>-header-report
          variant                    = <v>-header-variant
          flag_confirmscreen         = abap_true
          flag_delallclient          = abap_true    " other wise popup screen
*       IMPORTING
*         VARIANT                    =
        EXCEPTIONS
          not_authorized             = 1
          not_executed               = 2
          no_report                  = 3
          report_not_existent        = 4
          report_not_supplied        = 5
          variant_locked             = 6
          variant_not_existent       = 7
          no_corr_insert             = 8
          variant_protected          = 9
          OTHERS                     = 10.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = <v>-header-report
        curr_variant              = <v>-header-variant
        vari_desc                 = <v>-header
      TABLES
        vari_contents             = t_params
        vari_text                 = t_varit
        vscreens                  = t_varis
      EXCEPTIONS
        illegal_report_or_variant = 1
        illegal_variantname       = 2
        not_authorized            = 3
        not_executed              = 4
        report_not_existent       = 5
        report_not_supplied       = 6
        variant_exists            = 7
        variant_locked            = 8
        OTHERS                    = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
*    PERFORM enq_variant IN PROGRAM saplsvar USING <v>-header-report <v>-header-variant.      " Not release by function it seems
      IF set_variant_params( program = <v>-header-report   variant = <v>-header-variant    data = <v>-params ) = abap_true.
        APPEND <v> TO result.
      ENDIF.
      PERFORM deq_variant IN PROGRAM saplsvar USING <v>-header-report <v>-header-variant.
    ENDIF.
  ENDLOOP.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_prog     TYPE to_program.
  DATA o_variant  TYPE to_variant.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
      o_variant ?= object->raw.
      o_variant->anonymize( ).
    WHEN supportedtypes-program.
      o_prog ?= object->raw.
      o_prog->anonymize( ).
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE.
  DATA o_prog     TYPE to_program.
  DATA o_variant  TYPE to_variant.

  TRY.
      super->zaplink_cnx_ext_cleaner~unanonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
      o_variant ?= object->raw.
      o_variant->unanonymize( ).
    WHEN supportedtypes-program.
      o_prog ?= object->raw.
      o_prog->unanonymize( ).
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
  DATA d_type TYPE td_comptype.
  DATA d_prog TYPE td_progname.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN supportedtypes-program.
        d_prog = component->get_name( ).
        result = get_program_signature( d_prog ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_prog     TYPE to_program.
  DATA o_variant  TYPE to_variant.
  DATA o_raw_var  TYPE to_raw_variant.

  CASE type.
    WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
      CREATE OBJECT o_raw_var.
      object = o_raw_var.
      CREATE OBJECT o_variant.
      object->raw = o_variant.
    WHEN supportedtypes-program.
      CREATE OBJECT object.
      CREATE OBJECT o_prog.
      object->raw = o_prog.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA d_name     TYPE td_compname.
  DATA type       TYPE td_comptype.
  DATA program    TYPE sy-repid.
  DATA s_variant  TYPE ts_variant_key.

  TRY.
    type = component->get_type( ).
    d_name = component->get_name( ).
    CASE type.
      WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
        CHECK do_exists( component ) = exists-exists.     " UI when variant do not exists (detected with Issue 104)
        s_variant = zaplink_vari_data=>name_2_key( d_name ).
        CALL FUNCTION 'RS_VARIANT_DELETE'
          EXPORTING
            report                     = s_variant-program
            variant                    = s_variant-variant
*           FLAG_CONFIRMSCREEN         =
            flag_delallclient          = abap_true
*         IMPORTING
*           VARIANT                    =
          EXCEPTIONS
            not_authorized             = 1
            not_executed               = 2
            no_report                  = 3
            report_not_existent        = 4
            report_not_supplied        = 5
            variant_locked             = 6
            variant_not_existent       = 7
            no_corr_insert             = 8
            variant_protected          = 9
            OTHERS                     = 10.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'RS_VARIANT_DELETE' sy-subrc.
        ENDIF.
      WHEN supportedtypes-program.
        program = d_name.
        CALL FUNCTION 'RS_DELETE_PROGRAM'
          EXPORTING
*          CORRNUMBER                       =
            program                          = program
*          SUPPRESS_CHECKS                  = ' '
*          SUPPRESS_COMMIT                  = ' '
            suppress_popup                   = 'X'
*          MASS_DELETE_CALL                 = ' '
*          WITH_CUA                         = 'X'
*          WITH_DOCUMENTATION               = 'X'
*          WITH_DYNPRO                      = 'X'
*          WITH_INCLUDES                    = ' '
*          WITH_TEXTPOOL                    = 'X'
*          WITH_VARIANTS                    = 'X'
*          TADIR_DEVCLASS                   =
*          SKIP_PROGRESS_IND                = ' '
*          FORCE_DELETE_USED_INCLUDES       = ' '
*        IMPORTING
*          CORRNUMBER                       =
*          PROGRAM                          =
          EXCEPTIONS
            enqueue_lock                     = 1
            object_not_found                 = 2
            permission_failure               = 3
            reject_deletion                  = 4
            OTHERS                           = 5.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'RS_DELETE_PROGRAM' sy-subrc.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  result = abap_true.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = type
                                                name = d_name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA d_name     TYPE td_compname.
  DATA type       TYPE td_comptype.
  DATA d_subrc    TYPE sy-subrc.
  DATA s_variant  TYPE ts_variant_key.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    d_name = component->get_name( ).
    CASE type.
      WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
        s_variant = zaplink_vari_data=>name_2_key( d_name ).
        CALL FUNCTION 'RS_VARIANT_EXISTS'
          EXPORTING
            report              = s_variant-program
            variant             = s_variant-variant
          IMPORTING
            r_c                 = d_subrc
          EXCEPTIONS
            not_authorized      = 1
            no_report           = 2
            report_not_existent = 3
            report_not_supplied = 4
            OTHERS              = 5.
        IF sy-subrc = 0 AND d_subrc = 0.    exists = me->exists-exists.   ENDIF.
      WHEN supportedtypes-program.
        SELECT SINGLE name INTO d_name
          FROM trdir
          WHERE name = d_name.
        IF sy-subrc = 0.    exists = me->exists-exists.   ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~GET_VALUES.
  DATA s_variant   TYPE ts_variant_key.                     " Issue 53
  CASE type.
    WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
      s_variant = zaplink_vari_data=>name_2_key( name ).
      IF s_variant-program IS INITIAL.
        CALL METHOD super->zaplink_connector~get_values
          EXPORTING
            type = supportedtypes-program
          CHANGING
            name = name.
        s_variant-program = name.
      ENDIF.
      CHECK NOT s_variant-program IS INITIAL.
      CALL FUNCTION 'RS_VARIANT_CATALOG'
        EXPORTING
          report                     = s_variant-program
*         NEW_TITLE                  = ' '
*         DYNNR                      =
*         INTERNAL_CALL              = ' '
*         MASKED                     = 'X'
          variant                    = s_variant-variant
*         POP_UP                     = ' '
        IMPORTING
          sel_variant                = s_variant-variant
*         SEL_VARIANT_TEXT           =
*       TABLES
*         BELONGING_DYNNR            =
        EXCEPTIONS
          no_report                  = 1
          report_not_existent        = 2
          report_not_supplied        = 3
          no_variants                = 4
          no_variant_selected        = 0
          variant_not_existent       = 6
          OTHERS                     = 7.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        name = zaplink_vari_data=>key_2_name( s_variant ).
      ENDIF.
    WHEN supportedtypes-program.
      CALL METHOD super->zaplink_connector~get_values
        EXPORTING
          type = type
        CHANGING
          name = name.
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.    if version = _ver.    result = abap_true.   endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  TYPES:                                                    " Issue 7
    BEGIN OF ts_doc,
      prog TYPE programm,
      wildcard(1) TYPE c,
    END OF ts_doc.
  DATA s_obj       TYPE ts_doc.                             " Issue 7
  DATA o_prog      TYPE to_program.
  DATA s_fmd_prog  TYPE ts_fm_data.
  DATA o_variant   TYPE to_variant.
  DATA s_fmd_vari  TYPE ts_fmd_vari.
  DATA _prog       TYPE td_progname.
  DATA d_name      TYPE td_compname.
  DATA _obj        TYPE doku_obj.
  DATA type        TYPE td_comptype.
  DATA t_varid     TYPE tt_variant_list.                    " Issue 53
  DATA s_variant   TYPE ts_variant_key.                     " Issue 53
*  DATA s_vari      LIKE LINE OF s_fmd_prog-variants.        " Issue 53
  DATA f_subcomp   TYPE td_with_subcomp.
  DATA l_mandt     LIKE sy-mandt.                           " Issue 99

  TRY.
    type = component->get_type( ).
    d_name = component->get_name( ).
    f_subcomp = component->get_with_subcomp( ).
    CREATE OBJECT object.
    object->set_component( component ).
    CASE type.
      WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
        CREATE OBJECT o_variant.
        object->raw = o_variant.
        s_variant = zaplink_vari_data=>name_2_key( d_name ).
        PERFORM get_mandt IN PROGRAM saplsvar USING s_variant-variant " Issue 99
                                           CHANGING l_mandt.
*        CASE type.                                         " Issue 99
*          WHEN supportedtypes-sys_variant.
        SELECT * UP TO 1 ROWS
          INTO TABLE t_varid
          FROM varid CLIENT SPECIFIED
          WHERE mandt = l_mandt
            AND report = s_variant-program
            AND variant = s_variant-variant.
*          WHEN OTHERS.
*            SELECT * UP TO 1 ROWS
*              INTO TABLE t_varid
*              FROM varid
*              WHERE report = s_variant-program
*                AND variant = s_variant-variant.
*        ENDCASE.
        s_fmd_prog-variants = read_variants( t_varid ).
        CHECK NOT s_fmd_prog-variants IS INITIAL.
        READ TABLE s_fmd_prog-variants INDEX 1 INTO s_fmd_vari.
        o_variant->from_data( s_fmd_vari ).
        CLEAR: o_variant->a0_maindata-mandt, o_variant->a0_maindata-report, o_variant->a0_maindata-variant.
      WHEN supportedtypes-program.
        _prog = d_name = object->name.
        CREATE OBJECT o_prog.
        SELECT SINGLE *
          INTO s_fmd_prog-header
          FROM progdir    " reposrc
*The databae view "REPOSRC" is write-protected, so it cannot be changed.
          WHERE name = d_name
            AND state = 'A'.        " Active
        IF sy-subrc <> 0.
          SELECT SINGLE *
            INTO s_fmd_prog-header
            FROM progdir    "reposrc
            WHERE name = d_name
              AND state = 'I'.      " Inactive
        ENDIF.

* Source code
        s_fmd_prog-source = get_prog_source( _prog ).
*    INCLUDE TYPE reposrc. "The databae view "REPOSRC" is write-protected, so it cannot be changed.

* Texts Pool
        s_fmd_prog-textspool = get_prog_textpool( _prog ).

* Dynpro : Issue 8
        s_fmd_prog-dynpros = get_dynpros( _prog ).

* Menu Painter : Issue 9
        s_fmd_prog-menupainter = get_menus( _prog ).

        SELECT *
          INTO TABLE s_fmd_prog-texts
          FROM trdirti
          WHERE name = d_name.

* Documentation
*      CONCATENATE _name '*' INTO _obj. " Issue 7
        s_obj-prog = d_name.
        s_obj-wildcard = '*'.
        _obj = s_obj.
*Issue 7
        s_fmd_prog-docs = zaplink_documentation=>get( ids = zaplink_prog_data=>r_doc_ids-prog
                                                  object = _obj ).

* Issue 53
        IF f_subcomp >= sub_component-with_mine.
          SELECT * INTO TABLE t_varid
            FROM varid CLIENT SPECIFIED
            WHERE mandt = '000'
              AND report = _prog.

          IF f_subcomp >= sub_component-with_required.
            SELECT * APPENDING TABLE t_varid
              FROM varid
              WHERE report = _prog
                AND transport <> zaplink_vari_data=>transport-normal.
            IF f_subcomp >= sub_component-with_all.
              SELECT * APPENDING TABLE t_varid
                FROM varid
                WHERE report = _prog
                  AND transport = zaplink_vari_data=>transport-normal.
            ENDIF.
          ENDIF.
          SORT t_varid BY mandt report variant. DELETE ADJACENT DUPLICATES FROM t_varid COMPARING mandt REPORT variant.

          s_fmd_prog-variants = read_variants( t_varid ).
        ENDIF.

        o_prog->from_data( s_fmd_prog ).
        o_prog->_code_signature = get_program_signature( o_prog->a0_maindata-name ).

        CLEAR o_prog->a0_maindata-name.
        object->raw = o_prog.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA o_prog      TYPE to_program.
  DATA o_vari      TYPE to_variant.
  DATA s_variant   TYPE ts_variant_key.
  DATA _prog       TYPE programm.
  DATA _oname      TYPE e071-obj_name.
  DATA _rep        TYPE progdir.
  DATA o_comp      TYPE to_component.
*  DATA _name       TYPE td_compname.
  DATA s_fmd_prog  TYPE ts_fm_data.
  DATA s_fmd_vari  TYPE ts_fmd_vari.
  DATA t_vari      TYPE tt_variants.
  FIELD-SYMBOLS <v> LIKE LINE OF o_prog->variants.

  TRY.

    CREATE OBJECT components.                         " Issue 92
    CASE object->type.
      WHEN supportedtypes-sys_variant OR supportedtypes-appl_variant.
        o_vari ?= object->raw.    s_variant = zaplink_vari_data=>name_2_key( object->name ).
        o_vari->a0_maindata-report = s_variant-program.   o_vari->a0_maindata-variant = s_variant-variant.
        s_fmd_vari = o_vari->to_data( ).    APPEND s_fmd_vari TO s_fmd_prog-variants.
        write_variants( s_fmd_prog-variants ).

      WHEN supportedtypes-program.
        o_prog ?= object->raw.
        _oname = _prog = o_prog->a0_maindata-name = object->name.
        s_fmd_prog = o_prog->to_data( ).
        CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
          EXPORTING
            object                  = 'REPS'
            obj_name                = _oname
*              DELETED_FLAG            = ' '
*           IMPORTING
*             OBJECT_INSERTED         =
          EXCEPTIONS
            wrong_object_name       = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'RS_INSERT_INTO_WORKING_AREA' sy-subrc.
        ENDIF.

        CALL FUNCTION 'RS_INSERT_INTO_WORKING_AREA'
          EXPORTING
            object                  = 'REPT'
            obj_name                = _oname
*              DELETED_FLAG            = ' '
*            IMPORTING
*              OBJECT_INSERTED         =
          EXCEPTIONS
            wrong_object_name       = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'RS_INSERT_INTO_WORKING_AREA' sy-subrc.
        ENDIF.

        MOVE-CORRESPONDING s_fmd_prog-header TO _rep.
        _rep-state = 'I'.   MODIFY progdir FROM _rep.
*      MODIFY reposrc FROM _rep. "The databae view "REPOSRC" is write-protected, so it cannot be changed.
        _rep-state = 'A'.   MODIFY progdir FROM _rep.

* Source code
        set_prog_source( program = _prog
                          source = s_fmd_prog-source ).
* Text Pool
        set_prog_textpool( program = _prog
                          textpool = s_fmd_prog-textspool ).
* Dynpro : Issue 8
        set_dynpros( program = _prog
                     dynpros = s_fmd_prog-dynpros ).

* Menu Painter : Issue 9
        set_menus( program = _prog
                     menus = s_fmd_prog-menupainter ).

* Documentation
        zaplink_documentation=>set( t_docs = s_fmd_prog-docs ).

*Texts
        DELETE FROM trdirti WHERE name = s_fmd_prog-header-name.
        MODIFY trdirti FROM TABLE s_fmd_prog-texts.

* Variants
        t_vari = write_variants( s_fmd_prog-variants ).
        LOOP AT o_prog->variants ASSIGNING <v>.
          o_vari ?= <v>-zl_object->raw.
          READ TABLE t_vari TRANSPORTING NO FIELDS WITH KEY header-variant = o_vari->a0_maindata-variant.
          IF sy-subrc = 0.
            o_comp = <v>-zl_object->get_component( abap_true ).
            components->add( o_comp ).    components->select( o_comp ).
* Done in Write_variants
*          ELSE.
** Can't insert variant '&1' for program '&2', because don't exist as active
*            SET EXTENDED CHECK OFF.
*            IF 1 = 2.   MESSAGE i003 WITH space space.   ENDIF.
*            SET EXTENDED CHECK ON.
*            CALL METHOD application_log->add_error
*              EXPORTING
**                id_msgid = 'ZAPLINK'
*                id_msgno = '003'
*                id_msgv1 = s_fmd_prog-header-name
*                id_msgv2 = o_vari->a0_maindata-variant.
          ENDIF.
        ENDLOOP.

      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name object->type.
    ENDCASE.
    check_component_list( EXPORTING     object = object
                           CHANGING components = components ). " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
