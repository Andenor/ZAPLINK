class ZAPLINK_TRANSACTION definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.

  types:
    begin of ts_text,
      langu   type SPRAS,
      ENTRY	  type TEXTPOOLTX,
      LENGTH  type TEXTPOOLLN,
    end of ts_text .
  types:
    tt_texts type SORTED TABLE OF ts_text WITH UNIQUE KEY langu .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_TRANSACTION' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
    redefinition .
  methods ZAPLINK_CONNECTOR~CREATE_NEW_RAW
    redefinition .
  methods ZAPLINK_CONNECTOR~DELETE_FROM_SAP
    redefinition .
  methods ZAPLINK_CONNECTOR~DO_EXISTS
    redefinition .
  methods ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION
    redefinition .
  methods ZAPLINK_CONNECTOR~READ_FROM_SAP
    redefinition .
  methods ZAPLINK_CONNECTOR~WRITE_TO_SAP
    redefinition .
protected section.

  types TO_DATA type ref to ZAPLINK_TRAN_DATA .
private section.

  constants ST_TRANSACTION type TD_COMPTYPE value 'TRAN' ##NO_TEXT.
  class-data R_DOC_ID type TR_DOCID .
  constants _UUID type TD_CONNUUID value 'A816684B5215E22AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  constants C_MSG_ID type TD_DOC_ID value 'NA' ##NO_TEXT.
  constants:
    BEGIN OF t_component,
      program TYPE td_comptype VALUE 'PROG',
      object TYPE td_comptype VALUE 'CLAS',
      transaction TYPE td_comptype VALUE st_transaction,
      functionpool TYPE td_comptype VALUE 'FUGR',
    END OF t_component .
  constants:
    BEGIN OF transaction_types,   " FROM LSEUKTOP(93)
      transaction  TYPE x VALUE '00',               " Transaktion         T
      menu         TYPE x VALUE '01',               " Bereichsmen#        M
      parameter    TYPE x VALUE '02',               " Parametertrans.     P
      program      TYPE x VALUE '80',               " Report              R
      prog_variant TYPE x VALUE '10',               " Report  with Variant
      object       TYPE x VALUE '08',               " Transactional object
      check        TYPE x VALUE '04',               " With autjority check
      enqueue      TYPE x VALUE '20',                       " With SM01
    END OF transaction_types .
ENDCLASS.



CLASS ZAPLINK_TRANSACTION IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*NA       Message
  DATA _id LIKE LINE OF r_doc_id.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = C_MSG_ID. APPEND _id TO r_doc_id.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = st_transaction. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_data  TYPE to_data.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN st_transaction.
      o_data ?= object->raw.
      o_data->anonymize( ).
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_data TYPE to_data.

  CREATE OBJECT object.
  CASE type.
    WHEN st_transaction.
      CREATE OBJECT o_data.
      object->raw = o_data.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA d_name TYPE tstc-tcode.
  DATA d_type TYPE td_comptype.
  DATA _name  TYPE td_compname.

  TRY.

    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_transaction.
        d_name = _name = component->get_name( ).
        CALL FUNCTION 'RPY_TRANSACTION_DELETE'
          EXPORTING
            transaction              = d_name
*           TRANSPORT_NUMBER         =
*           SUPPRESS_AUTHORITY_CHECK = ' '
*           SUPPRESS_CORR_INSERT     = ' '
          EXCEPTIONS
            not_excecuted            = 1
            object_not_found         = 2
            OTHERS                   = 3.
        IF sy-subrc <> 0. " SAP NameSpace
          CASE sy-subrc.
            WHEN 2.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING
                  textid = zaplink_cx_connector=>not_found
                  type   = d_type
                  name   = _name.
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING
                  textid = zaplink_cx_connector=>system_error.
          ENDCASE.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  result = abap_true.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = d_type
                                                name = _name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA d_name TYPE tstc-tcode.
  DATA type  TYPE td_comptype.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN st_transaction.
        d_name = component->get_name( ).
        SELECT SINGLE tcode INTO d_name      " From MF RPY_TRANSACTION_READ
          FROM tstc
          WHERE tcode = d_name.
        IF sy-subrc = 0.
          exists = me->exists-exists.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.
    result = abap_true.
  endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA o_data  TYPE to_data.
  DATA d_name  TYPE tstc-tcode.
  DATA s_fm_data TYPE zaplink_tran_data=>ts_fm_data.
  DATA t_tstc  TYPE STANDARD TABLE OF tstc WITH DEFAULT KEY.
  DATA t_tstcc TYPE STANDARD TABLE OF tstcc WITH DEFAULT KEY.
  DATA d_remotedata TYPE string.
  DATA t_param TYPE STANDARD TABLE OF rsparam.
  DATA o_conn  TYPE to_connector.
  DATA o_comp  TYPE to_component.
  DATA _name   TYPE td_compname.
  DATA d_type TYPE td_comptype.
  FIELD-SYMBOLS:
    <param> TYPE STANDARD TABLE,
    <scr> TYPE rsstcd,
    <g> TYPE s_gui_inhe,
    <t> LIKE LINE OF t_tstc,
    <a> LIKE LINE OF t_tstcc.
  DATA f_subcomp  TYPE td_with_subcomp.

  TRY.

    CREATE OBJECT object.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_transaction.
        f_subcomp = component->get_with_subcomp( ).
        d_name = component->get_name( ).

        PERFORM refresh_global_data IN PROGRAM saplseuk.    " to initialize data : Issue 67
        CALL FUNCTION 'RPY_TRANSACTION_READ'
          EXPORTING
            transaction            = d_name
*             PROGRAM                =
*             DYNPRO                 =
*             TRANSACTION_TYPE       = ' '
          TABLES
            tcodes                 = t_tstc
            gui_attributes         = t_tstcc
          EXCEPTIONS
            permission_error       = 1
            cancelled              = 2
            not_found              = 3
            object_not_found       = 4
            OTHERS                 = 5.
        IF sy-subrc <> 0.
          o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = 'RPY_TRANSACTION_READ'
                                                      subrc = sy-subrc
                                                  classname = 'ZAPLINK_CX_CONNECTOR' ).
          application_log->add_symsg( ).
          RAISE EXCEPTION o_mycx.
        ENDIF.

        READ TABLE t_tstc ASSIGNING <t>
             WITH KEY tcode = d_name.
        CHECK sy-subrc = 0.

        CREATE OBJECT o_data.
        s_fm_data-tstc = <t>.

        PERFORM select_tstc_tables_new IN PROGRAM saplseuk USING d_name space space.

        d_remotedata = '(SAPLSEUK)G_GUI_INHE'.
        ASSIGN (d_remotedata) TO <g>.
        IF sy-subrc = 0. s_fm_data-gui_inh = <g>. ENDIF.

        d_remotedata = '(SAPLSEUK)param[]'.
        ASSIGN (d_remotedata) TO <param>.
        IF sy-subrc = 0. s_fm_data-t_params = <param>. ENDIF.

        d_remotedata = '(SAPLSEUK)RSSTCD'.
        ASSIGN (d_remotedata) TO <scr>.
        IF sy-subrc = 0. s_fm_data-rsstcd = <scr>. ENDIF.

        SELECT * INTO TABLE s_fm_data-tstct
          FROM tstct
          WHERE tcode = d_name.

        SELECT * INTO TABLE s_fm_data-tstca
          FROM tstca
          WHERE tcode = d_name.

        READ TABLE t_tstcc ASSIGNING <a>
             WITH KEY tcode = d_name.
        IF sy-subrc = 0.
          s_fm_data-tstcc = <a>.
** From MF : RPY_TRANSACTION_INSERT
**      if not easy_web_transaction is initial.
**        tstcc-s_webgui = '2'.
**      elseif not html_enabled is initial.
**        tstcc-s_webgui = '1'.
**      else.
**        clear tstcc-s_webgui.
**      endif.
*            CASE <a>-s_webgui.
*              WHEN '2'.
*                s_fm_data-easy_web_t = abap_true.
*              WHEN '1'.
*                s_fm_data-html_en = abap_true.
*            ENDCASE.
*            s_fm_data-wingui_en = <a>-s_win32.
*            s_fm_data-java_en = <a>-s_platin.
*            s_fm_data-servicefile = <a>-s_service.
**            s_fm_data-??? = <a>-s_pervas.
        ENDIF.

        IF s_fm_data-tstc-cinfo O transaction_types-prog_variant.    SUBTRACT transaction_types-prog_variant FROM s_fm_data-tstc-cinfo.   ENDIF.    " Issue 67 : Transaction of program with variant
        CREATE OBJECT o_data.
        o_data->from_data( s_fm_data ).

* Issue 40 : Sub object has been disabled in V0.0.003 and is working in V0.0.004
* Sub component
        IF f_subcomp >= sub_component-with_all.
          IF o_data->a0_maindata-cinfo O transaction_types-object.
* Object
            _name = o_data->a0_maindata-classname.
            IF NOT _name IS INITIAL.
              o_conn = zaplink_connectors=>create_connector( type = t_component-object ).
              IF o_conn IS BOUND.
                CREATE OBJECT o_comp.
                o_comp->set_type( t_component-object ).
                o_comp->set_name( _name ).
                o_data->zl_object = o_conn->read_from_sap( o_comp ).
                IF o_data->zl_object IS BOUND.
                  CLEAR o_data->a0_maindata-classname.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSEIF o_data->a0_maindata-cinfo O transaction_types-program.
* Program
            _name = o_data->a0_maindata-pgmna.
            IF NOT _name IS INITIAL.
              o_conn = zaplink_connectors=>create_connector( type = t_component-program ).
              IF o_conn IS BOUND.
                CREATE OBJECT o_comp.
                o_comp->set_type( t_component-program ).
                o_comp->set_name( _name ).
                o_data->zl_object = o_conn->read_from_sap( o_comp ).
                IF o_data->zl_object IS BOUND.
                  CLEAR o_data->a0_maindata-pgmna.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSEIF o_data->a0_maindata-cinfo O transaction_types-transaction.
* Transaction
            _name = o_data->a0_maindata-call_tcode.
            IF NOT _name IS INITIAL AND _name <> d_name.
              CREATE OBJECT o_comp.
              o_comp->set_type( st_transaction ).
              o_comp->set_name( _name ).
              o_data->zl_object = zaplink_connector~read_from_sap( o_comp ).
              IF o_data->zl_object IS BOUND.
                CLEAR o_data->a0_maindata-call_tcode.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        object->set_component( component ).
        CLEAR o_data->a0_maindata-tcode.
        object->raw = o_data.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  TYPES:              " FROM LSEUKTOP(82)
    BEGIN OF cobj,
      field TYPE tstca-field,
      value TYPE tstca-value,
      olen  TYPE dfies-outputlen,
    END OF cobj.
  DATA t_cobj TYPE STANDARD TABLE OF cobj.
  DATA s_cobj LIKE LINE OF t_cobj.
  DATA o_data TYPE to_data.
  DATA o_comp TYPE to_component.
  DATA d_name TYPE td_compname.
  DATA s_fm_data TYPE zaplink_tran_data=>ts_fm_data.
  DATA d_remotedata TYPE string.
  DATA s_tstca LIKE LINE OF s_fm_data-tstca.
  DATA s_tstct LIKE LINE OF s_fm_data-tstct.
  DATA o_conn  TYPE to_connector.
  DATA o_list  TYPE to_list.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF s_fm_data-tstct,
    <st> TYPE STANDARD TABLE,
    <v> LIKE LINE OF s_fm_data-tstca,
    <a> TYPE ANY.

  DEFINE lmac_assign.
    d_remotedata = &1.
    assign (d_remotedata) to <a>.
    if sy-subrc = 0. <a> = &2. endif.
  END-OF-DEFINITION.
  DEFINE lmac_assign_table.
    d_remotedata = &1.
    assign (d_remotedata) to <st>.
    if sy-subrc = 0. <st> = &2. endif.
  END-OF-DEFINITION.

  TRY.
    CREATE OBJECT components.
    CASE object->type.
      WHEN st_transaction.
        o_data ?= object->raw.
* Main data
        o_data->a0_maindata-tcode = object->name.

* Sub object
        IF o_data->zl_object IS BOUND.
          IF o_data->a0_maindata-cinfo O transaction_types-object.
* Object
            o_data->a0_maindata-classname = o_data->zl_object->name.
            o_conn = zaplink_connectors=>create_connector( type = t_component-object ).
          ELSEIF o_data->a0_maindata-cinfo O transaction_types-program.
* Program
            o_data->a0_maindata-pgmna = o_data->zl_object->name.
            o_conn = zaplink_connectors=>create_connector( type = t_component-program ).
          ELSEIF o_data->a0_maindata-cinfo O transaction_types-transaction.
* Transaction
            o_data->a0_maindata-call_tcode = o_data->zl_object->name.
            o_conn = me.
          ENDIF.
          IF o_conn IS BOUND.
            o_list = o_conn->write_to_sap( o_data->zl_object ).
            IF o_conn->application_log IS BOUND.    application_log->add_from_instance( o_conn->application_log ).   ENDIF.
            components->add_list( o_list ).
          ENDIF.
        ENDIF.

        s_fm_data = o_data->to_data( ).

        PERFORM refresh_global_data IN PROGRAM saplseuk.    " to load program in memory
* SEUK : LSEUKF01
*3441      perform save_tcode.
*   tstca
*   tstcp <= cobj
*   cobj
        LOOP AT s_fm_data-tstca ASSIGNING <v>.
          IF sy-tabix = 1. lmac_assign '(SAPLSEUK)TSTCA' <v>. ENDIF.
          s_cobj-field = <v>-field.
          s_cobj-value = <v>-value.
          APPEND s_cobj TO t_cobj.
        ENDLOOP.
        lmac_assign '(SAPLSEUK)COBJ[]' t_cobj.
*   tstcc
        lmac_assign '(SAPLSEUK)TSTCC' s_fm_data-tstcc.
*   tc_typ, tc_chk
        IF s_fm_data-tstc-cinfo O transaction_types-check. lmac_assign '(SAPLSEUK)TC_CHK' transaction_types-check. SUBTRACT transaction_types-check FROM s_fm_data-tstc-cinfo .  ENDIF.
        IF s_fm_data-tstc-cinfo O transaction_types-enqueue. lmac_assign '(SAPLSEUK)TC_ENQ' transaction_types-enqueue. SUBTRACT transaction_types-enqueue FROM s_fm_data-tstc-cinfo .  ENDIF.
        lmac_assign '(SAPLSEUK)TC_TYP' s_fm_data-tstc-cinfo.
*   rsstcd
        lmac_assign '(SAPLSEUK)RSSTCD' s_fm_data-rsstcd.
        IF NOT s_fm_data-rsstcd-call_tcode IS INITIAL. lmac_assign '(SAPLSEUK)PARAM_VARI' abap_true. ENDIF.      " For transaction parameter @*
*   action
*          lmac_assign '(SAPLSEUK)ACTION' 'ADD'. Issue 57
*   tstc
        lmac_assign '(SAPLSEUK)TSTC' s_fm_data-tstc.
*   tran_langu & tstct
        IF NOT s_fm_data-tstct IS INITIAL.
          READ TABLE s_fm_data-tstct INTO s_tstct INDEX 1.
          lmac_assign '(SAPLSEUK)TRAN_LANGU' s_tstct-sprsl.
          lmac_assign '(SAPLSEUK)TSTCT' s_tstct.
        ENDIF.
*   params
        lmac_assign_table '(SAPLSEUK)param[]' s_fm_data-t_params.

*3442      perform save_classification.
*   tstc
*   g_gui_inhe
        lmac_assign '(SAPLSEUK)G_GUI_INHE' s_fm_data-gui_inh.
*   g_iac_ewt <= tstcc-s_webgui
        IF s_fm_data-tstcc-s_webgui = '2'.    " LSEUKF01(426-427)
          lmac_assign '(SAPLSEUK)G_IAC_EWT' 'X'.
        ENDIF.
        PERFORM save_tcode IN PROGRAM saplseuk.
        PERFORM save_classification IN PROGRAM saplseuk.

        MODIFY tstct FROM TABLE s_fm_data-tstct.

*        CREATE OBJECT o_comp.
*        o_comp->set_type( st_transaction ).
*        d_name = o_data->a0_maindata-tcode.
*        o_comp->set_name( d_name ).
*        CREATE OBJECT components.
*        components->add( o_comp ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name object->type.
    ENDCASE.
    check_component_list( EXPORTING     object = object
                           CHANGING components = components ). " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
