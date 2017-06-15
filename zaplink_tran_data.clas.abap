class ZAPLINK_TRAN_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_TRANSACTION .

public section.
  type-pools STSTC .

  interfaces ZAPLINK_DATATYPES .

  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .

  types TO_RAW type ZAPLINK_CONNECTOR=>TO_RAW .
  types:
    begin of TS_SUB_OBJ,
      ZL_OBJECT Type  TO_RAW,
    end of TS_SUB_OBJ .
  types:
    tt_doc_usr TYPE STANDARD TABLE OF rpy_objtab WITH DEFAULT KEY .
  types:
    tt_doc_tech TYPE STANDARD TABLE OF tline WITH DEFAULT KEY .
  types:
    tt_params TYPE STANDARD TABLE OF rsparam WITH DEFAULT KEY .
  types:
    tt_values TYPE SORTED TABLE OF tstca WITH UNIQUE KEY objct field .
  types:
    tt_tstct TYPE SORTED TABLE OF tstct WITH UNIQUE KEY sprsl .
  types:
    BEGIN OF ts_fm_data,
* Data required
*        cinfo       TYPE tstc-cinfo,
          tstc        TYPE tstc,
          tstct       TYPE tt_tstct,
          tstcc       TYPE tstcc,
          tstca       TYPE tt_values,
          rsstcd      TYPE rsstcd,
          gui_inh     TYPE s_gui_inhe,  " (SAPLSEUK)G_GUI_INHE
* FM : RPY_TRANSACTION_INSERT
*            transaction TYPE tstc-tcode,
*            program     TYPE trdir-name,
*            dynpro      TYPE d020s-dnum,
*            language    TYPE sy-langu,
*            with_docu   TYPE rglif-with_docu,
*            docutype    TYPE rglif-docutype,
**          dev_class   TYPE rglif-devclass,
**          transport   TYPE rglif-trkorr,
*            trans_type  TYPE rglif-docutype,
*            shorttext   TYPE tstct-ttext,
*            called_t    TYPE tstc-tcode,
*            trans_skip  TYPE char01,
*            variant     TYPE tcvariant,
*            cl_indep    TYPE char01,
*            easy_web_t  TYPE s_ewt,
*            pro_user_t  TYPE s_prof,
*            html_en     TYPE s_webgui,
*            java_en     TYPE s_platin,
*            wingui_en   TYPE s_win32,
*            servicefile TYPE iacservic_,
*            genflag     TYPE tadir-genflag,
*          del_auth_ch TYPE char1,
*          del_corr_i  TYPE char1,
*"  TABLES
          t_doc_user  TYPE tt_doc_usr,
          t_doc_tech  TYPE tt_doc_tech,
          t_params    TYPE tt_params,
        END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE tstc.
    INCLUDE TYPE rsstcd.
*    TYPES:
*      BEGIN OF ts_dialog,
*            program TYPE tstc-pgmna,
*            dynnpro TYPE tstc-dypno,
*            webgui  TYPE tstcc-s_webgui,
*            java    TYPE tstcc-s_platin,
*            sapgui  TYPE tstcc-s_win32,
*            srvfile TYPE tstcc-s_service,
*          END OF ts_dialog .
*    TYPES:
*      BEGIN OF ts_report,
*            program TYPE tstc-pgmna,
*            variant TYPE rsstcd-repo_vari,
*            webgui  TYPE tstcc-s_webgui,
*            java    TYPE tstcc-s_platin,
*            sapgui  TYPE tstcc-s_win32,
*            srvfile TYPE tstcc-s_service,
*          END OF ts_report .
*    TYPES:
*      BEGIN OF ts_variant,
*            called_t TYPE tstc-tcode,
*            variant  TYPE rsstcd-repo_vari,
*            indep    TYPE char01,             " CL_INDEPENDEND
*          END OF ts_variant .
*    TYPES:
*      BEGIN OF ts_parameters,
*            program  TYPE tstc-pgmna,
*            dynnpro  TYPE tstc-dypno,
*            called_t TYPE tstc-tcode,
*            skip_1st TYPE rsstcd-st_skip_1,
*            webgui   TYPE tstcc-s_webgui,
*            java     TYPE tstcc-s_platin,
*            sapgui   TYPE tstcc-s_win32,
*            srvfile  TYPE tstcc-s_service,
*            values   TYPE tt_params,
*          END OF ts_parameters .
*  ststc_c_type_dialog     type sychar01 value 'D',           "Dialog
*  ststc_c_type_report     type sychar01 value 'R',           "Report
*  ststc_c_type_variant    type sychar01 value 'V',           "Varianten
*  ststc_c_type_parameters type sychar01 value 'P',           "Parameter
*  ststc_c_type_object     type sychar01 value 'O'.           "Objekt
*          type    TYPE rglif-docutype,
*          code    TYPE tstc-tcode,
*          dialog  TYPE ts_dialog,
*          report  TYPE ts_report,
*          variant TYPE ts_variant,
*          params  TYPE ts_parameters,
*        object  TYPE ts_object,     " not supported yet
    TYPES:
      gui_inh   TYPE s_gui_inhe,
* From TSTCC
      s_webgui  TYPE tstcc-s_webgui,
      s_win32   TYPE tstcc-s_win32,
      s_platin  TYPE tstcc-s_platin,
      s_service TYPE tstcc-s_service,
      s_pervas  TYPE tstcc-s_pervas,
    END OF ts_maindata .
  types:
    BEGIN OF ts_authority,
          object TYPE tstca-objct,
          values TYPE tt_values,
        END OF ts_authority .

  constants:
    BEGIN OF tran_types,
             dialog  TYPE sychar01 VALUE ststc_c_type_dialog,
             report  TYPE sychar01 VALUE ststc_c_type_report,
             param   TYPE sychar01 VALUE ststc_c_type_parameters,
             variant TYPE sychar01 VALUE ststc_c_type_variant,
             object  TYPE sychar01 VALUE ststc_c_type_object,    " Not supported yet
* not used yet
*  ststc_c_xxx     type sychar01 value 'A'.           "Objekt
*
           END OF tran_types .
  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .
  data AUTHORIZATION type TS_AUTHORITY .
  data PARAMETERS type TT_PARAMS .
  data ZL_OBJECT type TO_RAW .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods DATA_2_TYPE
    importing
      !FM_DATA type TS_FM_DATA
    returning
      value(RESULT) type RGLIF-DOCUTYPE .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
protected section.

  data TRANSACTION type TS_SUB_OBJ .
  data PROGRAM type TS_SUB_OBJ .
  data OBJECT type TS_SUB_OBJ .

  methods ANONYMIZE .
  methods UNANONYMIZE .
private section.

  constants:
    begin of tran_kinds,
       Trans_d type TSTC-cinfo value '00',
       report  type TSTC-cinfo value '80',
       param   type TSTC-cinfo value '02',
       variant type TSTC-cinfo value '10',    " Variant mask with report
       Object  type TSTC-cinfo value '08',    " Not supported yet
* not used yet
*      hex_men type x value '01',               " Bereichsmen#        M
*      hex_chk type x value '04',               " mit Pr#fobjekt
*      hex_enq type x value '20'.               " Gesperrt #ber SM01
*
     end of tran_kinds .
ENDCLASS.



CLASS ZAPLINK_TRAN_DATA IMPLEMENTATION.


  method ANONYMIZE.
  endmethod.


  method DATA_2_TYPE.
  endmethod.


  method FROM_DATA.
  DATA s_text  LIKE LINE OF texts.
  DATA s_auth  TYPE ts_authority.
  DATA s_value LIKE LINE OF s_auth-values.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF fm_data-tstca,
    <t> LIKE LINE OF fm_data-tstct.

  MOVE-CORRESPONDING fm_data-tstc TO a0_maindata.
  MOVE-CORRESPONDING fm_data-tstcc TO a0_maindata.
  MOVE-CORRESPONDING fm_data-rsstcd TO a0_maindata.
  a0_maindata-gui_inh = fm_data-gui_inh.
  clear a0_maindata-tc_type.          " transaction type in text

  parameters = fm_data-t_params.

  LOOP AT fm_data-tstct ASSIGNING <t>.
    CLEAR s_text.
    s_text-langu = <t>-sprsl.
    s_text-short_txt = <t>-ttext.
    INSERT s_text INTO TABLE texts.
  ENDLOOP.

  CLEAR authorization.
  LOOP AT fm_data-tstca ASSIGNING <o>.
    AT NEW objct.
      CLEAR s_auth.
      s_auth-object = <o>-objct.
    ENDAT.

    CLEAR s_value.
    s_value-field = <o>-field.
    s_value-value = <o>-value.
    INSERT s_value INTO TABLE s_auth-values.

    AT END OF objct.
      IF NOT authorization IS INITIAL.
*        raise EXCEPTION
      ENDIF.
      authorization = s_auth.
    ENDAT.
  ENDLOOP.
*  DEFINE mac_flags.
*    &1-webgui  = fm_data-html_en.
*    &1-java    = fm_data-java_en.
*    &1-sapgui  = fm_data-wingui_en.
*    &1-srvfile = fm_data-servicefile.
*  END-OF-DEFINITION.
*
*  a0_maindata-tcode = fm_data-tcode.
*  a0_maindata-type = data_2_type( fm_data ).
*
*  CASE a0_maindata-type.
*    WHEN tran_types-dialog.
*      a0_maindata-dialog-program = fm_data-program.
*      a0_maindata-dialog-dynnpro = fm_data-dynpro.
*      mac_flags a0_maindata-dialog.
*    WHEN tran_types-report.
*      a0_maindata-report-program = fm_data-program.
*      a0_maindata-report-variant = fm_data-variant.
*      mac_flags a0_maindata-report.
*    WHEN tran_types-param.
*      a0_maindata-params-program = fm_data-program.
*      a0_maindata-params-dynnpro = fm_data-dynpro.
*      a0_maindata-params-called_t = fm_data-called_t.
*      a0_maindata-params-skip_1st = fm_data-trans_skip.
*      mac_flags a0_maindata-params.
*    WHEN tran_types-variant.
*      a0_maindata-variant-called_t = fm_data-called_t.
*      a0_maindata-variant-variant = fm_data-variant.
*      a0_maindata-variant-indep = fm_data-cl_indep.
*    WHEN tran_types-object.   " not supported yet
*    WHEN OTHERS.
** Not expected
*  ENDCASE.
  endmethod.


  method TO_DATA.
  DATA s_text  LIKE LINE OF fm_data-tstct.
  DATA s_value LIKE LINE OF fm_data-tstca.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF authorization-values,
    <t> LIKE LINE OF texts.

  MOVE-CORRESPONDING a0_maindata TO fm_data-tstc.
  MOVE-CORRESPONDING a0_maindata TO fm_data-tstcc.
  MOVE-CORRESPONDING a0_maindata TO fm_data-rsstcd.
  fm_data-gui_inh = a0_maindata-gui_inh.
*  clear a0_maindata-tc_type.          " transaction type in text

  fm_data-t_params = parameters.

  LOOP AT texts ASSIGNING <t>.
    CLEAR s_text.
    s_text-tcode = a0_maindata-tcode.
    s_text-sprsl = <t>-langu.
    s_text-ttext = <t>-short_txt.
    INSERT s_text INTO TABLE fm_data-tstct.
  ENDLOOP.

  LOOP AT authorization-values ASSIGNING <o>.
    CLEAR s_value.
    s_value-tcode = a0_maindata-tcode.
    s_value-objct = authorization-object.
    s_value-field = <o>-field.
    s_value-value = <o>-value.
    INSERT s_value INTO TABLE fm_data-tstca.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
