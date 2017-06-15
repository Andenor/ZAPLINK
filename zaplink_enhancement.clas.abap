class ZAPLINK_ENHANCEMENT definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public

  global friends ZAPLINK_ENHANCEMENT_DATA .

public section.
  type-pools SEEX .
  class ZAPLINK_ENHANCEMENT_DATA definition load .

  aliases TT_LTEXTS
    for ZAPLINK_DATATYPES~TT_LTEXTS .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_ENHENCEMENT' ##NO_TEXT.
  constants ST_COMP_ENH_IMPL type TD_COMPTYPE value 'ENHC' ##NO_TEXT.
  constants ST_COMP_ENH_SPOT type TD_COMPTYPE value 'ENSC' ##NO_TEXT.
  constants ST_ENH_IMPL type TD_COMPTYPE value 'ENHO' ##NO_TEXT.
  constants ST_ENH_SPOT type TD_COMPTYPE value 'ENHS' ##NO_TEXT.
  constants:
    BEGIN OF tooltypes,
* List of type from table ENHTOOLS.
        badi            TYPE enhheader-enhtooltype VALUE 'BADI_IMPL',
        class           TYPE enhheader-enhtooltype VALUE 'CLASENH',
        function_group  TYPE enhheader-enhtooltype VALUE 'FUGRENH',
        interface       TYPE enhheader-enhtooltype VALUE 'INTFENH',
        hook            TYPE enhheader-enhtooltype VALUE 'HOOK_IMPL',
        web_dynpro      TYPE enhheader-enhtooltype VALUE 'WDYENH',
      END OF tooltypes .

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  class-methods CREATE_SOTR_ID
    returning
      value(RESULT) type ZAPLINK_ENHANCEMENT_DATA=>TD_SOTR_CONCEPT .

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

  types TD_SOTR_CONCEPT type SOTR_TEXT-CONCEPT .
  types:
    BEGIN OF ts_sotr,
      short TYPE td_SOTR_CONCEPT,
      long  TYPE td_SOTR_CONCEPT,
    END OF ts_sotr .

  methods SET_SOTR
    importing
      !SOTR type TS_SOTR
      !TEXTS type TT_LTEXTS .
private section.

  types TD_COMP_ENH_SPOT_NAME type ENHSPOTCOMPOSITENAME .
  types TD_ENH_IMPL_NAME type ENHNAME .
  types TD_COMP_ENH_IMPL_NAME type ENHCOMPOSITENAME .
  types TD_ENH_SPOT_NAME type ENHSPOTNAME .
  types TO_COMP_ENH_SPOT type ref to ZAPLINK_ENSC_DATA .
  types TO_COMP_ENH_SPOT_SAP type ref to CL_ENH_SPOT_COMPOSITE .
  types TO_ENH_SPOT type ref to ZAPLINK_ENHS_DATA .
  types TO_ENH_SPOT_SAP type ref to IF_ENH_SPOT_TOOL .
  types TO_ENH_IMPL type ref to ZAPLINK_ENHO_DATA .
  types TO_ENH_IMPL_SAP type ref to CL_ABSTRACT_ENH_TOOL .
  types TO_ENHANCEMENT_SAP type ref to IF_ENH_OBJECT .
  types TO_COMP_ENH_IMPL type ref to ZAPLINK_ENHC_DATA .
  types TO_COMP_ENH_IMPL_SAP type ref to CL_ENH_COMPOSITE .

  class-data R_DOC_ID_DEFINITION type TR_DOCID .
  class-data R_DOC_ID_IMPLEMENTATION type TR_DOCID .
  constants _UUID type TD_CONNUUID value '4181014E2AC27D33E1000000AC1201D6' ##NO_TEXT.
  constants _VER type TD_CONNVER value '0.0' ##NO_TEXT.

  methods REVERSE_RAW
    importing
      !OFF_SUB_COMPONENTS type TD_WITH_SUBCOMP optional
      !COMPONENTS_LIST type TO_LIST
      !RAW_OBJECT type TO_RAW_DATA
      !NAME type TD_COMPNAME
    returning
      value(SAP_OBJECT) type TO_ENHANCEMENT_SAP .
  methods REVERSE_COMP_ENH_SPOT
    importing
      !OFF_SUB_COMPONENTS type TD_WITH_SUBCOMP optional
      !COMPONENTS_LIST type TO_LIST
      !RAW_OBJECT type TO_COMP_ENH_SPOT
    returning
      value(SAP_OBJECT) type TO_COMP_ENH_SPOT_SAP .
  methods REVERSE_ENH_SPOT
    importing
      !OFF_SUB_COMPONENTS type TD_WITH_SUBCOMP optional
      !COMPONENTS_LIST type TO_LIST
      !RAW_OBJECT type TO_ENH_SPOT
    returning
      value(SAP_OBJECT) type TO_ENH_SPOT_SAP .
  methods REVERSE_COMP_ENH_IMPL
    importing
      !OFF_SUB_COMPONENTS type TD_WITH_SUBCOMP optional
      !COMPONENTS_LIST type TO_LIST
      !RAW_OBJECT type TO_COMP_ENH_IMPL
    returning
      value(SAP_OBJECT) type TO_COMP_ENH_IMPL_SAP .
  methods REVERSE_ENH_IMPL
    importing
      !OFF_SUB_COMPONENTS type TD_WITH_SUBCOMP optional
      !COMPONENTS_LIST type TO_LIST
      !RAW_OBJECT type TO_ENH_IMPL
    returning
      value(SAP_OBJECT) type TO_ENH_IMPL_SAP .
  methods CONVERT_ENHANCEMENT
    importing
      !SUB_COMPONENTS type TD_WITH_SUBCOMP
      value(SAP_OBJECT) type TO_ENHANCEMENT_SAP
      !COMPONENTS_LIST type TO_LIST
    returning
      value(RAW_OBJECT) type TO_RAW_DATA .
  methods CONVERT_COMP_ENH_SPOT
    importing
      !SUB_COMPONENTS type TD_WITH_SUBCOMP
      value(SAP_OBJECT) type TO_COMP_ENH_SPOT_SAP
      !COMPONENTS_LIST type TO_LIST
    returning
      value(RAW_OBJECT) type TO_COMP_ENH_SPOT .
  methods CONVERT_ENH_SPOT
    importing
      !SUB_COMPONENTS type TD_WITH_SUBCOMP
      value(SAP_OBJECT) type TO_ENH_SPOT_SAP
      !COMPONENTS_LIST type TO_LIST
    returning
      value(RAW_OBJECT) type TO_ENH_SPOT .
  methods CONVERT_COMP_ENH_IMPL
    importing
      !SUB_COMPONENTS type TD_WITH_SUBCOMP
      value(SAP_OBJECT) type TO_COMP_ENH_IMPL_SAP
      !COMPONENTS_LIST type TO_LIST
    returning
      value(RAW_OBJECT) type TO_COMP_ENH_IMPL .
  methods CONVERT_ENH_IMPL
    importing
      !SUB_COMPONENTS type TD_WITH_SUBCOMP
      value(SAP_OBJECT) type TO_ENH_IMPL_SAP
      !COMPONENTS_LIST type TO_LIST
    returning
      value(RAW_OBJECT) type TO_ENH_IMPL .
  methods OFF_EXPORT_FROM_ENHANCEMENT
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_RAW
    raising
      ZAPLINK_CX_CONNECTOR .
  methods OFF_EXPORT_FROM_COMP_ENH_SPOT
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_COMP_ENH_SPOT
    raising
      ZAPLINK_CX_CONNECTOR .
  methods OFF_EXPORT_FROM_ENH_SPOT
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_ENH_SPOT
    raising
      ZAPLINK_CX_CONNECTOR .
  methods OFF_EXPORT_FROM_COMP_ENH_IMPL
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_COMP_ENH_IMPL
    raising
      ZAPLINK_CX_CONNECTOR .
  methods OFF_EXPORT_FROM_ENH_IMPL
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(OBJECT) type TO_ENH_IMPL
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods GET_VERSION
    importing
      !ENHANCEMENT type ref to IF_ENH_OBJECT
    returning
      value(VERSION) type R3STATE .
  methods IMPORT_TO_COMP_ENH_SPOT
    importing
      !O_DATA type TO_COMP_ENH_SPOT
    returning
      value(COMPONENTS) type TO_LIST
    raising
      ZAPLINK_CX_CONNECTOR .
  methods LOAD_COMP_ENH_SPOT
    importing
      !COMPONENT type TO_COMPONENT
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_COMP_ENH_SPOT_SAP .
  methods LOAD_ENH_SPOT
    importing
      !COMPONENT type TO_COMPONENT
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_ENH_SPOT_SAP .
  methods LOAD_ENH_IMPL
    importing
      !COMPONENT type TO_COMPONENT
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_ENH_IMPL_SAP .
  methods LOAD_COMP_ENH_IMPL
    importing
      !COMPONENT type TO_COMPONENT
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_COMP_ENH_IMPL_SAP
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _LOAD_ENH_SPOT
    importing
      !NAME type TD_ENH_SPOT_NAME
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_ENH_SPOT_SAP .
  methods _LOAD_COMP_ENH_SPOT
    importing
      !NAME type TD_COMP_ENH_SPOT_NAME
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_COMP_ENH_SPOT_SAP
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _LOAD_ENH_IMPL
    importing
      !NAME type TD_ENH_IMPL_NAME
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_ENH_IMPL_SAP .
  methods _LOAD_COMP_ENH_IMPL
    importing
      !NAME type TD_COMP_ENH_IMPL_NAME
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_COMP_ENH_IMPL_SAP .
  methods LOAD_ENHANCEMENT
    importing
      !COMPONENT type TO_COMPONENT
      !LOCKING type ENHBOOLEAN optional
    returning
      value(OBJECT) type TO_ENHANCEMENT_SAP
    raising
      ZAPLINK_CX_CONNECTOR .
  methods _ENH_SPOT_EXISTS
    importing
      !NAME type TD_ENH_SPOT_NAME
    returning
      value(EXISTS) type TD_COMPEXISTS .
  methods _COMP_ENH_SPOT_EXISTS
    importing
      !NAME type TD_COMP_ENH_SPOT_NAME
    returning
      value(EXISTS) type TD_COMPEXISTS .
  methods _ENH_IMPL_EXISTS
    importing
      !NAME type TD_ENH_IMPL_NAME
    returning
      value(EXISTS) type TD_COMPEXISTS .
  methods _COMP_ENH_IMPL_EXISTS
    importing
      !NAME type TD_COMP_ENH_IMPL_NAME
    returning
      value(EXISTS) type TD_COMPEXISTS .
ENDCLASS.



CLASS ZAPLINK_ENHANCEMENT IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*BI       Business Add-Ins: Implementations
  DATA _id LIKE LINE OF r_doc_id_definition.

  _id-sign = 'I'.
  _id-option = 'EQ'.
  _id-low = 'HY'. APPEND _id TO r_doc_id_definition.

  _id-low = 'BI'. APPEND _id TO r_doc_id_implementation.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = st_enh_spot.        INSERT type INTO TABLE supported_types.
  type-type = st_comp_enh_spot.   INSERT type INTO TABLE supported_types.
  type-type = st_enh_impl.        INSERT type INTO TABLE supported_types.
  type-type = st_comp_enh_impl.   INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method CONVERT_COMP_ENH_IMPL.
  DATA s_comp         TYPE zaplink_enhc_data=>ts_fm_data.
  DATA version        TYPE r3state.
  DATA o_enh          TYPE to_enh_impl_sap.
  DATA o_comp         TYPE to_comp_enh_impl_sap.
  FIELD-SYMBOLS:
    <enh>   LIKE LINE OF raw_object->enhancement_implementations,
    <comp>  LIKE LINE OF raw_object->composit_enh_implementations.

  ASSERT sap_object IS BOUND.
  CREATE OBJECT raw_object.
  version = get_version( sap_object ).
  s_comp-header = sap_object->get_enhcompheader( version ).
  IF sub_components >= sub_component-with_required.
    s_comp-comp_enh = sap_object->if_enh_composite~get_composite_childs( version ).
    s_comp-enh = sap_object->if_enh_composite~get_enh_childs( version ).
  ENDIF.
  raw_object->from_data( s_comp ).

* Sub component as object if present (depend on sub_component value)
  LOOP AT raw_object->enhancement_implementations ASSIGNING <enh>.
    o_enh = _load_enh_impl( <enh>-name ).
    CHECK o_enh IS BOUND.
    CREATE OBJECT <enh>-zl_object.
    <enh>-zl_object->raw = convert_enh_impl( sap_object = o_enh
                                         sub_components = sub_components
                                        components_list = components_list ).
    IF NOT <enh>-zl_object->raw IS BOUND.    CLEAR <enh>-zl_object.     ENDIF.
    IF <enh>-zl_object IS BOUND.      CLEAR <enh>-name.       ENDIF.
  ENDLOOP.

  LOOP AT raw_object->composit_enh_implementations ASSIGNING <comp>.
    o_comp = _load_comp_enh_impl( <comp>-name ).
    CHECK o_comp IS BOUND.
    CREATE OBJECT <comp>-zl_object.
    <comp>-zl_object->raw = convert_comp_enh_impl( sap_object = o_comp
                                               sub_components = sub_components
                                              components_list = components_list ).
    IF NOT <comp>-zl_object->raw IS BOUND.    CLEAR <comp>-zl_object.     ENDIF.
    IF <comp>-zl_object IS BOUND.      CLEAR <comp>-name.       ENDIF.
  ENDLOOP.

  CLEAR raw_object->a0_maindata-enhcomposite.
  endmethod.


  method CONVERT_COMP_ENH_SPOT.
  DATA s_comp         TYPE zaplink_ensc_data=>ts_fm_data.
  DATA version        TYPE r3state.
*  DATA short_id       TYPE sotr_conc.
*  DATA long_id        TYPE sotr_conc.
  DATA o_enh          TYPE to_enh_spot_sap.
  DATA o_comp         TYPE to_comp_enh_spot_sap.
  FIELD-SYMBOLS:
    <enh>   LIKE LINE OF raw_object->enhancements,
    <comp>  LIKE LINE OF raw_object->composit_enhancements.

  ASSERT sap_object IS BOUND.
  CREATE OBJECT raw_object.
  version = get_version( sap_object ).
  s_comp-header = sap_object->get_enhspotcomphead( version ).
  IF sub_components >= sub_component-with_required.
    s_comp-comp_enh = sap_object->if_enh_spot_composite~get_composite_childs( version ).
    s_comp-enh = sap_object->if_enh_spot_composite~get_enh_spot_childs( version ).
  ENDIF.
  raw_object->from_data( s_comp ).

* Sub objects as object if present (depend on sub_component)
  LOOP AT raw_object->enhancements ASSIGNING <enh>.
    o_enh = _load_enh_spot( <enh>-name ).
    CHECK o_enh IS BOUND.
    CREATE OBJECT <enh>-zl_object.
    <enh>-zl_object->raw = convert_enh_spot( sap_object = o_enh
                                         sub_components = sub_components
                                        components_list = components_list ).
    IF NOT <enh>-zl_object->raw IS BOUND.    CLEAR <enh>-zl_object.     ENDIF.
    IF <enh>-zl_object IS BOUND.      CLEAR <enh>-name.       ENDIF.
  ENDLOOP.

  LOOP AT raw_object->composit_enhancements ASSIGNING <comp>.
    o_comp = _load_comp_enh_spot( <comp>-name ).
    CHECK o_comp IS BOUND.
    CREATE OBJECT <comp>-zl_object.
    <comp>-zl_object->raw = convert_comp_enh_spot( sap_object = o_comp
                                               sub_components = sub_components
                                              components_list = components_list ).
    IF NOT <comp>-zl_object->raw IS BOUND.    CLEAR <comp>-zl_object.     ENDIF.
    IF <comp>-zl_object IS BOUND.      CLEAR <comp>-name.       ENDIF.
  ENDLOOP.

  CLEAR raw_object->a0_maindata-enhspotcomposite.
  endmethod.


  method CONVERT_ENHANCEMENT.
  DATA o_ces      TYPE to_comp_enh_spot_sap.
  DATA o_es       TYPE to_enh_spot_sap.
  DATA o_cei      TYPE to_comp_enh_impl_sap.
  DATA o_ei       TYPE to_enh_impl_sap.

* Composit Enhancement Spot
  TRY.
      o_ces ?= sap_object.
      raw_object = convert_comp_enh_spot( sap_object = o_ces
                                      sub_components = sub_components
                                     components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  CHECK raw_object IS NOT BOUND.

* Enhancement Spot
  TRY.
      o_es ?= sap_object.
      raw_object = convert_enh_spot( sap_object = o_es
                                 sub_components = sub_components
                                components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  CHECK raw_object IS NOT BOUND.

* Composit Enhancement Implementation
  TRY.
      o_cei ?= sap_object.
      raw_object = convert_comp_enh_impl( sap_object = o_cei
                                      sub_components = sub_components
                                     components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  CHECK raw_object IS NOT BOUND.

* Enhancement Implementation
  TRY.
      o_ei ?= sap_object.
      raw_object = convert_enh_impl( sap_object = o_ei
                                 sub_components = sub_components
                                components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  ASSERT raw_object IS BOUND.
  endmethod.


  method CONVERT_ENH_IMPL.
  DATA s_comp         TYPE zaplink_enho_data=>ts_fm_data.
  DATA version        TYPE r3state.
  DATA name           TYPE enhname.
  DATA o_store        TYPE REF TO cl_enh_store.   " if_enh_store.
  DATA o_badi_impl    TYPE REF TO cl_enh_tool_badi_impl.
  DATA o_clasenh      TYPE REF TO cl_enh_tool_class.
  DATA o_fugrenh      TYPE REF TO cl_enh_tool_fugr.
  DATA o_hook_impl    TYPE REF TO cl_enh_tool_hook_impl.
  DATA o_intfenh      TYPE REF TO cl_enh_tool_intf.
  DATA o_wdyenh       TYPE REF TO cl_enh_tool_wdy.
  DATA hdr            TYPE enhheader.

  ASSERT sap_object IS BOUND.
  CREATE OBJECT raw_object.
  version = get_version( sap_object ).
*  name = s_comp-header-spot_name = sap_object->if_enh_tool~get_name( ).

* Texts
  o_store ?= sap_object->get_store( ).
  hdr = o_store->get_enhheader( version ).
  s_comp-header-text_ids-short = hdr-shorttext_id.
  s_comp-header-text_ids-long = hdr-docu_id.
  CLEAR: hdr-data, hdr-data_version.    " binary data

* Main Screen from FORM fuell_dy_enhspot (LENHANCEMENT_SPOT_EDITORF01)
*  s_comp-header-scr-devclass = i_ref_tool->if_enh_object~get_devclass( ).  " already before
  CALL METHOD sap_object->if_enh_object~get_language
    IMPORTING
      org_langu = s_comp-header-scr-langu.
*  CALL METHOD o_store->if_enh_spot_store~get_attributes
*    EXPORTING
*      version   = version
*    IMPORTING
*      author    = s_comp-header-scr-author
*      createdon = s_comp-header-scr-createdon
*      changedby = s_comp-header-scr-changedby
*      changedon = s_comp-header-scr-changedon.

* List of type from table ENHTOOLS.
  CASE hdr-enhtooltype.
    WHEN tooltypes-badi.
      o_badi_impl ?= sap_object.
* Implementations
*      s_comp-badi_impl-enh = o_badi_impl->get_implementations( version ).
      o_badi_impl->if_enh_cwb~get_data( EXPORTING i_version = version
                                        IMPORTING    e_data = s_comp-badi_impl-admin ).
    WHEN tooltypes-class.
      o_clasenh ?= sap_object.
      o_clasenh->if_enh_cwb~get_data( EXPORTING i_version = version
                                      IMPORTING    e_data = s_comp-clif_impl ).
    WHEN tooltypes-function_group.
      o_fugrenh ?= sap_object.
      o_fugrenh->if_enh_cwb~get_data( EXPORTING i_version = version
                                      IMPORTING    e_data = s_comp-fugr_impl ).
    WHEN tooltypes-hook.
      o_hook_impl ?= sap_object.
      o_hook_impl->if_enh_cwb~get_data( EXPORTING i_version = version
                                        IMPORTING    e_data = s_comp-hook_impl-admin ).
    WHEN tooltypes-interface.
      o_intfenh ?= sap_object.
      o_intfenh->if_enh_cwb~get_data( EXPORTING i_version = version
                                      IMPORTING    e_data = s_comp-clif_impl ).
    WHEN tooltypes-web_dynpro.
      o_wdyenh ?= sap_object.
      o_wdyenh->if_enh_cwb~get_data( EXPORTING i_version = version
                                     IMPORTING    e_data = s_comp-wdyn_impl ).
  ENDCASE.

*Composite Parent
* From : Module STATUS_0100 in SAPLENHANCEMENT_EDITOR
  s_comp-header-composite = cl_enh_factory=>enh_find_parent_composite( name ) .
  s_comp-header-hdr = hdr.

* From : FORM lesen_sfw in SAPLENHANCEMENT_EDITOR
*  l_wa_swf-objecttype = 'ENHO'.
*  l_wa_swf-objectname = i_objekt.
*
*  APPEND l_wa_swf TO l_it_swf.
*
*  CALL FUNCTION 'SFW_GET_SWITCHPOS'
*    EXPORTING
*      provide_text = 'X'
*    TABLES
*      switchtab    = l_it_swf.
*
*  SORT l_it_swf BY objecttype
*                   objectname.
*
*  READ TABLE l_it_swf WITH KEY objecttype = g_object_type    "'ENHO'
*                               objectname = enh_edt_layout-object1
*                      INTO l_wa_swf BINARY SEARCH.
*
*  IF sy-subrc = 0.
*    MOVE :
*       l_wa_swf-state        TO p_switch_pos,
*       l_wa_swf-text         TO p_switch_text,
*       l_wa_swf-switch_id    TO p_switch_id.
*  ENDIF.

* IF_ENH_PERSIST~LOAD_LOGS => Creation date

  raw_object->from_data( s_comp ).

*  IF sub_components = abap_true.
*  ENDIF.
  CLEAR raw_object->a0_maindata-composite.
  endmethod.


  method CONVERT_ENH_SPOT.
  DATA s_comp           TYPE zaplink_enhs_data=>ts_fm_data.
  DATA version          TYPE r3state.
  DATA name             TYPE enhspotcompositename.
  DATA texts            TYPE zaplink_enhs_data=>ts_sotr.
*  DATA et_textvers      TYPE enhtext_vers_it.
*  DATA et_textversdata  TYPE enhtext_versdata_it.

  ASSERT sap_object IS BOUND.
  CREATE OBJECT raw_object.
  s_comp-header-version = version = get_version( sap_object ).
  s_comp-header-name = name = sap_object->get_name( ).

* Tool Type : BADI or not BADI
  s_comp-header-tool_type = sap_object->get_tool( ).
* Texts
* From CL_R3STANDARD_PERSISTENCE=>LOAD_SPOT
  SELECT SINGLE shorttextid docuid
    INTO (texts-short, texts-long)
    FROM enhspotheader
    WHERE enhspot = name
      AND version = 'I'.
  IF sy-subrc <> 0.
    SELECT SINGLE shorttextid docuid
      INTO (texts-short, texts-long)
      FROM enhspotheader
      WHERE enhspot = name.
*        AND version = 'A'.
  ENDIF.
  IF texts IS NOT INITIAL.
    s_comp-texts = raw_object->sotr_2_texts( texts ).
  ENDIF.
* From CL_ENH_SPOT_EDITOR=>HANDLE_SPOT_NEW :
*      CALL METHOD sap_object->get_all_elements
*        EXPORTING
*          version  = l_state
*        RECEIVING
*          elements = l_it_elem_it.
**     Verarbeitung der ENHOBJ-S#tze
*      LOOP AT l_it_elem_it INTO l_r_w_elem.
*        CASE l_r_w_elem->get_type( ).
*          WHEN 'R3OB'.
*            l_r_obj_int = l_r_w_elem->get_as_sap_object_element( ).
*            l_enhobj = l_r_obj_int->get_enhspotobj( ).
*            APPEND l_enhobj TO l_it_enhobj.
* look at implementation
*      CALL METHOD cl_r3standard_persistence=>find_enhancements_by_obj
*        EXPORTING
*          obj_type        = 'ENHS'
*          obj_name        = l_obj_name
**          version         = l_state
*        RECEIVING
*          enhancement_ids = l_it_enhname.

* Language : Form FUELL_DY_ENHSPOT in program SAPLENHANCEMENT_SPOT_EDITOR
  sap_object->if_enh_object~get_language( IMPORTING org_langu = s_comp-header-lang-language ) .
*                                                   modi_langu = s_comp-header-lang-modi_langu ).

* Attributes
  sap_object->get_attributes(
      EXPORTING version = version
      IMPORTING  author = s_comp-header-attrs-author
              createdon = s_comp-header-attrs-createdon
              changedby = s_comp-header-attrs-changedby
              changedon = s_comp-header-attrs-changedon
      ).
* Composite
  s_comp-header-composite = cl_enh_factory=>enhspot_find_parent_composite( name ).
* Internal use
  s_comp-header-internal_use = sap_object->get_internal( version ).

*#TODO  s_comp-def_data = sap_object->get_badi_defs( version ).

  raw_object->from_data( s_comp ).

*  IF sub_components = abap_true.
*  ENDIF.
  CLEAR raw_object->a0_maindata-name.
  endmethod.


  method CREATE_SOTR_ID.
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
*     EV_GUID_16       =
*     EV_GUID_22       =
      ev_guid_32       = result.
  ASSERT result IS NOT INITIAL.
  endmethod.


  method GET_VERSION.
  IF enhancement->has_active_version( ) = abap_true.
    version = 'A'.
  ELSE.
    version = 'I'.
  ENDIF.
  endmethod.


  method IMPORT_TO_COMP_ENH_SPOT.
  DATA s_comp     TYPE zaplink_ensc_data=>ts_fm_data.
  DATA o_comp     TYPE REF TO if_enh_spot_composite.    "cl_enh_spot_composite.

  s_comp = o_data->to_data( ).
  CALL METHOD cl_r3standard_persistence=>create_spot_composite
    EXPORTING
      name      = o_data->a0_maindata-enhspotcomposite
      scwb_call = abap_false
      run_dark  = abap_true
    IMPORTING
      composite = o_comp
*          CHANGING
*            trkorr    = trkorr
*            devclass  = devclass
    .
*  o_comp->set_shorttext( l_text ).
*  o_comp->add_composite_child( enhspotcompcreate-enhspotcomposite ).
*  o_comp->add_enh_spot_child( ).
  o_comp->if_enh_object~save( ).
  endmethod.


  method LOAD_COMP_ENH_IMPL.
  DATA _name      TYPE td_enh_impl_name.

  ASSERT component->get_type( ) = st_comp_enh_impl.
  _name = component->get_name( ).

  object ?= _load_comp_enh_impl( name = _name
                              locking = locking ).
  endmethod.


  method LOAD_COMP_ENH_SPOT.
  DATA _name      TYPE td_comp_enh_spot_name.

  ASSERT component->get_type( ) = st_comp_enh_spot.
  _name = component->get_name( ).

  object ?= _load_comp_enh_spot( name = _name
                              locking = locking ).
  endmethod.


  method LOAD_ENHANCEMENT.
  DATA d_type         TYPE td_comptype.

  TRY.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_enh_spot.
        object = load_enh_spot( component = component
                                  locking = locking ).
      WHEN st_comp_enh_spot.
        object = load_comp_enh_spot( component = component
                                       locking = locking ).
      WHEN st_enh_impl.
        object = load_enh_impl( component = component
                                  locking = locking ).
      WHEN st_comp_enh_impl.
        object = load_comp_enh_impl( component = component
                                       locking = locking ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method LOAD_ENH_IMPL.
  DATA _name      TYPE td_enh_impl_name.

  ASSERT component->get_type( ) = st_enh_impl.
  _name = component->get_name( ).

  object = _load_enh_impl( name = _name
                        locking = locking ).
  endmethod.


  method LOAD_ENH_SPOT.
  DATA _name      TYPE td_enh_spot_name.

  ASSERT component->get_type( ) = st_enh_spot.
  _name = component->get_name( ).

  object ?= _load_enh_spot( name = _name
                         locking = locking ).
  endmethod.


  method OFF_EXPORT_FROM_COMP_ENH_IMPL.
  DATA f_subcomp  TYPE td_with_subcomp.
  DATA o_list     TYPE to_list.
  DATA o_sap_intf TYPE to_comp_enh_impl_sap.

  TRY.
    o_sap_intf = load_comp_enh_impl( component ).
    ASSERT o_sap_intf is bound.

    f_subcomp = component->get_with_subcomp( ).
    CREATE OBJECT o_list.
    component->set_subcomponents( o_list ).

*    object = convert_comp_enh_spot( sap_object = o_sap_intf
*                                sub_components = f_subcomp
*                               components_list = o_list ).

    TRY.
        o_list->remove( component ).
      CATCH zaplink_cx_list.
    ENDTRY.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method OFF_EXPORT_FROM_COMP_ENH_SPOT.
  DATA f_subcomp  TYPE td_with_subcomp.
  DATA o_list     TYPE to_list.
  DATA o_sap_intf TYPE to_comp_enh_spot_sap.

  TRY.
    o_sap_intf = load_comp_enh_spot( component ).
    ASSERT o_sap_intf is bound.

    f_subcomp = component->get_with_subcomp( ).
    CREATE OBJECT o_list.
    component->set_subcomponents( o_list ).

    object = convert_comp_enh_spot( sap_object = o_sap_intf
                                sub_components = f_subcomp
                               components_list = o_list ).

    TRY.
        o_list->remove( component ).
      CATCH zaplink_cx_list.
    ENDTRY.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method OFF_EXPORT_FROM_ENHANCEMENT.
*  DATA f_subcomp  TYPE td_with_subcomp.
*  DATA o_list     TYPE to_list.
*  DATA o_sap_intf TYPE to_enhancement_sap.
*
*  TRY.
*    o_sap_intf = load_comp_enh_spot( component ).
*    ASSERT o_sap_intf is bound.
*
*    f_subcomp = component->get_with_subcomp( ).
*    CREATE OBJECT o_list.
*    component->set_subcomponents( o_list ).
*
*    object = convert_enhancement( sap_object = o_sap_intf
*                              sub_components = f_subcomp
*                             components_list = o_list ).
*
*    TRY.
*        o_list->remove( component ).
*      CATCH zaplink_cx_list.
*    ENDTRY.
*
*    mac_def_catch zaplink_cx_connector.
*  ENDTRY.
  endmethod.


  method OFF_EXPORT_FROM_ENH_IMPL.
  DATA f_subcomp  TYPE td_with_subcomp.
  DATA o_list     TYPE to_list.
  DATA o_sap_intf TYPE to_comp_enh_spot_sap.

  TRY.
    o_sap_intf = load_comp_enh_spot( component ).
    ASSERT o_sap_intf is bound.

    f_subcomp = component->get_with_subcomp( ).
    CREATE OBJECT o_list.
    component->set_subcomponents( o_list ).

*    object = convert_comp_enh_spot( sap_object = o_sap_intf
*                                sub_components = f_subcomp
*                               components_list = o_list ).

    TRY.
        o_list->remove( component ).
      CATCH zaplink_cx_list.
    ENDTRY.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method OFF_EXPORT_FROM_ENH_SPOT.
*  DATA f_subcomp  TYPE td_with_subcomp.
*  DATA o_list     TYPE to_list.
*  DATA o_sap_intf TYPE to_comp_enh_spot_sap.
*
*  TRY.
*
*    o_sap_intf = load_comp_enh_spot( component ).
*    ASSERT o_sap_intf is bound.
*
*    f_subcomp = component->get_with_subcomp( ).
*    CREATE OBJECT o_list.
*    component->set_subcomponents( o_list ).
*
*    object = convert_enh_spot( sap_object = o_sap_intf
*                           sub_components = f_subcomp
*                          components_list = o_list ).
*
*    TRY.
*        o_list->remove( component ).
*      CATCH zaplink_cx_list.
*    ENDTRY.
*
*    mac_def_catch zaplink_cx_connector.
*  ENDTRY.
  endmethod.


  method REVERSE_COMP_ENH_IMPL.
  DATA s_comp         TYPE zaplink_enhc_data=>ts_fm_data.
  DATA o_gen          TYPE REF TO if_enh_composite.
*  DATA name           TYPE  td_comp_enh_spot_name.
  DATA object        TYPE to_comp_enh_impl_sap.
  DATA texts          TYPE zaplink_enhc_data=>ts_sotr.
  DATA l_id           TYPE enhdocuobject.
  FIELD-SYMBOLS: <l>  LIKE LINE OF raw_object->a0_maindata-texts.

  ASSERT raw_object IS BOUND.
  s_comp = raw_object->to_data( ).

* Check for existence & Delete if needed
  object ?= _load_comp_enh_impl( name = s_comp-header-enhcomposite
                              locking = abap_true ).
  IF object IS BOUND.
*    object->delete( run_dark = abap_true ).
*    object->unlock( ).
  ENDIF.

  cl_enh_factory=>create_enhancement_composite(
        EXPORTING name      = s_comp-header-enhcomposite
                  run_dark  = abap_true
        IMPORTING composite = o_gen ).
  sap_object ?= o_gen.

* Add texts
  l_id = texts-long  = create_sotr_id( ).
  sap_object->if_enh_composite~set_longtext_id( l_id ).
  READ TABLE raw_object->a0_maindata-texts ASSIGNING <l>
      WITH KEY lang = sy-langu.
  IF <l> IS ASSIGNED.
    sap_object->if_enh_object_docu~set_shorttext( <l>-text ).    " not blank string
  ELSE.
    sap_object->if_enh_object_docu~set_shorttext( 'X' ).    " not blank string
  ENDIF.
  sap_object->if_enh_object~save( ).
*  s_comp-header = sap_object->get_enhspotcomphead( 'I' ).
  texts-short = sap_object->if_enh_composite~get_shorttext_id(  ).
  set_sotr( sotr = texts
           texts = raw_object->a0_maindata-texts ).
  endmethod.


  method REVERSE_COMP_ENH_SPOT.
  DATA s_comp         TYPE zaplink_ensc_data=>ts_fm_data.
  DATA o_gen          TYPE REF TO if_enh_spot_composite.
  DATA name           TYPE  td_comp_enh_spot_name.
  DATA object	        TYPE REF TO if_enh_object.
  DATA texts          TYPE zaplink_enhs_data=>ts_sotr.
  DATA l_id           TYPE enhdocuobject.
  FIELD-SYMBOLS: <l>  LIKE LINE OF raw_object->a0_maindata-texts.

  ASSERT raw_object IS BOUND.
  s_comp = raw_object->to_data( ).

* Check for existence & Delete if needed
  object ?= _load_comp_enh_spot( name = s_comp-header-enhspotcomposite
                              locking = abap_true ).
  IF object IS BOUND.
    object->delete( run_dark = abap_true ).
    object->unlock( ).
  ENDIF.

* Create new one
  cl_enh_factory=>create_enhancement_spot_comp(
        EXPORTING name      = s_comp-header-enhspotcomposite
                  run_dark  = abap_true
        IMPORTING composite = o_gen ).
  sap_object ?= o_gen.

* Add texts
  l_id = texts-long  = create_sotr_id( ).
  sap_object->if_enh_spot_composite~set_longtext_id( l_id ).
  READ TABLE raw_object->a0_maindata-texts ASSIGNING <l>
      WITH KEY lang = sy-langu.
  IF <l> IS ASSIGNED.
    sap_object->if_enh_object_docu~set_shorttext( <l>-text ).    " not blank string
  ELSE.
    sap_object->if_enh_object_docu~set_shorttext( 'X' ).    " not blank string
  ENDIF.
  sap_object->if_enh_object~save( ).
*  s_comp-header = sap_object->get_enhspotcomphead( 'I' ).
  texts-short = sap_object->if_enh_spot_composite~get_shorttext_id(  ).
  set_sotr( sotr = texts
           texts = raw_object->a0_maindata-texts ).

* Only when sub component
*  LOOP AT s_comp-comp_enh ASSIGNING <ce>.
*    sap_object->if_enh_spot_composite~add_enh_spot_child( <ce> ).
*  ENDLOOP.
*  LOOP AT s_comp-enh ASSIGNING <e>.
*    sap_object->if_enh_spot_composite~add_composite_child( <e> ).
*  ENDLOOP.
  endmethod.


  method REVERSE_ENH_IMPL.
  DATA s_comp         TYPE zaplink_enho_data=>ts_fm_data.
  DATA object	        TYPE REF TO if_enh_object.
  DATA o_gen          TYPE REF TO if_enh_tool.
  DATA txt_id         TYPE enhdocuobject.
*  FIELD-SYMBOLS: <b> LIKE LINE OF s_comp-def_data.

  ASSERT raw_object IS BOUND.
  s_comp = raw_object->to_data( ).

* Check for existence & Delete if needed
  object ?= _load_enh_impl( name = s_comp-header-enhspot
                         locking = abap_true ).
  IF object IS BOUND.
    object->delete( run_dark = abap_true ).
    object->unlock( ).
  ENDIF.

  cl_enh_factory=>create_enhancement( EXPORTING enhname = s_comp-header-enhspot
* From CL_ENH_TOOL_BADI_DEF=>IF_ENH_SPOT_TOOL~IMPLEMENT
                                                enhtype = CL_ABSTRACT_ENH_TOOL_REDEF=>CREDEFINITION
                                            enhtooltype = cl_enh_tool_badi_def=>tooltype
                                                   dark = abap_true
                                  IMPORTING enhancement = o_gen ).
  sap_object ?= o_gen.
*  LOOP AT s_comp-def_data ASSIGNING <b>.
*    sap_object->add_badi_def( <b> ).
*  ENDLOOP.
  endmethod.


  method REVERSE_ENH_SPOT.
*  DATA s_comp         TYPE zaplink_enhs_data=>ts_fm_data.
*  DATA o_gen          TYPE REF TO if_enh_spot_tool.
*  DATA txt_id         TYPE enhdocuobject.
*  FIELD-SYMBOLS: <b> LIKE LINE OF s_comp-def_data.
*
*  ASSERT raw_object IS BOUND.
*  s_comp = raw_object->to_data( ).
*  cl_enh_factory=>create_enhancement_spot( EXPORTING spot_name = s_comp-header-name
*                                                      tooltype = cl_enh_tool_badi_def=>tooltype
*                                                          dark = abap_true
*                                           IMPORTING      spot = o_gen ).
*  sap_object ?= o_gen.
*  LOOP AT s_comp-def_data ASSIGNING <b>.
*    sap_object->add_badi_def( <b> ).
*  ENDLOOP.
**  txt_id = sap_object->get_longtext_id( ).
*
  endmethod.


  method REVERSE_RAW.
  DATA o_ces      TYPE to_comp_enh_spot.
  DATA o_es       TYPE to_enh_spot.
  DATA o_cei      TYPE to_comp_enh_impl.
  DATA o_ei       TYPE to_enh_impl.

* Composit Enhancement Spot
  TRY.
      o_ces ?= raw_object.
      o_ces->a0_maindata-enhspotcomposite = name.
      sap_object = reverse_comp_enh_spot( raw_object = o_ces
*                                      sub_components = sub_components
                                     components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  CHECK sap_object IS NOT BOUND.

* Enhancement Spot
  TRY.
      o_es ?= raw_object.
      o_es->a0_maindata-name = name.
      sap_object = reverse_enh_spot( raw_object = o_es
*                                 sub_components = sub_components
                                components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  CHECK sap_object IS NOT BOUND.

* Composit Enhancement Implementation
  TRY.
      o_cei ?= raw_object.
      o_cei->a0_maindata-enhcomposite = name.
      sap_object = reverse_comp_enh_impl( raw_object = o_cei
*                                      sub_components = sub_components
                                     components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  CHECK sap_object IS NOT BOUND.

* Enhancement Implementation
  TRY.
      o_ei ?= raw_object.
      o_ei->a0_maindata-enhspot = name.
      sap_object = reverse_enh_impl( raw_object = o_ei
*                                 sub_components = sub_components
                                components_list = components_list ).
    CATCH cx_sy_move_cast_error.    " Wrong Type
  ENDTRY.
  ASSERT raw_object IS BOUND.
  endmethod.


  method SET_SOTR.
  DATA _txt     TYPE string.
  DATA s_pack   TYPE devclass.
  DATA l_pack   TYPE devclass.
  DATA s_type   TYPE trobjtype.
  DATA l_type   TYPE trobjtype.
  FIELD-SYMBOLS: <t> LIKE LINE OF texts.

  DATA entry    TYPE sotr_text.

  SELECT SINGLE h~paket u~object        " See note 1481544
    INTO (s_pack, s_type)
    FROM sotr_head AS h INNER JOIN sotr_use AS u ON u~concept = h~concept
    WHERE h~concept = sotr-short.
  SELECT SINGLE h~paket u~object
    INTO (l_pack, l_type)
    FROM sotr_head AS h INNER JOIN sotr_use AS u ON u~concept = h~concept
    WHERE h~concept = sotr-long.
  IF sy-subrc <> 0.   l_pack = s_pack.    l_type = s_type.    ENDIF.

  IF sotr-short IS NOT INITIAL.
    CALL FUNCTION 'BTFR_MAINTAIN_SINGLE_TEXT'
      EXPORTING
        package                            = s_pack
        object_type                        = s_type
        langu                              = sy-langu    "master lang
        concept                            = sotr-short
*         ALIAS                              =
*         TRANSL_TYPE                        =
*         FLAG_STRING                        =
*         OPTIONS_EDITOR                     =
*         OPTIONS_CHECK                      =
*         FLAG_NO_CONTEXT                    =
*         FLAG_DISPLAY                       =
        flag_no_screen                     = abap_true
*         FLAG_CALLED_FROM_EDIT_SCREEN       =
        default_text                       = ` `
        flag_correction_entry              = abap_false
*         CORR_NUM                           =
*         USE_KORRNUM_IMMEDIATEDLY           =
*       IMPORTING
*         FLAG_CANCELLED                     =
*         HEADER                             =
*         TEXT                               =
*         STRING                             =
      EXCEPTIONS
        parameter_error                    = 1
        text_not_found                     = 2
        invalid_object_type                = 3
        invalid_package                    = 4
        header_diff_interface              = 5
        invalid_tadir_entry                = 6
        invalid_transl_type                = 7
        invalid_concept                    = 8
        OTHERS                             = 9.
    IF sy-subrc <> 0.
      mac_raise_mf 'BTFR_MAINTAIN_SINGLE_TEXT' sy-subrc.
    ENDIF.
  ENDIF.

  IF sotr-long IS NOT INITIAL.
    CALL FUNCTION 'BTFR_MAINTAIN_SINGLE_TEXT'
      EXPORTING
        package                            = l_pack
        object_type                        = l_type
        langu                              = sy-langu    " master lang
        concept                            = sotr-long
*         ALIAS                              =
*         TRANSL_TYPE                        =
        flag_string                        = abap_true
*         OPTIONS_EDITOR                     =
*         OPTIONS_CHECK                      =
*         FLAG_NO_CONTEXT                    =
*         FLAG_DISPLAY                       =
        flag_no_screen                     = abap_true
*         FLAG_CALLED_FROM_EDIT_SCREEN       =
        default_text                       = ` `
        flag_correction_entry              = abap_false
*         CORR_NUM                           =
*         USE_KORRNUM_IMMEDIATEDLY           =
*       IMPORTING
*         FLAG_CANCELLED                     =
*         HEADER                             =
*         TEXT                               =
*         STRING                             =
      EXCEPTIONS
        parameter_error                    = 1
        text_not_found                     = 2
        invalid_object_type                = 3
        invalid_package                    = 4
        header_diff_interface              = 5
        invalid_tadir_entry                = 6
        invalid_transl_type                = 7
        invalid_concept                    = 8
        OTHERS                             = 9.
    IF sy-subrc <> 0.
      mac_raise_mf 'BTFR_MAINTAIN_SINGLE_TEXT' sy-subrc.
    ENDIF.
  ENDIF.



  LOOP AT texts ASSIGNING <t>.
    IF <t>-text IS NOT INITIAL.
*      IF <t>-lang = sy-lang.
*        clear entry.
*        entry-concept = sotr-short.
*        CALL FUNCTION 'SOTR_UPDATE_CONCEPT_ENTRY'
*          EXPORTING
*            entry                       = entry
*            SOURCE_LANGU                = <t>-lang
*            CHECK_MASTER_LANGU          = abap_false
**         IN_UPDATE_TASK              =
**         FLAG_CORRECTION_ENTRY       =
*          EXCEPTIONS
*            CONCEPT_NOT_FOUND           = 1
*            ENTRY_NOT_FOUND             = 2
*            LANGU_MISSING               = 3
*            NO_MASTER_LANGU             = 4
*            ERROR_IN_CORRECTION         = 5
*            USER_CANCELLED              = 6
*            OTHERS                      = 7.
*        IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
*      ELSE.
      CLEAR entry.
      entry-langu = <t>-lang.
      entry-text = <t>-text.
      CALL FUNCTION 'SOTR_INSERT_CONCEPT_ENTRY'
        EXPORTING
          concept                             = sotr-short
*            SOURCE_LANGU                        = <t>-lang
          check_master_langu                  = abap_false
          check_master_system                 = abap_false
*           CHECK_CONCEPT_EXIST                 = 'X'
*           IN_UPDATE_TASK                      =
*           UPDATE_CONTEXT_FLAG                 = 'X'
*           FLAG_CORRECTION_ENTRY               =
        CHANGING
          entry                               = entry
*         EXCEPTIONS
*           CONCEPT_NOT_FOUND                   = 1
*           IDENTICAL_CONTEXT_NOT_ALLOWED       = 2
*           TEXT_TOO_LONG                       = 3
*           LANGU_MISSING                       = 4
*           NO_MASTER_LANGU                     = 5
*           ERROR_IN_UPDATE                     = 6
*           ERROR_IN_CORRECTION                 = 7
*           USER_CANCELLED                      = 8
*           OTHERS                              = 9
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*      ENDIF.

*      _txt = <t>-text.
*      CALL FUNCTION 'BTFR_MAINTAIN_SINGLE_TEXT'
*        EXPORTING
*          package                            = s_pack
*          object_type                        = s_type
*          langu                              = <t>-lang
*          concept                            = sotr-short
**         ALIAS                              =
**         TRANSL_TYPE                        =
**         FLAG_STRING                        =
**         OPTIONS_EDITOR                     =
**         OPTIONS_CHECK                      =
**         FLAG_NO_CONTEXT                    =
**         FLAG_DISPLAY                       =
*          flag_no_screen                     = abap_true
**         FLAG_CALLED_FROM_EDIT_SCREEN       =
*          default_text                       = _txt
*          flag_correction_entry              = abap_false
**         CORR_NUM                           =
**         USE_KORRNUM_IMMEDIATEDLY           =
**       IMPORTING
**         FLAG_CANCELLED                     =
**         HEADER                             =
**         TEXT                               =
**         STRING                             =
*        EXCEPTIONS
*          parameter_error                    = 1
*          text_not_found                     = 2
*          invalid_object_type                = 3
*          invalid_package                    = 4
*          header_diff_interface              = 5
*          invalid_tadir_entry                = 6
*          invalid_transl_type                = 7
*          invalid_concept                    = 8
*          OTHERS                             = 9.
*      IF sy-subrc <> 0.
*        mac_raise_mf 'BTFR_MAINTAIN_SINGLE_TEXT' sy-subrc.
*      ENDIF.
    ENDIF.
    IF <t>-_ IS NOT INITIAL.
      _txt = <t>-_.
*      CALL FUNCTION 'BTFR_MAINTAIN_SINGLE_TEXT'
*        EXPORTING
*          package                            = l_pack
*          object_type                        = l_type
*          langu                              = <t>-lang
*          concept                            = sotr-long
**         ALIAS                              =
**         TRANSL_TYPE                        =
**         FLAG_STRING                        =
**         OPTIONS_EDITOR                     =
**         OPTIONS_CHECK                      =
**         FLAG_NO_CONTEXT                    =
**         FLAG_DISPLAY                       =
*          flag_no_screen                     = abap_true
**         FLAG_CALLED_FROM_EDIT_SCREEN       =
*          default_text                       = _txt
*          flag_correction_entry              = abap_false
**         CORR_NUM                           =
**         USE_KORRNUM_IMMEDIATEDLY           =
**       IMPORTING
**         FLAG_CANCELLED                     =
**         HEADER                             =
**         TEXT                               =
**         STRING                             =
*        EXCEPTIONS
*          parameter_error                    = 1
*          text_not_found                     = 2
*          invalid_object_type                = 3
*          invalid_package                    = 4
*          header_diff_interface              = 5
*          invalid_tadir_entry                = 6
*          invalid_transl_type                = 7
*          invalid_concept                    = 8
*          OTHERS                             = 9.
*      IF sy-subrc <> 0.
*        mac_raise_mf 'BTFR_MAINTAIN_SINGLE_TEXT' sy-subrc.
*      ENDIF.
    ENDIF.
  ENDLOOP.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_conn   TYPE to_connector.
  DATA o_ces    TYPE to_comp_enh_spot.
  DATA o_es     TYPE to_enh_spot.
  DATA o_ei     TYPE to_enh_impl.
  DATA o_cei    TYPE to_comp_enh_impl.

  CREATE OBJECT object.
  CASE type.
    WHEN st_enh_spot.
      CREATE OBJECT o_es.
      object->raw = o_es.
    WHEN st_comp_enh_spot.
      CREATE OBJECT o_ces.
      object->raw = o_ces.
    WHEN st_enh_impl.
      CREATE OBJECT o_ei.
      object->raw = o_ei.
    WHEN st_comp_enh_impl.
      CREATE OBJECT o_cei.
      object->raw = o_cei.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA o_enh    TYPE to_enhancement_sap.
  DATA o_root   TYPE REF TO cx_root.
  DATA type     TYPE td_comptype.
  DATA name     TYPE td_compname.

  type = component->get_type( ).
  name = component->get_name( ).
  TRY.
      o_enh = load_enhancement( component = component
                                  locking = abap_true ).
      o_enh->delete( run_dark = abap_true ).
      result = abap_true.
*cx_enh_mod_not_allowed
*cx_enh_permission_denied
*cx_enh_canceled
*cx_enh_internal_error
*cx_enh_is_locked
*cx_enh_io_error
*cx_enh_composite_not_empty
    CATCH cx_root INTO o_root.
      mac_cascade_raise o_mycx o_root.
  ENDTRY.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = type
                                                name = name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA d_type             TYPE td_comptype.
  DATA d_name             TYPE td_compname.
  DATA n_enh_spot         TYPE td_enh_spot_name.
  DATA n_comp_enh_spot    TYPE td_comp_enh_spot_name.
  DATA n_enh_impl         TYPE td_enh_impl_name.
  DATA n_comp_enh_impl    TYPE td_comp_enh_spot_name.

  exists = me->exists-not_exists.
*  TRY.
  d_type = component->get_type( ).
  d_name = component->get_name( ).
  CASE d_type.
    WHEN st_enh_spot.
      n_enh_spot = d_name.
      exists = _enh_spot_exists( n_enh_spot ).
    WHEN st_comp_enh_spot.
      n_comp_enh_spot = d_name.
      exists = _comp_enh_spot_exists( n_comp_enh_spot ).
    WHEN st_enh_impl.
      n_enh_impl = d_name.
      exists = _enh_impl_exists( n_enh_impl ).
    WHEN st_comp_enh_impl.
      n_comp_enh_impl = d_name.
      exists = _comp_enh_impl_exists( n_comp_enh_impl ).
    WHEN OTHERS.
      mac_raise_type_not_supported me->class_name d_type.
  ENDCASE.
*    mac_def_catch zaplink_cx_connector.
*  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.    result = abap_true.   endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA f_subcomp  TYPE td_with_subcomp.
  DATA o_list     TYPE to_list.
  DATA o_sap_intf TYPE to_enhancement_sap.

  CREATE OBJECT object.
  object->set_component( component ).

  TRY.
    o_sap_intf = load_enhancement( component ).
    ASSERT o_sap_intf is bound.

    f_subcomp = component->get_with_subcomp( ).
    CREATE OBJECT o_list.
    component->set_subcomponents( o_list ).

    object->raw = convert_enhancement( sap_object = o_sap_intf
                                   sub_components = f_subcomp
                                  components_list = o_list ).

    TRY.
        o_list->remove( component ).
      CATCH zaplink_cx_list.
    ENDTRY.

    object->update_connector_data( me ).
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA o_sap_intf TYPE to_enhancement_sap.
  DATA o_err_list TYPE REF TO cl_wb_checklist.

  TRY.
    ASSERT object->raw IS BOUND.
    CREATE OBJECT components.
    o_sap_intf = reverse_raw( raw_object = object->raw
                                    name = object->name
*                          sub_components = f_subcomp
                         components_list = components ).
    o_sap_intf->check( EXPORTING    version = 'I' " Inactive
                        CHANGING error_list = o_err_list ).
    o_sap_intf->save( run_dark = abap_true ).
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method _COMP_ENH_IMPL_EXISTS.
  DATA d_name         TYPE td_compname.
  exists = me->exists-not_exists.
  SELECT SINGLE enhcomposite INTO d_name
    FROM enhcompheader
    WHERE enhcomposite = name.
  CHECK sy-subrc = 0.
  exists = me->exists-exists.
  endmethod.


  method _COMP_ENH_SPOT_EXISTS.
  DATA d_name         TYPE td_compname.
  exists = me->exists-not_exists.
  SELECT SINGLE enhspotcomposite INTO d_name
    FROM enhspotcomphead
    WHERE enhspotcomposite = name.
  CHECK sy-subrc = 0.
  exists = me->exists-exists.
  endmethod.


  method _ENH_IMPL_EXISTS.
  DATA d_name         TYPE td_compname.
  exists = me->exists-not_exists.
  SELECT SINGLE enhname INTO d_name
    FROM enhheader
    WHERE enhname = name.
  CHECK sy-subrc = 0.
  exists = me->exists-exists.
  endmethod.


  method _ENH_SPOT_EXISTS.
  DATA d_name         TYPE td_compname.
  exists = me->exists-not_exists.
  SELECT SINGLE enhspot INTO d_name
    FROM enhspotheader
    WHERE enhspot = name.
  CHECK sy-subrc = 0.
  exists = me->exists-exists.
  endmethod.


  method _LOAD_COMP_ENH_IMPL.
  IF _comp_enh_impl_exists( name ) = abap_true.
    object ?= cl_enh_factory=>load_enhancement_composite( name = name
                                                          lock = locking
                                                      run_dark = abap_true ).
  ENDIF.
  endmethod.


  method _LOAD_COMP_ENH_SPOT.
  DATA o_root         TYPE REF TO cx_root.
  IF _comp_enh_spot_exists( name ) = abap_true.
    TRY.
        object ?= cl_enh_factory=>load_enhancement_spot_comp( name = name
                                                              lock = locking
                                                          run_dark = abap_true ).

*     CATCH cx_enh_io_error .
*     CATCH cx_enh_internal_error .
      CATCH cx_enh_permission_denied INTO o_root.
      CATCH cx_enh_is_locked INTO o_root.
      CATCH cx_enh_canceled INTO o_root.
    ENDTRY.
    IF o_root IS BOUND.
      mac_cascade_raise o_mycx o_root.
    ENDIF.
  ENDIF.
  endmethod.


  method _LOAD_ENH_IMPL.
  IF _enh_impl_exists( name ) = abap_true.
    object ?= cl_enh_factory=>get_enhancement( enhancement_id = name
                                                         lock = locking
                                                     run_dark = abap_true ).
  ENDIF.
  endmethod.


  method _LOAD_ENH_SPOT.
  IF _enh_spot_exists( name ) = abap_true.
    object ?= cl_enh_factory=>get_enhancement_spot( spot_name = name
                                                         lock = locking
                                                     run_dark = abap_true ).
  ENDIF.
  endmethod.
ENDCLASS.
