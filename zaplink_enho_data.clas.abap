class ZAPLINK_ENHO_DATA definition
  public
  inheriting from ZAPLINK_ENHANCEMENT_DATA
  create protected

  global friends ZAPLINK_EASYXML
                 ZAPLINK_ENHANCEMENT .

public section.

  types:
    BEGIN OF ts_meth_src,
        name      TYPE seocpdname,
        redefine  TYPE td_checkbox,
        source    TYPE ts_sourceasstruc,
      END OF ts_meth_src .
  types:
    tt_meth_src TYPE SORTED TABLE OF ts_meth_src WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_event.
      include type ENHCLASSEVENT as hdr.
  types:
        parameters TYPE ENHEVENTTABPARAM,
      END OF ts_event .
  types:
    tt_events TYPE SORTED TABLE OF ts_event WITH UNIQUE KEY CMPNAME .
  types:
    BEGIN OF ts_clif_impl,
        name                    TYPE seoclsname,
        attributes              TYPE enhclasstabattrib,
        methodes                TYPE enhmethtab,
        pre_methodes            TYPE enhmeth_tabkeys,
        post_methodes	          TYPE enhmeth_tabkeys,
        save_enhincinx          TYPE enhincinx_it,
        new_methodes            TYPE enhnewmeth_tab,
        sources	                TYPE tt_meth_src,
        methodes_includes	      TYPE enhnewmeth_tabincl,
        events                  TYPE tt_events,     " enhevent_tab,
        implementings	          TYPE enhclasstabimplementing,
        overwrite_methodes      TYPE enhmeth_tabkeys,
        types	                  TYPE enhtype_tab,
        implementation_source   TYPE ts_sourceasstruc,
        access_pri_pro          TYPE td_checkbox,
      END OF ts_clif_impl .
  types:
    BEGIN OF ts_header_add,
          type        TYPE enhheader-type,
          tool_type   TYPE enhheader-enhtooltype,
*      STATE       type
          upgrade     TYPE enhheader-upgrade,
        END OF ts_header_add .
  types:
    BEGIN OF ts_fm_hdr.
    INCLUDE TYPE enh_dy_spot AS scr.
    TYPES:
        hdr       TYPE enhheader,
        text_ids  TYPE ts_sotr,
      END OF ts_fm_hdr .
  types:
    BEGIN OF ts_header.
    INCLUDE TYPE enh_dy_spot AS scr.
    INCLUDE TYPE ts_header_add AS add.
    TYPES:
        texts TYPE tt_ltexts,
      END OF ts_header .
  types:
    BEGIN OF ts_comp_enh_spot,
                name TYPE enhspotcompositename,
                zl_object TYPE to_raw,
              END OF ts_comp_enh_spot .
  types TO_COMP_ENH type ref to CL_ENH_SPOT_COMPOSITE .
  types:
    BEGIN OF ts_hook,   " both data from CL_ENH_TOOL_HOOK_IMPL
              admin  TYPE enh_hook_admin,
              enh    TYPE enhincinx_it,
            END OF ts_hook .
  types:
    BEGIN OF ts_hook_impl.
    INCLUDE TYPE enh_hook_admin AS main.
    TYPES:
        texts TYPE tt_ltexts,
        enhancement TYPE enhincinx_it,
      END OF ts_hook_impl .
  types:
    tt_hook_impls TYPE SORTED TABLE OF ts_hook_impl WITH UNIQUE KEY org_obj_type org_obj_name .
  types:
    BEGIN OF ts_badi,   " both data from CL_ENH_TOOL_BADI_IMPL
              admin  TYPE enh_badi_main,
              enh    TYPE enh_badi_impl_it,
            END OF ts_badi .
  types:
    BEGIN OF ts_badi_impl.
    INCLUDE TYPE enh_badi_impl_data AS main.
    TYPES:
        texts TYPE tt_ltexts,
      END OF ts_badi_impl .
  types:
    tt_badi_impls TYPE SORTED TABLE OF ts_badi_impl WITH UNIQUE KEY badi_name impl_name .
  types:
    BEGIN OF ts_fm_data,
      header      TYPE ts_fm_hdr,
      badi_impl   TYPE ts_badi,
      fugr_impl   TYPE enhfugrdata,
      clif_impl   TYPE enhclassmethdata,
      wdyn_impl   TYPE enhwdyn,
      hook_impl   TYPE ts_hook,
    END OF ts_fm_data .

  data A0_MAINDATA type TS_HEADER read-only .
  data BADI_IMPLEMENTATIONS type TT_BADI_IMPLS read-only .
  data HOOK_IMPLEMENTATIONS type TT_HOOK_IMPLS read-only .
  data FUNCTION_GROUP_IMPLEMENTATION type ENHFUGRDATA read-only .
  data CLASS_IMPLEMENTATION type TS_CLIF_IMPL read-only .
  data INTERFACE_IMPLEMENTATION type TS_CLIF_IMPL read-only .
  data WEB_DYNPRO_IMPLEMENTATION type ENHWDYN read-only .

  methods FROM_DATA
    importing
      !DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .

  methods ZAPLINK_CNX_EXT_CLEANER_4DATA~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER_4DATA~UNANONYMIZE
    redefinition .
protected section.

  methods CLIF_SAP2INT
    importing
      !SAP_DATA type ENHCLASSMETHDATA
    returning
      value(INT_DATA) type TS_CLIF_IMPL .
private section.
ENDCLASS.



CLASS ZAPLINK_ENHO_DATA IMPLEMENTATION.


  method CLIF_SAP2INT.
  DATA s_line   LIKE LINE OF int_data-sources.
  DATA s_attr   LIKE LINE OF int_data-attributes.
  DATA s_pre_m  LIKE LINE OF int_data-pre_methodes.
  DATA s_pos_m  LIKE LINE OF int_data-post_methodes.
  DATA s_save   LIKE LINE OF int_data-save_enhincinx.
  DATA s_n_met  LIKE LINE OF int_data-new_methodes.
  DATA s_m_inc  LIKE LINE OF int_data-methodes_includes.
  DATA s_event  LIKE LINE OF int_data-events.
*  DATA s_pmeth  LIKE LINE OF int_data-implementings.
  DATA s_impl   LIKE LINE OF int_data-implementings.
  DATA s_ow_me  LIKE LINE OF int_data-overwrite_methodes.
  DATA s_types  LIKE LINE OF int_data-types.
  FIELD-SYMBOLS:
    <e> LIKE LINE OF sap_data-enh_events,
    <m> LIKE LINE OF int_data-methodes,
    <i> LIKE LINE OF sap_data-enh_methsources.
  DATA s_param  LIKE LINE OF <m>-enhparams.
  DATA s_ev_pa  LIKE LINE OF s_event-parameters.

  int_data-name = sap_data-clsname.

  int_data-attributes = sap_data-enh_attributes.
  MODIFY int_data-attributes FROM s_attr TRANSPORTING clsname WHERE clsname = int_data-name.

  int_data-methodes = sap_data-enh_methodes.
  LOOP AT int_data-methodes ASSIGNING <m>.
    MODIFY <m>-enhparams FROM s_param TRANSPORTING clsname WHERE clsname = int_data-name.
  ENDLOOP.
  int_data-pre_methodes = sap_data-enh_premethodes.
  MODIFY int_data-pre_methodes FROM s_pre_m TRANSPORTING methkey-clsname WHERE methkey-clsname = int_data-name.

  int_data-post_methodes = sap_data-enh_postmethodes.
  MODIFY int_data-post_methodes FROM s_pos_m TRANSPORTING methkey-clsname WHERE methkey-clsname = int_data-name.

  int_data-save_enhincinx = sap_data-save_enhincinx.
  MODIFY int_data-save_enhincinx FROM s_save TRANSPORTING enhname WHERE enhname = a0_maindata-enhspot.

  int_data-new_methodes = sap_data-enh_newmethodes.
  MODIFY int_data-new_methodes FROM s_n_met TRANSPORTING methkey-clsname WHERE methkey-clsname = int_data-name.

  LOOP AT sap_data-enh_methsources ASSIGNING <i>.
    CLEAR s_line.
    s_line-name = <i>-cpdname.
    s_line-redefine = <i>-redefine.
    s_line-source-_ = zaplink_enhancement=>conv_abap_raw2source( <i>-source ).
    INSERT s_line INTO TABLE int_data-sources.
  ENDLOOP.

  int_data-methodes_includes = sap_data-enh_methincludes.
*  MODIFY int_data-methodes_includes FROM s_m_inc TRANSPORTING clsname
*         WHERE clsname = int_data-name.

  LOOP AT sap_data-enh_events ASSIGNING <e>.
    CLEAR s_event.
    s_event-hdr = <e>-event_header.
    CLEAR s_event-clsname.
    s_event-parameters = <e>-event_param.
    MODIFY s_event-parameters FROM s_ev_pa TRANSPORTING clsname cmpname
                              WHERE clsname = int_data-name AND cmpname = s_event-cmpname.
    INSERT s_event INTO TABLE int_data-events.
  ENDLOOP.

  int_data-implementings = sap_data-enh_implementings.
  MODIFY int_data-implementings FROM s_impl TRANSPORTING clsname WHERE clsname = int_data-name.

  int_data-overwrite_methodes = sap_data-enh_owrmethodes.
  MODIFY int_data-overwrite_methodes FROM s_ow_me TRANSPORTING methkey-clsname WHERE methkey-clsname = int_data-name.

  int_data-types = sap_data-enh_types.
  MODIFY int_data-types FROM s_types TRANSPORTING clsname WHERE clsname = int_data-name.

  int_data-implementation_source-_ = zaplink_enhancement=>conv_abap_raw2source( sap_data-enh_eimpsource ).
  int_data-access_pri_pro = sap_data-enh_access_pri_pro.
  endmethod.


  method FROM_DATA.
  DATA s_badi_impl    LIKE LINE OF badi_implementations.
  DATA s_txt          TYPE ts_sotr.
*  DATA s_clif_impl    LIKE LINE OF class_implementation.
  DATA s_hook_impl    LIKE LINE OF hook_implementations.
  FIELD-SYMBOLS:
    <table>       LIKE class_implementation,
*    <clif_impl>   LIKE LINE OF data-clif_impl,
    <hook_impl>   LIKE LINE OF data-hook_impl-enh,
    <badi_impl>   LIKE LINE OF data-badi_impl-admin-impl_data.

  a0_maindata-scr = data-header-scr.
  MOVE-CORRESPONDING data-header-hdr TO a0_maindata-scr.
  CLEAR a0_maindata-scr-state.
  a0_maindata-enhspot = data-header-hdr-enhname.
  a0_maindata-texts = sotr_2_texts( data-header-text_ids ).
  a0_maindata-add-type = data-header-hdr-type.
  a0_maindata-add-tool_type = data-header-hdr-enhtooltype.
  a0_maindata-add-upgrade = data-header-hdr-upgrade.

* List of type from table ENHTOOLS.
  CASE a0_maindata-add-tool_type.
    WHEN zaplink_enhancement=>tooltypes-badi.
      LOOP AT data-badi_impl-admin-impl_data ASSIGNING <badi_impl>.
        CLEAR s_badi_impl.
        s_badi_impl-main = <badi_impl>.
        s_txt-short = s_badi_impl-impl_shorttext_id.
        s_txt-long = s_badi_impl-impl_longtext_id.
        s_badi_impl-texts = sotr_2_texts( s_txt ).
        CLEAR: s_badi_impl-spot_name, s_badi_impl-impl_name,
               s_badi_impl-impl_shorttext_id, s_badi_impl-impl_shorttext, s_badi_impl-impl_longtext_id.
        INSERT s_badi_impl INTO TABLE badi_implementations.
      ENDLOOP.
    WHEN zaplink_enhancement=>tooltypes-class OR zaplink_enhancement=>tooltypes-interface.
      IF a0_maindata-add-tool_type = zaplink_enhancement=>tooltypes-class.
        ASSIGN class_implementation TO <table>.
        class_implementation = clif_sap2int( data-clif_impl ).
*        clear class_implementation-ENH_ATTRIBUTES-CLSNAME. ="CL_IWP_SWITCH_CHECK"
*        clear class_implementation-ENH_OWRMETHODES-CLSNAME. ="CL_IWP_SWITCH_CHECK"
      ELSEIF a0_maindata-add-tool_type = zaplink_enhancement=>tooltypes-interface.
        ASSIGN interface_implementation TO <table>.
        interface_implementation = clif_sap2int( data-clif_impl ).
      ELSE.
        ASSERT 1 = 0.
      ENDIF.
*      LOOP AT data-clif_impl ASSIGNING <clif_impl>.
*        CLEAR s_impl.
*        s_clif_impl-main = <clif_impl>.
*        s_txt-short = s_clif_impl-impl_shorttext_id.
*        s_txt-long = s_clif_impl-impl_longtext_id.
*        s_clif_impl-texts = sotr_2_texts( s_txt ).
**        CLEAR: s_impl-spot_name, s_impl-impl_shorttext_id, s_impl-impl_shorttext, s_impl-impl_longtext_id.
*        INSERT s_clif_impl INTO TABLE <table>.
*      ENDLOOP.
    WHEN zaplink_enhancement=>tooltypes-function_group.
      function_group_implementation = data-fugr_impl.
    WHEN zaplink_enhancement=>tooltypes-hook.
*      LOOP AT data-hook_impl-enh ASSIGNING <hook_impl>.
*        CLEAR s_hook_impl.
*        s_hook_impl-main = <hook_impl>.
*        s_txt-short = s_hook_impl-impl_shorttext_id.
*        s_txt-long = s_hook_impl-impl_longtext_id.
*        s_hook_impl-texts = sotr_2_texts( s_txt ).
**        CLEAR: s_badi_impl-spot_name, s_badi_impl-impl_shorttext_id, s_badi_impl-impl_shorttext, s_badi_impl-impl_longtext_id.
*        INSERT s_hook_impl INTO TABLE hook_implementations.
*      ENDLOOP.
*      hook_implementations = data-hook_impl.
      IF data-hook_impl-admin-org_obj_name <> data-hook_impl-admin-org_main_name.
        BREAK-POINT.
      ENDIF.
      CLEAR s_hook_impl.
      s_hook_impl-main = data-hook_impl-admin.
      IF s_hook_impl-main-org_obj_name = s_hook_impl-main-org_main_name AND
         s_hook_impl-main-org_obj_type = s_hook_impl-main-org_main_type.
        CLEAR: s_hook_impl-main-org_main_name, s_hook_impl-main-org_main_type.
      ENDIF.
      CLEAR s_hook_impl-main-programname.
*      CLEAR s_hook_impl-main-HOOK_IMPLS-ID.    Convert source to string
      s_hook_impl-enhancement = data-hook_impl-enh.
*        s_txt-short = s_hook_impl-impl_shorttext_id.
*        s_txt-long = s_hook_impl-impl_longtext_id.
*        s_hook_impl-texts = sotr_2_texts( s_txt ).
**        CLEAR: s_badi_impl-spot_name, s_badi_impl-impl_shorttext_id, s_badi_impl-impl_shorttext, s_badi_impl-impl_longtext_id.
      INSERT s_hook_impl INTO TABLE hook_implementations.
    WHEN zaplink_enhancement=>tooltypes-web_dynpro.
      web_dynpro_implementation = data-wdyn_impl.
  ENDCASE.
  CLEAR a0_maindata-enhspot.
  endmethod.


  method TO_DATA.
*  fm_data-header = a0_maindata-main.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER_4DATA~ANONYMIZE.
  CLEAR a0_maindata-createdon.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER_4DATA~UNANONYMIZE.
  endmethod.
ENDCLASS.
