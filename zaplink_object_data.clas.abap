class ZAPLINK_OBJECT_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create protected

  global friends ZAPLINK_EASYXML
                 ZAPLINK_OBJECT .

public section.
  type-pools ABAP .
  type-pools SEOF .
  type-pools SEOK .
  type-pools SEOO .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .

  interfaces ZAPLINK_DATATYPES .

  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .
  aliases TS_SOURCEASSTRUC
    for ZAPLINK_DATATYPES~TS_SOURCEASSTRUC .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_DYNPROS
    for ZAPLINK_DATATYPES~TT_DYNPROS .
  aliases TT_LTEXTS
    for ZAPLINK_DATATYPES~TT_LTEXTS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
  aliases TT_TXTP_TEXTS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTS .

  types TO_MENUPAINTER type ref to ZAPLINK_MENUPAINTER_DATA .
  types:
    BEGIN OF ts_deferred,                   " used to handle
                    classes TYPE seot_clsdeferrds_w,      " class * definition load
                    interfaces TYPE seot_intdeferrds_r,   " interface * load
                  END OF ts_deferred .
  types:
    tt_seoclasstx TYPE STANDARD TABLE OF seoclasstx WITH DEFAULT KEY .
  types:
    BEGIN OF ts_seoclasstx.
    INCLUDE TYPE seoclasstx AS main.          " M#thodes red#finies par relation d'h#ritage
    TYPES:
        documentation  TYPE ts_lang,
      END OF ts_seoclasstx .
  types:
    tt_seoclasstx_s TYPE SORTED TABLE OF ts_seoclasstx WITH UNIQUE KEY langu .
  types:
    tt_seocompotx TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY .
  types:
    tt_seocompotx_s TYPE SORTED TABLE OF seocompotx WITH UNIQUE KEY cmpname langu .
  types:
    BEGIN OF ts_seocompotx.
    INCLUDE TYPE seocompotx AS main.
    TYPES:
        documentation  TYPE ts_lang,
      END OF ts_seocompotx .
  types:
    tt_seocompotx_l TYPE SORTED TABLE OF ts_seocompotx WITH UNIQUE KEY langu .
  types:
    tt_seosubcotx TYPE STANDARD TABLE OF seosubcotx WITH DEFAULT KEY .
  types:
    tt_seosubcotx_s TYPE SORTED TABLE OF seosubcotx WITH UNIQUE KEY cmpname sconame langu .
  types:
    tt_seosubcotx_l TYPE SORTED TABLE OF seosubcotx WITH UNIQUE KEY langu .
  types:
    BEGIN OF ts_meth_inc.                                 " Issue 82
    INCLUDE TYPE seop_method_w_include AS hdr.
    TYPES:
        raw_source TYPE tt_abaprawsource,
      END OF ts_meth_inc .
  types:
    tt_meth_incs TYPE STANDARD TABLE OF ts_meth_inc WITH DEFAULT KEY .
  types:
    BEGIN OF ts_include,                                  " Issue 82
            name       TYPE program,
            raw_source TYPE tt_abaprawsource,
          END OF ts_include .
  types:
    BEGIN OF ts_includes,
            classpool      TYPE ts_include,
            pubsec         TYPE ts_include,
            prisec         TYPE ts_include,
            prosec         TYPE ts_include,
            cl             TYPE ts_include,
            ccimp          TYPE ts_include,
            ccdef          TYPE ts_include,
            ccmac          TYPE ts_include,
            interfacepool  TYPE ts_include,
            intfsec        TYPE ts_include,
            localtestclass TYPE ts_include,                 " Issue 82
            methods        TYPE tt_meth_incs,
          END OF ts_includes .
  types:
    tt_sotr_texts TYPE SORTED TABLE OF sotr_text WITH UNIQUE KEY langu object .
  types:
    tt_sotr_textus TYPE STANDARD TABLE OF sotr_textu WITH DEFAULT KEY .
  types:
    BEGIN OF ts_cx_text,
              cmpname   TYPE seoo_attribute_r-cmpname.
    INCLUDE TYPE sotr_head AS hdr.
    TYPES:
        lhdr      TYPE sotr_headu,
        texts     TYPE tt_sotr_texts,
        longtexts TYPE tt_sotr_textus,
      END OF ts_cx_text .
  types:
    tt_cx_texts TYPE SORTED TABLE OF ts_cx_text WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF t_fm_data,
*      name TYPE seoclsname,
*      version TYPE seoversion,
              class TYPE  vseoclass,
              interface TYPE  vseointerf,
              attributes  TYPE  seoo_attributes_r,
              methods TYPE  seoo_methods_r,
              events  TYPE  seoo_events_r,
              types TYPE  seoo_types_r,
              parameters  TYPE  seos_parameters_r,
              exceps  TYPE  seos_exceptions_r,
              implementings TYPE  seor_implementings_r,
              inheritance TYPE  vseoextend,
              redefinitions TYPE  seor_redefinitions_r,
              impl_details  TYPE  seor_redefinitions_r,
              friendships TYPE  seof_friendships_r,
              typeusages TYPE  seot_typepusages_r,
              clsdeferrds TYPE  seot_clsdeferrds_r,
              intdeferrds TYPE  seot_intdeferrds_r,
              comprisings TYPE  seor_comprisings_r,
              explore_inheritance TYPE  seok_cls_typeinfos,
              explore_implementings TYPE  seok_int_typeinfos,
              explore_comprisings TYPE  seok_int_typeinfos,
              aliases TYPE  seoo_aliases_r,
              enhancement_methods TYPE  enhmeth_tabheader,
              enhancement_attributes  TYPE  enhclasstabattrib,
              enhancement_events  TYPE  enhclasstabevent,
              enhancement_implementings TYPE  enhclasstabimplementing,
              enhancement_comprisings TYPE  enhclasstabintfcompri,
              class_desc  TYPE tt_seoclasstx,
              component_desc  TYPE  tt_seocompotx,
              subcomponent_desc TYPE  tt_seosubcotx,
              includes TYPE ts_includes,
* FOR SEO_CLASS_RESOLVE_INHERITANCE (Issue 128)
              enhancement_premeth TYPE  enhmeth_tabkeys,
              enhancement_postmeth TYPE  enhmeth_tabkeys,
              enhancement_owrmeth TYPE  enhmeth_tabkeys,
* FOR SEO_CLASS_CREATE_COMPLETE only
              method_sources TYPE seo_method_source_table,
              textspool   TYPE tt_txtp_textpools,
              locals_def  TYPE  rswsourcet,
              locals_imp  TYPE  rswsourcet,
              locals_mac  TYPE  rswsourcet,
              locals_tst  TYPE  rswsourcet,   " Issue 82 : Test Class
* FOR Exception class
              exceptions_texts TYPE tt_cx_texts,
* FOR Class dynpros
              dynpros     TYPE tt_dynpros,
* FOR Class MENU PAINTER
              menupainter TYPE to_menupainter,
* FOR documentation
              documentation TYPE tt_docs,
            END OF t_fm_data .
  types:
    BEGIN OF ts_param_data.
    INCLUDE TYPE seos_parameter_r AS hdr.
    TYPES:
        texts   TYPE tt_ltexts,     " tt_seosubcotx_l,
      END OF ts_param_data .
  types:
    tt_parameters TYPE SORTED TABLE OF ts_param_data WITH UNIQUE KEY sconame .
  types:
    BEGIN OF ts_excep_data.
    INCLUDE TYPE seos_exception_r AS hdr.
    TYPES:
        texts   TYPE tt_ltexts,   " tt_seosubcotx_l,
      END OF ts_excep_data .
  types:
    tt_exceptions TYPE SORTED TABLE OF ts_excep_data WITH UNIQUE KEY sconame .
  types:
    BEGIN OF ts_event_data.
    INCLUDE TYPE seoo_event_r AS hdr.
    TYPES:
        texts         TYPE tt_ltexts,     " tt_seocompotx_l,
*      redefinition  TYPE seoredef,
        parameters    TYPE tt_parameters,
*        exceptions    TYPE tt_exceptions,
*      _               TYPE ts_source,
      END OF ts_event_data .
  types:
    tt_events TYPE SORTED TABLE OF ts_event_data WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF ts_meth_data.
    INCLUDE TYPE seoo_method_r AS hdr.
    TYPES:
        texts         TYPE tt_ltexts,     " tt_seocompotx_l,
*      redefinition  TYPE ts_redefinition,
        parameters    TYPE tt_parameters,
        exceptions    TYPE tt_exceptions,
        source        TYPE ts_sourceasstruc,
      END OF ts_meth_data .
  types:
    tt_methods TYPE SORTED TABLE OF ts_meth_data WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF ts_alias_data.
    INCLUDE TYPE seoo_alias_r AS hdr.
    TYPES:
        texts   TYPE tt_ltexts,     " tt_seocompotx_l,
      END OF ts_alias_data .
  types:
    tt_alias TYPE SORTED TABLE OF ts_alias_data WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF ts_attr_data.
    INCLUDE TYPE seoo_attribute_r AS hdr.
    TYPES:
        texts   TYPE tt_ltexts,     " tt_seocompotx_l,
        _       TYPE td_abapsource,
      END OF ts_attr_data .
  types:
    tt_attributs TYPE SORTED TABLE OF ts_attr_data WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF ts_type_data.
    INCLUDE TYPE seoo_type_r AS hdr.
    TYPES:
        texts   TYPE tt_ltexts,     " tt_seocompotx_l,
        _       TYPE td_abapsource,
      END OF ts_type_data .
  types:
    tt_types TYPE SORTED TABLE OF ts_type_data WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF ts_tgrp_data.
    INCLUDE TYPE seot_typepusage_r AS hdr.
    TYPES:
        defered TYPE seot_clsdeferrd_r,
        itf_def TYPE seot_intdeferrd_r,
      END OF ts_tgrp_data .
  types:
    tt_typegroups TYPE SORTED TABLE OF ts_tgrp_data WITH UNIQUE KEY typegroup .
  types:
    BEGIN OF ts_redef_data.
    INCLUDE TYPE seoredef AS hdr.
    TYPES:
        source        TYPE ts_sourceasstruc,
      END OF ts_redef_data .
  types:
    tt_comp_impl TYPE SORTED TABLE OF ts_redef_data WITH UNIQUE KEY mtdname .
  types:
    BEGIN OF ts_interface.
    INCLUDE TYPE seor_implementing_r AS hdr.  " M#thodes red#finies par relation d'h#ritage
    TYPES:
        components TYPE tt_comp_impl,   " Interface public attribute default value & Method implementation
*      attributes_redefinition TYPE tt_itf_default_values,   " Interface public attribute default value
      END OF ts_interface .
  types:
    tt_interfaces TYPE SORTED TABLE OF ts_interface WITH UNIQUE KEY refclsname .
  types:
    BEGIN OF ts_sources,
                        classpool   TYPE  ts_sourceasstruc,
                        local_types TYPE  ts_sourceasstruc,
                        local_imp   TYPE  ts_sourceasstruc,
                        macros      TYPE  ts_sourceasstruc,
                        public      TYPE  ts_sourceasstruc,
                        private     TYPE  ts_sourceasstruc,
                        protected   TYPE  ts_sourceasstruc,
                      END OF ts_sources .

  data ALIASES type TT_ALIAS .
  data ATTRIBUTS type TT_ATTRIBUTS .
  data DOCUMENTATION type TT_DOCS .
  data EVENTS type TT_EVENTS .
  data INTERFACES type TT_INTERFACES .
  data METHODS type TT_METHODS .
  data TYPEGROUPS type TT_TYPEGROUPS .
  data TYPES type TT_TYPES .
  data DEFERRED type TS_DEFERRED .

  class-methods CLASS_CONSTRUCTOR .
  methods FROM_DATA
    importing
      !FM_DATA type T_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type T_FM_DATA .
  methods COMPLETE_DATA
    changing
      !FM_DATA type T_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.

  aliases TD_COMPTYPE
    for ZAPLINK_DATATYPES~TD_COMPTYPE .
  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TD_TRANSPORT_KIND
    for ZAPLINK_DATATYPES~TD_TRANSPORT_KIND .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .

  types:
    BEGIN OF ts_doc_key,                                    " equiv DOKU_OBJ = CHAR 60
      name       TYPE seoclsname,                           " CHAR 30
      other(30)  TYPE c,
    END OF ts_doc_key .
  types:
    BEGIN OF ts_component,
      cmpname TYPE seoo_method_r-cmpname,
    END OF ts_component .
  types:
    tt_components TYPE SORTED TABLE OF ts_component WITH UNIQUE KEY cmpname .
  types TO_EXCEPTION type ref to ZAPLINK_CX_CONNECTOR .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .

  data COMPONENTS type TT_COMPONENTS .
  data OBJ_NAME type SEOCLSNAME .
  data OBJ_VERSION type SEOVERSION .
  data OBJ_LANGU type SYLANGU .
  constants:
    BEGIN OF Object_types,                                  "#EC NOTEXT
      class        TYPE td_comptype VALUE 'CLAS',
      interface    TYPE td_comptype VALUE 'INTF',
    END OF Object_types .
  constants LINE_SEPARATOR type ABAP_CHAR1 value CL_ABAP_CHAR_UTILITIES=>NEWLINE ##NO_TEXT.
  constants METHOD_SEPARATOR type ABAP_CHAR1 value '~' ##NO_TEXT.
  class-data:
    begin of R_DOC_IDS,
      class       type TR_DOCID,
      interface   type TR_DOCID,
    end of R_DOC_IDS .
  constants:
    BEGIN OF doc_ids,
      begin of class,
*table TDCLT
*DOKCLASS DOKTITEL
*CA       Class attribute
*CE       Class event
*CL       Class
*CO       Class method
*CT       Class type
        header    TYPE td_doc_id VALUE 'CL',
        attribute TYPE td_doc_id VALUE 'CA',
        method    TYPE td_doc_id VALUE 'CO',
        event     TYPE td_doc_id VALUE 'CE',
        type      TYPE td_doc_id VALUE 'CT',
      end of class,
      begin of interface,
        header    TYPE td_doc_id VALUE 'IF',
        attribute TYPE td_doc_id VALUE 'IA',
        method    TYPE td_doc_id VALUE 'IO',
        event     TYPE td_doc_id VALUE 'IE',
        type      TYPE td_doc_id VALUE 'IT',   " does not exists yet
      end of interface,
    END OF doc_ids .
  data OBJ_TYPE type TD_COMPTYPE .

  methods FD_PARAMETERS
    importing
      !FM_DATA type T_FM_DATA
      !COMPONENT type SEOCMPNAME
    returning
      value(PARAMETERS) type TT_PARAMETERS .
  methods FD_EXCEPTIONS
    importing
      !FM_DATA type T_FM_DATA
      !COMPONENT type SEOCMPNAME
    returning
      value(EXCEPTIONS) type TT_EXCEPTIONS .
  methods FD_SUBC_DESC
    importing
      !DESCRIPTIONS type TT_SEOSUBCOTX
      !SUB_COMPONENT type SEOSCONAME
      !COMPONENT type SEOCMPNAME
    returning
      value(TEXTS) type TT_LTEXTS .
  methods FD_COMP_DESC
    importing
      !DESCRIPTIONS type TT_SEOCOMPOTX
      !COMPONENT type SEOCMPNAME
      !ID type DOKU_ID
      !DOCUMENTATION type TT_DOCS
    returning
      value(TEXTS) type TT_LTEXTS .
  methods TD_SUBC_DESC
    importing
      !SUB_COMPONENT type SEOSCONAME
      !COMPONENT type SEOCMPNAME
      !TEXTS type TT_LTEXTS
    changing
      !DESCRIPTIONS type TT_SEOSUBCOTX .
  methods TD_COMP_DESC
    importing
      !COMPONENT type SEOCMPNAME
      !TEXTS type TT_LTEXTS
      !ID type DOKU_ID
    changing
      !DOCUMENTATION type TT_DOCS
      !DESCRIPTIONS type TT_SEOCOMPOTX .
  methods TD_PARAMETERS
    importing
      !COMPONENT type SEOCMPNAME
      !PARAMETERS type TT_PARAMETERS
    changing
      !FM_DATA type T_FM_DATA .
  methods TD_EXCEPTIONS
    importing
      !COMPONENT type SEOCMPNAME
      !EXCEPTION type TT_EXCEPTIONS
    changing
      !FM_DATA type T_FM_DATA .
  methods TD_TEXT
    importing
      !CLASSNAME type SEOCLSNAME
      !TEXTS type TT_LTEXTS
      !ID type DOKU_ID
    changing
      !DOCUMENTATION type TT_DOCS
      !DESCRIPTIONS type TT_SEOCLASSTX .
  methods FD_TEXT
    importing
      !DESCRIPTIONS type TT_SEOCLASSTX
      !CLASSNAME type SEOCLSNAME
      !ID type DOKU_ID
      !DOCUMENTATION type TT_DOCS
    returning
      value(TEXTS) type TT_LTEXTS .
  methods CLEAR_TAGS .
  methods FIX_INCLUDED_TYPE_POOL
    importing
      !SOURCECODE type TT_ABAPRAWSOURCE
      !TYPEPOOLS type SEOT_TYPEPUSAGES_R
      !PUBLIC type ABAP_BOOL optional
    exporting
      !NEW_SOURCECODE type TT_ABAPRAWSOURCE
      !NEW_TYPEPOOLS type SEOT_TYPEPUSAGES_R .
private section.

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_OBJECT_DATA IMPLEMENTATION.


  method ANONYMIZE.
  DATA s_al  LIKE LINE OF aliases.
  DATA s_at LIKE LINE OF attributs.
*  DATA s_ev LIKE LINE OF events.
  FIELD-SYMBOLS <ev>    LIKE LINE OF events.
*  DATA s_in LIKE LINE OF interfaces.
  FIELD-SYMBOLS <in>    LIKE LINE OF interfaces.
  DATA s_co  LIKE LINE OF <in>-components.
  DATA s_ty  LIKE LINE OF types.
  DATA s_tg  LIKE LINE OF typegroups.
*  DATA s_me  LIKE LINE OF methods.
  FIELD-SYMBOLS <me>    LIKE LINE OF methods.
  DATA s_pa  LIKE LINE OF <me>-parameters.
  DATA s_ex  LIKE LINE OF <me>-exceptions.
  DATA s_dc  LIKE LINE OF deferred-classes.
  DATA s_di  LIKE LINE OF deferred-interfaces.

* ALIASES
  MODIFY aliases FROM s_al TRANSPORTING version              " Issue 75
         WHERE NOT cmpname IS INITIAL OR cmpname IS INITIAL.

* ATTRIBUTS
  MODIFY attributs FROM s_at TRANSPORTING author createdon changedby changedon r3release version editorder " Issue 75 / Issue 108
         WHERE NOT cmpname IS INITIAL OR cmpname IS INITIAL.

*DOCUMENTATION

*EVENTS
*  MODIFY events FROM s_ev TRANSPORTING author createdon changedby changedon r3release
*         WHERE NOT cmpname IS INITIAL.
  LOOP AT events ASSIGNING <ev>.
    CLEAR: <ev>-author, <ev>-createdon, <ev>-changedby, <ev>-changedon, <ev>-r3release, <ev>-version, <ev>-editorder. " Issue 75 / Issue 108
    MODIFY <ev>-parameters FROM s_pa TRANSPORTING author createdon changedby changedon  version editorder             " Issue 75 / Issue 108
           WHERE NOT cmpname IS INITIAL OR cmpname IS INITIAL.
* Issue 39
*    MODIFY <ev>-exceptions FROM s_ex TRANSPORTING author createdon changedby changedon
*           WHERE NOT cmpname IS INITIAL OR cmpname IS INITIAL.
  ENDLOOP.

*INTERFACES
*  MODIFY interfaces FROM s_in TRANSPORTING author createdon changedby changedon version " Issue 75
*         WHERE NOT refclsname IS INITIAL.
  LOOP AT interfaces ASSIGNING <in>.
    CLEAR: <in>-author, <in>-createdon, <in>-changedby, <in>-changedon, <in>-version. " Issue 75
    MODIFY <in>-components FROM s_co TRANSPORTING version   " Issue 75
           WHERE NOT clsname IS INITIAL OR clsname IS INITIAL.
  ENDLOOP.

*METHODS
*  MODIFY methods FROM s_me TRANSPORTING author createdon changedby changedon r3release
*         WHERE NOT cmpname IS INITIAL.
  LOOP AT methods ASSIGNING <me>.
    CLEAR: <me>-author, <me>-createdon, <me>-changedby, <me>-changedon, <me>-r3release, <me>-version, <me>-editorder. " Issue 75 / Issue 108
    MODIFY <me>-parameters FROM s_pa TRANSPORTING author createdon changedby changedon version editorder              " Issue 75 / Issue 108
           WHERE NOT sconame IS INITIAL OR sconame IS INITIAL. " Issue 22
    MODIFY <me>-exceptions FROM s_ex TRANSPORTING author createdon changedby changedon version editorder              " Issue 75 / Issue 108
           WHERE NOT sconame IS INITIAL OR sconame IS INITIAL. " Issue 22
  ENDLOOP.

*TYPEGROUPS
  MODIFY typegroups FROM s_tg TRANSPORTING version             " Issue 75
         WHERE NOT clsname IS INITIAL OR clsname IS INITIAL.

*TYPES
  MODIFY types FROM s_ty TRANSPORTING author createdon changedby changedon r3release version " editorder Issue 117 (preserve edit order) " Issue 75 / Issue 108
         WHERE NOT cmpname IS INITIAL OR cmpname IS INITIAL.

*DEFERRED
  MODIFY deferred-classes FROM s_dc TRANSPORTING version    " Issue 75
         WHERE NOT clsname IS INITIAL OR clsname IS INITIAL.
  MODIFY deferred-interfaces FROM s_di TRANSPORTING version " Issue 75
         WHERE NOT clsname IS INITIAL OR clsname IS INITIAL.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids-class.

  _id-sign = 'I'.
  _id-option = 'EQ'.
  _id-low = doc_ids-class-header.     APPEND _id TO r_doc_ids-class.
  _id-low = doc_ids-class-attribute.  APPEND _id TO r_doc_ids-class.
  _id-low = doc_ids-class-method.     APPEND _id TO r_doc_ids-class.
  _id-low = doc_ids-class-event.      APPEND _id TO r_doc_ids-class.
  _id-low = doc_ids-class-type.       APPEND _id TO r_doc_ids-class.

  _id-low = doc_ids-interface-header.     APPEND _id TO r_doc_ids-interface.    " Issue 33 : Wrong Doc IDs
  _id-low = doc_ids-interface-attribute.  APPEND _id TO r_doc_ids-interface.    " Issue 33 : Wrong Doc IDs
  _id-low = doc_ids-interface-method.     APPEND _id TO r_doc_ids-interface.    " Issue 33 : Wrong Doc IDs
  _id-low = doc_ids-interface-event.      APPEND _id TO r_doc_ids-interface.    " Issue 33 : Wrong Doc IDs
  _id-low = doc_ids-interface-type.       APPEND _id TO r_doc_ids-interface.    " Issue 33 : Wrong Doc IDs
  endmethod.


  method CLEAR_TAGS.
  CLEAR: obj_name, obj_version, obj_langu, obj_type.
  endmethod.


  method COMPLETE_DATA.
  DATA s_impl_detail LIKE LINE OF fm_data-impl_details.
  FIELD-SYMBOLS:
    <m> LIKE LINE OF fm_data-includes-methods.

  CHECK NOT obj_name IS INITIAL.                            " Issue 23

  SELECT * INTO TABLE fm_data-class_desc
    FROM seoclasstx
    WHERE clsname = obj_name.                               " Issue 23

  SELECT * INTO TABLE fm_data-component_desc
    FROM seocompotx
    WHERE clsname = obj_name.                               " Issue 23

  SELECT * INTO TABLE fm_data-subcomponent_desc
    FROM seosubcotx
    WHERE clsname = obj_name.                               " Issue 23

* Issue 24 : Start
  DELETE fm_data-class_desc WHERE descript IS INITIAL.
  DELETE fm_data-component_desc WHERE descript IS INITIAL.
  DELETE fm_data-subcomponent_desc WHERE descript IS INITIAL.
* Issue 24 : End

*  IF NOT fm_data-class-clsname IS INITIAL.                  " Issue 23
*    fm_data-includes = get_includes( fm_data-class-clsname ).
*  ENDIF.

* Issue 4
*impl_details doesn't contains methods implemented when class is abstract
* we check that all methods (comparing include) has it's entry.
  LOOP AT fm_data-includes-methods ASSIGNING <m>
          WHERE cpdkey-clsname = obj_name.                  " Issue 23

* Check if method is a classic method
    READ TABLE fm_data-methods TRANSPORTING NO FIELDS
         WITH KEY clsname = <m>-cpdkey-clsname
                  cmpname = <m>-cpdkey-cpdname.
    IF sy-subrc = 0.
* OK Method exists as a method.
      CONTINUE.
    ENDIF.

* Check if method is a redefinition
    READ TABLE fm_data-redefinitions TRANSPORTING NO FIELDS
         WITH KEY clsname = <m>-cpdkey-clsname
               refclsname = fm_data-inheritance-refclsname
                  mtdname = <m>-cpdkey-cpdname.
    IF sy-subrc = 0.
* OK Method exists as a method's redefinition.
      CONTINUE.
    ENDIF.

* Check if method is a interface implementation
    CLEAR s_impl_detail.
    s_impl_detail-clsname = <m>-cpdkey-clsname.
    SPLIT <m>-cpdkey-cpdname AT method_separator INTO s_impl_detail-refclsname s_impl_detail-mtdname.
    IF s_impl_detail-mtdname IS INITIAL. " no ~ in name
* Need to add new method which is not possible.
*Fatal Error : Method &1 (&2) is implemented but without any definition
      MESSAGE a000(zaplink_object) WITH <m>-cpdkey-cpdname <m>-cpdkey-clsname.
    ENDIF.

    READ TABLE fm_data-impl_details TRANSPORTING NO FIELDS
         WITH KEY clsname = s_impl_detail-clsname
               refclsname = s_impl_detail-refclsname
                  mtdname = s_impl_detail-mtdname.
    IF sy-subrc = 0.
* OK Method exists as a implemented method.
      CONTINUE.
    ENDIF.
* Method's doesn't exist as interface implementation => insert data
    s_impl_detail-version = '1'. APPEND s_impl_detail TO fm_data-impl_details.
  ENDLOOP.
  endmethod.


  method FD_COMP_DESC.
  DATA _t     LIKE LINE OF texts.
  DATA s_key  TYPE ts_doc_key.
  FIELD-SYMBOLS:
    <d>    LIKE LINE OF documentation,
    <dt>   LIKE LINE OF <d>-texts,
    <desc> LIKE LINE OF descriptions.

  LOOP AT descriptions ASSIGNING <desc>
       WHERE clsname = obj_name
         AND cmpname = component.
    _t-lang = <desc>-langu.      _t-text = <desc>-descript.
*    CLEAR: _t-clsname, _t-cmpname. " VERSION
* Issue 33 : Documentation conencted to component
    s_key-name = obj_name.
    s_key-other = component.
    READ TABLE documentation ASSIGNING <d>
         WITH KEY object = s_key           " Component name should be unique so no realy need to check ID
                      id = id.
    IF sy-subrc = 0.
      READ TABLE <d>-texts ASSIGNING <dt>
          WITH KEY tdspras = _t-lang.
      IF sy-subrc = 0.   _t-_ = <dt>-_.   ENDIF.
    ENDIF.
    INSERT _t INTO TABLE texts.
  ENDLOOP.
  endmethod.


  method FD_EXCEPTIONS.
  DATA: _ex  LIKE LINE OF exceptions.
  FIELD-SYMBOLS:
    <ex>  LIKE LINE OF fm_data-exceps.

  LOOP AT fm_data-exceps ASSIGNING <ex>
       WHERE clsname = obj_name
         AND cmpname = component.
    CLEAR: _ex.
    _ex-hdr = <ex>.
    _ex-texts = fd_subc_desc( descriptions = fm_data-subcomponent_desc
                                 component = <ex>-cmpname
                             sub_component = <ex>-sconame ).
    CLEAR: _ex-clsname, _ex-cmpname, _ex-descript. " VERSION
    INSERT _ex INTO TABLE exceptions.
  ENDLOOP.
  endmethod.


  method FD_PARAMETERS.
DATA: _pa  LIKE LINE OF parameters.
  FIELD-SYMBOLS:
    <pa>  LIKE LINE OF fm_data-parameters.

  LOOP AT fm_data-parameters ASSIGNING <pa>
       WHERE clsname = obj_name
         AND cmpname = component.
    CLEAR: _pa.
    _pa-hdr = <pa>.
    _pa-texts = fd_subc_desc( descriptions = fm_data-subcomponent_desc
                                 component = <pa>-cmpname
                             sub_component = <pa>-sconame ).
    CLEAR: _pa-clsname, _pa-cmpname, _pa-langu, _pa-descript. " VERSION
    INSERT _pa INTO TABLE parameters.
  ENDLOOP.
  endmethod.


  method FD_SUBC_DESC.
  DATA: _t   LIKE LINE OF texts.
  FIELD-SYMBOLS:
    <desc> LIKE LINE OF descriptions.

  LOOP AT descriptions ASSIGNING <desc>
       WHERE clsname = obj_name
         AND cmpname = component
         AND sconame = sub_component.
    _t-lang = <desc>-langu.      _t-text = <desc>-descript.
*    CLEAR: _t-clsname, _t-cmpname, _t-sconame. " VERSION
    INSERT _t INTO TABLE texts.
  ENDLOOP.
  endmethod.


  method FD_TEXT.
  DATA _t     LIKE LINE OF texts.
  DATA s_key  TYPE ts_doc_key.
  DATA s_txt  LIKE LINE OF texts.
  FIELD-SYMBOLS:
*    <d>    LIKE LINE OF documentation,
    <doc>  LIKE LINE OF documentation,
    <dt>   LIKE LINE OF <doc>-texts,
    <desc> LIKE LINE OF descriptions.

* Issue 33 : Documentation connected to text
  LOOP AT descriptions ASSIGNING <desc>
       WHERE clsname = classname.
    s_txt-lang = <desc>-langu.      s_txt-text = <desc>-descript.
*    CLEAR: s_txt-clsname. " VERSION
    s_key-name = obj_name.
* Class documentation
    READ TABLE documentation ASSIGNING <doc>
         WITH KEY object = s_key.           " Component name should be unique so no need to check ID
    IF sy-subrc = 0.
      READ TABLE <doc>-texts ASSIGNING <dt>
          WITH KEY tdspras = s_txt-lang.
      IF sy-subrc = 0.    s_txt-_ = <dt>-_.     ENDIF.
    ENDIF.
    INSERT s_txt INTO TABLE texts.
  ENDLOOP.
  endmethod.


  method FIX_INCLUDED_TYPE_POOL.
  DATA d_tabix TYPE sy-tabix.
  DATA t_tokens TYPE STANDARD TABLE OF stokes.
  DATA t_statments TYPE STANDARD TABLE OF sstmnt.
  DATA s_tp     LIKE LINE OF NEW_TYPEPOOLS.
  DATA d_flag TYPE abap_bool.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF new_sourcecode,
    <t> LIKE LINE OF t_tokens,
    <n> LIKE LINE OF t_tokens.

  CHECK NOT sourcecode IS INITIAL.
  new_sourcecode = sourcecode.

  SCAN ABAP-SOURCE sourcecode
                   TOKENS INTO t_tokens
                   STATEMENTS INTO t_statments.

  LOOP AT t_tokens ASSIGNING <t>
    WHERE str CP 'TYPE-POOLS'.
    d_tabix = sy-tabix + 1.
    READ TABLE t_tokens ASSIGNING <n> INDEX d_tabix.
    CHECK sy-subrc = 0.
    CHECK <n>-row = <t>-row.    " TYPE-POOLS ABAP.
    READ TABLE typepools TRANSPORTING NO FIELDS
         WITH KEY typegroup = <n>-str
                   explicit = abap_true.
    CHECK sy-subrc <> 0 OR public IS INITIAL.
    IF sy-subrc <> 0.
* Type pool doesn't exist in class definition but is declared in the middle of sections
      CLEAR s_tp.
      s_tp-typegroup = <n>-str.
      s_tp-tputype = 0.     " Type group use                (TYPE-POOLS tp)
      s_tp-explicit = abap_true.
      s_tp-implicit = abap_false.
      APPEND s_tp TO new_typepools.
    ENDIF.
* Line has to be commented
    READ TABLE new_sourcecode ASSIGNING <s> INDEX <n>-row.
    CONCATENATE '*' <s> '"Auto commented by ZAPLINK_OBJECT connector and added as Explicit type group'(cmt) INTO <s>.
    d_flag = abap_true.
  ENDLOOP.

  IF d_flag IS INITIAL. CLEAR new_sourcecode. ENDIF.
  endmethod.


  method FROM_DATA.
  DATA _at  LIKE LINE OF attributs.
  DATA _ty  LIKE LINE OF types.
  DATA _tg  LIKE LINE OF typegroups.
  DATA _me  LIKE LINE OF methods.
  DATA _al  LIKE LINE OF aliases.
  DATA _ev  LIKE LINE OF events.
  DATA _mk  TYPE seocpdkey.
  DATA _dc  LIKE LINE OF deferred-classes.
  DATA _di  LIKE LINE OF deferred-interfaces.
  DATA id   TYPE doku_id.
  FIELD-SYMBOLS:
*    <m_>  LIKE LINE OF fm_data-includes-methods,
    <at>  LIKE LINE OF fm_data-attributes,
    <al>  LIKE LINE OF fm_data-aliases,
    <me>  LIKE LINE OF fm_data-methods,
    <ev>  LIKE LINE OF fm_data-events,
    <cd>  LIKE LINE OF fm_data-clsdeferrds,
    <id>  LIKE LINE OF fm_data-intdeferrds,
    <ty>  LIKE LINE OF fm_data-types,
    <tu>  LIKE LINE OF fm_data-typeusages.

*TYPEPUSAGES  TYPE  SEOT_TYPEPUSAGES_R
  LOOP AT fm_data-typeusages ASSIGNING <tu>
          WHERE clsname = obj_name.
    CLEAR _tg.
    _tg-hdr = <tu>.

    READ TABLE fm_data-clsdeferrds ASSIGNING <cd>
         WITH TABLE KEY clsname = <tu>-clsname
                      typegroup = <tu>-typegroup
                        version = <tu>-version.
    IF sy-subrc = 0.
      _tg-defered = <cd>.
    ENDIF.

    READ TABLE fm_data-intdeferrds ASSIGNING <id>
         WITH TABLE KEY clsname = <tu>-clsname
                      typegroup = <tu>-typegroup
                        version = <tu>-version.
    IF sy-subrc = 0.
      _tg-itf_def = <id>.
    ENDIF.

    CLEAR: _tg-clsname, _tg-defered-clsname, _tg-itf_def-clsname. " VERSION
    INSERT _tg INTO TABLE typegroups.
  ENDLOOP.

* ISSUE 2 : Begin
*CLSDEFERRDS  TYPE  SEOT_CLSDEFERRDS_R
  LOOP AT fm_data-clsdeferrds ASSIGNING <cd>.
    _dc = <cd>.
    CLEAR: _dc-clsname.
    INSERT _dc INTO TABLE deferred-classes.
  ENDLOOP.

*INTDEFERRDS  TYPE  SEOT_INTDEFERRDS_R
  LOOP AT fm_data-intdeferrds ASSIGNING <id>.
    _di = <id>.
    CLEAR: _di-clsname.
    INSERT _di INTO TABLE deferred-interfaces.
  ENDLOOP.
* ISSUE 2 : End

*ATTRIBUTES TYPE  SEOO_atTTRIBUTES_R
*COMPONENT_DESC  TYPE  tt_SEOCOMPOTX,
  IF obj_type = object_types-class. id = doc_ids-class-attribute. ELSE. id = doc_ids-interface-attribute. ENDIF.
  LOOP AT fm_data-attributes ASSIGNING <at>
    WHERE clsname = obj_name.
    CLEAR _at.
    _at-hdr = <at>.
    _at-_ = _at-typesrc. CLEAR _at-typesrc.                 " Issue 68
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN _at-_ WITH cl_abap_char_utilities=>newline. " Issue 68
    _at-texts = fd_comp_desc( descriptions = fm_data-component_desc
                                        id = id
                             documentation = fm_data-documentation
                                 component = <at>-cmpname ).
    CLEAR: _at-clsname, _at-langu, _at-descript. " VERSION ?
    CLEAR: _at-srcrow1, _at-srccolumn1, _at-srcrow2, _at-srccolumn2, _at-typesrc_leng. " Issue 68
    INSERT _at INTO TABLE attributs.
  ENDLOOP.

*METHODS  TYPE  SEOO_meETHODS_R
*PARAMETERS TYPE  SEOS_paARAMETERS_R
*EXCEPS TYPE  SEOS_exXCEPTIONS_R
*SUBCOMPONENT_DESC type  tt_SEOSUBCOTX,
  IF obj_type = object_types-class. id = doc_ids-class-method. ELSE. id = doc_ids-interface-method. ENDIF.
  LOOP AT fm_data-methods ASSIGNING <me>
          WHERE clsname = obj_name.
    CLEAR _me.
    _me-hdr = <me>.

    _me-texts = fd_comp_desc( descriptions = fm_data-component_desc
                                        id = id
                             documentation = fm_data-documentation
                                 component = <me>-cmpname ).

    _me-exceptions = fd_exceptions( fm_data = fm_data
                                  component = <me>-cmpname ).

    _me-parameters = fd_parameters( fm_data = fm_data
                                  component = <me>-cmpname ).

    CLEAR: _me-clsname, _me-langu, _me-descript. " VERSION
    INSERT _me INTO TABLE methods.
  ENDLOOP.

*EVENTS TYPE  SEOO_exVENTS_R
*PARAMETERS TYPE  SEOS_paARAMETERS_R
*EXCEPS TYPE  SEOS_exXCEPTIONS_R
*SUBCOMPONENT_DESC type  tt_SEOSUBCOTX,
  IF obj_type = object_types-class. id = doc_ids-class-event. ELSE. id = doc_ids-interface-event. ENDIF.
  LOOP AT fm_data-events ASSIGNING <ev>
          WHERE clsname = obj_name.
    CLEAR _ev.
    _ev-hdr = <ev>.

    _ev-texts = fd_comp_desc( descriptions = fm_data-component_desc
                                        id = id
                             documentation = fm_data-documentation
                                 component = <ev>-cmpname ).

    _ev-parameters = fd_parameters( fm_data = fm_data
                                  component = <ev>-cmpname ).
* No exception in event
*    _ev-exceptions = fd_exceptions( fm_data = fm_data
*                                  component = <ev>-cmpname ).

    CLEAR: _ev-clsname, _ev-langu, _ev-descript. " VERSION
    INSERT _ev INTO TABLE events.
  ENDLOOP.

*TYPES  TYPE  SEOO_TYPES_R
  IF obj_type = object_types-class. id = doc_ids-class-type. ELSE. id = doc_ids-interface-type. ENDIF.
  LOOP AT fm_data-types ASSIGNING <ty>
          WHERE clsname = obj_name.
    CLEAR _ty.
    _ty-hdr = <ty>.
    _ty-_ = _ty-typesrc. CLEAR _ty-typesrc.                 " Issue 68
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN _ty-_ WITH cl_abap_char_utilities=>newline. " Issue 68

    _ty-texts = fd_comp_desc( descriptions = fm_data-component_desc
                                        id = id
                             documentation = fm_data-documentation
                                 component = <ty>-cmpname ).

    CLEAR: _ty-clsname, _ty-langu, _ty-descript. " VERSION
    CLEAR: _ty-srcrow1, _ty-srccolumn1, _ty-srcrow2, _ty-srccolumn2, _ty-typesrc_leng. " Issue 68
*    CLEAR: _ty-editorder.   " Issue 68 like => Editorder is required to ensure interface creation will lead to the same code signature
    INSERT _ty INTO TABLE types.
  ENDLOOP.

*ALIASES  TYPE  SEOO_atLIASES_R
  LOOP AT fm_data-aliases ASSIGNING <al>
          WHERE clsname = obj_name.
    CLEAR _al.    _al-hdr = <al>.   CLEAR: _al-clsname. " VERSION
    INSERT _al INTO TABLE aliases.
  ENDLOOP.

* No use detected
*EXPLORE_INHERITANCE  TYPE  SEOK_CLS_TYPEINFOS
*EXPLORE_IMPLEMENTINGS  TYPE  SEOK_INT_TYPEINFOS
*ENHANCEMENT_meETHODS  TYPE  ENHMETH_TABHEADER
*ENHANCEMENT_atTTRIBUTES TYPE  ENHCLASSTABATTRIB
*ENHANCEMENT_exVENTS TYPE  ENHCLASSTABEVENT
*ENHANCEMENT_IMPLEMENTINGS  TYPE  ENHCLASSTABIMPLEMENTING
  endmethod.


  method TD_COMP_DESC.
  DATA _t     LIKE LINE OF descriptions.
  DATA _d     LIKE LINE OF documentation.
  DATA _dt    LIKE LINE OF _d-texts.
  DATA s_key  TYPE ts_doc_key.
  FIELD-SYMBOLS:
    <t>   LIKE LINE OF texts.

* Issue 33 : Documentation conencted to component
* Prepare documentaion if any.
  _d-application = 'DOKU'.
  _d-id = id.
  s_key-name = obj_name. s_key-other = component.
  _d-object = s_key.
  _d-langu = sy-langu.
  _d-masterlang = abap_true.
  _d-typ = 'E'.
  _d-dokform = 'S_DOCU_SHOW'.
  _d-dokstyle = 'S_DOCUS1'.
  LOOP AT texts ASSIGNING <t>.
    CLEAR _t.    _t-langu = <t>-lang.   _t-descript = <t>-text.    _t-clsname = obj_name.    _t-cmpname = component.
    INSERT _t INTO TABLE descriptions.
* Issue 33 : Documentation conencted to component
    CHECK NOT <t>-_ IS INITIAL.
    _dt-_ = <t>-_.    _dt-tdspras = <t>-lang.
    INSERT _dt INTO TABLE _d-texts.
  ENDLOOP.
  IF NOT _d-texts IS INITIAL. INSERT _d INTO TABLE documentation. ENDIF.
  endmethod.


  method TD_EXCEPTIONS.
  DATA _ex  LIKE LINE OF fm_data-exceps.
  FIELD-SYMBOLS:
    <ex>  LIKE LINE OF exception,
    <t>   LIKE LINE OF <ex>-texts.

  LOOP AT exception ASSIGNING <ex>.

    CLEAR: _ex.
    _ex = <ex>-hdr.
    CALL METHOD td_subc_desc
      EXPORTING
        sub_component = <ex>-sconame
        component     = component
        texts         = <ex>-texts
      CHANGING
        descriptions  = fm_data-subcomponent_desc.

    CLEAR: _ex-clsname, _ex-cmpname. " VERSION
    _ex-clsname = obj_name.   _ex-cmpname = component.    _ex-langu = obj_langu.
    READ TABLE <ex>-texts ASSIGNING <t>
         WITH TABLE KEY lang = _ex-langu.
    IF sy-subrc = 0.    _ex-descript = <t>-text.   ENDIF.
    INSERT _ex INTO TABLE fm_data-exceps.
  ENDLOOP.
  endmethod.


  method TD_PARAMETERS.
  DATA: _pa  LIKE LINE OF fm_data-parameters.
  FIELD-SYMBOLS:
    <pa>  LIKE LINE OF parameters,
    <t>   LIKE LINE OF <pa>-texts.

  LOOP AT parameters ASSIGNING <pa>.
    CLEAR: _pa.
    _pa = <pa>-hdr.

    CALL METHOD td_subc_desc
      EXPORTING
        sub_component = <pa>-sconame
        component     = component
        texts         = <pa>-texts
      CHANGING
        descriptions  = fm_data-subcomponent_desc.

    _pa-clsname = obj_name.
    _pa-cmpname = component.
    _pa-langu = obj_langu.
    READ TABLE <pa>-texts ASSIGNING <t>
         WITH TABLE KEY lang = _pa-langu.
    IF sy-subrc = 0.    _pa-descript = <t>-text.   ENDIF.
    INSERT _pa INTO TABLE fm_data-parameters.
  ENDLOOP.
  endmethod.


  method TD_SUBC_DESC.
  DATA _d   LIKE LINE OF descriptions.
  FIELD-SYMBOLS <t> LIKE LINE OF texts.

  LOOP AT texts ASSIGNING <t>.
    _d-clsname = obj_name.    _d-cmpname = component.   _d-sconame = sub_component.
    _d-langu = <t>-lang.     _d-descript = <t>-text.

    INSERT _d INTO TABLE descriptions.
  ENDLOOP.
  endmethod.


  method TD_TEXT.
  DATA _d   LIKE LINE OF documentation.
  DATA _dt  LIKE LINE OF _d-texts.
  DATA _ct  LIKE LINE OF descriptions.
  FIELD-SYMBOLS:
    <t>   LIKE LINE OF texts.

* Prepare documentaion if any.
  _d-application = 'DOKU'.
  _d-id = id.
  _d-object = classname.
  _d-langu = sy-langu.
  _d-masterlang = abap_true.
  _d-typ = 'E'.
  _d-dokform = 'S_DOCU_SHOW'.
  _d-dokstyle = 'S_DOCUS1'.
* Issue 33 : Documentation connected to text
  LOOP AT texts ASSIGNING <t>.
    CLEAR _ct.    _ct-langu = <t>-lang.   _ct-descript = <t>-text.    _ct-clsname = classname.
    INSERT _ct INTO TABLE descriptions.

    CHECK NOT <t>-_ IS INITIAL.
    _dt-_ = <t>-_.    _dt-tdspras = <t>-lang.
    INSERT _dt INTO TABLE _d-texts.
  ENDLOOP.
  IF NOT _d-texts IS INITIAL. INSERT _d INTO TABLE documentation. ENDIF.
  endmethod.


  method TO_DATA.
  DATA _id  LIKE LINE OF fm_data-impl_details.
  DATA _at  LIKE LINE OF fm_data-attributes.
  DATA _ty  LIKE LINE OF fm_data-types.
  DATA _tg  LIKE LINE OF fm_data-typeusages.
  DATA _me  LIKE LINE OF fm_data-methods.
  DATA _al  LIKE LINE OF fm_data-aliases.
  DATA _ev  LIKE LINE OF fm_data-events.
  DATA _ms  LIKE LINE OF fm_data-method_sources.
  DATA _dc  LIKE LINE OF fm_data-clsdeferrds.
  DATA _di  LIKE LINE OF deferred-interfaces.
  DATA tmp_me LIKE LINE OF methods.
  DATA id   TYPE doku_id.
  FIELD-SYMBOLS:
    <at>  LIKE LINE OF attributs,
    <al>  LIKE LINE OF aliases,
    <me>  LIKE LINE OF methods,
    <ev>  LIKE LINE OF events,
    <ct>  LIKE LINE OF <at>-texts,
    <dc>  LIKE LINE OF deferred-classes,
    <di>  LIKE LINE OF deferred-interfaces,
    <ty>  LIKE LINE OF types,
    <tu>  LIKE LINE OF typegroups.

********  maindata-textpool = zaplink_program=>get_textpool( fm_data-includes-classpool ).

* to handle version 1.0
  APPEND LINES OF documentation TO fm_data-documentation.

*TYPEPUSAGES  TYPE  SEOT_TYPEPUSAGES_R
  LOOP AT typegroups ASSIGNING <tu>.
    CLEAR _tg.
    _tg = <tu>-hdr.

    IF NOT <tu>-defered IS INITIAL.
      <tu>-defered-clsname = <tu>-clsname.
      <tu>-defered-typegroup = <tu>-typegroup.
      <tu>-defered-version = <tu>-version.
      APPEND <tu>-defered TO fm_data-clsdeferrds.
    ENDIF.

    IF NOT <tu>-itf_def IS INITIAL.
      <tu>-itf_def-clsname = <tu>-clsname.
      <tu>-itf_def-typegroup = <tu>-typegroup.
      <tu>-itf_def-version = <tu>-version.
      APPEND <tu>-itf_def TO fm_data-intdeferrds.
    ENDIF.

    _tg-clsname = obj_name.
    INSERT _tg INTO TABLE fm_data-typeusages.
  ENDLOOP.

* ISSUE 2 : Begin
*CLSDEFERRDS  TYPE  SEOT_CLSDEFERRDS_R
  LOOP AT deferred-classes ASSIGNING <dc>.
    CLEAR _dc. _dc = <dc>.
    _dc-clsname = obj_name..
    INSERT _dc INTO TABLE fm_data-clsdeferrds.
  ENDLOOP.

*INTDEFERRDS  TYPE  SEOT_INTDEFERRDS_R
  LOOP AT deferred-interfaces ASSIGNING <di>.
    CLEAR _di. _di = <di>.
    _di-clsname = obj_name.
    INSERT _di INTO TABLE fm_data-intdeferrds.
  ENDLOOP.
* ISSUE 2 : End

*ATTRIBUTES TYPE  SEOO_atTTRIBUTES_R
*COMPONENT_DESC  TYPE  tt_SEOCOMPOTX,
  IF obj_type = object_types-class. id = doc_ids-class-attribute. ELSE. id = doc_ids-interface-attribute. ENDIF.
  LOOP AT attributs ASSIGNING <at>.
    CLEAR _at.
    _at = <at>-hdr.
    _at-typesrc = <at>-_.                                   " Issue 68
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN _at-typesrc WITH cl_abap_char_utilities=>cr_lf.

    CALL METHOD td_comp_desc
      EXPORTING
        component     = <at>-cmpname
        texts         = <at>-texts
        id            = id
      CHANGING
        documentation = fm_data-documentation
        descriptions  = fm_data-component_desc.

    _at-clsname = obj_name.
    _at-langu = obj_langu.
    READ TABLE <at>-texts ASSIGNING <ct>
         WITH TABLE KEY lang = _at-langu.
    IF sy-subrc = 0.    _at-descript = <ct>-text.      ENDIF.
    INSERT _at INTO TABLE fm_data-attributes.
  ENDLOOP.

*METHODS  TYPE  SEOO_meETHODS_R
*PARAMETERS TYPE  SEOS_paARAMETERS_R
*EXCEPS TYPE  SEOS_exXCEPTIONS_R
*SUBCOMPONENT_DESC type  tt_SEOSUBCOTX,
  IF obj_type = object_types-class. id = doc_ids-class-method. ELSE. id = doc_ids-interface-method. ENDIF.
  LOOP AT methods ASSIGNING <me>.
    CLEAR: _me, _ms.
    _me = <me>-hdr.

    CALL METHOD td_comp_desc
      EXPORTING
        component     = <me>-cmpname
        texts         = <me>-texts
        id            = id
      CHANGING
        documentation = fm_data-documentation
        descriptions  = fm_data-component_desc.

    CALL METHOD td_parameters
      EXPORTING
        component  = <me>-cmpname
        PARAMETERS = <me>-parameters
      CHANGING
        fm_data    = fm_data.

    CALL METHOD td_exceptions
      EXPORTING
        component = <me>-cmpname
        exception = <me>-exceptions
      CHANGING
        fm_data   = fm_data.

    tmp_me = <me>.
    CLEAR: tmp_me-cmpname, tmp_me-source.
    IF tmp_me IS INITIAL.
      CONTINUE.  " Interface implementation
    ENDIF.

    _ms-cpdname = <me>-cmpname.
    _ms-source = zaplink_object=>conv_abap_source2raw( <me>-source-_ ).
    APPEND _ms TO fm_data-method_sources.

    _me-clsname = obj_name.
    _me-langu = obj_langu.
    READ TABLE <me>-texts ASSIGNING <ct>
         WITH TABLE KEY lang = _me-langu.
    IF sy-subrc = 0.    _me-descript = <ct>-text.      ENDIF.
    INSERT _me INTO TABLE fm_data-methods.
  ENDLOOP.

*EVENTS TYPE  SEOO_exVENTS_R
*PARAMETERS TYPE  SEOS_paARAMETERS_R
*EXCEPS TYPE  SEOS_exXCEPTIONS_R
*SUBCOMPONENT_DESC type  tt_SEOSUBCOTX,
  IF obj_type = object_types-class. id = doc_ids-class-event. ELSE. id = doc_ids-interface-event. ENDIF.
  LOOP AT events ASSIGNING <ev>.
    CLEAR _ev.
    _ev = <ev>-hdr.
    CALL METHOD td_comp_desc
      EXPORTING
        component     = <ev>-cmpname
        texts         = <ev>-texts
        id            = id
      CHANGING
        documentation = fm_data-documentation
        descriptions  = fm_data-component_desc.

    CALL METHOD td_parameters
      EXPORTING
        component  = <ev>-cmpname
        PARAMETERS = <ev>-parameters
      CHANGING
        fm_data    = fm_data.

    _ev-clsname = obj_name.
    _ev-langu = obj_langu.
    READ TABLE <ev>-texts ASSIGNING <ct>
         WITH TABLE KEY lang = _ev-langu.
    IF sy-subrc = 0.    _ev-descript = <ct>-text.      ENDIF.
    INSERT _ev INTO TABLE fm_data-events.
  ENDLOOP.

*TYPES  TYPE  SEOO_TYPES_R
  IF obj_type = object_types-class. id = doc_ids-class-type. ELSE. id = doc_ids-interface-type. ENDIF.
  LOOP AT types ASSIGNING <ty>.
    CLEAR _ty.
    _ty = <ty>-hdr.
    _ty-typesrc = <ty>-_.                                   " Issue 68
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN _ty-typesrc WITH cl_abap_char_utilities=>cr_lf.

    CALL METHOD me->td_comp_desc
      EXPORTING
        component     = <ty>-cmpname
        texts         = <ty>-texts
        id            = id
      CHANGING
        documentation = fm_data-documentation
        descriptions  = fm_data-component_desc.

    _ty-clsname = obj_name.
    _ty-langu = obj_langu.
    READ TABLE <ty>-texts ASSIGNING <ct>
         WITH TABLE KEY lang = _ty-langu.
    IF sy-subrc = 0.    _ty-descript = <ct>-text.      ENDIF.
    INSERT _ty INTO TABLE fm_data-types.
  ENDLOOP.

*ALIASES  TYPE  SEOO_atLIASES_R
  LOOP AT aliases ASSIGNING <al>.
    CLEAR _al.
    _al = <al>-hdr.

*    CALL METHOD me->td_comp_desc                    " not yet supported by SAP
*      EXPORTING
*        component     = <al>-cmpname
*        texts         = <al>-texts
*        id            = space
*      CHANGING
*        documentation = fm_data-documentation
*        descriptions  = fm_data-component_desc.

    _al-clsname = obj_name.
    INSERT _al INTO TABLE fm_data-aliases.
  ENDLOOP.

* No use detected
*EXPLORE_INHERITANCE  TYPE  SEOK_CLS_TYPEINFOS
*EXPLORE_IMPLEMENTINGS  TYPE  SEOK_INT_TYPEINFOS
*ENHANCEMENT_meETHODS  TYPE  ENHMETH_TABHEADER
*ENHANCEMENT_atTTRIBUTES TYPE  ENHCLASSTABATTRIB
*ENHANCEMENT_exVENTS TYPE  ENHCLASSTABEVENT
*ENHANCEMENT_IMPLEMENTINGS  TYPE  ENHCLASSTABIMPLEMENTING
  endmethod.


  method UNANONYMIZE.
* Every thing should be done by SAP injection (data will be completed)
**  DATA s_al  LIKE LINE OF aliases.
*  DATA s_at LIKE LINE OF attributs.
**  DATA s_ev LIKE LINE OF events.
*  FIELD-SYMBOLS:
*    <ev>    LIKE LINE OF events.
*  DATA s_in LIKE LINE OF interfaces.
*  DATA s_ty  LIKE LINE OF types.
**  DATA s_tg  LIKE LINE OF typegroups.
**  DATA s_me  LIKE LINE OF methods.
*  FIELD-SYMBOLS:
*    <me>    LIKE LINE OF methods.
*  DATA s_pa  LIKE LINE OF <me>-parameters.
*  DATA s_ex  LIKE LINE OF <me>-exceptions.
*
** ALIASES : OK
** ATTRIBUTS
*  MODIFY attributs FROM s_at TRANSPORTING author createdon changedby changedon r3release
*         WHERE NOT cmpname IS INITIAL.
*
**DOCUMENTATION
*
**EVENTS
**  MODIFY events FROM s_ev TRANSPORTING author createdon changedby changedon r3release
**         WHERE NOT cmpname IS INITIAL.
*  LOOP AT events ASSIGNING <ev>.
*    CLEAR:
*      <ev>-author,
*      <ev>-createdon,
*      <ev>-changedby,
*      <ev>-changedon,
*      <ev>-r3release.
*    MODIFY <me>-parameters FROM s_pa TRANSPORTING author createdon changedby changedon
*           WHERE NOT cmpname IS INITIAL.
*    MODIFY <me>-exceptions FROM s_ex TRANSPORTING author createdon changedby changedon
*           WHERE NOT cmpname IS INITIAL.
*  ENDLOOP.
*
**INTERFACES
*  MODIFY interfaces FROM s_in TRANSPORTING author createdon changedby changedon
*         WHERE NOT refclsname IS INITIAL.
*
**METHODS
**  MODIFY methods FROM s_me TRANSPORTING author createdon changedby changedon r3release
**         WHERE NOT cmpname IS INITIAL.
*  LOOP AT methods ASSIGNING <me>.
*    CLEAR:
*      <me>-author,
*      <me>-createdon,
*      <me>-changedby,
*      <me>-changedon,
*      <me>-r3release.
*    MODIFY <me>-parameters FROM s_pa TRANSPORTING author createdon changedby changedon
*           WHERE NOT cmpname IS INITIAL.
*    MODIFY <me>-exceptions FROM s_ex TRANSPORTING author createdon changedby changedon
*           WHERE NOT cmpname IS INITIAL.
*  ENDLOOP.
*
**TYPEGROUPS : OK
**TYPES
*  MODIFY types FROM s_ty TRANSPORTING author createdon changedby changedon r3release
*         WHERE NOT cmpname IS INITIAL.
*
**DEFERRED : OK
  endmethod.
ENDCLASS.
