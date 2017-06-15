class ZAPLINK_ENHS_DATA definition
  public
  inheriting from ZAPLINK_ENHANCEMENT_DATA
  create protected

  global friends ZAPLINK_EASYXML
                 ZAPLINK_ENHANCEMENT .

public section.

  types:
    BEGIN OF ts_badi_def_main,
*      include type ENH_BADI_DATA.
* Will use move-corresponding
          badi_name       TYPE enh_badi_data-badi_name,
          interface_name  TYPE enh_badi_data-interface_name,
          single_use      TYPE enh_badi_data-single_use,
          context_mode    TYPE enh_badi_data-context_mode,
          sorter_badi     TYPE enh_badi_data-sorter_badi,
          default_class   TYPE enh_badi_data-default_class,
          mig_from_badi   TYPE enh_badi_data-mig_from_badi,
          img_docu_id     TYPE enh_badi_data-img_docu_id,
          use_fallback_class  TYPE enh_badi_data-use_fallback_class,
          filter_limitation   TYPE enh_badi_data-filter_limitation,
          exception_expected  TYPE enh_badi_data-exception_expected,
        END OF ts_badi_def_main .
  types:
    BEGIN OF ts_filter_main,
*      include type ENH_BADI_FILTER.
* Will use move-corresponding
          filter_name         TYPE enh_badi_filter-filter_name,
          filter_type         TYPE enh_badi_filter-filter_type,
          constant            TYPE enh_badi_filter-constant,
          check_type          TYPE enh_badi_filter-check_type,
          check_type_is_dom   TYPE enh_badi_filter-check_type_is_dom,
          check_class         TYPE enh_badi_filter-check_class,
          check_length        TYPE enh_badi_filter-check_length,
          check_decs          TYPE enh_badi_filter-check_decs,
        END OF ts_filter_main .
  types:
    BEGIN OF ts_filter_def.
*      include type ENH_BADI_DATA.
    INCLUDE TYPE ts_filter_main AS main.
    TYPES:
        texts TYPE tt_ltexts,
      END OF ts_filter_def .
  types:
    tt_filter_defs TYPE SORTED TABLE OF ts_filter_def WITH UNIQUE KEY filter_name .
  types:
    BEGIN OF ts_badi_def.
*      include type ENH_BADI_DATA.
    INCLUDE TYPE ts_badi_def_main AS main.
    TYPES:
        texts     TYPE tt_ltexts,
        filters   TYPE tt_filter_defs,
      END OF ts_badi_def .
  types:
    tt_badi_defs TYPE SORTED TABLE OF ts_badi_def WITH UNIQUE KEY badi_name .
  types:
    BEGIN OF ts_get_language,
          language      TYPE  spras,
*        modi_langu    TYPE  spras,  " only on editing
        END OF ts_get_language .
  types:
    BEGIN OF ts_get_attributes,
          author      TYPE  cnam,
          createdon   TYPE  rdir_cdate,
          changedby   TYPE  unam,
          changedon   TYPE  rdir_udate,
        END OF ts_get_attributes .
  types:
    BEGIN OF ts_enhspotcomphead,
          name              TYPE enhspotcompositename,
          version	          TYPE r3state,
          comp_type	        TYPE enhspotcomptype,
          tool_type         TYPE enhspottooltype,
          composite         TYPE enhspotcompositename,
          internal_use      TYPE enh_dy_spot-internal_use.
    INCLUDE TYPE ts_get_language AS lang.
    INCLUDE TYPE ts_get_attributes AS attrs.
    TYPES:
      END OF ts_enhspotcomphead .
  types:
    BEGIN OF ts_header.
    INCLUDE TYPE ts_enhspotcomphead AS main.
    TYPES:
        texts             TYPE tt_ltexts,
      END OF ts_header .
  types:
    BEGIN OF ts_fm_data,
          header          TYPE ts_enhspotcomphead,
          texts           TYPE tt_ltexts,
          def_data        TYPE enh_badi_data_it,
*     documentation   TYPE tt_docs,
        END OF ts_fm_data .

  data A0_MAINDATA type TS_HEADER read-only .
  data BADI_DEFINITIONS type TT_BADI_DEFS read-only .

  methods FROM_DATA
    importing
      !DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(DATA) type TS_FM_DATA .

  methods ZAPLINK_CNX_EXT_CLEANER_4DATA~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER_4DATA~UNANONYMIZE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_ENHS_DATA IMPLEMENTATION.


  method FROM_DATA.
  DATA s_def      LIKE LINE OF badi_definitions.
  DATA s_filter   LIKE LINE OF s_def-filters.
  DATA s_texts    TYPE ts_sotr.
  FIELD-SYMBOLS:
    <def> LIKE LINE OF data-def_data,
    <f>   LIKE LINE OF <def>-filters.

* Header
  MOVE-CORRESPONDING data-header TO a0_maindata-main.
* Texts
  a0_maindata-texts = data-texts.

* Childs
  LOOP AT data-def_data ASSIGNING <def>.
    CLEAR s_def.
    MOVE-CORRESPONDING <def> TO s_def-main.
    s_texts-short = <def>-badi_shorttext_id.        s_texts-long = <def>-badi_longtext_id.
    s_def-texts = sotr_2_texts( s_texts ).
    LOOP AT <def>-filters ASSIGNING <f>.
      MOVE-CORRESPONDING <f> TO s_filter-main.
      s_texts-short = <f>-filtertext_id.            s_texts-long = space.
      s_filter-texts = sotr_2_texts( s_texts ).
      INSERT s_filter INTO TABLE s_def-filters.
    ENDLOOP.
    INSERT s_def INTO TABLE badi_definitions.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_def      LIKE LINE OF data-def_data.
  DATA s_filter   LIKE LINE OF s_def-filters.
  FIELD-SYMBOLS:
    <def> LIKE LINE OF badi_definitions,
    <f>   LIKE LINE OF <def>-filters.

  MOVE-CORRESPONDING a0_maindata-main to data-header.
  LOOP AT badi_definitions ASSIGNING <def>.
    CLEAR s_def.
    MOVE-CORRESPONDING <def>-main TO s_def.
    LOOP AT <def>-filters ASSIGNING <f>.
      MOVE-CORRESPONDING <f>-main TO s_filter.
      INSERT s_filter INTO TABLE s_def-filters.
    ENDLOOP.
    INSERT s_def INTO TABLE data-def_data.
  ENDLOOP.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER_4DATA~ANONYMIZE.
  CLEAR a0_maindata-createdon.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER_4DATA~UNANONYMIZE.
  endmethod.
ENDCLASS.
