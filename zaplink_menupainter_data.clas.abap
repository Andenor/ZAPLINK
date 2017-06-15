class ZAPLINK_MENUPAINTER_DATA definition
  public
  create public

  global friends ZAPLINK_EASYXML .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TD_PROGNAME
    for ZAPLINK_DATATYPES~TD_PROGNAME .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .

  types:
    BEGIN OF ts_mp_text,
          langu TYPE rsmptexts-sprsl,
          text  TYPE rsmptexts-text,
*    types:
*      begin of ts_menu.
*        include TYPE rsmpe_mnl as hdr.
*    types:
*        texts type tt_mp_texts,
*      end of ts_menu.
*    TYPES:
*      tt_mtexts TYPE STANDARD TABLE OF t WITH unique KEY code .
        END OF ts_mp_text .
  types:
    tt_mp_texts TYPE SORTED TABLE OF ts_mp_text WITH UNIQUE KEY langu .
  types:
    BEGIN OF ts_mp_ftext,
          langu TYPE rsmptexts-sprsl,
          text  TYPE rsmptexts-text,
          icon  TYPE rsmptexts-text,
          info  TYPE rsmptexts-text,
        END OF ts_mp_ftext .
  types:
    tt_mp_ftexts TYPE SORTED TABLE OF ts_mp_ftext WITH UNIQUE KEY langu .
  types:
    tt_funcs TYPE SORTED TABLE OF gui_func WITH UNIQUE KEY table_line .
  types:
    BEGIN OF ts_status.
    INCLUDE TYPE rsmpe_sta AS hdr.
    TYPES:
        texts TYPE tt_mp_texts,
        functions TYPE tt_funcs,
      END OF ts_status .
  types:
    tt_status TYPE SORTED TABLE OF ts_status WITH UNIQUE KEY code .
  types:
    BEGIN OF ts_function.
    INCLUDE TYPE rsmpe_fun AS hdr.
    TYPES:
        texts         TYPE tt_mp_ftexts,
      END OF ts_function .
  types:
    tt_functions TYPE SORTED TABLE OF ts_function WITH UNIQUE KEY code textno .
  types:
    tt_menu_details TYPE SORTED TABLE OF rsmpe_men WITH UNIQUE KEY no .
  types:
    BEGIN OF ts_menu.
    INCLUDE TYPE rsmpe_mnl AS hdr.
    TYPES:
        texts        TYPE tt_mp_texts,
        composition  TYPE tt_menu_details,
      END OF ts_menu .
  types:
    tt_menus TYPE SORTED TABLE OF ts_menu WITH UNIQUE KEY code .
  types:
    tt_bar_details TYPE SORTED TABLE OF rsmpe_act WITH UNIQUE KEY no .
  types:
    BEGIN OF ts_bar.
    INCLUDE TYPE rsmpe_atr AS hdr.
    TYPES:
        texts        TYPE tt_mp_texts,
        composition  TYPE tt_bar_details,
      END OF ts_bar .
  types:
    tt_bars TYPE SORTED TABLE OF ts_bar WITH UNIQUE KEY obj_code .
  types:
    tt_button_details TYPE SORTED TABLE OF rsmpe_but WITH UNIQUE KEY no .
  types:
    BEGIN OF ts_button.
    INCLUDE TYPE rsmpe_atr AS hdr.
    TYPES:
        texts        TYPE tt_mp_texts,
        composition  TYPE tt_button_details,
      END OF ts_button .
  types:
    tt_buttons TYPE SORTED TABLE OF ts_button WITH UNIQUE KEY sub_code .
  types:
    tt_keyssettings TYPE SORTED TABLE OF rsmpe_pfk WITH UNIQUE KEY pfno .
  types:
    BEGIN OF ts_keyboard.
    INCLUDE TYPE rsmpe_atr AS hdr.
    TYPES:
        texts        TYPE tt_mp_texts,
        keys         TYPE tt_keyssettings,
        pushbuttons  TYPE tt_buttons,
      END OF ts_keyboard .
  types:
    tt_keyboards TYPE SORTED TABLE OF ts_keyboard WITH UNIQUE KEY obj_code .
  types:
    BEGIN OF ts_attribute.
    INCLUDE TYPE rsmpe_atr AS hdr.
    TYPES:
        texts TYPE tt_mp_texts,
      END OF ts_attribute .
  types:
    tt_attributes TYPE SORTED TABLE OF ts_attribute WITH UNIQUE KEY obj_type obj_code sub_code .
  types:
    BEGIN OF ts_title.
    INCLUDE TYPE rsmpe_tit AS hdr.
    TYPES:
        texts TYPE tt_mp_texts,
      END OF ts_title .
  types:
    tt_titles TYPE SORTED TABLE OF ts_title WITH UNIQUE KEY code .
  types:
    tt_ffuncs TYPE SORTED TABLE OF rsmpe_buts WITH UNIQUE KEY obj_code sub_code .
  types:
    tt_stat TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY .
  types:
    tt_funt TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY .
  types:
    tt_men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY .
  types:
    tt_mnlt TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY .
  types:
    tt_act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY .
  types:
    tt_but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY .
  types:
    tt_pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY .
  types:
    tt_staf TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY .
  types:
    tt_atrt TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY .
  types:
    tt_titt TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY .
  types:
    tt_buts TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY .
  types:
    tt_stexts TYPE SORTED TABLE OF rsmptexts WITH UNIQUE KEY obj_type obj_code sub_code texttype sprsl .
  types:
    BEGIN OF ts_fm_data,
          header        TYPE rsmpe_adm,
          status        TYPE tt_stat,
          functions     TYPE tt_funt,
          menus_det     TYPE tt_men,
          menus         TYPE tt_mnlt,
          bars_det      TYPE tt_act,
          buttons_det   TYPE tt_but,
          keyboards     TYPE tt_pfk,
          stat_funcs    TYPE tt_staf,
          attributes    TYPE tt_atrt,
          titles        TYPE tt_titt,
          ffuncs        TYPE tt_buts,
          texts         TYPE tt_stexts,
        END OF ts_fm_data .

  class-methods CLASS_CONSTRUCTOR .
  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    importing
      !PROGRAM type TD_PROGNAME
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.

  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .

  class-data R_DOC_IDS type TR_DOCID .
  constants:
    BEGIN OF doc_ids,
      header TYPE td_doc_id VALUE 'DO',
    END OF doc_ids .
  constants:
    BEGIN OF obj_types,   " FROM Include RSMPECON
      menu      TYPE mp_o_type VALUE 'M',
      function  TYPE mp_o_type VALUE 'F',
      title     TYPE mp_o_type VALUE 'T',
      menu_bar  TYPE mp_o_type VALUE 'A',
      key_set   TYPE mp_o_type VALUE 'P',  " Function Key Setting
      button    TYPE mp_o_type VALUE 'B',  " Pushbutton settings
      status    TYPE mp_o_type VALUE 'C',
      separator TYPE mp_o_type VALUE 'S',
      icon      TYPE mp_o_type VALUE 'I',
      program   TYPE mp_o_type VALUE 'D',
      dynpro    TYPE mp_o_type VALUE 'E',
      text      TYPE mp_o_type VALUE 'Z',
      context   TYPE mp_o_type VALUE 'X',
      symbolbar TYPE mp_o_type VALUE 'Y',
    END OF obj_types .
  constants:
    BEGIN OF text_types,
      object      TYPE mp_txttype VALUE 'M',
      icon        TYPE mp_txttype VALUE 'I',
      quick_info  TYPE mp_txttype VALUE 'Q',
      tech_info   TYPE mp_txttype VALUE 'T',  " Technical Information
    END OF text_types .
  data BAR type GUI_FUNC .
  data MENU type GUI_FUNC .
  data FUNCTION type GUI_FUNC .
  data DEFAULT_BAR type GUI_FUNC .
  data DEFAULT_FUNCTION type GUI_FUNC .
  data MASTER_LANG type SPRSL .
  data STATUS type TT_STATUS .
  data MENU_LIST type TT_MENUS .
  data MENU_BARS type TT_BARS .
  data KEY_SETTINGS type TT_KEYBOARDS .
  data FUNCTIONS type TT_FUNCTIONS .
  data TITLES type TT_TITLES .
private section.

  class-methods GET_TEXTS
    importing
      !TEXTS type TT_STEXTS
      !OBJ_TYPE type RSMPTEXTS-OBJ_TYPE
      !OBJ_CODE type RSMPTEXTS-OBJ_CODE
      !SUB_CODE type CLIKE
      !TEXTTYPE type RSMPTEXTS-TEXTTYPE
    returning
      value(RESULT) type TT_MP_TEXTS .
  class-methods GET_FUNC_TEXTS
    importing
      !TEXTS type TT_STEXTS
      !OBJ_CODE type RSMPTEXTS-OBJ_CODE
      !SUB_CODE type CLIKE
    returning
      value(RESULT) type TT_MP_FTEXTS .
  class-methods SET_TEXTS
    importing
      !OBJ_TYPE type RSMPTEXTS-OBJ_TYPE
      !OBJ_CODE type RSMPTEXTS-OBJ_CODE
      !SUB_CODE type CLIKE
      !TEXTTYPE type RSMPTEXTS-TEXTTYPE
      value(TEXTS) type TT_MP_TEXTS
    changing
      !RESULT type TT_STEXTS .
  class-methods SET_FUNC_TEXTS
    importing
      !OBJ_CODE type RSMPTEXTS-OBJ_CODE
      !SUB_CODE type CLIKE
      !TEXTS type TT_MP_FTEXTS
    changing
      !RESULT type TT_STEXTS .
ENDCLASS.



CLASS ZAPLINK_MENUPAINTER_DATA IMPLEMENTATION.


  method ANONYMIZE.
*  FIELD-SYMBOLS:
*    <t> like LINE OF texts.
*
*  clear: a0_maindata-AS4USER, a0_maindata-AS4DATE, a0_maindata-AS4TIME.
*  loop at texts ASSIGNING <t>.
*    clear: <t>-LONG_TXT-TDFUSER, <t>-LONG_TXT-TDFDATE, <t>-LONG_TXT-TDFTIME.
*  endloop.
  endmethod.


  method CLASS_CONSTRUCTOR.
  DATA _id LIKE LINE OF r_doc_ids.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = doc_ids-header. APPEND _id TO r_doc_ids.
  endmethod.


  method FROM_DATA.
  DATA t_tmp   TYPE tt_atrt.
  DATA t_attr  TYPE SORTED TABLE OF rsmpe_atrt WITH UNIQUE KEY obj_code sub_code obj_type.
  DATA s_attr  LIKE LINE OF t_attr.
  DATA t_tmp_m TYPE tt_mnlt.
  DATA t_menus TYPE SORTED TABLE OF rsmpe_mnlt WITH UNIQUE KEY code.
  DATA s_mnu   LIKE LINE OF t_menus.
  DATA s_title LIKE LINE OF titles.
  DATA s_func  LIKE LINE OF functions.
  DATA s_statu LIKE LINE OF status.
  DATA s_s_fu  LIKE LINE OF s_statu-functions.
  DATA s_menu  LIKE LINE OF menu_list.
  DATA s_m_det LIKE LINE OF s_menu-composition.
  DATA s_bar   LIKE LINE OF menu_bars.
  DATA s_b_det LIKE LINE OF s_bar-composition.
  DATA s_key   LIKE LINE OF key_settings.
  DATA s_k_det LIKE LINE OF s_key-keys.
  DATA s_but   LIKE LINE OF s_key-pushbuttons.
  DATA s_u_det LIKE LINE OF s_but-composition.
  FIELD-SYMBOLS:
    <k>  LIKE LINE OF key_settings,
    <at> LIKE LINE OF fm_data-attributes,
    <bd> LIKE LINE OF fm_data-bars_det,
    <kb> LIKE LINE OF fm_data-keyboards,
    <bu> LIKE LINE OF fm_data-buttons_det,
    <sf> LIKE LINE OF fm_data-stat_funcs,
    <st> LIKE LINE OF fm_data-status,
    <mn> LIKE LINE OF fm_data-menus,
    <md> LIKE LINE OF fm_data-menus_det,
    <fu> LIKE LINE OF fm_data-functions,
    <ti> LIKE LINE OF fm_data-titles.

  bar = fm_data-header-actcode.
  menu = fm_data-header-mencode.
  function = fm_data-header-pfkcode.
  default_bar = fm_data-header-defaultact.
  default_function = fm_data-header-defaultpfk.
  master_lang = fm_data-header-mod_langu.

  LOOP AT fm_data-titles ASSIGNING <ti>.
    CLEAR s_title.
    MOVE-CORRESPONDING <ti> TO s_title-hdr.
    s_title-texts = get_texts( texts = fm_data-texts
                            obj_type = obj_types-title
                            obj_code = s_title-code
                            sub_code = space
                            texttype = text_types-tech_info ).
    INSERT s_title INTO TABLE titles.
  ENDLOOP.

  LOOP AT fm_data-functions ASSIGNING <fu>.
    CLEAR s_func.
    MOVE-CORRESPONDING <fu> TO s_func-hdr.
    s_func-texts = get_func_texts( texts = fm_data-texts
                                obj_code = s_func-code
                                sub_code = s_func-textno ).
    INSERT s_func INTO TABLE functions.
  ENDLOOP.

  t_tmp_m = fm_data-menus.
  SORT t_tmp_m                                   BY code.
  DELETE ADJACENT DUPLICATES FROM t_tmp_m COMPARING code.
  t_menus = t_tmp_m.
* Security Menu bars without name are not in attributes tables.
  LOOP AT fm_data-menus_det ASSIGNING <md>.
    READ TABLE t_menus TRANSPORTING NO FIELDS
         WITH TABLE KEY code = <md>-code.
    IF sy-subrc <> 0.
      CLEAR s_mnu.        s_mnu-code = <md>-code.
      INSERT s_mnu INTO TABLE t_menus.
    ENDIF.
  ENDLOOP.
  LOOP AT t_menus ASSIGNING <mn>.
    CLEAR s_menu.
    MOVE-CORRESPONDING <mn> TO s_menu-hdr.
    s_menu-texts = get_texts( texts = fm_data-texts
                           obj_type = obj_types-menu
                           obj_code = s_menu-code
                           sub_code = space
                           texttype = text_types-object ).
    LOOP AT fm_data-menus_det ASSIGNING <md>
            WHERE code = <mn>-code.
      CLEAR s_m_det.
      MOVE-CORRESPONDING <md> TO s_m_det.
      CLEAR: s_m_det-code.
      INSERT s_m_det INTO TABLE s_menu-composition.
    ENDLOOP.
    INSERT s_menu INTO TABLE menu_list.
  ENDLOOP.

  t_tmp = fm_data-attributes.
  SORT t_tmp                                   BY obj_code sub_code obj_type.
  DELETE ADJACENT DUPLICATES FROM t_tmp COMPARING obj_code sub_code obj_type.
  t_attr = t_tmp.
* Security Menu bars without name are not in attributes tables.
  LOOP AT fm_data-bars_det ASSIGNING <bd>.
    READ TABLE t_attr TRANSPORTING NO FIELDS
         WITH TABLE KEY obj_code = <bd>-code
                        sub_code = space
                        obj_type = obj_types-menu_bar.
    IF sy-subrc <> 0.
      CLEAR s_attr.        s_attr-obj_type = obj_types-menu_bar.         s_attr-obj_code = <bd>-code.
      INSERT s_attr INTO TABLE t_attr.
    ENDIF.
  ENDLOOP.
* Security keyboars without name are not in attributes tables.
  LOOP AT fm_data-keyboards ASSIGNING <kb>.
    READ TABLE t_attr TRANSPORTING NO FIELDS
         WITH TABLE KEY obj_code = <kb>-code
                        sub_code = space
                        obj_type = obj_types-key_set.
    IF sy-subrc <> 0.
      CLEAR s_attr.        s_attr-obj_type = obj_types-key_set.         s_attr-obj_code = <kb>-code.
      INSERT s_attr INTO TABLE t_attr.
    ENDIF.
  ENDLOOP.
* Security button without name are not in attributes tables.
  LOOP AT fm_data-buttons_det ASSIGNING <bu>.
    READ TABLE t_attr TRANSPORTING NO FIELDS
         WITH TABLE KEY obj_code = <bu>-pfk_code
                        sub_code = <bu>-code
                        obj_type = obj_types-button.
    IF sy-subrc <> 0.
      CLEAR s_attr.        s_attr-obj_type = obj_types-button.         s_attr-obj_code = <bu>-pfk_code.     s_attr-sub_code = <bu>-code.
      INSERT s_attr INTO TABLE t_attr.
    ENDIF.
  ENDLOOP.

  LOOP AT t_attr ASSIGNING <at>.
    CASE <at>-obj_type.
      WHEN obj_types-menu_bar.
        CLEAR s_bar.        MOVE-CORRESPONDING <at> TO s_bar-hdr.
        s_bar-texts = get_texts( texts = fm_data-texts
                              obj_type = obj_types-menu_bar
                              obj_code = s_bar-obj_code
                              sub_code = space
                              texttype = text_types-tech_info ).
        LOOP AT fm_data-bars_det ASSIGNING <bd>
                WHERE code = <at>-obj_code.
          CLEAR s_b_det.        MOVE-CORRESPONDING <bd> TO s_b_det.         CLEAR: s_b_det-code.
          INSERT s_b_det INTO TABLE s_bar-composition.
        ENDLOOP.
        CLEAR s_bar-obj_type.
        INSERT s_bar INTO TABLE menu_bars.
      WHEN obj_types-key_set.
        CLEAR s_key.          MOVE-CORRESPONDING <at> TO s_key-hdr.
        s_key-texts = get_texts( texts = fm_data-texts
                              obj_type = obj_types-key_set
                              obj_code = s_key-obj_code
                              sub_code = space
                              texttype = text_types-tech_info ).
        LOOP AT fm_data-keyboards ASSIGNING <kb>
                WHERE code = <at>-obj_code.
          CLEAR s_k_det.          MOVE-CORRESPONDING <kb> TO s_k_det.       CLEAR: s_k_det-code.
          INSERT s_k_det INTO TABLE s_key-keys.
        ENDLOOP.
        CLEAR s_key-obj_type.
        INSERT s_key INTO TABLE key_settings.
      WHEN obj_types-button.
        READ TABLE key_settings ASSIGNING <k>
             WITH TABLE KEY obj_code = <at>-obj_code.
        IF sy-subrc <> 0.
          WRITE:/ 'button without key', <at>-obj_code.
        ELSE.
          CLEAR s_but.        MOVE-CORRESPONDING <at> TO s_but-hdr.
          s_but-texts = get_texts( texts = fm_data-texts
                                obj_type = obj_types-button
                                obj_code = s_but-obj_code
                                sub_code = s_but-sub_code
                                texttype = text_types-tech_info ).
          LOOP AT fm_data-buttons_det ASSIGNING <bu>
                  WHERE pfk_code = <at>-obj_code
                    AND code = <at>-sub_code.
            CLEAR s_u_det.        MOVE-CORRESPONDING <bu> TO s_u_det.       CLEAR: s_u_det-pfk_code, s_u_det-code.
            INSERT s_u_det INTO TABLE s_but-composition.
          ENDLOOP.
          CLEAR: s_but-obj_type, s_but-obj_code.
          INSERT s_but INTO TABLE <k>-pushbuttons.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  LOOP AT fm_data-status ASSIGNING <st>.
    CLEAR s_statu.     MOVE-CORRESPONDING <st> TO s_statu-hdr.
    s_statu-texts = get_texts( texts = fm_data-texts
                            obj_type = obj_types-status
                            obj_code = s_statu-code
                            sub_code = space
                            texttype = text_types-tech_info ).
    LOOP AT fm_data-stat_funcs ASSIGNING <sf>
            WHERE status = <st>-code.
      s_s_fu = <sf>-function.
      INSERT s_s_fu INTO TABLE s_statu-functions.
    ENDLOOP.
    INSERT s_statu INTO TABLE status.
  ENDLOOP.
  endmethod.


  method GET_FUNC_TEXTS.
  DATA s_txt LIKE LINE OF result.
  FIELD-SYMBOLS:
    <r> LIKE LINE OF result,
    <t> LIKE LINE OF texts.

  LOOP AT texts ASSIGNING <t>
       WHERE obj_type = obj_types-function
         AND obj_code = obj_code
         AND sub_code = sub_code.
    READ TABLE result ASSIGNING <r>
         WITH TABLE KEY langu = <t>-sprsl.
    IF sy-subrc <> 0.
      s_txt-langu = <t>-sprsl.
      INSERT s_txt INTO TABLE result.
      READ TABLE result ASSIGNING <r>
           WITH TABLE KEY langu = <t>-sprsl.
      IF sy-subrc <> 0.
* Not possible ==> will dump
      ENDIF.
    ENDIF.

    CASE <t>-texttype.
      WHEN text_types-object.
        <r>-text = <t>-text.
      WHEN text_types-icon.
        <r>-icon = <t>-text.
      WHEN text_types-quick_info.
        <r>-info = <t>-text.
      WHEN OTHERS.
* Erro handling
    ENDCASE.
  ENDLOOP.
  endmethod.


  method GET_TEXTS.
  DATA s_txt LIKE LINE OF result.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF texts.

  LOOP AT texts ASSIGNING <t>
       WHERE obj_type = obj_type
         AND obj_code = obj_code
         AND sub_code = sub_code
         AND texttype = texttype.
    s_txt-langu = <t>-sprsl.
    s_txt-text = <t>-text.
    INSERT s_txt INTO TABLE result.
  ENDLOOP.
  endmethod.


  method SET_FUNC_TEXTS.
  DATA s_txt LIKE LINE OF result.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF texts.

  LOOP AT texts ASSIGNING <t>.
    clear s_txt.
    s_txt-obj_type = obj_types-function.
    s_txt-obj_code = obj_code.
    s_txt-sub_code = sub_code.
    s_txt-sprsl = <t>-langu.
    s_txt-texttype = text_types-object.     s_txt-text = <t>-text.      if not s_txt-text is INITIAL. INSERT s_txt INTO TABLE result. endif.
    s_txt-texttype = text_types-icon.       s_txt-text = <t>-icon.      if not s_txt-text is INITIAL. INSERT s_txt INTO TABLE result. endif.
    s_txt-texttype = text_types-quick_info. s_txt-text = <t>-info.      if not s_txt-text is INITIAL. INSERT s_txt INTO TABLE result. endif.
  ENDLOOP.
  endmethod.


  method SET_TEXTS.
  DATA s_txt LIKE LINE OF result.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF texts.

  LOOP AT texts ASSIGNING <t>.
    s_txt-obj_type = obj_type.
    s_txt-obj_code = obj_code.
    s_txt-sub_code = sub_code.
    s_txt-texttype = texttype.
    s_txt-sprsl = <t>-langu.
    s_txt-text = <t>-text.
    INSERT s_txt INTO TABLE result.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_title LIKE LINE OF fm_data-titles.
  DATA s_func  LIKE LINE OF fm_data-functions.
  DATA s_statu LIKE LINE OF fm_data-status.
  DATA s_s_fu  LIKE LINE OF fm_data-stat_funcs.
  DATA s_menu  LIKE LINE OF fm_data-menus.
  DATA s_m_det LIKE LINE OF fm_data-menus_det.
  DATA s_attr  LIKE LINE OF fm_data-attributes.
  DATA s_b_det LIKE LINE OF fm_data-bars_det.
  DATA s_k_det LIKE LINE OF fm_data-keyboards.
  DATA s_but   LIKE LINE OF fm_data-buttons_det.
  DATA s_text  LIKE LINE OF fm_data-texts.
  DATA d_lang  TYPE sy-langu.
  FIELD-SYMBOLS:
    <mb> LIKE LINE OF menu_bars,
    <bc> LIKE LINE OF <mb>-composition,
    <ks> LIKE LINE OF key_settings,
    <kk> LIKE LINE OF <ks>-keys,
    <pb> LIKE LINE OF <ks>-pushbuttons,
    <pc> LIKE LINE OF <pb>-composition,
    <st> LIKE LINE OF status,
    <sf> LIKE LINE OF <st>-functions,
    <mn> LIKE LINE OF menu_list,
    <md> LIKE LINE OF <mn>-composition,
    <fu> LIKE LINE OF functions,
    <ti> LIKE LINE OF titles,
    <tl> LIKE LINE OF <fu>-texts,
    <t>  LIKE LINE OF <st>-texts.

  fm_data-header-actcode = bar.
  fm_data-header-mencode = menu.
  fm_data-header-pfkcode = function.
  fm_data-header-defaultact = default_bar.
  fm_data-header-defaultpfk = default_function.
  fm_data-header-mod_langu = d_lang = master_lang.
  IF d_lang IS INITIAL.  d_lang = sy-langu.    ENDIF.

  LOOP AT titles ASSIGNING <ti>.
    CLEAR s_title.        MOVE-CORRESPONDING <ti>-hdr TO s_title.
    INSERT s_title INTO TABLE fm_data-titles.
    CALL METHOD set_texts
      EXPORTING
        obj_type = obj_types-title
        obj_code = s_title-code
        sub_code = space
        texttype = text_types-tech_info
        texts    = <ti>-texts
      CHANGING
        RESULT   = fm_data-texts.
  ENDLOOP.

  LOOP AT functions ASSIGNING <fu>.
    CLEAR s_func.     MOVE-CORRESPONDING <fu>-hdr TO s_func.
    READ TABLE <fu>-texts ASSIGNING <tl> WITH TABLE KEY langu = d_lang.
    IF sy-subrc = 0.    s_func-fun_text = <tl>-text.  s_func-icon_text = <tl>-icon.  s_func-info_text = <tl>-info.  ENDIF.    " Issue 116

    INSERT s_func INTO TABLE fm_data-functions.
    CALL METHOD set_func_texts
      EXPORTING
        obj_code = s_func-code
        sub_code = s_func-textno
        texts    = <fu>-texts
      CHANGING
        RESULT   = fm_data-texts.
  ENDLOOP.

  LOOP AT menu_list ASSIGNING <mn>.
    CLEAR s_menu.     MOVE-CORRESPONDING <mn>-hdr TO s_menu.
    INSERT s_menu INTO TABLE fm_data-menus.
    CALL METHOD set_texts
      EXPORTING
        obj_type = obj_types-menu
        obj_code = s_menu-code
        sub_code = space
        texttype = text_types-object
        texts    = <mn>-texts
      CHANGING
        RESULT   = fm_data-texts.
    LOOP AT <mn>-composition ASSIGNING <md>.
      CLEAR s_m_det.      MOVE-CORRESPONDING <md> TO s_m_det.       s_m_det-code = <mn>-code.
      INSERT s_m_det INTO TABLE fm_data-menus_det.
    ENDLOOP.
  ENDLOOP.

* Menu Bars
  LOOP AT menu_bars ASSIGNING <mb>.
    CLEAR s_attr.         MOVE-CORRESPONDING <mb>-hdr TO s_attr.    s_attr-obj_type = obj_types-menu_bar.
    READ TABLE <mb>-texts ASSIGNING <t> WITH TABLE KEY langu = d_lang.
    IF sy-subrc = 0.    s_attr-int_note = <t>-text.  ENDIF.
    INSERT s_attr INTO TABLE fm_data-attributes.
    CALL METHOD set_texts
      EXPORTING
        obj_type = obj_types-menu_bar
        obj_code = s_attr-obj_code
        sub_code = space
        texttype = text_types-tech_info
        texts    = <mb>-texts
      CHANGING
        RESULT   = fm_data-texts.
    LOOP AT <mb>-composition ASSIGNING <bc>.
      CLEAR s_b_det.      MOVE-CORRESPONDING <bc> TO s_b_det.       s_b_det-code = <mb>-obj_code.
      INSERT s_b_det INTO TABLE fm_data-bars_det.
    ENDLOOP.
  ENDLOOP.

* Key settings
  LOOP AT key_settings ASSIGNING <ks>.
    CLEAR s_attr.         MOVE-CORRESPONDING <ks>-hdr TO s_attr.    s_attr-obj_type = obj_types-key_set.
    READ TABLE <ks>-texts ASSIGNING <t> WITH TABLE KEY langu = d_lang.
    IF sy-subrc = 0.    s_attr-int_note = <t>-text.  ENDIF.
    INSERT s_attr INTO TABLE fm_data-attributes.
    CALL METHOD set_texts
      EXPORTING
        obj_type = obj_types-key_set
        obj_code = s_attr-obj_code
        sub_code = space
        texttype = text_types-tech_info
        texts    = <ks>-texts
      CHANGING
        RESULT   = fm_data-texts.
    LOOP AT <ks>-keys ASSIGNING <kk>.
      CLEAR s_k_det.      MOVE-CORRESPONDING <kk> TO s_k_det.       s_k_det-code = <ks>-obj_code.
      INSERT s_k_det INTO TABLE fm_data-keyboards.
    ENDLOOP.
    LOOP AT <ks>-pushbuttons ASSIGNING <pb>.
      CLEAR s_attr.       MOVE-CORRESPONDING <pb> TO s_attr.        s_attr-obj_type = obj_types-button.       s_attr-obj_code = <ks>-obj_code.
      READ TABLE <pb>-texts ASSIGNING <t> WITH TABLE KEY langu = d_lang.
      IF sy-subrc = 0.    s_attr-int_note = <t>-text.  ENDIF.
      INSERT s_attr INTO TABLE fm_data-attributes.
      CALL METHOD set_texts
        EXPORTING
          obj_type = obj_types-button
          obj_code = s_attr-obj_code
          sub_code = s_attr-sub_code
          texttype = text_types-tech_info
          texts    = <pb>-texts
        CHANGING
          RESULT   = fm_data-texts.
      LOOP AT <pb>-composition ASSIGNING <pc>.
        CLEAR s_but.      MOVE-CORRESPONDING <pc> TO s_but.         s_but-pfk_code = <ks>-obj_code.        s_but-code = <pb>-sub_code.
        INSERT s_but INTO TABLE fm_data-buttons_det.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  LOOP AT status ASSIGNING <st>.
    CLEAR s_statu.        MOVE-CORRESPONDING <st>-hdr TO s_statu.
    READ TABLE <st>-texts ASSIGNING <t> WITH TABLE KEY langu = d_lang.
    IF sy-subrc = 0.    s_statu-int_note = <t>-text.  ENDIF.
    INSERT s_statu INTO TABLE fm_data-status.
    CALL METHOD set_texts
      EXPORTING
        obj_type = obj_types-status
        obj_code = s_statu-code
        sub_code = space
        texttype = text_types-tech_info
        texts    = <st>-texts
      CHANGING
        RESULT   = fm_data-texts.
    LOOP AT <st>-functions ASSIGNING <sf>.
      CLEAR s_s_fu.     s_s_fu-status = <st>-code.      s_s_fu-function = <sf>.
      INSERT s_s_fu INTO TABLE fm_data-stat_funcs.
    ENDLOOP.
  ENDLOOP.

  s_text-progname = program.
  MODIFY fm_data-texts FROM s_text TRANSPORTING progname WHERE progname <> program.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
