class ZAPLINK_VARI_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_PROGRAM
                 ZAPLINK_PROG_DATA
                 ZAPLINK_VARI_RAW .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .
  aliases TS_SOURCEASSTRUC
    for ZAPLINK_DATATYPES~TS_SOURCEASSTRUC .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_DYNPROS
    for ZAPLINK_DATATYPES~TT_DYNPROS .
  aliases TT_LTEXTS
    for ZAPLINK_DATATYPES~TT_LTEXTS .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .
protected section.

  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_COMPNAME
    for ZAPLINK_DATATYPES~TD_COMPNAME .

  types:
    tt_varit TYPE SORTED TABLE OF varit WITH UNIQUE KEY langu .
  types:
    tr_string TYPE RANGE OF string .
  types:
    tt_screens TYPE SORTED TABLE OF rsdynnr WITH UNIQUE KEY dynnr .
  types:
    BEGIN OF ts_param_data.
  INCLUDE TYPE vanz AS hdr.
  TYPES:
    value           TYPE string,
    values          TYPE tr_string,
    params          TYPE tr_string,
  END OF ts_param_data .
  types:
    tt_params TYPE SORTED TABLE OF ts_param_data WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_fm_data,
* xx pour type doc
      header          TYPE varid,
      texts           TYPE tt_varit,
      screens         TYPE tt_screens,
      params          TYPE tt_params,
    END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
  INCLUDE TYPE varid.
  TYPES:
    END OF ts_maindata .
  types:
    BEGIN OF ts_variant_key,
        program  TYPE varid-report,
        variant  TYPE varid-variant,
      END OF ts_variant_key .

  constants:
    BEGIN OF variant_prefix,
      system    TYPE varid-variant VALUE 'SAP&*',
      customer  TYPE varid-variant VALUE 'CUS&*',
    END OF variant_prefix .
  data A0_MAINDATA type TS_MAINDATA .
  data VALUES type TT_PARAMS .
  data TEXTS type TT_LTEXTS .
  data SCREENS type TT_SCREENS .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
  class-methods NAME_2_KEY
    importing
      !DATA type TD_COMPNAME
    returning
      value(RESULT) type TS_VARIANT_KEY .
  class-methods KEY_2_NAME
    importing
      !DATA type TS_VARIANT_KEY
    returning
      value(RESULT) type TD_COMPNAME .
private section.

  constants:
    BEGIN OF transport,
* FROM FORM BUILD_DESCRIPTION (LSVARF02)
** Variante transportieren
** Systemvariante sind immer gesch#tzt
** Variante nicht bei F4 anzeigen und transportieren
*  IF RSVAR-NODISPLAY EQ 'X' AND RSVAR-TRANSPORT EQ 'X'.
*    MOVE 'X' TO VARID-TRANSPORT.
** Variante nicht bei F4 anzeigen und nicht transportieren
*  ELSEIF RSVAR-NODISPLAY EQ 'X' AND RSVAR-TRANSPORT EQ SPACE.
*    MOVE 'N' TO VARID-TRANSPORT.
** Variante bei F4 anzeigen und transportieren
*  ELSEIF RSVAR-NODISPLAY EQ SPACE AND RSVAR-TRANSPORT EQ 'X'.
*    MOVE SPACE TO VARID-TRANSPORT.
** Ansonsten F
*  ELSEIF RSVAR-NODISPLAY EQ SPACE AND RSVAR-TRANSPORT EQ SPACE.
*    MOVE 'F' TO VARID-TRANSPORT.
*  ENDIF.
      no_display  TYPE varid-transport VALUE 'N',           "#EC NOTEXT
      hidden      TYPE varid-transport VALUE 'X',           "#EC NOTEXT
      protected   TYPE varid-transport VALUE space,         "#EC NOTEXT
      normal      TYPE varid-transport VALUE 'F',           "#EC NOTEXT
    END OF transport .
ENDCLASS.



CLASS ZAPLINK_VARI_DATA IMPLEMENTATION.


  method ANONYMIZE.
  CLEAR: a0_maindata-mandt, a0_maindata-ename, a0_maindata-edat, a0_maindata-etime, a0_maindata-aename, a0_maindata-aedat, a0_maindata-aetime.
  endmethod.


  method FROM_DATA.
  DATA s_txt     LIKE LINE OF texts.
  DATA t_docs    TYPE tt_docs.
  DATA s_value   LIKE LINE OF values.
  DATA s_scr     LIKE LINE OF screens.
  FIELD-SYMBOLS:
    <s>  LIKE LINE OF fm_data-screens,
    <t>  LIKE LINE OF fm_data-texts.

  a0_maindata = fm_data-header.
  values = fm_data-params.
  MODIFY values FROM s_value TRANSPORTING olen  FROM  to  text kind WHERE name <> space OR name = space.
* Event or other stuff might be present in VNAME when no TVARVC is used => Clean it
  MODIFY values FROM s_value TRANSPORTING vname WHERE vtype = space.

  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_txt.    s_txt-lang = <t>-langu.   s_txt-text = <t>-vtext.
    INSERT s_txt INTO TABLE texts.
  ENDLOOP.

  screens = fm_data-screens.
*  LOOP AT fm_data-screens ASSIGNING <s>.
*    CLEAR s_scr.    s_scr-dynnr = <s>-dynnr.
*    INSERT s_scr INTO TABLE screens.
*  ENDLOOP.
  endmethod.


  method KEY_2_NAME.
  result = data.
  endmethod.


  method NAME_2_KEY.
  result = data.
  endmethod.


  method TO_DATA.
  DATA s_txt     LIKE LINE OF fm_data-texts.
  data s_scr     like LINE OF fm_data-screens.
  FIELD-SYMBOLS:
    <s>  like LINE OF screens,
    <t>  LIKE LINE OF texts.

  fm_data-header = a0_maindata.
  fm_data-params = values.

  LOOP AT texts ASSIGNING <t>.
    CLEAR s_txt.    s_txt-mandt = a0_maindata-mandt.    s_txt-report = a0_maindata-report.    s_txt-variant = a0_maindata-variant.    s_txt-langu = <t>-lang.   s_txt-vtext = <t>-text.
    INSERT s_txt INTO TABLE fm_data-texts.
  ENDLOOP.
  fm_data-screens = screens.
*  LOOP AT screens ASSIGNING <s>.
*    CLEAR s_scr.    " s_scr-mandt = a0_maindata-mandt.    s_scr-report = a0_maindata-report.    s_scr-variant = a0_maindata-variant.
*    s_scr-dynnr = <s>-dynnr.
*    INSERT s_scr INTO TABLE fm_data-screens.
*  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  a0_maindata-ename = a0_maindata-aename = sy-uname.
  endmethod.
ENDCLASS.
