class ZAPLINK_FUGR_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_FUNCTION_GROUP .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TT_ABAPRAWSOURCE
    for ZAPLINK_DATATYPES~TT_ABAPRAWSOURCE .
  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_DYNPROS
    for ZAPLINK_DATATYPES~TT_DYNPROS .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .

  class-methods CLASS_CONSTRUCTOR .
protected section.

  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .

  types:
    tt_fm_data TYPE STANDARD TABLE OF zaplink_func_data=>ts_fm_data WITH DEFAULT KEY .
  types:
    BEGIN OF ts_doc_param,
      func 	TYPE rs38l_fnam,
      param TYPE rs38l_par_,
    END OF ts_doc_param .
  types T_RAW type RSWSOURCET .
  types TO_MENUPAINTER type ref to ZAPLINK_MENUPAINTER_DATA .
  types:
    tt_tlibt TYPE SORTED TABLE OF tlibt WITH UNIQUE KEY spras .
  types:
    tt_tftit TYPE SORTED TABLE OF tftit WITH UNIQUE KEY funcname spras .
  types:
    tt_funct TYPE SORTED TABLE OF funct WITH UNIQUE KEY funcname parameter kind spras .
  types:
    tt_rsimp TYPE STANDARD TABLE OF rsimp WITH DEFAULT KEY .
  types:
    tt_rscha TYPE STANDARD TABLE OF rscha WITH DEFAULT KEY .
  types:
    tt_rsexp TYPE STANDARD TABLE OF rsexp WITH DEFAULT KEY .
  types:
    tt_rstbl TYPE STANDARD TABLE OF rstbl WITH DEFAULT KEY .
  types:
    tt_rsexc TYPE STANDARD TABLE OF rsexc WITH DEFAULT KEY .
  types:
    BEGIN OF ts_source,
            _ TYPE string,
          END OF ts_source .
  types TD_INCLUDENAME type D010INC-INCLUDE .
  types:
    BEGIN OF ts_include,
      name   TYPE td_includename,
      source TYPE ts_source,
    END OF ts_include .
  types:
    tt_includes TYPE SORTED TABLE OF ts_include WITH UNIQUE KEY name .
  types:
    BEGIN OF ts_rawinclude,
      short  TYPE rs38l-suffix,   " Short name TOP, F01
      name   TYPE td_includename,
      source TYPE tt_abaprawsource,
    END OF ts_rawinclude .
  types:
    tt_rawincludes TYPE STANDARD TABLE OF ts_rawinclude WITH NON-UNIQUE KEY name .
  types TS_FUNC type ZAPLINK_FUNC_DATA=>TS_FM_DATA .
  types:
    tt_funcs TYPE SORTED TABLE OF ts_func WITH UNIQUE KEY header-name .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
                header          TYPE tlibg,
                texts           TYPE tt_tlibt,
                docs            TYPE tt_docs,
                textspool	      TYPE tt_txtp_textpools,
                dynpros	        TYPE tt_dynpros,
                menupainter	    TYPE to_menupainter,
                includes        TYPE tt_rawincludes,
                functions       TYPE tt_funcs,
*                function_texts  TYPE tt_tftit,             " Issue 100
*                param_texts     TYPE tt_funct,             " Issue 100
              END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
  INCLUDE TYPE tlibg AS hdr.
  TYPES:
      texts       TYPE tt_texts,
    END OF ts_maindata .
  types:
    BEGIN OF ts_textpool,
          id    TYPE  textpoolid,
          key	  TYPE  textpoolky,
          texts TYPE  tt_texts,
        END OF ts_textpool .
  types:
    tt_textpools TYPE SORTED TABLE OF ts_textpool WITH UNIQUE KEY id KEY .
  types TD_PARAMKIND type KIND .
  types:
    BEGIN OF ts_param,
      kind TYPE td_paramkind.
  INCLUDE TYPE rsimp AS hdr.
  TYPES:
      texts       TYPE tt_texts,
    END OF ts_param .
  types:
    tt_params TYPE SORTED TABLE OF ts_param WITH UNIQUE KEY parameter .
  types:
    BEGIN OF ts_excep.
  INCLUDE TYPE rsexc AS hdr.
  TYPES:
      texts       TYPE tt_texts,
    END OF ts_excep .
  types:
    tt_exceptions TYPE SORTED TABLE OF ts_excep WITH UNIQUE KEY exception .
  types TO_FUNCTION type ref to ZAPLINK_FUNC_RAW .
  types:
    BEGIN OF ts_function,
      zl_object TYPE to_function,
      END OF ts_function .
  types:
    tt_functions TYPE STANDARD TABLE OF ts_function WITH DEFAULT KEY .

  constants:
    BEGIN OF parameter_kinds,
      importing   TYPE td_paramkind VALUE 'I',
      changing    TYPE td_paramkind VALUE 'C',
      exporting   TYPE td_paramkind VALUE 'E',
      table       TYPE td_paramkind VALUE 'T',
*      exceptions  type td_paramkind value 'X',
    END OF parameter_kinds .
  data A0_MAINDATA type TS_MAINDATA .
  data TEXTSPOOL type TT_TXTP_TEXTPOOLS .
  data SOURCE type TS_SOURCE .
  data DYNPROS type TT_DYNPROS .
  data MENUPAINTER type TO_MENUPAINTER .
  data FUNCTIONS type TT_FUNCTIONS .
  data INCLUDES type TT_INCLUDES .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA
    raising
      ZAPLINK_CX .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
  class-methods CONV_FUNCTION
    importing
      !FM_DATA type TS_FM_DATA
    returning
      value(RESULT) type TT_FM_DATA .
private section.

  constants:
    BEGIN OF markups,
      area TYPE string VALUE '[FUNCTIONGROUP NAME]',
    END OF markups .
  constants:
    BEGIN OF suffixes,
      top TYPE rs38l-suffix VALUE 'TOP',
    END OF suffixes .
  constants:
    BEGIN OF DOC_IDS,
      group     TYPE td_doc_id VALUE 'RE',    " Report
      function  TYPE td_doc_id VALUE 'FU',
    END OF DOC_IDS .
  class-data:
    BEGIN OF R_DOC_IDS,
      group    TYPE TR_DOCID,
      function TYPE TR_DOCID,
    END OF R_DOC_IDS .
ENDCLASS.



CLASS ZAPLINK_FUGR_DATA IMPLEMENTATION.


  method ANONYMIZE.
  IF menupainter IS BOUND.    menupainter->anonymize( ).    ENDIF.
  endmethod.


  method CLASS_CONSTRUCTOR.
*'FU'
*table TDCLT
*DOKCLASS DOKTITEL
*FU       Function
*RE       Report/Function Group, Logical DB                            Program/module pool
  DATA _id LIKE LINE OF r_doc_ids-group.

  _id-sign = 'I'.
  _id-option = 'EQ'.
  _id-low = doc_ids-group.      APPEND _id TO r_doc_ids-group.
  _id-low = doc_ids-function.   APPEND _id TO r_doc_ids-function.
  endmethod.


  method CONV_FUNCTION.
  DATA s_func LIKE LINE OF result.
  FIELD-SYMBOLS <f> LIKE LINE OF fm_data-functions.
  LOOP AT fm_data-functions ASSIGNING <f>.
    CLEAR s_func.   MOVE-CORRESPONDING <f> TO s_func. " header, import_p, changing_p, export_p, tables, exceptions, source.
    s_func-docs = fm_data-docs.
*    s_func-texts = fm_data-function_texts.    DELETE s_func-texts WHERE funcname <> <f>-header-name.             " Issue 100
*    s_func-param_texts = fm_data-param_texts.    DELETE s_func-param_texts WHERE funcname <> <f>-header-name.    " Issue 100
    APPEND s_func TO result.
  ENDLOOP.
  SORT result BY header-name.
  endmethod.


  method FROM_DATA.
  DATA s_func    LIKE LINE OF functions.
  DATA s_txt     LIKE LINE OF a0_maindata-texts.
*  DATA s_f_txt   LIKE LINE OF s_func-texts.
*  DATA s_param   LIKE LINE OF s_func-parameters.
*  DATA s_excep   LIKE LINE OF s_func-exceptions.
  DATA d_suffix  TYPE rs38l-suffix.
  DATA d_group   TYPE rs38l-area.
  DATA s_inc     LIKE LINE OF includes.
  DATA t_abapsrc TYPE tt_abaprawsource.
  DATA d_search  TYPE string.
  DATA d_replace TYPE string.
  DATA d_lcount  TYPE i.
  DATA d_doc     TYPE doku_obj.
  DATA d_doc_p   TYPE ts_doc_param.
  DATA t_docs    TYPE tt_docs.
  DATA t_funcs TYPE tt_fm_data.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t>  LIKE LINE OF fm_data-texts,
*    <ft> LIKE LINE OF fm_data-function_texts,
    <f>  LIKE LINE OF fm_data-functions,        " Issue 100
    <i>  LIKE LINE OF fm_data-includes,
    <s>  LIKE LINE OF <i>-source,
    <im> LIKE LINE OF <f>-import_p,
    <ch> LIKE LINE OF <f>-changing_p,
    <ex> LIKE LINE OF <f>-export_p,
*    <pa> LIKE LINE OF s_func-parameters,
*    <pt> LIKE LINE OF fm_data-param_texts,
    <cx> LIKE LINE OF <f>-exceptions,
    <ta> LIKE LINE OF <f>-tables.

  a0_maindata-hdr = fm_data-header.
  textspool = fm_data-textspool.
  dynpros = fm_data-dynpros.
  menupainter = fm_data-menupainter.

  d_doc = zaplink_function_group=>get_mainprogram( fm_data-header-area ).
  t_docs = fm_data-docs.
  READ TABLE t_docs ASSIGNING <d>
       WITH KEY id = doc_ids-group
            object = d_doc.
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_txt.    s_txt-langu = <t>-spras.   s_txt-short_txt = <t>-areat.
    IF <d> IS ASSIGNED.
      READ TABLE <d>-texts ASSIGNING <dt>
          WITH KEY tdspras = s_txt-langu.
      IF sy-subrc = 0.    s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.    ENDIF.
    ENDIF.
    INSERT s_txt INTO TABLE a0_maindata-texts.
  ENDLOOP.
  IF <d> IS ASSIGNED.
    LOOP AT <d>-texts ASSIGNING <dt>.
      CLEAR s_txt.    s_txt-langu = <dt>-tdspras.   s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    INSERT s_txt INTO TABLE a0_maindata-texts.
    ENDLOOP.
  ENDIF.

  LOOP AT fm_data-includes ASSIGNING <i>.
    CLEAR s_inc.    s_inc-name = <i>-name.            t_abapsrc = <i>-source.
    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
*     EXPORTING
*       PROGRAM                            =
*       SUPPRESS_SELECT                    = 'X'
*       COMPLETE_AREA                      = ' '
      IMPORTING
*       NAMESPACE                          =
*       FUNCTION_NOT_EXISTS                =
        group                              = d_group
*       FUNCNAME                           =
*       INCLUDE_NUMBER                     =
*       NO_FUNCTION_INCLUDE                =
*       NO_FUNCTION_MODULE                 =
        suffix                             = d_suffix
*       RESERVED_NAME                      =
*       TOO_MANY_DELIMITERS                =
*       RESERVED_FOR_EXITS                 =
*       HIDDEN_NAME                        =
      CHANGING
        include                            = s_inc-name
      EXCEPTIONS
        include_not_exists                 = 1
        group_not_exists                   = 2
        no_selections                      = 3
        no_function_include                = 4
        no_function_pool                   = 5
        delimiter_wrong_position           = 6
        no_customer_function_group         = 7
        no_customer_function_include       = 8
        reserved_name_customer             = 9
        namespace_too_long                 = 10
        area_length_error                  = 11
        OTHERS                             = 12.
    IF sy-subrc = 0 AND d_group = a0_maindata-area.
      s_inc-name = d_suffix.
      IF s_inc-name = suffixes-top AND NOT <i>-source IS INITIAL.
*FUNCTION-POOL SVIM MESSAGE-ID SV.
        READ TABLE t_abapsrc ASSIGNING <s> INDEX 1.
        TRANSLATE <s> TO UPPER CASE.                      "#EC SYNTCHAR
        IF <s> CP 'FUNCTION-POOL *'.
          CONCATENATE space a0_maindata-area space INTO d_search.   TRANSLATE d_search TO UPPER CASE.
          CONCATENATE space markups-area space INTO d_replace.
          REPLACE FIRST OCCURRENCE OF d_search IN <s> WITH d_replace.
        ENDIF.
      ENDIF.
    ENDIF.
    s_inc-source-_ = zaplink_function_group=>conv_abap_raw2source( t_abapsrc ).
    INSERT s_inc INTO TABLE includes.
  ENDLOOP.

* Functions
*  t_funcs = conv_function( fm_data ).          " Issue 100
  LOOP AT fm_data-functions ASSIGNING <f>.      " Issue 100
    CLEAR s_func.
    CREATE OBJECT s_func-zl_object EXPORTING fm_data = <f>.
    APPEND s_func TO functions.
  ENDLOOP.

*  LOOP AT fm_data-functions ASSIGNING <f>.
*    CLEAR s_func.   MOVE-CORRESPONDING <f>-header TO s_func.
** Source code : remove FUNCTION XXX & ENDFUNCTION.
*    IF NOT <f>-source IS INITIAL.
*      t_abapsrc = <f>-source.
*      READ TABLE t_abapsrc ASSIGNING <s> INDEX 1.
*      TRANSLATE <s> TO UPPER CASE.                        "#EC SYNTCHAR
*      IF <s> CP 'FUNCTION *.'.    DELETE t_abapsrc INDEX 1.   ENDIF.
*      d_lcount = LINES( t_abapsrc ).
*      READ TABLE t_abapsrc ASSIGNING <s> INDEX d_lcount.
*      TRANSLATE <s> TO UPPER CASE.                        "#EC SYNTCHAR
*      IF <s> CP 'ENDFUNCTION*.'.    DELETE t_abapsrc INDEX d_lcount.   ENDIF.
*      s_func-source-_ = zaplink_function_group=>conv_abap_raw2source( t_abapsrc ).
*    ENDIF.
*
*    UNASSIGN <d>.
*    READ TABLE t_docs ASSIGNING <d>
*         WITH KEY id = doc_ids-function
*              object = s_func-name.
*    LOOP AT fm_data-function_texts ASSIGNING <ft>
*            WHERE funcname = s_func-name.
*      CLEAR s_f_txt.    s_f_txt-langu = <ft>-spras.   s_f_txt-short_txt = <ft>-stext.
*      IF <d> IS ASSIGNED.
*        READ TABLE <d>-texts ASSIGNING <dt>
*            WITH KEY tdspras = s_f_txt-langu.
*        IF sy-subrc = 0.    s_f_txt-long_txt = <dt>.   CLEAR s_f_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.   ENDIF.
*      ENDIF.
*      INSERT s_f_txt INTO TABLE s_func-texts.
*    ENDLOOP.
*    IF <d> IS ASSIGNED.
*      LOOP AT <d>-texts ASSIGNING <dt>.
*        CLEAR s_f_txt.    s_f_txt-langu = <dt>-tdspras.   s_f_txt-long_txt = <dt>.   CLEAR s_f_txt-long_txt-tdspras.    INSERT s_f_txt INTO TABLE s_func-texts.
*      ENDLOOP.
*    ENDIF.
*
*    LOOP AT <f>-import_p ASSIGNING <im>.
*      CLEAR s_param.    s_param-hdr = <im>.    s_param-kind = parameter_kinds-importing.   INSERT s_param INTO TABLE s_func-parameters.
*    ENDLOOP.
*    LOOP AT <f>-changing_p ASSIGNING <ch>.
*      CLEAR s_param.    s_param-hdr = <ch>.    s_param-kind = parameter_kinds-changing.   INSERT s_param INTO TABLE s_func-parameters.
*    ENDLOOP.
*    LOOP AT <f>-export_p ASSIGNING <ex>.
*      CLEAR s_param.    MOVE-CORRESPONDING <ex> TO s_param-hdr.    s_param-kind = parameter_kinds-exporting.   INSERT s_param INTO TABLE s_func-parameters.
*    ENDLOOP.
*    LOOP AT <f>-tables ASSIGNING <ta>.
*      CLEAR s_param.    MOVE-CORRESPONDING <ta> TO s_param-hdr.    s_param-dbfield = <ta>-dbstruct.   s_param-kind = parameter_kinds-table.   INSERT s_param INTO TABLE s_func-parameters.
*    ENDLOOP.
*    d_doc_p-func = s_func-name.
*    LOOP AT s_func-parameters ASSIGNING <pa>.
*      d_doc_p-param = <pa>-parameter.   UNASSIGN <d>.
*      READ TABLE t_docs ASSIGNING <d>
*           WITH KEY id = doc_ids-function
*                object = d_doc_p.
*      LOOP AT fm_data-param_texts ASSIGNING <pt>
*           WHERE funcname = s_func-name
*             AND parameter = <pa>-parameter
*             AND kind = 'P'.
*        CLEAR s_f_txt.    s_f_txt-langu = <pt>-spras.   s_f_txt-short_txt = <pt>-stext.
*        IF <d> IS ASSIGNED.
*          READ TABLE <d>-texts ASSIGNING <dt>
*              WITH KEY tdspras = s_f_txt-langu.
*          IF sy-subrc = 0.    s_f_txt-long_txt = <dt>.   CLEAR s_f_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.   ENDIF.
*        ENDIF.
*        INSERT s_f_txt INTO TABLE <pa>-texts.
*      ENDLOOP.
*      IF <d> IS ASSIGNED.
*        LOOP AT <d>-texts ASSIGNING <dt>.
*          CLEAR s_f_txt.    s_f_txt-langu = <dt>-tdspras.   s_f_txt-long_txt = <dt>.   CLEAR s_f_txt-long_txt-tdspras.    INSERT s_f_txt INTO TABLE <pa>-texts.
*        ENDLOOP.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT <f>-exceptions ASSIGNING <cx>.
*      CLEAR s_excep.    s_excep-hdr = <cx>.
*      LOOP AT fm_data-param_texts ASSIGNING <pt>
*           WHERE funcname = s_func-name
*             AND parameter = <cx>-exception
*             AND kind = 'X'.
*        CLEAR s_f_txt.    s_f_txt-langu = <pt>-spras.   s_f_txt-short_txt = <pt>-stext.    INSERT s_f_txt INTO TABLE s_excep-texts.
*      ENDLOOP.
*      INSERT s_excep INTO TABLE s_func-exceptions.
*    ENDLOOP.
*
*    INSERT s_func INTO TABLE functions.
*  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_func    LIKE LINE OF fm_data-functions.
  DATA s_line    LIKE LINE OF s_func-source.
  DATA s_txt     LIKE LINE OF fm_data-texts.
*  DATA s_f_txt   LIKE LINE OF fm_data-function_texts.
  DATA s_p_im    LIKE LINE OF s_func-import_p.
  DATA s_p_ch    LIKE LINE OF s_func-changing_p.
  DATA s_p_ex    LIKE LINE OF s_func-export_p.
  DATA s_p_ta    LIKE LINE OF s_func-tables.
*  DATA s_p_txt   LIKE LINE OF fm_data-param_texts.
  DATA s_excep   LIKE LINE OF s_func-exceptions.
  DATA d_suffix  TYPE rs38l-suffix.
  DATA d_group   TYPE rs38l-area.
  DATA s_inc     LIKE LINE OF fm_data-includes.
  DATA d_search  TYPE string.
  DATA d_replace TYPE string.
  DATA d_lcount  TYPE i.
  DATA s_doc     LIKE LINE OF fm_data-docs.
  DATA s_doc_txt LIKE LINE OF s_doc-texts.
  DATA d_doc_p   TYPE ts_doc_param.
  FIELD-SYMBOLS:
    <t>  LIKE LINE OF a0_maindata-texts,
    <i>  LIKE LINE OF includes,
    <f>  LIKE LINE OF functions,
*    <ft> LIKE LINE OF <f>-texts,
*    <pa> LIKE LINE OF <f>-parameters,
*    <cx> LIKE LINE OF <f>-exceptions,
*    <pt> LIKE LINE OF <pa>-texts,
    <s>  LIKE LINE OF s_inc-source.

  fm_data-header = a0_maindata-hdr.
  fm_data-textspool = textspool.
  fm_data-dynpros = dynpros.
  fm_data-menupainter = menupainter.

*APPLICATION                                      DOKU
*ID	                                   	RE
*OBJECT	                                   	SAPLxxxx
*LANGU                                      E
*MASTERLANG	                                   	X
*TYP                                      E
*DOKFORM                                      S_DOCU_SHOW
*DOKSTYLE	                                   	S_DOCUS1
  s_doc-application = 'DOKU'.   s_doc-id = doc_ids-group.   s_doc-object = zaplink_function_group=>get_mainprogram( fm_data-header-area ).
  s_doc-masterlang = abap_true.   s_doc-typ = 'E'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
  LOOP AT a0_maindata-texts ASSIGNING <t>.
    IF NOT <t>-short_txt IS INITIAL.    CLEAR s_txt.    s_txt-spras = <t>-langu.   s_txt-areat = <t>-short_txt.   INSERT s_txt INTO TABLE fm_data-texts.    ENDIF.
    IF NOT <t>-long_txt IS INITIAL.
      CLEAR s_doc_txt.    s_doc_txt = <t>-long_txt.   s_doc_txt-tdspras = <t>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
      IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.

  LOOP AT includes ASSIGNING <i>.
    CLEAR s_inc.   d_suffix = s_inc-name = s_inc-short = <i>-name.
    s_inc-source = zaplink_function_group=>conv_abap_source2raw( <i>-source-_ ).
    IF d_suffix = s_inc-name.   " include has been truncated
      CONCATENATE 'L' a0_maindata-area d_suffix INTO s_inc-name.
      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
*     EXPORTING
*       PROGRAM                            =
*       SUPPRESS_SELECT                    = 'X'
*       COMPLETE_AREA                      = ' '
        IMPORTING
*       NAMESPACE                          =
*       FUNCTION_NOT_EXISTS                =
          group                              = d_group
*       FUNCNAME                           =
*       INCLUDE_NUMBER                     =
*       NO_FUNCTION_INCLUDE                =
*       NO_FUNCTION_MODULE                 =
          suffix                             = d_suffix
*       RESERVED_NAME                      =
*       TOO_MANY_DELIMITERS                =
*       RESERVED_FOR_EXITS                 =
*       HIDDEN_NAME                        =
        CHANGING
          include                            = s_inc-name
        EXCEPTIONS
          include_not_exists                 = 1
          group_not_exists                   = 2
          no_selections                      = 3
          no_function_include                = 4
          no_function_pool                   = 5
          delimiter_wrong_position           = 6
          no_customer_function_group         = 7
          no_customer_function_include       = 8
          reserved_name_customer             = 9
          namespace_too_long                 = 10
          area_length_error                  = 11
          OTHERS                             = 12.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF d_group <> a0_maindata-area.   CONTINUE.   ENDIF.    " Internal error
      IF d_suffix = suffixes-top AND NOT s_inc-source IS INITIAL.
*FUNCTION-POOL SVIM MESSAGE-ID SV.
        READ TABLE s_inc-source ASSIGNING <s> INDEX 1.
        TRANSLATE <s> TO UPPER CASE.                      "#EC SYNTCHAR
        IF <s> CP 'FUNCTION-POOL *'.
          CONCATENATE space markups-area space INTO d_search.   TRANSLATE d_search TO UPPER CASE.
          CONCATENATE space a0_maindata-area space INTO d_replace.
          REPLACE FIRST OCCURRENCE OF d_search IN <s> WITH d_replace.
        ENDIF.
      ENDIF.
    ENDIF.
    INSERT s_inc INTO TABLE fm_data-includes.
  ENDLOOP.

  LOOP AT functions ASSIGNING <f>.      " previously missing => No function imported :-(
    s_func = <f>-zl_object->to_data( ).
    s_func-header-str_area = fm_data-header-area.
    INSERT s_func INTO TABLE fm_data-functions.
  ENDLOOP.
*  LOOP AT functions ASSIGNING <f>.
*    CLEAR s_func.   MOVE-CORRESPONDING <f> TO s_func-header.
** Source code : remove FUNCTION XXX & ENDFUNCTION.
*    IF NOT <f>-source-_ IS INITIAL.
*      s_func-source = zaplink_function_group=>conv_abap_source2raw( <f>-source-_ ).
*      READ TABLE s_func-source ASSIGNING <s> INDEX 1.   TRANSLATE <s> TO UPPER CASE. "#EC SYNTCHAR
*      IF NOT <s> CP 'FUNCTION *.'.    CONCATENATE `FUNCTION ` <f>-name '.' INTO s_line.   INSERT s_line INTO s_func-source INDEX 1.   ENDIF.
*      d_lcount = LINES( s_func-source ).    READ TABLE s_func-source ASSIGNING <s> INDEX d_lcount.    TRANSLATE <s> TO UPPER CASE. "#EC SYNTCHAR
*      IF NOT <s> CP 'ENDFUNCTION*.'.    APPEND 'ENDFUNCTION.' TO s_func-source.   ENDIF.
*    ENDIF.
*
**APPLICATION                                      DOKU
**ID                                      FU
**OBJECT                                      xxxxx
**LANGU                                      F
**MASTERLANG                                      X
**TYP                                      T
**DOKFORM                                      S_DOCU_SHOW
**DOKSTYLE                                      S_DOCUS1
*    CLEAR s_doc.    s_doc-application = 'DOKU'.   s_doc-id = doc_ids-function.   s_doc-object = <f>-name.
*    s_doc-masterlang = abap_true.   s_doc-typ = 'T'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
*    LOOP AT <f>-texts ASSIGNING <ft>.
*      IF NOT <ft>-short_txt IS INITIAL.    CLEAR s_f_txt.    s_f_txt-spras = <ft>-langu.   s_f_txt-stext = <ft>-short_txt.   s_f_txt-funcname = <f>-name.   INSERT s_f_txt INTO TABLE fm_data-function_texts.    ENDIF.
*      IF NOT <ft>-long_txt IS INITIAL.
*        CLEAR s_doc_txt.    s_doc_txt = <ft>-long_txt.   s_doc_txt-tdspras = <ft>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
*        IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
*      ENDIF.
*    ENDLOOP.
*    IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.
*
*    LOOP AT <f>-parameters ASSIGNING <pa>.
*      CASE <pa>-kind.
*        WHEN parameter_kinds-importing.
*          CLEAR s_p_im.   s_p_im = <pa>-hdr.    INSERT s_p_im INTO TABLE s_func-import_p.
*        WHEN parameter_kinds-changing.
*          CLEAR s_p_ch.   s_p_ch = <pa>-hdr.    INSERT s_p_ch INTO TABLE s_func-changing_p.
*        WHEN parameter_kinds-exporting.
*          CLEAR s_p_ex.   MOVE-CORRESPONDING <pa>-hdr TO s_p_ex.    INSERT s_p_ex INTO TABLE s_func-export_p.
*        WHEN parameter_kinds-table.
*          CLEAR s_p_ta.   MOVE-CORRESPONDING <pa>-hdr TO s_p_ta.    s_p_ta-dbstruct = <pa>-dbfield.    INSERT s_p_ta INTO TABLE s_func-tables.
*        WHEN OTHERS.
*      ENDCASE.
**APPLICATION                                      DOKU
**ID                                      FU
**OBJECT                                      xxxxx yyy
**LANGU                                      F
**MASTERLANG                                      X
**TYP                                      T
**DOKFORM                                      S_DOCU_SHOW
**DOKSTYLE                                      S_DOCUS1
*      CLEAR: d_doc_p, s_doc.    d_doc_p-func = <f>-name.    d_doc_p-param = <pa>-parameter.    s_doc-application = 'DOKU'.   s_doc-id = doc_ids-function.
*      s_doc-object = d_doc_p.   s_doc-masterlang = abap_true.   s_doc-typ = 'T'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
*      LOOP AT <pa>-texts ASSIGNING <pt>.
*        IF NOT <pt>-short_txt IS INITIAL.
*          CLEAR s_p_txt. s_p_txt-spras = <pt>-langu. s_f_txt-stext = <pt>-short_txt. s_p_txt-funcname = <f>-name. s_p_txt-parameter = <pa>-parameter. s_p_txt-kind = 'P'. INSERT s_p_txt INTO TABLE fm_data-param_texts.
*        ENDIF.
*        IF NOT <pt>-long_txt IS INITIAL.
*          CLEAR s_doc_txt.    s_doc_txt = <pt>-long_txt.   s_doc_txt-tdspras = <pt>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
*          IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
*        ENDIF.
*      ENDLOOP.
*      IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.
*    ENDLOOP.
*
*    LOOP AT <f>-exceptions ASSIGNING <cx>.
*      CLEAR s_excep.    s_excep = <cx>-hdr.
*
*      LOOP AT <cx>-texts ASSIGNING <pt>.
*        CLEAR s_p_txt.    s_p_txt-spras = <pt>-langu.   s_f_txt-stext = <pt>-short_txt.   s_p_txt-funcname = <f>-name.    s_p_txt-parameter = <cx>-exception.   s_p_txt-kind = 'X'.    INSERT s_p_txt INTO TABLE fm_data-param_texts.
*      ENDLOOP.
*      INSERT s_excep INTO TABLE s_func-exceptions.
*    ENDLOOP.
*
*    INSERT s_func INTO TABLE fm_data-functions.
*  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
