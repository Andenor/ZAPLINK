class ZAPLINK_FUNC_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_FUGR_DATA
                 ZAPLINK_FUNCTION_GROUP
                 ZAPLINK_FUNC_RAW .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases LINE_SEPARATOR
    for ZAPLINK_DATATYPES~LINE_SEPARATOR .
  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .
  aliases TD_CHECKSUM
    for ZAPLINK_DATATYPES~TD_CHECKSUM .
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
      name   TYPE td_includename,
      source TYPE tt_abaprawsource,
    END OF ts_rawinclude .
  types:
    tt_rawincludes TYPE STANDARD TABLE OF ts_rawinclude WITH NON-UNIQUE KEY name .
  types:
    BEGIN OF ts_fm_data,
* DO pour type doc
                header          TYPE rs38l,
                code_signature  TYPE td_checksum,
                docs            TYPE tt_docs,
                import_p        TYPE tt_rsimp,
                changing_p      TYPE tt_rscha,
                export_p        TYPE tt_rsexp,
                tables          TYPE tt_rstbl,
                exceptions      TYPE tt_rsexc,
                source          TYPE tt_abaprawsource,
                texts           TYPE tt_tftit,
                param_texts     TYPE tt_funct,
              END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata,
      name        TYPE rs38l-name,
      global      TYPE rs38l-global,
      remote      TYPE rs38l-remote,
      utask       TYPE rs38l-utask,
      str_area    TYPE rs38l-str_area,
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
  types:
    BEGIN OF ts_function,
      name        TYPE rs38l-name,
      global      TYPE rs38l-global,
      remote      TYPE rs38l-remote,
      utask       TYPE rs38l-utask,
      texts       TYPE tt_texts,
      parameters  TYPE tt_params,
      exceptions  TYPE tt_exceptions,
      source      TYPE ts_source,
    END OF ts_function .
  types:
    tt_functions TYPE SORTED TABLE OF ts_function WITH UNIQUE KEY name .

  constants:
    BEGIN OF parameter_kinds,
      importing   TYPE td_paramkind VALUE 'I',
      changing    TYPE td_paramkind VALUE 'C',
      exporting   TYPE td_paramkind VALUE 'E',
      table       TYPE td_paramkind VALUE 'T',
*      exceptions  type td_paramkind value 'X',
    END OF parameter_kinds .
  data A0_MAINDATA type TS_MAINDATA .
  data SOURCE type TS_SOURCE .
  data TEXTS type TT_TEXTS .
  data PARAMETERS type TT_PARAMS .
  data EXCEPTIONS type TT_EXCEPTIONS .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
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



CLASS ZAPLINK_FUNC_DATA IMPLEMENTATION.


  method ANONYMIZE.
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


  method FROM_DATA.
  DATA s_txt     LIKE LINE OF texts.
  DATA s_param   LIKE LINE OF parameters.
  DATA s_excep   LIKE LINE OF exceptions.
  DATA d_suffix  TYPE rs38l-suffix.
  DATA d_group   TYPE rs38l-area.
  DATA t_abapsrc TYPE tt_abaprawsource.
  DATA d_search  TYPE string.
  DATA d_replace TYPE string.
  DATA d_lcount  TYPE i.
  DATA d_doc     TYPE doku_obj.
  DATA d_doc_p   TYPE ts_doc_param.
  DATA t_docs    TYPE tt_docs.
  FIELD-SYMBOLS:
    <d>  LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t>  LIKE LINE OF fm_data-texts,
    <s>  LIKE LINE OF fm_data-source,
    <im> LIKE LINE OF fm_data-import_p,
    <ch> LIKE LINE OF fm_data-changing_p,
    <ex> LIKE LINE OF fm_data-export_p,
    <pa> LIKE LINE OF parameters,
    <pt> LIKE LINE OF fm_data-param_texts,
    <cx> LIKE LINE OF fm_data-exceptions,
    <ta> LIKE LINE OF fm_data-tables.

  MOVE-CORRESPONDING fm_data-header TO a0_maindata.
  _code_signature = fm_data-code_signature.

*  d_doc = zaplink_function_group=>get_mainprogram( fm_data-header-area ).
*  t_docs = fm_data-docs.
*  READ TABLE t_docs ASSIGNING <d>
*       WITH KEY id = doc_ids-group
*            object = d_doc.
*  LOOP AT fm_data-texts ASSIGNING <t>.
*    CLEAR s_txt.    s_txt-langu = <t>-spras.   s_txt-short_txt = <t>-areat.
*    IF <d> IS ASSIGNED.
*      READ TABLE <d>-texts ASSIGNING <dt>
*          WITH KEY tdspras = s_txt-langu.
*      IF sy-subrc = 0.    s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.    ENDIF.
*    ENDIF.
*    INSERT s_txt INTO TABLE a0_maindata-texts.
*  ENDLOOP.
*  IF <d> IS ASSIGNED.
*    LOOP AT <d>-texts ASSIGNING <dt>.
*      CLEAR s_txt.    s_txt-langu = <dt>-tdspras.   s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    INSERT s_txt INTO TABLE a0_maindata-texts.
*    ENDLOOP.
*  ENDIF.

* Source code : remove FUNCTION XXX & ENDFUNCTION.
  IF NOT fm_data-source IS INITIAL.
    t_abapsrc = fm_data-source.
    READ TABLE t_abapsrc ASSIGNING <s> INDEX 1.
    TRANSLATE <s> TO UPPER CASE.                          "#EC SYNTCHAR
    IF <s> CP 'FUNCTION *.'.    DELETE t_abapsrc INDEX 1.   ENDIF.
    d_lcount = LINES( t_abapsrc ).
    READ TABLE t_abapsrc ASSIGNING <s> INDEX d_lcount.
    TRANSLATE <s> TO UPPER CASE.                          "#EC SYNTCHAR
    IF <s> CP 'ENDFUNCTION*.'.    DELETE t_abapsrc INDEX d_lcount.   ENDIF.
    source-_ = zaplink_function_group=>conv_abap_raw2source( t_abapsrc ).
  ENDIF.

  UNASSIGN <d>.
  READ TABLE t_docs ASSIGNING <d>
       WITH KEY id = doc_ids-function
            object = a0_maindata-name.
  LOOP AT fm_data-texts ASSIGNING <t>
          WHERE funcname = a0_maindata-name.
    CLEAR s_txt.    s_txt-langu = <t>-spras.   s_txt-short_txt = <t>-stext.
    IF <d> IS ASSIGNED.
      READ TABLE <d>-texts ASSIGNING <dt>
          WITH KEY tdspras = s_txt-langu.
      IF sy-subrc = 0.    s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.   ENDIF.
    ENDIF.
    INSERT s_txt INTO TABLE texts.
  ENDLOOP.
  IF <d> IS ASSIGNED.
    LOOP AT <d>-texts ASSIGNING <dt>.
      CLEAR s_txt.    s_txt-langu = <dt>-tdspras.   s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    INSERT s_txt INTO TABLE texts.
    ENDLOOP.
  ENDIF.

  LOOP AT fm_data-import_p ASSIGNING <im>.
    CLEAR s_param.    s_param-hdr = <im>.    s_param-kind = parameter_kinds-importing.   INSERT s_param INTO TABLE parameters.
  ENDLOOP.
  LOOP AT fm_data-changing_p ASSIGNING <ch>.
    CLEAR s_param.    s_param-hdr = <ch>.    s_param-kind = parameter_kinds-changing.   INSERT s_param INTO TABLE parameters.
  ENDLOOP.
  LOOP AT fm_data-export_p ASSIGNING <ex>.
    CLEAR s_param.    MOVE-CORRESPONDING <ex> TO s_param-hdr.    s_param-kind = parameter_kinds-exporting.   INSERT s_param INTO TABLE parameters.
  ENDLOOP.
  LOOP AT fm_data-tables ASSIGNING <ta>.
    CLEAR s_param.    MOVE-CORRESPONDING <ta> TO s_param-hdr.    s_param-dbfield = <ta>-dbstruct.   s_param-kind = parameter_kinds-table.   INSERT s_param INTO TABLE parameters.
  ENDLOOP.
  d_doc_p-func = a0_maindata-name.
  LOOP AT parameters ASSIGNING <pa>.
    d_doc_p-param = <pa>-parameter.   UNASSIGN <d>.
    READ TABLE t_docs ASSIGNING <d>
         WITH KEY id = doc_ids-function
              object = d_doc_p.
    LOOP AT fm_data-param_texts ASSIGNING <pt>
         WHERE funcname = a0_maindata-name
           AND parameter = <pa>-parameter
           AND kind = 'P'.
      CLEAR s_txt.    s_txt-langu = <pt>-spras.   s_txt-short_txt = <pt>-stext.
      IF <d> IS ASSIGNED.
        READ TABLE <d>-texts ASSIGNING <dt>
            WITH KEY tdspras = s_txt-langu.
        IF sy-subrc = 0.    s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.   ENDIF.
      ENDIF.
      INSERT s_txt INTO TABLE <pa>-texts.
    ENDLOOP.
    IF <d> IS ASSIGNED.
      LOOP AT <d>-texts ASSIGNING <dt>.
        CLEAR s_txt.    s_txt-langu = <dt>-tdspras.   s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    INSERT s_txt INTO TABLE <pa>-texts.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  LOOP AT fm_data-exceptions ASSIGNING <cx>.
    CLEAR s_excep.    s_excep-hdr = <cx>.
    LOOP AT fm_data-param_texts ASSIGNING <pt>
         WHERE funcname = a0_maindata-name
           AND parameter = <cx>-exception
           AND kind = 'X'.
      CLEAR s_txt.    s_txt-langu = <pt>-spras.   s_txt-short_txt = <pt>-stext.    INSERT s_txt INTO TABLE s_excep-texts.
    ENDLOOP.
    INSERT s_excep INTO TABLE exceptions.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_line    LIKE LINE OF fm_data-source.
  DATA s_txt     LIKE LINE OF fm_data-texts.
  DATA s_f_txt   LIKE LINE OF fm_data-texts.
  DATA s_p_im    LIKE LINE OF fm_data-import_p.
  DATA s_p_ch    LIKE LINE OF fm_data-changing_p.
  DATA s_p_ex    LIKE LINE OF fm_data-export_p.
  DATA s_p_ta    LIKE LINE OF fm_data-tables.
  DATA s_p_txt   LIKE LINE OF fm_data-param_texts.
  DATA s_excep   LIKE LINE OF fm_data-exceptions.
  DATA d_suffix  TYPE rs38l-suffix.
  DATA d_group   TYPE rs38l-area.
*  DATA s_inc     LIKE LINE OF fm_data-includes.
  DATA d_search  TYPE string.
  DATA d_replace TYPE string.
  DATA d_lcount  TYPE i.
  DATA s_doc     LIKE LINE OF fm_data-docs.
  DATA s_doc_txt LIKE LINE OF s_doc-texts.
  DATA d_doc_p   TYPE ts_doc_param.
  FIELD-SYMBOLS:
*    <t>  LIKE LINE OF a0_maindata-texts,
*    <i>  LIKE LINE OF includes,
*    <f>  LIKE LINE OF functions,
    <ft> LIKE LINE OF texts,
    <s>  LIKE LINE OF fm_data-source,
    <pa> LIKE LINE OF parameters,
    <pt> LIKE LINE OF <pa>-texts,
    <cx> LIKE LINE OF exceptions.


  MOVE-CORRESPONDING a0_maindata TO fm_data-header.
* Source code : remove FUNCTION XXX & ENDFUNCTION.
  IF NOT source-_ IS INITIAL.
    fm_data-source = zaplink_function_group=>conv_abap_source2raw( source-_ ).
    READ TABLE fm_data-source ASSIGNING <s> INDEX 1.   TRANSLATE <s> TO UPPER CASE. "#EC SYNTCHAR
    IF NOT <s> CP 'FUNCTION *.'.    CONCATENATE `FUNCTION ` A0_maindata-name '.' INTO s_line.   INSERT s_line INTO fm_data-source INDEX 1.   ENDIF.
    d_lcount = LINES( fm_data-source ).    READ TABLE fm_data-source ASSIGNING <s> INDEX d_lcount.    TRANSLATE <s> TO UPPER CASE. "#EC SYNTCHAR
    IF NOT <s> CP 'ENDFUNCTION*.'.    APPEND 'ENDFUNCTION.' TO fm_data-source.   ENDIF.
  ENDIF.

*APPLICATION                                      DOKU
*ID                                      FU
*OBJECT                                      xxxxx
*LANGU                                      F
*MASTERLANG                                      X
*TYP                                      T
*DOKFORM                                      S_DOCU_SHOW
*DOKSTYLE                                      S_DOCUS1
  CLEAR s_doc.    s_doc-application = 'DOKU'.   s_doc-id = doc_ids-function.   s_doc-object = A0_maindata-name.
  s_doc-masterlang = abap_true.   s_doc-typ = 'T'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
  LOOP AT texts ASSIGNING <ft>.
    IF NOT <ft>-short_txt IS INITIAL.    CLEAR s_f_txt.    s_f_txt-spras = <ft>-langu.   s_f_txt-stext = <ft>-short_txt.   s_f_txt-funcname = A0_maindata-name.   INSERT s_f_txt INTO TABLE fm_data-texts.    ENDIF.
    IF NOT <ft>-long_txt IS INITIAL.
      CLEAR s_doc_txt.    s_doc_txt = <ft>-long_txt.   s_doc_txt-tdspras = <ft>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
      IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.

  LOOP AT parameters ASSIGNING <pa>.
    CASE <pa>-kind.
      WHEN parameter_kinds-importing.
        CLEAR s_p_im.   s_p_im = <pa>-hdr.    INSERT s_p_im INTO TABLE fm_data-import_p.
      WHEN parameter_kinds-changing.
        CLEAR s_p_ch.   s_p_ch = <pa>-hdr.    INSERT s_p_ch INTO TABLE fm_data-changing_p.
      WHEN parameter_kinds-exporting.
        CLEAR s_p_ex.   MOVE-CORRESPONDING <pa>-hdr TO s_p_ex.    INSERT s_p_ex INTO TABLE fm_data-export_p.
      WHEN parameter_kinds-table.
        CLEAR s_p_ta.   MOVE-CORRESPONDING <pa>-hdr TO s_p_ta.    s_p_ta-dbstruct = <pa>-dbfield.    INSERT s_p_ta INTO TABLE fm_data-tables.
      WHEN OTHERS.
    ENDCASE.
*APPLICATION                                      DOKU
*ID                                      FU
*OBJECT                                      xxxxx yyy
*LANGU                                      F
*MASTERLANG                                      X
*TYP                                      T
*DOKFORM                                      S_DOCU_SHOW
*DOKSTYLE                                      S_DOCUS1
    CLEAR: d_doc_p, s_doc.    d_doc_p-func = A0_maindata-name.    d_doc_p-param = <pa>-parameter.    s_doc-application = 'DOKU'.   s_doc-id = doc_ids-function.
    s_doc-object = d_doc_p.   s_doc-masterlang = abap_true.   s_doc-typ = 'T'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
    LOOP AT <pa>-texts ASSIGNING <pt>.
      IF NOT <pt>-short_txt IS INITIAL.
        CLEAR s_p_txt. s_p_txt-spras = <pt>-langu. s_f_txt-stext = <pt>-short_txt. s_p_txt-funcname = A0_maindata-name. s_p_txt-parameter = <pa>-parameter. s_p_txt-kind = 'P'. INSERT s_p_txt INTO TABLE fm_data-param_texts.
      ENDIF.
      IF NOT <pt>-long_txt IS INITIAL.
        CLEAR s_doc_txt.    s_doc_txt = <pt>-long_txt.   s_doc_txt-tdspras = <pt>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
        IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.
  ENDLOOP.

  LOOP AT exceptions ASSIGNING <cx>.
    CLEAR s_excep.    s_excep = <cx>-hdr.

    LOOP AT <cx>-texts ASSIGNING <pt>.
      CLEAR s_p_txt.    s_p_txt-spras = <pt>-langu.   s_f_txt-stext = <pt>-short_txt.   s_p_txt-funcname = A0_maindata-name.    s_p_txt-parameter = <cx>-exception.   s_p_txt-kind = 'X'.    INSERT s_p_txt INTO TABLE fm_data-param_texts.
    ENDLOOP.
    INSERT s_excep INTO TABLE fm_data-exceptions.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
