class ZAPLINK_PROG_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_PROGRAM .

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
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .
  aliases TT_TXTP_TEXTPOOLS
    for ZAPLINK_DATATYPES~TT_TXTP_TEXTPOOLS .

  class-methods CLASS_CONSTRUCTOR .
protected section.

  aliases TD_ABAPSOURCE
    for ZAPLINK_DATATYPES~TD_ABAPSOURCE .

  types TO_MENUPAINTER type ref to ZAPLINK_MENUPAINTER_DATA .
  types TO_VARIANT type ref to ZAPLINK_VARI_RAW .
  types:
    BEGIN OF ts_variant,
      zl_object TYPE to_variant,
      END OF ts_variant .
  types:
    tt_variants TYPE STANDARD TABLE OF ts_variant WITH DEFAULT KEY .
  types:
    tt_trdirti TYPE SORTED TABLE OF trdirti WITH UNIQUE KEY sprsl .
  types:
    tt_vari_data TYPE STANDARD TABLE OF zaplink_vari_data=>ts_fm_data WITH DEFAULT KEY .
  types:
    BEGIN OF ts_fm_data,
* xx pour type doc
      header          TYPE progdir,
      texts           TYPE tt_trdirti,
      docs            TYPE tt_docs,
      textspool	      TYPE tt_txtp_textpools,
      dynpros	        TYPE tt_dynpros,
      menupainter	    TYPE to_menupainter,
      source          TYPE td_abapsource,
      variants        TYPE tt_vari_data,                    " Issue 53
    END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
*    INCLUDE TYPE reposrc. "The databae view "REPOSRC" is write-protected, so it cannot be changed.
  INCLUDE TYPE progdir.
  TYPES:
    END OF ts_maindata .

  data A0_MAINDATA type TS_MAINDATA .
  data DOCUMENTATION_OFF type TT_DOCS .
  data TEXTSPOOL type TT_TXTP_TEXTPOOLS .
  data SOURCE type TS_SOURCEASSTRUC .
  data DYNPROS type TT_DYNPROS .
  data MENUPAINTER type TO_MENUPAINTER .
  data TEXTS type TT_TEXTS .
  data VARIANTS type TT_VARIANTS .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA
    raising
      ZAPLINK_CX .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE
    raising
      ZAPLINK_CX .
  methods UNANONYMIZE .
private section.

  constants:
    BEGIN OF DOC_IDS,
      Prog      TYPE td_doc_id VALUE 'RE',    " Report
    END OF DOC_IDS .
  class-data:
    BEGIN OF R_DOC_IDS,
      prog     TYPE TR_DOCID,
    END OF R_DOC_IDS .
ENDCLASS.



CLASS ZAPLINK_PROG_DATA IMPLEMENTATION.


  method ANONYMIZE.
  FIELD-SYMBOLS:
    <t> LIKE LINE OF texts,
    <v> LIKE LINE OF variants.
  CLEAR: a0_maindata-cnam, a0_maindata-cdat, a0_maindata-unam, a0_maindata-udat, a0_maindata-sdate, a0_maindata-stime, a0_maindata-idate,
         a0_maindata-itime, a0_maindata-vern, a0_maindata-state. " Issue 68.
  LOOP AT variants ASSIGNING <v>.
    <v>-zl_object->anonymize( ).
  ENDLOOP.
* Issue 133 : Release still predent in documentation
  LOOP AT texts ASSIGNING <t>.
    CLEAR: <t>-long_txt-tdlreles, <t>-long_txt-tdfreles.
  ENDLOOP.
  endmethod.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*RE       Report/Function Group, Logical DB                            Program/module pool
  DATA _id LIKE LINE OF r_doc_ids-prog.

  _id-sign = 'I'.   _id-option = 'EQ'.
  _id-low = doc_ids-prog.      APPEND _id TO r_doc_ids-prog.
  endmethod.


  method FROM_DATA.
  DATA s_txt     LIKE LINE OF texts.
  DATA t_docs    TYPE tt_docs.
  DATA s_variant LIKE LINE OF variants.
  FIELD-SYMBOLS:
    <v>  LIKE LINE OF fm_data-variants,
    <d>  LIKE LINE OF fm_data-docs,
    <dt> LIKE LINE OF <d>-texts,
    <t>  LIKE LINE OF fm_data-texts.

  a0_maindata = fm_data-header.
  textspool = fm_data-textspool.
  source-_ = fm_data-source.
  dynpros = fm_data-dynpros.
  menupainter = fm_data-menupainter.

  t_docs = fm_data-docs.
  READ TABLE t_docs ASSIGNING <d>
       WITH KEY id = doc_ids-prog
            object = a0_maindata-name.
  LOOP AT fm_data-texts ASSIGNING <t>.
    CLEAR s_txt.    s_txt-langu = <t>-sprsl.   s_txt-short_txt = <t>-text.
    IF <d> IS ASSIGNED.
      READ TABLE <d>-texts ASSIGNING <dt>
          WITH KEY tdspras = s_txt-langu.
      IF sy-subrc = 0.    s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    DELETE TABLE <d>-texts FROM <dt>.    ENDIF.
    ENDIF.
    INSERT s_txt INTO TABLE texts.
  ENDLOOP.
  IF <d> IS ASSIGNED.
    LOOP AT <d>-texts ASSIGNING <dt>.
      CLEAR s_txt.    s_txt-langu = <dt>-tdspras.   s_txt-long_txt = <dt>.   CLEAR s_txt-long_txt-tdspras.    INSERT s_txt INTO TABLE texts.
    ENDLOOP.
  ENDIF.

  LOOP AT fm_data-variants ASSIGNING <v>.
    CREATE OBJECT s_variant-zl_object EXPORTING fm_data = <v>.
    APPEND s_variant TO variants.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_txt     LIKE LINE OF fm_data-texts.
  DATA s_doc     LIKE LINE OF fm_data-docs.
  DATA s_doc_txt LIKE LINE OF s_doc-texts.
  DATA s_variant LIKE LINE OF fm_data-variants.
  DATA d_variant TYPE zaplink_vari_data=>ts_variant_key.
  FIELD-SYMBOLS:
    <v>  LIKE LINE OF variants,
    <t>  LIKE LINE OF texts.

  fm_data-header = a0_maindata.
  fm_data-textspool = textspool.
  fm_data-source = source-_.
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
  s_doc-application = 'DOKU'.   s_doc-id = doc_ids-prog.   s_doc-object = a0_maindata-name.
  s_doc-masterlang = abap_true.   s_doc-typ = 'E'.    s_doc-dokform = 'S_DOCU_SHOW'.    s_doc-dokstyle = 'S_DOCUS1'.
  LOOP AT texts ASSIGNING <t>.
    IF NOT <t>-short_txt IS INITIAL.    CLEAR s_txt.    s_txt-sprsl = <t>-langu.   s_txt-text = <t>-short_txt.   INSERT s_txt INTO TABLE fm_data-texts.    ENDIF.
    IF NOT <t>-long_txt IS INITIAL.
      CLEAR s_doc_txt.    s_doc_txt = <t>-long_txt.   s_doc_txt-tdspras = <t>-langu.    INSERT s_doc_txt INTO TABLE s_doc-texts.
      IF s_doc-langu IS INITIAL.    s_doc-langu = s_doc_txt-tdspras.    ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT s_doc-texts IS INITIAL.    INSERT s_doc INTO TABLE fm_data-docs.   ENDIF.

  LOOP AT variants ASSIGNING <v>.
    d_variant = zaplink_vari_data=>name_2_key( <v>-zl_object->name ).
    d_variant-program = fm_data-header-name.
    <v>-zl_object->name = zaplink_vari_data=>key_2_name( d_variant ).
    s_variant = <v>-zl_object->to_data( ).
    APPEND s_variant TO fm_data-variants.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  FIELD-SYMBOLS <v> LIKE LINE OF variants.
  a0_maindata-cnam = sy-uname.
  LOOP AT variants ASSIGNING <v>.
    <v>-zl_object->unanonymize( ).
  ENDLOOP.
  endmethod.
ENDCLASS.
