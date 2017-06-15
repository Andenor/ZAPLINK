class ZAPLINK_CLAS_DATA definition
  public
  inheriting from ZAPLINK_OBJECT_DATA
  final
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_OBJECT .

public section.
  type-pools SEOC .
  type-pools SEOF .
  type-pools SEOK .
  type-pools SEOO .
  type-pools SEOR .
  type-pools SEOS .
  type-pools SEOT .
  class ZAPLINK_PROGRAM definition load .

  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .

  types TO_MSG_COL type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types:
    begin of ts_message,
      class       type symsgid,
      number      type symsgno,
      attribut_1  type scx_attrname,
      attribut_2  type scx_attrname,
      attribut_3  type scx_attrname,
      attribut_4  type scx_attrname,
    end of ts_message .
  types:
    BEGIN OF ts_exception_text,
*    BEGIN OF ts_exception_text.
*    INCLUDE TYPE seoo_attribute_r AS attr.
*    INCLUDE TYPE sotr_head AS cx.
*    TYPES:
        cmpname   TYPE seoo_attribute_r-cmpname,      " Issue 62 : Reorg Exception text
        editorder TYPE seoo_attribute_r-editorder,    " Issue 71 : Keep SOTR Attribut order
        texts     TYPE tt_texts,
*        texts     TYPE tt_seocompotx_l,
*        longtexts TYPE tt_sotr_texts,
        message   type ts_message,
      END OF ts_exception_text .
  types:
    tt_exceptions_texts TYPE SORTED TABLE OF ts_exception_text WITH UNIQUE KEY cmpname .
  types:
    BEGIN OF ts_locals,
          types           TYPE ts_sourceasstruc,
          implementations TYPE ts_sourceasstruc,
          macros          TYPE ts_sourceasstruc,
          local_test_class TYPE ts_sourceasstruc,  " Issue 82
        END OF ts_locals .
  types:
    BEGIN OF ts_redefinition.
    INCLUDE TYPE seoredef AS hdr.  " M#thodes red#finies par relation d'h#ritage
    TYPES:
        source  TYPE ts_sourceasstruc,
      END OF ts_redefinition .
  types:
    tt_redefinitions TYPE SORTED TABLE OF ts_redefinition WITH UNIQUE KEY mtdname .
  types:
    tt_friends    TYPE SORTED TABLE OF seofriends WITH UNIQUE KEY refclsname friendtype .
  types:
    BEGIN OF ts_class_data.
    INCLUDE TYPE vseoclass AS hdr.
*      Include type VSEOEXTEND as Inh.  " Duplicate name for some fields
    TYPES:
**      CLSNAME     type SEOMETAREL-CLSNAME,
*      REFCLSNAME  type SEOMETAREL-REFCLSNAME,
**      VERSION     type SEOMETAREL-VERSION,
**      STATE       type SEOMETAREL-STATE,
*      AUTHOR      type SEOMETAREL-AUTHOR,
*      CREATEDON   type SEOMETAREL-CREATEDON,
*      CHANGEDBY   type SEOMETAREL-CHANGEDBY,
*      CHANGEDON   type SEOMETAREL-CHANGEDON,
*      RELNAME     type SEOMETAREL-RELNAME,
        inheritance TYPE vseoextend,
*        typegroups  TYPE tt_typegroups,
*        interfaces  TYPE tt_interfaces,
*        friends     TYPE tt_friends,
        texts       TYPE tt_ltexts,   " tt_seoclasstx_s,
        textpool    TYPE tt_txtp_textpools,
        sources     TYPE ts_sources,
      END OF ts_class_data .

  data A0_MAINDATA type TS_CLASS_DATA .
  data FRIENDS type TT_FRIENDS .
  data REDEFINITIONS type TT_REDEFINITIONS .
  data LOCAL type TS_LOCALS .
  data EXCEPTIONS_TEXTS type TT_EXCEPTIONS_TEXTS .
  data TEXTSPOOL type TT_TXTP_TEXTPOOLS .
  data DYNPROS type TT_DYNPROS .

  events MY_EVENT2 .

  class-methods CLASS_CONSTRUCTOR .
  class-methods UPDATE_CONCEPT
    importing
      !MSG_COL type TO_MSG_COL
    changing
      !FM_DATA type T_FM_DATA
    raising
      ZAPLINK_CX_CONNECTOR .

  methods ANONYMIZE
    redefinition .
  methods COMPLETE_DATA
    redefinition .
  methods FROM_DATA
    redefinition .
  methods TO_DATA
    redefinition .
protected section.

  data MENUPAINTER type TO_MENUPAINTER .
  constants INTF_4_MESSAGE_EXCEPTION_CL type SEOCLSNAME value 'IF_T100_MESSAGE' ##NO_TEXT.
  constants:
    begin of MESSAGE_BASED_FIELDS,
      class       type string value 'MSGID',
      number      type string value 'MSGNO',
      attribut_1  type string value 'ATTR1',
      attribut_2  type string value 'ATTR2',
      attribut_3  type string value 'ATTR3',
      attribut_4  type string value 'ATTR4',
    end of MESSAGE_BASED_FIELDS .
  class-data MESSAGE_CX_TYPE_SOURCE type STRING .
private section.

  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
  class-data C_KIND type TD_TRANSPORT_KIND .
  class-data C_TYPE type TD_COMPTYPE value OBJECT_TYPES-CLASS ##NO_TEXT.

  class-methods CONV_SOURCE
    importing
      value(SOURCE) type TT_ABAPRAWSOURCE
    returning
      value(RESULT) type TD_ABAPSOURCE .
  class-methods VALUE2SOTR
    importing
      !DATA type SEOVALUE
    returning
      value(RESULT) type SOTR_CONC .
  class-methods SOTR2VALUE
    importing
      !DATA type SOTR_CONC
    returning
      value(RESULT) type SEOVALUE .
ENDCLASS.



CLASS ZAPLINK_CLAS_DATA IMPLEMENTATION.


  method ANONYMIZE.
  DATA s_fr LIKE LINE OF friends.
  DATA s_re LIKE LINE OF redefinitions.

  super->anonymize( ).
  CLEAR: a0_maindata-author, a0_maindata-createdon, a0_maindata-changedby, a0_maindata-changedon, a0_maindata-chgdanyby, a0_maindata-chgdanyon, a0_maindata-r3release,
    a0_maindata-inheritance-author, a0_maindata-inheritance-createdon, a0_maindata-inheritance-changedby, a0_maindata-inheritance-changedon, a0_maindata-inheritance-version. " Issue 75

* FRIENDS
  MODIFY friends FROM s_fr TRANSPORTING author createdon changedby changedon version " Issue 75
         WHERE NOT refclsname IS INITIAL OR refclsname IS INITIAL. " Issue 68

*REDEFINITIONS
  MODIFY redefinitions FROM s_re TRANSPORTING version       " Issue 75
         WHERE NOT refclsname IS INITIAL OR refclsname IS INITIAL.

*LOCAL : OK
*EXCEPTIONS_TEXTS : OK " Issue 62
*  MODIFY exceptions_texts FROM s_et TRANSPORTING author createdon changedby changedon r3release crea_name crea_tstut chan_name chan_tstut
*         WHERE NOT cmpname IS INITIAL.
  endmethod.


  method CLASS_CONSTRUCTOR.
  c_kind = zaplink_connectors=>get_typekind( c_type ).
  CONCATENATE 'begin of #NAME#,' CL_ABAP_CHAR_UTILITIES=>CR_LF
'      msgid type symsgid value ''#CLASS#'',' CL_ABAP_CHAR_UTILITIES=>CR_LF
'      msgno type symsgno value ''#NUMBER#'',' CL_ABAP_CHAR_UTILITIES=>CR_LF
'      attr1 type scx_attrname value ''#A1#'',' CL_ABAP_CHAR_UTILITIES=>CR_LF
'      attr2 type scx_attrname value ''#A2#'',' CL_ABAP_CHAR_UTILITIES=>CR_LF
'      attr3 type scx_attrname value ''#A3#'',' CL_ABAP_CHAR_UTILITIES=>CR_LF
'      attr4 type scx_attrname value ''#A4#'',' CL_ABAP_CHAR_UTILITIES=>CR_LF
'    end of #NAME#'
      into MESSAGE_CX_TYPE_SOURCE.
  endmethod.


  method COMPLETE_DATA.
  DATA _val TYPE seovalue.
  DATA _sotr LIKE LINE OF fm_data-exceptions_texts.
  DATA _text TYPE STANDARD TABLE OF sotr_text.
  DATA _attr_text LIKE LINE OF fm_data-component_desc.
  FIELD-SYMBOLS:
    <pt> LIKE LINE OF fm_data-component_desc,
    <st> LIKE LINE OF _sotr-texts,
    <a> LIKE LINE OF fm_data-attributes.

  obj_name = fm_data-class-clsname.                         " Issue 23
  CALL METHOD super->complete_data
    CHANGING
      fm_data = fm_data.

  IF fm_data-class-category = '40'.  " 40	Exception Class
    LOOP AT fm_data-attributes ASSIGNING <a>
         WHERE attdecltyp = '2'   " Constant
           AND locked = abap_true
           AND type = 'SOTR_CONC'
           AND NOT attvalue IS INITIAL.
      CLEAR _sotr. _sotr-cmpname = <a>-cmpname.
      _sotr-concept = value2sotr( <a>-attvalue ).
*      CALL FUNCTION 'SOTR_LINK_GET_LINK'
      SELECT SINGLE concept_2 INTO _sotr-lhdr-concept
        FROM sotr_link
        WHERE concept_1 = _sotr-concept.
      IF sy-subrc = 0.
*        CALL FUNCTION 'SOTR_STRING_READ_TEXT_WITH_KEY'
        SELECT SINGLE * INTO _sotr-lhdr
          FROM sotr_headu
          WHERE concept = _sotr-lhdr-concept.
        SELECT * INTO TABLE _sotr-longtexts
          FROM sotr_textu
          WHERE concept = _sotr-lhdr-concept.
      ENDIF.

      CALL FUNCTION 'SOTR_GET_CONCEPT'
        EXPORTING
          concept        = _sotr-concept
        IMPORTING
          header         = _sotr-hdr
        TABLES
          entries        = _text
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR: _sotr, _val, _text.
        CONTINUE.
      ENDIF.

      _sotr-texts = _text.
      _sotr-cmpname = <a>-cmpname.
      INSERT _sotr INTO TABLE fm_data-exceptions_texts.
      LOOP AT _sotr-texts ASSIGNING <st>
           WHERE NOT text IS INITIAL.
        READ TABLE fm_data-component_desc ASSIGNING <pt>
             WITH KEY cmpname = <a>-cmpname
                        langu = <st>-langu.
        IF sy-subrc = 0.
          IF <pt>-descript IS INITIAL.
            <pt>-descript = <st>-text.
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING <a> TO _attr_text.
          _attr_text-langu = <st>-langu.
          _attr_text-descript = <st>-text.
          INSERT _attr_text INTO TABLE fm_data-component_desc.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ENDIF.
  endmethod.


  method CONV_SOURCE.
*  DATA code TYPE zaplink_object=>tt_abaprawsource.
  DATA linecount TYPE i.
  DATA d_line LIKE LINE OF source.                          " Issue 68
  FIELD-SYMBOLS: <l> LIKE LINE OF source.

*  code = zaplink_object=>get_prog_rawsource( program ).

  IF NOT source IS INITIAL.
    linecount = LINES( source ).
    IF linecount > 1.
*      READ TABLE source ASSIGNING <l> INDEX linecount.      " Issue 68
*      TRANSLATE <l> TO UPPER CASE.                        "#EC SYNTCHAR
*      IF <l> CP 'ENDMETHOD.*'.
      READ TABLE source INTO d_line INDEX linecount.
*      TRANSLATE d_line TO UPPER CASE.                     "#EC SYNTCHAR
      CONDENSE d_line.    " '   ENDMETHOD.' will also be removed
      IF d_line CP 'ENDMETHOD+.*' OR d_line CP 'ENDMETHOD.*'.
        DELETE source INDEX linecount.
      ENDIF.
    ENDIF.
*    READ TABLE source ASSIGNING <l> INDEX 1.               " Issue 68
*    TRANSLATE <l> TO UPPER CASE.                            "#EC *
*    IF <l> CP 'METHOD *'.                                 "#EC SYNTCHAR
    READ TABLE source INTO d_line INDEX 1.
*    TRANSLATE d_line TO UPPER CASE.                         "#EC *
    CONDENSE d_line.
    IF d_line CP 'METHOD *'.                              "#EC SYNTCHAR
      DELETE source INDEX 1.
    ENDIF.

* Issue 68 Start : Blank line at the end of source source can't be imported => Remove it
    IF NOT source IS INITIAL.
      linecount = LINES( source ).
      READ TABLE source ASSIGNING <l> INDEX linecount.
      WHILE <l> IS INITIAL.
        DELETE source INDEX linecount.
        linecount = LINES( source ).
        READ TABLE source ASSIGNING <l> INDEX linecount.
        IF NOT <l> IS ASSIGNED.   EXIT.   ENDIF.
      ENDWHILE.
    ENDIF.
* Issue 68 END
  ENDIF.

  result = zaplink_object=>conv_abap_raw2source( source ).
  endmethod.


  method FROM_DATA.
  DATA _in  LIKE LINE OF interfaces.
  DATA _ia  LIKE LINE OF _in-components.
  DATA _t   LIKE LINE OF a0_maindata-texts.
  DATA _fr  LIKE LINE OF friends.
  DATA _re  LIKE LINE OF me->redefinitions.
  DATA _me  LIKE LINE OF methods.
  DATA _dep LIKE LINE OF _dependencies.
  DATA _mk  TYPE seocpdkey.
  DATA _source TYPE zaplink_program=>ts_source.
  FIELD-SYMBOLS:
    <dc>  LIKE LINE OF deferred-classes,
    <di>  LIKE LINE OF deferred-interfaces,
    <m_>  LIKE LINE OF fm_data-includes-methods,
    <im>  LIKE LINE OF fm_data-implementings,
    <ei>  LIKE LINE OF fm_data-explore_implementings,
    <ic>  LIKE LINE OF <ei>-comprisings,
    <rd>  LIKE LINE OF fm_data-impl_details,
    <me>  LIKE LINE OF methods,
    <fr>  LIKE LINE OF fm_data-friendships,
    <re>  LIKE LINE OF fm_data-redefinitions.

* added for exception classes
  FIELD-SYMBOLS:
    <cx>   LIKE LINE OF fm_data-exceptions_texts,
    <cxt>  LIKE LINE OF <cx>-texts,
    <cxl>  LIKE LINE OF <cx>-longtexts,
    <at>   LIKE LINE OF fm_data-attributes.
  DATA _cx_text  LIKE LINE OF exceptions_texts.
  DATA _sotr_key TYPE sotr_conc.
*  DATA _text     LIKE LINE OF _cx_text-longtexts.
  DATA _text     LIKE LINE OF _cx_text-texts.
  DATA _fm_data  TYPE t_fm_data.
* added for documentation move
*  FIELD-SYMBOLS:
*    <desc> LIKE LINE OF fm_data-class_desc,
*    <doc>  LIKE LINE OF fm_data-documentation,
*    <dt>   LIKE LINE OF <doc>-texts.
  DATA s_txt  LIKE LINE OF a0_maindata-texts.
  DATA s_key  TYPE ts_doc_key.
  DATA t_tokens       TYPE STANDARD TABLE OF stokes.
  DATA t_statements   TYPE STANDARD TABLE OF sstmnt.
  DATA d_field        TYPE string.
  DATA d_is_ok        TYPE abap_bool.
  DATA d_len          TYPE i.
  FIELD-SYMBOLS:
    <st> LIKE LINE OF t_statements,
    <tk> LIKE LINE OF t_tokens.

  a0_maindata-hdr = fm_data-class.
  obj_name = a0_maindata-clsname.
  obj_version = a0_maindata-version.
  obj_langu = a0_maindata-langu.
  obj_type = c_type.
  CLEAR a0_maindata-uuid.                                   " Issue 59

*CLASS_DESC  TYPE tt_SEOCLASSTX,
  a0_maindata-inheritance = fm_data-inheritance.
  CLEAR a0_maindata-inheritance-clsname.

* Class texts
* Issue 33 : Documentation connected to text
  a0_maindata-texts = fd_text( descriptions  = fm_data-class_desc
                                   classname = obj_name
                                          id = doc_ids-class-header
                               documentation = fm_data-documentation ).
  IF NOT a0_maindata-texts IS INITIAL. CLEAR a0_maindata-descript. ENDIF.

* Attribute to Exception_text for exception class, must stand before calling super
  _fm_data = fm_data.
  IF fm_data-class-category = seoc_category_exception.
    DELETE _fm_data-methods
           WHERE clsname = obj_name
             AND cmpname = 'CONSTRUCTOR'.    " Auto generated
    READ TABLE _fm_data-implementings TRANSPORTING NO FIELDS WITH KEY refclsname = intf_4_message_exception_cl.
    IF sy-subrc = 0.      " Message based Exception
      LOOP AT _fm_data-attributes ASSIGNING <at>
           WHERE clsname = obj_name
             AND attdecltyp = '2'   " Constant
             AND locked = abap_true
             AND typtype = '4'
             AND type = ''
             AND NOT typesrc IS INITIAL.
        CLEAR _cx_text.   MOVE-CORRESPONDING <at> TO _cx_text.
        SCAN ABAP-SOURCE  <at>-typesrc TOKENS INTO t_tokens
                                   STATEMENTS INTO t_statements.
        ASSERT sy-subrc = 0.
        CHECK LINES( t_statements ) = 8.
        d_is_ok = abap_true.
        LOOP AT t_statements ASSIGNING <st> FROM 2 TO 7.
          CASE sy-tabix.
            WHEN 2.   d_field = message_based_fields-class.
            WHEN 3.   d_field = message_based_fields-number.
            WHEN 4.   d_field = message_based_fields-attribut_1.
            WHEN 5.   d_field = message_based_fields-attribut_2.
            WHEN 6.   d_field = message_based_fields-attribut_3.
            WHEN 7.   d_field = message_based_fields-attribut_4.
            WHEN OTHERS.
              ASSERT sy-tabix >= 2 AND sy-tabix <= 7.
          ENDCASE.
          ADD 1 TO <st>-from.   " CRLF is the first token
          READ TABLE t_tokens ASSIGNING <tk> INDEX <st>-from.
          TRANSLATE <tk>-str TO UPPER CASE.   TRANSLATE d_field TO UPPER CASE.
          IF <tk>-str <> d_field.
            CLEAR d_is_ok.
            EXIT.
          ENDIF.
          READ TABLE t_tokens ASSIGNING <tk> INDEX <st>-to.
          d_len = STRLEN( <tk>-str ) - 2.
          ASSERT  d_len >= 0.
          <tk>-str = <tk>-str+1(d_len).    " 'ZAPLINK_DEVTOOLS' => ZAPLINK_DEVTOOLS
          CASE d_field.
            WHEN message_based_fields-class.        _cx_text-message-class = <tk>-str.
            WHEN message_based_fields-number.       _cx_text-message-number = <tk>-str.
            WHEN message_based_fields-attribut_1.   _cx_text-message-attribut_1 = <tk>-str.
            WHEN message_based_fields-attribut_2.   _cx_text-message-attribut_2 = <tk>-str.
            WHEN message_based_fields-attribut_3.   _cx_text-message-attribut_3 = <tk>-str.
            WHEN message_based_fields-attribut_4.   _cx_text-message-attribut_4 = <tk>-str.
            WHEN OTHERS.
              ASSERT d_field IS INITIAL.
          ENDCASE.
        ENDLOOP.
        CHECK d_is_ok = abap_true.
        INSERT _cx_text INTO TABLE exceptions_texts.
        DELETE _fm_data-attributes.                 " exception text no logner an attribute
      ENDLOOP.
    ELSE.     " Standard Exception
      LOOP AT _fm_data-attributes ASSIGNING <at>
           WHERE clsname = obj_name
             AND attdecltyp = '2'   " Constant
             AND locked = abap_true
             AND typtype = '1'
             AND type = 'SOTR_CONC'
             AND NOT attvalue IS INITIAL.
        CLEAR _cx_text.   MOVE-CORRESPONDING <at> TO _cx_text. " Issue 71.
*      _sotr_key = value2sotr( <at>-attvalue ).
* Issue 62 : Start
        READ TABLE fm_data-exceptions_texts ASSIGNING <cx>
             WITH TABLE KEY cmpname = _cx_text-cmpname.
        IF sy-subrc = 0.
          LOOP AT <cx>-texts ASSIGNING <cxt>.       " Short & Long texts
            CLEAR _text.
            _text-langu = <cxt>-langu.
            _text-short_txt = <cxt>-text.
            READ TABLE <cx>-longtexts ASSIGNING <cxl>
                 WITH KEY langu = _text-langu.
            IF sy-subrc = 0.    " has long texts
              _text-long_txt-_ = <cxl>-text.
              REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN _text-long_txt-_ WITH cl_abap_char_utilities=>newline.   " Issue 63 : Text has CR & CR+LF
            ENDIF.
            INSERT _text INTO TABLE _cx_text-texts.
          ENDLOOP.
        ENDIF.
** SOTR can't be change or created using external key.
** When you change texts of an exception it's SOTR GUID change :-(
*      CLEAR: _cx_text-concept.                                                          " SOTR
** So don't save KEY to component useless
        IF <at>-cmpname = a0_maindata-clsname. CLEAR _cx_text-cmpname. ENDIF.  " do not repeat self exception text : Issue 64
        INSERT _cx_text INTO TABLE exceptions_texts.
* Issue 62 : End
        DELETE _fm_data-attributes.                 " exception text no logner an attribute
      ENDLOOP.
    ENDIF.
  ENDIF.

  CALL METHOD super->from_data
    EXPORTING
      fm_data = _fm_data.

* Dynpro : Issue 8
  dynpros = _fm_data-dynpros.

* Menupainter : Issue 9
  menupainter = _fm_data-menupainter.

* TEXTPools : Issue 1
  textspool = _fm_data-textspool.

*LOCAL definitions
  local-types-_ = conv_source( fm_data-includes-ccdef-raw_source ).
  local-implementations-_ = conv_source( fm_data-includes-ccimp-raw_source ).
  local-macros-_ = conv_source( fm_data-includes-ccmac-raw_source ).
  local-local_test_class-_ = conv_source( fm_data-includes-localtestclass-raw_source ). " Issue 82

*IMPLEMENTINGS  TYPE  SEOR_IMPLEMENTINGS_R
  LOOP AT fm_data-implementings ASSIGNING <im>
          WHERE clsname = a0_maindata-clsname.
    CLEAR _in.
    _in-hdr = <im>.

    LOOP AT fm_data-impl_details ASSIGNING <rd>
         WHERE clsname = <im>-clsname
           AND refclsname = <im>-refclsname.
*           and version = <im>-version   " needed ?
      CLEAR _ia.
*      _ia = <rd>.
      _ia-hdr = <rd>.
      CLEAR: _ia-clsname, _ia-refclsname.
*      APPEND _ia  TO _in-attributes_redefinition.

      IF <rd>-attvalue IS INITIAL             " Component is a method
* because attribute without new default value are not displayed in impl_details because not implemented (no change made)
* Attribute in interface can't have any default value
* If in the future they could have it will be necessary to test a interface with default value implemented by a class where default value is return to none.
         AND <rd>-mtdabstrct IS INITIAL.      " not abstract : with source code
        CLEAR _mk.
        _mk-clsname = <rd>-clsname.
        CONCATENATE <rd>-refclsname method_separator <rd>-mtdname INTO _mk-cpdname.
        READ TABLE _fm_data-includes-methods ASSIGNING <m_>
             WITH KEY cpdkey = _mk.
        IF sy-subrc = 0.
          _ia-source-_ = conv_source( <m_>-raw_source ).
          IF NOT _ia-source-_ IS INITIAL.
            CLEAR <m_>. " do it once : DELETE _fm_data-includes-methods. dump
          ENDIF.
        ENDIF.
* Issue 120 : Method can be empty... Shame on me.
*        IF _ia-source-_ IS INITIAL.
*          _ia-source-_ = 'Source code Expected'(sce). " to generate a compilation error on target method in order to user to investigate the reson
*        ENDIF.
      ENDIF.

      INSERT _ia  INTO TABLE _in-components.
    ENDLOOP.

    CLEAR _in-clsname.
    INSERT _in INTO TABLE interfaces.
  ENDLOOP.

* Interface declared in an other interface should be removed, because when adding interface the included interface will be added also
* This prevent import failure
  LOOP AT fm_data-explore_implementings ASSIGNING <ei>
       WHERE NOT comprisings IS INITIAL.
    LOOP AT <ei>-comprisings ASSIGNING <ic>.
      DELETE interfaces WHERE refclsname = <ic>-refclsname.
    ENDLOOP.
  ENDLOOP.

*Methodes Source
* ISSUE 4
*  LOOP AT _fm_data-includes-methods ASSIGNING <m_>.
*    _source = conv_source( <m_>-incname ).
*    CHECK NOT _source IS INITIAL.
*    _mk = <m_>-cpdkey.
*    CHECK _mk-clsname = a0_maindata-clsname.
*    READ TABLE methods ASSIGNING <me>
*         WITH TABLE KEY cmpname = _mk-cpdname.
*    IF sy-subrc = 0.
*      <me>-source-_ = _source.
*    ELSE.
*      _me-cmpname = _mk-cpdname.
*      _me-source-_ = _source.
*      INSERT _me INTO TABLE methods.
*    ENDIF.
*  ENDLOOP.
  LOOP AT methods ASSIGNING <me>.
    _mk-clsname = a0_maindata-clsname.
    _mk-cpdname = <me>-cmpname.
    READ TABLE _fm_data-includes-methods ASSIGNING <m_>
         WITH KEY cpdkey = _mk.
    CHECK sy-subrc = 0.
    _source = conv_source( <m_>-raw_source ).
    CHECK NOT _source IS INITIAL.
    <me>-source-_ = _source.
  ENDLOOP.
* ISSUE 4

*FRIENDSHIPS  TYPE  SEOF_FRIENDSHIPS_R
  LOOP AT fm_data-friendships ASSIGNING <fr>
    WHERE clsname = a0_maindata-clsname.
    CLEAR _fr.  _fr = <fr>.   CLEAR _fr-clsname.
    IF _fr-refclsname = a0_maindata-clsname. CLEAR _fr-refclsname. ENDIF.  " do not repeat self : Issue 64
    INSERT _fr INTO TABLE friends.
  ENDLOOP.

*REDEFINITIONS  TYPE  SEOR_REDEFINITIONS_R
*IMPL_DETAILS TYPE  SEOR_REDEFINITIONS_R
  LOOP AT fm_data-redefinitions ASSIGNING <re>
       WHERE clsname = a0_maindata-clsname.
    CLEAR: _re.
    _re-hdr = <re>.

    _mk-clsname = <re>-clsname.
    _mk-cpdname = <re>-mtdname.
    READ TABLE fm_data-includes-methods ASSIGNING <m_>
         WITH KEY cpdkey = _mk.
    IF sy-subrc = 0.
      _re-source-_ = conv_source( <m_>-raw_source ).
    ENDIF.

    CLEAR _re-clsname.
    INSERT _re INTO TABLE redefinitions.
  ENDLOOP.

* Issue 47 : Introducing dependencies
  IF fm_data-class-category = seoc_category_exception     " Issue 123 : Exception class should always depend on inheritance because contructor is autogenerated
  OR NOT redefinitions IS INITIAL.
    CLEAR _dep.   _dep-type = c_type.   _dep-name = a0_maindata-inheritance-refclsname. INSERT _dep INTO TABLE _dependencies.
  ENDIF.
* Issue 74 : Adding Deffered elements to dependencies
  LOOP AT deferred-classes ASSIGNING <dc>.
    CLEAR _dep.   _dep-type = c_type.   _dep-name = <dc>-typegroup.
    INSERT _dep INTO TABLE _dependencies.
  ENDLOOP.
  LOOP AT deferred-interfaces ASSIGNING <di>.
    CLEAR _dep.   _dep-type = object_types-interface.   _dep-name = <di>-typegroup.
    INSERT _dep INTO TABLE _dependencies.
  ENDLOOP.

  clear_tags( ).
  endmethod.


  method SOTR2VALUE.
  concatenate '''' data '''' into result.
  endmethod.


  method TO_DATA.
  DATA _t   LIKE LINE OF a0_maindata-texts.
  DATA _ct  LIKE LINE OF fm_data-class_desc.
  DATA _in  LIKE LINE OF fm_data-implementings.
  DATA _fr  LIKE LINE OF fm_data-friendships.
  DATA _id  LIKE LINE OF fm_data-impl_details.
  DATA _ms  LIKE LINE OF fm_data-method_sources.
  DATA _at  LIKE LINE OF fm_data-attributes.
  DATA _cx  LIKE LINE OF fm_data-exceptions_texts.
  FIELD-SYMBOLS:
    <cx>  LIKE LINE OF exceptions_texts,
    <re>  LIKE LINE OF redefinitions,
    <in>  LIKE LINE OF interfaces,
*    <t>   LIKE LINE OF a0_maindata-texts,
    <ia>  LIKE LINE OF <in>-components,
    <ct>  LIKE LINE OF <cx>-texts,
    <fr>  LIKE LINE OF friends.
  DATA _text LIKE LINE OF _cx-texts.
  DATA _cx_l LIKE LINE OF _cx-longtexts.
  DATA _cx_t LIKE LINE OF _cx-texts.
  DATA l_obj_name  TYPE  tadir-obj_name.
  DATA d_type_src  TYPE string.

  obj_name = a0_maindata-clsname.
  obj_version = a0_maindata-version.
  obj_langu = a0_maindata-langu.
  obj_type = c_type.

* Warning recieving (will reset FM_DATA)
  CALL METHOD super->to_data
    RECEIVING
      fm_data = fm_data.

  fm_data-class = a0_maindata-hdr.
  READ TABLE a0_maindata-texts INTO _t
       WITH TABLE KEY lang = a0_maindata-langu.
  IF sy-subrc = 0.    fm_data-class-descript = _t-text.   ENDIF.

* Dynpro : Issue 8
  fm_data-dynpros = dynpros.

* TEXTPools : Issue 1
  fm_data-textspool = textspool.

*LOCAL definitions
  fm_data-locals_def = zaplink_object=>conv_abap_source2raw( local-types-_ ).
  fm_data-locals_imp = zaplink_object=>conv_abap_source2raw( local-implementations-_ ).
  fm_data-locals_mac = zaplink_object=>conv_abap_source2raw( local-macros-_ ).
  fm_data-locals_tst = zaplink_object=>conv_abap_source2raw( local-local_test_class-_ ). " Issue 82

*CLASS_DESC  TYPE tt_SEOCLASSTX,
* Issue 33 : Documentation connected to text
  CALL METHOD td_text
    EXPORTING
      classname     = a0_maindata-clsname
      texts         = a0_maindata-texts
      id            = doc_ids-class-header
    CHANGING
      documentation = fm_data-documentation
      descriptions  = fm_data-class_desc.

  fm_data-inheritance = a0_maindata-inheritance.
  fm_data-inheritance-clsname = a0_maindata-clsname.

*IMPLEMENTINGS  TYPE  SEOR_IMPLEMENTINGS_R
  LOOP AT interfaces ASSIGNING <in>.
    CLEAR _in.
    _in = <in>-hdr.
    _in-clsname = a0_maindata-clsname.
    INSERT _in INTO TABLE fm_data-implementings.

*    LOOP AT <in>-attributes_redefinition ASSIGNING <ia>.
*      _id = <ia>.
    LOOP AT <in>-components ASSIGNING <ia>.
      _id = <ia>-hdr.
      _id-clsname = _in-clsname.
      _id-refclsname = _in-refclsname .
      APPEND _id TO fm_data-impl_details.
      IF <ia>-attvalue IS INITIAL             " Component is a method
         AND <ia>-mtdabstrct IS INITIAL.      " not abstract : with source code
        CLEAR: _ms.

        CONCATENATE <in>-refclsname method_separator <ia>-mtdname INTO _ms-cpdname.
        _ms-redefine = abap_true.
        _ms-source = zaplink_object=>conv_abap_source2raw( <ia>-source-_ ).
        APPEND _ms TO fm_data-method_sources.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

*FRIENDS
*FRIENDSHIPS  TYPE  SEOF_FRIENDSHIPS_R
  LOOP AT friends ASSIGNING <fr>.
    CLEAR _fr.
    _fr = <fr>.
    _fr-clsname = a0_maindata-clsname.
    IF _fr-refclsname IS INITIAL. _fr-refclsname = a0_maindata-clsname. ENDIF.  " do not repeat self : Issue 64
    INSERT _fr INTO TABLE fm_data-friendships.
  ENDLOOP.

*REDEFINITIONS  TYPE  SEOR_REDEFINITIONS_R
*IMPL_DETAILS TYPE  SEOR_REDEFINITIONS_R
  LOOP AT redefinitions ASSIGNING <re>.
    CLEAR: _ms.
    <re>-clsname = a0_maindata-clsname.

    APPEND <re>-hdr TO fm_data-redefinitions.

    _ms-cpdname = <re>-mtdname.
    _ms-redefine = abap_true.
    _ms-source = zaplink_object=>conv_abap_source2raw( <re>-source-_ ).
    APPEND _ms TO fm_data-method_sources.
  ENDLOOP.

* For Exception classes
  LOOP AT exceptions_texts ASSIGNING <cx>.
    CLEAR: _at, _cx.
    _at-clsname = a0_maindata-clsname.    _at-cmpname = <cx>-cmpname.     _at-editorder = <cx>-editorder. " Issue 71.
    IF _at-cmpname IS INITIAL. _at-cmpname = a0_maindata-clsname. ENDIF.  " do not repeat self exception text : Issue 64
    _at-attdecltyp = '2'.   " Constant
    _at-exposure = '2'.     " Public
    _at-state = '1'.        " Implemented
    _at-locked = abap_true.
    IF <cx>-message IS INITIAL.
      _at-typtype = '1'.      " Type
      _at-type = 'SOTR_CONC'.
* Useless :
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = _cx-concept.
      _at-attvalue = sotr2value( _cx-concept ).
    ELSE.
      _at-typtype = '4'.      " Type
      d_type_src = message_cx_type_source.
      REPLACE ALL OCCURRENCES OF REGEX '\#NAME\#' IN d_type_src WITH _at-cmpname.
      REPLACE ALL OCCURRENCES OF REGEX '\#CLASS\#' IN d_type_src WITH <cx>-message-class.
      REPLACE ALL OCCURRENCES OF REGEX '\#NUMBER\#' IN d_type_src WITH <cx>-message-number.
      REPLACE ALL OCCURRENCES OF REGEX '\#A1\#' IN d_type_src WITH <cx>-message-attribut_1.
      REPLACE ALL OCCURRENCES OF REGEX '\#A2\#' IN d_type_src WITH <cx>-message-attribut_2.
      REPLACE ALL OCCURRENCES OF REGEX '\#A3\#' IN d_type_src WITH <cx>-message-attribut_3.
      REPLACE ALL OCCURRENCES OF REGEX '\#A4\#' IN d_type_src WITH <cx>-message-attribut_4.
      _at-typesrc = d_type_src.
    ENDIF.
    _cx-cmpname = _at-cmpname.

    l_obj_name = a0_maindata-clsname.
    CALL FUNCTION 'TRINT_TADIR_QUERY'
      EXPORTING
        iv_pgmid           = c_kind
        iv_object          = c_type
        iv_obj_name        = l_obj_name
      IMPORTING
*     EV_OBJECT          =
*     EV_SRCSYSTEM       =
*     EV_AUTHOR          =
*     EV_GENFLAG         =
*     EV_EXIST           =
        ev_devclass        = _cx-paket.

    _cx-trala_type = 1.
    _cx-crea_lan = a0_maindata-langu.
    _cx-objid_vec = '08000000000000000000'.
    _cx-lhdr = _cx-hdr.
    LOOP AT <cx>-texts ASSIGNING <ct>.
      CLEAR: _cx_t, _cx_l.
      _cx_t-concept = _cx-concept.
      _cx_t-langu = <ct>-langu.
      _cx_t-flag_cntxt = abap_true.
      _cx_t-status = 'R'.
      IF NOT <ct>-long_txt IS INITIAL.
        MOVE-CORRESPONDING _cx_t TO _cx_l.
        _cx_l-text = <ct>-long_txt-_.
        APPEND _cx_l TO _cx-longtexts.
      ENDIF.
      _cx_t-length = 255.
      _cx_t-text = <ct>-short_txt.
      APPEND _cx_t TO _cx-texts.
    ENDLOOP.
    IF NOT _cx-texts IS INITIAL OR NOT _cx-longtexts IS INITIAL.    INSERT _cx INTO TABLE fm_data-exceptions_texts.   ENDIF.
    IF NOT _at IS INITIAL.    INSERT _at INTO TABLE fm_data-attributes.   ENDIF.
  ENDLOOP.
  clear_tags( ).
  endmethod.


  method UPDATE_CONCEPT.
  DATA lt_types TYPE sotr_objects.
  DATA _type    LIKE LINE OF lt_types.
  DATA _paket   TYPE sotr_pack.
  DATA _entries TYPE sotr_text_tt.
  DATA _entry   LIKE LINE OF _entries.
  DATA lt_cx    TYPE tt_cx_texts.
  DATA ls_cx    LIKE LINE OF fm_data-exceptions_texts.
  FIELD-SYMBOLS:
    <at> LIKE LINE OF fm_data-attributes,
    <cx> LIKE LINE OF fm_data-exceptions_texts,
    <lt> LIKE LINE OF <cx>-longtexts.
  DATA _at     LIKE LINE OF fm_data-attributes.
  DATA concept TYPE  sotr_text-concept.
  DATA t_texts TYPE  sotr_textl_tt.
  DATA s_text  LIKE LINE OF t_texts.
  DATA s_sotrl TYPE sotr_link.
  DATA d_cxtxt TYPE string.

  DEFINE mac_handle_cx.
    o_mycx ?= zaplink_cx=>create_from_mf_cx( subrc = sy-subrc
                                         classname = 'ZAPLINK_CX_CONNECTOR'
                                          funcname = &1 ).
    if not msg_col is bound.
      raise exception o_mycx.
    else.
      msg_col->add_symsg( ).
      msg_col->add_exception( exception = o_mycx ).
    endif.
  END-OF-DEFINITION.

  TRY.
    LOOP AT fm_data-exceptions_texts ASSIGNING <cx>.
* Get Type
      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <cx>-objid_vec
        IMPORTING
          OBJECTS          = lt_types
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0 OR lt_types IS INITIAL.
        mac_handle_cx 'SOTR_OBJECT_GET_OBJECTS'.
        CONTINUE.
      ENDIF.
      READ TABLE lt_types INTO _type INDEX 1.

      _paket-paket = <cx>-paket.

      IF NOT <cx>-longtexts IS INITIAL.
        LOOP AT <cx>-longtexts ASSIGNING <lt>.
          CLEAR s_text. MOVE-CORRESPONDING <lt> TO s_text.
          APPEND s_text TO t_texts.
        ENDLOOP.

        CALL FUNCTION 'SOTR_STRING_CREATE_CONCEPT'
          EXPORTING
            paket                               = _paket    " <cx>-lhdr-paket
            crea_lan                            = <cx>-lhdr-crea_lan
*           ALIAS_NAME                          =
*           CATEGORY                            =
            object                              = _type
            entries                             = t_texts
*           FLAG_CORRECTION_ENTRY               =
*           IN_UPDATE_TASK                      =
          IMPORTING
            concept                             = <cx>-lhdr-concept
*           ALIAS                               =
          EXCEPTIONS
            package_missing                     = 1
            crea_lan_missing                    = 2
            object_missing                      = 3
            paket_does_not_exist                = 4
            alias_already_exist                 = 5
            object_type_not_found               = 6
            langu_missing                       = 7
            identical_context_not_allowed       = 8
            text_too_long                       = 9
            error_in_update                     = 10
            no_master_langu                     = 11
            error_in_concept_id                 = 12
            internal_error                      = 13
            concept_not_found                   = 14
            tadir_entry_creation_failed         = 15
            error_in_correction                 = 16
            user_cancelled                      = 17
            no_entry_found                      = 18
            alias_not_allowed                   = 19
            OTHERS                              = 20.
        IF sy-subrc <> 0.
          mac_handle_cx 'SOTR_STRING_CREATE_CONCEPT'.
        ENDIF.
      ENDIF.

      _entries = <cx>-texts.
      MODIFY _entries FROM _entry TRANSPORTING concept WHERE concept <> _entry-concept.
      ls_cx = <cx>.
      TRY.
          CALL FUNCTION 'SOTR_CREATE_CONCEPT'
            EXPORTING
              paket                               = _paket
              crea_lan                            = <cx>-crea_lan
              alias_name                          = <cx>-alias_name
*                CATEGORY                            =
              object                              = _type
              entries                             = _entries
*                FLAG_CORRECTION_ENTRY               =
*                IN_UPDATE_TASK                      =
              concept_default                     = <cx>-concept
*                FLAG_COPY_TEXT                      =
            IMPORTING
              concept                             = ls_cx-concept
*               NEW_ENTRIES                         =
            EXCEPTIONS
              package_missing                     = 1
              crea_lan_missing                    = 2
              object_missing                      = 3
              paket_does_not_exist                = 4
              alias_already_exist                 = 5
              object_type_not_found               = 6
              langu_missing                       = 7
              identical_context_not_allowed       = 8
              text_too_long                       = 9
              error_in_update                     = 10
              no_master_langu                     = 11
              error_in_concept_id                 = 12
              alias_not_allowed                   = 13
              tadir_entry_creation_failed         = 14
              internal_error                      = 15
              error_in_correction                 = 16
              user_cancelled                      = 17
              no_entry_found                      = 18
              OTHERS                              = 19.
        CATCH cx_root.
*        CALL METHOD msg_col->add_warning

* Concept can't be created try build new one
          CALL FUNCTION 'SOTR_CREATE_CONCEPT'
            EXPORTING
              paket                               = _paket
              crea_lan                            = <cx>-crea_lan
              alias_name                          = <cx>-alias_name
*       CATEGORY                            =
              object                              = _type
              entries                             = _entries
*       FLAG_CORRECTION_ENTRY               =
*       IN_UPDATE_TASK                      =
*       CONCEPT_DEFAULT                     =
*       FLAG_COPY_TEXT                      =
            IMPORTING
              concept                             = ls_cx-concept
*       NEW_ENTRIES                         =
            EXCEPTIONS
              package_missing                     = 1
              crea_lan_missing                    = 2
              object_missing                      = 3
              paket_does_not_exist                = 4
              alias_already_exist                 = 5
              object_type_not_found               = 6
              langu_missing                       = 7
              identical_context_not_allowed       = 8
              text_too_long                       = 9
              error_in_update                     = 10
              no_master_langu                     = 11
              error_in_concept_id                 = 12
              alias_not_allowed                   = 13
              tadir_entry_creation_failed         = 14
              internal_error                      = 15
              error_in_correction                 = 16
              user_cancelled                      = 17
              no_entry_found                      = 18
              OTHERS                              = 19.
      ENDTRY.
      IF sy-subrc <> 0.
        mac_handle_cx 'SOTR_CREATE_CONCEPT'.
        CONTINUE.
      ENDIF.

      IF <cx>-lhdr-concept <> ls_cx-concept AND NOT <cx>-lhdr-concept IS INITIAL.
        s_sotrl-concept_1 = ls_cx-concept.    " FLAG_STR_1
        s_sotrl-concept_2 = <cx>-lhdr-concept.    s_sotrl-flag_str_2 = abap_true.
        CALL FUNCTION 'SOTR_LINK_UPDATE_LINK'
          EXPORTING
            sotr_link         = s_sotrl
          EXCEPTIONS
            incomplete_data   = 1
            concept_not_found = 2
            OTHERS            = 3.
        IF sy-subrc <> 0.
          mac_handle_cx 'SOTR_LINK_UPDATE_LINK'.
        ENDIF.

      ENDIF.

* Update Attribute
      _at-clsname = fm_data-class-clsname.    _at-cmpname = <cx>-cmpname.
      IF <cx>-cmpname IS INITIAL.
        READ TABLE fm_data-attributes ASSIGNING <at>
             WITH KEY cmpname = fm_data-class-clsname.
      ELSE.
        READ TABLE fm_data-attributes ASSIGNING <at>
             WITH KEY cmpname = <cx>-cmpname.
      ENDIF.
      IF sy-subrc <> 0.
        d_cxtxt = 'Attribut should exists !!'(f01).
        RAISE EXCEPTION TYPE zaplink_cx_connector
           EXPORTING
             textid = zaplink_cx=>system_error
             cx_name = d_cxtxt.
      ENDIF.
      <at>-attvalue = sotr2value( ls_cx-concept ).
*        CLEAR: _at.
*        _at-clsname = fm_data-class-clsname.    _at-cmpname = <cx>-cmpname.
*        IF _at-cmpname IS INITIAL. _at-cmpname = fm_data-class-clsname. ENDIF.  " do not repeat self exception text : Issue 64
*        _at-attdecltyp = '2'.   " Constant
*        _at-exposure = '2'.     " Public
*        _at-state = '1'.        " Implemented
*        _at-locked = abap_true.
*        _at-typtype = '1'.      " Type
*        _at-type = 'SOTR_CONC'.
*        _at-attvalue = sotr2value( ls_cx-concept ).
*        INSERT _at INTO TABLE fm_data-attributes.

      CHECK ls_cx-concept <> <cx>-concept.

      INSERT ls_cx INTO TABLE lt_cx.
      DELETE fm_data-exceptions_texts.
    ENDLOOP.

    INSERT LINES OF fm_data-exceptions_texts INTO TABLE lt_cx.
    fm_data-exceptions_texts = lt_cx.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method VALUE2SOTR.
  DATA _val TYPE seovalue.

  _val = data.
  REPLACE ALL OCCURRENCES OF '''' IN _val WITH space.
  CONDENSE _val.
  result = _val.
  endmethod.
ENDCLASS.
