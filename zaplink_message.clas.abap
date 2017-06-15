class ZAPLINK_MESSAGE definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.

  types TD_RAW____ type RSWSOURCET .
  types:
    begin of ts_text,
      langu   type SPRAS,
      ENTRY	  type TEXTPOOLTX,
      LENGTH  type TEXTPOOLLN,
    end of ts_text .
  types:
    tt_texts type SORTED TABLE OF ts_text WITH UNIQUE KEY langu .
  types:
    begin of ts_textpool,
      ID    type  TEXTPOOLID,
      KEY	  type  TEXTPOOLKY,
      texts type  tt_texts,
    end of ts_textpool .
  types:
    TT_TEXTPOOLs type SORTED TABLE OF ts_textpool WITH UNIQUE key ID KEY .
  types TS_SOURCE type STRING .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_MESSAGE' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE
    redefinition .
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

  types TO_DATA type ref to ZAPLINK_MSAG_DATA .
private section.

  types:
    BEGIN OF ts_prog_attr.
  INCLUDE TYPE ts_base_attributs AS base.
  TYPES:
    END OF ts_prog_attr .

  constants ST_MSGCLASS type TD_COMPTYPE value 'MSAG' ##NO_TEXT.
  class-data R_DOC_ID type TR_DOCID .
  constants _UUID type TD_CONNUUID value '1FAC0A4B07A5A05AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
  constants C_MSG_ID type TD_DOC_ID value 'NA' ##NO_TEXT.
ENDCLASS.



CLASS ZAPLINK_MESSAGE IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
*table TDCLT
*DOKCLASS DOKTITEL
*NA       Message
  DATA _id LIKE LINE OF r_doc_id.

  _id-sign = 'I'. _id-option = 'EQ'.
  _id-low = C_MSG_ID. APPEND _id TO r_doc_id.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = st_msgclass. INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_msag TYPE to_data.

  CHECK object IS BOUND.

  o_msag ?= object->raw.
  o_msag->anonymize( ).

  TRY.
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE.
  DATA o_msag TYPE to_data.

  TRY.
      super->zaplink_cnx_ext_cleaner~unanonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.

  o_msag ?= object->raw.
  o_msag->unanonymize( ).
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_data TYPE to_data.

  CREATE OBJECT object.
  CASE type.
    WHEN st_msgclass.
      CREATE OBJECT o_data.
      object->raw = o_data.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA _name TYPE t100a-arbgb.
  DATA type  TYPE td_comptype.
  DATA name  TYPE td_compname.

  TRY.

    type = component->get_type( ).
    CASE type.
      WHEN st_msgclass.
        _name = name = component->get_name( ).
        CALL FUNCTION 'RS_DELETE_MESSAGE_ID'
          EXPORTING
            nachrichtenklasse = _name
          EXCEPTIONS
            not_executed      = 1
            not_found         = 2
            no_permission     = 3
            OTHERS            = 4.
        IF sy-subrc <> 0. " SAP NameSpace
          CASE sy-subrc.
            WHEN 2.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>not_found.
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zaplink_cx_connector
                EXPORTING textid = zaplink_cx_connector=>system_error.
          ENDCASE.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  result = abap_true.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = type
                                                name = name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA _name TYPE t100a-arbgb.
  DATA type  TYPE td_comptype.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN st_msgclass.
        _name = component->get_name( ).
        SELECT SINGLE arbgb INTO _name
          FROM t100a
          WHERE arbgb = _name.
        IF sy-subrc = 0.
          exists = me->exists-exists.
        ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.
    result = abap_true.
  endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA _data  TYPE to_data.
  DATA _obj   TYPE doku_obj.
  DATA t_t100 TYPE SORTED TABLE OF t100 WITH UNIQUE KEY msgnr sprsl.
  DATA _name  TYPE arbgb.
  DATA t_docs TYPE zaplink_documentation=>tt_docs.
  FIELD-SYMBOLS:
    <l> LIKE LINE OF t_docs,
    <lt> LIKE LINE OF <l>-texts,
    <t> LIKE LINE OF t_t100,
    <m> LIKE LINE OF _data->messages,
    <mt> LIKE LINE OF <m>-texts.

  DATA _text LIKE LINE OF <m>-texts.
  DATA _htxt LIKE LINE OF _data->texts.
  DATA _key  TYPE doku_obj.
  DATA d_type TYPE td_comptype.

  TRY.

    CREATE OBJECT object.
    d_type = component->get_type( ).
    CASE d_type.
      WHEN st_msgclass.
        _name = component->get_name( ).
* Documentation
* example : ZAPLINK_EASYXML000
        CONCATENATE _name '+++' INTO _obj.  " +++ stand for message number
        t_docs = zaplink_documentation=>get( ids = r_doc_id
                                          object = _obj ).
        DELETE t_docs WHERE texts IS INITIAL.

        CREATE OBJECT _data.
* Main data
        SELECT SINGLE *
          INTO CORRESPONDING FIELDS OF _data->a0_maindata
          FROM t100a
          WHERE arbgb = _name.

* Descriptions
        SELECT sprsl stext
          INTO TABLE _data->texts
          FROM t100t
          WHERE arbgb = _name
            AND stext <> space.        " ignore null

        READ TABLE _data->texts TRANSPORTING NO FIELDS
             WITH TABLE KEY langu = _data->a0_maindata-masterlang.
        IF sy-subrc <> 0.
          _htxt-langu = _data->a0_maindata-masterlang.
          _htxt-short_txt = _data->a0_maindata-stext.
          INSERT _htxt INTO TABLE _data->texts.
        ENDIF.
        CLEAR _data->a0_maindata-stext.

* Messages
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE _data->messages
          FROM t100u
          WHERE arbgb = _name.
* Messages texts
        SELECT * INTO TABLE t_t100
          FROM t100
          WHERE arbgb = _name
            AND text <> space.          " ignore null

        LOOP AT _data->messages ASSIGNING <m>.
          LOOP AT t_t100 ASSIGNING <t>
                  WHERE msgnr = <m>-msgnr.
            CLEAR _text.
            _text-langu = <t>-sprsl.
            _text-short_txt = <t>-text.
            INSERT _text INTO TABLE <m>-texts.
          ENDLOOP.

          CONCATENATE _name <m>-msgnr INTO _key.
          LOOP AT t_docs ASSIGNING <l>
                  WHERE object = _key.

            IF <m>-application IS INITIAL.
              <m>-application = <l>-application.
            ELSE.
              ASSERT <m>-application = <l>-application.
            ENDIF.

            IF <m>-typ IS INITIAL.
              <m>-typ = <l>-typ.
            ELSE.
              ASSERT <m>-typ = <l>-typ.
            ENDIF.

            IF <m>-dokform IS INITIAL.
              <m>-dokform = <l>-dokform.
            ELSE.
              ASSERT <m>-dokform = <l>-dokform.
            ENDIF.

            IF <m>-dokstyle IS INITIAL.
              <m>-dokstyle = <l>-dokstyle.
            ELSE.
              ASSERT <m>-dokstyle = <l>-dokstyle.
            ENDIF.

            LOOP AT <l>-texts ASSIGNING <lt>.

              READ TABLE <m>-texts ASSIGNING <mt>
                   WITH TABLE KEY langu = <lt>-tdspras.
              IF sy-subrc = 0.
                <mt>-long_txt = <lt>.
                CLEAR <mt>-long_txt-tdspras.
              ELSE.
                CLEAR _text.
                _text-long_txt = <lt>.
                CLEAR _text-long_txt-tdspras.
                INSERT _text INTO TABLE <m>-texts.
              ENDIF.
            ENDLOOP.

          ENDLOOP.
          IF <m>-texts IS INITIAL.    DELETE _data->messages.     ENDIF.
        ENDLOOP.

        object->set_component( component ).
        CLEAR _data->a0_maindata-arbgb.
        object->raw = _data.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name d_type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA _data      TYPE to_data.
  DATA o_comp     TYPE to_component.
  DATA _name      TYPE td_compname.
  DATA t_docs     TYPE zaplink_documentation=>tt_docs.
  DATA ls_doc     LIKE LINE OF t_docs.
  DATA: lt_t100   TYPE STANDARD TABLE OF t100,
        ls_t100   LIKE LINE OF lt_t100,
        lt_t100t  TYPE STANDARD TABLE OF t100t,
        ls_t100t  LIKE LINE OF lt_t100t,
        lt_t100u  TYPE STANDARD TABLE OF t100u,
        ls_t100u  LIKE LINE OF lt_t100u,
        ls_t100a  TYPE t100a.
  FIELD-SYMBOLS:
    <m> LIKE LINE OF _data->messages,
    <l> LIKE LINE OF <m>-texts,
    <t> LIKE LINE OF _data->texts.

  TRY.

    CREATE OBJECT components.                         " Issue 92
    CASE object->type.
      WHEN st_msgclass.
        _data ?= object->raw.
* Main data
        _data->a0_maindata-arbgb = object->name.
        MOVE-CORRESPONDING _data->a0_maindata TO ls_t100a.
* description
        LOOP AT _data->texts ASSIGNING <t>.
          CLEAR ls_t100t.
          ls_t100t-arbgb = _data->a0_maindata-arbgb.
          ls_t100t-sprsl = <t>-langu.
          ls_t100t-stext = <t>-short_txt.
          APPEND ls_t100t TO lt_t100t.
          IF <t>-langu = _data->a0_maindata-masterlang.
* Restore text
            ls_t100a-stext = <t>-short_txt.
          ENDIF.
        ENDLOOP.

* Messages
        LOOP AT _data->messages ASSIGNING <m>.
          CLEAR ls_t100u.
          MOVE-CORRESPONDING <m> TO ls_t100u.
          ls_t100u-arbgb = _data->a0_maindata-arbgb.
          APPEND ls_t100u TO lt_t100u.

          CLEAR ls_doc.
          ls_doc-application = <m>-application.
          ls_doc-id = c_msg_id.
          CONCATENATE _data->a0_maindata-arbgb <m>-msgnr INTO ls_doc-object.
*        ls_doc-langu = <m>-langu.
*        ls_doc-masterlang = <m>-masterlang.
          ls_doc-typ = <m>-typ.
          ls_doc-dokform = <m>-dokform.
          ls_doc-dokstyle = <m>-dokstyle.
          ls_doc-selfdef = <m>-selfdef.

          CLEAR ls_t100.
          MOVE-CORRESPONDING <m> TO ls_t100.
          MOVE-CORRESPONDING ls_t100u TO ls_t100.
          LOOP AT <m>-texts ASSIGNING <l>.
            IF NOT <l>-short_txt IS INITIAL.
              ls_t100-sprsl = <l>-langu.
              ls_t100-text = <l>-short_txt.
              MOVE-CORRESPONDING <l> TO ls_t100.
              APPEND ls_t100 TO lt_t100.
            ENDIF.
* Long texts
            IF NOT <l>-long_txt IS INITIAL.
              <l>-long_txt-tdspras = <l>-langu.
              INSERT <l>-long_txt INTO TABLE ls_doc-texts .
            ENDIF.
          ENDLOOP.
          IF NOT ls_doc-texts IS INITIAL.
            INSERT ls_doc INTO TABLE t_docs.
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'RS_ACCESS_PERMISSION'
          EXPORTING
            mode         = 'FREE'
            object       = ls_t100a-arbgb
            object_class = 'T100'.

*--- Check permission
        CALL FUNCTION 'RS_ACCESS_PERMISSION'
          EXPORTING
            authority_check          = 'X'
            global_lock              = 'X'
            mode                     = 'INSERT'
            language_upd_exit        = 'UPDATE_MASTER'
            master_language          = ls_t100a-masterlang
            object                   = ls_t100a-arbgb
            object_class             = 'T100'
            suppress_language_check  = ' '
          EXCEPTIONS
            canceled_in_corr         = 01
            enqueued_by_user         = 02
            enqueue_system_failure   = 03
            illegal_parameter_values = 04
            locked_by_author         = 05
            no_modify_permission     = 06
            no_show_permission       = 07
            permission_failure       = 08.
        CASE sy-subrc.
          WHEN 0.
          WHEN 2 OR 5.
            RAISE EXCEPTION TYPE zaplink_cx_connector
                  EXPORTING
                      name = _name
                      type = object->type
                    textid = zaplink_cx_connector=>object_locked.
          WHEN 6 OR 7 OR 8 OR 9.
            RAISE EXCEPTION TYPE zaplink_cx_connector
                  EXPORTING
                      name = _name
                      type = object->type
                    textid = zaplink_cx_connector=>not_authorized.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zaplink_cx_connector
                  EXPORTING
                      name = _name
                      type = object->type
                    textid = zaplink_cx_connector=>system_error.
        ENDCASE.

*      CALL FUNCTION 'RS_CORR_INSERT'
*           EXPORTING
*                author              = sy-uname
*                global_lock         = 'X'
*                object              = ls_t100a-arbgb
*                object_class        = 'T100'
*                devclass            = '$TMP'
*                master_language     = sy-langu
*                mode                = 'INSERT'
**       IMPORTING
**            AUTHOR              = UNAME
**            KORRNUM             = CORRNUMBER_LOCAL
**            DEVCLASS            = DEVCLASS_LOCAL
*           EXCEPTIONS
*                cancelled           = 1
*                permission_failure  = 2
*                unknown_objectclass = 3.
*
*      IF sy-subrc <> 0.
*        CASE sy-subrc.
*          WHEN 2.
*            RAISE EXCEPTION TYPE zaplink_cx_connector
*                  EXPORTING
*                      name = _name
*                      type = object->type
*                  devclass = '$TMP'
*                    textid = zaplink_cx_connector=>not_authorized.
*          WHEN OTHERS.
*            RAISE EXCEPTION TYPE zaplink_cx_connector
*                  EXPORTING
*                      name = _name
*                      type = object->type
*                    textid = zaplink_cx_connector=>system_error.
*        ENDCASE.
*      ENDIF.

        MODIFY t100a FROM ls_t100a.
        MODIFY t100t FROM TABLE lt_t100t.
        MODIFY t100u FROM TABLE lt_t100u.
        MODIFY t100 FROM TABLE lt_t100.

* Documentation
        CALL METHOD zaplink_documentation=>set
          EXPORTING
            t_docs = t_docs.

*        CREATE OBJECT _comp.
*        _comp->set_type( st_msgclass ).
*        _name = _data->a0_maindata-arbgb.
*        _comp->set_name( _name ).
*        CREATE OBJECT components.
*        components->add( _comp ).
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name object->type.
    ENDCASE.
    check_component_list( EXPORTING     object = object
                           CHANGING components = components ). " Issue 92
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.
ENDCLASS.
