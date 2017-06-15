class ZAPLINK_DOCUMENTATION definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.

  types TO_LOG type ref to ZAPLINK_MESSAGE_COLLECTOR .

  constants NODE_NAME type STRING value 'DOCS' ##NO_TEXT.
  class-data LOG type TO_MSG_COLL .
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_DOCUMENTATION' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_SINGLE
    importing
      !IDS type TD_DOC_ID
      !OBJECT type DOKU_OBJ
    returning
      value(RESULT) type TS_DOC
    raising
      ZAPLINK_CX .
  class-methods GET
    importing
      !IDS type TR_DOCID
      !OBJECT type DOKU_OBJ
    returning
      value(T_DOCS) type TT_DOCS
    raising
      ZAPLINK_CX .
  class-methods SET
    importing
      !T_DOCS type TT_DOCS
    raising
      ZAPLINK_CX .
  class-methods SET_SINGLE
    importing
      !DATA type TS_DOC
    raising
      ZAPLINK_CX .

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

  types TO_DOC type ref to ZAPLINK_DOCV_DATA .
  types:
    BEGIN OF ts_name_mask,
      ID 	    TYPE DOKIL-ID,
      OBJECT  TYPE DOKIL-OBJECT,
    END OF ts_name_mask .
private section.

  constants:
    BEGIN OF supportedtypes,                                  "#EC NOTEXT
      general           TYPE td_comptype VALUE 'DOCT',    " General Text
      Indep             TYPE td_comptype VALUE 'DOCV',    " Documentation (Independent)
    END OF supportedtypes .
  constants _UUID type TD_CONNUUID value '3D34954BEABE5F12E1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.

  class-methods RESET .
  class-methods RAW2STRING
    importing
      !RAW type TT_RAWTEXT
    returning
      value(STRING) type STRING .
  class-methods STRING2RAW
    importing
      !STRING type STRING
    returning
      value(RAW) type TT_RAWTEXT .
  class-methods DOCU_GET
    importing
      !DOCU type DOKIL
    exporting
      !HEAD type TS_HEAD
      !LANG type TS_LANG
    raising
      ZAPLINK_CX_CONNECTOR .
  class-methods NAME_2_MASK
    importing
      !TYPE type TD_COMPTYPE
      !NAME type TD_COMPNAME
    returning
      value(RESULT) type TS_NAME_MASK .
ENDCLASS.



CLASS ZAPLINK_DOCUMENTATION IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  mac_create_log log ballog_subobject space.
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  super->constructor( ).
  mac_create_log application_log ballog_subobject space.
  type-type = supportedtypes-general. INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-indep.   INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method DOCU_GET.
  DATA _text  TYPE tt_rawtext.

  TRY.
    head-id = docu-id.
    head-object = docu-object.
    head-masterlang = docu-masterlang.
    head-langu = docu-langu.
    head-typ = docu-typ.
    head-selfdef = docu-selfdef.
    IF docu-selfdef = space.
      CALL FUNCTION 'DOCU_GET'
        EXPORTING
*       EXTEND_EXCEPT                = ' '
          id                           = docu-id
          langu                        = docu-langu
          object                       = docu-object
          typ                          = docu-typ
          version                      = docu-version
          version_active_or_last       = 'A'
*         PRINT_PARAM_GET              = 'X'
        IMPORTING
          dokstate                     = lang-state
*         DOKTITLE                     =
          head                         = lang-head
*        doktyp                       = head-typ
        TABLES
          line                         = _text
        EXCEPTIONS
          no_docu_on_screen            = 1
          no_docu_self_def             = 2
          no_docu_temp                 = 3
          ret_code                     = 4
          OTHERS                       = 5.
      IF sy-subrc <> 0.
        lmac_add_mf_and_raise 'DOCU_GET' sy-subrc.
      ENDIF.
      lang-_ = raw2string( _text ).
      head-application = lang-tdobject.
      head-dokform = lang-tdform.
      head-dokstyle = lang-tdstyle.
      CLEAR:
        lang-tdobject,
        lang-tdname,
        lang-tdid,
        lang-tdform,
        lang-tdstyle.
    ENDIF.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method GET.
  DATA _obj   TYPE RANGE OF doku_obj.
  DATA my_obj LIKE LINE OF _obj.
  DATA docs   TYPE STANDARD TABLE OF dokil WITH DEFAULT KEY.
  DATA doc    LIKE LINE OF t_docs.
  DATA _lang  LIKE LINE OF doc-texts.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF docs.

  IF ids IS INITIAL.
    EXIT.
  ENDIF.

  my_obj-sign = 'I'. my_obj-option = 'CP'.
  my_obj-low = object. APPEND my_obj TO _obj.

  SELECT * INTO TABLE docs
    FROM dokil
    WHERE id IN ids
      AND object IN _obj.

  IF docs IS INITIAL.
    EXIT.
  ENDIF.

  reset( ).

  SORT docs BY id object masterlang DESCENDING langu.

  LOOP AT docs ASSIGNING <d>.
    AT NEW object. " ID + OBJECT
      CLEAR doc.
      CALL METHOD docu_get
        EXPORTING
          docu = <d>
        IMPORTING
          head = doc-hdr
          lang = _lang.
    ENDAT.

    IF _lang IS INITIAL.
      CALL METHOD docu_get
        EXPORTING
          docu   = <d>
        IMPORTING
*          head   = _head
          lang   = _lang.
    ENDIF.
    IF NOT _lang IS INITIAL.
      CLEAR: _lang-tdversion, _lang-tdfuser, _lang-tdfdate, _lang-tdftime, _lang-tdluser, _lang-tdldate, _lang-tdltime.   " Issue 68 : Remove useless data
      INSERT _lang INTO TABLE doc-texts.
      CLEAR _lang.
    ENDIF.

    AT END OF object. " ID + OBJECT
      APPEND doc TO t_docs.
    ENDAT.
  ENDLOOP.
  endmethod.


  method GET_SINGLE.
  DATA r_ids  TYPE tr_docid.
  DATA my_id  LIKE LINE OF r_ids.
  DATA t_docs TYPE tt_docs.

  my_id-sign = 'I'. my_id-option = 'EQ'.    my_id-low = ids. APPEND my_id TO r_ids.
  t_docs = get( ids = r_ids
             object = object ).
  IF NOT t_docs IS INITIAL.   READ TABLE t_docs INTO result INDEX 1.    ENDIF.
  endmethod.


  method NAME_2_MASK.
  CASE type.
    WHEN supportedtypes-general. result-id = 'TX'.    result-object = name.
    WHEN supportedtypes-indep.   result = name.
  ENDCASE.
  endmethod.


  method RAW2STRING.
FIELD-SYMBOLS <l> LIKE LINE OF raw.
  DATA str TYPE string.

  LOOP AT raw ASSIGNING <l>.
    str = <l>.
    IF string IS INITIAL.
      string = str.
    ELSE.
      CONCATENATE string line_separator str INTO string.
    ENDIF.
  ENDLOOP.
  endmethod.


  method RESET.
  DATA object	TYPE balobj_d.
  DATA subobject  TYPE balsubobj.
  DATA ext_id	TYPE balnrext.

  IF NOT log IS BOUND.    EXIT.   ENDIF.

  object = zaplink_connector=>ballog_object.
  subobject	= 'DOCT'.
*  CONCATENATE sy-uname sy-datum sy-uzeit INTO ext_id SEPARATED BY space.
*01    Create
*02    Change
*03    Display
*04    Modify (Direct Input: Create/Change)
*06    Delete
*11    Create defaults
*12    Change defaults
*13    Display defaults
  CALL METHOD log->init
    EXPORTING
      id_object      = object
      id_subobject   = subobject
      id_extnumber   = ext_id
      auto_upd_custo = abap_true
      id_activity    = zaplink_message_collector=>actions-create
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* ignore
  ENDIF.
  endmethod.


  method SET.
  DATA _text  TYPE tt_rawtext.
  DATA _dokil TYPE dokil.
  DATA _head  TYPE thead.
  FIELD-SYMBOLS:
    <o> LIKE LINE OF t_docs,
    <t> LIKE LINE OF <o>-texts.

  IF t_docs IS INITIAL.
    EXIT. "nothing to do
  ENDIF.

  LOOP AT t_docs ASSIGNING <o>.
    _dokil-id = <o>-id.
    _dokil-object = <o>-object.
    _dokil-selfdef = <o>-selfdef.

    CALL FUNCTION 'DOCU_SELFDEF_WRITE'
      EXPORTING
        object_id   = _dokil-id
        object_name = _dokil-object
        selfdef     = _dokil-selfdef
      EXCEPTIONS
        not_deleted = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      lmac_add_mf_and_raise 'DOCU_SELFDEF_WRITE' sy-subrc.
    ENDIF.
    LOOP AT <o>-texts ASSIGNING <t>.
      _text = string2raw( <t>-_  ).
      _head = <t>-head.
*    head-application = lang-tdobject.
*    head-dokform = lang-tdform.
*    head-dokstyle = lang-tdstyle.
      _head-tdobject = <o>-application.
      _head-tdname = <o>-object.
      _head-tdid = <o>-id.
      _head-tdform = <o>-dokform.
      _head-tdstyle = <o>-dokstyle.

      IF <o>-selfdef IS INITIAL.
        CALL FUNCTION 'DOCU_UPDATE'
          EXPORTING
*       ACTCLASS            = ' '
            head                = _head
*       NO_MASTERLANG       = ' '
            state               = <t>-state
            typ                 = <o>-typ
            version             = 0
          TABLES
            line                = _text.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  endmethod.


  method SET_SINGLE.
  DATA t_docs TYPE tt_docs.

  append data to t_docs.
  set( t_docs ).
  endmethod.


  method STRING2RAW.
SPLIT string AT LINE_SEPARATOR
        INTO TABLE raw.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_doc TYPE to_doc.

  CREATE OBJECT object.
  CASE type.
    WHEN supportedtypes-general or supportedtypes-indep.
      CREATE OBJECT o_doc.
      object->raw = o_doc.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA pool     TYPE rs38l-area.
  DATA type     TYPE td_comptype.
  DATA name     TYPE td_compname.
  DATA s_mask   TYPE ts_name_mask.
  DATA d_ori_modified TYPE c.
  DATA d_doc_type TYPE doku_typ.
  DATA s_dokil  TYPE dokil.
  DATA s_head   TYPE thead.

  TRY.
    type = component->get_type( ).
    name = component->get_name( ).
    CASE type.
      WHEN supportedtypes-general OR supportedtypes-indep.
        CASE type.
          WHEN supportedtypes-general. s_mask-id = 'TX'.    s_mask-object = name. " Issue 60
          WHEN supportedtypes-indep.   s_mask = name.
        ENDCASE.
* FROM MSDCUI01 : MODULE d0100_get_docuobject (583)
        CALL FUNCTION 'EXT_MODIFY_ORIGINAL_CHECK'
          EXPORTING
            p_id                = s_mask-id
            p_object            = s_mask-object
          IMPORTING
            p_original_modified = d_ori_modified
          EXCEPTIONS
            OTHERS              = 1.
        IF d_ori_modified = abap_true."Modi. vorh.
          d_doc_type = 'M'.
        ELSEIF s_mask-id = 'FU' OR s_mask-id = 'FX'.
          d_doc_type = 'T'.
        ELSE.
          d_doc_type = 'E'.
        ENDIF.

        SELECT SINGLE * INTO s_dokil
          FROM dokil
          WHERE id = s_mask-id
            AND object = s_mask-object
            AND typ = d_doc_type
            AND masterlang = abap_true.
        IF sy-subrc <> 0.
          SELECT SINGLE * INTO s_dokil
            FROM dokil
            WHERE id = s_mask-id
              AND object = s_mask-object
              AND typ = d_doc_type.
        ENDIF.

        s_head-tdid     = s_mask-id.
        s_head-tdname   = s_mask-object.
        s_head-tdspras  = s_dokil-langu.

        CALL FUNCTION 'ENQUEUE_ESDOC'
          EXPORTING
*             MODE_DOKHL           = 'E'
            id     = s_dokil-id
            object = s_dokil-object
            langu  = s_dokil-langu
            typ    = s_dokil-typ
*             _SCOPE               = '2'
*             _WAIT                = ' '
          EXCEPTIONS
            foreign_lock         = 1
            system_failure       = 2
            OTHERS               = 3.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise 'ENQUEUE_ESDOC' sy-subrc.
        ENDIF.

        CALL FUNCTION 'DOCU_DELETE'
          EXPORTING
*             ACTCLASS              = ' '
*             DELETE_RAW_ONLY       = ' '
            head                  = s_head
            typ                   = d_doc_type
            version               = s_dokil-version.
*        IF sy-subrc <> 0.
*          mac_add_mf_and_raise 'DOCU_DELETE' sy-subrc.
*        ENDIF.
        CALL FUNCTION 'DEQUEUE_ESDOC'
         EXPORTING
*             MODE_DOKHL         = 'E'
*             _SCOPE             = '3'
            id     = s_dokil-id
            object = s_dokil-object
            langu  = s_dokil-langu
            typ    = s_dokil-typ.

        SELECT SINGLE * INTO s_dokil
          FROM dokil
          WHERE id = s_mask-id
            AND object = s_mask-object
            AND typ = d_doc_type.
        IF sy-subrc <> 0.   result = abap_true.   ENDIF.

      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
* Issue 137 => Remove from Working Area
  zaplink_tools=>remove_comp_from_working_area( type = type
                                                name = name ).
  endmethod.


  method ZAPLINK_CONNECTOR~DO_EXISTS.
  DATA type     TYPE td_comptype.
  DATA s_mask   TYPE ts_name_mask.
  DATA d_name   TYPE td_compname.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN supportedtypes-general OR supportedtypes-indep..
        d_name = component->get_name( ).
        s_mask = name_2_mask( type = type    name = d_name ).
        SELECT SINGLE id INTO s_mask-id
          FROM dokil
          WHERE     id = s_mask-id
            AND object = s_mask-object.
        IF sy-subrc = 0.    exists = me->exists-exists.   ENDIF.
*          CALL FUNCTION 'DOCU_INIT'
*            EXPORTING
*              id            = s_mask-id
*              langu         =
*              object        = s_mask-object
*              typ           =
*            IMPORTING
**             FOUND         =
*              XDOKIL        = s_dokil.
*          IF not s_dokil is INITIAL.    exists = me->exists-exists.   ENDIF.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~IS_SUPPORTED_VERSION.
  result = abap_false.
  if version = _ver.    result = abap_true.   endif.
  endmethod.


  method ZAPLINK_CONNECTOR~READ_FROM_SAP.
  DATA o_data   TYPE to_doc.
  DATA d_name   TYPE td_compname.
  DATA type     TYPE td_comptype.
  DATA s_mask   TYPE ts_name_mask.
  DATA s_doc    TYPE ts_doc.

  TRY.
    type = component->get_type( ).
    CREATE OBJECT object.
    object->set_component( component ).
    CASE type.
      WHEN supportedtypes-general OR supportedtypes-indep.
        d_name = component->get_name( ).
        s_mask = name_2_mask( type = type    name = d_name ).
        s_doc = get_single( ids = s_mask-id
                         object = s_mask-object ).

        CREATE OBJECT o_data.
        o_data->from_data( s_doc ).
        CLEAR: o_data->a0_maindata-id, o_data->a0_maindata-object.
        object->raw = o_data.
      WHEN OTHERS.
        CLEAR object.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.
    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA o_data   TYPE to_doc.
  DATA o_comp   TYPE to_component.
  DATA s_mask   TYPE ts_name_mask.
  DATA s_doc    TYPE ts_doc.

  TRY.
      CASE object->type.
        WHEN supportedtypes-general OR supportedtypes-indep.
          o_data ?= object->raw.
          CASE object->type.
            WHEN supportedtypes-general. s_mask-id = 'TX'.    s_mask-object = object->name.
            WHEN supportedtypes-indep.   s_mask = object->name.
          ENDCASE.
          o_data->a0_maindata-id = s_mask-id.   o_data->a0_maindata-object = s_mask-object.
          s_doc = o_data->to_data( ).
          set_single( s_doc ).
          CREATE OBJECT o_comp.
          o_comp->set_type( object->type ).   o_comp->set_name( object->name ).
          CREATE OBJECT components.
          components->add( o_comp ).
        WHEN OTHERS.
          mac_raise_type_not_supported me->class_name object->type.
      ENDCASE.
      check_component_list( EXPORTING     object = object
                             CHANGING components = components ). " Issue 92
    CATCH zaplink_cx_connector INTO o_mycx.
      ROLLBACK WORK.
      RAISE EXCEPTION o_mycx.
    CATCH zaplink_cx INTO o_cx.
      ROLLBACK WORK.
      mac_cascade_raise o_mycx o_cx.
  ENDTRY.
  endmethod.
ENDCLASS.
