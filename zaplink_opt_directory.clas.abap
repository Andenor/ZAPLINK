class ZAPLINK_OPT_DIRECTORY definition
  public
  create public .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases BALLOG_OBJECT
    for ZAPLINK_DATATYPES~BALLOG_OBJECT .
  aliases TD_SUBSTITUTIONKIND
    for ZAPLINK_DATATYPES~TD_SUBSTITUTIONKIND .
  aliases TS_DIRECTORY
    for ZAPLINK_DATATYPES~TS_DIRECTORY .

  types TO_COMPONENT type ref to ZAPLINK_RAW_BASE .
  types TO_DEVCLASS type ref to ZAPLINK_OPT_DEVCLASS .
  types TO_EXCEPTION type ref to ZAPLINK_CX_OPTIONS .
  types TO_LOG type ref to ZAPLINK_MESSAGE_COLLECTOR .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_TR type ref to ZAPLINK_OPT_TRANSPORT_REQUEST .

  data APPLICATION_LOG type TO_LOG read-only .
  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'DIRECTORY' ##NO_TEXT.
  constants:
    BEGIN OF substitutionkinds,
        fixed          TYPE td_substitutionkind VALUE 'F',    " Assign allways the same specified devclass
        keep_container TYPE td_substitutionkind VALUE 'K',    " Keep dev class in XML file
        use_component  TYPE td_substitutionkind VALUE 'C',    " Use devclass define in component
        local          TYPE td_substitutionkind VALUE 'L',    " Devclass = $TMP
      END OF substitutionkinds .

  methods CONSTRUCTOR .
  methods GET_DEVCLASS
    returning
      value(RESULT) type TO_DEVCLASS
    raising
      ZAPLINK_CX_OPTIONS .
  methods GET_TRANSPORT_REQUEST
    returning
      value(RESULT) type TO_TR
    raising
      ZAPLINK_CX_OPTIONS .
  methods SET_DEVCLASS
    importing
      !DATA type TO_DEVCLASS
    raising
      ZAPLINK_CX_OPTIONS .
  methods SET_DIRECTORY
    importing
      !COMPONENT type TO_COMPONENT
    returning
      value(RESULT) type TS_DIRECTORY
    raising
      ZAPLINK_CX_OPTIONS .
  methods SET_TRANSPORT_REQUEST
    importing
      !DATA type TO_TR
    raising
      ZAPLINK_CX_OPTIONS .
protected section.

  data O_MYCX type TO_EXCEPTION .
  data _DEVCLASS type TO_DEVCLASS .
  data _SUBSTITUTION type TD_SUBSTITUTIONKIND .
  data _TR type TO_TR .
private section.

  class-data O_CX type TO_ROOT_EXCEPTION .
ENDCLASS.



CLASS ZAPLINK_OPT_DIRECTORY IMPLEMENTATION.


  method CONSTRUCTOR.
  CREATE OBJECT _devclass.
  CREATE OBJECT _tr.
  mac_create_log application_log ballog_subobject space.
  endmethod.


  method GET_DEVCLASS.
result = _devclass.
  endmethod.


  method GET_TRANSPORT_REQUEST.
  result = _tr.
  endmethod.


  method SET_DEVCLASS.
  CHECK data <> _devclass.

*  IF _devclass IS INITIAL AND _substitution = substitutionkinds-fixed.
*    RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
*          EXPORTING textid = zaplink_cx_opt_devclass=>missing_devclass
*                 substkind = _substitution.
*  ENDIF.
  _devclass = data.
  endmethod.


  method SET_DIRECTORY.
  DATA _name        TYPE trobj_name.    " tadir-obj_name.
  DATA d_tdname     TYPE tadir-obj_name.
  DATA _type        TYPE tadir-object.
  DATA _pgmid       TYPE tadir-pgmid.
  DATA _act_devc    TYPE tadir-devclass.
  DATA _exists      TYPE saus_dpara-tadirexist.
  DATA f_global     TYPE abap_bool.                         " Issue 87
  DATA s_e071       TYPE e071.
  DATA s_parent_key TYPE tadir.
  DATA f_hide_dia   TYPE abap_bool.

  CHECK component IS BOUND.
  result = component->directory.

  TRY.

    _type = component->get_type( ).
    d_tdname = _name = component->get_name( ).
    _pgmid = component->get_typekind( ).                    " Issue 87

    IF _pgmid = 'R3TR'.   f_global = abap_true.   ELSEIF _pgmid = 'LIMU'.   f_global = abap_false.    ELSE.   EXIT.   ENDIF.    " Form DETERMINE_LOCK_KEY(LSEUQF01) 125

    IF me->_devclass IS BOUND.
      TRY.
          result-devclass = me->_devclass->substitute( component ).
        CATCH zaplink_cx_options INTO o_mycx.
          RAISE EXCEPTION o_mycx.
      ENDTRY.
    ENDIF.

    IF me->_tr IS BOUND.
      TRY.
          result-trkorr = _tr->substitute( component ).
        CATCH zaplink_cx_options INTO o_mycx.
          RAISE EXCEPTION o_mycx.
      ENDTRY.
    ENDIF.

    IF _pgmid = 'LIMU'.
      s_e071-pgmid = _pgmid.
      s_e071-object = _type.
      s_e071-obj_name = _name.
      CALL FUNCTION 'TR_CHECK_TYPE'
        EXPORTING
          wi_e071                              = s_e071
        IMPORTING
*          WE_LOCK_KEY                          =
          we_tadir                             =  s_parent_key.
      IF s_parent_key-pgmid = 'R3TR'.     " Check parent's devclass
        CALL FUNCTION 'TRINT_TADIR_QUERY'
          EXPORTING
            iv_pgmid    = s_parent_key-pgmid
            iv_object   = s_parent_key-object
            iv_obj_name = s_parent_key-obj_name
          IMPORTING
            ev_devclass = _act_devc
            ev_exist    = _exists.
        IF NOT _exists IS INITIAL.
          result-devclass = _act_devc.    " Never change devclass of parent because of child devclass
        ENDIF.
      ENDIF.
    ELSE.
      s_parent_key-pgmid = _pgmid.
      s_parent_key-object = _type.
      s_parent_key-obj_name = d_tdname.
*    ENDIF.
*
*    IF s_parent_key-pgmid = 'R3TR'.     " Do TADIR entry : Issue 87
      CALL FUNCTION 'TRINT_TADIR_QUERY'
        EXPORTING
          iv_pgmid    = s_parent_key-pgmid
          iv_object   = s_parent_key-object
          iv_obj_name = s_parent_key-obj_name
        IMPORTING
          ev_devclass = _act_devc
          ev_exist    = _exists.
      IF _exists IS INITIAL.
        CALL FUNCTION 'TRINT_TADIR_INSERT'
          EXPORTING
*        AUTHOR                     = SY-UNAME
*        MASTERLANG                 = ' '
            devclass                   = result-devclass
*        GENFLAG                    = ' '
            object                     = _type
            obj_name                   = d_tdname
            pgmid                      = _pgmid
*        SRCSYSTEM                  = SY-SYSID
*        EDTFLAG                    = ' '
*        SRCDEP                     =
*        FORCE_MODE                 = ' '
*        PAKNOCHECK                 = ' '
*        OBJSTABLTY                 = ' '
*        TRANSL_TECH_TEXT           = ' '
*        DELFLAG                    = ' '
*      IMPORTING
*        ES_TADIR                   =
          EXCEPTIONS
            object_exists_global       = 1
            object_exists_local        = 2
            OTHERS                     = 3.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise_class 'ZAPLINK_CX_OPT_DEVCLASS' 'TRINT_TADIR_INSERT' sy-subrc.
        ENDIF.
      ELSEIF _act_devc <> result-devclass.
        CALL FUNCTION 'TRINT_TADIR_MODIFY'
          EXPORTING
*        AUTHOR                     = '~'
*        CPROJECT                   = '~'
            devclass                   = result-devclass
*        EDTFLAG                    = '~'
*        EXISTS                     = '~'
*        GENFLAG                    = '~'
*        MASTERLANG                 = '~'
            object                     = _type
            obj_name                   = d_tdname
            pgmid                      = _pgmid
*        SRCDEP                     = '~'
*        SRCSYSTEM                  = '~'
*        VERSID                     = '~'
*        CHANGE_MASTERLANG          = '~'
*        FORCE_MODE                 = '~'
*        PAKNOCHECK                 = '~'
*        OBJSTABLTY                 = '~'
*        DELFLAG                    = '~'
*        TRANSL_TECH_TEXT           = '~'
*      IMPORTING
*        ES_TADIR                   =
          EXCEPTIONS
            object_exists_global       = 1
            object_exists_local        = 2
            object_has_no_tadir        = 3
            OTHERS                     = 4.
        IF sy-subrc <> 0.
          mac_add_mf_and_raise_class 'ZAPLINK_CX_OPT_DEVCLASS' 'TRINT_TADIR_MODIFY' sy-subrc.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT result-trkorr IS INITIAL.    f_hide_dia = abap_true.   ENDIF.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object                         = _name
        object_class                   = _type
        mode                           = 'I'        " Insert
        global_lock                    = f_global   " R3TR or LIMU
        devclass                       = result-devclass
        korrnum                        = result-trkorr
*        USE_KORRNUM_IMMEDIATEDLY       = ' '
*        AUTHOR                         = ' '
*        MASTER_LANGUAGE                = ' '
*        GENFLAG                        = ' '
*        PROGRAM                        = ' '
*        OBJECT_CLASS_SUPPORTS_MA       = ' '
*        EXTEND                         = ' '
        suppress_dialog                = f_hide_dia
*        MOD_LANGU                      = ' '
*        ACTIVATION_CALL                = ' '
      IMPORTING
*        DEVCLASS                       =
        korrnum                        = result-trkorr
*        ORDERNUM                       =
*        NEW_CORR_ENTRY                 =
*        AUTHOR                         =
*        TRANSPORT_KEY                  =
*        NEW_EXTEND                     =
      EXCEPTIONS
        cancelled                      = 1
        permission_failure             = 2
        unknown_objectclass            = 3
        OTHERS                         = 4.
    IF sy-subrc <> 0.
      mac_add_mf_and_raise_class 'ZAPLINK_CX_OPT_DEVCLASS' 'RS_CORR_INSERT' sy-subrc.
    ENDIF.

    mac_def_catch zaplink_cx_options.
  ENDTRY.
  endmethod.


  method SET_TRANSPORT_REQUEST.
  CHECK data <> _tr.

*  IF _devclass IS INITIAL AND _substitution = substitutionkinds-fixed.
*    RAISE EXCEPTION TYPE zaplink_cx_opt_devclass
*          EXPORTING textid = zaplink_cx_opt_devclass=>missing_devclass
*                 substkind = _substitution.
*  ENDIF.
  _tr = data.
  endmethod.
ENDCLASS.
