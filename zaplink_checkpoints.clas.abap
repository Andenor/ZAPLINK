class ZAPLINK_CHECKPOINTS definition
  public
  inheriting from ZAPLINK_CONNECTOR_
  create public .

public section.
  type-pools ABAP .
  class ZAPLINK_ACID_DATA definition load .
  class ZAPLINK_AVAR_DATA definition load .

  constants BALLOG_SUBOBJECT type BALSUBOBJ value 'CNX_CHECKPOINTS' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .

  methods ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE
    redefinition .
  methods ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE
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

  types TO_CHECKPOINT type ref to ZAPLINK_ACID_DATA .
  types TO_VARIANT type ref to ZAPLINK_AVAR_DATA .
  types TD_CHECKPOINT type AAB_ID_NAME .
  types TD_VARIANT type STRING .
  types TS_FMD_ACID type ZAPLINK_ACID_DATA=>TS_FM_DATA .
  types TS_FMD_AVAR type ZAPLINK_AVAR_DATA=>TS_FM_DATA .

  class-data:
    BEGIN OF supportedkinds,
      checkpoint        TYPE td_transport_kind,
      variant           TYPE td_transport_kind,
    END OF supportedkinds .
  constants:
    BEGIN OF supportedtypes,
      checkpoint        TYPE td_comptype VALUE 'ACID',      "#EC NOTEXT
      variant           TYPE td_comptype VALUE 'AVAR',      "#EC NOTEXT
    END OF supportedtypes .
private section.

  constants _UUID type TD_CONNUUID value 'D8638E4C3E11CC5AE1000000AC120173' ##NO_TEXT.
  constants _VER type TD_CONNVER value '1.0' ##NO_TEXT.
ENDCLASS.



CLASS ZAPLINK_CHECKPOINTS IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  supportedkinds-checkpoint =  zaplink_connectors=>get_typekind( supportedtypes-checkpoint ).
  supportedkinds-variant =  zaplink_connectors=>get_typekind( supportedtypes-variant ).
  endmethod.


  method CONSTRUCTOR.
  DATA type LIKE LINE OF supported_types.

  CALL METHOD super->constructor.
  mac_create_log application_log ballog_subobject space.
  type-type = supportedtypes-checkpoint.    INSERT type INTO TABLE supported_types.
  type-type = supportedtypes-variant.       INSERT type INTO TABLE supported_types.

  uuid = _uuid.
  version = _ver.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~ANONYMIZE.
  DATA o_checkpoint    TYPE to_checkpoint.
*  DATA o_variant       TYPE to_variant.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN supportedtypes-checkpoint.
      o_checkpoint ?= object->raw.
      o_checkpoint->anonymize( ).
*    WHEN supportedtypes-variant.
*      o_variant ?= object->raw.
*      o_variant->anonymize( ).
    WHEN OTHERS.
       mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.
      super->zaplink_cnx_ext_cleaner~anonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CLEANER~UNANONYMIZE.
  DATA o_checkpoint    TYPE to_checkpoint.
*  DATA o_variant       TYPE to_variant.

  CHECK object IS BOUND.

  CASE object->type.
    WHEN supportedtypes-checkpoint.
      o_checkpoint ?= object->raw.
      o_checkpoint->unanonymize( ).
*    WHEN supportedtypes-variant.
*      o_variant ?= object->raw.
*      o_variant->unanonymize( ).
    WHEN OTHERS.
       mac_raise_type_not_supported me->class_name object->type.
  ENDCASE.

  TRY.
      super->zaplink_cnx_ext_cleaner~unanonymize( object ).
    CATCH zaplink_cx_connector INTO o_mycx.
      RAISE EXCEPTION o_mycx.
  ENDTRY.
  endmethod.


  method ZAPLINK_CNX_EXT_CODE_SIGNATURE~GET_SIGNATURE.
*  DATA d_group TYPE td_functiongroup_name.
*  DATA d_func  TYPE td_function_name.
*  DATA d_type  TYPE td_comptype.
*
*  TRY.
*    d_type = component->get_type( ).
*    CASE d_type.
*      WHEN supportedtypes-checkpoint.
*        d_group = component->get_name( ).
*        result = get_group_signature( d_group ).
*      WHEN supportedtypes-variant.
*        d_func = component->get_name( ).
*        result = get_function_signature( d_func ).
*      WHEN OTHERS.
*        mac_raise_type_not_supported me->class_name d_type.
*    ENDCASE.
*
*    mac_def_catch zaplink_cx_connector.
*  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~CREATE_NEW_RAW.
  DATA o_checkpoint   TYPE to_checkpoint.
  DATA o_variant      TYPE to_variant.

  CASE type.
    WHEN supportedtypes-checkpoint.
      CREATE OBJECT object.
      CREATE OBJECT o_checkpoint.
      object->raw = o_checkpoint.
    WHEN supportedtypes-variant.
      CREATE OBJECT object.
      CREATE OBJECT o_variant.
      object->raw = o_variant.
    WHEN OTHERS.
      CLEAR object.
      mac_raise_type_not_supported me->class_name type.
  ENDCASE.
  endmethod.


  method ZAPLINK_CONNECTOR~DELETE_FROM_SAP.
  DATA o_checkpoint   TYPE REF TO cl_aab_id.
  DATA d_checkpoint   TYPE td_checkpoint.
  DATA state          TYPE flag.
  DATA o_variant      TYPE REF TO cl_aab_variant.
*  DATA d_variant      TYPE td_checkpoint.
  DATA type           TYPE td_comptype.
  DATA name           TYPE td_compname.
  DATA s_variant      TYPE zaplink_avar_data=>ts_name.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN supportedtypes-checkpoint.
        d_checkpoint = name = component->get_name( ).

        CREATE OBJECT o_checkpoint
          EXPORTING
            im_name          = d_checkpoint
          EXCEPTIONS
            name_not_allowed = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zaplink_cx_connector
            EXPORTING textid = zaplink_cx_connector=>not_found
                        type = type
                        name = name.
        ENDIF.

        CALL METHOD o_checkpoint->enqueue
*          EXPORTING
*            enqueue_mode = 'E'
*            no_cts       = SPACE
           EXCEPTIONS
             cts_error    = 1
             foreign_lock = 2
             system_error = 3
             OTHERS       = 4.
        IF sy-subrc <> 0.
          mac_add_obj_meth_and_raise o_checkpoint 'ENQUEUE' sy-subrc.
        ENDIF.

        CALL METHOD o_checkpoint->delete
          EXCEPTIONS
            act_error        = 1
            cts_devclass     = 2
            cts_error        = 3
            id_not_found     = 4
            id_still_used    = 5
            no_authorization = 6
            propt_error      = 7
            prop_error       = 8
            where_used_error = 9
            OTHERS           = 10.
        IF sy-subrc <> 0.
          mac_add_obj_meth_and_raise o_checkpoint 'DELETE' sy-subrc.
        ENDIF.

*        o_checkpoint->dequeue => Made on commit

      WHEN supportedtypes-variant.
        name = component->get_name( ).
        s_variant = zaplink_avar_data=>name_2_key( name ).
        CREATE OBJECT o_variant
          EXPORTING
            im_name          = s_variant-name
            im_local         = s_variant-user
          EXCEPTIONS
            name_not_allowed = 1
            no_authorization = 2
            user_not_valid   = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          mac_add_name_meth_and_raise 'cl_aab_variant' 'CONSTRUCTOR' sy-subrc 'ZAPLINK_CX_CONNECTOR'.
        ENDIF.

        CALL METHOD o_variant->delete
          EXCEPTIONS
            cts_devclass     = 1
            cts_error        = 2
            no_authorization = 3
            prop_error       = 4
            propt_error      = 5
            var_id_error     = 6
            var_not_found    = 7
            others           = 8.
        IF sy-subrc <> 0.
          mac_add_obj_meth_and_raise o_variant 'DELETE' sy-subrc.
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
  DATA o_checkpoint   TYPE REF TO cl_aab_id.
  DATA d_checkpoint   TYPE td_checkpoint.
  DATA state          TYPE flag.
  DATA o_variant      TYPE REF TO cl_aab_variant.
*  DATA d_variant      TYPE td_checkpoint.
  DATA type           TYPE td_comptype.
  DATA name           TYPE td_compname.
  DATA s_variant      TYPE zaplink_avar_data=>ts_name.

  exists = me->exists-not_exists.

  TRY.
    type = component->get_type( ).
    CASE type.
      WHEN supportedtypes-checkpoint.
        d_checkpoint = component->get_name( ).
        CREATE OBJECT o_checkpoint
          EXPORTING
            im_name          = d_checkpoint
          EXCEPTIONS
            name_not_allowed = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          mac_add_name_meth_and_raise 'cl_aab_variant' 'CONSTRUCTOR' sy-subrc 'ZAPLINK_CX_CONNECTOR'.
        ELSE.
          o_checkpoint->get_state( IMPORTING ex_state = state ).
          IF NOT state IS INITIAL.     exists = me->exists-exists.     ENDIF.
        ENDIF.

      WHEN supportedtypes-variant.
        name = component->get_name( ).
        s_variant = zaplink_avar_data=>name_2_key( name ).
        CREATE OBJECT o_variant
          EXPORTING
            im_name          = s_variant-name
            im_local         = s_variant-user
          EXCEPTIONS
            name_not_allowed = 1
            no_authorization = 2
            user_not_valid   = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          mac_add_name_meth_and_raise 'cl_aab_variant' 'CONSTRUCTOR' sy-subrc 'ZAPLINK_CX_CONNECTOR'.
        ELSE.
          o_variant->get_state( IMPORTING ex_state = state ).
          IF NOT state IS INITIAL.     exists = me->exists-exists.     ENDIF.
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
  DATA o_aab_id         TYPE REF TO cl_aab_id.
  DATA o_checkpoint     TYPE to_checkpoint.
  DATA s_fmd_acid       TYPE ts_fmd_acid.
*  DATA d_checkpoint     TYPE td_checkpoint.
  DATA type             TYPE td_comptype.
  DATA name             TYPE td_compname.
  DATA l_mode           TYPE aab_id_mode.
  DATA l_tstamp         TYPE aab_id_act-act_tstamp.
  DATA o_aab_variant    TYPE REF TO cl_aab_variant.
  DATA o_variant        TYPE to_variant.
  DATA s_variant        TYPE zaplink_avar_data=>ts_name.
  DATA s_fmd_avar       TYPE ts_fmd_avar.

  TRY.
    type = component->get_type( ).
    CREATE OBJECT object.
    object->set_component( component ).
    CASE type.
      WHEN supportedtypes-checkpoint.
        s_fmd_acid-header-name = name = component->get_name( ).
        CREATE OBJECT o_aab_id
          EXPORTING
            im_name          = s_fmd_acid-header-name
          EXCEPTIONS
            name_not_allowed = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zaplink_cx_connector
            EXPORTING textid = zaplink_cx_connector=>not_found
                        type = type
                        name = name.
        ENDIF.

*        CALL METHOD o_aab_id->get_descript
*          EXPORTING
*            im_enforce_db_read   = abap_true
**            im_set_attribute     = 'X'
*          IMPORTING
*            ex_descript          = s_fmd_acid-header-descript
*          EXCEPTIONS
*            no_description_found = 1
*            OTHERS               = 2.
*        IF sy-subrc <> 0.
*        ENDIF.
* Multilang =>
        SELECT * INTO TABLE s_fmd_acid-descriptions
          FROM aab_id_propt
          WHERE name = s_fmd_acid-header-name.

* From LSAABP01 : Methode pbo_0310.
        CALL METHOD o_aab_id->get_all_modes
          IMPORTING
            ex_modes_tab  = s_fmd_acid-modes
          EXCEPTIONS
            no_mode_found = 1
            system_error  = 2
            OTHERS        = 3.
        IF sy-subrc <> 0.
          mac_add_obj_meth_and_raise o_aab_id 'GET_ALL_MODES' sy-subrc.
        ENDIF.

        CREATE OBJECT o_checkpoint.
        o_checkpoint->from_data( s_fmd_acid ).
        CLEAR s_fmd_acid-header-name.
        object->raw = o_checkpoint.

      WHEN supportedtypes-variant.
        name = component->get_name( ).
        s_variant = zaplink_avar_data=>name_2_key( name ).
        s_fmd_avar-header-name = s_variant-name.
        s_fmd_avar-header-local = s_variant-user.
        CREATE OBJECT o_aab_variant
          EXPORTING
            im_name          = s_fmd_avar-header-name
            im_local         = s_fmd_avar-header-local
          EXCEPTIONS
            name_not_allowed = 1
            no_authorization = 2
            user_not_valid   = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          mac_add_name_meth_and_raise 'cl_aab_variant' 'CONSTRUCTOR' sy-subrc 'ZAPLINK_CX_CONNECTOR'.
        ENDIF.

* Multilang =>
        SELECT * INTO TABLE s_fmd_avar-descriptions
          FROM aab_var_propt
          WHERE name = s_fmd_avar-header-name
           AND local = s_fmd_avar-header-local.

        CALL METHOD o_aab_variant->get_ids
          IMPORTING
            ex_ids       = s_fmd_avar-modes
          EXCEPTIONS
            id_not_valid = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
          mac_add_obj_meth_and_raise o_aab_variant 'GET_IDS' sy-subrc.
        ENDIF.

        CREATE OBJECT o_variant.
        o_variant->from_data( s_fmd_avar ).
        CLEAR: s_fmd_avar-header-name, s_fmd_avar-header-local.
        object->raw = o_variant.
      WHEN OTHERS.
        mac_raise_type_not_supported me->class_name type.
    ENDCASE.

    object->update_connector_data( me ).                    " Issue 66
    mac_def_catch zaplink_cx_connector.
  ENDTRY.
  endmethod.


  method ZAPLINK_CONNECTOR~WRITE_TO_SAP.
  DATA o_aab_id         TYPE REF TO cl_aab_id.
  DATA o_checkpoint     TYPE to_checkpoint.
  DATA s_fmd_acid       TYPE ts_fmd_acid.
  DATA o_variant        TYPE to_variant.
  DATA s_fmd_avar       TYPE ts_fmd_avar.
  DATA _comp            TYPE to_component.
  DATA s_desc           LIKE LINE OF s_fmd_acid-descriptions.
  DATA d_nb_err         TYPE i.
  DATA lt_modes         TYPE aab_id_act_tab.
  DATA o_aab_variant    TYPE REF TO cl_aab_variant.
  DATA s_variant        TYPE zaplink_avar_data=>ts_name.
  DATA s_var_desc       LIKE LINE OF s_fmd_avar-descriptions.
  DATA lt_var_modes     TYPE aab_var_obj_act_tab.
  FIELD-SYMBOLS:
    <o>  LIKE LINE OF lt_modes,
    <v>  LIKE LINE OF lt_var_modes,
    <w>  LIKE LINE OF s_fmd_avar-modes,
    <m>  LIKE LINE OF s_fmd_acid-modes.

  TRY.
      CREATE OBJECT components.
      CASE object->type.
        WHEN supportedtypes-variant.
          o_variant ?= object->raw.   s_variant = object->name.
          o_variant->a0_maindata-name = s_variant-name.     o_variant->a0_maindata-local = s_variant-user.
          s_fmd_avar = o_variant->to_data( ).
          CREATE OBJECT o_aab_variant
            EXPORTING
              im_name          = s_variant-name
              im_local         = s_variant-user
            EXCEPTIONS
              name_not_allowed = 1
              no_authorization = 2
              user_not_valid   = 3
              OTHERS           = 4.
          IF sy-subrc <> 0.
            mac_add_name_meth_and_raise 'cl_aab_variant' 'CONSTRUCTOR' sy-subrc 'ZAPLINK_CX_CONNECTOR'.
          ENDIF.

          CALL METHOD o_aab_variant->enqueue
            EXPORTING
              enqueue_mode = 'E'
            EXCEPTIONS
              cts_error    = 1
              foreign_lock = 2
              system_error = 3
              OTHERS       = 4.
          IF sy-subrc <> 0.
            mac_add_obj_meth_and_raise o_aab_variant 'ENQUEUE' sy-subrc.
          ENDIF.

          CALL METHOD o_aab_variant->get_ids
            IMPORTING
              ex_ids       = lt_var_modes
            EXCEPTIONS
              id_not_valid = 1
              OTHERS       = 2.
          IF sy-subrc = 0.
            LOOP AT lt_var_modes ASSIGNING <v>.
              READ TABLE s_fmd_avar-modes TRANSPORTING NO FIELDS WITH KEY
                  name = <v>-name
                object = <v>-object.
              CHECK sy-subrc <> 0.
              CALL METHOD o_aab_variant->del_id
                EXPORTING
                  im_name          = <v>-name
                  im_object        = <v>-object
                EXCEPTIONS
                  id_not_found     = 0
                  no_authorization = 2
                  OTHERS           = 3.
              IF sy-subrc <> 0.
                mac_add_obj_meth_and_raise o_aab_variant 'DEL_ID' sy-subrc.
              ENDIF.
            ENDLOOP.
          ENDIF.

          LOOP AT s_fmd_avar-modes ASSIGNING <w>.
            CALL METHOD o_aab_variant->set_id
              EXPORTING
                im_name              = <w>-name
                im_object            = <w>-object
                im_actmode           = <w>-actmode
              EXCEPTIONS
                id_not_exists        = 1
                id_not_transportable = 2
                no_authorization     = 3
                OTHERS               = 4.
            IF sy-subrc <> 0.
              mac_add_obj_meth_and_raise o_aab_variant 'SET_ID' sy-subrc.
            ENDIF.
          ENDLOOP.

          READ TABLE s_fmd_avar-descriptions INTO s_var_desc WITH KEY langu = sy-langu.
          IF sy-subrc <> 0.     READ TABLE s_fmd_avar-descriptions INTO s_var_desc INDEX 1.     ENDIF.
          IF sy-subrc <> 0.     s_var_desc-descript = s_variant-name.     ENDIF.

          CALL METHOD o_aab_variant->set_descript
            EXPORTING
              im_descript      = s_var_desc-descript
            EXCEPTIONS
              no_authorization = 1
              OTHERS           = 2.
          IF sy-subrc <> 0.
            mac_add_obj_meth_and_raise o_aab_variant 'SET_DESCRIPT' sy-subrc.
          ENDIF.

          CALL METHOD o_aab_variant->save
            EXCEPTIONS
              cts_error             = 1
              no_changes_found      = 2
              no_descript_specified = 3
              prop_error            = 4
              propt_error           = 5
              var_id_error          = 6
              OTHERS                = 7.
          IF sy-subrc <> 0.
            mac_add_obj_meth_and_raise o_aab_variant 'SAVE' sy-subrc.
          ENDIF.

*          CALL METHOD o_aab_id->set_descript
*            EXPORTING
*              im_descript      =
**            EXCEPTIONS
**              no_authorization = 1
**              others           = 2
*                  .
* Multilang =>
          DELETE FROM aab_var_propt WHERE name = s_fmd_avar-header-name
                                     AND local = s_fmd_avar-header-local.
          s_var_desc-name = s_variant-name.     s_var_desc-local = s_variant-user.
          MODIFY s_fmd_avar-descriptions FROM s_var_desc TRANSPORTING name local WHERE name IS INITIAL OR name IS NOT INITIAL.
          INSERT aab_var_propt FROM TABLE s_fmd_avar-descriptions.

          CALL METHOD o_aab_variant->dequeue
            EXPORTING
              dequeue_mode = 'E'.

        WHEN supportedtypes-checkpoint.
          o_checkpoint ?= object->raw.
          o_checkpoint->a0_maindata-name = object->name.
          s_fmd_acid = o_checkpoint->to_data( ).
          s_fmd_acid-header-name = object->name.
          CREATE OBJECT o_aab_id
            EXPORTING
              im_name          = s_fmd_acid-header-name
            EXCEPTIONS
              name_not_allowed = 1
              OTHERS           = 2.
          IF sy-subrc <> 0.
            mac_add_name_meth_and_raise 'cl_aab_id' 'CONSTRUCTOR' sy-subrc 'ZAPLINK_CX_CONNECTOR'.
          ENDIF.

          CALL METHOD o_aab_id->enqueue
            EXPORTING
              enqueue_mode = 'E'
              no_cts       = space
            EXCEPTIONS
              cts_error    = 1
              foreign_lock = 2
              system_error = 3
              OTHERS       = 4.
          IF sy-subrc <> 0.
            mac_add_obj_meth_and_raise o_aab_id 'ENQUEUE' sy-subrc.
          ENDIF.

* Delete previously existing modes
          CALL METHOD o_aab_id->get_all_modes
            IMPORTING
              ex_modes_tab  = lt_modes
            EXCEPTIONS
              no_mode_found = 1
              OTHERS        = 2.
          IF sy-subrc = 0.
            LOOP AT lt_modes ASSIGNING <o>.
              READ TABLE s_fmd_acid-modes TRANSPORTING NO FIELDS WITH KEY
*              name = <o>-name
                username = <o>-username
                server = <o>-server
                is_program = <o>-is_program
                actdefault = <o>-actdefault.
              CHECK sy-subrc <> 0.
              CALL METHOD o_aab_id->delete_mode
                EXPORTING
                  im_user           = <o>-username
                  im_server         = <o>-server
                  im_actdefault     = <o>-actdefault
                  im_delete_from_db = space
                EXCEPTIONS
                  no_mode_found     = 0
                  no_authorization  = 2
                  act_error         = 1
                  OTHERS            = 4.
              IF sy-subrc <> 0.
                mac_add_obj_meth_and_raise o_aab_id 'DELETE_MODE' sy-subrc.
              ENDIF.
            ENDLOOP.
          ENDIF.

          LOOP AT s_fmd_acid-modes ASSIGNING <m>.
            CALL METHOD o_aab_id->set_mode
              EXPORTING
                im_user            = <m>-username
                im_server          = <m>-server
                im_actdefault      = <m>-actdefault
                im_mode            = <m>-actmode
*                im_act_tstamp      =
*                im_exp_tstamp      =
*                im_no_autho_check  = SPACE
               IMPORTING
                 ex_nr_of_conflicts = d_nb_err
               EXCEPTIONS
                 no_authorization   = 1
                 OTHERS             = 2.
            IF sy-subrc <> 0.
              mac_add_obj_meth_and_raise o_aab_id 'SET_MODE' sy-subrc.
            ELSEIF d_nb_err <> 0.

            ENDIF.
          ENDLOOP.

          CALL METHOD o_aab_id->save
*            EXPORTING
*              no_change_rtm_activation = SPACE
            EXCEPTIONS
              act_error                = 1
              cts_error                = 2
              no_changes_found         = 3
              no_descript_specified    = 4
              prop_error               = 5
              propt_error              = 6
              sync_attributes_error    = 7
              OTHERS                   = 8.
          IF sy-subrc <> 0.
            mac_add_obj_meth_and_raise o_aab_id 'SAVE' sy-subrc.
          ENDIF.

*          CALL METHOD o_aab_id->set_descript
*            EXPORTING
*              im_descript      =
**            EXCEPTIONS
**              no_authorization = 1
**              others           = 2
*                  .
* Multilang =>
          DELETE FROM aab_id_propt WHERE name = s_fmd_acid-header-name.
          s_desc-name = s_fmd_acid-header-name.   MODIFY s_fmd_acid-descriptions FROM s_desc TRANSPORTING name WHERE name <> s_fmd_acid-header-name.
          INSERT aab_id_propt FROM TABLE s_fmd_acid-descriptions.

          CALL METHOD o_aab_id->dequeue
            EXPORTING
              dequeue_mode = 'E'.

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
