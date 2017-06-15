*---------------------------------------------------------------------*
*  Include           ZAPLINK_FRM
*---------------------------------------------------------------------*
  DEFINE mac_replace.
    replace all occurrences of regex '<:&2:>' in &1 with sy-&2.
  END-OF-DEFINITION.
*/---------------------writeMessage-----------------------\
  FORM writemessage USING value(p_type) TYPE sy-msgty
                          value(p_msg).
    CASE p_type.
      WHEN 'E' OR 'A' OR 'X'.
        WRITE / icon_red_light AS ICON.
      WHEN 'W'.
        WRITE / icon_yellow_light AS ICON.
      WHEN 'S'.
        WRITE / icon_green_light AS ICON.
      WHEN OTHERS.
        WRITE / icon_light_out AS ICON. " ICON_ARROW_RIGHT
    ENDCASE.

    WRITE p_msg.
  ENDFORM.                    "WriteMessage
*&---------------------------------------------------------------------*
*&      Form  DSP_RESULT_UNINSTALL
*&---------------------------------------------------------------------*
  FORM dsp_result_uninstall USING name TYPE td_name.
    DATA msg    TYPE string.
    DATA t_comp TYPE t_comp.
    DATA _type  TYPE t_obj_type.
    DATA _name  TYPE t_obj_name.
    DATA _cx    TYPE t_exception.
    DATA _msg_col TYPE t_msg_col.
    DATA o_cx   TYPE REF TO zaplink_cx.

    WRITE: / 'Object list for Container'(001), name. SKIP.
    TRY.
        IF objlist IS BOUND.
          objlist->display_progress = abap_false.
          objlist->init_iter( ).
          t_comp = objlist->get_next( ).
        ENDIF.
        IF NOT t_comp IS BOUND.
          PERFORM writemessage USING 'W' 'You have an empty Container'(002).
          EXIT.
        ENDIF.

        WHILE t_comp IS BOUND.
          _type = t_comp->get_type( ).
          _name = t_comp->get_name( ).

          IF t_comp->connector_exists( ) = zaplink_gui=>exists-not_exists.
            CONCATENATE _type '-' _name 'no connector'(nco)
                INTO msg SEPARATED BY space.
            PERFORM writemessage USING 'E' msg.
          ELSEIF objlist->is_selected( t_comp ) = abap_false.
            CONCATENATE 'Uninstall'(u01) ':' _type '-' _name 'not requested'(nre)
                INTO msg SEPARATED BY space.
            PERFORM writemessage USING 'I' msg.
          ELSE.
            t_comp->refresh( ).                             " Issue 31
            IF t_comp->do_exists( ) = zaplink_gui=>exists-not_exists.
              CONCATENATE 'Uninstall'(u01) ':' _type '-' _name 'SUCCEED'(suc)
                  INTO msg SEPARATED BY space.
              PERFORM writemessage USING 'S' msg.
            ELSE.
              CONCATENATE 'Uninstall'(u01) ':' _type '-' _name 'FAILED'(fai)
                  INTO msg SEPARATED BY space.
              PERFORM writemessage USING 'E' msg.
              _cx = t_comp->get_exception( ).
              IF _cx IS BOUND.
                _cx->write( ).
              ELSE.
                _msg_col = t_comp->get_msg_coll( ).
                IF _msg_col IS BOUND.
                  _msg_col->write( ).
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          t_comp = objlist->get_next( ).
        ENDWHILE.
      CATCH zaplink_cx INTO o_cx.
        o_cx->write( ).
    ENDTRY.
  ENDFORM.                    " DSP_RESULT_UNINSTALL
*&---------------------------------------------------------------------*
*&      Form  DSP_RESULT_DISPLAY
*&---------------------------------------------------------------------*
  FORM dsp_result_display USING name TYPE td_name
                             objlist TYPE t_objlist
                              offset TYPE string.
    DATA msg    TYPE string.
    DATA t_comp TYPE t_comp.
    DATA _type  TYPE t_obj_type.
    DATA _name  TYPE t_obj_name.
    DATA _act   TYPE zaplink_component=>td_action.
    DATA o_cx   TYPE REF TO zaplink_cx.
    DATA _cx    TYPE t_exception.
    DATA _msg_col TYPE t_msg_col.
    DATA _subc  TYPE t_objlist.
    DATA _offset TYPE string.
    CONSTANTS s_inc TYPE string VALUE `  `.

    IF NOT name IS INITIAL.
      WRITE: / 'Object list for Container '(001), name. SKIP.
    ENDIF.
    TRY.
        IF objlist IS BOUND.
          objlist->display_progress = abap_false.
          objlist->init_iter( ).
          t_comp = objlist->get_next( ).
        ENDIF.
        IF NOT name IS INITIAL AND NOT t_comp IS BOUND.
          PERFORM writemessage USING 'W' 'You have an empty Container'(002).
          EXIT.
        ENDIF.

        CONCATENATE offset s_inc INTO _offset.

        WHILE t_comp IS BOUND.
          _type = t_comp->get_type( ).
          _name = t_comp->get_name( ).
          _act  = t_comp->get_action( ).

          IF t_comp->connector_exists( ) = zaplink_gui=>exists-not_exists.
            CONCATENATE offset _type '-' _name 'no connector'(nco)
                INTO msg SEPARATED BY space.
            PERFORM writemessage USING 'E' msg.
          ELSEIF objlist->is_selected( t_comp ) = abap_false.
            CONCATENATE offset _type '-' _name 'not requested'(nre)
                INTO msg SEPARATED BY space.
            PERFORM writemessage USING 'I' msg.
          ELSE.

            _cx = t_comp->get_exception( ).
            _msg_col = t_comp->get_msg_coll( ).
            IF _msg_col IS BOUND.
              IF _msg_col->has_messages_of_msgt( 'E' ) = abap_false .
                CLEAR _msg_col.
              ENDIF.
            ENDIF.
            IF _cx IS BOUND OR _msg_col IS BOUND.
              CONCATENATE offset _type '-' _name 'FAILED'(fai)
                  INTO msg SEPARATED BY space.
              PERFORM writemessage USING 'E' msg.
              IF _cx IS BOUND.
                _cx->write( ).
                _msg_col = t_comp->get_msg_coll( ).
                IF _msg_col IS BOUND.
                  _msg_col->write( ).
                ENDIF.
              ELSE.
                IF _msg_col IS BOUND.
                  _msg_col->write( ).
                ENDIF.
              ENDIF.
            ELSE.
              IF _act = zaplink_list=>sel_actions-activated.
                CONCATENATE offset _type '-' _name 'SUCCEED & Activated'(sac)
                    INTO msg SEPARATED BY space.
                PERFORM writemessage USING 'S' msg.
              ELSEIF _act = zaplink_list=>sel_actions-not_active.
                CONCATENATE offset _type '-' _name 'SUCCEED but NOT activated'(sin)
                    INTO msg SEPARATED BY space.
                PERFORM writemessage USING 'W' msg.
              ELSE.
                CONCATENATE offset _type '-' _name 'SUCCEED'(suc)
                    INTO msg SEPARATED BY space.
                PERFORM writemessage USING 'S' msg.
              ENDIF.
              _msg_col = t_comp->get_msg_coll( ).
              IF _msg_col IS BOUND.
                IF _msg_col->has_messages_of_msgt( 'W' ) = abap_true .
                  _msg_col->write( ).
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          _subc = t_comp->get_subcomponents( ).
          IF _subc IS BOUND.
            PERFORM dsp_result_display USING space _subc _offset.
          ENDIF.

          t_comp = objlist->get_next( ).
        ENDWHILE.
      CATCH zaplink_cx INTO o_cx.
        o_cx->write( ).
    ENDTRY.

  ENDFORM.                    " DSP_RESULT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  create
*&---------------------------------------------------------------------*
  FORM create .
    d_name = c_name.
    TRY.
        CALL METHOD zaplink_gui=>create
          EXPORTING
            name = d_name
            file = o_file.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.
    PERFORM writemessage USING 'S' 'Container created'(ncs).
  ENDFORM.                    " create
*&---------------------------------------------------------------------*
*&      Form  auto
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM auto.
    DATA _keys TYPE zaplink_gui=>tt_compkeys.

    TRY.
        _keys = zaplink_gui=>get_keys_from_tadir( srcsystem = s_srcs[]
                                                       type = s_type[]
                                                       name = s_name[]
                                                   packages = s_pack[]
                                                        ext = p_ext
                                                     author = s_author[] ).
* No component was selected with this parameters
        IF _keys  IS INITIAL.   MESSAGE s054.   EXIT.   ENDIF.

        d_name = c_name.
        mac_replace d_name: uname, sysid, datum, uzeit.
        IF d_name IS INITIAL. CONCATENATE sy-datum sy-uzeit INTO d_name. ENDIF.
        CALL METHOD zaplink_gui=>create
          EXPORTING
            name = d_name
            file = o_file.

        CREATE OBJECT objlist.
        objlist->add_keys( _keys ).
        objlist->select_default( zaplink_list=>sel_actions-export ).
        objlist->change_selection( ).

        CALL METHOD zaplink_gui=>export
          EXPORTING
            file         = o_file
            with_subcomp = f_sub
          CHANGING
            objlist      = objlist.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.

    PERFORM dsp_result_display USING space objlist space.

  ENDFORM.                    " auto
*&---------------------------------------------------------------------*
*&      Form  add
*&---------------------------------------------------------------------*
  FORM refresh .

    TRY.
        objlist = zaplink_gui=>refresh( file = o_file
                                with_subcomp = f_sub ).
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.

    PERFORM dsp_result_display USING space objlist space.

  ENDFORM.                    " add
*&---------------------------------------------------------------------*
*&      Form  add
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM add .
    DATA _keys TYPE zaplink_gui=>tt_compkeys.

    TRY.
        _keys = zaplink_gui=>get_keys_from_tadir( srcsystem = s_srcs[]
                                                       type = s_type[]
                                                       name = s_name[]
                                                   packages = s_pack[]
                                                        ext = p_ext
                                                     author = s_author[] ).
* No component was selected with this parameters
        IF _keys  IS INITIAL.   MESSAGE s054.   EXIT.   ENDIF.
        CREATE OBJECT objlist.
        objlist->add_keys( _keys ).
        zaplink_gui=>change_selection( action = zaplink_list=>sel_actions-export
                                         list = objlist ).

        CALL METHOD zaplink_gui=>export
          EXPORTING
            file         = o_file
            with_subcomp = f_sub
          CHANGING
            objlist      = objlist.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.

    PERFORM dsp_result_display USING space objlist space.

  ENDFORM.                    " add
*&---------------------------------------------------------------------*
*&      Form  import
*&---------------------------------------------------------------------*
  FORM import .
    DATA o_dir  TYPE REF TO zaplink_opt_directory.
    DATA o_devc TYPE REF TO zaplink_opt_devclass.
    DATA o_tr   TYPE REF TO zaplink_opt_transport_request.

    TRY.
        o_dir = zaplink_gui=>o_options->get_directory( ).
        o_devc = o_dir->get_devclass( ).
        IF NOT devc_no IS INITIAL.
          o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-no_overwrite ).
        ELSEIF NOT devc_uc IS INITIAL.
          o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-use_container ).
        ELSEIF NOT devc_ke IS INITIAL.
          o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-keep_existing ).
        ELSEIF NOT devc_lo IS INITIAL.
          o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-local ).
        ELSEIF NOT devc_f IS INITIAL.
          o_devc->set_devclass( devclass ).
          o_devc->set_substitutionkind( zaplink_opt_devclass=>substitutionkinds-fixed ).
        ENDIF.

        o_tr = o_dir->get_transport_request( ).
        IF NOT tr_us IS INITIAL.
          o_tr->set_substitutionkind( zaplink_opt_transport_request=>substitutionkinds-user ).
        ELSEIF NOT tr_uc IS INITIAL.
          o_tr->set_substitutionkind( zaplink_opt_transport_request=>substitutionkinds-same_as_container ).
        ELSEIF NOT tr_in IS INITIAL.
          o_tr->set_substitutionkind( zaplink_opt_transport_request=>substitutionkinds-input ).
        ELSEIF NOT tr_fi IS INITIAL.
          o_tr->set_transport_request( tr ).
          o_tr->set_substitutionkind( zaplink_opt_transport_request=>substitutionkinds-fixed ).
        ENDIF.

        CALL METHOD zaplink_gui=>import
          EXPORTING
            file            = o_file
            with_subcomp    = f_sub
            activate        = p_activ
            activate_anyway = P_force
          IMPORTING
            name         = d_name
*        CHANGING
            list       = objlist.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.

    PERFORM dsp_result_display USING d_name objlist space.
  ENDFORM.                    " import
*&---------------------------------------------------------------------*
*&      Form  display
*&---------------------------------------------------------------------*
  FORM display .
    TRY.
        objlist = zaplink_gui=>get_objlist( file = o_file ).
        objlist->display( ).
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
    ENDTRY.
*  PERFORM dsp_result_display USING space objlist space.
  ENDFORM.                    " display
*&---------------------------------------------------------------------*
*&      Form  uninstall
*&---------------------------------------------------------------------*
  FORM uninstall .
    TRY.
        CALL METHOD zaplink_gui=>uninstall
          EXPORTING
            file         = o_file
            with_subcomp = f_sub
          IMPORTING
            name         = d_name
          CHANGING
            list         = objlist.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
    ENDTRY.
    PERFORM dsp_result_uninstall USING d_name.
  ENDFORM.                    " uninstall
*&---------------------------------------------------------------------*
*&      Form  add_comp
*&---------------------------------------------------------------------*
  FORM add_comp.
    DATA o_comp    TYPE zaplink_gui=>to_component.

    CHECK NOT co_type IS INITIAL AND NOT co_name IS INITIAL.

    TRY.
        CREATE OBJECT: o_comp, objlist.
        o_comp->set_type( co_type ).    o_comp->set_name( co_name ).
        IF o_comp->connector_exists( ) <> zaplink_gui=>exists-exists.
* There is no ZAPLink connector installed for object type '&1'
          MESSAGE i050 WITH co_type.
          EXIT.
        ENDIF.
        IF o_comp->do_exists( ) <> zaplink_gui=>exists-exists.
* Component '&2' (object type '&1') does not exists
          MESSAGE i052 WITH co_type co_name.
          EXIT.
        ENDIF.
        objlist->add( o_comp ).
        objlist->select_all( ).

        CALL METHOD zaplink_gui=>export
          EXPORTING
            file         = o_file
            with_subcomp = f_sub
          CHANGING
            objlist      = objlist.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.

    PERFORM dsp_result_display USING space objlist space.

  ENDFORM.                    " add_package
*&---------------------------------------------------------------------*
*&      Form  add_tr
*&---------------------------------------------------------------------*
  FORM add_tr .
    DATA:
      r_tr TYPE zaplink_gui=>tr_tr.
    DATA _keys TYPE zaplink_gui=>tt_compkeys.

    IF p_tr[] IS INITIAL.
      MESSAGE i000(0k) WITH 'Transport Request required'(003).
      EXIT.
    ENDIF.

    r_tr = p_tr[].

    TRY.
        _keys = zaplink_gui=>get_keys_from_tr( r_tr ).
        CREATE OBJECT objlist.
        objlist->add_keys( _keys ).
        objlist->select_default( zaplink_list=>sel_actions-export ).
        objlist->change_selection( ).

        CALL METHOD zaplink_gui=>export
          EXPORTING
            file         = o_file
            with_subcomp = f_sub
          CHANGING
            objlist      = objlist.
      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        EXIT.
    ENDTRY.

    PERFORM dsp_result_display USING space objlist space.

  ENDFORM.                    " add_tr
*&---------------------------------------------------------------------*
*&      Form  CREATE_FILE
*&---------------------------------------------------------------------*
  FORM create_file .
    DATA d_file TYPE t_filename.

    TRY.
        IF d_file_type IS INITIAL OR p_zip IS INITIAL.
          CREATE OBJECT o_file.
        ELSE.
          CREATE OBJECT o_file TYPE (d_file_type).
        ENDIF.
        IF NOT f_local IS INITIAL.
          o_file->set_filetype( zaplink_file=>filetypes-local ).
        ELSEIF NOT f_server IS INITIAL.
          o_file->set_filetype( zaplink_file=>filetypes-server ).
        ENDIF.
        d_file = p_file.
        mac_replace d_file: uname, sysid, datum, uzeit.
        o_file->set_filename( d_file ).

      CATCH zaplink_cx INTO o_exception.
        o_exception->write( ).
        LEAVE PROGRAM.
    ENDTRY.

  ENDFORM.                    " CREATE_FILE
