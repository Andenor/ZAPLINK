class ZAPLINK_FILE definition
  public
  create public .

public section.
  type-pools ABAP .

  types TD_FULLNAME type STRING .
  types TD_DIRECTORYNAME type STRING .
  types TD_FILEDIR_KIND type CHAR01 .
  types:
    BEGIN OF ts_file_info,
            name TYPE td_fullname,
            kind TYPE td_filedir_kind,
          END OF ts_file_info .
  types TD_FILENAME type ZAPLINK_CX_FILE=>T_FILENAME .
  types TD_FILETYPE type ZAPLINK_CX_FILE=>T_FILETYPE .
  types:
    tt_file_list TYPE STANDARD TABLE OF ts_file_info WITH DEFAULT KEY .

  constants C_FT_LOCAL type TD_FILETYPE value 'L' ##NO_TEXT.
  constants C_FT_SERVER type TD_FILETYPE value 'S' ##NO_TEXT.
  constants:
    BEGIN OF filetypes,
              local     TYPE td_filetype VALUE 'L',         "#EC NOTEXT
              server    TYPE td_filetype VALUE 'S',         "#EC NOTEXT
            END OF filetypes .
  constants:
    BEGIN OF filedirkinds,
              unknown   TYPE td_filedir_kind VALUE space,   "#EC NOTEXT
              file      TYPE td_filedir_kind VALUE 'F',     "#EC NOTEXT
              directory TYPE td_filedir_kind VALUE 'D',     "#EC NOTEXT
            END OF filedirkinds .
  class-data FILE_SEP type C read-only .

  class-methods CLASS_CONSTRUCTOR .
  methods GET_FILENAME
    returning
      value(RESULT) type TD_FILENAME .
  methods GET_FILETYPE
    returning
      value(RESULT) type TD_FILETYPE .
  methods SET_FILENAME
    importing
      !DATA type TD_FILENAME
    raising
      ZAPLINK_CX_FILE .
  methods SET_FILETYPE
    importing
      !DATA type TD_FILETYPE
    raising
      ZAPLINK_CX_FILE .
  methods GET_FILECONTENT
    returning
      value(RESULT) type STRING .
  methods SET_FILECONTENT
    importing
      !DATA type STRING .
  methods SAVE
    raising
      ZAPLINK_CX_FILE .
  methods LOAD
    raising
      ZAPLINK_CX_FILE .
  methods SAVE_DIALOG
    importing
      !FILENAME type TD_FILENAME optional
    raising
      ZAPLINK_CX_FILE .
  methods LOAD_DIALOG
    importing
      !FILENAME type TD_FILENAME optional
    raising
      ZAPLINK_CX_FILE .
  methods FILE_EXISTS
    returning
      value(RESULT) type ABAP_BOOL
    raising
      ZAPLINK_CX_FILE .
  methods SEARCH
    importing
      !MASK type TD_FILENAME
      !ROOT_DIR type TD_DIRECTORYNAME
    returning
      value(RESULT) type TT_FILE_LIST
    raising
      ZAPLINK_CX_FILE .
  methods GET_TEMP_DIRECTORY
    returning
      value(RESULT) type TD_DIRECTORYNAME
    raising
      ZAPLINK_CX_FILE .
  methods DELETE
    raising
      ZAPLINK_CX_FILE .
protected section.

  types TD_CONTENT type STRING .
  types TO_ROOT_EXCEPTION type ref to ZAPLINK_CX .
  types TO_EXCEPTION type ref to ZAPLINK_CX_FILE .

  data _FILENAME type TD_FILENAME .
  data _FILETYPE type TD_FILETYPE .
  data _CONTENT type TD_CONTENT .
  class-data O_MYCX type TO_EXCEPTION .
  class-data O_CX type TO_ROOT_EXCEPTION .
private section.
ENDCLASS.



CLASS ZAPLINK_FILE IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
  CALL METHOD cl_gui_frontend_services=>get_file_separator
    CHANGING
      file_separator       = file_sep
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    file_sep = '\'.
  ENDIF.
  endmethod.


  method DELETE.
  DATA d_rc TYPE i.
  DATA cx   TYPE REF TO cx_root.

  IF _filetype = c_ft_local.
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = _filename
      CHANGING
        rc                   = d_rc
      EXCEPTIONS
        access_denied        = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_delete_failed   = 4
        file_not_found       = 5
        not_supported_by_gui = 6
        unknown_error        = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF d_rc <> 0 AND sy-subrc = 0.     sy-subrc = 9.   ENDIF.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                       method = 'FILE_DELETE'
                                                        subrc = sy-subrc
                                                 cx_classname = 'ZAPLINK_CX_FILE' ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ELSEIF _filetype = c_ft_server.
    TRY.
        DELETE DATASET _filename.
      CATCH cx_root INTO cx.
* Exception &cx_name& while loading file &filename& (&filetype&)
        CREATE OBJECT o_mycx
          EXPORTING
            textid   = zaplink_cx_file=>delete_error
            filename = _filename
            filetype = _filetype
            previous = cx.
        o_mycx->update( ).
        RAISE EXCEPTION o_mycx.
    ENDTRY.
    IF sy-subrc <> 0.
* Exception &cx_name& while loading file &filename& (&filetype&)
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>delete_error
                    filename = _filename
                    filetype = _filetype
                     cx_name = 'DELETE DATASET'.
    ENDIF.
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.
  endmethod.


  method FILE_EXISTS.
  DATA tmptable TYPE table_of_strings.
  DATA cx_name TYPE string.
  DATA cx TYPE REF TO cx_root.
*  DATA _cx TYPE REF TO zaplink_cx_file.
  DATA _str LIKE LINE OF tmptable.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF tmptable.

  CASE _filetype.
    WHEN c_ft_local.
      CALL METHOD cl_gui_frontend_services=>file_exist
        EXPORTING
          file                 = _filename
        RECEIVING
          result               = result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          wrong_parameter      = 4
          OTHERS               = 5.
      IF sy-subrc <> 0.
* Exception &cx_name& while loading file &filename& (&filetype&)
        o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                         method = 'FILE_EXIST'
                                                          subrc = sy-subrc
                                                         textid = zaplink_cx_file=>load_error
                                                   cx_classname = 'ZAPLINK_CX_FILE' ).
        RAISE EXCEPTION o_mycx.
      ENDIF.
    WHEN c_ft_server.
      TRY.
          OPEN DATASET _filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        CATCH cx_root INTO cx.
* Exception &cx_name& while loading file &filename& (&filetype&)
          CREATE OBJECT o_mycx
            EXPORTING
              textid   = zaplink_cx_file=>load_error
              filename = _filename
              filetype = _filetype
              previous = cx.
          o_mycx->update( ).
          RAISE EXCEPTION o_mycx.
      ENDTRY.
      IF sy-subrc <> 0.
        result = abap_false.
        EXIT.
      ENDIF.

      result = abap_true.
      TRY.
          CLOSE DATASET _filename.
        CATCH cx_root INTO cx.
          EXIT.
      ENDTRY.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>invalid_filetype
                    filetype = _filetype.
  ENDCASE.
  endmethod.


  method GET_FILECONTENT.
  result = _content.
  endmethod.


  method GET_FILENAME.
  result = _filename.
  endmethod.


  method GET_FILETYPE.
  result = _filetype.
  endmethod.


  method GET_TEMP_DIRECTORY.
  DATA d_is_ok   TYPE abap_bool.

  IF _filetype = c_ft_local.
    CALL METHOD cl_gui_frontend_services=>get_temp_directory
      CHANGING
        temp_dir             = result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc = 0.
      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory            = result
        RECEIVING
          result               = d_is_ok
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.
      IF sy-subrc <> 0 OR d_is_ok IS INITIAL.
        CLEAR result.
      ENDIF.
    ENDIF.
    IF result IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
        CHANGING
          sapworkdir           = result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc = 0.
        CALL METHOD cl_gui_frontend_services=>directory_exist
          EXPORTING
            directory            = result
          RECEIVING
            result               = d_is_ok
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5.
        IF sy-subrc <> 0 OR d_is_ok IS INITIAL.
          CLEAR result.
        ENDIF.
      ENDIF.
    ENDIF.
    IF result IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>get_desktop_directory
        CHANGING
          desktop_directory    = result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc = 0.
        CALL METHOD cl_gui_frontend_services=>directory_exist
          EXPORTING
            directory            = result
          RECEIVING
            result               = d_is_ok
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5.
        IF sy-subrc <> 0 OR d_is_ok IS INITIAL.
          CLEAR result.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSEIF _filetype = c_ft_server.
* No dialog right now
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.
  endmethod.


  method LOAD.
  DATA tmptable TYPE table_of_strings.
  DATA cx_name TYPE string.
  DATA cx TYPE REF TO cx_root.
*  DATA _cx TYPE REF TO zaplink_cx_file.
  DATA _str LIKE LINE OF tmptable.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF tmptable.

  IF _filetype = c_ft_local.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = _filename
      CHANGING
        data_tab                = tmptable
      EXCEPTIONS
        access_denied           = 1
        bad_data_format         = 2
        disk_full               = 3
        dp_out_of_memory        = 4
        dp_timeout              = 5
        error_no_gui            = 6
        file_open_error         = 7
        file_read_error         = 8
        gui_refuse_filetransfer = 9
        header_not_allowed      = 10
        header_too_long         = 11
        invalid_type            = 12
        no_authority            = 13
        no_batch                = 14
        not_supported_by_gui    = 15
        separator_not_allowed   = 16
        unknown_dp_error        = 17
        unknown_error           = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                       method = 'GUI_UPLOAD'
                                                        subrc = sy-subrc
                                                 cx_classname = 'ZAPLINK_CX_FILE' ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
  ELSEIF _filetype = c_ft_server.
    TRY.
        OPEN DATASET _filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      CATCH cx_root INTO cx.
* Exception &cx_name& while loading file &filename& (&filetype&)
        CREATE OBJECT o_mycx
          EXPORTING
            textid   = zaplink_cx_file=>load_error
            filename = _filename
            filetype = _filetype
            previous = cx.
        o_mycx->update( ).
        RAISE EXCEPTION o_mycx.
    ENDTRY.
    IF sy-subrc <> 0.
* Exception &cx_name& while loading file &filename& (&filetype&)
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>load_error
                    filename = _filename
                    filetype = _filetype
                     cx_name = 'OPEN DATASET'.
    ENDIF.

    DO.
      TRY.
          READ DATASET _filename INTO _str.
        CATCH cx_root INTO cx.
* Exception &cx_name& while loading file &filename& (&filetype&)
          CREATE OBJECT o_mycx
            EXPORTING
              textid   = zaplink_cx_file=>load_error
              filename = _filename
              filetype = _filetype
              previous = cx.
          o_mycx->update( ).
          RAISE EXCEPTION o_mycx.
      ENDTRY.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      APPEND _str TO tmptable.
    ENDDO.

    TRY.
        CLOSE DATASET _filename.
      CATCH cx_root INTO cx.
* Exception &cx_name& while loading file &filename& (&filetype&)
        CREATE OBJECT o_mycx
          EXPORTING
            textid   = zaplink_cx_file=>load_error
            filename = _filename
            filetype = _filetype
            previous = cx.
        o_mycx->update( ).
        RAISE EXCEPTION o_mycx.
    ENDTRY.
    IF sy-subrc <> 0.
* Exception &cx_name& while loading file &filename& (&filetype&)
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>load_error
                    filename = _filename
                    filetype = _filetype
                     cx_name = 'CLOSE DATASET'.
    ENDIF.
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.

  CLEAR _content.
  LOOP AT tmptable ASSIGNING <s>.
    CONCATENATE _content <s> cl_abap_char_utilities=>newline
                INTO _content.
  ENDLOOP.
  endmethod.


  method LOAD_DIALOG.
  DATA ft TYPE filetable.
  DATA filecount TYPE i.
  DATA cx_name TYPE string.

  IF _filetype = c_ft_local.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
*        window_title            =
*        default_extension       =
        default_filename        = filename
*        file_filter             =
*        with_encoding           =
*        initial_directory       =
*        multiselection          =
      CHANGING
        file_table              = ft
        rc                      = filecount
*        user_action             =
*        file_encoding           =
      EXCEPTIONS
        cntl_error              = 1
        error_no_gui            = 2
        file_open_dialog_failed = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.   " OR filecount < 0.
* Exception &cx_name& while loading file &filename& (&filetype&)
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                       method = 'FILE_OPEN_DIALOG'
                                                        subrc = sy-subrc
                                                       textid = zaplink_cx_file=>load_error
                                                 cx_classname = 'ZAPLINK_CX_FILE' ).
      o_mycx->filename = filename.
      o_mycx->filetype = _filetype.
      RAISE EXCEPTION o_mycx.
    ENDIF.
    IF filecount = 0 OR ft IS INITIAL.
* Dialog canceled by user
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>dialog_canceled
                    filename = filename
                    filetype = _filetype.
    ENDIF.
    READ TABLE ft INTO _filename INDEX 1.
  ELSEIF _filetype = c_ft_server.
* No dialog right now
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.
  endmethod.


  method SAVE.
  DATA tmptable  TYPE table_of_strings.
*  DATA cx_name   TYPE string.
  DATA o_cx      TYPE REF TO cx_root.
  DATA o_cx_file TYPE REF TO zaplink_cx_file.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF tmptable.

  SPLIT _content AT cl_abap_char_utilities=>newline
            INTO TABLE tmptable.

  IF _filetype = c_ft_local.
    IF _filename IS INITIAL.
      save_dialog( ).
    ELSE.
*     CALL METHOD cl_gui_frontend_services=>gui_download
*     Not used because of missing param : SHOW_TRANSFER_STATUS
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
*         BIN_FILESIZE                    =
          filename                        = _filename
          filetype                        = 'DAT'
*         APPEND                          = ' '
*         WRITE_FIELD_SEPARATOR           = ' '
*         HEADER                          = '00'
*         TRUNC_TRAILING_BLANKS           = ' '
*         WRITE_LF                        = 'X'
*         COL_SELECT                      = ' '
*         COL_SELECT_MASK                 = ' '
*         DAT_MODE                        = ' '
*         CONFIRM_OVERWRITE               = ' '
*         NO_AUTH_CHECK                   = ' '
*         CODEPAGE                        = ' '
*         IGNORE_CERR                     = ABAP_TRUE
*         REPLACEMENT                     = '#'
*         WRITE_BOM                       = ' '
*         TRUNC_TRAILING_BLANKS_EOL       = 'X'
*         WK1_N_FORMAT                    = ' '
*         WK1_N_SIZE                      = ' '
*         WK1_T_FORMAT                    = ' '
*         WK1_T_SIZE                      = ' '
*         WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
          show_transfer_status            = abap_false
*       IMPORTING
*         FILELENGTH                      =
        TABLES
          data_tab                        = tmptable
*         FIELDNAMES                      =
        EXCEPTIONS
          file_write_error                = 1
          no_batch                        = 2
          gui_refuse_filetransfer         = 3
          invalid_type                    = 4
          no_authority                    = 5
          unknown_error                   = 6
          header_not_allowed              = 7
          separator_not_allowed           = 8
          filesize_not_allowed            = 9
          header_too_long                 = 10
          dp_error_create                 = 11
          dp_error_send                   = 12
          dp_error_write                  = 13
          unknown_dp_error                = 14
          access_denied                   = 15
          dp_out_of_memory                = 16
          disk_full                       = 17
          dp_timeout                      = 18
          file_not_found                  = 19
          dataprovider_exception          = 20
          control_flush_error             = 21
          OTHERS                          = 22.
      IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
*        TRY.
            o_cx_file ?= zaplink_cx=>create_from_mf_cx( funcname = 'GUI_DOWNLOAD'
                                                           subrc = sy-subrc
                                                       classname = 'ZAPLINK_CX_FILE'
                                                          textid = zaplink_cx_file=>save_error ).
            o_cx_file->filename = _filename.
            o_cx_file->filetype = _filetype.
*          CATCH cx_root INTO o_cx.
** Exception &cx_name& while saving file &filename& (&filetype&)
*            CREATE OBJECT o_cx_file
*              EXPORTING
*                textid   = zaplink_cx_file=>save_error
*                filename = _filename
*                filetype = _filetype
*                previous = o_cx.
*            o_cx_file->update( ).
*            RAISE EXCEPTION o_cx_file.
*        ENDTRY.
        RAISE EXCEPTION o_cx_file.
      ENDIF.
    ENDIF.
  ELSEIF _filetype = c_ft_server.
    TRY.
        OPEN DATASET _filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      CATCH cx_root INTO o_cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
        CREATE OBJECT o_cx_file
          EXPORTING
            textid   = zaplink_cx_file=>save_error
            filename = _filename
            filetype = _filetype
            previous = o_cx.
        o_cx_file->update( ).
        RAISE EXCEPTION o_cx_file.
    ENDTRY.
    IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>save_error
                    filename = _filename
                    filetype = _filetype
                     cx_name = 'OPEN DATASET'.
    ENDIF.

    LOOP AT tmptable ASSIGNING <s>.
      TRY.
          TRANSFER <s> TO _filename.
        CATCH cx_root INTO o_cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
          CREATE OBJECT o_cx_file
            EXPORTING
              textid   = zaplink_cx_file=>save_error
              filename = _filename
              filetype = _filetype
              previous = o_cx.
          o_cx_file->update( ).
          RAISE EXCEPTION o_cx_file.
      ENDTRY.
    ENDLOOP.

    TRY.
        CLOSE DATASET _filename.
      CATCH cx_root INTO o_cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
        CREATE OBJECT o_cx_file
          EXPORTING
            textid   = zaplink_cx_file=>save_error
            filename = _filename
            filetype = _filetype
            previous = o_cx.
        o_cx_file->update( ).
        RAISE EXCEPTION o_cx_file.
    ENDTRY.
    IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>save_error
                    filename = _filename
                    filetype = _filetype
                     cx_name = 'CLOSE DATASET'.
    ENDIF.
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.
  endmethod.


  method SAVE_DIALOG.
  DATA retfilename TYPE string.
  DATA retpath TYPE string.
  DATA retfullpath TYPE string.
  DATA retuseract TYPE i.

  IF filename IS INITIAL.
    retfilename = _filename.
  ELSE.
    retfilename = filename.
  ENDIF.
  IF _filetype = c_ft_local.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name = retfilename
      CHANGING
        filename          = retfilename
        path              = retpath
        fullpath          = retfullpath
        user_action       = retuseract.
    IF retuseract <> 0.
* Dialog canceled by user
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>dialog_canceled
                    filename = filename
                    filetype = _filetype.
    ENDIF.
    _filename = retfullpath.                                " Issue 69
  ELSEIF _filetype = c_ft_server.
* No dialog right now
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.
  endmethod.


  method SEARCH.
  DATA d_tmpdir  TYPE string.
  DATA d_tmpfile TYPE string.
  DATA d_guid    TYPE guid_32.
  DATA d_is_ok   TYPE abap_bool.
  DATA d_cmdline TYPE string.
  DATA t_filelst TYPE STANDARD TABLE OF string.
  DATA s_file    LIKE LINE OF result.
  DATA d_rc      TYPE sy-subrc.
  FIELD-SYMBOLS <f> LIKE LINE OF t_filelst.

  IF _filetype = c_ft_local.
    d_tmpdir = get_temp_directory( ).
    IF d_tmpdir IS INITIAL.
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>invalid_filetype
                    filetype = _filetype.
    ENDIF.
    DO.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = d_guid.
      CONCATENATE d_tmpdir file_sep d_guid INTO d_tmpfile.
      CALL METHOD cl_gui_frontend_services=>file_exist
        EXPORTING
          file                 = d_tmpfile
        RECEIVING
          result               = d_is_ok
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.
      IF sy-subrc = 0 AND d_is_ok IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

* example : /C "dir /S /B c:\GoogleCode\ZAPLINK\*.*.XML > c:\temp\12345678901234567890123456789012"
    CONCATENATE '/c' ' "dir /S /B "' root_dir file_sep mask '" > "' d_tmpfile '""' INTO d_cmdline.     "#EC NOTEXT

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
*        document               =
        application            = 'CMD'
        parameter              = d_cmdline
*        default_directory      =
*        maximized              =
        minimized              = 'X'
        synchronous            = 'X'                        " Issue 83
*        operation              = 'OPEN'
      EXCEPTIONS
        bad_parameter          = 1
        cntl_error             = 2
        error_execute_failed   = 3
        error_no_gui           = 4
        file_extension_unknown = 5
        file_not_found         = 6
        path_not_found         = 7
        not_supported_by_gui   = 8
        synchronous_failed     = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
* Exception '&CX_NAME&' while searching in '&FILENAME&'
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                       method = 'EXECUTE'
                                                        subrc = sy-subrc
                                                       textid = zaplink_cx_file=>search_error
                                                 cx_classname = 'ZAPLINK_CX_FILE' ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
*    wait up to 1 SECONDS.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = d_tmpfile
      RECEIVING
        result               = d_is_ok
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        wrong_parameter      = 4
        OTHERS               = 5.
    IF sy-subrc <> 0 OR d_is_ok IS INITIAL.
* Exception '&CX_NAME&' while searching in '&FILENAME&'
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                       method = 'FILE_EXIST'
                                                        subrc = sy-subrc
                                                       textid = zaplink_cx_file=>search_error
                                                 cx_classname = 'ZAPLINK_CX_FILE' ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = d_tmpfile
*        filetype                = 'ASC'
*        has_field_separator     = SPACE
*        header_length           = 0
*        read_by_line            = 'X'
*        dat_mode                = SPACE
*        codepage                = SPACE
*        ignore_cerr             = ABAP_TRUE
*        replacement             = '#'
*        virus_scan_profile      =
*      IMPORTING
*        filelength              =
*        header                  =
      CHANGING
        data_tab                = t_filelst
      EXCEPTIONS
        access_denied           = 1
        bad_data_format         = 2
        disk_full               = 3
        dp_out_of_memory        = 4
        dp_timeout              = 5
        error_no_gui            = 6
        file_open_error         = 7
        file_read_error         = 8
        gui_refuse_filetransfer = 9
        header_not_allowed      = 10
        header_too_long         = 11
        invalid_type            = 12
        not_supported_by_gui    = 13
        no_authority            = 14
        no_batch                = 15
        separator_not_allowed   = 16
        unknown_dp_error        = 17
        unknown_error           = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
* Exception '&CX_NAME&' while searching in '&FILENAME&'
      o_mycx ?= zaplink_cx=>create_from_method_cx( class_name = 'CL_GUI_FRONTEND_SERVICES'
                                                       method = 'FILE_EXIST'
                                                        subrc = sy-subrc
                                                       textid = zaplink_cx_file=>search_error
                                                 cx_classname = 'ZAPLINK_CX_FILE' ).
      RAISE EXCEPTION o_mycx.
    ENDIF.
    LOOP AT t_filelst ASSIGNING <f>.
      CLEAR s_file.   s_file-name = <f>.
      CALL METHOD cl_gui_frontend_services=>file_exist
        EXPORTING
          file                 = d_tmpfile
        RECEIVING
          result               = d_is_ok
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.
      IF sy-subrc = 0 AND d_is_ok = abap_true.
        s_file-kind = filedirkinds-file.
      ELSE.
        CALL METHOD cl_gui_frontend_services=>directory_exist
          EXPORTING
            directory            = d_tmpfile
          RECEIVING
            result               = d_is_ok
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5.
        IF sy-subrc = 0 AND d_is_ok = abap_true.
          s_file-kind = filedirkinds-directory.
        ELSE.
          s_file-kind = filedirkinds-unknown.
        ENDIF.
      ENDIF.
      APPEND s_file TO result.
    ENDLOOP.
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = d_tmpfile
      CHANGING
        rc                   = d_rc
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
* Ignore
    ELSEIF d_rc <> 0.
* Ignore
    ENDIF.

  ELSEIF _filetype = c_ft_server.
* No dialog right now
  ELSE.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = _filetype.
  ENDIF.
  endmethod.


  method SET_FILECONTENT.
  _content = data.
  endmethod.


  method SET_FILENAME.
  IF data IS INITIAL.
* Filename '&file&' is invalid. Please provide a valid filename for filetype '&filetype&'
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filename
                  filename = data
                  filetype = _filetype.
  ENDIF.
  IF NOT _filetype IS INITIAL.
* TODO Check filename
  ENDIF.
  _filename = data.
  endmethod.


  method SET_FILETYPE.
  IF data <> c_ft_local AND data <> c_ft_server.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filetype
                  filetype = data.
  ENDIF.
  IF NOT _filename IS INITIAL.
* TODO Check filename
  ENDIF.
  _filetype = data.
  endmethod.
ENDCLASS.
