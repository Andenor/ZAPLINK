class ZAPLINK_FILE_ZIP definition
  public
  inheriting from ZAPLINK_FILE
  create public .

public section.

  types TD_FILEKIND type CHAR1 .

  constants:
    BEGIN OF filekinds,
        normal    TYPE td_filekind VALUE space,              "#EC NOTEXT
        zipped    TYPE td_filekind VALUE 'Z',                "#EC NOTEXT
      END OF filekinds .

  methods GET_FILEKIND
    returning
      value(RESULT) type TD_FILEKIND
    raising
      ZAPLINK_CX_FILE .
  methods SET_FILEKIND
    importing
      !DATA type TD_FILEKIND
    raising
      ZAPLINK_CX_FILE .

  methods LOAD
    redefinition .
  methods SAVE
    redefinition .
protected section.

  data _FILEKIND type TD_FILEKIND value FILEKINDS-ZIPPED ##NO_TEXT.
  constants ZIP_FILE_EXTENSION type TD_FILENAME value '.zip' ##NO_TEXT.
  constants FILENAME_IN_ZIP type TD_FILENAME value 'container.xml' ##NO_TEXT.
private section.
ENDCLASS.



CLASS ZAPLINK_FILE_ZIP IMPLEMENTATION.


  method GET_FILEKIND.
result = _filekind.
  endmethod.


  method LOAD.
  DATA _cx       TYPE REF TO zaplink_cx_file.
  DATA tmptable  TYPE STANDARD TABLE OF x.
  DATA cx_name   TYPE string.
  DATA cx        TYPE REF TO cx_root.
  DATA x_string  TYPE xstring.
  DATA string_size  TYPE i.
  DATA file_type TYPE char10.
  DATA file_size TYPE i.
  DATA _str    LIKE LINE OF tmptable.
  DATA o_zip   TYPE REF TO cl_abap_zip.
  DATA o_conv  TYPE REF TO cl_abap_conv_in_ce.
  DATA x TYPE string.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF tmptable.

  CASE _filekind.
    WHEN filekinds-normal.
      TRY.
          super->load( ).
        CATCH zaplink_cx_file INTO _cx.
          RAISE EXCEPTION _cx.
      ENDTRY.
    WHEN filekinds-zipped.

      IF _filetype = c_ft_local.
        IF _filename IS INITIAL.
          load_dialog( ).
        ELSE.
          file_type = 'BIN'.      " change in debug
          CALL METHOD cl_gui_frontend_services=>gui_upload
            EXPORTING
              filename                 = _filename
              filetype                 = file_type
*              has_field_separator     = SPACE
*              header_length           = 0
*              read_by_line            = 'X'
*              dat_mode                = SPACE
*              codepage                = SPACE
*              ignore_cerr             = ABAP_TRUE
*              replacement             = '#'
*              virus_scan_profile      =
            IMPORTING
              filelength              = file_size
*              header                  =
            CHANGING
              data_tab                = tmptable
            EXCEPTIONS
              file_open_error         = 1
              file_read_error         = 2
              no_batch                = 3
              gui_refuse_filetransfer = 4
              invalid_type            = 5
              no_authority            = 6
              unknown_error           = 7
              bad_data_format         = 8
              header_not_allowed      = 9
              separator_not_allowed   = 10
              header_too_long         = 11
              unknown_dp_error        = 12
              access_denied           = 13
              dp_out_of_memory        = 14
              disk_full               = 15
              dp_timeout              = 16
              not_supported_by_gui    = 17
              error_no_gui            = 18
              OTHERS                  = 19.
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN '1'.
                cx_name = ''.
              WHEN OTHERS.
                cx_name = 'OTHERS'.
            ENDCASE.
* Exception &cx_name& while saving file &filename& (&filetype&)
            RAISE EXCEPTION TYPE zaplink_cx_file
                  EXPORTING textid = zaplink_cx_file=>save_error
                          filename = _filename
                          filetype = _filetype
                           cx_name = cx_name.
          ENDIF.
          file_size = LINES( tmptable ).
          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
            EXPORTING
              input_length       = file_size
*             FIRST_LINE         = 0
*             LAST_LINE          = 0
            IMPORTING
              buffer             = x_string
            TABLES
              binary_tab         = tmptable
           EXCEPTIONS
             failed             = 1
             OTHERS             = 2
                    .
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        ENDIF.
      ELSEIF _filetype = c_ft_server.
        TRY.
            OPEN DATASET _filename FOR INPUT IN BINARY MODE.
          CATCH cx_root INTO cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
            CREATE OBJECT _cx
              EXPORTING
                textid   = zaplink_cx_file=>save_error
                filename = _filename
                filetype = _filetype
                previous = cx.
            _cx->update( ).
            RAISE EXCEPTION _cx.
        ENDTRY.
        IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
          RAISE EXCEPTION TYPE zaplink_cx_file
                EXPORTING textid = zaplink_cx_file=>save_error
                        filename = _filename
                        filetype = _filetype
                         cx_name = 'OPEN DATASET'.
        ENDIF.

*    DO.
        TRY.
            READ DATASET _filename INTO x_string.
          CATCH cx_root INTO cx.
* Exception &cx_name& while loading file &filename& (&filetype&)
            CREATE OBJECT _cx
              EXPORTING
                textid   = zaplink_cx_file=>load_error
                filename = _filename
                filetype = _filetype
                previous = cx.
            _cx->update( ).
            RAISE EXCEPTION _cx.
        ENDTRY.
*      IF sy-subrc <> 0.
*        EXIT.
*      ENDIF.
*      APPEND _str TO tmptable.
*    ENDDO.

        TRY.
            CLOSE DATASET _filename.
          CATCH cx_root INTO cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
            CREATE OBJECT _cx
              EXPORTING
                textid   = zaplink_cx_file=>save_error
                filename = _filename
                filetype = _filetype
                previous = cx.
            _cx->update( ).
            RAISE EXCEPTION _cx.
        ENDTRY.
        IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
          RAISE EXCEPTION TYPE zaplink_cx_file
                EXPORTING textid = zaplink_cx_file=>save_error
                        filename = _filename
                        filetype = _filetype
                         cx_name = 'CLOSE DATASET'.
        ENDIF.
      ENDIF.

      TRY.
          CREATE OBJECT o_zip.
          CALL METHOD o_zip->load
            EXPORTING
              zip             = x_string
            EXCEPTIONS
              zip_parse_error = 1
              OTHERS          = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          CALL METHOD o_zip->get
             EXPORTING
               name                    = filename_in_zip
*              index                   = 0
             IMPORTING
               content                 = x_string
             EXCEPTIONS
               zip_index_error         = 1
               zip_decompression_error = 2
               OTHERS                  = 3.
          IF sy-subrc <> 0.
            CALL METHOD o_zip->get
               EXPORTING
*               name                    = filename_in_zip
                 index                   = 1
               IMPORTING
                 content                 = x_string
               EXCEPTIONS
                 zip_index_error         = 1
                 zip_decompression_error = 2
                 OTHERS                  = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.

          o_conv = cl_abap_conv_in_ce=>create( ).
          o_conv->convert( EXPORTING input = x_string
                           IMPORTING  data = _content ).
        CATCH cx_root INTO cx.
          CREATE OBJECT _cx
            EXPORTING
              textid   = zaplink_cx_file=>save_error
              filename = _filename
              filetype = _filetype
              previous = cx.
          _cx->update( ).
          RAISE EXCEPTION _cx.
      ENDTRY.

    WHEN OTHERS.
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>invalid_filetype
                    filetype = _filetype.
  ENDCASE.
  endmethod.


  method SAVE.
  DATA _cx TYPE REF TO zaplink_cx_file.
  DATA tmptable TYPE STANDARD TABLE OF x.
  DATA cx_name TYPE string.
  DATA cx TYPE REF TO cx_root.
  DATA x_string TYPE xstring.
  DATA file_type TYPE char10.
  DATA file_size TYPE i.
  DATA o_zip   TYPE REF TO cl_abap_zip.
  DATA o_conv  TYPE REF TO cl_abap_conv_out_ce.
  FIELD-SYMBOLS:
    <s> LIKE LINE OF tmptable.

  CASE _filekind.
    WHEN filekinds-normal.
      TRY.
          super->save( ).
        CATCH zaplink_cx_file INTO _cx.
          RAISE EXCEPTION _cx.
      ENDTRY.
    WHEN filekinds-zipped.

      TRY.
          o_conv = cl_abap_conv_out_ce=>create( ).
          o_conv->convert( EXPORTING   data = _content
                           IMPORTING buffer = x_string ).
          CREATE OBJECT o_zip.
          CALL METHOD o_zip->add
            EXPORTING
              name    = filename_in_zip
              content = x_string.
          x_string = o_zip->save( ).
        CATCH cx_root INTO cx.
          CREATE OBJECT _cx
            EXPORTING
              textid   = zaplink_cx_file=>save_error
              filename = _filename
              filetype = _filetype
              previous = cx.
          _cx->update( ).
          RAISE EXCEPTION _cx.
      ENDTRY.

      IF _filetype = c_ft_local.
        IF _filename IS INITIAL.
          save_dialog( ).
        ELSE.
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer                = x_string
*       APPEND_TO_TABLE       = ' '
           IMPORTING
             output_length         = file_size
            TABLES
              binary_tab            =  tmptable.

          CONCATENATE _filename zip_file_extension INTO _filename.
          file_type = 'BIN'.      " change in debug
          CALL METHOD cl_gui_frontend_services=>gui_download
            EXPORTING
              bin_filesize              = file_size
              filename                  = _filename
              filetype                  = file_type
*      append                    = SPACE
*      write_field_separator     = SPACE
*      header                    = '00'
*      trunc_trailing_blanks     = SPACE
*      write_lf                  = 'X'
*      col_select                = SPACE
*      col_select_mask           = SPACE
*      dat_mode                  = SPACE
*      confirm_overwrite         = SPACE
*      no_auth_check             = SPACE
*      codepage                  = SPACE
*      ignore_cerr               = ABAP_TRUE
*      replacement               = '#'
*      write_bom                 = SPACE
*      trunc_trailing_blanks_eol = 'X'
*      wk1_n_format              = SPACE
*      wk1_n_size                = SPACE
*      wk1_t_format              = SPACE
*      wk1_t_size                = SPACE
*    IMPORTING
*      filelength                =
            CHANGING
              data_tab                  = tmptable
            EXCEPTIONS
              file_write_error          = 1
              no_batch                  = 2
              gui_refuse_filetransfer   = 3
              invalid_type              = 4
              no_authority              = 5
              unknown_error             = 6
              header_not_allowed        = 7
              separator_not_allowed     = 8
              filesize_not_allowed      = 9
              header_too_long           = 10
              dp_error_create           = 11
              dp_error_send             = 12
              dp_error_write            = 13
              unknown_dp_error          = 14
              access_denied             = 15
              dp_out_of_memory          = 16
              disk_full                 = 17
              dp_timeout                = 18
              file_not_found            = 19
              dataprovider_exception    = 20
              control_flush_error       = 21
              not_supported_by_gui      = 22
              error_no_gui              = 23
              OTHERS                    = 24.
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN '1'.
                cx_name = ''.
              WHEN OTHERS.
                cx_name = 'OTHERS'.
            ENDCASE.
* Exception &cx_name& while saving file &filename& (&filetype&)
            RAISE EXCEPTION TYPE zaplink_cx_file
                  EXPORTING textid = zaplink_cx_file=>save_error
                          filename = _filename
                          filetype = _filetype
                           cx_name = cx_name.
          ENDIF.
        ENDIF.
      ELSEIF _filetype = c_ft_server.
        TRY.
            OPEN DATASET _filename FOR OUTPUT IN BINARY MODE.
          CATCH cx_root INTO cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
            CREATE OBJECT _cx
              EXPORTING
                textid   = zaplink_cx_file=>save_error
                filename = _filename
                filetype = _filetype
                previous = cx.
            _cx->update( ).
            RAISE EXCEPTION _cx.
        ENDTRY.
        IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
          RAISE EXCEPTION TYPE zaplink_cx_file
                EXPORTING textid = zaplink_cx_file=>save_error
                        filename = _filename
                        filetype = _filetype
                         cx_name = 'OPEN DATASET'.
        ENDIF.

        TRY.
            TRANSFER x_string TO _filename.
          CATCH cx_root INTO cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
            CREATE OBJECT _cx
              EXPORTING
                textid   = zaplink_cx_file=>save_error
                filename = _filename
                filetype = _filetype
                previous = cx.
            _cx->update( ).
            RAISE EXCEPTION _cx.
        ENDTRY.

        TRY.
            CLOSE DATASET _filename.
          CATCH cx_root INTO cx.
* Exception &cx_name& while saving file &filename& (&filetype&)
            CREATE OBJECT _cx
              EXPORTING
                textid   = zaplink_cx_file=>save_error
                filename = _filename
                filetype = _filetype
                previous = cx.
            _cx->update( ).
            RAISE EXCEPTION _cx.
        ENDTRY.
        IF sy-subrc <> 0.
* Exception &cx_name& while saving file &filename& (&filetype&)
          RAISE EXCEPTION TYPE zaplink_cx_file
                EXPORTING textid = zaplink_cx_file=>save_error
                        filename = _filename
                        filetype = _filetype
                         cx_name = 'CLOSE DATASET'.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      RAISE EXCEPTION TYPE zaplink_cx_file
            EXPORTING textid = zaplink_cx_file=>invalid_filetype
                    filetype = _filetype.
  ENDCASE.
  endmethod.


  method SET_FILEKIND.
IF     data <> filekinds-normal
     AND data <> filekinds-zipped.
    RAISE EXCEPTION TYPE zaplink_cx_file
          EXPORTING textid = zaplink_cx_file=>invalid_filekind
                  filekind = data.
  ENDIF.

  _filekind = data.
  endmethod.
ENDCLASS.
