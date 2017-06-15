*&---------------------------------------------------------------------*
*& Report  ZAPLINK_INFOS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zaplink_infos MESSAGE-ID zaplink.

PARAMETERS p_file LOWER CASE TYPE string DEFAULT 'C:\ZAPLink.info.xml'.

DATA o_file     TYPE REF TO zaplink_file.
DATA o_cx_file  TYPE REF TO zaplink_cx_file.

INITIALIZATION.
  CREATE OBJECT o_file.
  o_file->set_filetype( zaplink_file=>filetypes-local ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  DATA l_file TYPE zaplink_file=>t_filename.
  DATA d_msg  TYPE string.

  TRY.
      o_file->load_dialog( p_file ).
      p_file = o_file->get_filename( ).
    CATCH zaplink_cx_file INTO o_cx_file.
      CHECK o_cx_file->is_exception_text( zaplink_cx_file=>dialog_canceled ) IS INITIAL.
      d_msg = o_cx_file->get_text( ).
      MESSAGE d_msg TYPE 'W'.
  ENDTRY.

START-OF-SELECTION.
  DATA o_infos TYPE REF TO zaplink_infos.
  DATA o_ezxml TYPE REF TO zaplink_easyxml.
  DATA o_xml   TYPE REF TO if_ixml_document.
  DATA o_xmlengine  TYPE REF TO if_ixml.
  DATA o_streamfactory TYPE REF TO if_ixml_stream_factory.
  DATA o_outputstream TYPE REF TO if_ixml_ostream.
  DATA o_renderer TYPE REF TO if_ixml_renderer.
  DATA d_tempstr TYPE string.
  DATA o_printxmldoc TYPE REF TO cl_xml_document.
  DATA t_xml TYPE STANDARD TABLE OF string.
  FIELD-SYMBOLS <l> LIKE LINE OF t_xml.

  o_xmlengine = cl_ixml=>create( ).
  o_streamfactory = o_xmlengine->create_stream_factory( ).
  o_outputstream = o_streamfactory->create_ostream_cstring( d_tempstr ).
  CREATE OBJECT o_infos.
  CREATE OBJECT o_ezxml.
  o_xml = o_ezxml->any2xml( any = o_infos
                           type = 'INFOS' ).
  o_renderer = o_xmlengine->create_renderer( document = o_xml
                                              ostream = o_outputstream ).
  o_renderer->set_normalizing( ).
  o_renderer->render( ).
  CREATE OBJECT o_printxmldoc.
  o_printxmldoc->parse_string( d_tempstr ).

  WHILE d_tempstr(1) <> '<'.    SHIFT d_tempstr LEFT BY 1 PLACES.   ENDWHILE.
  SPLIT d_tempstr AT cl_abap_char_utilities=>newline INTO TABLE t_xml.
  LOOP AT t_xml ASSIGNING <l>.  WRITE:/ <l>.  ENDLOOP.
  o_file->set_filename( p_file ).
  o_file->set_filecontent( d_tempstr ).
  o_file->save( ).
