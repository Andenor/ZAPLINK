*"* USE THIS SOURCE FILE FOR ANY MACRO DEFINITIONS YOU NEED
*"* in the implementation part of the class


* Add error message to application log.
DEFINE mac_syserr_log.
  IF &1 <> 0.
    SET EXTENDED CHECK OFF.
    IF 1 = 2. MESSAGE ID msgid TYPE 'E' NUMBER 000 WITH abap_true abap_true abap_true. ENDIF.
    SET EXTENDED CHECK ON.
*      IF not application_log is bound. MESSAGE ID MSGID TYPE 'E' NUMBER 000 WITH abap_true abap_true abap_true. ENDIF.
* Assuming application log is created
    crc = &1.
* System Error &1 : on &2 in &3
    CALL METHOD application_log->add_error
      EXPORTING
*          id_msgid     = MSGID
        id_msgno     = '000'
        id_msgv1     = crc
        id_msgv2     = &2
        id_msgv3     = &3
*          id_msgv4     =
*          id_detlevel  =
*          id_probclass =
        .
  ENDIF.
END-OF-DEFINITION.
