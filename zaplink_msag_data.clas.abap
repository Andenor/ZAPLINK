class ZAPLINK_MSAG_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_EASYXML
                 ZAPLINK_MESSAGE .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TS_LANG
    for ZAPLINK_DATATYPES~TS_LANG .

  types:
    BEGIN OF ts_maindata.
    INCLUDE TYPE t100a.
    TYPES:
      END OF ts_maindata .
  types:
    BEGIN OF ts_text,
* T100T
          langu      TYPE spras,
          short_txt	 TYPE string,
          long_txt   TYPE ts_lang,
        END OF ts_text .
  types:
    tt_texts TYPE SORTED TABLE OF ts_text WITH UNIQUE KEY langu .
  types:
    BEGIN OF ts_msgtext,
          msgnr	      TYPE msgnr,
* T100U
          name        TYPE syuname,
          datum	      TYPE sydatum,
          selfdef	    TYPE doku_selfd,
* Documetation : long text
          application TYPE tdobject,
          typ	        TYPE doku_typ,
          dokform	    TYPE tdformnew,
          dokstyle    TYPE tdstyle,
* T100 & documentation
          texts       TYPE tt_texts,
        END OF ts_msgtext .
  types:
    tt_msgtexts TYPE SORTED TABLE OF ts_msgtext WITH UNIQUE KEY msgnr .

  data A0_MAINDATA type TS_MAINDATA .
  data TEXTS type TT_TEXTS .
  data MESSAGES type TT_MSGTEXTS .

  methods ANONYMIZE .
  methods UNANONYMIZE .
protected section.
private section.
ENDCLASS.



CLASS ZAPLINK_MSAG_DATA IMPLEMENTATION.


  method ANONYMIZE.
  DATA s_msg LIKE LINE OF messages.

  CLEAR:
*    a0_maindata-respuser,    " Person respons.
    a0_maindata-lastuser,
    a0_maindata-ldate,
    a0_maindata-ltime.

  MODIFY messages FROM s_msg TRANSPORTING name datum
         WHERE NOT msgnr IS INITIAL OR msgnr IS INITIAL.
  endmethod.


  method UNANONYMIZE.
  DATA s_msg LIKE LINE OF messages.

  IF a0_maindata-respuser IS INITIAL. a0_maindata-respuser = sy-uname. ENDIF.
  s_msg-name = a0_maindata-respuser.
  MODIFY messages FROM s_msg TRANSPORTING name WHERE name IS INITIAL.
  endmethod.
ENDCLASS.
