class ZAPLINK_TABU_DATA definition
  public
  inheriting from ZAPLINK_RAW_DATA
  create public

  global friends ZAPLINK_DICTIONARY
                 ZAPLINK_EASYXML .

public section.

  interfaces ZAPLINK_DATATYPES .

  aliases TT_DOCS
    for ZAPLINK_DATATYPES~TT_DOCS .
  aliases TT_TEXTS
    for ZAPLINK_DATATYPES~TT_TEXTS .

  types TD_TABLE_NAME type DD02L-TABNAME .

  data CONTENT type ref to DATA .

  methods FROM_DATA
    importing
      !TABLE type TD_TABLE_NAME
    raising
      ZAPLINK_CX .
  methods TO_DATA .
protected section.

  aliases TD_DOC_ID
    for ZAPLINK_DATATYPES~TD_DOC_ID .
  aliases TR_DOCID
    for ZAPLINK_DATATYPES~TR_DOCID .
private section.
ENDCLASS.



CLASS ZAPLINK_TABU_DATA IMPLEMENTATION.


  method FROM_DATA.
  DATA d_name     TYPE ddobjname.
  DATA t_fields   TYPE STANDARD TABLE OF dd03p.
  DATA s_line     TYPE REF TO data.
  FIELD-SYMBOLS:
    <f> LIKE LINE OF t_fields,
    <s> TYPE ANY,
    <m> TYPE ANY,
    <t> TYPE STANDARD TABLE.

  CREATE DATA content TYPE STANDARD TABLE OF (table).
  ASSERT sy-subrc = 0.

  ASSIGN content->* TO <t>.
  ASSERT sy-subrc = 0.

  SELECT *
    INTO TABLE <t>
    FROM (table).
  ASSERT sy-subrc = 0.

  CHECK <t> IS NOT INITIAL.
  d_name = table.
  CALL FUNCTION 'DDIF_TABL_GET'
  EXPORTING
    name                = d_name
    state               = 'M'
*       LANGU               = ' '
*  IMPORTING
**       GOTSTATE            =
*    dd02v_wa            = s_fm_data-header
*    dd09l_wa            = s_fm_data-tech
  TABLES
    dd03p_tab           = t_fields
*    dd05m_tab           = s_fm_data-fk_fields
*    dd08v_tab           = s_fm_data-forein_keys
*    dd12v_tab           = s_fm_data-indexes
*    dd17v_tab           = s_fm_data-idx_fields
*    dd35v_tab           = s_fm_data-search_helps
*    dd36m_tab           = s_fm_data-sh_fields
  EXCEPTIONS
    illegal_input       = 1
    OTHERS              = 2.

  CREATE DATA s_line TYPE (table).
  ASSERT sy-subrc = 0.

  ASSIGN s_line->* TO <s>.
  ASSERT sy-subrc = 0.

  LOOP AT t_fields ASSIGNING <f>
          WHERE datatype = 'CLNT'
            AND keyflag = abap_true.
    LOOP AT <t> ASSIGNING <s>.
      ASSIGN COMPONENT <f>-fieldname OF STRUCTURE <s> TO <m>.
      ASSERT sy-subrc = 0.
      CLEAR <m>.
    ENDLOOP.
    EXIT.   " expect only one : Second one might be application value
  ENDLOOP.
  endmethod.


  method TO_DATA.
  endmethod.
ENDCLASS.
