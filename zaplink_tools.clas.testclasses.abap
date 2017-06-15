*----------------------------------------------------------------------*
*       CLASS abap_unit_testclass DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass DEFINITION FOR TESTING "#AU Risk_Level Harmless
      "#AU Duration Short
  .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      m_ref TYPE REF TO zaplink_tools.                      "#EC NOTEXT

    METHODS: setup.
    METHODS: teardown.
    METHODS: calculate_md5_hash FOR TESTING.
ENDCLASS.       "Abap_Unit_Testclass
* ======================================================================
CLASS abap_unit_testclass IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT m_ref.
  ENDMETHOD.       "Setup
* ----------------------------------------------------------------------
  METHOD teardown.
  ENDMETHOD.       "Teardown
* ----------------------------------------------------------------------
  METHOD calculate_md5_hash.
    DATA string TYPE string.
    DATA result TYPE zaplink_datatypes=>td_checksum.
*    DATA o_file TYPE REF TO zaplink_file.

    CONCATENATE '... FOR TESTING [RISK LEVEL {CRITICAL|DANGEROUS|HARMLESS}]'
                '                [DURATION   {SHORT|MEDIUM|LONG}]  ... . ' INTO string SEPARATED BY cl_abap_char_utilities=>cr_lf.
    result = zaplink_tools=>calculate_md5_hash( string ).

    cl_aunit_assert=>assert_equals(
      act   = result
      exp   = '4993052479518AEA317C383B8764919A'
      msg   = text-t01
*      level = cl_aunit_assert=>tolerable
    ).

*    CREATE OBJECT o_file.
*    o_file->set_filetype( zaplink_file=>filetypes-local ).
*    o_file->load_dialog( ).
*    o_file->load( ).
*    string = o_file->get_filecontent( ).
*    result = zaplink_tools=>calculate_md5_hash( string ).
*
*    cl_aunit_assert=>assert_equals(
*      act   = result
*      exp   = '4993052479518AEA317C383B8764919A'
*      msg   = text-t01
*      level = cl_aunit_assert=>tolerable
*    ).

  ENDMETHOD.       "Calculate_Md5_Hash
ENDCLASS.       "Abap_Unit_Testclass
