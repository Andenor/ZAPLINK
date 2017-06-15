*"* USE THIS SOURCE FILE FOR ANY MACRO DEFINITIONS YOU NEED
*"* IN THE IMPLEMENTATION PART OF THE CLASS

INCLUDE zaplink_macros.

DEFINE lmac_add_mf_and_raise.
  o_mycx ?= zaplink_cx=>create_from_mf_cx( funcname = &1
                                              subrc = &2
                                          classname = 'ZAPLINK_CX_CONNECTOR' ).
  log->add_symsg( ).
  log->add_exception( o_mycx ).
  raise exception o_mycx.
END-OF-DEFINITION.
