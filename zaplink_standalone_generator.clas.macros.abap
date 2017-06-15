*"* USE THIS SOURCE FILE FOR ANY MACRO DEFINITIONS YOU NEED
*"* IN THE IMPLEMENTATION PART OF THE CLASS

  DEFINE mac_read_tadir.
    select single devclass dlvunit into (&4, &5)
      from v_tralan
      where pgmid = &1
        and object = &2
        and obj_name = &3.
  END-OF-DEFINITION.

  DEFINE mac_add_comp.
    if not s_comp-kind is initial and not s_comp-type is initial and not s_comp-name is initial.
      mac_read_tadir s_comp-kind s_comp-type s_comp-name s_comp-devclass s_comp-softcomp.
      if s_comp-devclass in packages and s_comp-softcomp in softcomponents.
        insert s_comp into table all_components.
      endif.
    endif.
  END-OF-DEFINITION.
