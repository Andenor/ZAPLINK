*---------------------------------------------------------------------*
*  Include           ZAPLINLK_TOP
*---------------------------------------------------------------------*
TABLES: sscrfields, e071, tadir.

TYPE-POOLS: icon, trwbo, seox.

TYPES t_objlist  TYPE zaplink_gui=>to_list.
TYPES t_comp     TYPE zaplink_gui=>to_component.
TYPES t_msg_col  TYPE zaplink_gui=>to_msg_coll.
TYPES t_exception TYPE REF TO zaplink_cx.

TYPES t_filename TYPE zaplink_gui=>td_filename.
TYPES to_file TYPE zaplink_gui=>to_file.
TYPES td_name TYPE zaplink_gui=>td_contname.
TYPES t_obj_type TYPE zaplink_gui=>td_comptype.
TYPES t_obj_name TYPE zaplink_gui=>td_compname.
TYPES td_devc_sk TYPE zaplink_opt_devclass=>td_substitutionkind.

DATA is_selection        TYPE trwbo_selection.

DATA objlist TYPE t_objlist.
DATA o_file TYPE to_file.
DATA d_name TYPE td_name.
DATA obj_type TYPE t_obj_type.
DATA obj_name TYPE t_obj_name.
DATA o_exception TYPE t_exception.
