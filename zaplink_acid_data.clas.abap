class ZAPLINK_ACID_DATA definition
  public
  inheriting from ZAPLINK_CHECKPOINTS_DATA
  create public

  global friends ZAPLINK_CHECKPOINTS
                 ZAPLINK_EASYXML .

public section.
protected section.

  types:
    BEGIN OF ts_fm_data,
      header          TYPE aab_id_prop,     " aab_id_sfields,
      descriptions    TYPE STANDARD TABLE OF aab_id_propt WITH DEFAULT KEY,
      modes           TYPE aab_id_act_tab,
    END OF ts_fm_data .
  types:
    BEGIN OF ts_maindata.
  INCLUDE TYPE aab_id_sfields.
  TYPES:
    END OF ts_maindata .
  types:
    BEGIN OF ts_4user,
      user    TYPE aab_id_act-username.
  INCLUDE TYPE ts_activation as activation.
  TYPES:
    END OF ts_4user .
  types:
    tt_users TYPE SORTED TABLE OF ts_4user WITH UNIQUE KEY user .
  types:
    BEGIN OF ts_4server,
      server  TYPE aab_id_act-server.
  INCLUDE TYPE ts_activation as activation.
  TYPES:
    END OF ts_4server .
  types:
    tt_servers TYPE SORTED TABLE OF ts_4server WITH UNIQUE KEY server .

  data A0_MAINDATA type TS_MAINDATA .
  data GLOBAL_SETTINGS type TS_ACTIVATION .
  data SETTINGS_FOR_USERS type TT_USERS .
  data SETTINGS_FOR_SERVERS type TT_SERVERS .
  data DESCRIPTIONS type TT_LTEXTS .

  methods FROM_DATA
    importing
      !FM_DATA type TS_FM_DATA .
  methods TO_DATA
    returning
      value(FM_DATA) type TS_FM_DATA .
  methods ANONYMIZE .
  methods UNANONYMIZE .
private section.
ENDCLASS.



CLASS ZAPLINK_ACID_DATA IMPLEMENTATION.


  method ANONYMIZE.
  CLEAR: a0_maindata-create_user,  a0_maindata-create_date.
*ACT_USER
*ACT_DATE
*ACT_TIME
  endmethod.


  method FROM_DATA.
  DATA s_user     LIKE LINE OF settings_for_users.
  DATA s_server   LIKE LINE OF settings_for_servers.
  DATA s_desc     LIKE LINE OF descriptions.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF fm_data-descriptions,
    <m> LIKE LINE OF fm_data-modes.

*  a0_maindata = fm_data-header.
*  CLEAR: a0_maindata-devclass_txt, a0_maindata-masterlang_txt, a0_maindata-component_txt.
  LOOP AT fm_data-descriptions ASSIGNING <d>.
    CLEAR s_desc.   s_desc-lang = <d>-langu.     s_desc-text = <d>-descript.   INSERT s_desc INTO TABLE descriptions.
  ENDLOOP.

*ACT_USERS_INFO
*ACT_SERVERS_INFO
  LOOP AT fm_data-modes ASSIGNING <m>.
    IF NOT <m>-username IS INITIAL.
      CLEAR s_user.   s_user-user = <m>-username.   s_user-activation = conv_mode2activation( <m>-actmode ).
      INSERT s_user INTO TABLE settings_for_users.
    ELSEIF NOT <m>-server IS INITIAL.
      CLEAR s_server.   s_server-server = <m>-server.   s_server-activation = conv_mode2activation( <m>-actmode ).
      INSERT s_server INTO TABLE settings_for_servers.
    ELSEIF NOT <m>-actdefault IS INITIAL OR global_settings IS INITIAL.
      global_settings = conv_mode2activation( <m>-actmode ).
    ENDIF.
  ENDLOOP.
  endmethod.


  method TO_DATA.
  DATA s_mode     LIKE LINE OF fm_data-modes.
  DATA s_server   LIKE LINE OF settings_for_servers.
  DATA s_desc     LIKE LINE OF fm_data-descriptions.
  FIELD-SYMBOLS:
    <d> LIKE LINE OF descriptions,
    <s> LIKE LINE OF settings_for_servers,
    <u> LIKE LINE OF settings_for_users.

  LOOP AT descriptions ASSIGNING <d>.
    CLEAR s_desc.   s_desc-langu = <d>-lang.     s_desc-descript = <d>-text.   INSERT s_desc INTO TABLE fm_data-descriptions.
  ENDLOOP.

  s_mode-actmode = conv_activation2mode( global_settings ).     s_mode-actdefault = abap_true.
  INSERT s_mode INTO TABLE fm_data-modes.
*ACT_USERS_INFO
  LOOP AT settings_for_users ASSIGNING <u>.
    CLEAR s_mode.     s_mode-username = <u>-user.   s_mode-actmode = conv_activation2mode( <u>-activation ).
    INSERT s_mode INTO TABLE fm_data-modes.
  ENDLOOP.

*ACT_SERVERS_INFO
  LOOP AT settings_for_servers ASSIGNING <s>.
    CLEAR s_mode.     s_mode-server = <s>-server.   s_mode-actmode = conv_activation2mode( <s>-activation ).
    INSERT s_mode INTO TABLE fm_data-modes.
  ENDLOOP.
  endmethod.


  method UNANONYMIZE.
  endmethod.
ENDCLASS.
