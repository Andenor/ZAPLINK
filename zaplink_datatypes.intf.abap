interface ZAPLINK_DATATYPES
  public .

  type-pools ABAP .

  types TD_CHECKBOX type ABAP_BOOL .
  types:
    tt_gensetc TYPE STANDARD TABLE OF gensetc WITH DEFAULT KEY .
  types TD_TRANSPORT_KIND type KO100-PGMID .
  types TD_COMPADD_DATA type STRING .
  types TD_FRAMEWORKVER type STRING .
  types TD_WITH_SUBCOMP type ZAPLINK_SUBCOMPONENT_LEVEL .
  types TD_TYPEORDER type I .
  types TD_ORDERKIND type CHAR1 .
  types TD_CHECKSUM type MD5_FIELDS-HASH .
  types TD_SUBSTITUTIONKIND type CHAR1 .
  types TD_DEVCLASS type TADIR-DEVCLASS .
  types TD_SRCSYSTEM type TADIR-SRCSYSTEM .
  types:
    BEGIN OF ts_directory,
* TADIR
*      PGMID Type TADIR-PGMID,
*      OBJECT Type TADIR-OBJECT,
*      OBJ_NAME Type TADIR-OBJ_NAME,
      korrnum TYPE tadir-korrnum,
      srcsystem TYPE td_srcsystem,
      author TYPE tadir-author,
      srcdep TYPE tadir-srcdep,
      devclass TYPE td_devclass,
      genflag TYPE tadir-genflag,
      edtflag TYPE tadir-edtflag,
*      CPROJECT Type TADIR-CPROJECT,
      masterlang TYPE tadir-masterlang,
*      VERSID Type TADIR-VERSID,
      paknocheck TYPE tadir-paknocheck,
      objstablty TYPE tadir-objstablty,
      component TYPE tadir-component,
      crelease TYPE tadir-crelease,
*      delflag TYPE tadir-delflag,
      translttxt TYPE tadir-translttxt,
* E071
      trkorr TYPE e070-trkorr,
    END OF ts_directory .
  types TD_CONNUUID type GUID_16 .
  types TD_TYPE_AS_TEXT type KO100-TEXT .
  types TD_COMPTEXT type KO100-TEXT .
  types TD_CONTNAME type STRING .
  types TD_CONTVER type STRING .
  types TD_FILENAME type STRING .
  types TD_FILETYPE type CHAR1 .
  types TD_TRANSPORT_REQUEST type E070-TRKORR .
  types TD_ABAPSOURCE type STRING .
  types:
    BEGIN OF ts_sourceasstruc,
      _ TYPE td_abapsource,
    END OF ts_sourceasstruc .
  types TD_PROGNAME type PROGRAMM .
  types TT_ABAPRAWSOURCE type RSWSOURCET .
  types TS_TOKEN type STOKES .
  types TS_STATMENT type SSTMNT .
  types:
    tt_tokens     TYPE STANDARD TABLE OF stokes .
  types:
    tt_statments  TYPE STANDARD TABLE OF sstmnt .
  types TO_XML type ref to IF_IXML_DOCUMENT .
  types TD_LIGHT type ICON_L4 .
  types TD_ACTION type CHAR1 .
  types TD_CONNVER type STRING .
  types TD_EXISTS type ABAP_BOOL .
  types TD_CLASSNAME type SEOCLSNAME .
  types TD_COMPEXISTS type TD_EXISTS .
  types TD_CONNCLASS type TD_CLASSNAME .
  types TD_CONNEXISTS type TD_EXISTS .
  types TD_OBJ_TYPE type ABAP_ABSTYPENAME .
  types TD_COMPNAME type STRING .
  types TD_COMPTYPE type TADIR-OBJECT .
  types:
    BEGIN OF ts_compkey,
      type TYPE td_comptype,
      name TYPE td_compname,
    END OF ts_compkey .
  types:
    tt_compkeys TYPE STANDARD TABLE OF ts_compkey WITH DEFAULT KEY .
  types:
    tr_tr TYPE RANGE OF td_transport_request .
  types:
    tr_packages TYPE RANGE OF tadir-devclass .
  types:
    tr_comptype TYPE RANGE OF td_comptype .
  types:
    tr_compname TYPE RANGE OF tadir-obj_name .
  types:
    tr_author TYPE RANGE OF tadir-author .
  types:
    tr_srcsystem TYPE RANGE OF tadir-srcsystem .
  types TD_SOFTCOMP type TDEVC-DLVUNIT .
  types:
    tr_dlvunit TYPE RANGE OF tdevc-dlvunit .
  types:
    BEGIN OF ts_base_attributs,
        name     TYPE td_compname,
        class    TYPE td_connclass,
        devclass TYPE td_devclass,
        version  TYPE td_connver,
      END OF ts_base_attributs .
  types:
    BEGIN OF ts_conn_class,
      uuid      TYPE td_connuuid,
      classname TYPE td_connclass,
      devclass  TYPE td_devclass,
      END OF ts_conn_class .
  types:
    tt_conn_classes TYPE SORTED TABLE OF ts_conn_class WITH UNIQUE KEY uuid .
  types:
    BEGIN OF ts_conn_def,
          object        TYPE ko100-object,
          text          TYPE ko100-text,
          default_class TYPE td_connclass,
          classes       TYPE tt_conn_classes,
        END OF ts_conn_def .
  types:
    tt_typelist TYPE SORTED TABLE OF ts_conn_def WITH UNIQUE KEY object .
  types:
    BEGIN OF ts_type,
      type TYPE td_comptype,
    END OF ts_type .
  types:
    tt_types TYPE SORTED TABLE OF ts_type WITH UNIQUE KEY type .
  types TD_LANG type SPRAS .
  types TD_TXTP_TEXT type TEXTPOOLTX .
  types TD_TXTP_LEN type TEXTPOOLLN .
  types TD_TXTP_KIND type TEXTPOOLID .
  types TD_TXTP_ID type TEXTPOOLKY .
  types:
    BEGIN OF ts_txtp_text,
        langu   TYPE td_lang,
        entry	  TYPE td_txtp_text,
        length  TYPE td_txtp_len,
      END OF ts_txtp_text .
  types:
    tt_txtp_texts TYPE SORTED TABLE OF ts_txtp_text WITH UNIQUE KEY langu .
  types:
    BEGIN OF ts_txtp_textpool,
        id    TYPE  td_txtp_kind,
        key	  TYPE  td_txtp_id,
        texts TYPE  tt_txtp_texts,
      END OF ts_txtp_textpool .
  types:
    tt_txtp_textpools TYPE SORTED TABLE OF ts_txtp_textpool WITH UNIQUE KEY id KEY .
  types:
    BEGIN OF ts_conndata,
      zsl_name      TYPE td_compname,         " component name
      zsl_connector TYPE td_connclass,        " Connector class name
      zsl_version   TYPE td_connver,          " Connector version
    END OF ts_conndata .
  types:
    BEGIN OF ts_contdata,
          name TYPE td_contname,
        END OF ts_contdata .
  types TD_DOC_ID type DOKU_ID .
  types:
    tt_rawtext TYPE STANDARD TABLE OF tline WITH DEFAULT KEY .
  types:
    BEGIN OF ts_head,   " from DOKHL
        application TYPE tdobject,
        id          TYPE doku_id,
        object      TYPE doku_obj,
        langu       TYPE doku_langu,
        masterlang  TYPE dokil-masterlang,
        typ	        TYPE doku_typ,
        dokform	    TYPE tdformnew,
        dokstyle    TYPE tdstyle,
        selfdef     TYPE dokil-selfdef,
      END OF ts_head .
  types:
    BEGIN OF ts_lang.   " from DOKHL
  INCLUDE TYPE thead AS head.
  TYPES:
      state       TYPE dokhl-dokstate,
      _           TYPE string,      " Long Text
    END OF ts_lang .
  types:
    BEGIN OF ts_text,
      langu      TYPE spras,
      short_txt	 TYPE string,
      long_txt   TYPE ts_lang,
    END OF ts_text .
  types:
    tt_texts TYPE SORTED TABLE OF ts_text WITH UNIQUE KEY langu .
  types:
    BEGIN OF ts_ltext,
      lang       TYPE spras,
      text       TYPE string,
      _          TYPE string,     " Long text
    END OF ts_ltext .
  types:
    tt_ltexts TYPE SORTED TABLE OF ts_ltext WITH UNIQUE KEY lang .
  types:
    BEGIN OF ts_doc.
  INCLUDE TYPE ts_head AS hdr.
  TYPES:
      texts TYPE SORTED TABLE OF ts_lang WITH UNIQUE KEY tdspras,
    END OF ts_doc .
  types:
    tt_docs TYPE STANDARD TABLE OF ts_doc WITH DEFAULT KEY .
  types:
    tr_docid TYPE RANGE OF td_doc_id .
  types:
    BEGIN OF ts_component,
          type TYPE td_comptype,
          name TYPE td_compname,
        END OF ts_component .
  types:
    BEGIN OF ts_field.
*  INCLUDE TYPE d021s AS hdr.
  INCLUDE TYPE rpy_dyfatc AS hdr.     " dyfatc_tab
  TYPES:
      texts TYPE tt_texts,
    END OF ts_field .
  types:
    tt_fields TYPE SORTED TABLE OF ts_field WITH NON-UNIQUE KEY name .
  types:
    BEGIN OF ts_dynp_cont.
*  INCLUDE TYPE d021s AS hdr.
  INCLUDE TYPE rpy_dycatt AS hdr.       " dycatt_tab
  TYPES:
      fields  TYPE tt_fields,
    END OF ts_dynp_cont .
  types:
    tt_dynp_containers TYPE SORTED TABLE OF ts_dynp_cont WITH NON-UNIQUE KEY name .
  types:
    tt_flow_logic TYPE STANDARD TABLE OF rpy_dyflow WITH DEFAULT KEY .
  types:
    tt_matchcodes TYPE STANDARD TABLE OF rpy_dypara WITH DEFAULT KEY .
  types:
    tt_dyntexts TYPE SORTED TABLE OF d020t WITH UNIQUE KEY lang .
  types:
    BEGIN OF ts_dynpro.
*  INCLUDE TYPE d020s AS hdr.
  INCLUDE TYPE rpy_dyhead AS hdr.
  TYPES:
      texts      TYPE tt_dyntexts,
      flow_logic TYPE ts_sourceasstruc,
      matchcodes TYPE tt_matchcodes,
      containers TYPE tt_dynp_containers,
*      fields TYPE tt_fields,
    END OF ts_dynpro .
  types:
    tt_dynpros TYPE SORTED TABLE OF ts_dynpro WITH UNIQUE KEY screen .
  types:
    BEGIN OF ts_comptype,
      type  TYPE td_comptype,
      kind  TYPE td_transport_kind,
    END OF ts_comptype .

  constants:
    BEGIN OF sub_component,
      without_any   TYPE td_with_subcomp VALUE '0',         "#EC NOTEXT
      with_mine     TYPE td_with_subcomp VALUE '3',         "#EC NOTEXT
      with_required TYPE td_with_subcomp VALUE '6',         "#EC NOTEXT
      with_all      TYPE td_with_subcomp VALUE '9',         "#EC NOTEXT
    END OF sub_component .
  constants:
    BEGIN OF exists,
      exists     TYPE td_exists VALUE abap_true,
      not_exists TYPE td_exists VALUE abap_false,
    END OF exists .
  constants:
    BEGIN OF textpool_kinds,
      prog_title  TYPE td_txtp_kind VALUE 'R', "#EC NOTEXT R - Program title
      list_title  TYPE td_txtp_kind VALUE 'T', "#EC NOTEXT T - List Title: Titlebar
      list_header TYPE td_txtp_kind VALUE 'H', "#EC NOTEXT H 001 to 004 List header: Column headings
      text_symbol TYPE td_txtp_kind VALUE 'I', "#EC NOTEXT I Text symbol identifier Text symbol text
      parameters  TYPE td_txtp_kind VALUE 'S', "#EC NOTEXT S Name of a parameter or selection criterion Selection text
    END OF textpool_kinds .
  constants:
    BEGIN OF actions,
      delete_file  TYPE td_action VALUE 'D',                "#EC NOTEXT
      export       TYPE td_action VALUE 'E',                "#EC NOTEXT
      import       TYPE td_action VALUE 'I',                "#EC NOTEXT
      uninstall    TYPE td_action VALUE 'U',                "#EC NOTEXT
      unreplicable TYPE td_action VALUE 'K',                "#EC NOTEXT
      none         TYPE td_action VALUE space,
      activated    TYPE td_action VALUE 'A',                "#EC NOTEXT
      not_active   TYPE td_action VALUE 'N',                "#EC NOTEXT
    END OF actions .
  constants LINE_SEPARATOR type ABAP_CHAR1 value CL_ABAP_CHAR_UTILITIES=>NEWLINE ##NO_TEXT.
  constants BALLOG_OBJECT type BALOBJ_D value 'ZAPLINK' ##NO_TEXT.
  constants COMP_NODENAME type STRING value 'ZL_OBJECT' ##NO_TEXT.
  constants EXT_SEP type C value '.' ##NO_TEXT.
  constants FILE_EXT type STRING value 'xml' ##NO_TEXT.
  constants FRAMEWORK_VERSION type TD_FRAMEWORKVER value '0.0.009a' ##NO_TEXT.
  constants DEFAULT_SUB_COMPONENT_LEVEL type TD_WITH_SUBCOMP value SUB_COMPONENT-WITH_MINE ##NO_TEXT.
endinterface.
