*&---------------------------------------------------------------------*
*& Report zabap_object_contrast
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_object_contrast  NO STANDARD PAGE HEADING.
**********************************************************************SATRT
*功能：
*批量对比两个系统间程序、函数、class、表、视图、数据元素、域、
*搜索帮助、锁等等开发对象的差别。两个系统可以是在同一个传输域，
*也可以是能用rfc连接在一起的两个完全不相关的s4/ecc系统。
**********************************************************************E N D
TABLES: sci_dynp,versobj_alias.

DATA: gt_fldct TYPE lvc_t_fcat,
      gs_slayt TYPE lvc_s_layo.
DATA: gt_syst TYPE tmscsyslst_typ,
      gs_syst TYPE tmscsyslst,
      gt_vrmv TYPE vrm_values,
      gs_vrmv TYPE vrm_value.
DATA: gt_ko100 TYPE TABLE OF ko100,
      gs_ko100 TYPE ko100.
DATA: BEGIN OF gs_out,
        pgmid    TYPE e071-pgmid,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
        devclass TYPE tadir-devclass,
        author   TYPE tadir-author,
        objtxt   TYPE char40,
        result   TYPE char20,
        tract    TYPE trkorr,
        trloc    TYPE trkorr,
        trpre    TYPE trkorr,
        trrem    TYPE trkorr,
        trsta    TYPE char10,
      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.
DATA: gr_ext TYPE RANGE OF sci_dynp-i_obj_t. "排除类型


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE btxt1.
  SELECT-OPTIONS: s_prog FOR sci_dynp-o_repo  MEMORY ID prog,
                  s_fugr FOR sci_dynp-o_fugr  MEMORY ID fugr,
                  s_clas FOR sci_dynp-o_clas  MEMORY ID clas,
                  s_ddic FOR sci_dynp-o_ddic  MEMORY ID ddic.
  SELECT-OPTIONS: s_pckg FOR sci_dynp-f_devc  MEMORY ID pckg,
                  s_tran FOR sci_dynp-o_order MEMORY ID tran.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE btxt2.
  SELECT-OPTIONS: s_user FOR sci_dynp-f_resp,
                  s_otyp FOR sci_dynp-i_obj_t.
SELECTION-SCREEN END OF BLOCK b02.
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE btxt3.
  PARAMETERS: p_dest TYPE versdest MEMORY ID rfc,
              p_syst TYPE verssysnam AS LISTBOX VISIBLE LENGTH 20 MEMORY ID verssysnam.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE btxt4.
  PARAMETERS: p_same AS CHECKBOX DEFAULT 'X',
              p_diff AS CHECKBOX DEFAULT 'X',
              p_noex AS CHECKBOX DEFAULT 'X',
              p_unsu AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b04.

INITIALIZATION.
  APPEND 'EEQAVAS' TO gr_ext.
  APPEND 'EEQTOBJ' TO gr_ext.
  APPEND 'EEQSTOB' TO gr_ext.

  CALL FUNCTION 'TRINT_OBJECT_TABLE'
    EXPORTING
      iv_complete  = 'X'
    TABLES
      tt_types_out = gt_ko100.
  SORT gt_ko100 BY object.

  CALL FUNCTION 'TMS_CI_GET_SYSTEMLIST'
    EXPORTING
      iv_only_active = 'X'
    TABLES
      tt_syslst      = gt_syst.
  LOOP AT gt_syst INTO gs_syst.
    gs_vrmv-key  = gs_syst-sysnam.
    gs_vrmv-text = gs_syst-systxt.
    APPEND gs_vrmv TO gt_vrmv.
  ENDLOOP.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_SYST'
      values = gt_vrmv.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prog-low.
  CALL FUNCTION 'RS_HELP_HANDLING'
    EXPORTING
      dynpfield = 'S_PROG-LOW'
      dynpname  = sy-dynnr
      object    = 'PR'
      progname  = sy-repid.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_fugr-low.
  CALL FUNCTION 'RS_HELP_HANDLING'
    EXPORTING
      dynpfield = 'S_FUGR-LOW'
      dynpname  = sy-dynnr
      object    = 'FG'
      progname  = sy-repid.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_clas-low.
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type          = 'CLIF'
    IMPORTING
      object_name_selected = s_clas-low
    EXCEPTIONS
      OTHERS               = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ddic-low.
  CALL FUNCTION 'F4_DD_ALLTYPES'
    IMPORTING
      result = s_ddic-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tran-low.
  CALL FUNCTION 'TR_F4_REQUESTS'
    EXPORTING
      iv_trfunctions      = 'K'
      iv_trstatus         = 'RDL'
    IMPORTING
      ev_selected_request = s_tran-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_otyp-low.
  CALL FUNCTION 'TRINT_PGMID_OBJECT_SELECT_F4'
    IMPORTING
      we_object = s_otyp-low.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'ONLI'.
    IF p_dest = '' AND p_syst = ''.
      MESSAGE e000(oo) WITH 'RFC目标和目标系统至少输入一个'.
    ELSEIF p_dest = ''.
      READ TABLE gt_syst INTO gs_syst WITH KEY sysnam = p_syst.
      CONCATENATE 'TMSADM@' gs_syst-sysnam '.' gs_syst-domnam INTO p_dest.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据来源'.
  btxt2 = '数据限制'.
  btxt3 = '远程系统'.
  btxt4 = '结果显示包括'.
  %_p_syst_%_app_%-text   = '目标系统'.
  %_p_dest_%_app_%-text   = 'RFC目标(优先)'.
  %_s_clas_%_app_%-text  = '类/接口'.
  %_s_ddic_%_app_%-text  = '表/视图/数据类型/域/搜索帮助/锁'.
  %_s_fugr_%_app_%-text  = '函数组'.
  %_s_prog_%_app_%-text  = '程序(包含Z开头INCLUDE)'.
  %_s_tran_%_app_%-text  = '请求/任务号'.
  %_s_pckg_%_app_%-text  = '包/开发类'.
  %_s_user_%_app_%-text  = '创建者'.
  %_s_otyp_%_app_%-text  = '对象类型'.
  %_p_same_%_app_%-text  = '本地和远程相同'.
  %_p_diff_%_app_%-text  = '本地和远程不同'.
  %_p_noex_%_app_%-text  = '对象在远程不存在'.
  %_p_unsu_%_app_%-text  = '不支持对比的对象'.

START-OF-SELECTION.
  PERFORM get_objects.
  PERFORM expand_objects.
  PERFORM check_objects.
  PERFORM outdata.

*&---------------------------------------------------------------------*
*& get_objects
*&---------------------------------------------------------------------*
FORM get_objects.
  DATA ls_tadir TYPE tadir.
  DATA lt_progs LIKE gt_out.
  DATA ls_progs LIKE gs_out.
  DATA lt_incl  TYPE TABLE OF sobj_name.
  DATA lt_e070  TYPE TABLE OF e070.
  DATA lt_e071  TYPE TABLE OF e071.
  DATA ls_e071  TYPE e071.

  IF s_prog[] IS NOT INITIAL.
    SELECT pgmid object obj_name devclass author
      INTO CORRESPONDING FIELDS OF TABLE lt_progs
      FROM tadir INNER JOIN trdir ON obj_name = trdir~name
      WHERE pgmid    EQ 'R3TR' AND
            object   EQ 'PROG' AND
            obj_name IN s_prog AND
            author   IN s_user AND
            devclass NE '$TMP'.
    LOOP AT lt_progs INTO ls_progs.
      CLEAR lt_incl.
      CALL FUNCTION 'GET_INCLUDETAB'
        EXPORTING
          progname = ls_progs-obj_name
        TABLES
          incltab  = lt_incl.
      CHECK lt_incl[] IS NOT INITIAL.

      SELECT pgmid object obj_name devclass author
        INTO CORRESPONDING FIELDS OF ls_progs
        FROM tadir INNER JOIN trdir ON obj_name = trdir~name
        FOR ALL ENTRIES IN lt_incl
        WHERE pgmid    EQ 'R3TR' AND
              object   EQ 'PROG' AND
              obj_name EQ lt_incl-table_line AND
              obj_name LIKE 'Z%' AND
              devclass NE '$TMP'.
        COLLECT ls_progs INTO lt_progs.
      ENDSELECT.
    ENDLOOP.

    APPEND LINES OF lt_progs TO gt_out.
  ENDIF.

  IF s_fugr[] IS NOT INITIAL.
    SELECT pgmid object obj_name devclass author
      APPENDING CORRESPONDING FIELDS OF TABLE gt_out
      FROM tadir
      WHERE pgmid    EQ 'R3TR' AND
            object   IN ('FUGR','FUGS','FUGX') AND
            obj_name IN s_fugr AND
            author   IN s_user AND
            devclass NE '$TMP'.
  ENDIF.

  IF s_clas[] IS NOT INITIAL.
    SELECT pgmid object obj_name devclass author
      APPENDING CORRESPONDING FIELDS OF TABLE gt_out
      FROM tadir
      WHERE pgmid    EQ 'R3TR' AND
            object   IN ('CLAS','INTF') AND
            obj_name IN s_clas AND
            author   IN s_user AND
            devclass NE '$TMP'.
  ENDIF.

  IF s_ddic[] IS NOT INITIAL.
    SELECT pgmid object obj_name devclass author
      APPENDING CORRESPONDING FIELDS OF TABLE gt_out
      FROM tadir
      WHERE pgmid    EQ 'R3TR' AND
            object   IN ('DTEL','TABL','VIEW','SQLT','DOMA',
                         'SHLP','ENQU','DDLS','TTYP') AND
            obj_name IN s_ddic AND
            author   IN s_user AND
            devclass NE '$TMP'.
  ENDIF.

  IF s_tran[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_e070 FROM e070
      WHERE strkorr IN s_tran OR trkorr IN s_tran.
    IF sy-subrc = 0.
      SELECT pgmid object obj_name
        INTO CORRESPONDING FIELDS OF TABLE lt_e071
        FROM e071
        FOR ALL ENTRIES IN lt_e070
        WHERE trkorr EQ lt_e070-trkorr  AND
              pgmid  IN ('LIMU','R3TR') AND
              object IN gr_ext.
      LOOP AT lt_e071 INTO ls_e071.
        CALL FUNCTION 'TR_CHECK_TYPE'
          EXPORTING
            wi_e071  = ls_e071
          IMPORTING
            we_tadir = ls_tadir.

        SELECT SINGLE * INTO ls_tadir FROM tadir
          WHERE pgmid    EQ ls_tadir-pgmid   AND
                object   EQ ls_tadir-object  AND
                obj_name EQ ls_tadir-obj_name AND
                author   IN s_user AND
                object   IN gr_ext AND
                devclass NE '$TMP'.
        CHECK sy-subrc = 0.

        gs_out-pgmid     = ls_e071-pgmid.
        gs_out-object    = ls_e071-object.
        gs_out-obj_name  = ls_e071-obj_name.
        gs_out-devclass  = ls_tadir-devclass.
        gs_out-author    = ls_tadir-author.
        APPEND gs_out TO gt_out.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF s_pckg[] IS NOT INITIAL.
    SELECT pgmid object obj_name devclass author
      APPENDING CORRESPONDING FIELDS OF TABLE gt_out
      FROM tadir
      WHERE devclass IN s_pckg AND
            author   IN s_user AND
            object   IN gr_ext AND
            devclass NE '$TMP'.
  ENDIF.

  IF gt_out IS INITIAL.
    MESSAGE s000(oo) WITH '无数据'.
  ENDIF.
ENDFORM.                    "get_objects

*&---------------------------------------------------------------------*
*& expand_objects
*&---------------------------------------------------------------------*
FORM expand_objects.
  DATA: ls_e071    TYPE e071,
        ls_object  LIKE gs_out,
        lt_object  LIKE gt_out,
        ls_ver_obj TYPE svrs2_versionable_object,
        ls_vrso    TYPE vrso,
        lt_vrso    TYPE STANDARD TABLE OF vrso.

  LOOP AT gt_out INTO gs_out.
    ls_object-author   = gs_out-author.
    ls_object-devclass = gs_out-devclass.

    CLEAR ls_e071.
    ls_e071-pgmid    = gs_out-pgmid.
    ls_e071-object   = gs_out-object.
    ls_e071-obj_name = gs_out-obj_name.
    CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
      EXPORTING
        e071_obj        = ls_e071
      TABLES
        obj_tab         = lt_vrso
      EXCEPTIONS
        not_versionable = 1.
    IF sy-subrc = 0.
      LOOP AT lt_vrso INTO ls_vrso WHERE objtype NE 'DOCU'.
        CLEAR ls_ver_obj.
        ls_ver_obj-objtype = ls_vrso-objtype.
        ls_ver_obj-objname = ls_vrso-objname.
        ls_ver_obj-versno  = '00000'.

        CALL FUNCTION 'SVRS_GET_VERSION_LOCAL'
          CHANGING
            object             = ls_ver_obj
          EXCEPTIONS
            no_version         = 3
            version_unreadable = 4
            OTHERS             = 5.
        CHECK sy-subrc = 0.
        CHECK ls_ver_obj-objtype IN s_otyp.

        READ TABLE gt_ko100 INTO gs_ko100
                            WITH KEY object = ls_vrso-objtype BINARY SEARCH.
        ls_object-objtxt   = gs_ko100-text.
        ls_object-pgmid    = 'LIMU'.
        ls_object-object   = ls_vrso-objtype.
        ls_object-obj_name = ls_vrso-objname.
        APPEND ls_object TO lt_object.
      ENDLOOP.
    ELSE.
      CHECK gs_out-object IN s_otyp.
      READ TABLE gt_ko100 INTO gs_ko100
                          WITH KEY object = gs_out-object BINARY SEARCH.
      ls_object-objtxt   = gs_ko100-text.
      ls_object-object   = gs_out-object.
      ls_object-obj_name = gs_out-obj_name.
      ls_object-pgmid    = gs_out-pgmid.
      ls_object-result    = '@BZ@类型不支持'.
      APPEND ls_object TO lt_object.
      CLEAR ls_object.
    ENDIF.
  ENDLOOP.

  gt_out = lt_object.
  SORT gt_out BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_out COMPARING object obj_name.
ENDFORM.                    "expand_objects

*&---------------------------------------------------------------------*
*& check_objects
*&---------------------------------------------------------------------*
FORM check_objects.
  DATA: lv_text    TYPE char50,
        lv_lines   TYPE i,
        lv_empty   TYPE c,
        ls_loc_obj TYPE svrs2_versionable_object,
        ls_rem_obj TYPE svrs2_versionable_object,
        ls_delta   TYPE svrs2_xversionable_object.

  lv_lines = lines( gt_out ).
  LOOP AT gt_out INTO gs_out WHERE result = ''.
    MESSAGE s000(oo) WITH sy-tabix '/' lv_lines gs_out-obj_name INTO lv_text.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = lv_text.

    PERFORM read_object USING gs_out '' CHANGING ls_loc_obj.
    PERFORM read_object USING gs_out p_dest CHANGING ls_rem_obj.

    IF ls_rem_obj IS INITIAL.
      gs_out-result = '@5C@不存在'.
    ELSE.
      PERFORM clear_data USING ls_loc_obj.
      PERFORM clear_data USING ls_rem_obj.

      CLEAR ls_delta.
      CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
        EXPORTING
          obj_old = ls_loc_obj
          obj_new = ls_rem_obj
        CHANGING
          delta   = ls_delta
        EXCEPTIONS
          OTHERS  = 2.
      CALL FUNCTION 'SVRS_CHECK_DELTA_EMPTY'
        EXPORTING
          obj_new       = ls_loc_obj
          delta         = ls_delta
        IMPORTING
          p_delta_empty = lv_empty.
      IF lv_empty = 'X'.
        gs_out-result = '@5B@相同'.
      ELSE.
        gs_out-result = '@5D@不同'.
      ENDIF.
    ENDIF.

    PERFORM get_trkorr USING p_dest gs_out-object gs_out-obj_name
                       CHANGING gs_out-tract gs_out-trloc gs_out-trpre gs_out-trrem.
    IF gs_out-trrem NE ''.
      IF gs_out-trrem = gs_out-trloc.
        gs_out-trsta = '最新请求'.
      ELSEIF gs_out-trrem = gs_out-trpre.
        gs_out-trsta = '次新请求'.
      ELSE.
        gs_out-trsta = '@5D@非最新/次新'.
      ENDIF.
    ENDIF.

    MODIFY gt_out FROM gs_out.
  ENDLOOP.

  IF p_same = ''.
    DELETE gt_out WHERE result = '@5B@相同'.
  ENDIF.
  IF p_diff = ''.
    DELETE gt_out WHERE result = '@5D@不同'.
  ENDIF.
  IF p_noex = ''.
    DELETE gt_out WHERE result = '@5C@不存在'.
  ENDIF.
  IF p_unsu = ''.
    DELETE gt_out WHERE result = '@BZ@类型不支持'.
  ENDIF.
ENDFORM.                    "check_objects
*&---------------------------------------------------------------------*
*& read_object
*&---------------------------------------------------------------------*
FORM read_object USING ps_object LIKE gs_out pv_dest
                 CHANGING cs_verobj TYPE svrs2_versionable_object.

  CLEAR: cs_verobj.
  cs_verobj-destination = pv_dest.
  cs_verobj-objtype     = ps_object-object.
  cs_verobj-objname     = ps_object-obj_name.
  cs_verobj-versno      = '00000'.

  CALL FUNCTION 'SVRS_GET_VERSION'
    CHANGING
      object              = cs_verobj
    EXCEPTIONS
      communication_error = 1
      system_error        = 2
      no_version          = 3
      version_unreadable  = 4
      OTHERS              = 5.
  IF sy-subrc NE 0.
    CLEAR: cs_verobj.
  ENDIF.
ENDFORM.                    "read_object

*&---------------------------------------------------------------------*
*& clear_data
*&---------------------------------------------------------------------*
FORM clear_data USING ps_verobj TYPE svrs2_versionable_object.
  DATA: lv_type  TYPE c,
        lv_count TYPE i.
  FIELD-SYMBOLS: <ls_part>  TYPE any,
                 <ls_row>   TYPE any,
                 <lt_table> TYPE ANY TABLE.

  IF ps_verobj-data_pointer IS INITIAL.
    ps_verobj-data_pointer = ps_verobj-objtype.
  ENDIF.

  ASSIGN COMPONENT ps_verobj-data_pointer OF STRUCTURE ps_verobj TO <ls_part>.
  CHECK sy-subrc = 0.

  PERFORM clear_stru_field USING: <ls_part> 'MDLOG' ,
                                  <ls_part> 'TRDIR' ,
                                  <ls_part> 'TFDIR' ,
                                  <ls_part> 'TRDIRT',
                                  <ls_part> 'CLASS' ,
                                  <ls_part> 'DD08TV'.
  DESCRIBE FIELD <ls_part> TYPE lv_type COMPONENTS lv_count.
  DO lv_count TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE <ls_part> TO <lt_table>.
    CHECK sy-subrc = 0.
    LOOP AT <lt_table> ASSIGNING <ls_row>.
      PERFORM clear_stru_field USING: <ls_row> 'AUTHOR'   ,
                                      <ls_row> 'AUTH'     ,
                                      <ls_row> 'CREATEDON',
                                      <ls_row> 'CHANGEDBY',
                                      <ls_row> 'CHANGEDON',
                                      <ls_row> 'UDATE'    ,
                                      <ls_row> 'UTIME'    ,
                                      <ls_row> 'AS4DATE'  ,
                                      <ls_row> 'AS4TIME'  ,
                                      <ls_row> 'AS4USER'  ,
                                      <ls_row> 'DESCRIPT' ,
                                      <ls_row> 'EDITORDER',
                                      <ls_row> 'R3RELEASE'.
    ENDLOOP.
  ENDDO.
ENDFORM.                    "clear_data

*&---------------------------------------------------------------------*
*& clear_stru_field
*&---------------------------------------------------------------------*
FORM clear_stru_field USING ps_stru pv_fldnam.
  FIELD-SYMBOLS <lv_value> TYPE any.

  ASSIGN COMPONENT pv_fldnam OF STRUCTURE ps_stru TO <lv_value>.
  IF sy-subrc = 0.
    CLEAR <lv_value>.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& outdata
*&---------------------------------------------------------------------*
FORM outdata.
  gs_slayt-zebra = 'X'.

  PERFORM catset1 TABLES gt_fldct
                  USING: 'OBJ_NAME ' '' '' '对象名称',
                         'OBJECT   ' '' '' '对象类型',
                         'OBJTXT   ' '' '' '类型描述',
                         'AUTHOR   ' '' '' '创建人',
                         'DEVCLASS ' '' '' '包',
                         'TRACT    ' '' '' '本地活动TASK',
                         'TRLOC    ' '' '' '本地最新请求号',
                         'TRPRE    ' '' '' '本地次新请求号',
                         'TRREM    ' '' '' '远程请求号',
                         'TRSTA    ' '' '' '远程请求状态',
                         'RESULT   ' '' '' '对比结果'.

  CHECK gt_out IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc         = gt_fldct
      is_layout_lvc           = gs_slayt
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'
    TABLES
      t_outtab                = gt_out.
ENDFORM.                    " outdata

*---------------------------------------------------------------------*
* catset1
*---------------------------------------------------------------------*
FORM catset1 TABLES t_fldcat USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.
  ls_fldcat-scrtext_l =  pv_text.
  ls_fldcat-coltext   =  pv_text.
  ls_fldcat-ref_table =  pv_reftab.
  ls_fldcat-ref_field =  pv_reffld.
  ls_fldcat-col_opt   = 'A'.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.                    "catset1

*&--------------------------------------------------------------------*
*& user_command
*&--------------------------------------------------------------------*
FORM user_command USING pv_ucomm TYPE sy-ucomm        ##called
                        pv_field TYPE slis_selfield.
  DATA: lv_repid TYPE sy-repid.

  READ TABLE gt_out INTO gs_out INDEX pv_field-tabindex.
  CASE pv_ucomm.
    WHEN '&IC1'.
      CASE pv_field-fieldname.
        WHEN 'OBJ_NAME' OR 'OBJECT' OR 'OBJTXT'. "对象跳转
          CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
            EXPORTING
              iv_pgmid          = gs_out-pgmid
              iv_object         = gs_out-object
              iv_obj_name       = gs_out-obj_name
              iv_action         = 'SHOW'
            EXCEPTIONS
              jump_not_possible = 1
              OTHERS            = 2.
        WHEN 'AUTHOR'.
          CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
            EXPORTING
              bname = gs_out-author.
        WHEN 'TRACT'.
          CALL FUNCTION 'SVRS_DISPLAY_DIRECTORY'
            EXPORTING
              object_name = gs_out-obj_name(110)
              object_type = gs_out-object.
        WHEN 'TRLOC'.
          PERFORM display_request USING gs_out-trloc.
        WHEN 'TRPRE'.
          PERFORM display_request USING gs_out-trpre.
        WHEN 'TRREM'.
          PERFORM display_request USING gs_out-trrem.
        WHEN 'TRSTA'.
          CALL FUNCTION 'SVRS_DISPLAY_REMOTE_DIR'
            EXPORTING
              destination  = p_dest
              object_name  = gs_out-obj_name(110)
              object_type  = gs_out-object
            EXCEPTIONS
              no_directory = 01
              no_selection = 02.
        WHEN 'RESULT'.  "对比版本
          SELECT SINGLE * FROM versobj_alias
            WHERE objtype_appl = gs_out-object.
          IF sy-subrc = 0.
            lv_repid = 'RSVRS_TLOGO_COMPARE_VERSIONS'.
          ELSE.
            SELECT SINGLE rep_comp INTO lv_repid
              FROM versobj
              WHERE object  = gs_out-object AND
                    versno  = '99999'       AND
                    actflag = 'A'.
            IF sy-subrc NE 0.
              MESSAGE s000(oo) WITH '未找到此对象类型的对比程序'.
              RETURN.
            ENDIF.
          ENDIF.
          SUBMIT (lv_repid)
                 WITH objname = gs_out-obj_name
                 WITH objnam2 = gs_out-obj_name
                 WITH objtyp1 = gs_out-object
                 WITH objtyp2 = gs_out-object
                 WITH versno1 = '00000'
                 WITH versno2 = '00000'
                 WITH log_dest = p_dest
                 WITH rem_syst = p_syst
                 AND RETURN.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "user_command


*&--------------------------------------------------------------------*
*& 获取请求号
*&--------------------------------------------------------------------*
FORM get_trkorr USING pv_dest pv_objtype pv_objname
                CHANGING cv_tract cv_trloc cv_trpre cv_trrem.
  DATA: lt_vrsn TYPE vrsn_tab.
  DATA: lt_vrsd TYPE STANDARD TABLE OF vrsd,
        ls_vrsd TYPE vrsd.

  CLEAR: cv_trloc,cv_trpre,cv_trrem.
  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
    EXPORTING
      destination  = ''
      objname      = pv_objname(110)
      objtype      = pv_objtype
    TABLES
      lversno_list = lt_vrsn
      version_list = lt_vrsd
    EXCEPTIONS
      no_entry     = 1.

  READ TABLE lt_vrsd INTO ls_vrsd WITH KEY versno = '00000'.
  IF sy-subrc = 0.
    cv_tract = ls_vrsd-korrnum.
  ENDIF.

  LOOP AT lt_vrsd INTO ls_vrsd WHERE korrnum NE ''.
    IF cv_trloc IS INITIAL.
      cv_trloc = ls_vrsd-korrnum.
    ELSEIF ls_vrsd-korrnum NE cv_trloc.
      cv_trpre = ls_vrsd-korrnum.
      EXIT.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
    EXPORTING
      destination  = pv_dest
      objname      = pv_objname(110)
      objtype      = pv_objtype
    TABLES
      lversno_list = lt_vrsn
      version_list = lt_vrsd
    EXCEPTIONS
      no_entry     = 1.
  LOOP AT lt_vrsd INTO ls_vrsd WHERE korrnum NE ''.
    cv_trrem = ls_vrsd-korrnum.
    EXIT.
  ENDLOOP.
ENDFORM.                    "get_trkorr

*&---------------------------------------------------------------------*
*& 显示请求
*&---------------------------------------------------------------------*
FORM display_request USING pv_request.
  CHECK pv_request IS NOT INITIAL.
  CALL FUNCTION 'TR_PRESENT_REQUEST'
    EXPORTING
      iv_trkorr = pv_request.
ENDFORM.