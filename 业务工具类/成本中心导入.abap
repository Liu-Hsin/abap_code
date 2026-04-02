*&*********************************************************************
*& program name:              [ZCOE009]
*& module name :              []
*& apply author:              []
*& author:                    [NULL]
*& started on:                [11.05.2024 15:49:38]
*& transaction:               []
*& program type:              [Report]
*& transfer requests:         []
*& program description :      []
*&*&*******************************************************************
*& revision log                                                       *
*&                                                                    *
*& log          date                 author       description         *
*& ------      ------------         ---------     -----------         *
*& v1.0       11.05.2024 15:49:38             初稿                *
*&*********************************************************************
REPORT  zcoe009.

*--------------------------------------------------------------------*
*type-pools
TYPE-POOLS:abap,icon.
*--------------------------------------------------------------------*
*tables
TABLES:sscrfields.
*--------------------------------------------------------------------*
*constants
CONSTANTS: BEGIN OF gc,
             success TYPE bapi_mtype VALUE 'S',
             fail    TYPE bapi_mtype VALUE 'E',
             green   TYPE icon-name VALUE icon_led_green,
             red     TYPE icon-name VALUE icon_led_red,
             objid   TYPE wwwdata-objid VALUE 'ZCOE009',
           END OF gc.
*--------------------------------------------------------------------*
*type
TYPES: BEGIN OF ty_data,
         costcenter       TYPE kostl,
         valid_from       TYPE datab,
         descript         TYPE kltxt,
         person_in_charge TYPE verak,
         costcenter_type  TYPE kosar,
         costctr_hier_grp TYPE khinr,
         comp_code        TYPE bukrs,
         profit_ctr       TYPE prctr,
         addr_name1       TYPE name1_gp,
         addr_name2       TYPE name2_gp,
         addr_street      TYPE stras_gp,
         func_area_long   TYPE fkber,
       END OF ty_data.
*--------------------------------------------------------------------*
*data
DATA: gs_tmp TYPE ty_data.
DATA: BEGIN OF gs_data .
        INCLUDE TYPE ty_data.
DATA:
        sel,
        valid_to                      TYPE datbi,
        name                          TYPE ktext,
        currency                      TYPE waers,
        record_quantity               TYPE  mgefl,
        lock_ind_actual_primary_costs TYPE  bkzkp,
        lock_ind_plan_primary_costs   TYPE  pkzkp,
        lock_ind_act_secondary_costs  TYPE  bkzks,
        lock_ind_plan_secondary_costs TYPE  pkzks,
        lock_ind_actual_revenues      TYPE  bkzer,
        lock_ind_plan_revenues        TYPE  pkzer,
        lock_ind_commitment_update    TYPE  bkzob,
        co_area                       TYPE kokrs,
        ktext                         TYPE ktext,
        msgty                         TYPE msgty,
        msgtx                         TYPE msgtx,
      END OF gs_data.
DATA: gt_data LIKE TABLE OF gs_data.
DATA: gv_tempname TYPE string VALUE '成本中心导入模板'.
*--------------------------------------------------------------------*
*class

*--------------------------------------------------------------------*
*screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  PARAMETERS: p_file TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
*--------------------------------------------------------------------*
*screen event
INITIALIZATION.
  sscrfields-functxt_01 = '下载模板'.
  sscrfields-functxt_02 = '部门信息查询'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_file_open_dialog CHANGING p_file.

AT SELECTION-SCREEN.
  PERFORM:frm_screen_action.

START-OF-SELECTION.
  PERFORM:frm_main.
  PERFORM : frm_alv_display.
  
END-OF-SELECTION.
  
*&---------------------------------------------------------------------*
*& Form frm_screen_action
*&---------------------------------------------------------------------*
FORM frm_screen_action .
  CHECK sy-ucomm EQ 'FC01' OR sy-ucomm = 'FC02'.
  CASE sy-ucomm.
    WHEN 'FC01'.  PERFORM frm_file_save_dialog .
    WHEN 'FC02'.  PERFORM frm_skip_sm30 .
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      form  frm_download_tmp
*&---------------------------------------------------------------------*
FORM frm_file_save_dialog .
  DATA: lv_path	       TYPE string,
        lv_fullpath	   TYPE string,
        lv_user_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
*     window_title              =
      default_extension         = 'xlsx'
      default_file_name         = gv_tempname "模版名称 string
*     with_encoding             =
      file_filter               = 'Excel文件*.xlsx;*.xls'
    CHANGING
      filename                  = gv_tempname
      path                      = lv_path
      fullpath                  = lv_fullpath
      user_action               = lv_user_action
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5.
  IF sy-subrc = 0 AND lv_user_action = 0.
    PERFORM: frm_file_download_tmp USING lv_fullpath.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      form  frm_download_tmp
*&---------------------------------------------------------------------*
FORM frm_file_download_tmp USING p_fullpath.
  DATA: lv_key         LIKE  wwwdatatab,
        lv_destination LIKE  rlgrap-filename,
        lv_rc          TYPE i.

  SELECT SINGLE relid, objid
  INTO @DATA(ls_wwwdata)
        FROM wwwdata
        WHERE srtf2 = 0
        AND relid = 'MI'
        AND objid = @gc-objid." 模板对象名
  IF sy-subrc NE 0 OR ls_wwwdata-objid EQ space.
    MESSAGE |{ TEXT-001 } { gc-objid } { TEXT-002 }| TYPE 'I'.
    " 模板文件不存在
  ELSE.
    lv_key = VALUE #( relid = ls_wwwdata-relid
    objid = ls_wwwdata-objid  ).
    lv_destination = p_fullpath.
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = lv_key
        destination = lv_destination
      IMPORTING
        rc          = lv_rc.
    IF lv_rc NE 0.
      MESSAGE |{ TEXT-001 } { gc-objid } { TEXT-003 }| TYPE 'I'.
      "模板文件下载失败
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      form  frm_file_open_dialog
*&---------------------------------------------------------------------*
FORM frm_file_open_dialog CHANGING p_file.
  DATA: lt_table       TYPE filetable,
        lv_rc	         TYPE i,
        lv_user_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
*     window_title            =
      default_extension       = 'xlsx'
      file_filter             = 'Excel文件*.xlsx;*.xls'
    CHANGING
      file_table              = lt_table
      rc                      = lv_rc
      user_action             = lv_user_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0 AND lv_user_action = 0.
    CLEAR: p_file.
    READ TABLE lt_table INTO p_file INDEX 1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_main
*&---------------------------------------------------------------------*
FORM frm_main .

  CHECK p_file IS NOT INITIAL.

  PERFORM: frm_upload_file.
  PERFORM: frm_check_data.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM frm_upload_file .
  DATA: lt_intern TYPE TABLE OF alsmex_tabline.
  CHECK p_file IS NOT INITIAL.
  DO 5 TIMES.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 20
        i_end_row               = 9999
      TABLES
        intern                  = lt_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ELSEIF lt_intern[] IS NOT INITIAL.
      PERFORM: frm_file_edit_excel_data TABLES lt_intern.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EDIT_EXCEL_DATA
*&---------------------------------------------------------------------*
FORM frm_file_edit_excel_data  TABLES p_intern STRUCTURE alsmex_tabline.
  FIELD-SYMBOLS: <fs_value>.
  LOOP AT p_intern ASSIGNING FIELD-SYMBOL(<wa_intern>).
    ASSIGN COMPONENT <wa_intern>-col OF STRUCTURE gs_tmp TO <fs_value>.
    IF <fs_value> IS ASSIGNED.
      CONDENSE <wa_intern>-value.
      TRANSLATE <wa_intern>-value TO UPPER CASE.
      TRY.
        <fs_value> = <wa_intern>-value.
      CATCH cx_root INTO DATA(lo_cx_root).
        MESSAGE |数据异常行{ <wa_intern>-row }=>{ lo_cx_root->get_text() }| TYPE 'I'." DISPLAY 'E'.
        STOP.
      AT END OF row.
        APPEND CORRESPONDING #( gs_tmp ) TO gt_data.
        CLEAR: gs_tmp.
      ENDAT.
      UNASSIGN <fs_value>.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_check_data
*&---------------------------------------------------------------------*
FORM frm_check_data .

  SELECT * FROM tka05 INTO TABLE @DATA(lt_tka05).

  SELECT kosar,ktext FROM tkt05 INTO TABLE @DATA(lt_tkt05).

  SELECT kokrs,kostl FROM csks INTO TABLE @DATA(lt_csks) WHERE kokrs = 'ZZZZ'.

  SELECT zdep,stufe,up FROM zfit020 INTO TABLE @DATA(lt_ehr).

  LOOP AT  gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

    PERFORM:frm_value_is_null USING <fs_data>-costcenter       '成本中心'     CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-valid_from       '有效开始日期' CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-descript         '名称'         CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-person_in_charge '负责人'       CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-costcenter_type  '成本中心类型' CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-costctr_hier_grp '层次结构范围' CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-comp_code        '公司代码'     CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-profit_ctr       '利润中心'     CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-addr_name1       '一级部门'     CHANGING <fs_data>-msgtx <fs_data>-msgty.
    PERFORM:frm_value_is_null USING <fs_data>-addr_street      '预算部门'     CHANGING <fs_data>-msgtx <fs_data>-msgty.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_data>-costcenter
      IMPORTING
        output = <fs_data>-costcenter.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_data>-profit_ctr
      IMPORTING
        output = <fs_data>-profit_ctr.

    READ TABLE lt_tkt05 INTO DATA(ls_tkt05) WITH KEY kosar = <fs_data>-costcenter_type.
    IF sy-subrc EQ 0.
      <fs_data>-ktext = ls_tkt05-ktext.
    ELSE.
      <fs_data>-msgtx = |成本中心类型{ <fs_data>-costcenter_type }不正确,{ <fs_data>-msgtx }|.
      <fs_data>-msgty = gc-fail.
    ENDIF.

    READ TABLE lt_ehr TRANSPORTING NO FIELDS WITH KEY zdep = <fs_data>-addr_name1 stufe = '1' .
    IF sy-subrc NE 0.
      <fs_data>-msgtx = |一级部门{ <fs_data>-addr_name1 }不存在,{ <fs_data>-msgtx }|.
      <fs_data>-msgty = gc-fail.
    ENDIF.

    IF <fs_data>-addr_name2 IS NOT INITIAL.
      READ TABLE lt_ehr INTO DATA(ls_ehr_low) WITH KEY zdep = <fs_data>-addr_name2 stufe = '2' .
      IF sy-subrc NE 0.
        <fs_data>-msgtx = |二级部门{ <fs_data>-addr_name2 }不存在,{ <fs_data>-msgtx }|.
        <fs_data>-msgty = gc-fail.
      ELSE.
        IF ls_ehr_low-up NE <fs_data>-addr_name1.
          <fs_data>-msgtx = |该二级部门{ <fs_data>-addr_name2 }不在一级部门{ <fs_data>-addr_name1 }下,{ <fs_data>-msgtx }|.
          <fs_data>-msgty = gc-fail.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_csks TRANSPORTING NO FIELDS WITH KEY kostl = <fs_data>-costcenter.
    IF sy-subrc EQ 0.
      <fs_data>-msgtx = |成本中心{ <fs_data>-costcenter }已存在,{ <fs_data>-msgtx }|.
      <fs_data>-msgty = gc-fail.
    ENDIF.

    <fs_data>-co_area  = 'ZZZZ'.
    <fs_data>-valid_to = '99991231'.
    <fs_data>-name     = <fs_data>-descript.

    READ TABLE lt_tka05 INTO DATA(ls_tka05) WITH KEY kosar = <fs_data>-costcenter_type.
    IF sy-subrc EQ 0.
      <fs_data>-func_area_long                = ls_tka05-func_area.
      <fs_data>-record_quantity               = ls_tka05-mgefl.
      <fs_data>-lock_ind_actual_primary_costs = ls_tka05-bkzkp.
      <fs_data>-lock_ind_plan_primary_costs   = ls_tka05-pkzkp.
      <fs_data>-lock_ind_act_secondary_costs  = ls_tka05-bkzks.
      <fs_data>-lock_ind_plan_secondary_costs = ls_tka05-pkzks.
      <fs_data>-lock_ind_actual_revenues      = ls_tka05-bkzer.
      <fs_data>-lock_ind_plan_revenues        = ls_tka05-pkzer.
      <fs_data>-lock_ind_commitment_update    = ls_tka05-bkzob..
    ENDIF.

*    <fs_data>-lock_ind_actual_revenues = <fs_data>-lock_ind_commitment_update = <fs_data>-lock_ind_plan_revenues = abap_true.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_value_is_null
*&---------------------------------------------------------------------*
FORM frm_value_is_null  USING    p_value
                                 VALUE(p_name)
                        CHANGING pc_msgtx
                                 pc_msgty.
  IF p_value IS INITIAL.
    pc_msgtx = |{ p_name }不可为空,{ pc_msgtx }|.
    pc_msgty = gc-fail.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM frm_alv_display .
  DATA: lv_repid        TYPE  sy-repid,
        is_layout_lvc   TYPE  lvc_s_layo,
        it_fieldcat_lvc TYPE  lvc_t_fcat.

  REFRESH: it_fieldcat_lvc.
  CLEAR: is_layout_lvc.

  PERFORM:frm_alv_layout_fieldcat TABLES it_fieldcat_lvc CHANGING  is_layout_lvc .

  lv_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = lv_repid
      is_layout_lvc            = is_layout_lvc
      it_fieldcat_lvc          = it_fieldcat_lvc
      i_callback_pf_status_set = 'FRM_ALV_PF_SET_STATUS'
      i_callback_user_command  = 'FRM_ALV_USER_COMMAND'
      i_default                = 'X'
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_data[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  FRM_LAYOUT_FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_alv_layout_fieldcat  TABLES   pt_fieldcat_lvc TYPE  lvc_t_fcat
CHANGING  is_layout_lvc TYPE  lvc_s_layo.
  is_layout_lvc-cwidth_opt = 'X'.
  is_layout_lvc-zebra = 'X'.
  is_layout_lvc-box_fname = 'SEL'.
  is_layout_lvc-sel_mode = 'D'.

  PERFORM: frm_alv_get_fieldcat TABLES pt_fieldcat_lvc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_GET_FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_alv_get_fieldcat  TABLES   pt_fieldcat_lvc TYPE lvc_t_fcat.
  PERFORM frm_alv_set_fieldcat TABLES pt_fieldcat_lvc USING:
    'MSGTX'             'L' '消息' '' '',
    'CO_AREA'           'C' '控制范围'      'CO_AREA'           'BAPI0012_GEN'  ,
    'COSTCENTER'        'C' '成本中心'      'COSTCENTER'        'BAPI0012_CCINPUTLIST'  ,
    'VALID_FROM'        'C' '有效开始日期'  'VALID_FROM'        'BAPI0012_CCINPUTLIST'  ,
    'VALID_TO'          'C' '有效截至日期'  'VALID_TO'          'BAPI0012_CCINPUTLIST'  ,
    'DESCRIPT'          'L' '名称'          'DESCRIPT'          'BAPI0012_CCINPUTLIST'  ,
    'PERSON_IN_CHARGE'  'L' '负责人'        'PERSON_IN_CHARGE'  'BAPI0012_CCINPUTLIST'  ,
    'COSTCENTER_TYPE'   'C' '成本中心类型'  'COSTCENTER_TYPE'   'BAPI0012_CCINPUTLIST'  ,
    'KTEXT'             'L' '类型描述'      'KTEXT' 'TKT05' ,
    'COSTCTR_HIER_GRP'  'C' '层次结构范围' 'COSTCTR_HIER_GRP'  'BAPI0012_CCINPUTLIST'  ,
    'COMP_CODE'         'C' '公司代码'      'COMP_CODE'   'BAPI0012_CCINPUTLIST'  ,
    'PROFIT_CTR'        'C' '利润中心'      'PROFIT_CTR'  'BAPI0012_CCINPUTLIST'  ,
    'ADDR_NAME1'        'C' '一级部门'      'ADDR_NAME1'  'BAPI0012_CCINPUTLIST'  ,
    'ADDR_NAME2'        'C' ' 二级部门'     'ADDR_NAME2'  'BAPI0012_CCINPUTLIST'  ,
    'ADDR_STREET'       'C' '预算部门'      'ADDR_STREET' 'BAPI0012_CCINPUTLIST'  .
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_alv_set_fieldcat
*&---------------------------------------------------------------------*
FORM frm_alv_set_fieldcat  TABLES pt_fieldcat_lvc TYPE  lvc_t_fcat
USING  p_fieldname     TYPE lvc_s_fcat-fieldname
      p_just          TYPE lvc_s_fcat-just
      p_coltext       TYPE lvc_s_fcat-coltext
      p_ref_field     TYPE lvc_s_fcat-ref_field
      p_ref_table     TYPE lvc_s_fcat-ref_table.
  DATA: ls_fieldcat TYPE lvc_s_fcat.
  ls_fieldcat = VALUE #( tabname   = 'GT_DATA'
  fieldname = p_fieldname
  just      = p_just
  coltext   = p_coltext
  ref_field = p_ref_field
  ref_table = p_ref_table ).
  IF p_fieldname = 'COSTCENTER'.
    ls_fieldcat-hotspot =  abap_true.
  ENDIF.
  APPEND ls_fieldcat TO pt_fieldcat_lvc.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_PF_SET_STATUS
*&---------------------------------------------------------------------*
FORM frm_alv_pf_set_status USING pt_exclude TYPE kkblo_t_extab.
  DATA:lt_exclude TYPE kkblo_t_extab WITH HEADER LINE.
  DATA: lv_string TYPE string.

  SET PF-STATUS 'STANDARD' EXCLUDING lt_exclude.
  SET TITLEBAR 'TITLE' WITH  lv_string.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_USER_COMMAND
*&---------------------------------------------------------------------*
FORM frm_alv_user_command USING p_ucomm LIKE sy-ucomm
      p_selfield TYPE slis_selfield .
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  CALL METHOD lr_grid->check_changed_data.

  CASE p_ucomm.
    WHEN '&IC1'.
      CASE p_selfield-fieldname.
        WHEN 'COSTCENTER'.
          PERFORM:frm_skip_ks03 USING p_selfield-tabindex..
        WHEN OTHERS.
          MESSAGE '无效的功能码' TYPE 'S' DISPLAY LIKE 'W'.
      ENDCASE.
    WHEN '&ZUPLOAD'.
      PERFORM:frm_create_data.
      p_selfield-refresh = 'X'.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_create_data
*&---------------------------------------------------------------------*
FORM frm_create_data .
  DATA lv_controllingarea      TYPE bapi0012_gen-co_area.
  DATA lt_costcenterlist TYPE STANDARD TABLE OF bapi0012_ccinputlist.
  DATA language TYPE bapi0015_10.
  DATA return            TYPE STANDARD TABLE OF bapiret2.
  DATA lv_msg TYPE msgtx.


  LOOP AT gt_data INTO gs_data WHERE msgty IS INITIAL AND sel = abap_true.
    APPEND INITIAL LINE TO lt_costcenterlist ASSIGNING FIELD-SYMBOL(<fs_list>).
    MOVE-CORRESPONDING gs_data TO <fs_list>.
  ENDLOOP.
  lv_controllingarea = 'ZZZZ'.

  language-langu = sy-langu.

  CALL FUNCTION 'BAPI_COSTCENTER_CREATEMULTIPLE'
    EXPORTING
      controllingarea = lv_controllingarea
      language        = language
    TABLES
      costcenterlist  = lt_costcenterlist
      return          = return.
  LOOP AT return INTO DATA(ls_return) WHERE type CA 'AEX'.
    lv_msg = ls_return-message && lv_msg.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MODIFY gt_data FROM VALUE #( msgtx = |{ gc-red }创建失败:{ lv_msg }| msgty = '' ) TRANSPORTING msgtx msgty WHERE sel = abap_true AND msgty IS INITIAL.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MODIFY gt_data FROM VALUE #( msgtx = |{ gc-green }创建成功| msgty = gc-success ) TRANSPORTING msgtx msgty WHERE sel = abap_true AND msgty IS INITIAL.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_skip_ks03
*&---------------------------------------------------------------------*
FORM frm_skip_ks03 USING p_index TYPE i.
  READ TABLE gt_data INTO gs_data INDEX p_index.
  IF sy-subrc EQ 0.
    SET PARAMETER ID 'CAC' FIELD 'ZZZZ'.
    SET PARAMETER ID 'KOS' FIELD gs_data-costcenter..
    CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
    CLEAR gs_data.
  ENDIF.
  
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_skip_sm30
*&---------------------------------------------------------------------*
FORM frm_skip_sm30 .
*DATA DBA_SELLIST    TYPE STANDARD TABLE OF VIMSELLIST.
*DATA EXCL_CUA_FUNCT TYPE STANDARD TABLE OF VIMEXCLFUN.

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                       = 'S'
      view_name                    = 'ZFIT020'
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      invalid_action               = 3
      no_clientindependent_auth    = 4
      no_database_function         = 5
      no_editor_function           = 6
      no_show_auth                 = 7
      no_tvdir_entry               = 8
      no_upd_auth                  = 9
      only_show_allowed            = 10
      system_failure               = 11
      unknown_field_in_dba_sellist = 12
      view_not_found               = 13
      maintenance_prohibited       = 14.

ENDFORM.
