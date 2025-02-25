*&*********************************************************************
*& program name:              [ZFIE000]
*& module name :              [FI]
*& apply author:              []
*& author:                    []
*& started on:                [21.02.2024 16:51:22]
*& transaction:               [NULL]
*& program type:              [Report]
*& transfer requests:         []
*& program description :      [打开物料及财务帐]
*&*&*******************************************************************
*& revision log                                                       *
*&                                                                    *
*& log          date                 author       description         *
*& ------      ------------         ---------     -----------         *
*& v1.0       21.02.2024 16:51:22               初稿                *
*&*********************************************************************
REPORT  zfie000.

*--------------------------------------------------------------------*
*type-pools
TYPE-POOLS:abap,icon.
*--------------------------------------------------------------------*
*tables
TABLES:t001b,t001.
*--------------------------------------------------------------------*
*constants
CONSTANTS: BEGIN OF gc,
             success TYPE bapi_mtype VALUE 'S',
             fail    TYPE bapi_mtype VALUE 'E',
             green   TYPE icon-name VALUE icon_led_green,
             red     TYPE icon-name VALUE icon_led_red,
           END OF gc.
*--------------------------------------------------------------------*
*type
TYPES: BEGIN OF ty_comp,
         bukrs TYPE bukrs,
         opvar TYPE opvar,
       END OF ty_comp.
*--------------------------------------------------------------------*
*data
DATA: gt_comp  TYPE TABLE OF ty_comp,
      gt_t001b TYPE TABLE OF t001b..
*--------------------------------------------------------------------*
*class

*--------------------------------------------------------------------*
*screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  SELECT-OPTIONS: s_bukrs FOR  t001-bukrs .
  PARAMETERS    : p_datum TYPE sy-datum .
  SELECTION-SCREEN SKIP.
  PARAMETERS    : p_ob52 AS CHECKBOX DEFAULT 'X'.
  PARAMETERS    : p_mmpv AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN FUNCTION KEY 1.
*--------------------------------------------------------------------*
*screen event
INITIALIZATION.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  PERFORM:frm_main.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form frm_main
*&---------------------------------------------------------------------*
FORM frm_main .

  IF p_datum IS INITIAL OR s_bukrs[] IS INITIAL.
    MESSAGE '输入条件不可为空' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM:frm_select_comp_code.

  IF p_ob52 EQ abap_true.
    PERFORM:frm_select_posting_process.
    PERFORM frm_set_new_posting_process. " 开财务账期
  ENDIF.

  IF p_mmpv EQ abap_true.
    IF gt_comp[] IS NOT INITIAL.
      LOOP AT gt_comp INTO DATA(ls_comp).
        PERFORM:frm_post_material_process USING ls_comp-bukrs.  " 开物料账期
      ENDLOOP.
      MESSAGE 'Job设置执行成功' TYPE 'S'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_select_comp_code
*&---------------------------------------------------------------------*
FORM frm_select_comp_code .
  SELECT bukrs,opvar INTO TABLE @gt_comp
    FROM t001 WHERE bukrs IN @s_bukrs.
  IF sy-subrc NE 0.
    MESSAGE '没有相关公司记录' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING..
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_select_posting_process
*&---------------------------------------------------------------------*
FORM frm_select_posting_process .
  IF NOT gt_comp[] IS INITIAL.
    SELECT
      *
    FROM
      t001b
    FOR ALL ENTRIES IN @gt_comp
    WHERE bukrs EQ @gt_comp-opvar
      AND rrcty EQ '0'
    INTO CORRESPONDING FIELDS OF TABLE @gt_t001b.
    IF sy-subrc NE 0.
      MESSAGE '公司代码不存在对应记帐期间变式' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_new_posting_process
*&---------------------------------------------------------------------*
FORM frm_set_new_posting_process .
  DATA: lv_to_year_1    TYPE t001b-toye1,
        lv_to_process_1 TYPE t001b-tope1.
  FIELD-SYMBOLS: <lfs_t001b> LIKE LINE OF gt_t001b.

  lv_to_year_1 = p_datum(4).
  lv_to_process_1 = p_datum+4(2).

  LOOP AT gt_t001b ASSIGNING <lfs_t001b>.
    <lfs_t001b>-toye1 = lv_to_year_1.
    <lfs_t001b>-tope1 = lv_to_process_1.
  ENDLOOP.

  MODIFY t001b FROM TABLE gt_t001b.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    MESSAGE |财务账期{ lv_to_year_1 }{ lv_to_process_1 }已打开| TYPE 'S'.
  ELSE.
    ROLLBACK WORK .
    MESSAGE |财务账期{ lv_to_year_1 }{ lv_to_process_1 }打开失败| TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_post_material_process
*&---------------------------------------------------------------------*
FORM frm_post_material_process USING pu_bukrs TYPE bukrs.

  DATA: iv_jobcount TYPE tbtcjob-jobcount,
        iv_jobname  TYPE tbtcjob-jobname.
  DATA: lv_flag TYPE x ."""允许负库存存在

  lv_flag = abap_false.
  IF pu_bukrs = '2020'.
    lv_flag = abap_true.
  ENDIF.

  iv_jobname = |{ sy-repid }_{ sy-datum }_{ pu_bukrs }|." sy-repid && sy-datum && pu_bukrs.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = iv_jobname
      jobclass         = 'A'
    IMPORTING
      jobcount         = iv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  SUBMIT rmmmperi
      WITH i_vbukr = pu_bukrs
      WITH i_bbukr = pu_bukrs
      WITH i_datum = p_datum
      WITH i_xcomp = 'X'
      WITH i_xinco = ''
      WITH i_xmove = ''
      WITH i_xnegq = lv_flag
      WITH i_xnegv = lv_flag
      USER sy-uname
      VIA JOB iv_jobname
      NUMBER  iv_jobcount
      AND RETURN.

  DATA: iv_sdlstrtdt TYPE tbtcjob-sdlstrtdt,
        iv_sdlstrttm TYPE tbtcjob-sdlstrttm.
  GET TIME.
  iv_sdlstrtdt = sy-datum.
  iv_sdlstrttm = sy-uzeit + 3.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = iv_jobcount
      jobname              = iv_jobname
      sdlstrtdt            = iv_sdlstrtdt
      sdlstrttm            = iv_sdlstrttm
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      invalid_target       = 8
      invalid_time_zone    = 9
      OTHERS               = 10.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



ENDFORM.
