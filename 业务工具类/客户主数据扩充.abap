*&*********************************************************************
*& program name:              [ZSDE014]
*& module name :              [SD]
*& apply author:              []
*& author:                    []
*& started on:                [30.01.2024 10:34:03]
*& transaction:               [ZSDE014]
*& program type:              [Report]
*& transfer requests:         [ZSDE014]
*& program description :      [客户主数据扩充]
*&*&*******************************************************************
*& revision log                                                       *
*&                                                                    *
*& log          date                 author       description         *
*& ------      ------------         ---------     -----------         *
*& v1.0       30.01.2024 10:34:15             初稿                *
*& v2.0       23.04.2024                      仅扩充公司视图功能  *
*&*********************************************************************
REPORT  zsde014.

*--------------------------------------------------------------------*
*type-pools
TYPE-POOLS:abap,icon.
*--------------------------------------------------------------------*
*tables
TABLES:ztsd013a,ztsd013b,kna1,knvv,knb1.
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
TYPES: BEGIN OF ty_alv,
         kunnr    TYPE kna1-kunnr,
         name1    TYPE kna1-name1,
         bu_group TYPE but000-bu_group,
         zcompy   TYPE char1,
         zsaleg   TYPE char1,
       END OF ty_alv.
*--------------------------------------------------------------------*
*data
DATA: gt_t013a LIKE TABLE OF ztsd013a,
      gt_t013b LIKE TABLE OF ztsd013b.

DATA:BEGIN OF gs_data,
       kunnr     TYPE kna1-kunnr,
       name1     TYPE kna1-name1,
       bu_group  TYPE but000-bu_group,
       zcompy    TYPE char1,
       zsaleg    TYPE char1,
       compylist TYPE TABLE OF ztsd013a,
       saleglist TYPE TABLE OF ztsd013b,
       sel,
       msgtx     TYPE msgtx,
     END OF gs_data.
DATA: gt_data LIKE  TABLE OF gs_data.
*--------------------------------------------------------------------*
*class


*--------------------------------------------------------------------*
*screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  SELECT-OPTIONS:s_kunnr FOR kna1-kunnr,
                 s_katr1 FOR kna1-katr1,
                 s_erdat FOR kna1-erdat..
SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-bl2.
  PARAMETERS: p_comp AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK bl2.
SELECTION-SCREEN FUNCTION KEY 1.
*--------------------------------------------------------------------*
*screen event
START-OF-SELECTION.
  PERFORM:frm_main.

END-OF-SELECTION.

  IF sy-batch EQ abap_true.
    PERFORM:frm_set_job.
  ELSE.
    PERFORM:frm_alv_display.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form frm_main
*&---------------------------------------------------------------------*
FORM frm_main .

  PERFORM:frm_get_init_data.

  PERFORM:frm_get_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_init_data
*&---------------------------------------------------------------------*
FORM frm_get_init_data .
  SELECT * FROM ztsd013a INTO TABLE @gt_t013a .
  SELECT * FROM ztsd013b INTO TABLE @gt_t013b .
  "有时存在销售视图数据维护缺少，但是，要确保维护了公司视图就必须维护销售视图，
  "这时，需要通过公司代码生成一个通用的销售视图，
  LOOP AT gt_t013a INTO DATA(ls_t013a).
    READ TABLE gt_t013b TRANSPORTING NO FIELDS WITH KEY vkorg = ls_t013a-bukrs.
    IF sy-subrc NE 0.
      SELECT SINGLE COUNT(*) FROM tvko WHERE bukrs = ls_t013a-bukrs.
      CHECK sy-subrc EQ 0.
      APPEND INITIAL LINE TO gt_t013b ASSIGNING FIELD-SYMBOL(<fs_t013b>).
      <fs_t013b> = VALUE #( vkorg = ls_t013a-bukrs
                            vtweg = '00'
                            spart = '00'
                            bzirk = '000001'
                            waers = 'CNY'
                            konda = '01'
                            kalks = '1'
                            vwerk = ls_t013a-bukrs
                            vsbed = '01'
                            zterm = '0001'
                            ktgrd = ''
                            taxkd = '1' ).
    ENDIF.
  ENDLOOP.

  SORT gt_t013a BY bu_group bukrs.
  SORT gt_t013b BY vkorg vtweg spart.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
FORM frm_get_data .

  SELECT a~kunnr,a~name1,b~bu_group
    FROM kna1 AS a INNER JOIN but000 AS b
    ON a~kunnr = b~partner
    INTO TABLE @DATA(lt_kna1)
    WHERE  a~kunnr IN @s_kunnr
      AND  a~katr1 IN @s_katr1
      AND  a~erdat IN @s_erdat.
  IF sy-subrc  EQ 0 AND lt_kna1[] IS NOT INITIAL.

    SELECT kunnr,bukrs FROM knb1
      INTO TABLE @DATA(lt_knb1)
      FOR ALL ENTRIES IN @lt_kna1
      WHERE kunnr = @lt_kna1-kunnr..

    SELECT kunnr,vkorg,vtweg,spart FROM knvv
      INTO TABLE @DATA(lt_knvv)
      FOR ALL ENTRIES IN @lt_kna1
      WHERE kunnr = @lt_kna1-kunnr.

    SORT lt_kna1 BY kunnr.
    SORT lt_knb1 BY kunnr bukrs.
    SORT lt_knvv BY kunnr vkorg vtweg spart..

    LOOP AT lt_kna1 INTO DATA(ls_kna1).

      APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      MOVE-CORRESPONDING ls_kna1 TO <fs_data>.
      "公司数据
      LOOP AT gt_t013a INTO DATA(ls_t013a) WHERE bu_group = ls_kna1-bu_group..
        READ TABLE lt_knb1 TRANSPORTING NO FIELDS WITH KEY kunnr = ls_kna1-kunnr
                                                           bukrs = ls_t013a-bukrs BINARY SEARCH.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO <fs_data>-compylist ASSIGNING FIELD-SYMBOL(<fs_compy>).
          MOVE-CORRESPONDING ls_t013a TO <fs_compy>.
          <fs_data>-zcompy = abap_true.
        ENDIF.
      ENDLOOP.
      "销售数据
      LOOP AT gt_t013b INTO DATA(ls_t013b).
        READ TABLE lt_knvv TRANSPORTING NO FIELDS WITH KEY kunnr = ls_kna1-kunnr
                                                           vkorg = ls_t013b-vkorg
                                                           vtweg = ls_t013b-vtweg
                                                           spart = ls_t013b-spart BINARY SEARCH.
        IF sy-subrc NE 0.
          APPEND INITIAL LINE TO <fs_data>-saleglist ASSIGNING FIELD-SYMBOL(<fs_saleg>).
          MOVE-CORRESPONDING ls_t013b TO <fs_saleg>.
          <fs_data>-zsaleg = abap_true.
        ENDIF.
      ENDLOOP.

    ENDLOOP.
  ELSE.
    MESSAGE '没有找到符合条件的数据' TYPE 'S'.
    LEAVE LIST-PROCESSING.
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
      t_outtab                 = gt_data
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
     'KUNNR ' 'C' '客户编码'  'KUNNR' 'KNA1',
     'NAME1'  'L' '客户名称'  'NAME1' 'KNA1',
     'ZCOMPY' 'C' '需扩展公司代码'  '' '',
     'ZSALEG' 'C' '需扩展销售组织'  '' '',
     'MSGTX'  'L' '消息'  '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_alv_set_fieldcat
*&---------------------------------------------------------------------*
FORM frm_alv_set_fieldcat  TABLES pt_fieldcat_lvc TYPE  lvc_t_fcat
                            USING p_fieldname     TYPE lvc_s_fcat-fieldname
                                  p_just          TYPE lvc_s_fcat-just
                                  p_coltext       TYPE lvc_s_fcat-coltext
                                  p_ref_field     TYPE lvc_s_fcat-ref_field
                                  p_ref_table     TYPE lvc_s_fcat-ref_table.
  DATA: ls_fieldcat TYPE lvc_s_fcat.
  ls_fieldcat = VALUE #( fieldname = p_fieldname
                         just      = p_just
                         coltext   = p_coltext
                         ref_field = p_ref_field
                         ref_table = p_ref_table ).
  IF p_fieldname = 'ZCOMPY' OR p_fieldname = 'ZSALEG'.
    ls_fieldcat-checkbox = abap_true.
  ENDIF.
  IF p_fieldname = 'ZSALEG' AND p_comp EQ abap_true.
    RETURN.
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
        WHEN 'KUNNR'.
          SET PARAMETER ID 'KUN' FIELD p_selfield-value.
          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
          MESSAGE '无效的功能码' TYPE 'S' DISPLAY LIKE 'W'.
      ENDCASE.
    WHEN '&ZEXON'.
      READ TABLE gt_data TRANSPORTING NO FIELDS WITH KEY sel = abap_true.
      IF sy-subrc NE 0.
        MESSAGE '选择需要扩充的客户' TYPE 'S'.
      ELSE.
        PERFORM:frm_expand_data.
      ENDIF.
      p_selfield-refresh = 'X'.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_expand_data
*&---------------------------------------------------------------------*
FORM frm_expand_data .
  DATA: lv_msgty TYPE msgty.
  DATA:ls_data   TYPE cvis_ei_extern,
       ls_header TYPE bus_ei_header.
  DATA:ls_compy   TYPE cmds_ei_cmd_company,
       ls_sales   TYPE cmds_ei_cmd_sales,
       ls_central TYPE cmds_ei_central_data.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE sel = abap_true.
    CHECK <fs_data>-zcompy EQ abap_true OR <fs_data>-zsaleg EQ abap_true.
    ls_header-object_instance-bpartner = <fs_data>-kunnr.

    PERFORM:frm_get_guid USING <fs_data>-kunnr CHANGING ls_header-object_instance-bpartnerguid.

    PERFORM:frm_expand_compy_data USING <fs_data> CHANGING ls_compy-company.

    PERFORM:frm_expand_saleg_data USING <fs_data> CHANGING ls_sales-sales ls_central-tax_ind-tax_ind.

    PERFORM:frm_add_other_data_to_bapi USING <fs_data> ls_header ls_compy ls_sales ls_central  CHANGING ls_data .

    PERFORM:frm_call_bapi_create_data USING ls_data CHANGING lv_msgty <fs_data>-msgtx..
    IF lv_msgty EQ 'S'.
      CLEAR: <fs_data>-compylist[],<fs_data>-saleglist[],<fs_data>-zcompy,<fs_data>-zsaleg.
    ENDIF.
    CLEAR:ls_data,ls_header,ls_compy,ls_sales,ls_central.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_expand_compy_data
*&---------------------------------------------------------------------*
FORM frm_expand_compy_data  USING  pu_data LIKE gs_data
                            CHANGING pc_company TYPE cmds_ei_company_t.

  DATA: ls_cmds_compy TYPE cmds_ei_company.
  CHECK pu_data-compylist[] IS NOT INITIAL.
  LOOP AT pu_data-compylist INTO DATA(ls_compy).
    ls_cmds_compy-task = 'M'.
    ls_cmds_compy-data_key-bukrs = ls_compy-bukrs.
    ls_cmds_compy-data-akont     = ls_compy-akont.
    IF pu_data-kunnr = 'R6666'.
      ls_cmds_compy-data-akont   = '1122010000'.
    ENDIF.
    ls_cmds_compy-data-zterm     = ls_compy-zterm.
    ls_cmds_compy-datax-akont    = 'X'.
    ls_cmds_compy-datax-zterm    = 'X'.
    APPEND ls_cmds_compy TO pc_company.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_expand_saleg_data
*&---------------------------------------------------------------------*
FORM frm_expand_saleg_data  USING  pu_data LIKE gs_data
                            CHANGING pc_sales TYPE cmds_ei_sales_t
                                     pc_tax   TYPE cmds_ei_tax_ind_t.
  DATA:ls_cmds_sales TYPE cmds_ei_sales,
       ls_tax        TYPE cmds_ei_tax_ind.
  CHECK pu_data-saleglist[] IS NOT INITIAL.
  CHECK p_comp NE abap_true.
  LOOP AT pu_data-saleglist INTO DATA(ls_sales).
    ls_cmds_sales-task = 'M'.
    ls_cmds_sales-data_key-vkorg = ls_sales-vkorg.
    ls_cmds_sales-data_key-vtweg = ls_sales-vtweg.
    ls_cmds_sales-data_key-spart = ls_sales-spart.

    ls_cmds_sales-data-bzirk = ls_sales-bzirk.
    ls_cmds_sales-data-waers = ls_sales-waers.
    ls_cmds_sales-data-konda = ls_sales-konda.
    ls_cmds_sales-data-kalks = ls_sales-kalks.
    ls_cmds_sales-data-vwerk = ls_sales-vwerk.
    ls_cmds_sales-data-vsbed = ls_sales-vsbed.
    ls_cmds_sales-data-zterm = ls_sales-zterm.
    ls_cmds_sales-data-ktgrd = '01'.

    ls_cmds_sales-datax-bzirk = 'X'.
    ls_cmds_sales-datax-waers = 'X'.
    ls_cmds_sales-datax-konda = 'X'.
    ls_cmds_sales-datax-kalks = 'X'.
    ls_cmds_sales-datax-vwerk = 'X'.
    ls_cmds_sales-datax-vsbed = 'X'.
    ls_cmds_sales-datax-zterm = 'X'.
    ls_cmds_sales-datax-ktgrd = 'X'.
    APPEND ls_cmds_sales TO pc_sales.

    ls_tax-task = 'M'.
    ls_tax-data_key-aland = 'CN' .
    ls_tax-data_key-tatyp = 'MWST' .
    ls_tax-data-taxkd  = ls_sales-taxkd.
    ls_tax-datax-taxkd = 'X'.
    APPEND ls_tax TO pc_tax.
  ENDLOOP.
  SORT pc_tax.
  DELETE ADJACENT DUPLICATES FROM pc_tax COMPARING ALL FIELDS.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_add_other_data_to_bapi
*&---------------------------------------------------------------------*
FORM frm_add_other_data_to_bapi  USING  pu_data  LIKE gs_data
                                        pu_header TYPE bus_ei_header
                                        pu_compy TYPE cmds_ei_cmd_company
                                        pu_sales TYPE cmds_ei_cmd_sales
                                        pu_central TYPE cmds_ei_central_data
                              CHANGING  pc_data TYPE cvis_ei_extern.
  DATA: ls_roles TYPE bus_ei_bupa_roles,
        lt_roles TYPE bus_ei_bupa_roles_t.

  pc_data-partner-header = pu_header.
  pc_data-customer-header-object_instance-kunnr = pu_data-kunnr.
  pc_data-customer-central_data       = pu_central.
  pc_data-customer-company_data       = pu_compy.
  pc_data-customer-sales_data         = pu_sales.
  pc_data-customer-header-object_task = 'U'.

  SELECT rltyp FROM but100
    INTO TABLE @DATA(lt_but100)
    WHERE partner = @pu_data-kunnr
      AND ( rltyp EQ 'FLCU00' OR  rltyp EQ 'FLCU01' ).
  IF sy-subrc <> 0 OR sy-dbcnt <> 2.
    READ TABLE lt_but100 TRANSPORTING NO FIELDS WITH KEY rltyp = 'FLCU00'.
    IF sy-subrc NE 0.
      ls_roles-task = 'I'.
      ls_roles-data_key = 'FLCU00'.
      APPEND ls_roles TO lt_roles.
      CLEAR:ls_roles.
    ENDIF.
    READ TABLE lt_but100 TRANSPORTING NO FIELDS WITH KEY rltyp = 'FLCU01'.
    IF sy-subrc NE 0 AND ( p_comp NE abap_true OR pu_data-saleglist[] IS NOT INITIAL ).
      ls_roles-task = 'I'.
      ls_roles-data_key = 'FLCU01'.
      APPEND ls_roles TO lt_roles.
    ENDIF.
    pc_data-partner-header-object_task = 'M'.
    pc_data-partner-central_data-role-roles = lt_roles.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_call_bapi_create_data
*&---------------------------------------------------------------------*
FORM frm_call_bapi_create_data USING pu_data TYPE cvis_ei_extern CHANGING pu_msgty pu_msgtx.
  DATA:lt_data   TYPE cvis_ei_extern_t,
       lt_return TYPE bapiretm.

  APPEND pu_data TO lt_data.
  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
    EXPORTING
      i_data   = lt_data
    IMPORTING
      e_return = lt_return.
  READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>) INDEX 1.
  IF sy-subrc = 0.
    LOOP AT <fs_ret>-object_msg ASSIGNING FIELD-SYMBOL(<fs_msg>) WHERE type CO 'EAX'.
      pu_msgtx = <fs_msg>-message && pu_msgtx.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      pu_msgty = 'E'.
      pu_msgtx = |{ gc-red }客户主数据扩展失败:{ pu_msgtx }! |.
    ELSE.
      pu_msgty = 'S'.
      pu_msgtx = |{ gc-green }客户主数据扩展成功! |.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      WAIT UP TO '0.5' SECONDS.
    ENDIF.
  ELSE.
    pu_msgty = 'S'.
    pu_msgtx = |{ gc-green }客户主数据扩展成功! |.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    WAIT UP TO '0.5' SECONDS.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_GUID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_HEADER_OBJECT_INSTANCE_BPAR
*&---------------------------------------------------------------------*
FORM frm_get_guid USING pu_kunnr TYPE kunnr  CHANGING pc_guid.
  DATA:i_partner_ids   TYPE bu_partner_t,
       i_partner_guids TYPE bu_partner_guid_t,
       r_customers     TYPE cvis_cust_link_t.
  APPEND INITIAL LINE TO i_partner_ids ASSIGNING FIELD-SYMBOL(<wa_data>).
  <wa_data>-partner = pu_kunnr.
  r_customers = cvi_mapper=>get_instance( )->get_assigned_customers_for_bps(
                                               i_partner_ids = i_partner_ids[] ).
  READ TABLE r_customers INTO DATA(ls_customers) INDEX 1.
  pc_guid  = ls_customers-partner_guid.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_job
*&---------------------------------------------------------------------*
FORM frm_set_job .

  CHECK gt_data[] IS NOT INITIAL.

  DELETE gt_data WHERE zcompy IS INITIAL AND zsaleg IS INITIAL.

  gs_data-sel = abap_true.

  MODIFY gt_data FROM gs_data TRANSPORTING sel WHERE sel = abap_false.

  PERFORM: frm_expand_data.

  MESSAGE '后台任务执行结束' TYPE 'S'.
ENDFORM.

* ztsd013a
* BU_GROUP
* BUKRS	
* AKONT	
* ZTERM	

*ztsd013b
*VKORG
*VTWEG
*SPART
*BZIRK
*WAERS
*KONDA
*KALKS
*VWERK
*VSBED
*ZTERM
*KTGRD
*TAXKD