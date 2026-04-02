*&---------------------------------------------------------------------*
*& Report zfir005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir005.

TYPE-POOLS: slis,icon,truxs.
TABLES: sscrfields.

*&---------------------------------------------------------------------&*
*&              alv parameter   *&
*&---------------------------------------------------------------------&*

DATA: g_repid     LIKE sy-repid,
      it_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
      is_layout   TYPE lvc_s_layo.

DATA: is_functxt TYPE smp_dyntxt.
DATA: it_raw  TYPE truxs_t_text_data.

*--------------------------------------------------------------------*
*&   导入字段
*--------------------------------------------------------------------*、、

DATA: BEGIN OF gs_record,
        headid TYPE char5, " 凭证序号
        bukrs  TYPE bukrs, " 公司代码 h COMP_CODE
        blart  TYPE blart, " 凭证类型 h DOC_TYPE
        bldat  TYPE bldat, " 凭证日期 h DOC_DATE
        budat  TYPE budat, " 过账日期 h PSTNG_DATE
        monat  TYPE monat, " 期间 h FIS_PERIOD  / FIS_PERIOD
        waers  TYPE waers , " 货币 09  WAERS
        xblnr  TYPE xblnr, " 参考凭证号 h  REF_DOC_NO
        numpg  TYPE j_1anopg, " 页数
        bktxt  TYPE bktxt, " 凭证抬头文本 h HEADER_TXT
        bschl  TYPE bschl, " 过账代码
        newko  TYPE bu_partner, " 屏幕科目   PART_ACCT
        umskz  TYPE umskz, " 特别总帐标识
        hkont  TYPE hkont, " 总账科目 1 GL_ACCOUNT
        wrbtr  TYPE wrbtr, " 凭证货币金额  09  AMT_DOCCUR
        dmbtr  TYPE dmbtr, " 按本位币计的金额
        mwskz  TYPE mwskz, " 税码，
        hwbas  TYPE hwbas, " 计税基础
        kostl  TYPE kostl, " 成本中心编号  1 COSTCENTER " ITEMNO_ACC
        aufnr  TYPE aufnr, " 订单号 09 / ORDERID
        matnr  TYPE matnr, " 物料 1 MATERIAL
        werks  TYPE werks_d, " 工厂 1 PLANT
        menge  TYPE menge_d, " 数量  1  QUANTITY
        meins  TYPE meins,  " 单位 1 BASE_UOM
        zfbdt  TYPE dzfbdt, " 基准日期
        zuonr  TYPE acpi_zuonr, " 分配
        sgtxt  TYPE sgtxt, " 项目文本 1  ITEM_TEXT
        rstgr  TYPE rstgr, " 原因代码
        xnegp  TYPE xnegp, " 反记账标识 h NEG_POSTNG
      END OF gs_record,
      gt_record LIKE TABLE OF gs_record.


DATA: BEGIN OF gs_out.
        INCLUDE STRUCTURE gs_record.
DATA: sel ,
        message TYPE bapi_msg,
        type,
        belnr   TYPE belnr_d,
      END OF gs_out,
      gt_out LIKE TABLE OF gs_out.


" bapi 数据
DATA: ls_header     TYPE bapiache09, "表头
      lt_amount     TYPE TABLE OF bapiaccr09, "货币项目
      ls_amount     TYPE          bapiaccr09,
      lt_return     TYPE TABLE OF bapiret2,  "返回参数
      ls_return     TYPE          bapiret2,
      lt_accountgl  TYPE TABLE OF bapiacgl09, "总账科目
      ls_accountgl  TYPE          bapiacgl09,
      lt_vendor     TYPE TABLE OF bapiacap09, "供应商科目
      ls_vendor     TYPE          bapiacap09,
      lt_customer   TYPE TABLE OF bapiacar09, "客户科目
      ls_customer   TYPE          bapiacar09,
      ls_accounttax TYPE          bapiactx09,
      lt_accounttax TYPE TABLE OF bapiactx09.
DATA  lt_extension2 TYPE TABLE OF bapiparex.
DATA  ls_extension2 TYPE          bapiparex.
DATA: lv_item TYPE i.


DATA: ls_zexten TYPE  zbadi_acc.

DATA: lv_awtype TYPE  awtyp,
      lv_awkey  TYPE  awkey,
      lv_awsys  TYPE  awsys.


*--------------------------------------------------------------------*
* 选择屏幕
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file LIKE rlgrap-filename MODIF ID m1.  " 文件上传
  SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*          AT SELECTION-SCREEN)                                        *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_file_input CHANGING p_file .

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    PERFORM frm_downdload_template USING 'ZFIR005' '会计凭证导入模板'. " 放下载模板
  ENDIF.
*----------------------------------------------------------------------*
*       初始化                                           *
*----------------------------------------------------------------------*
INITIALIZATION.
  is_functxt-icon_id   = icon_xls.
  is_functxt-icon_text = '下载模板'.
  sscrfields-functxt_01 = is_functxt.

*----------------------------------------------------------------------*
*          START-OF-SELECTION 主程序                     *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lt_excel  LIKE TABLE OF gs_record.
  PERFORM frm_upload_data TABLES lt_excel USING p_file.
  PERFORM frm_process_data.
  PERFORM frm_disp_alv.

*&---------------------------------------------------------------------*
*& FORM frm_process_data
*&---------------------------------------------------------------------*
*& 权限
*&---------------------------------------------------------------------*
FORM frm_process_data .


  LOOP AT gt_out INTO gs_out.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD gs_out-bukrs.
    IF sy-subrc <> 0.
      gs_out-type = 'E'.
      gs_out-message = '您没有公司' && gs_out-bukrs && '的权限!'.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BLA'
    ID 'BLART' FIELD gs_out-blart.
    IF sy-subrc <> 0.
      IF sy-subrc = 0.
        gs_out-type = 'E'.
        gs_out-message = '您没有订单类型' && gs_out-blart && '的权限!'.
      ENDIF.
    ENDIF.

    MODIFY gt_out FROM gs_out.
    CLEAR: gs_out.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& FORM frm_simulation_import_data
*&---------------------------------------------------------------------*
*& 模拟导入处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_simulation_import_data .

  DATA: ls_out LIKE gs_out,
        lt_out LIKE TABLE OF ls_out.
  DATA: lv_kunnr TYPE kunnr,
        lv_lifnr TYPE lifnr.

  IF gt_out[] IS NOT INITIAL.
    lt_out[] = gt_out[].
    SORT lt_out BY headid.
    DELETE ADJACENT DUPLICATES FROM lt_out COMPARING headid.

    SELECT bukrs,saknr
    INTO TABLE @DATA(lt_skb1)
          FROM skb1
          FOR ALL ENTRIES IN @gt_out
          WHERE bukrs = @gt_out-bukrs
          AND saknr = @gt_out-hkont.

    SORT lt_skb1 BY bukrs saknr.

    LOOP AT lt_out INTO ls_out.

      LOOP AT gt_out INTO gs_out WHERE headid = ls_out-headid.

        READ TABLE lt_skb1 INTO  DATA(ls_skb1) WITH KEY bukrs = gs_out-bukrs saknr = gs_out-hkont BINARY SEARCH.
        IF sy-subrc <> 0.
          gs_out-message = gs_out-message && '/' && '总账科目' && gs_out-hkont && '不存在!'.
          gs_out-type = 'E'.
        ENDIF.

        IF gs_out-type <> 'E'.
          PERFORM frm_bapi_fill_document .
        ENDIF.
        CLEAR: gs_out.
      ENDLOOP.

      PERFORM frm_bapi_check_document.

      LOOP AT lt_return INTO ls_return WHERE type CA 'AEX'.
        ls_out-type = ls_return-type.
        ls_out-message = ls_return-message && ls_out-message.
      ENDLOOP.
      IF ls_out-type = ''.
        ls_out-type = 'S'.
        ls_out-message = '凭证检查无错误!'.
      ENDIF.

      MODIFY gt_out FROM ls_out TRANSPORTING  type message WHERE headid = ls_out-headid.
      CLEAR: lt_return[],ls_return,lt_accountgl[],ls_accountgl.
      CLEAR: ls_out,ls_header,lv_item,ls_customer,lt_customer[],ls_vendor,lt_vendor[],ls_amount,lt_amount[],ls_extension2,lt_extension2[],ls_zexten,lt_accounttax[].
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& FORM frm_formal_import_data
*&---------------------------------------------------------------------*
*& 正式导入处理
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_formal_import_data .
  DATA: ls_out LIKE gs_out,
        lt_out LIKE TABLE OF ls_out.

  IF lv_x = 'X'.
    lt_out[] = gt_out[].
    SORT lt_out BY headid.
    DELETE ADJACENT DUPLICATES FROM lt_out[] COMPARING headid.

    LOOP AT lt_out INTO ls_out WHERE type = 'S'.

      LOOP AT gt_out INTO gs_out WHERE headid = ls_out-headid..
        PERFORM frm_bapi_fill_document . " 填充数据
        CLEAR: gs_out.
      ENDLOOP.
      " 创建凭证
      PERFORM frm_bapi_creat_document.
      " 根据结果判断是否成功!
      LOOP AT lt_return INTO ls_return WHERE type = 'E'.
        ls_out-type = ls_return-type.
        ls_out-message = ls_return-message.
      ENDLOOP.

      IF ls_out-type = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
          .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'
*         IMPORTING
*           RETURN        =
          .
        READ TABLE lt_return INTO ls_return WITH KEY type = 'S' id  = 'RW' number = '605'.
        IF sy-subrc = 0.
          ls_out-belnr = ls_return-message_v2+0(10)." 会计凭证号码
          ls_out-message = '创建成功!'.
        ENDIF.

      ENDIF.
      " 回写到内表
      MODIFY gt_out FROM ls_out TRANSPORTING  type message belnr  WHERE headid = ls_out-headid.

      CLEAR: lt_return[],ls_return,lt_accountgl[],ls_accountgl.
      CLEAR: ls_out,ls_header,lv_item,ls_customer,lt_customer[],ls_vendor,lt_vendor[],ls_amount,lt_amount[],ls_extension2,lt_extension2[],ls_zexten,lt_accounttax[].

    ENDLOOP.

  ELSE.
    MESSAGE i888(sabapdocu) WITH '请通过模拟导入检查数据!'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM frm_bapi_fill_document
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_bapi_fill_document.

  ls_header-username   = sy-uname. " 用户名
  ls_header-header_txt = gs_out-bktxt. " 抬头文本
  ls_header-comp_code  = gs_out-bukrs.
  ls_header-doc_type   = gs_out-blart. "
  ls_header-doc_date   = gs_out-bldat . "凭证日期
  ls_header-fisc_year  = gs_out-budat+0(4).  "会计年度
  ls_header-pstng_date = gs_out-budat. " 过账日期
  ls_header-fis_period = gs_out-monat. " 期间
  ls_header-ref_doc_no = gs_out-xblnr. " 参考凭证
  ls_header-header_txt = gs_out-bktxt.
  ls_header-neg_postng = gs_out-xnegp.

  "行项目
  lv_item = lv_item + 1.

  IF gs_out-mwskz IS INITIAL.

    CASE gs_out-bschl.
      WHEN '40' OR '50'. "总账
        CLEAR: ls_accountgl.
        ls_accountgl-itemno_acc    = lv_item.
        ls_accountgl-gl_account    = gs_out-hkont. "总账科目
        ls_accountgl-tax_code      = gs_out-mwskz. " 税码
        ls_accountgl-item_text     = gs_out-sgtxt. "项目文本
        ls_accountgl-costcenter    = gs_out-kostl. "成本中心编号
        ls_accountgl-alloc_nmbr    = gs_out-zuonr. "分配号
        ls_accountgl-orderid       = gs_out-aufnr. "订单号
        ls_accountgl-material_long = gs_out-matnr.
        ls_accountgl-quantity      = gs_out-menge.
        ls_accountgl-base_uom      = gs_out-meins."基本计量单位
        ls_accountgl-plant         = gs_out-werks.
        APPEND ls_accountgl TO lt_accountgl.
        CLEAR: ls_accountgl.

      WHEN '09' OR '19' OR '01' OR '11'.  " 客户
        CLEAR: ls_customer.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_out-newko
          IMPORTING
            output = ls_customer-customer.

        ls_customer-itemno_acc   = lv_item.
        ls_customer-gl_account   = gs_out-hkont.   "总账科目
        ls_customer-item_text    = gs_out-sgtxt.   "项目文本
        ls_customer-alloc_nmbr   = gs_out-zuonr. "分配号
        ls_customer-sp_gl_ind    = gs_out-umskz. "特别总帐标识 特殊总分类帐标志
        ls_customer-tax_code     = gs_out-mwskz. "
        ls_customer-bline_date   = gs_out-zfbdt. "到期日计算的基限日期

        APPEND ls_customer TO lt_customer.
        CLEAR: ls_customer.

      WHEN '29' OR '39' OR '21' OR '31'.  "供应商科目
        CLEAR: ls_vendor.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_out-newko
          IMPORTING
            output = ls_vendor-vendor_no.

        ls_vendor-itemno_acc   = lv_item.
        ls_vendor-gl_account   = gs_out-hkont. "总账科目
        ls_vendor-item_text    = gs_out-sgtxt. "项目文本
        ls_vendor-alloc_nmbr   = gs_out-zuonr. "分配号
        ls_vendor-sp_gl_ind    = gs_out-umskz. "特别总帐标识 特殊总分类帐标志
        ls_vendor-bline_date   = gs_out-zfbdt. "到期日计算的基限日期
        ls_vendor-tax_code     = gs_out-mwskz.
        APPEND ls_vendor TO lt_vendor.
        CLEAR: ls_vendor.
*    WHEN '70' OR '75'." 资产

      WHEN OTHERS.
    ENDCASE.

  ENDIF.
  "货币项目
  CASE gs_out-bschl.
    WHEN '40' OR '01' OR '09' OR '21' OR '29'." OR '70'.
      IF gs_out-waers = 'CNY'.
        ls_amount-amt_doccur = gs_out-wrbtr.  "金额
        ls_amount-itemno_acc = lv_item.
        ls_amount-currency   = 'CNY'.  "货币码
        IF gs_out-hwbas > 0.
          ls_amount-amt_base = gs_out-hwbas.
        ENDIF.

        APPEND ls_amount TO lt_amount.
        CLEAR: ls_amount.
      ELSE.
        ls_amount-amt_doccur = gs_out-dmbtr."wrbtr.  "金额
        ls_amount-curr_type  = '10'.
        ls_amount-itemno_acc = lv_item.
        ls_amount-currency   = 'CNY'.  "货币码
        IF gs_out-hwbas > 0.
          ls_amount-amt_base = gs_out-hwbas.
        ENDIF.
        APPEND ls_amount TO lt_amount.
        CLEAR: ls_amount.

        ls_amount-amt_doccur = gs_out-wrbtr."dmbtr.  "金额
        ls_amount-curr_type  = '00'.
        ls_amount-itemno_acc = lv_item.
        ls_amount-currency   = gs_out-waers.  "货币码
        IF gs_out-hwbas > 0.
          ls_amount-amt_base = gs_out-hwbas.
        ENDIF.
        APPEND ls_amount TO lt_amount.
        CLEAR: ls_amount.
      ENDIF.
    WHEN '50' OR '11' OR '19' OR '31' OR '39'." OR '75'.

      IF gs_out-waers = 'CNY'.
        ls_amount-amt_doccur = gs_out-wrbtr * -1.  "金额
        ls_amount-itemno_acc = lv_item.
        ls_amount-currency   = 'CNY'.  "货币码
        IF gs_out-hwbas > 0.
          ls_amount-amt_base = gs_out-hwbas.
        ENDIF.
        APPEND ls_amount TO lt_amount.
        CLEAR: ls_amount.

      ELSE.

        ls_amount-amt_doccur = gs_out-dmbtr * -1.  "金额
        ls_amount-curr_type  = '10'.
        ls_amount-itemno_acc = lv_item.
        ls_amount-currency   = 'CNY'.  "货币码
        IF gs_out-hwbas > 0.
          ls_amount-amt_base = gs_out-hwbas.
        ENDIF.
        APPEND ls_amount TO lt_amount.
        CLEAR: ls_amount.

        ls_amount-amt_doccur = gs_out-wrbtr * -1.  "金额
        ls_amount-curr_type  = '00'.
        ls_amount-itemno_acc = lv_item.
        ls_amount-currency   = gs_out-waers.  "货币码
        IF gs_out-hwbas > 0.
          ls_amount-amt_base = gs_out-hwbas.
        ENDIF.
        APPEND ls_amount TO lt_amount.
        CLEAR: ls_amount.
      ENDIF.
  ENDCASE.

  IF gs_out-mwskz IS  NOT INITIAL.
    ls_accounttax-itemno_acc      = lv_item.     "行项目号
    ls_accounttax-gl_account      = gs_out-hkont. "科目
    ls_accounttax-tax_code        = gs_out-mwskz.      "销售税代码
    ls_accounttax-direct_tax      = 'X'.           "标识: 直接税收过帐

    APPEND ls_accounttax TO lt_accounttax.
    CLEAR: ls_accounttax.
  ENDIF.

  CLEAR:ls_zexten.
  ls_extension2-structure  = 'ZBADI_ACC'.
  ls_zexten-posnr          = lv_item.
  IF gs_out-hwbas IS NOT INITIAL.
    ls_zexten-taxit          = 'X'.
  ENDIF.

  ls_zexten-rstgr          = gs_out-rstgr.
  ls_zexten-bschl          = gs_out-bschl.  "记帐代码.
  ls_zexten-xnegp          = gs_out-xnegp.  "反记帐
  ls_zexten-numpg          = gs_out-numpg.

  CALL METHOD cl_abap_container_utilities=>fill_container_c
    EXPORTING
      im_value               = ls_zexten
    IMPORTING
      ex_container           = ls_extension2+30
    EXCEPTIONS
      illegal_parameter_type = 1
      OTHERS                 = 2.

  APPEND ls_extension2 TO lt_extension2.
  CLEAR: ls_extension2,ls_zexten.


ENDFORM.
*&---------------------------------------------------------------------*
*& FORM frm_bapi_check_document
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_bapi_check_document .

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = ls_header
    TABLES
      accountgl         = lt_accountgl
      accountreceivable = lt_customer
      accountpayable    = lt_vendor
      accounttax        = lt_accounttax
      currencyamount    = lt_amount
      return            = lt_return
      extension2        = lt_extension2.

ENDFORM.
*&---------------------------------------------------------------------*
*& FORM frm_bapi_creat_document
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_bapi_creat_document .
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = ls_header
    IMPORTING
      obj_type          = lv_awtype
      obj_key           = lv_awkey
      obj_sys           = lv_awsys
    TABLES
      accountgl         = lt_accountgl
      accountreceivable = lt_customer
      accountpayable    = lt_vendor
      accounttax        = lt_accounttax
      currencyamount    = lt_amount
      return            = lt_return
      extension2        = lt_extension2.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM frm_file_input
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM frm_file_input  CHANGING p_p_file.
  DATA:lt_tab         TYPE filetable,         "存放文件名的内表
       lv_file_filter TYPE string,            "file filter
       lv_rc          TYPE i.                 "函数返回值

  REFRESH:lt_tab.
  CLEAR:lv_file_filter,lv_rc.

  lv_file_filter = 'EXCEL文件|*.XLS;*.XLSX'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = '选择数据文件'
      file_filter             = lv_file_filter
*     initial_directory       = 'C:\'
    CHANGING
      file_table              = lt_tab
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lv_rc = 1.
    READ TABLE lt_tab INTO p_file INDEX 1.
  ELSE.
    MESSAGE '没有选择文件或是选择文件错误！' TYPE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& FORM frm_downdload_template
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_downdload_template USING p_objid TYPE wwwdatatab-objid
      p_filename TYPE string.

  DATA: w_objdata     LIKE wwwdatatab,
        w_mime        LIKE w3mime,
        "w_filename    TYPE string,
        w_fullpath    TYPE string VALUE 'C:/TEMP/',
        w_path        TYPE string VALUE 'C:/TEMP/',
        w_destination LIKE rlgrap-filename,
        w_objnam      TYPE string,
        w_rc          LIKE sy-subrc,
        w_errtxt      TYPE string,
        p_dest        LIKE sapb-sappfad,
        w_action      TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = '下载导入模板'
      default_extension    = 'XLS;XLSX'
      default_file_name    = p_filename
      file_filter          = 'EXCEL文件|*.XLSX;*.XLS'
    CHANGING
      filename             = p_filename
      path                 = w_path
      fullpath             = w_fullpath
      user_action          = w_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc = 0 AND w_action EQ 0.
    p_dest = w_fullpath.
    CONDENSE w_objnam NO-GAPS.
    SELECT SINGLE relid objid
    INTO CORRESPONDING FIELDS OF w_objdata
    FROM wwwdata
    WHERE srtf2 = 0 AND relid = 'MI' AND objid = p_objid.
    IF sy-subrc NE 0 OR w_objdata-objid EQ space.
      CONCATENATE '模板文件' w_objnam '不存在' INTO w_errtxt.
      MESSAGE w_errtxt TYPE 'I'.
    ENDIF.
    w_destination = p_dest.
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
      EXPORTING
        key         = w_objdata
        destination = w_destination
      IMPORTING
        rc          = w_rc.
    IF w_rc NE 0.
      CONCATENATE '模板文件：' w_objnam '下载失败' INTO w_errtxt.
      MESSAGE w_errtxt TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM frm_upload_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_upload_data TABLES pt_file TYPE STANDARD TABLE
USING p_file.

  DATA: wa_raw  TYPE truxs_t_text_data,
        li_line TYPE sy-tabix,
        lv_file TYPE rlgrap-filename,
        lv_str  TYPE string.
  DATA:lt_excel TYPE TABLE OF alsmex_tabline.
  DATA ls_excel LIKE LINE OF lt_excel.
  FIELD-SYMBOLS:<fs>.

  lv_file = p_file.

  CLEAR:pt_file,pt_file[].


  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_file
      i_begin_col             = 1
      i_begin_row             = 4
      i_end_col               = 99
      i_end_row               = 9999
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_excel INTO ls_excel.
    ASSIGN COMPONENT ls_excel-col OF STRUCTURE gs_record TO <fs>.
    CONDENSE ls_excel-value.                  " 去掉空格
    TRANSLATE ls_excel-value TO UPPER CASE."转换大写
    <fs> = ls_excel-value.
    AT END OF row.
      "gs_tab-row = ls_excel-row.
      MOVE-CORRESPONDING gs_record TO gs_out.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_out-kostl
        IMPORTING
          output = gs_out-kostl.

      APPEND gs_out TO gt_out.
      CLEAR gs_out.
      CLEAR: gs_record.
    ENDAT.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM frm_disp_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_disp_alv .
  DATA: l_user_command  TYPE slis_formname,
        l_pf_status_set TYPE slis_formname.

  CLEAR it_fieldcat.REFRESH it_fieldcat.

  PERFORM frm_get_catlog.

  l_user_command  = 'USER_COMMAND'.
  l_pf_status_set =  'SET_PF_STATUS'.

  is_layout-zebra = 'X'.
  is_layout-box_fname = 'SEL'.
  is_layout-cwidth_opt = 'X'.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_interface_check        = ' '
      i_buffer_active          = ' '
      i_callback_pf_status_set = l_pf_status_set
      i_callback_user_command  = l_user_command
      i_callback_program       = g_repid
      is_layout_lvc            = is_layout      "it_events = lt_event
      it_fieldcat_lvc          = it_fieldcat[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_out[].
ENDFORM.

FORM frm_set_pf_status USING pwa_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTATUS' .
ENDFORM.

FORM frm_user_command USING pa_ucomm LIKE sy-ucomm
      pwa_selfield TYPE slis_selfield.

  DATA:
    ls_layout TYPE lvc_s_layo,
    lr_grid   TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.                   "
  CALL METHOD lr_grid->check_changed_data."

  CASE pa_ucomm.
    WHEN 'ZUP'. " 模拟导入
      PERFORM frm_simulation_import_data.
      lv_x = 'X'.
    WHEN 'ZUPLOAD'. " 正式导入
      PERFORM frm_formal_import_data.
    WHEN '&IC1'.
    WHEN 'OTHER'.
  ENDCASE.

  pwa_selfield-refresh = 'X'.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*& FORM frm_get_catlog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_catlog .
  DEFINE lit_fieldcat. "
    it_fieldcat-tabname  = 'GT_OUT'.
    it_fieldcat-fieldname = &1.
    it_fieldcat-coltext = &2.
    it_fieldcat-just = &3.
    it_fieldcat-ref_field = &4.
    it_fieldcat-ref_table = &5.

    APPEND  it_fieldcat.
    CLEAR  it_fieldcat.
  END-OF-DEFINITION.   "lit_fieldcat

  lit_fieldcat :
  'TYPE'  '状态'   'C' '' '' ,
  'MESSAGE' '消息' 'L' '' '',
  'BELNR'  '凭证编号'    'L' 'BELNR'  'BSEG' ,
  'HEADID' '凭证序号'    'L' ''  '',
  'BUKRS' '公司代码'     'L' 'BUKRS'  'BSEG',
  'BLART' '凭证类型'     'L' ''  '',
  'BLDAT' '凭证日期'     'L' 'DATUM'  'SYST',
  'BUDAT' '过账日期'     'L' 'DATUM'  'SYST',
  'MONAT' '期间'         'L' ''  '',
  'WAERS' '货币'         'L' 'WAERS'  'T001',
  'XBLNR' '参考凭证号'   'L' ''  '',
  'NUMPG' '附件张数'     'L' ''  '',
  'BKTXT' '凭证抬头文本' 'L' ''  '',
  'BSCHL' '记帐代码'     'L' 'BSCHL'  'BSEG',
  'NEWKO' '屏幕科目'     'L' 'PARTNER'  'BUT000',
  'UMSKZ' '特别总帐标识' 'L' 'UMSKZ'  'BSEG',
  'HKONT' '总账科目'     'L' 'HKONT'  'BSEG',
  'MWSKZ' '税码'         'L' '' '',
  'HWBAS' '计税基础'     'L' '' '',
  'WRBTR' '凭证货币金额' 'L' 'WRBTR'  'BSEG',
  'DMBTR' '按本位币计的金额'  'L' 'DMBTR'  'BSEG',
  'KOSTL' '成本中心编号' 'L' 'KOSTL'  'BSEG',
  'AUFNR' '订单号'       'L' 'AUFNR'  'BSEG',
  'MATNR' '物料'         'L' 'MATNR'  'MARA',
  'WERKS' '工厂'         'L' 'WERKS'  'MARC',
  'MENGE' '数量'         'L' 'MENGE'  'BSEG',
  'MEINS' '单位'         'L' 'MEINS'  'BSEG',
  'ZFBDT' '基准日期'     'L' 'DATUM'  'SYST',
  'ZUONR' '分配'         'L' ''  '',
  'SGTXT' '项目文本'     'L' 'SGTXT'  'BSEG',
  'RSTGR' '原因代码'     'L' 'RSTGR'  'BSEG',
  'XNEGP' '反记账标识'   'L' 'XNEGP'  'BSEG'.

ENDFORM.
