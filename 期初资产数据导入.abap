*&---------------------------------------------------------------------*
*& Report ZFIR008
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir008.
TABLES:sscrfields.

*&---------------------------------------------------------------------&*
*&              alv parameter   *&
*&---------------------------------------------------------------------&*

DATA: g_repid LIKE sy-repid,
      it_fieldcat TYPE lvc_t_fcat WITH HEADER LINE,
      is_layout   TYPE lvc_s_layo.

DATA: is_functxt TYPE smp_dyntxt.
DATA: it_raw TYPE truxs_t_text_data.

DATA: BEGIN OF gs_record,
        anlkl      LIKE  anla-anlkl, " CONVERSION_EXIT_ALPHA_INPUT
        bukrs      LIKE  anla-bukrs, " 公司代码
        txt50      LIKE  anla-txt50, " 资产描述
        txa50      LIKE  anla-txa50, " 附加资产描述
        anlhtxt    LIKE  anlh-anlhtxt, " 资产主号说明
        sernr      LIKE  anla-sernr, " CONVERSION_EXIT_GERNR_INPUT
        invnr      LIKE  anla-invnr, " 库存编号
        aktiv      LIKE  anla-aktiv, " 资产资本化日期
        kostl      LIKE  anlz-kostl, " 成本中心 CONVERSION_EXIT_ALPHA_INPUT
        caufn      LIKE  anlz-caufn, " 内部订单 CONVERSION_EXIT_ALPHA_INPUT
        ord41      LIKE  anla-ord41, " 资产状态
        lifnr      LIKE  anla-lifnr, " 供应商科目编号(其他关键字)
        afasl      LIKE  anlb-afasl, " 折旧码
        ndjar      LIKE  anlb-ndjar, " 计划年使用期
        ndper      LIKE  anlb-ndper, " 计划使用期间
        afabg      LIKE  anlb-afabg, " 折旧计算开始日期
        schrw      LIKE  anlb-schrw, " 资产残值
        schrw_proz LIKE  anlb-schrw_proz, " 残值作为 APC 的百分比
        kansw      TYPE  kansw, " 累积购置和生产费用
        knafa      TYPE  knafa, " 累计正常折旧
        nafag      TYPE  nafag, " 记帐在当前年的正常折旧
      END OF gs_record.

DATA: gt_record LIKE TABLE OF gs_record.

DATA: BEGIN OF gs_out.
        INCLUDE STRUCTURE gs_record.
DATA: sel,
        light(30),
        message   TYPE bapi_msg,
        anln1     LIKE  anla-anln1,
        anln2     LIKE  anla-anln2,
      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.
*-----------------------------------------------------------------------
* BAPI
*-----------------------------------------------------------------------
DATA:ls_key TYPE bapi1022_key,
     ls_data      TYPE bapi1022_feglg001,
     ls_datax     TYPE bapi1022_feglg001x,
     ls_post      TYPE bapi1022_feglg002,
     ls_postx     TYPE bapi1022_feglg002x,
     ls_time      TYPE bapi1022_feglg003,
     ls_timex     TYPE bapi1022_feglg003x,
     ls_allo      TYPE bapi1022_feglg004,
     ls_allox     TYPE bapi1022_feglg004x,
     ls_origin    TYPE bapi1022_feglg009,
     ls_originx   TYPE bapi1022_feglg009x,
     ls_inven     TYPE bapi1022_feglg011,
     ls_invenx    TYPE bapi1022_feglg011x,
     lt_depareas  TYPE bapi1022_dep_areas OCCURS 0 WITH HEADER LINE,
     lt_depareasx TYPE bapi1022_dep_areasx OCCURS 0 WITH HEADER LINE,
     lt_cvalues   TYPE bapi1022_cumval OCCURS 0 WITH HEADER LINE,
     lt_pvalues   TYPE bapi1022_postval OCCURS 0 WITH HEADER LINE,
     lt_trans     TYPE bapi1022_trtype OCCURS 0 WITH HEADER LINE,
     lt_return    TYPE bapiret2 OCCURS 0 WITH HEADER LINE..

*-----------------------------------------------------------------------
*SELECTION SCREEN
*-----------------------------------------------------------------------
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename .
SELECTION-SCREEN:END OF BLOCK b1.
SELECTION-SCREEN FUNCTION KEY 1.
*----------------------------------------------------------------------*
*       初始化                                           *
*----------------------------------------------------------------------*
INITIALIZATION.
  is_functxt-icon_id   = icon_xls.
  is_functxt-icon_text = '下载模板'.
  sscrfields-functxt_01 = is_functxt.
*---------------------------------------------------------------*
*AT SELECTION-SCREEN
*---------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM file_input USING p_file.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM downdload_template  USING  'ZFIR008' '期初资产数据导入模板'.
  ENDCASE.
*&----------------------------------------------------------------------&
*&                             主程序
*&----------------------------------------------------------------------&
START-OF-SELECTION.
  DATA: lt_excel  LIKE TABLE OF gs_record.
  " 上传文件。
  PERFORM: upload_data TABLES lt_excel USING p_file.
  " 添加前导零
  PERFORM: prcoess_data.
  " ALV输出
  PERFORM: disp_alv.
*&---------------------------------------------------------------------*
*& Form file_input
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM file_input CHANGING p_p_file.
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
*& Form downdload_template
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM downdload_template USING p_objid TYPE wwwdatatab-objid
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
*& Form upload_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXCEL
*&      --> P_FILE
*&---------------------------------------------------------------------*
FORM upload_data TABLES pt_file TYPE STANDARD TABLE
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
      i_begin_row             = 3
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

      APPEND gs_out TO gt_out.
      CLEAR: gs_out.
      CLEAR: gs_record.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form prcoess_data
*&---------------------------------------------------------------------*
FORM prcoess_data .
  " c处理数据，补充前导零。
  LOOP AT gt_out INTO gs_out..

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_out-anlkl
      IMPORTING
        output = gs_out-anlkl.

    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        input  = gs_out-sernr
      IMPORTING
        output = gs_out-sernr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_out-kostl
      IMPORTING
        output = gs_out-kostl.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_out-caufn
      IMPORTING
        output = gs_out-caufn.


    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD gs_out-bukrs.
    IF sy-subrc <> 0.
      gs_out-light = '@5C@'.
      gs_out-message = '您没有公司' && gs_out-bukrs && '的权限!'.
    ENDIF.



    MODIFY gt_out FROM gs_out.
    CLEAR: gs_out.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form disp_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM disp_alv .
  DATA: l_user_command  TYPE slis_formname,
        l_pf_status_set TYPE slis_formname.

  CLEAR it_fieldcat.REFRESH it_fieldcat.

  PERFORM get_catlog.

  l_pf_status_set =  'SET_PF_STATUS'. " GUI状态。
  l_user_command  = 'USER_COMMAND'.  " 状态按钮命令

  is_layout-zebra = 'X'.
  is_layout-box_fname = 'SEL'. " 选择列
  is_layout-cwidth_opt = 'X'.  " 优化列宽
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
FORM set_pf_status USING pwa_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTATUS' .
ENDFORM.

FORM user_command USING pa_ucomm LIKE sy-ucomm
      pwa_selfield TYPE slis_selfield.

  DATA: ls_layout TYPE lvc_s_layo,
        lr_grid   TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data."

  CASE pa_ucomm.
    WHEN 'ZCRT'.
      " 导入资产数据。，判断是否为空。
      IF gt_out[] IS NOT INITIAL.
        PERFORM: importing_asset_data.
      ELSE.
        MESSAGE e888(sabapdocu) WITH '无数据进行导入!'.
      ENDIF.

    WHEN '&IC1'.

    WHEN 'OTHER'.
  ENDCASE.

  pwa_selfield-refresh = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_catlog
*&---------------------------------------------------------------------*
FORM get_catlog .

  DEFINE lit_fieldcat. "
    it_fieldcat-tabname  = 'GT_OUT'.
    it_fieldcat-fieldname = &1.
    it_fieldcat-coltext = &2.
    it_fieldcat-just = &3.
    it_fieldcat-ref_table = &4.
    it_fieldcat-ref_field = &5.


    APPEND  it_fieldcat.
    CLEAR  it_fieldcat.
  END-OF-DEFINITION.

  lit_fieldcat :
    'LIGHT'       '状态灯'        'C' ''   '',
    'MESSAGE'     '消息'          'L' ''   '',
    'ANLN1'       '资产编号'      'L' 'ANLA'  'ANLN1' ,
    'ANLN2'       '资产子编号'    'L' 'ANLA'  'ANLN2' ,
    'ANLKL'       '资产分类'      'L' 'ANLA'  'ANLKL' ,
    'BUKRS'       '公司代码'      'L' 'ANLA'  'BUKRS' ,
    'TXT50'       '资产描述'      'L' 'ANLA'  'TXT50' ,
    'TXA50'       '附加资产描述'  'L' 'ANLA'  'TXA50' ,
    'ANLHTXT'     '资产主号文本'  'L' 'ANLH'  'ANLHTXT' ,
    'SERNR'       '序列号'        'L' 'ANLA'  'SERNR' ,
    'INVNR'       '库存编号'      'L' 'ANLA'  'INVNR' ,
    'AKTIV'       '资本化日期'    'L' 'ANLA'  'AKTIV' ,
    'KOSTL'       '成本中心'      'L' 'ANLZ'  'KOSTL' ,
    'CAUFN'       '内部订单'      'L' 'ANLZ'  'CAUFN' ,
    'ORD41'       '资产状态'      'L' 'ANLA'  'ORD41' ,
    'LIFNR'       '供应商'        'L' 'ANLA'  'LIFNR' ,
    'AFASL'       '折旧码'        'L' 'ANLB'  'AFASL' ,
    'NDJAR'       '使用年限'      'L' 'ANLB'  'NDJAR' ,
    'NDPER'       '使用月份'      'L' 'ANLB'  'NDPER' ,
    'AFABG'       '折旧开始日期'  'L' 'ANLB'  'AFABG' ,
    'SCHRW'       '残值'          'L' 'ANLB'  'SCHRW' ,
    'SCHRW_PROZ'  '残值率'        'L' 'ANLB'  'SCHRW_PROZ',
    'KANSW'       '购置价值'      'L' ''  '' ,
    'KNAFA'       '以前年度折旧'  'L' ''  '' ,
    'NAFAG'      '本年已计提折旧' 'L' ''  '' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form importing_asset_data
*&---------------------------------------------------------------------**
FORM importing_asset_data .

  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE i888(sabapdocu) WITH '请至少选择一行数据!'.
  ELSE.


    LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
      " 填充BAPI 数据。
      CLEAR:ls_key,ls_data,ls_datax,ls_post,ls_postx,ls_time,ls_timex,ls_allo,
            ls_allox, ls_origin, ls_originx, ls_inven, ls_invenx.
      REFRESH:lt_depareas, lt_depareasx, lt_cvalues, lt_pvalues,lt_trans, lt_return.

      ls_key-companycode            = gs_out-bukrs.
      ls_key-asset                  = gs_out-anln1.

      ls_data-assetclass            = gs_out-anlkl.
      ls_data-descript              = gs_out-txt50.
      ls_data-descript2             = gs_out-txa50.
      ls_data-serial_no             = gs_out-sernr.
      ls_data-invent_no             = gs_out-invnr.
      ls_data-main_descript         = gs_out-anlhtxt.

      ls_datax-assetclass           = 'X'.
      ls_datax-descript             = 'X'.
      ls_datax-descript2            = 'X'.
      ls_datax-serial_no            = 'X'.
      ls_datax-invent_no            = 'X'.
      ls_datax-main_descript        = 'X'.

      ls_post-cap_date              = gs_out-aktiv.
      ls_postx-cap_date             = 'X'.


      ls_time-costcenter            = gs_out-kostl."成本中心
      ls_time-intern_ord            = gs_out-caufn."内部订单
      ls_timex-costcenter           = 'X'."成本中心
      ls_timex-intern_ord           = 'X'."内部订单


      ls_allo-evalgroup1            = gs_out-ord41. "评审小组1
      ls_allox-evalgroup1           = 'X'."评审小组1


      ls_origin-vendor_no           = gs_out-lifnr.
      ls_originx-vendor_no          = 'X'.

      lt_depareas-area              = '01'.
      lt_depareas-dep_key           = gs_out-afasl."折旧码
      lt_depareas-ulife_yrs         = gs_out-ndjar."使用年限
      lt_depareas-ulife_prds        = gs_out-ndper."使用月份
      lt_depareas-odep_start_date   = gs_out-afabg."折旧开始日期
      lt_depareas-scrapvalue        = gs_out-schrw."残值
      lt_depareas-scrapvalue_prctg  = gs_out-schrw_proz."残值率
      APPEND lt_depareas.
      CLEAR  lt_depareas.

      lt_depareasx-area             = '01'.
      lt_depareasx-dep_key          = 'X'."
      lt_depareasx-ulife_yrs        = 'X'."
      lt_depareasx-ulife_prds       = 'X'."
      lt_depareasx-odep_start_date  = 'X'."
      lt_depareasx-scrapvalue       = 'X'.
      lt_depareasx-scrapvalue_prctg = 'X'.
      APPEND lt_depareasx.
      CLEAR  lt_depareasx.


      SELECT SINGLE datum INTO @DATA(lv_datum) FROM t093c WHERE bukrs = @gs_out-bukrs.
      IF lv_datum+4(4) = '1231'. " 年末导入
        lt_cvalues-fisc_year  = sy-datum(4) + 1."财年
        lt_cvalues-area       = '01'.
        lt_cvalues-acq_value  =  gs_out-kansw."购置价值
        lt_cvalues-ord_dep    =  gs_out-knafa * ( -1 )."以前年度折旧
        APPEND lt_cvalues.
        CLEAR  lt_cvalues.

      ELSE. " 不是年末导入
        " 判断资产数据的资本化日期前四位和T093C-LGJAHR（当年会计年度）是否相等
        " CUMULATEDVALUES
        IF  gs_out-aktiv(4) <> sy-datum(4). " 资本化日期
          lt_cvalues-fisc_year  = sy-datum(4)."财年
          lt_cvalues-area       = '01'.
          lt_cvalues-acq_value  =  gs_out-kansw."购置价值
          lt_cvalues-ord_dep    =  gs_out-knafa * ( -1 )."以前年度折旧
          APPEND lt_cvalues.
          CLEAR  lt_cvalues.
        ENDIF.

        "POSTEDVALUES
        IF  gs_out-nafag IS NOT INITIAL.
          lt_pvalues-fisc_year  = sy-datum(4)."财年
          lt_pvalues-area       = '01'.
          lt_pvalues-ord_dep    = gs_out-nafag * ( -1 ). "本年已计提折旧
          APPEND lt_pvalues.
          CLEAR  lt_pvalues.
        ENDIF.

        " TRANSACTION
        IF gs_out-aktiv(4) = sy-datum(4).
          lt_trans-fisc_year    = gs_out-aktiv(4)   ."  财年
          lt_trans-current_no   = 1   . "  会计年资产行项目的序号
          lt_trans-area         = '01'   ."  实际折旧范围
          lt_trans-valuedate    = gs_out-aktiv   ."  参考日期
          lt_trans-assettrtyp   = '100'    ."  资产交易类型
          lt_trans-amount       = gs_out-kansw   ."  过帐金额
          lt_trans-currency     = 'CNY' ."  货币码
          APPEND lt_trans.
          CLEAR  lt_trans.
        ENDIF.

      ENDIF.

      " 调用BAPI函数，用来创建资产数据
      IF gs_out-light <> '@5C@'.
        PERFORM: creat_data.
      ENDIF.
      " 判断返回值，判断是否创建成功
      LOOP AT lt_return WHERE type CA 'AXE' . " 返回消息中含有AXE说明创建失败。
        gs_out-light = '@5C@'. " 失败返回红色警告灯
        gs_out-message = lt_return-message && '，' && gs_out-message. " 将错误消息传入前台
      ENDLOOP.
      " 如过返回了资产编号跟子编号，说明成功。
      IF gs_out-anln1 IS NOT INITIAL AND gs_out-anln2 IS NOT INITIAL.
        gs_out-light = '@5B@'." 成功返回绿色灯
        gs_out-message = '资产编号'  && gs_out-anln1 && '-' && gs_out-anln2  && '创建成功!'. " 返回消息，含有编号跟子编号。
        " 成功需要进行commit.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        " 失败进行回滚数据。
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
      "数据写回内表。
      MODIFY gt_out FROM gs_out.
      CLEAR: gs_out,lt_return[],lt_return.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREAT_DATA
*&---------------------------------------------------------------------*
FORM creat_data .
  " 创建资产。

  CALL FUNCTION 'BAPI_FIXEDASSET_OVRTAKE_CREATE'
    EXPORTING
      key                 = ls_key
      generaldata         = ls_data
      generaldatax        = ls_datax
      inventory           = ls_inven
      inventoryx          = ls_invenx
      postinginformation  = ls_post
      postinginformationx = ls_postx
      timedependentdata   = ls_time
      timedependentdatax  = ls_timex
      allocations         = ls_allo
      allocationsx        = ls_allox
      origin              = ls_origin
      originx             = ls_originx
    IMPORTING
      asset               = gs_out-anln1
      subnumber           = gs_out-anln2
    TABLES
      depreciationareas   = lt_depareas
      depreciationareasx  = lt_depareasx
      cumulatedvalues     = lt_cvalues
      postedvalues        = lt_pvalues
      transactions        = lt_trans
      return              = lt_return.

ENDFORM.
