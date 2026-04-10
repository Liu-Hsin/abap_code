*&*********************************************************************
*& program name:              [ZFIE332]
*& module name :              []
*& apply author:              []
*& author:                    [LiuX]
*& started on:                [03.04.2026 17:29:16]
*& transaction:               []
*& program type:              [REPORT ]
*& transfer requests:         []
*& program description :      [批量取消发票]
*&*&*******************************************************************
*& revision log                                                       *
*&                                                                    *
*& log   date                 author   description                    *
*& ----  -------------------  -------  -----------                    *
*& v1.0  03.04.2026 17:29:20  liux     初稿                           *
*&*********************************************************************
report  zfie332  message-id zfi001.
data:
  begin of gs_sel,
    vbeln type vbeln_vf,
    belnr type re_belnr,
    datum type datum,
  end of gs_sel,
  begin of gs_data,
    sel,
    doc      type re_belnr,
    bukrs    type bukrs,
    partner  type bu_partner,
    budat    type budat,
    rvdat    type rvdat_d,
    dmbtr    type dmbtr,
    waers    type waerk,
    bstnk_vf type bstkd,
    msgty    type msgty,
    msgtx    type msgtx,
  end of gs_data,
  gt_data like table of gs_data.


selection-screen begin of block bl1 with frame title text-bl1 .

  parameters p_miro no-display Default 'X'.
  parameters p_bill no-display.

  select-options:
    s_vbeln for gs_sel-vbeln modif id m1,
    s_belnr for gs_sel-belnr modif id m2.
  parameters:
    p_datum like gs_sel-datum .

selection-screen end of block bl1.
selection-screen function key 1.

initialization.
  if sy-tcode = 'ZFIE332A'.
    p_bill = abap_false.
    p_miro = abap_true.
  elseif sy-tcode = 'ZFIE332B'.
    p_bill = abap_true.
    p_miro = abap_false.
  endif.

at selection-screen output.
  if p_miro = abap_true.
    set titlebar 'START' with text-001.
  else.
    set titlebar 'START' with text-002.
  endif.

  loop at screen.
    if p_miro  = abap_true and screen-group1 = 'M1'.
      screen-active = '0'.
    endif.
    if p_bill = abap_true and screen-group1 = 'M2'.
      screen-active = '0'.
    endif.
    if screen-name = 'P_DATUM'.
      screen-required = '2'.
    endif.
    modify screen.
  endloop.

start-of-selection.
  perform: frm_main.
  perform: frm_alv_display.
*&---------------------------------------------------------------------*
*& Form frm_main
*&---------------------------------------------------------------------*
form frm_main.
  if p_datum is initial.
    message s055(00) display like 'E'.
    return.
  endif.

  if p_miro = abap_true.
    "RBKP.:发票号，公司代码，出票方，原过账日期，冲销日期，金额，币别
    select
      belnr as doc,
      bukrs,
      lifnr as partner,
      budat,
      rmwwr as dmbtr,
      waers
      from rbkp
      into corresponding fields of table @gt_data
      where belnr in @s_belnr
        and vgart = 'RD' "物流发票
        and stblg = @space.
  elseif p_bill = abap_true.
    "BILLING：发票号，公司代码，客户，原过账日期，冲销日期，金额，币别，客户参考
    select
      vbeln as doc,
      bukrs,
      kunag as partner,
      fkdat as budat,
      netwr as dmbtr,
      waerk as waers,
      bstnk_vf
      from vbrk
      into corresponding fields of table @gt_data
      where vbeln in @s_vbeln
        and fkart = 'F2' "发票 限制只能为发票
        and fksto = @space..
  endif.
  if gt_data[] is initial.
    message s041 display like 'E' .
    return.
  endif.
  loop at gt_data assigning field-symbol(<fs_data>).
    <fs_data>-rvdat = p_datum.
  endloop.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DISPLAY
*&---------------------------------------------------------------------*
form frm_alv_display .
  data: lv_repid        type  sy-repid,
        is_layout_lvc   type  lvc_s_layo,
        it_fieldcat_lvc type  lvc_t_fcat.

  check gt_data[] is not initial.

  perform:frm_alv_layout_fieldcat tables it_fieldcat_lvc changing  is_layout_lvc .

  lv_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program       = lv_repid
      is_layout_lvc            = is_layout_lvc
      it_fieldcat_lvc          = it_fieldcat_lvc
      i_callback_pf_status_set = 'FRM_ALV_PF_SET_STATUS'
      i_callback_user_command  = 'FRM_ALV_USER_COMMAND'
      i_default                = 'X'
      i_save                   = 'A'
    tables
      t_outtab                 = gt_data
    exceptions
      program_error            = 1
      others                   = 2.
endform.
*&---------------------------------------------------------------------*
*&      FORM  FRM_LAYOUT_FIELDCAT
*&---------------------------------------------------------------------*
form frm_alv_layout_fieldcat  tables   pt_fieldcat_lvc type  lvc_t_fcat
                          changing  is_layout_lvc type  lvc_s_layo.
  is_layout_lvc-cwidth_opt = 'X'.
  is_layout_lvc-zebra = 'X'.
  is_layout_lvc-box_fname = 'SEL'.
  is_layout_lvc-sel_mode = 'D'.

  perform: frm_alv_get_fieldcat tables pt_fieldcat_lvc.
endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_GET_FIELDCAT
*&---------------------------------------------------------------------*
form frm_alv_get_fieldcat  tables   pt_fieldcat_lvc type lvc_t_fcat.
  perform frm_alv_set_fieldcat tables pt_fieldcat_lvc using:
    'DOC'       'C' '发票号'       ''  ''  ,
    'BUKRS'     'C' '公司代码'     ''  ''  ,
    'PARTNER'   'L' '出票方'       'PARTNER'  'BUT000'  ,
    'BUDAT'     'C' '原过账日期' ''  ''  ,
    'RVDAT'     'C' '冲销日期'  ''  ''  ,
    'DMBTR'     'R' '金额'  ''  ''  ,
    'WAERS'     'C' '币别'  ''  ''  ,
    'BSTNK_VF'  'L' '客户参考'  'BSTNK_VF'  'VBRK'  ,
    'MSGTX'     'L' '消息'      'MESSAGE'  'BAPIRET2'  .

endform.
*&---------------------------------------------------------------------*
*&      Form  frm_alv_set_fieldcat
*&---------------------------------------------------------------------*
form frm_alv_set_fieldcat  tables pt_fieldcat_lvc type  lvc_t_fcat
                            using  p_fieldname     type lvc_s_fcat-fieldname
                                   p_just          type lvc_s_fcat-just
                                   p_coltext       type any
                                   p_ref_field     type lvc_s_fcat-ref_field
                                   p_ref_table     type lvc_s_fcat-ref_table.
  data: ls_fieldcat type lvc_s_fcat.
  ls_fieldcat = value #( tabname   = 'GT_DATA'
                          fieldname = p_fieldname
                          just      = p_just
                          coltext   = p_coltext
                          ref_field = p_ref_field
                          ref_table = p_ref_table ).

  if p_miro = abap_true.
    if p_fieldname = 'PARTNER'.
      ls_fieldcat-coltext = '出票方'.
    endif.
    if p_fieldname = 'BSTNK_VF'.
      return.
    endif.
    if p_fieldname = 'DOC'.
      ls_fieldcat-ref_field = 'BELNR'.
      ls_fieldcat-ref_table = 'RBKP'.
      ls_fieldcat-hotspot = 'X'.
    endif.
  else.
    if p_fieldname = 'PARTNER'.
      ls_fieldcat-coltext = '客户'.
    endif.
    if p_fieldname = 'DOC'.
      ls_fieldcat-ref_field = 'VBELN'.
      ls_fieldcat-ref_table = 'VBRK'.
      ls_fieldcat-hotspot = 'X'.
    endif.
  endif.

  append ls_fieldcat to pt_fieldcat_lvc.
endform.
*--------------------------------------------------------------------*
* FORM  FRM_ALV_PF_SET_STATUS
*--------------------------------------------------------------------*
form frm_alv_pf_set_status changing pt_exclude type kkblo_t_extab.
  if p_miro = abap_true.
    set titlebar 'START' with  text-001.
  else.
    set titlebar 'START' with  text-002.
  endif.
  set pf-status 'STANDARD'.

endform.
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_USER_COMMAND
*&---------------------------------------------------------------------*
form frm_alv_user_command using p_ucomm like sy-ucomm
                            p_selfield type slis_selfield .
  data: lr_grid type ref to cl_gui_alv_grid.
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    importing
      e_grid = lr_grid.
  call method lr_grid->check_changed_data.

  case p_ucomm.
    when '&IC1'.
      case p_selfield-fieldname.
        when 'DOC'.
          perform:frm_read_doc using p_selfield-value.
        when others.
          message '无效的功能码' type 'S' display like 'W'.
      endcase.
    when 'ZCANC'.
      read table gt_data transporting no fields with key  sel = 'X'.
      if sy-subrc <> 0.
        message s021 display like 'E'.
        return.
      endif.
      perform:frm_rev_doc.
    when others.
  endcase.

  p_selfield-refresh = 'X'.
  p_selfield-col_stable = 'X'.
  p_selfield-row_stable = 'X'.
endform.
*&---------------------------------------------------------------------*
*& Form frm_rev_doc
*&---------------------------------------------------------------------*
form frm_rev_doc .

  loop at gt_data assigning field-symbol(<fs_data>) where sel = abap_true and msgty <> 'S'.

    case abap_true.
      when p_miro.
        perform: frm_rev_miro changing <fs_data>.
      when p_bill.
        perform: frm_rev_bill changing <fs_data>.
      when others.
    endcase.

  endloop.

endform.
*&---------------------------------------------------------------------*
*& Form frm_rev_miro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_rev_miro changing cs_data like gs_data .


  data: lt_return                    type standard table of bapiret2,
        lv_invoicedocnumber_reversal type bapi_incinv_fld-inv_doc_no,
        lv_fiscalyear_reversal       type bapi_incinv_fld-fisc_year,
        lv_postingdate               type budat.

  select single belnr,gjahr into @data(ls_rbkp) from rbkp
    where belnr = @cs_data-doc
      and stblg = ''.
  if sy-subrc ne 0.
    cs_data-msgty = 'E'.
    cs_data-msgtx = |{ icon_red_light }发票校验凭证{ cs_data-doc }不存在或已被冲销,检查输入|.
    return.
  endif.

  lv_postingdate = cs_data-rvdat.

  call function 'BAPI_INCOMINGINVOICE_CANCEL'
    exporting
      invoicedocnumber          = ls_rbkp-belnr
      fiscalyear                = ls_rbkp-gjahr
      reasonreversal            = '02'
      postingdate               = lv_postingdate
    importing
      invoicedocnumber_reversal = lv_invoicedocnumber_reversal
      fiscalyear_reversal       = lv_fiscalyear_reversal
    tables
      return                    = lt_return.

  if lv_invoicedocnumber_reversal is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    cs_data-msgty = 'S'.
    cs_data-msgtx = |{ icon_green_light }发票取消成功|.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    loop at lt_return into data(ls_return) where type ca 'AEX'.
      cs_data-msgtx = |{ cs_data-msgtx }{ ls_return-message };|.
    endloop.
    cs_data-msgty = 'E'.
    cs_data-msgtx = |{ icon_red_light }取消失败:{ cs_data-msgtx }|.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form frm_rev_bill
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form frm_rev_bill changing cs_data like gs_data.

  data: lv_billingdate type budat.
  data: lt_return  type standard table of bapireturn1,
        lt_success type standard table of bapivbrksuccess.

  select count(*) from vbrk where vbeln = @cs_data-doc and fksto = ''.
  if sy-subrc ne 0.
    cs_data-msgtx = |{ icon_red_light }销售发票{ cs_data-doc }不存在或已取消,检查输入|.
    cs_data-msgty = 'E'.
    return.
  endif.

  lv_billingdate = cs_data-rvdat.

  call function 'BAPI_BILLINGDOC_CANCEL1'
    exporting
      billingdocument = cs_data-doc
      billingdate     = lv_billingdate
    tables
      return          = lt_return
      success         = lt_success.
  loop at lt_return into data(ls_return) where type ca 'AEX'.
    cs_data-msgtx = |{ cs_data-msgtx }{ ls_return-message };|.
  endloop.
  if sy-subrc ne 0.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    cs_data-msgty = 'S'.
    cs_data-msgtx = |{ icon_green_light }发票取消成功|.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    cs_data-msgty    = 'E'.
    cs_data-msgtx = |{ icon_red_light }取消失败:{ cs_data-msgtx }|.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form frm_read_doc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_SELFIELD_VALUE
*&---------------------------------------------------------------------*
form frm_read_doc  using    p_value.
  data tcode like tstc-tcode.

  check p_value is not initial.

  case abap_true.
    when p_miro.
      tcode = 'MIR4'.
      set parameter id 'RBN' field p_value.
    when p_bill.
      tcode = 'VF03'.
      set parameter id 'VF' field p_value.
    when others.
  endcase.
  call function 'AUTH_CHECK_TCODE'
    exporting
      tcode                          = tcode
    exceptions
      parameter_error                = 1
      transaction_not_found          = 2
      transaction_locked             = 3
      transaction_is_menu            = 4
      menu_via_parameter_transaction = 5
      not_authorized                 = 6
      others                         = 7.
  if sy-subrc <> 0.
    message id sy-msgid type 'S' number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    return.
  endif.
  call transaction tcode and skip first screen.
endform.