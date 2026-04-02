*&*********************************************************************
*& program name:              [ZMME366]
*& module name :              [MM]
*& apply author:              [ZhouZL]
*& author:                    [LiuX]
*& started on:                [23.03.2026 16:11:10]
*& transaction:               [ZMME366]
*& program type:              [REPORT ]
*& transfer requests:         [S4DK904765]
*& program description :      [批量取消poso]
*&*&*******************************************************************
*& revision log                                                       *
*&                                                                    *
*& log   date                 author   description                    *
*& ----  -------------------  -------  -----------                    *
*& v1.0  23.03.2026 16:11:14  liux     初稿                           *
*&*********************************************************************
report  zmme366  message-id zmm001.

class lcl_types definition final.
  public section.
    types:ty_doctype type char2.
    types:
      begin of ty_doc_list,
        doctype type ty_doctype,
        docname type ty_doctype,
        indexno type syindex, "顺序号,决定一键冲销时的顺序
      end of ty_doc_list.
    types:
      begin of ty_select_param,
        vbeln type range of vbak-vbeln,
        ebeln type range of ekko-ebeln,
        ernam type range of vbak-ernam,
        stdat type datum,
        so    type abap_bool,
        po    type abap_bool,
      end of ty_select_param.
    types:
      begin of ty_so_data,
        stdat    type rvdat_d,
        vbeln_va type vbak-vbeln,
        posnr_va type vbap-posnr,
        vkorg    type vbak-vkorg,
        vtext    type tvkot-vtext,
        kunnr    type vbak-kunnr,
        name1    type kna1-name1,
        vbeln_vl type lips-vbeln,
        posnr_vl type lips-posnr,
        wbsta    type lips-wbsta,
        wadat    type likp-wadat,
        matnr    type vbap-matnr,
        mblnr    type matdoc-mblnr,
        zeile    type matdoc-zeile,
        vbeln_vf type vbrk-vbeln,
        fkdat    type vbrk-fkdat,
        vin      type objk-sernr,
        msgty    type bapiret2-type,
        msgtx    type bapiret2-message,
      end of ty_so_data.
    types:
      begin of ty_po_data,
        stdat type rvdat_d,
        ebeln type ekko-ebeln,
        ebelp type ekpo-ebelp,
        bukrs type ekko-bukrs,
        butxt type t001-butxt,
        lifnr type ekko-lifnr,
        name1 type lfa1-name1,
        budat type matdoc-budat,
        matnr type ekpo-matnr,
        mblnr type matdoc-mblnr,
        zeile type matdoc-zeile,
        belnr type rbkp-belnr,
        ivdat type rbkp-budat,
        vin   type objk-sernr,
        msgty type bapiret2-type,
        msgtx type bapiret2-message,
      end of ty_po_data.

    types: tt_doc_list type table of ty_doc_list.

    class-data:
      begin of ms_sel_opt,
        vbeln type vbak-vbeln,
        ebeln type ekko-ebeln,
        ernam type vbak-ernam,
        stdat type sy-datum,
      end of ms_sel_opt,
      ok_code type syucomm.
    constants:
      doctyp_so      type ty_doctype value 'SO',
      doctyp_po      type ty_doctype value 'PO',
      doctyp_dn      type ty_doctype value 'DN',
      doctyp_gi      type ty_doctype value 'GI',
      doctyp_gr      type ty_doctype value 'GR',
      doctyp_all     type ty_doctype value 'AL',
      container_name type c length 10 value 'CC_ALV',
      title_bar      type gui_title  value 'START',
      gui_status     type gui_status value 'STANDARD',
      cancel_all     type ui_func  value 'ZCANC_AL',
      cancel_gr      type ui_func  value 'ZCANC_GR',
      cancel_po      type ui_func  value 'ZCANC_PO',
      cancel_gi      type ui_func  value 'ZCANC_GI',
      cancel_dn      type ui_func  value 'ZCANC_DN',
      cancel_so      type ui_func  value 'ZCANC_SO',
      back           type ui_func  value '&F03',
      exit           type ui_func  value '&F15',
      canc           type ui_func  value '&F12'.

endclass.

class lcx_exception definition inheriting from cx_static_check.
  public section.
    methods: constructor
      importing
        text type any optional,
      get_longtext redefinition.
  private section.
    data: mv_msg type string.
endclass.

interface lif_cancel_doc.
  methods:
    cancel_before_check
      raising lcx_exception,
    cancel_doc
      importing i_data        type ref to data
      returning value(rs_ret) type bapiret2
      raising   lcx_exception.
endinterface.

class lcl_cancel_factory definition.
  public section.
    class-methods:
      create_handler_class
        importing
          iv_doctype        type lcl_types=>ty_doctype
        returning
          value(ro_handler) type ref to lif_cancel_doc
        raising
          lcx_exception.

endclass.

class lcl_cancel_po definition final.
  public section.
    interfaces lif_cancel_doc.
  private section.
    data ms_po type lcl_types=>ty_po_data.
endclass.
class lcl_cancel_gr definition final.
  public section.
    interfaces lif_cancel_doc.
  private section.
    data ms_po type lcl_types=>ty_po_data.
endclass.
class lcl_cancel_so definition final.
  public section.
    interfaces lif_cancel_doc.
  private section.
    data ms_so type lcl_types=>ty_so_data.
endclass.
class lcl_cancel_gi definition final.
  public section.
    interfaces lif_cancel_doc.
  private section.
    data ms_so type lcl_types=>ty_so_data.
endclass.
class lcl_cancel_dn definition final.
  public section.
    interfaces lif_cancel_doc.
  private section.
    data ms_so type lcl_types=>ty_so_data.
endclass.

class lcl_public_tools definition final.
  public section.
    class-methods:
      read_so
        importing i_doc          type re_belnr
        returning value(rt_vbap) type vbap_t,
      read_po
        importing i_doc          type re_belnr
        returning value(rt_ekpo) type ekpo_tty,
      read_invoice
        importing i_doc          type re_belnr
        returning value(rs_rbkp) type rbkp,
      read_goods_receipt
        importing i_doc            type mblnr
        returning value(rs_matdoc) type matdoc,
      read_billing
        importing i_doc          type vbeln_vf
        returning value(rs_vbrk) type vbrk,
      read_goods_issue
        importing i_doc            type mblnr
        returning value(rs_matdoc) type matdoc,
      read_delivery_notes
        importing i_doc          type vbeln_vl
        returning value(rs_lips) type lips,
      read_sales_org
        returning value(rt_tvkot) type le_t_dlv_tvkot,
      read_company
        returning value(rt_t001) type t_t001.
endclass.

class lcl_model definition.
  public section.
    data:
      mo_data    type ref to data.
    methods get_data_from_db
      importing is_param type lcl_types=>ty_select_param
      raising   lcx_exception.

    methods get_data_ref
      returning value(ro_data) type ref to data.

    methods cancel
      importing iv_doctype type lcl_types=>ty_doctype
                it_rows    type lvc_t_row
      raising   lcx_exception.

  private section.
    data: mv_doctype  type lcl_types=>ty_doctype,
          mt_doc_list type lcl_types=>tt_doc_list,
          mt_so_data  type standard table of lcl_types=>ty_so_data with key stdat vbeln_va posnr_va,
          mt_po_data  type standard table of lcl_types=>ty_po_data with key stdat ebeln ebelp,
          ms_param    type lcl_types=>ty_select_param.
    methods param_check
      raising lcx_exception.
    methods get_so_data
      raising lcx_exception.
    methods get_po_data
      raising lcx_exception.
    methods get_doc_list.
endclass.

class lcl_view definition.
  public section.
    methods set_init_title
      importing iv_doctype type lcl_types=>ty_doctype.
    methods set_screen_required.
    methods get_selected_rows
      returning value(et_rows) type lvc_t_row.
    methods set_model
      importing io_model type ref to lcl_model.
    methods container_init
      importing iv_gui_status type gui_status.
    methods function_code_event
      changing cv_ucomm type syucomm.
    methods display_alv
      importing io_alvdata type ref to data.

  private section.
    data:
      mv_doctype   type lcl_types=>ty_doctype,
      mo_model     type ref to lcl_model,
      mo_alv       type ref to cl_gui_alv_grid,
      mo_container type ref to cl_gui_custom_container,
      mt_field     type lvc_t_fcat.
    methods:
      set_pf_status
        importing iv_gui_status type gui_status,
      set_fieldcat
        importing io_data type ref to data.
endclass.


class lcl_controller definition create private.
  public section.
    class-methods
      get_instance returning value(ro_instance) type ref to lcl_controller.
    methods:
      screen_output
        importing
          iv_so type abap_bool
          iv_po type abap_bool,
      run
        importing
          is_param type lcl_types=>ty_select_param,

      container_init,

      container_event.

  private section.
    class-data:
      mo_instance type ref to lcl_controller.
    data:
      mo_model type ref to lcl_model,
      mo_view  type ref to lcl_view.
    methods:
      call_screen,
      constructor.
endclass.


selection-screen begin of block bl1 with frame title text-bl1 .
  parameters:
    p_so radiobutton group g user-command click default 'X',
    p_po radiobutton group g.
selection-screen end of block bl1.

selection-screen begin of block bl2 with frame title text-bl2 .
  select-options:
    s_vbeln for lcl_types=>ms_sel_opt-vbeln modif id m1,
    s_ebeln for lcl_types=>ms_sel_opt-ebeln modif id m2,
    s_ernam for lcl_types=>ms_sel_opt-ernam modif id m default sy-uname.
  parameters
    p_stdat like lcl_types=>ms_sel_opt-stdat modif id m.
selection-screen end of block bl2.

at selection-screen output.
  lcl_controller=>get_instance( )->screen_output( iv_so = p_so iv_po = p_po ).

start-of-selection.
  lcl_controller=>get_instance( )->run( value #( so     = p_so
                                                  po     = p_po
                                                  stdat  = p_stdat
                                                  vbeln  = s_vbeln[]
                                                  ebeln  = s_ebeln[]
                                                  ernam  = s_ernam[] ) ).


*&---------------------------------------------------------------------*
*& Class (Implementation) lcx_exception
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcx_exception implementation.
  method constructor.
    super->constructor( ).
    mv_msg = text.
  endmethod.
  method get_longtext.
    result = mv_msg.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_controller
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_controller implementation.
  method get_instance.
    if mo_instance is not bound.
      mo_instance = new lcl_controller( ).
    endif.
    ro_instance = mo_instance.
  endmethod.
  method  constructor.
    mo_model = new lcl_model( ).
    mo_view  = new lcl_view( ).
  endmethod.
  method: screen_output.
    mo_view->set_init_title( switch #( iv_so when abap_true then lcl_types=>doctyp_so else lcl_types=>doctyp_po ) ).
    mo_view->set_screen_required( ).
  endmethod.

  method run.
    try.
        mo_model->get_data_from_db( is_param ).
      catch lcx_exception into data(lo_exp).
        message lo_exp->get_longtext( ) type 'S' display like 'E'.
        return.
    endtry.

    call_screen( ).
  endmethod.

  method call_screen.
    call screen 9000.
  endmethod.

  method container_init.
    mo_view->set_model( mo_model ).
    mo_view->container_init( lcl_types=>gui_status ).
  endmethod.

  method container_event.
    data l_ok_code type syucomm.
    l_ok_code = lcl_types=>ok_code.
    clear lcl_types=>ok_code.

    check l_ok_code is not initial.

    case l_ok_code(5).
      when 'ZCANC'.
        data(et_rows) = mo_view->get_selected_rows( ).
        try.
            mo_model->cancel( iv_doctype = l_ok_code+6(2) it_rows = et_rows ).
          catch lcx_exception into data(lo_cx).
            message lo_cx->get_longtext( ) type 'S' display like 'E'.
        endtry.
      when lcl_types=>back or lcl_types=>exit or lcl_types=>canc .
        clear:mo_model,mo_view.
        set screen 0.
        leave to screen 0.
      when others.

        mo_view->function_code_event( changing cv_ucomm =  l_ok_code ).

    endcase.

  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_model
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_model implementation.
  method get_data_from_db.
    clear ms_param.

    ms_param = is_param.

    param_check( ).

    mv_doctype = cond #( when ms_param-po = abap_true then lcl_types=>doctyp_po else lcl_types=>doctyp_so ).

    if ms_param-so = abap_true.
      get_so_data( ).      "销售订单
    elseif ms_param-po = abap_true.
      get_po_data( ).     "采购订单
    endif.

    if mo_data is not bound.
      raise exception type lcx_exception
        exporting
          text = '没有符合条件的数据'(M01).
    endif.
  endmethod.
  method param_check.
    "字段均是必输字段
    if ( ms_param-so = abap_true and ms_param-vbeln[] is initial ) or
       ( ms_param-po = abap_true and ms_param-ebeln[] is initial ) or
       ( ms_param-ernam[] is initial ) or
       ( ms_param-stdat is initial ).
      raise exception type lcx_exception
        exporting
          text = '请输入必输值'(M02).
    endif.
  endmethod.
  method get_so_data.
    "SO 基本单据 根据销售订单获取发货物料凭证没有被取消的销售流程单据
    " SO 流程：VBAP → LIPS → MATDOC → VBRP/VBRK（发票）
    select
      a~vkorg_ana as vkorg,
      a~kunnr_ana as kunnr,
      a~vbeln     as vbeln_va,  " 销售订单号
      a~posnr     as posnr_va,
      a~matnr,
      b~vbeln     as vbeln_vl,  " 交货单
      b~posnr     as posnr_vl,
      b~wbsta     as wbsta,
      c~budat     as wadat,
      c~mblnr,                   " 物料凭证号
      c~zeile
    from vbap as a
    left join lips as b on a~vbeln = b~vgbel        "左关联，有可能该销售订单没有交货单跟物料凭证。
                       and a~posnr = b~vgpos
    left join matdoc as c on b~vbeln = c~vbeln_im
                         and b~posnr = c~vbelp_im
                         and c~smbln = @space
                         and c~cancelled = @space
    where a~vbtyp_ana = 'C' " C 表示订单
      and a~vbeln in @ms_param-vbeln[]
      and a~ernam in @ms_param-ernam[]
      and a~abgru = @space
      into corresponding fields of table @mt_so_data.
    if sy-subrc = 0 and mt_so_data[] is not initial.

      "销售组织
      data(rt_tvkot) = lcl_public_tools=>read_sales_org( ).

      "销售发票
      select a~vbeln,a~aubel,a~aupos,b~fkdat into table @data(lt_vbrk)
        from vbrp as a inner join vbrk as b on a~vbeln = b~vbeln
        for all entries in @mt_so_data
        where a~aubel = @mt_so_data-vbeln_va
          and a~aupos = @mt_so_data-posnr_va
          and b~fkart = 'F2' "发票 限制只能为发票
          and b~fksto = @space.

      select kunnr,name1
        from kna1
        into table @data(lt_kna1).

      select a~sernr,b~lief_nr,b~posnr
        from objk as a
       inner join ser01 as b on a~obknr = b~obknr
       inner join @mt_so_data as z on z~vbeln_vl = b~lief_nr and z~posnr_vl = b~posnr
       into table @data(lt_vin).

      sort lt_vbrk by aubel aupos.
      sort lt_kna1 by kunnr.
      sort lt_vin by lief_nr posnr.

      loop at mt_so_data assigning field-symbol(<fs_so>).

        <fs_so>-stdat = ms_param-stdat."用户输入字段，后续需要支持编辑

        read table rt_tvkot assigning field-symbol(<fs_tvkot>) with key vkorg = <fs_so>-vkorg binary search.
        if sy-subrc = 0.
          <fs_so>-vtext = <fs_tvkot>-vtext.
        endif.
        read table lt_vbrk assigning field-symbol(<fs_vbrk>) with key aubel = <fs_so>-vbeln_va aupos = <fs_so>-posnr_va binary search.
        if sy-subrc = 0.
          <fs_so>-vbeln_vf = <fs_vbrk>-vbeln.
          <fs_so>-fkdat = <fs_vbrk>-fkdat.
          <fs_so>-msgty = 'E'.
          <fs_so>-msgtx = icon_red_light && text-m06.
        endif.
        read table lt_kna1 assigning field-symbol(<fs_kna1>) with key kunnr = <fs_so>-kunnr binary search.
        if sy-subrc = 0.
          <fs_so>-name1 = <fs_kna1>-name1.
        endif.
        read table lt_vin assigning field-symbol(<fs_vin>) with key lief_nr = <fs_so>-vbeln_vl posnr = <fs_so>-posnr_vl binary search.
        if sy-subrc = 0.
          <fs_so>-vin = <fs_vin>-sernr.
        endif.
      endloop.
      get reference of mt_so_data into mo_data.
    endif.
  endmethod.
  method get_po_data.
    "PO 流程：EKPO → MATDOC → RSEG/RBKP（发票）
    "根据物料跟采购订单获取没有被删除，且收货物料凭证不是取消及被取消的单据.
    select
      a~bukrs,
      a~lifnr,
      b~ebeln,
      b~ebelp,
      b~matnr,
      c~budat,
      c~mblnr,
      c~zeile
    from ekko as a
    inner join ekpo as b on a~ebeln = b~ebeln
    left join matdoc as c on b~ebeln = c~ebeln
                         and b~ebelp = c~ebelp
                         and c~bwart = '101' "收货物料凭证
                         and c~smbln = @space
                         and c~cancelled = @space
    where a~ebeln in @ms_param-ebeln[]
      and a~ernam in @ms_param-ernam[]
      and b~loekz = @space
      and a~bstyp = 'F' " F 采购订单.
      into corresponding fields of table @mt_po_data.

    if sy-subrc = 0 and mt_po_data[] is not initial.
      "发票
      select a~belnr,a~ebeln,a~ebelp,b~budat into table @data(lt_rbkp)
        from rseg as a inner join rbkp as b on a~belnr = b~belnr
      for all entries in @mt_po_data
       where a~ebeln = @mt_po_data-ebeln
         and a~ebelp = @mt_po_data-ebelp
         and b~vgart = 'RD' "物流发票
         and b~stblg = @space.

      select lifnr,name1
        from lfa1
       where substring( lifnr,1,1 ) <> 'E' "E开头是员工，排除
        into table @data(lt_lfa1).


      select a~sernr,b~mblnr,b~zeile
        from objk as a
       inner join ser03 as b on a~obknr = b~obknr
       inner join @mt_po_data as z on z~mblnr = b~mblnr and z~zeile = b~zeile
       into table @data(lt_vin).

      sort lt_rbkp by ebeln ebelp.
      sort lt_lfa1 by lifnr.
      sort lt_vin by mblnr zeile.

      data(rt_t001) = lcl_public_tools=>read_company( )."公司代码

      loop at mt_po_data assigning field-symbol(<fs_po>).

        <fs_po>-stdat = ms_param-stdat."用户输入字段，后续需要支持编辑
        read table rt_t001 assigning field-symbol(<fs_t001>) with key bukrs = <fs_po>-bukrs binary search.
        if sy-subrc = 0.
          <fs_po>-butxt = <fs_t001>-butxt.
        endif.
        read table lt_rbkp assigning field-symbol(<fs_rbkp>) with key ebeln = <fs_po>-ebeln ebelp = <fs_po>-ebelp binary search.
        if sy-subrc = 0 .
          <fs_po>-belnr = <fs_rbkp>-belnr.
          <fs_po>-ivdat = <fs_rbkp>-budat.
          <fs_po>-msgty = 'E'.
          <fs_po>-msgtx = icon_red_light && text-m04.
        endif.
        read table lt_lfa1 assigning field-symbol(<fs_lfa1>) with key lifnr = <fs_po>-lifnr binary search.
        if sy-subrc = 0.
          <fs_po>-name1 = <fs_lfa1>-name1.
        endif.
        read table lt_vin assigning field-symbol(<fs_vin>) with key mblnr = <fs_po>-mblnr zeile = <fs_po>-zeile binary search.
        if sy-subrc = 0.
          <fs_po>-vin = <fs_vin>-sernr.
        endif.
      endloop.
      get reference of mt_po_data into mo_data.
    endif.

  endmethod.

  method get_data_ref.
    ro_data = mo_data.
  endmethod.
  method get_doc_list.
    if mt_doc_list[] is initial.
      if mv_doctype = lcl_types=>doctyp_po.
        append initial line to mt_doc_list assigning field-symbol(<fs_doc_list>).
        <fs_doc_list>-doctype = lcl_types=>doctyp_po.
        <fs_doc_list>-docname = lcl_types=>doctyp_gr.
        <fs_doc_list>-indexno = 1."顺序号，最先执行
        append initial line to mt_doc_list assigning <fs_doc_list>.
        <fs_doc_list>-doctype = lcl_types=>doctyp_po.
        <fs_doc_list>-docname = lcl_types=>doctyp_po.
        <fs_doc_list>-indexno = 2."顺序号
      endif.
      if mv_doctype = lcl_types=>doctyp_so.
        append initial line to mt_doc_list assigning <fs_doc_list>.
        <fs_doc_list>-doctype = lcl_types=>doctyp_so.
        <fs_doc_list>-docname = lcl_types=>doctyp_gi.
        <fs_doc_list>-indexno = 1.
        append initial line to mt_doc_list assigning <fs_doc_list>.
        <fs_doc_list>-doctype = lcl_types=>doctyp_so.
        <fs_doc_list>-docname = lcl_types=>doctyp_dn.
        <fs_doc_list>-indexno = 2.
        append initial line to mt_doc_list assigning <fs_doc_list>.
        <fs_doc_list>-doctype = lcl_types=>doctyp_so.
        <fs_doc_list>-docname = lcl_types=>doctyp_so.
        <fs_doc_list>-indexno = 3.
      endif.
    endif.
  endmethod.

  method cancel.
    field-symbols <fs_tab> type standard table.
    data lo_data type ref to data.
    data lo_handler type ref to lif_cancel_doc.

    if lines( it_rows ) = 0.
      raise exception type lcx_exception
        exporting
          text = '请选择需要操作的行'(M09).
    endif.

    get_doc_list( )."需要处理的单据清单。
    sort mt_doc_list by indexno.

    assign mo_data->* to <fs_tab>.
    create data lo_data like line of <fs_tab>.

    loop at it_rows assigning field-symbol(<fs_rows>).
      "一行处理一次，数据引用方便传递参数
      read table <fs_tab> reference into lo_data index <fs_rows>-index.
      if sy-subrc = 0.
        if iv_doctype = lcl_types=>doctyp_all."一键冲销，根据单据顺序依次执行
          loop at mt_doc_list assigning field-symbol(<fs_doclist>).
            try.
                lo_handler = lcl_cancel_factory=>create_handler_class(
                  exporting
                    iv_doctype = <fs_doclist>-docname
                ).
                lo_handler->cancel_doc( lo_data ).
              catch lcx_exception into data(lo_cx).
                exit.
            endtry.
            clear lo_handler.
          endloop.
        else.
          "特定的单据，直接执行.
          try.
              lo_handler = lcl_cancel_factory=>create_handler_class(
                exporting
                  iv_doctype = iv_doctype
              ).
              lo_handler->cancel_doc( lo_data ).
            catch lcx_exception into lo_cx.
          endtry.
          clear lo_handler.
        endif.

        "执行结束，对结果赋值，没出现异常即成功.
        assign lo_data->* to field-symbol(<fs_wa>).
        assign component 'MSGTY' of structure <fs_wa> to field-symbol(<fs_msgty>).
        assign component 'MSGTX' of structure <fs_wa> to field-symbol(<fs_msgtx>).
        if lo_cx is bound.
          <fs_msgty> = 'E'.
          <fs_msgtx> = icon_red_light  && lo_cx->get_longtext( ).
          clear lo_cx.
        else.
          <fs_msgty> = 'S'.
          <fs_msgtx> = icon_green_light && '冲销成功'(M03).
        endif.
        unassign:<fs_msgty>,<fs_msgtx>.
      endif.

    endloop.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_view
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_view implementation.
  method: set_init_title.
    "根据用户的选择，展示不同的标题
    mv_doctype = iv_doctype.
    case mv_doctype.
      when lcl_types=>doctyp_so.
        set titlebar lcl_types=>title_bar with '销售'(001).
      when lcl_types=>doctyp_po.
        set titlebar lcl_types=>title_bar with '采购'(002).
      when others.
    endcase.
  endmethod.

  method set_screen_required.
    " 设置初始屏幕的显示
    loop at screen.
      if screen-group1 is not initial.
        screen-required = '2'. " 必输字段
      endif.
      if screen-group1 = 'M1' and mv_doctype = lcl_types=>doctyp_po.
        screen-active = '0'." PO 模式隐藏销售订单号
      endif.
      if screen-group1 = 'M2' and mv_doctype = lcl_types=>doctyp_so.
        screen-active = '0'. " SO 模式隐藏采购订单号
      endif.
      modify screen.
    endloop.
  endmethod.

  method get_selected_rows.
    mo_alv->get_selected_rows(
      importing
        et_index_rows = et_rows
    ).
  endmethod.

  method set_model.
    mo_model = io_model.
  endmethod.
  method container_init.

    set_pf_status(  iv_gui_status  ).

    display_alv( mo_model->get_data_ref( ) ).

  endmethod.

  method  function_code_event.
    mo_alv->set_function_code( changing c_ucomm = cv_ucomm ).
  endmethod.

  method display_alv.
    data lr_data type ref to data.
    field-symbols <fs> type standard table.
    if io_alvdata is bound.
      assign io_alvdata->* to <fs>.
    else.
      return.
    endif.
    if mo_container is not bound.
      mo_container = new cl_gui_custom_container( container_name = lcl_types=>container_name ).
    endif.

    if mo_alv is not bound.
      create data lr_data like line of <fs>.
      set_fieldcat( lr_data ).

      mo_alv = new cl_gui_alv_grid( i_parent  = mo_container ).

      mo_alv->set_table_for_first_display(
        exporting
          is_layout       = value lvc_s_layo( sel_mode = 'D' zebra = abap_true cwidth_opt = abap_true no_toolbar = abap_true )
          i_save          = abap_true
          i_default       = abap_true
        changing
          it_fieldcatalog = mt_field
          it_outtab       = <fs>
      ).
    else.
      mo_alv->refresh_table_display( is_stable = value #( row = abap_true col = abap_true ) ).
    endif.
  endmethod.

  method set_fieldcat.
    check mt_field[] is initial.

    assign io_data->* to field-symbol(<fs>).
    data(lr_data) = ref #( <fs> ).
    data(lo_struc) = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( lr_data ) ).
    data(lt_dfies) = cl_salv_data_descr=>read_structdescr( lo_struc ).

    loop at lt_dfies assigning field-symbol(<fs_dfies>).
      append initial line to mt_field assigning field-symbol(<fs_fieldcat>).
      <fs_fieldcat>-fieldname = <fs_dfies>-fieldname.
      <fs_fieldcat>-coltext   = <fs_dfies>-scrtext_l.
      <fs_fieldcat>-ref_field = <fs_dfies>-reffield.
      <fs_fieldcat>-ref_table = <fs_dfies>-reftable.
      <fs_fieldcat>-outputlen = <fs_dfies>-outputlen.
      case <fs_fieldcat>-fieldname.
        when 'EBELN' or 'VBELN_VA' or 'MBLNR' or 'VBELN_VL'.
          <fs_fieldcat>-emphasize = 'C500'.
*          <fs_fieldcat>-hotspot   = abap_true.
        when others.
      endcase.
    endloop.
  endmethod.

  method set_pf_status.
    data lt_exfunc type ui_functions.

    case mv_doctype.
      when lcl_types=>doctyp_po.
        append lcl_types=>cancel_gi to lt_exfunc.
        append lcl_types=>cancel_dn to lt_exfunc.
        append lcl_types=>cancel_so to lt_exfunc.
      when lcl_types=>doctyp_so.
        append lcl_types=>cancel_gr to lt_exfunc.
        append lcl_types=>cancel_po to lt_exfunc.
      when others.
    endcase.

    set pf-status iv_gui_status excluding lt_exfunc.

    set_init_title( mv_doctype ).

  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Module PBO_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module pbo_9000 output.
  lcl_controller=>get_instance( )->container_init( ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  PAI_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai_9000 input.
  lcl_controller=>get_instance( )->container_event( ).
endmodule.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_public_tools
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_public_tools implementation.
  method read_so.
    if i_doc is not initial.
      select vbeln,posnr from vbap
        into corresponding fields of table @rt_vbap
        where vbeln = @i_doc
          and abgru = @space.
    endif.
  endmethod.
  method read_po.
    if i_doc is not initial.
      select ebeln,ebelp from ekpo
       into corresponding fields of table @rt_ekpo
        where ebeln = @i_doc
          and loekz = @space.
    endif.
  endmethod.
  method read_invoice.
    if i_doc is not initial.
      select single belnr,gjahr from rbkp
        into corresponding fields of @rs_rbkp
        where belnr = @i_doc
          and vgart = 'RD' "物流发票
          and stblg = @space.
    endif.
  endmethod.
  method read_goods_receipt.
    if i_doc is not initial.
      select single mblnr,mjahr from matdoc
        into corresponding fields of @rs_matdoc
        where mblnr = @i_doc
          and smbln = @space
          and cancelled = @space.
    endif.
  endmethod.
  method read_billing.
    if i_doc is not initial.
      select single vbeln from vbrk
        into corresponding fields of @rs_vbrk
       where vbeln = @i_doc
         and fkart = 'F2' "发票
         and fksto = @space.
    endif.
  endmethod.
  method read_goods_issue.
    if i_doc is not initial.
      select single mblnr,mjahr from matdoc
        into corresponding fields of @rs_matdoc
       where mblnr = @i_doc
         and smbln = @space
         and cancelled = @space.
    endif.
  endmethod.
  method read_delivery_notes.
    if i_doc is not initial.
      select single vbeln from likp
        into corresponding fields of @rs_lips
        where vbeln = @i_doc.
    endif.
  endmethod.
  method read_sales_org.
    select vkorg,vtext
      from tvkot
      into corresponding fields of table @rt_tvkot
     where spras = @sy-langu.
    sort rt_tvkot by vkorg.
  endmethod.
  method read_company.
    select bukrs,butxt from t001
      into corresponding fields of table @rt_t001.
    sort rt_t001 by bukrs.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_cancel_factory
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_cancel_factory implementation.
  method create_handler_class.
    data lv_object type char35.
    lv_object = |LCL_CANCEL_{ iv_doctype }|.
    try.
        create object ro_handler type (lv_object).
      catch cx_root into data(lo_error).
        raise exception type lcx_exception
          exporting
            text = lo_error->get_longtext( ).
    endtry.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_cancel_po
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_cancel_po implementation.
  method lif_cancel_doc~cancel_before_check.
    if ms_po-belnr is not initial."检查发票是否被冲销
      if lcl_public_tools=>read_invoice( ms_po-belnr ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '请先找财务冲销MIRO凭证'(M04).
      endif.
    endif.
    if ms_po-mblnr is not initial."检查收货是否冲销
      if lcl_public_tools=>read_goods_receipt( ms_po-mblnr ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '收货凭证未冲销，不允许对采购订单标记删除'(M05).
      endif.
    endif.
  endmethod.

  method lif_cancel_doc~cancel_doc.
    assign i_data->* to field-symbol(<fs_data>).
    move-corresponding <fs_data> to ms_po.
    lif_cancel_doc~cancel_before_check( ).

    data(lt_ekpo) = lcl_public_tools=>read_po( ms_po-ebeln ).

    if lt_ekpo[] is initial.
      return.
    endif.

    data lt_return  type standard table of bapiret2.
    data lt_poitem  type standard table of bapimepoitem.
    data lt_poitemx type standard table of bapimepoitemx.
    loop at lt_ekpo into data(ls_ekpo).
      append initial line to lt_poitem assigning field-symbol(<fs_poitem>).
      <fs_poitem>-po_item     = ls_ekpo-ebelp.
      <fs_poitem>-delete_ind  = abap_true.
      append initial line to lt_poitemx assigning field-symbol(<fs_poitemx>).
      <fs_poitemx>-po_item    = ls_ekpo-ebelp.
      <fs_poitemx>-po_itemx   = abap_true.
      <fs_poitemx>-delete_ind = abap_true.
    endloop.

    call function 'BAPI_PO_CHANGE'
      exporting
        purchaseorder = ms_po-ebeln
      tables
        return        = lt_return
        poitem        = lt_poitem
        poitemx       = lt_poitemx.

    loop at lt_return into data(ls_return) where type ca 'AEX'.
      ms_po-msgtx = |{ ms_po-msgtx }{ ls_return-message };|.
    endloop.
    if sy-subrc ne 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      data(msgtx) = |采购凭证:{ ms_po-ebeln }标记删除失败:{ ms_po-msgtx }|.
      raise exception type lcx_exception
        exporting
          text = msgtx.
    endif.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_cancel_gr
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_cancel_gr implementation.
  method lif_cancel_doc~cancel_before_check.
    if ms_po-belnr is not initial."检查发票是否被冲销
      if lcl_public_tools=>read_invoice( ms_po-belnr ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '请先找财务冲销MIRO凭证'(M04).
      endif.
    endif.
  endmethod.

  method lif_cancel_doc~cancel_doc.
    assign i_data->* to field-symbol(<fs_data>).
    move-corresponding <fs_data> to ms_po.
    lif_cancel_doc~cancel_before_check( ).

    data(ls_matdoc) = lcl_public_tools=>read_goods_receipt( ms_po-mblnr ).

    if ls_matdoc is initial.
      return.
    endif.

    data goodsmvt_headret  type bapi2017_gm_head_ret.
    data lt_return         type standard table of bapiret2.

    call function 'BAPI_GOODSMVT_CANCEL'
      exporting
        materialdocument    = ls_matdoc-mblnr
        matdocumentyear     = ls_matdoc-mjahr
        goodsmvt_pstng_date = ms_po-stdat
      importing
        goodsmvt_headret    = goodsmvt_headret
      tables
        return              = lt_return.
    if goodsmvt_headret is not initial.

      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      loop at lt_return into data(ls_return) where type ca 'AEX'.
        ms_po-msgtx = |{ ms_po-msgtx }{ ls_return-message };|.
      endloop.
      data(msgtx) = |采购收货凭证{ ls_matdoc-mblnr }冲销失败:{ ms_po-msgtx }|.
      raise exception type lcx_exception
        exporting
          text = msgtx.
    endif.
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_cancel_so
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_cancel_so implementation.
  method lif_cancel_doc~cancel_before_check.
    if ms_so-vbeln_vf is not initial.
      if lcl_public_tools=>read_billing( ms_so-vbeln_vf ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '请先找财务冲销BILLING'(M06).
      endif.
    endif.
    if ms_so-mblnr is not initial.
      if lcl_public_tools=>read_goods_issue( ms_so-mblnr ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '发货凭证未冲销，不允许删除交货单'(M07).
      endif.
    endif.
    if ms_so-vbeln_vl is not initial.
      if lcl_public_tools=>read_delivery_notes( ms_so-vbeln_vl ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '交货单未删除，不允许拒绝销售订单'(M08).
      endif.
    endif.
  endmethod.

  method lif_cancel_doc~cancel_doc.
    assign i_data->* to field-symbol(<fs_data>).
    move-corresponding <fs_data> to ms_so.
    lif_cancel_doc~cancel_before_check( ).

    data(lt_vbap) = lcl_public_tools=>read_so( ms_so-vbeln_va ).

    if lt_vbap[] is initial.
      return.
    endif.

    data: lt_return           type standard table of bapiret2,
          ls_order_header_in  type bapisdh1,
          ls_order_header_inx type bapisdh1x.
    data: lt_item  type standard table of bapisditm,
          lt_itemx type standard table of bapisditmx.

    ls_order_header_inx-updateflag = 'U'.

    loop at lt_vbap into data(ls_vbap).
      append initial line to lt_item assigning field-symbol(<fs_item>).
      <fs_item>-itm_number = ls_vbap-posnr.
      <fs_item>-reason_rej = '11'.
      append initial line to lt_itemx assigning field-symbol(<fs_itemx>).
      <fs_itemx>-itm_number = ls_vbap-posnr.
      <fs_itemx>-updateflag = 'U'.
      <fs_itemx>-reason_rej = abap_true.
    endloop.

    call function 'BAPI_SALESORDER_CHANGE'
      exporting
        salesdocument    = ms_so-vbeln_va
        order_header_inx = ls_order_header_inx
        order_header_in  = ls_order_header_in
      tables
        order_item_in    = lt_item
        order_item_inx   = lt_itemx
        return           = lt_return.
    loop at lt_return into data(ls_return) where type ca 'AEX'.
      ms_so-msgtx = |{ ms_so-msgtx }{ ls_return-message };|.
    endloop.
    if sy-subrc ne 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      data(msgtx) = |销售凭证{ ms_so-vbeln_va }标记拒绝失败:{ ms_so-msgtx }|.
      raise exception type lcx_exception
        exporting
          text = msgtx.
    endif.

  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_cancel_dn
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_cancel_dn implementation.
  method lif_cancel_doc~cancel_before_check.
    if ms_so-vbeln_vf is not initial.
      if lcl_public_tools=>read_billing( ms_so-vbeln_vf ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '请先找财务冲销BILLING'(M06).
      endif.
    endif.
    if ms_so-mblnr is not initial.
      if lcl_public_tools=>read_goods_issue( ms_so-mblnr ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '发货凭证未冲销，不允许删除交货单'(M07).
      endif.
    endif.
  endmethod.

  method lif_cancel_doc~cancel_doc.
    assign i_data->* to field-symbol(<fs_data>).
    move-corresponding <fs_data> to ms_so.
    lif_cancel_doc~cancel_before_check( ).

    data(ls_likp) = lcl_public_tools=>read_delivery_notes( ms_so-vbeln_vl ).

    if ls_likp is initial.
      return.
    endif.

    data: ls_header_data    type bapiobdlvhdrchg,
          ls_header_control type bapiobdlvhdrctrlchg,
          lt_return         type standard table of bapiret2.

    ls_header_data-deliv_numb    = ms_so-vbeln_vl.
    ls_header_control-deliv_numb = ms_so-vbeln_vl.
    ls_header_control-dlv_del    = abap_true.

    call function 'BAPI_OUTB_DELIVERY_CHANGE'
      exporting
        header_data    = ls_header_data
        header_control = ls_header_control
        delivery       = ms_so-vbeln_vl
      tables
        return         = lt_return.
    loop at lt_return into data(ls_return) where type ca 'AEX'.
      ms_so-msgtx = |{ ms_so-msgtx }{ ls_return-message };|.
    endloop.
    if sy-subrc ne 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      data(msgtx) = |交货单{ ms_so-vbeln_vl }删除失败:{ ms_so-msgtx }|.
      raise exception type lcx_exception
        exporting
          text = msgtx.
    endif.

  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_cancel_gi
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_cancel_gi implementation.
  method lif_cancel_doc~cancel_before_check.
    if ms_so-vbeln_vf is not initial.
      if lcl_public_tools=>read_billing( ms_so-vbeln_vf ) is not initial.
        raise exception type lcx_exception
          exporting
            text = '请先找财务冲销BILLING'(M06).
      endif.
    endif.
  endmethod.

  method lif_cancel_doc~cancel_doc.
    assign i_data->* to field-symbol(<fs_data>).
    move-corresponding <fs_data> to ms_so.
    lif_cancel_doc~cancel_before_check( ).

    data(ls_matdoc) = lcl_public_tools=>read_goods_issue( ms_so-mblnr ).

    if ls_matdoc is initial.
      return.
    endif.

    data: i_budat  type sy-datlo,
          i_vbtyp  type likp-vbtyp,
          es_emkpf type emkpf,
          t_mesg   type standard table of mesg.

    i_budat = ms_so-stdat.

    call function 'WS_REVERSE_GOODS_ISSUE'
      exporting
        i_vbeln                   = ms_so-vbeln_vl
        i_budat                   = i_budat
        i_tcode                   = 'VL09'
        i_vbtyp                   = i_vbtyp
      importing
        es_emkpf                  = es_emkpf
      tables
        t_mesg                    = t_mesg
      exceptions
        error_reverse_goods_issue = 1
        others                    = 2.
    if sy-subrc eq 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into ms_so-msgtx.
      data(msgtx) = |交货单{ ms_so-vbeln_vl }发货过账取消失败:{ ms_so-msgtx }|.
      raise exception type lcx_exception
        exporting
          text = msgtx.
    endif.
  endmethod.
endclass.