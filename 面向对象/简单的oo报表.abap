*&---------------------------------------------------------------------*  
*& Report ZOOALV  
*&---------------------------------------------------------------------*  
*&  
*&---------------------------------------------------------------------*  
report zooalv.  
tables sscrfields.  
  
data: ok_code type sy-ucomm.  
data: begin of gs_alv,  
        vbeln type vbak-vbeln,  
        posnr type vbap-posnr,  
        auart type vbak-auart,  
        kunnr type vbak-kunnr,  
        vkorg type vbak-vkorg,  
        erdat type vbak-erdat,  
        matnr type vbap-matnr,  
        menge type vbap-kwmeng,  
        meins type vbap-meins,  
        netpr type vbap-netpr,  
      end of gs_alv.  
data: gt_alv like standard table of gs_alv.  
data go_container type ref to cl_gui_custom_container.  
data go_alv type ref to cl_gui_alv_grid.  
  
selection-screen: begin of screen 9001 as subscreen.  
  selection-screen begin of block b1 with frame title text-t01.  
    select-options:  
    s_vbeln for gs_alv-vbeln," OBLIGATORY,  
    s_auart for gs_alv-auart,  
    s_kunnr for gs_alv-kunnr,  
    s_vkorg for gs_alv-vkorg,  
    s_matnr for gs_alv-matnr.  
  selection-screen end of block b1.  
selection-screen end of screen 9001.  
  
initialization.  
*  sscrfields-functxt_01 = 'FC01'.  
  
at selection-screen output.  
  
at selection-screen.  
  
start-of-selection.  
  call screen 9000.  
  
load-of-program.  
*&---------------------------------------------------------------------*  
*& Module STATUS_9000 OUTPUT  
*&---------------------------------------------------------------------*  
*&  
*&---------------------------------------------------------------------*  
module status_9000 output.  
  set pf-status 'STATUS'.  
endmodule.  
*&---------------------------------------------------------------------*  
*&      Module  USER_COMMAND_9000  INPUT  
*&---------------------------------------------------------------------*  
module user_command_9000 input.  
  check ok_code is not initial.  
  case ok_code.  
    when 'BACK' or 'RW'.  
      leave program.  
    when 'ONLI'.  
      perform: frm_get_data.  
      perform: frm_create_alv.  
    when 'GET ' or 'VDEL' or 'VSHO' or 'SAVE' or 'SPOS' or  
         'SALE' or 'GOON' or 'VATT' or 'PREV' or 'NEXT' or 'RWCV'.  
      sy-dynnr = '9001'.  
      perform %_ok_code_1000 in program rsdbrunt if found.  
    when others.  
  endcase.  
  clear ok_code.  
endmodule.  
*&---------------------------------------------------------------------*  
*&      Module  EXIT  INPUT  
*&---------------------------------------------------------------------*  
module exit input.  
  leave program.  
endmodule.  
*&---------------------------------------------------------------------*  
*& Form frm_get_data  
*&---------------------------------------------------------------------*  
form frm_get_data .  
  data msg type string.  
  select a~vbeln,b~posnr,a~auart,a~kunnr,a~vkorg,a~erdat,b~matnr,b~kwmeng as menge,b~meins,b~netpr  
    into table @gt_alv  
    from vbak as a inner join vbap as b  
    on b~vbeln = a~vbeln  
    where a~vbeln in @s_vbeln  
      and a~auart in @s_auart  
      and a~kunnr in @s_kunnr  
      and a~vkorg in @s_vkorg  
      and b~matnr in @s_matnr.  
  if sy-subrc ne 0.  
    msg = text-i01.  
    replace '&1' into msg with 'HHHH'.  
    message msg type 'E' .  
  endif.  
endform.  
*&---------------------------------------------------------------------*  
*& Form frm_create_alv  
*&---------------------------------------------------------------------*    
form frm_create_alv .  
  statics t_field type lvc_t_fcat.  
  
  if not go_container is bound.  
    go_container = new cl_gui_custom_container( container_name = 'CONTAINER' ).  
  endif.  
  
  if t_field[] is initial.  
    perform: frm_add_fieldcat changing t_field.  
  endif.  
  
  if not go_alv is bound.  
    go_alv = new cl_gui_alv_grid(  i_parent  = go_container ).  
  
    go_alv->set_table_for_first_display(  
      exporting  
        i_save          = 'X'  
        i_default       = 'X'  
      changing  
        it_fieldcatalog = t_field  
        it_outtab       = gt_alv  
    ).  
  else.  
    go_alv->refresh_table_display( ).  
  endif.  
  go_alv->set_frontend_layout( value lvc_s_layo( sel_mode = 'D' zebra = 'X' cwidth_opt = 'X'  ) ).  
  if gt_alv[] is initial.  
    message '没有符合条件的数据' type 'S'.  
  endif.  
  
endform.  
*&---------------------------------------------------------------------*  
*& Form FRM_ADD_FIELDCAT  
*&---------------------------------------------------------------------*  
form frm_add_fieldcat  changing p_t_field type lvc_t_fcat.  
  append initial line to p_t_field assigning field-symbol(<fs_field>).  
  <fs_field>-fieldname = 'VBELN'.  
  <fs_field>-coltext = '销售订单'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'POSNR'.  
  <fs_field>-coltext = '销售订单行'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'AUART'.  
  <fs_field>-coltext = '订单类型'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'KUNNR'.  
  <fs_field>-coltext = '客户'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'VKORG'.  
  <fs_field>-coltext = '销售组织'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'ERDAT'.  
  <fs_field>-coltext = '创建日期'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'MATNR'.  
  <fs_field>-coltext = '物料'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'MENGE'.  
  <fs_field>-coltext = '数量'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'MEINS'.  
  <fs_field>-coltext = '单位'.  
  
  append initial line to p_t_field assigning <fs_field>.  
  <fs_field>-fieldname = 'NETPR'.  
  <fs_field>-coltext = '单价'.  
endform.

* 屏幕9000的逻辑流 *
process before output.  
  call subscreen sub9001 including 'ZOOALV' '9001'.  
  module status_9000.  
*  
process after input.  
  call subscreen sub9001.  
  module user_command_9000.  
  
  module exit at exit-command.  