class lcl_model definition final.  
  public section.   
    class-methods  
      get_instance  
        returning value(ro_instance) type ref to lcl_model.  
    methods:  
      get_data_from_db  
        importing is_param type lcl_attributes_types=>ty_param,  
      get_data 
	     returning value(rt_data) type lcl_attributes_types=>tt_data.  
    
  private section.  
    class-data:  
      mo_instance    type ref to lcl_model.
    data:  
      mt_data type standard table of lcl_attributes_types=>ty_data.   
endclass.  
  
class lcl_view definition final.  
  public section.  
    class-methods  
      get_instance  
        returning value(ro_instance) type ref to lcl_view.  
    methods:  
      display,  
      set_status,   
      refresh_display  
        importing io_grid type ref to cl_gui_alv_grid.  
  
  private section.  
    class-data:  
      mo_instance type ref to lcl_view.  
    data:  
      mv_only_show  type flag,  
      mv_repid      type syrepid,  
      ms_layout_lvc type lvc_s_layo,  
      mt_fieldcat   type lvc_t_fcat.  
    methods:  
      set_layout,  
      set_fieldcat.  
endclass.  
  
class lcl_controller definition final.  
  public section.  
    class-methods  
      get_instance  
        returning value(ro_instance) type ref to lcl_controller.  
    methods:  
      program_init,  
      run,  
      handle_user_command  
        importing iv_ucomm    type syucomm  
                  is_selfield type slis_selfield  
                  io_grid     type ref to cl_gui_alv_grid.  
  private section.  
    class-data:  
      mo_instance type ref to lcl_controller,  
      mo_model    type ref to lcl_model,  
      mo_view     type ref to lcl_view.  
    methods:  
      init_instance.  
endclass.

class lcl_model implementation.  
  method get_instance.  
    if mo_instance is not bound.  
      mo_instance = new lcl_model( ).  
    endif.  
    ro_instance = mo_instance.  
  endmethod.  
  
  method get_data_from_db.  
	 
  endmethod.  
  
  method get_data.
	 rt_data = mt_data.
  endmethod.  
endclass.  
*&---------------------------------------------------------------------*  
*& Class (Implementation) lcl_view  
*&---------------------------------------------------------------------*  
*&  
*&---------------------------------------------------------------------*  
class lcl_view implementation.  
  method get_instance.  
    if mo_instance is not bound.  
      mo_instance = new lcl_view( ).  
    endif.  
    ro_instance = mo_instance.  
  endmethod.  
  
  method display.  
    data(lt_data) = lcl_model=>get_instance( )->get_data( ).  
  
    mv_repid = sy-repid.  
  
    me->set_fieldcat( ).  
    me->set_layout( ).  
  
    call function 'REUSE_ALV_GRID_DISPLAY_LVC'  
      exporting  
        i_callback_program       = mv_repid  
        is_layout_lvc            = ms_layout_lvc  
        it_fieldcat_lvc          = mt_fieldcat  
        i_callback_pf_status_set = 'FRM_ALV_PF_SET_STATUS'  
        i_callback_user_command  = 'FRM_ALV_USER_COMMAND'  
        i_default                = abap_true  
        i_save                   = 'A'  
      tables  
        t_outtab                 = lt_data  
      exceptions  
        program_error            = 1  
        others                   = 2.  
  endmethod.  
  method set_layout.  
    ms_layout_lvc-cwidth_opt = abap_true.  
    ms_layout_lvc-zebra      = abap_true.  
  endmethod.  
  
  method set_fieldcat.  
    data ct_fieldcat type lvc_t_fcat.  
    mt_fieldcat = ct_fieldcat.  
  endmethod.  
  
  method set_status.  
    data lt_exclude type kkblo_t_extab.  
  
    if mv_only_show = abap_true.  
      append lcl_attributes_types=>c_func_create to lt_exclude.  
    endif.  
    set pf-status 'STANDARD' excluding lt_exclude.  
  endmethod.  
  
  
  method refresh_display.  
    data ls_stable type lvc_s_stbl.  
  
    ls_stable-row = abap_true.  
    ls_stable-col = abap_true.  
  
    io_grid->refresh_table_display( is_stable = ls_stable ).  
    io_grid->set_frontend_layout( me->ms_layout_lvc ).  
  endmethod.  
  
endclass.  
form frm_alv_pf_set_status using ut_excluding_merged type kkblo_t_extab.  
  lcl_view=>get_instance( )->set_status( ).  
endform.  
  
form frm_alv_user_command using p_ucomm    like sy-ucomm  
                                p_selfield type slis_selfield.  
  
  data lr_grid type ref to cl_gui_alv_grid.  
  
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'  
    importing  
      e_grid = lr_grid.  
  
  lr_grid->check_changed_data( ).  
  
  lcl_controller=>get_instance( )->handle_user_command(  
                                        iv_ucomm    = p_ucomm  
                                        is_selfield = p_selfield  
                                        io_grid     = lr_grid ).  
endform.  
*&---------------------------------------------------------------------*  
*& Class (Implementation) lcl_controller  
*&---------------------------------------------------------------------*  
*&  
*&---------------------------------------------------------------------*  
class lcl_controller implementation.  
  method get_instance.  
    if mo_instance is not bound.  
      mo_instance = new lcl_controller( ).  
    endif.  
    ro_instance = mo_instance.  
  endmethod.  
  
  method program_init.  
    me->init_instance( ).  
  endmethod.  
  
  method init_instance.  
    me->mo_model = lcl_model=>get_instance( ).  
    me->mo_view  = lcl_view=>get_instance( ).  
    me->mo_model->set_service(  
      io_service = lcl_service=>get_instance( ) ).  
  endmethod.  
    
  method run.  
    data(is_param) = value lcl_attributes_types=>ty_param( ).  
    me->mo_model->get_data_from_db( is_param ).    
    if sy-batch = abap_true.  
      me->mo_model->create_document( ).  
    else.  
      me->mo_view->display( ).  
    endif.  
  endmethod.  
  
  method handle_user_command.  
    case iv_ucomm.  
      when '&IC1'.  
        mo_view->show_doc( is_selfield ).  
      when lcl_attributes_types=>c_func_create.  
        mo_model->create_document( ).  
        mo_view->refresh_display( io_grid ).  
      when others.  
    endcase.  
  endmethod.  
endclass.