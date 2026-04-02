CLASS zcl_ooalv DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ooalv_screen.

    TYPES:
      BEGIN OF ty_alv,
        grid      TYPE REF TO cl_gui_alv_grid,
        data_name TYPE structure,
      END OF ty_alv.

    DATA lo_grid TYPE REF TO data.

    METHODS constructor
      IMPORTING iv_repid TYPE syrepid.

    METHODS arrange_data.
    METHODS get_data.
    METHODS run.
    METHODS pbo.

    METHODS pai
      IMPORTING iv_ucomm TYPE syucomm.

  PROTECTED SECTION.
    DATA mo_container TYPE REF TO cl_gui_container.
    DATA mt_alv       TYPE TABLE OF ty_alv.

    METHODS alv_display
      IMPORTING io_parent TYPE REF TO cl_gui_container OPTIONAL
                iv_dynnr  TYPE sydynnr                 OPTIONAL.

    METHODS set_layout_fieldcat
      IMPORTING i_dataname TYPE structure
      CHANGING  co_grid    TYPE REF TO cl_gui_alv_grid.

    METHODS call_screen
      IMPORTING iv_dynnr TYPE sydynnr.

  PRIVATE SECTION.
    DATA mv_repid TYPE syrepid.
    DATA mv_dynnr TYPE sydynnr.

    METHODS set_status_toolsbar
      IMPORTING iv_status   TYPE cua_status OPTIONAL
                iv_toolsbar TYPE gui_title  OPTIONAL.

    METHODS create_grid.

    METHODS standard_button
      IMPORTING iv_ucomm TYPE syucomm
      CHANGING  co_grid  TYPE REF TO cl_gui_alv_grid.

    METHODS add_fieldcat
      IMPORTING i_data             TYPE REF TO data OPTIONAL
      RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat.

    METHODS output_data
      IMPORTING is_layout   TYPE lvc_s_layo
      CHANGING  co_grid     TYPE REF TO cl_gui_alv_grid
                ct_fieldcat TYPE lvc_t_fcat
                ct_tab      TYPE STANDARD TABLE.
ENDCLASS.


CLASS zcl_ooalv IMPLEMENTATION.
  METHOD add_fieldcat.
    IF i_data IS NOT SUPPLIED OR i_data IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_struc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( i_data ) ).

    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lo_struc ).

    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<fs_dfies>).
      IF <fs_dfies>-fieldname = 'SEL'.
        CONTINUE.
      ENDIF.
      APPEND INITIAL LINE TO rt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
      <fs_fieldcat>-fieldname = <fs_dfies>-fieldname.
      <fs_fieldcat>-coltext   = <fs_dfies>-scrtext_l.
      <fs_fieldcat>-outputlen = <fs_dfies>-outputlen.
      IF <fs_fieldcat>-coltext IS INITIAL.
        <fs_fieldcat>-coltext = <fs_dfies>-fieldtext.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD alv_display.
    IF io_parent IS BOUND.
      mo_container = io_parent.

      create_grid( ).
      RETURN.
    ENDIF.

    IF iv_dynnr IS NOT INITIAL.
      mv_dynnr = iv_dynnr.
      call_screen( mv_dynnr ).
    ENDIF.
  ENDMETHOD.

  METHOD arrange_data.
  ENDMETHOD.

  METHOD call_screen.
    " Must Be Redefined
    IF iv_dynnr IS NOT INITIAL.
      MESSAGE 'CALL_SCREEN 方法必须要重定义' TYPE 'I'.
    ENDIF.
    " call screen iv_dynnr
  ENDMETHOD.

  METHOD create_grid.
    DATA lo_docker TYPE REF TO cl_gui_docking_container.

    IF mt_alv IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF mo_container IS NOT BOUND.
      lo_docker = NEW cl_gui_docking_container( repid     = mv_repid
                                                dynnr     = mv_dynnr
                                                extension = 9999 ).
      mo_container ?= lo_docker.
    ENDIF.

    APPEND INITIAL LINE TO mt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
    <fs_alv>-data_name = 'MT_DATA'.
    <fs_alv>-grid      = NEW cl_gui_alv_grid( i_parent = mo_container ).

    set_layout_fieldcat( EXPORTING i_dataname = <fs_alv>-data_name
                         CHANGING  co_grid    = <fs_alv>-grid ).

    GET REFERENCE OF <fs_alv>-grid INTO lo_grid.
  ENDMETHOD.

  METHOD get_data.
  ENDMETHOD.

  METHOD output_data.
    co_grid->set_table_for_first_display( EXPORTING  is_layout                     = is_layout
                                          CHANGING   it_fieldcatalog               = ct_fieldcat
                                                     it_outtab                     = ct_tab
                                          EXCEPTIONS invalid_parameter_combination = 1
                                                     program_error                 = 2
                                                     too_many_lines                = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Program Error' TYPE 'A'.
    ENDIF.
  ENDMETHOD.

  METHOD pai.
    FIELD-SYMBOLS <fs_grid> TYPE REF TO cl_gui_alv_grid.
    DATA save_code TYPE sy-ucomm.

    save_code = iv_ucomm.
    ASSIGN lo_grid->* TO <fs_grid>.
    IF <fs_grid> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CASE save_code.
      WHEN 'EXIT' OR 'BACK' OR 'CANC' OR '&F03' OR '&F15' OR '&F12'.
        SET SCREEN 0.
        LEAVE TO SCREEN 0.
      WHEN ''.
      WHEN OTHERS.
        standard_button( EXPORTING iv_ucomm = save_code
                         CHANGING  co_grid  = <fs_grid> ).
    ENDCASE.
  ENDMETHOD.

  METHOD pbo.
    set_status_toolsbar( iv_status = 'STANDARD' ).

    create_grid( ).
  ENDMETHOD.

  METHOD run.
    get_data( ).

    arrange_data( ).

    alv_display( iv_dynnr = '0100' ).
  ENDMETHOD.

  METHOD set_layout_fieldcat.
    FIELD-SYMBOLS <fs_tab> TYPE STANDARD TABLE.
    DATA ls_layout   TYPE lvc_s_layo.
    DATA lt_fieldcat TYPE lvc_t_fcat.
    DATA lo_data     TYPE REF TO data.

    ls_layout-zebra      = 'X'.
    ls_layout-cwidth_opt = 'X'.
    ls_layout-no_toolbar = 'X'.
    ls_layout-sel_mode   = 'D'.
    IF co_grid IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN (i_dataname) TO <fs_tab>.
    IF <fs_tab> IS NOT ASSIGNED.
      MESSAGE 'Program Error' TYPE 'A'.
    ENDIF.

    CREATE DATA lo_data LIKE LINE OF <fs_tab>.

    lt_fieldcat = add_fieldcat( i_data = lo_data ).

    output_data( EXPORTING is_layout   = ls_layout
                 CHANGING  co_grid     = co_grid
                           ct_fieldcat = lt_fieldcat
                           ct_tab      = <fs_tab> ).
  ENDMETHOD.

  METHOD set_status_toolsbar.
    IF iv_status IS SUPPLIED.
      SET PF-STATUS iv_status OF PROGRAM mv_repid.
    ENDIF.
    IF iv_toolsbar IS SUPPLIED.
      SET TITLEBAR iv_toolsbar OF PROGRAM mv_repid.
    ENDIF.
  ENDMETHOD.

  METHOD standard_button.
    DATA lv_ucomm TYPE syucomm.

    lv_ucomm = iv_ucomm.
    IF co_grid IS BOUND.
      co_grid->set_function_code( CHANGING c_ucomm = lv_ucomm ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ooalv_screen~at_screen.
    IF iv_ucomm IS SUPPLIED.
      CASE iv_ucomm.
        WHEN 'FC01'.
        WHEN 'FC02'.
        WHEN 'FC03'.
        WHEN 'FC04'.
        WHEN 'FC05'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD zif_ooalv_screen~init_screen.
  ENDMETHOD.

  METHOD zif_ooalv_screen~screen_button.
    DATA lv_fname TYPE fieldname.

    LOOP AT it_button ASSIGNING FIELD-SYMBOL(<fs_button>) WHERE number BETWEEN 1 AND 5.
      " REFERENCE INTO data()
      lv_fname = |FUNCTXT_0{ <fs_button>-number }|.
      ASSIGN COMPONENT lv_fname OF STRUCTURE rs_sscrfields TO FIELD-SYMBOL(<fs_functext>).
      IF <fs_functext> IS ASSIGNED.
        <fs_functext> = <fs_button>-text.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_ooalv_screen~screen_out.
  ENDMETHOD.

  METHOD constructor.
    mv_repid = iv_repid.
  ENDMETHOD.
ENDCLASS.