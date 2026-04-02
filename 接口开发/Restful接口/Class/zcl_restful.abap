CLASS ZCL_restful DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_rest_resource~delete REDEFINITION.
    METHODS if_rest_resource~get    REDEFINITION.
    METHODS if_rest_resource~post   REDEFINITION.
    METHODS if_rest_resource~put    REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS return_error
      IMPORTING iv_return TYPE bapiret2.
ENDCLASS.



CLASS ZCL_RESTFUL IMPLEMENTATION.


  METHOD if_rest_resource~delete.
*CALL METHOD SUPER->IF_REST_RESOURCE~DELETE
*    .

    DATA(lv_id) = mo_request->get_uri_query_parameter( iv_name = 'ID' ).

    SELECT SINGLE COUNT(*) FROM z2ui5_t_core_01 WHERE id = lv_id.
    IF sy-subrc = 0.
      DELETE FROM z2ui5_t_core_01 WHERE id = lv_id.
      IF sy-subrc = 0.
        COMMIT WORK.
        mo_response->set_reason( 'success' ).
        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
      ELSE.
        ROLLBACK WORK.
        mo_response->set_reason( 'fail' ).
        mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
      ENDIF.
    ELSE.
      mo_response->set_reason( 'no_found' ).
      mo_response->set_status( cl_rest_status_code=>gc_success_accepted ).
    ENDIF.
  ENDMETHOD.


  METHOD if_rest_resource~get.
*CALL METHOD SUPER->IF_REST_RESOURCE~GET
*    .
    DATA(lv_id) = mo_request->get_uri_query_parameter( iv_name = 'ID' ).
    SELECT SINGLE * FROM z2ui5_t_core_01 INTO @DATA(ls_01) WHERE id = @lv_id.

    DATA(lo_entity) = mo_response->create_entity( ).

    DATA lv_data TYPE xstring.
    lv_data = ls_01-data.

    lo_entity->set_binary_data( iv_data = lv_data ).
  ENDMETHOD.


  METHOD if_rest_resource~post.
    DATA lo_object TYPE REF TO object.
    DATA lo_infter TYPE REF TO zif_interface_http.
    DATA lv_clname TYPE c LENGTH 30.
    DATA ls_head   TYPE zsifheadinfo.
    DATA ls_return TYPE bapiret2.
    DATA: BEGIN OF ls_data,
            head TYPE zsifheadinfo,
            data TYPE REF TO data,  "data部分所有的内容格式必须要为字符串
          END OF ls_data.

    " 获取报文。
    DATA(lv_json) = mo_request->get_entity( )->get_string_data( ).
    /ui2/cl_json=>deserialize( EXPORTING json = lv_json
                               CHANGING  data = ls_data ).

    ASSIGN COMPONENT 'HEAD' OF STRUCTURE ls_data TO FIELD-SYMBOL(<fs_head>).
    IF sy-subrc <> 0 OR <fs_head> IS NOT ASSIGNED OR <fs_head> IS INITIAL.
      ls_return-message = '数据解析异常:缺少HEAD数据'.
      return_error( ls_return ).
      RETURN.
    ELSE.
      MOVE-CORRESPONDING <fs_head> TO ls_head.
    ENDIF.

    ASSIGN COMPONENT 'DATA' OF STRUCTURE ls_data TO FIELD-SYMBOL(<fs_data>).
    IF sy-subrc <> 0 OR <fs_data> IS NOT ASSIGNED OR <fs_data> IS INITIAL.
      ls_return-message = '数据解析异常:缺少DATA数据'.
      return_error( ls_return ).
      RETURN.
    ENDIF.

    " 根据接口id确定业务实现类
    CASE ls_head-intf_id.
      WHEN 'INT_01'.lv_clname = 'ZCL_CL_INT_01'.
      WHEN 'INT_02'.lv_clname = 'ZCL_CL_INT_02'.
    ENDCASE.

    TRY.
        CREATE OBJECT lo_object TYPE (lv_clname)
          EXPORTING iv_head = ls_head
                    iv_uuid = cl_system_uuid=>create_uuid_c36_static( ).
      CATCH cx_uuid_error INTO DATA(lo_uuid_error).
        ls_return-message = lo_uuid_error->get_text( ).
        return_error( ls_return ).
        RETURN.
      CATCH cx_sy_create_object_error INTO DATA(lo_object_error).
        ls_return-message = lo_object_error->get_text( ).
        return_error( ls_return ).
        RETURN.
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_illegal_method).
        ls_return-message = lo_illegal_method->get_text( ).
        return_error( ls_return ).
        RETURN.
      CATCH cx_sy_dyn_call_illegal_type INTO DATA(lo_illegal_type).
        ls_return-message = lo_illegal_type->get_text( ).
        return_error( ls_return ).
        RETURN.
      CATCH cx_sy_dyn_call_param_not_found INTO DATA(lo_param_not_found).
        ls_return-message = lo_param_not_found->get_text( ).
        return_error( ls_return ).
        RETURN.
      CATCH cx_root INTO DATA(lo_root).
        ls_return-message = lo_root->get_text( ).
        return_error( ls_return ).
        RETURN.
    ENDTRY.

    IF lo_object IS INSTANCE OF zif_interface_http.
      lo_infter ?= lo_object.
    ENDIF.

    IF lo_infter IS INITIAL.
      ls_return-message = '实例未引用接口类(ZIF_INTERFACE_HTTP)'.
      return_error( ls_return ).
      RETURN.
    ENDIF.

    DATA re_data TYPE string.
    DATA iv_data TYPE string.

    iv_data = /ui2/cl_json=>serialize( <fs_data> ).

    lo_infter->procrss_data( EXPORTING iv_data   = iv_data
                                       i_data    = ls_data-data
                             CHANGING  re_data   = re_data
                                       re_return = ls_return ).

    IF re_data IS NOT INITIAL.
      mo_response->set_reason( 'ok' ).
      mo_response->set_status( 200 ).
      mo_response->create_entity( )->set_string_data( re_data ).
      mo_response->create_entity( )->set_content_type( if_rest_media_type=>gc_appl_json ).
    ELSE.
      return_error( ls_return ).
    ENDIF.
  ENDMETHOD.


  METHOD if_rest_resource~put.
*CALL METHOD SUPER->IF_REST_RESOURCE~PUT
*  EXPORTING
*    IO_ENTITY =
*    .
    DATA z2ui5_t_core_01 TYPE z2ui5_t_core_01.

    DATA(rv_data) = io_entity->get_binary_data( ).
    z2ui5_t_core_01-id   = sy-uzeit.
    z2ui5_t_core_01-data = rv_data.
    INSERT z2ui5_t_core_01 FROM z2ui5_t_core_01.
    COMMIT WORK.
    mo_response->set_reason( 'Success' ).
  ENDMETHOD.


  METHOD return_error.
    DATA iv_json TYPE string.
    DATA: BEGIN OF ls_error,
            code    TYPE string,
            message TYPE string,
          END OF ls_error.

    ls_error-code    = 'E'.
    ls_error-message = iv_return-message.
    iv_json = /ui2/cl_json=>serialize( ls_error ).

    mo_response->set_reason( 'error' ).
    mo_response->set_status( 400 ).
    mo_response->create_entity( )->set_string_data( iv_json ).
    mo_response->create_entity( )->set_content_type( if_rest_media_type=>gc_appl_json ).
  ENDMETHOD.
ENDCLASS.