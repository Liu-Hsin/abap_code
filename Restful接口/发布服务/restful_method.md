```abap
class ZCL_RESTFUL definition
  public
  inheriting from CL_REST_RESOURCE
  final
  create public .

public section.

  methods IF_REST_RESOURCE~POST
    redefinition .
protected section.
private section.

  methods RETURN_ERROR
    importing
      !IV_RETURN type BAPIRET2 .
ENDCLASS.



CLASS ZCL_RESTFUL IMPLEMENTATION.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RESTFUL->IF_REST_RESOURCE~POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ENTITY                      TYPE REF TO IF_REST_ENTITY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_rest_resource~post.
    DATA: lo_json   TYPE REF TO zcl_json_to_data,
          lo_object TYPE REF TO object,
          lo_infter TYPE REF TO zcl_interface_http,
          lv_clname TYPE c LENGTH 30,
          ls_head   TYPE zsifheadinfo,
          ls_return TYPE bapiret2.
    "获取报文。
    DATA(lv_json) = mo_request->get_entity( )->get_string_data( ).

    DATA: lv_data TYPE REF TO data.

    CREATE OBJECT lo_json.
    lo_json->deserialize_json_declare_main(
        EXPORTING
          iv_json = lv_json
        IMPORTING
          data    = lv_data ).

    IF lv_data IS BOUND.
      ASSIGN lv_data->* TO FIELD-SYMBOL(<fs_str>).
    ELSE.
      ls_return-message = '对象构造异常'.
      me->return_error( ls_return ).
      RETURN.
    ENDIF.

    IF <fs_str> IS ASSIGNED..
      /ui2/cl_json=>deserialize(
         EXPORTING
             json = lv_json
         CHANGING
             data = <fs_str> ).
    ENDIF.

    IF <fs_str> IS INITIAL.
      ls_return-message = '数据解析异常'.
      me->return_error( ls_return ).
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'HEAD' OF STRUCTURE <fs_str> TO FIELD-SYMBOL(<fs_head>).
    IF sy-subrc NE 0 OR <fs_head> IS NOT ASSIGNED.
      ls_return-message = '数据解析异常:缺少HEAD数据'.
      me->return_error( ls_return ).
      RETURN.
    ELSE.
      MOVE-CORRESPONDING <fs_head> TO ls_head.
    ENDIF.

    ASSIGN COMPONENT 'DATA' OF STRUCTURE <fs_str> TO FIELD-SYMBOL(<fs_data>).
    IF sy-subrc NE 0 OR <fs_data> IS NOT ASSIGNED.
      ls_return-message = '数据解析异常:缺少DATA数据'.
      me->return_error( ls_return ).
      RETURN.
    ENDIF.

    "根据接口id确定业务实现类
    CASE ls_head-intf_id.
      WHEN 'INT_01'.lv_clname = 'ZCL_INT_01'.
      WHEN 'INT_02'.lv_clname = 'ZCL_INT_02'.
    ENDCASE.

    TRY.
        CREATE OBJECT lo_object TYPE (lv_clname)
          EXPORTING
            iv_head = ls_head
            iv_uuid = cl_system_uuid=>create_uuid_c36_static( ).
      CATCH cx_uuid_error INTO DATA(cx_uuid_error).
        ls_return-message = cx_uuid_error->get_text( ).
        me->return_error( ls_return ).
        RETURN.
      CATCH cx_sy_create_object_error INTO DATA(cx_sy_create_object_error).
        ls_return-message = cx_sy_create_object_error->get_text( ).
        me->return_error( ls_return ).
        RETURN.
      CATCH cx_sy_dyn_call_illegal_method INTO DATA(cx_sy_dyn_call_illegal_method).
        ls_return-message = cx_sy_dyn_call_illegal_method->get_text( ).
        me->return_error( ls_return ).
        RETURN.
      CATCH cx_sy_dyn_call_illegal_type INTO DATA(cx_sy_dyn_call_illegal_type).
        ls_return-message = cx_sy_dyn_call_illegal_type->get_text( ).
        me->return_error( ls_return ).
        RETURN.
      CATCH cx_sy_dyn_call_param_not_found INTO DATA(cx_sy_dyn_call_param_not_found).
        ls_return-message = cx_sy_dyn_call_param_not_found->get_text( ).
        me->return_error( ls_return ).
        RETURN.
    ENDTRY.

    IF lo_object IS INSTANCE OF zcl_interface_http.
      lo_infter ?= lo_object.
    ENDIF.

    IF lo_infter IS INITIAL.
      ls_return-message = '实例未引用接口类(ZCL_INTERFACE_HTTP)'.
      me->return_error( ls_return ).
      RETURN.
    ENDIF.

    DATA: re_data TYPE string,
          iv_data TYPE string.

    iv_data = /ui2/cl_json=>serialize( <fs_data> ).

    lo_infter->me_procrss_data(
        EXPORTING
          iv_data   = iv_data
       CHANGING
          re_data   = re_data
          re_return = ls_return ).

    IF re_data IS NOT INITIAL.
      mo_response->set_reason( 'ok' ).
      mo_response->set_status( 200 ).
      mo_response->create_entity( )->set_string_data( re_data ).
      mo_response->create_entity( )->set_content_type( if_rest_media_type=>gc_appl_json ).
    ELSE.
      me->return_error( ls_return ).
    ENDIF.

  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RESTFUL->RETURN_ERROR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_RETURN                      TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD return_error.
    DATA: iv_json TYPE string.
    DATA: BEGIN OF ls_error,
            code    TYPE string,
            message TYPE string,
          END OF ls_error.

    ls_error-code = 'E'.
    ls_error-message = iv_return-message.
    iv_json = /ui2/cl_json=>serialize( ls_error ).

    mo_response->set_reason( 'error' ).
    mo_response->set_status( 400 ).
    mo_response->create_entity( )->set_string_data( iv_json ).
    mo_response->create_entity( )->set_content_type( if_rest_media_type=>gc_appl_json ).
  ENDMETHOD.
ENDCLASS.

```
