CLASS zcl_int_002 DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_interface_http.

    METHODS constructor
      IMPORTING VALUE(iv_uuid) TYPE sysuuid_c36  OPTIONAL
                VALUE(iv_head) TYPE zsifheadinfo OPTIONAL.

  PRIVATE SECTION.
    TYPES tt_data TYPE STANDARD TABLE OF zfis003.
    TYPES:
      BEGIN OF ty_out,
        requestid TYPE string,
        code      TYPE string,
        desc      TYPE string,
      END OF ty_out.
    TYPES tt_outdata TYPE STANDARD TABLE OF ty_out.

    DATA ms_request_head TYPE zsifheadinfo.
    DATA mv_uuid         TYPE string.
    DATA mt_data         TYPE tt_data.
    DATA mt_outdata      TYPE tt_outdata.
ENDCLASS.


CLASS zcl_int_002 IMPLEMENTATION.
  METHOD constructor.
    " TODO: parameter IV_HEAD is never used (ABAP cleaner)

    mv_uuid = iv_uuid.
  ENDMETHOD.

  METHOD zif_interface_http~abap2json.
    APPEND INITIAL LINE TO mt_outdata ASSIGNING FIELD-SYMBOL(<fs_out>).
    <fs_out>-code      = iv_return-type.
    <fs_out>-desc      = iv_return-message.
    <fs_out>-requestid = mv_uuid.

    ev_data = /ui2/cl_json=>serialize( mt_outdata ).
  ENDMETHOD.

  METHOD zif_interface_http~call_bapi.
    CHECK mt_data[] IS NOT INITIAL.
    cs_return-type    = 'S'.
    cs_return-message = '业务执行无错误！'.
  ENDMETHOD.

  METHOD zif_interface_http~check_data.
    CHECK mt_data[] IS NOT INITIAL.
    cs_return-type    = 'S'.
    cs_return-message = '检查无错误！'.
  ENDMETHOD.

  METHOD zif_interface_http~json2data.
    /ui2/cl_json=>deserialize( EXPORTING json = iv_data
                               CHANGING  data = re_data ).
  ENDMETHOD.

  METHOD zif_interface_http~lock_order.
  ENDMETHOD.

  METHOD zif_interface_http~procrss_data.
    DATA lo_error TYPE REF TO cx_sy_dyn_call_illegal_method.

    TRY.
        zif_interface_http~json2data( EXPORTING iv_data = iv_data
                                               CHANGING  re_data = mt_data ).

        zif_interface_http~check_data( CHANGING cs_return = re_return ).

        zif_interface_http~lock_order( CHANGING cs_return = re_return ).

        zif_interface_http~call_bapi( CHANGING cs_return = re_return ).

        zif_interface_http~save_data( CHANGING cs_return = re_return  ).

        re_data = zif_interface_http~abap2json( iv_return = re_return ).

      CATCH cx_sy_dyn_call_illegal_method INTO lo_error.
        re_return-message = lo_error->if_message~get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_interface_http~save_data.
    CHECK mt_data[] IS NOT INITIAL.

    cs_return-type    = 'S'.
    cs_return-message = '数据保存无错误！'.
  ENDMETHOD.
ENDCLASS.