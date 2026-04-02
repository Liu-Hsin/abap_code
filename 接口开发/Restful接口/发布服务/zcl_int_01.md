```abap
class ZCL_INT_01 definition
  public
  final
  create public .

public section.

  interfaces ZCL_INTERFACE_HTTP .

  aliases ME_ABAP2JSON
    for ZCL_INTERFACE_HTTP~ME_ABAP2JSON .
  aliases ME_CALL_BAPI
    for ZCL_INTERFACE_HTTP~ME_CALL_BAPI .
  aliases ME_CHECK_DATA
    for ZCL_INTERFACE_HTTP~ME_CHECK_DATA .
  aliases ME_JSON2DATA
    for ZCL_INTERFACE_HTTP~ME_JSON2DATA .
  aliases ME_PROCRSS_DATA
    for ZCL_INTERFACE_HTTP~ME_PROCRSS_DATA .
  aliases ME_SAVE_DATA
    for ZCL_INTERFACE_HTTP~ME_SAVE_DATA .

  methods CONSTRUCTOR
    importing
      value(IV_UUID) type SYSUUID_C36 optional
      value(IV_HEAD) type ZSIFHEADINFO optional .
protected section.
private section.

  types:
    BEGIN OF ty_data,
      erdat      TYPE erdat     , "订单日期
      matnr      TYPE matnr     , "整车编码
      vin        TYPE zvin      , "VIN码
      netpr      TYPE netpr     , "单价（含税）
      bukrs      TYPE bukrs     ,
      kunag      TYPE kunag     , "客户代码 .
    END OF ty_data .
  types:
    tt_data TYPE STANDARD TABLE OF ty_data .
  types:
    BEGIN OF ty_out,
           requestid TYPE string,
           code      TYPE string,
           desc      TYPE string,
         END OF ty_out .
  types:
    tt_outdata TYPE STANDARD TABLE OF ty_out .

  data MS_REQUEST_HEAD type ZSIFHEADINFO .
  data MV_UUID type STRING .
  data MT_DATA type TT_DATA .
  data MT_OUTDATA type TT_OUTDATA .
ENDCLASS.



CLASS YY_LOCAL_CL_INT_01 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UUID                        TYPE        SYSUUID_C36(optional)
* | [--->] IV_HEAD                        TYPE        ZSIFHEADINFO(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    mv_uuid = iv_uuid.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->ZCL_INTERFACE_HTTP~ME_ABAP2JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_RETURN                      TYPE        BAPIRET2
* | [<-()] EV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ZCL_INTERFACE_HTTP~ME_ABAP2JSON.
    APPEND INITIAL LINE TO me->mt_outdata ASSIGNING FIELD-SYMBOL(<fs_out>).
    <fs_out>-code = iv_return-type.
    <fs_out>-desc = iv_return-message.
    <fs_out>-requestid = me->mv_uuid.

    ev_data = /ui2/cl_json=>serialize( me->mt_outdata ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->ZCL_INTERFACE_HTTP~ME_CALL_BAPI
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CS_RETURN                      TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zcl_interface_http~me_call_bapi.

    CHECK me->mt_data[] IS NOT INITIAL.
    cs_return-type = 'S'.
    cs_return-message = '业务执行无错误！'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->ZCL_INTERFACE_HTTP~ME_CHECK_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CS_RETURN                      TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zcl_interface_http~me_check_data.

    CHECK me->mt_data[] IS NOT INITIAL.
    cs_return-type = 'S'.
    cs_return-message = '检查无错误！'.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->ZCL_INTERFACE_HTTP~ME_JSON2DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        STRING(optional)
* | [<-->] RE_DATA                        TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ZCL_INTERFACE_HTTP~ME_JSON2DATA.

        /ui2/cl_json=>deserialize(
       EXPORTING
           json = iv_data
       CHANGING
           data = re_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->ZCL_INTERFACE_HTTP~ME_PROCRSS_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        STRING(optional)
* | [--->] IV_UUID                        TYPE        STRING(optional)
* | [<-->] RE_DATA                        TYPE        STRING(optional)
* | [<-->] RE_RETURN                      TYPE        BAPIRET2(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zcl_interface_http~me_procrss_data.
BREAK wis_liux.
    TRY.
        me->zcl_interface_http~me_json2data( EXPORTING iv_data = iv_data CHANGING re_data = me->mt_data ).

        me->zcl_interface_http~me_check_data( CHANGING cs_return = re_return ).

        me->zcl_interface_http~me_lock_order( CHANGING cs_return = re_return ).

        me->zcl_interface_http~me_call_bapi( CHANGING cs_return = re_return ).

        me->zcl_interface_http~me_save_data( CHANGING cs_return = re_return  ).

        re_data = me->zcl_interface_http~me_abap2json( EXPORTING iv_return = re_return ).

      CATCH cx_sy_dyn_call_illegal_method INTO DATA(lo_error).

        re_return-message = lo_error->if_message~get_text( ).
        re_return-type    = 'E'.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_01->ZCL_INTERFACE_HTTP~ME_SAVE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CS_RETURN                      TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zcl_interface_http~me_save_data.

    CHECK me->mt_data[] IS NOT INITIAL.

    cs_return-type = 'S'.
    cs_return-message = '数据保存无错误！'.

  ENDMETHOD.
ENDCLASS.

```
