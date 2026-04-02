```abap
CLASS zcl_inf DEFINITION
  PUBLIC
  INHERITING FROM cl_rest_http_handler
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS if_rest_application~get_root_handler
        REDEFINITION .
  PROTECTED SECTION.

    METHODS handle_csrf_token
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INF IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_INF->HANDLE_CSRF_TOKEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CSRF_HANDLER                TYPE REF TO IF_REST_CSRF_HANDLER
* | [--->] IO_REQUEST                     TYPE REF TO IF_REST_REQUEST
* | [--->] IO_RESPONSE                    TYPE REF TO IF_REST_RESPONSE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_csrf_token.
*CALL METHOD SUPER->HANDLE_CSRF_TOKEN
*  EXPORTING
*    IO_CSRF_HANDLER =
*    IO_REQUEST      =
*    IO_RESPONSE     =
*    .
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INF->IF_REST_APPLICATION~GET_ROOT_HANDLER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_ROOT_HANDLER                TYPE REF TO IF_REST_HANDLER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_rest_application~get_root_handler.
    DATA:lo_handler TYPE REF TO cl_rest_router.

    CREATE OBJECT lo_handler.

    lo_handler->attach( iv_template = '/HttpService' iv_handler_class = 'ZCL_RESTFUL' ).

    ro_root_handler = lo_handler.

  ENDMETHOD.
ENDCLASS.
```
