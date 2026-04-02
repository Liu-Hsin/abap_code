interface zif_inerface_http
  PUBLIC.


  METHODS procrss_data
    IMPORTING iv_data        TYPE string      OPTIONAL
              iv_uuid        TYPE string      OPTIONAL
              i_data         TYPE REF TO data OPTIONAL
    CHANGING  VALUE(re_data) TYPE string      OPTIONAL
              re_return      TYPE bapiret2    OPTIONAL.

  METHODS check_data
    CHANGING VALUE(cs_return) TYPE bapiret2.

  METHODS save_data
    CHANGING VALUE(cs_return) TYPE bapiret2.

  METHODS call_bapi
    CHANGING VALUE(cs_return) TYPE bapiret2.

  METHODS abap2json
    IMPORTING iv_return      TYPE bapiret2
    RETURNING VALUE(ev_data) TYPE string.

  METHODS json2data
    IMPORTING iv_data        TYPE string OPTIONAL
    CHANGING  VALUE(re_data) TYPE data.

  METHODS lock_order
    CHANGING VALUE(cs_return) TYPE bapiret2.
endinterface.
