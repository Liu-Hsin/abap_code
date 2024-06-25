* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_NORMAL_TOOLS=>RSA_ENCRYPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PUBLIC_KEY                  TYPE        STRING
* | [--->] IV_VALUE                       TYPE        STRING
* | [<-()] EV_CIPHERTEXT                  TYPE        STRING
* | [EXC!] ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD RSA_ENCRYPTION.
    DATA: lv_value_x TYPE xstring,
          lv_pubkey  TYPE xstring.
    DATA: lv_key_string     TYPE string,
          lv_output         TYPE xstring,
          lv_crc            TYPE ssfparms-ssfcrc,
          lt_recipient_list TYPE STANDARD TABLE OF ssfinfo,
          ls_recipient_list LIKE LINE OF lt_recipient_list.

    TRY.
        lv_value_x = cl_bcs_convert=>string_to_xstring( iv_value ).
      CATCH cx_bcs INTO DATA(lo_bcs).
        RAISE error.
    ENDTRY.

    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
      EXPORTING
        input    = iv_public_key
        unescape = 'X'
      IMPORTING
        output   = lv_pubkey
      EXCEPTIONS
        failed   = 1
        OTHERS   = 2.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

    lv_key_string = lv_pubkey.

    DATA(length) = strlen( lv_key_string ).
    IF length > 514.    " 2048位密钥
      DATA(pos) = length - 10 - 514.
      lv_key_string = lv_key_string+pos(514).
    ELSEIF length > 256. " 1024位密钥
      pos = length - 10 - 256.
      lv_key_string = lv_key_string+pos(256).
    ENDIF.

    " 函数使用密钥格式
    lv_key_string = 'R:m=' && lv_key_string && ':e=010001:'.
    TRY.
        lv_pubkey = cl_bcs_convert=>string_to_xstring( lv_key_string ).
      CATCH cx_bcs INTO lo_bcs.
        RAISE error.
    ENDTRY.
    ls_recipient_list-id = '<implicit>'."
    APPEND ls_recipient_list TO lt_recipient_list.

    CALL FUNCTION 'SSFW_KRN_ENVELOPE'
      EXPORTING
        str_format                   = 'PKCS1-V1.5'
        str_pab                      = '<no_certificate_check>'
        str_chainfmt                 = 'KEYVALUE'
        ostr_chain_data              = lv_pubkey
        ostr_input_data              = lv_value_x
      IMPORTING
        ostr_enveloped_data          = lv_output
        crc                          = lv_crc
      TABLES
        recipient_list               = lt_recipient_list
      EXCEPTIONS
        ssf_krn_error                = 1
        ssf_krn_noop                 = 2
        ssf_krn_nomemory             = 3
        ssf_krn_opinv                = 4
        ssf_krn_nossflib             = 5
        ssf_krn_recipient_list_error = 6
        ssf_krn_input_data_error     = 7
        ssf_krn_invalid_par          = 8
        ssf_krn_invalid_parlen       = 9
        ssf_fb_input_parameter_error = 10
        OTHERS                       = 11.
    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.

    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = lv_output
      IMPORTING
        output = ev_ciphertext.

  ENDMETHOD.
