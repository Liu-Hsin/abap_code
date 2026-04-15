* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_NORMAL_TOOLS=>HMAC_FOR_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ALGORITHM                   TYPE        STRING (default ='SHA256')
* | [--->] IV_CONTENT                     TYPE        STRING
* | [--->] IV_KEY                         TYPE        STRING
* | [<-()] RV_HMACB64STRING               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method hmac_for_char.
  "对字符串内容，使用指定密钥和哈希算法，生成 HMAC 签名并返回 Base64 编码字符串
    data lv_xstr type xstring.

    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text     = iv_key
        encoding = '4110'
      importing
        buffer   = lv_xstr
      exceptions
        failed   = 1
        others   = 2.

    data if_length        type i.

    if_length = strlen( iv_content ).
    try.
        cl_abap_hmac=>calculate_hmac_for_char(
          exporting
            if_algorithm     = iv_algorithm
            if_key           = lv_xstr
            if_data          = iv_content
            if_length        = if_length
          importing
            ef_hmacb64string = rv_hmacb64string
        ).
      catch cx_abap_message_digest.
    endtry.
  endmethod.