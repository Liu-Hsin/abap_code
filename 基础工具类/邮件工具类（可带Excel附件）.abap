class zcl_mail_util definition
  public
  final
  create private .

  public section.

    class-methods send
      importing
        value(iv_subject)      type so_obj_des
        value(iv_content_type) type so_obj_tp default 'RAW'
        value(it_content)      type soli_tab
        value(it_recipient)    type somlreci1_t
        value(ir_alv_data)     type ref to data optional
        value(it_fieldcat)     type lvc_t_fcat optional
        value(iv_attach_name)  type so_obj_des optional
      exporting
        value(rv_result)       type abap_bool
      raising
        cx_bcs .
  private section.

    class-data mo_mail type ref to zcl_mail_util .
    data mv_subject type so_obj_des .
    data mv_content_type type so_obj_tp .
    data mt_content type soli_tab .
    data mt_recipient type somlreci1_t .
    data mo_alv_data type ref to data .
    data mt_fieldcat type lvc_t_fcat .
    data mv_attach_name type so_obj_des .

    methods set_attribute
      importing
        value(iv_subject)      type so_obj_des
        value(iv_content_type) type so_obj_tp
        value(it_content)      type soli_tab
        value(it_recipient)    type somlreci1_t
        value(ir_alv_data)     type ref to data optional
        value(it_fieldcat)     type lvc_t_fcat optional
        value(iv_attach_name)  type so_obj_des optional .

    methods check_parameters
      raising
        cx_bcs .
    methods convert_alv_to_xlsx
      returning
        value(rv_xml) type xstring
      raising
        cx_bcs .
    methods send_bcs_mail
      importing
        !iv_xml          type xstring
      returning
        value(rv_result) type abap_bool
      raising
        cx_bcs .

    methods clear_attribute.
ENDCLASS.



CLASS ZCL_MAIL_UTIL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MAIL_UTIL->CHECK_PARAMETERS
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] CX_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method check_parameters.
    if mv_subject is initial.
      raise exception type cx_bcs
        exporting
          error_type = cx_bcs=>parameter_error
          error_text = '邮件主题不能为空'.
    endif.

    if mt_recipient is initial.
      raise exception type cx_bcs
        exporting
          error_type = cx_bcs=>parameter_error
          error_text = '收件人不能为空'.
    endif.
    if mo_alv_data is bound or mt_fieldcat[] is not initial or mv_attach_name is not initial.
      if mo_alv_data is not bound.
        raise exception type cx_bcs
          exporting
            error_type = cx_bcs=>parameter_error
            error_text = '传入字段目录时，ALV数据引用不能为空'.
      endif.
      if mt_fieldcat[] is initial.
        raise exception type cx_bcs
          exporting
            error_type = cx_bcs=>parameter_error
            error_text = '传入ALV数据时，字段目录不能为空'.
      endif.

      if mv_attach_name  is initial.
        raise exception type cx_bcs
          exporting
            error_type = cx_bcs=>parameter_error
            error_text = '附件名称不能为空'.
      else.
        translate mv_attach_name to upper case.
        if substring_after( val  = mv_attach_name sub = '.'  ) <> 'XLSX'.
          mv_attach_name = mv_attach_name && '.xlsx'.
        endif.
      endif.
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MAIL_UTIL->CONVERT_ALV_TO_XLSX
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_XML                         TYPE        XSTRING
* | [!CX!] CX_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method convert_alv_to_xlsx.

    check mo_alv_data is bound and mt_fieldcat[] is not initial.

    cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
      exporting
        xml_version   = cl_salv_bs_a_xml_base=>get_version( )
        r_result_data = cl_salv_ex_util=>factory_result_data_table(
                          r_data         = mo_alv_data
                          t_fieldcatalog = mt_fieldcat
                        )
        xml_type      = if_salv_bs_xml=>c_type_xlsx
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      importing
        xml           = rv_xml
    ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MAIL_UTIL->SEND_BCS_MAIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XML                         TYPE        XSTRING
* | [<-()] RV_RESULT                      TYPE        ABAP_BOOL
* | [!CX!] CX_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method send_bcs_mail.
    data(lo_bcs) = cl_bcs=>create_persistent( ).

    data(lo_document) = cl_document_bcs=>create_document(
      i_type        = mv_content_type
      i_subject     = mv_subject
      i_importance  = '1'
      i_sensitivity = 'P'
      i_text        = mt_content[]
    ).

    if iv_xml is not initial.
      lo_document->add_attachment(
        i_attachment_type     = 'BIN'
        i_attachment_subject  = mv_attach_name
        i_att_content_hex     = cl_bcs_convert=>xstring_to_solix( iv_xml )
        i_attachment_language = '1'
        i_attachment_size     = conv so_obj_len( xstrlen( iv_xml ) )
      ).
    endif.

    lo_bcs->set_document( lo_document ).

    loop at mt_recipient assigning field-symbol(<fs_recipient>).
      lo_bcs->add_recipient(
        i_recipient = cl_cam_address_bcs=>create_internet_address(
                        i_address_string = conv ad_smtpadr( <fs_recipient>-receiver ) )
        i_copy      = <fs_recipient>-copy
         i_express  = 'X' ).
    endloop.


    lo_bcs->set_send_immediately( 'X' ).
    lo_bcs->send_request->set_link_to_outbox( 'X' ).

    rv_result = lo_bcs->send( i_with_error_screen = abap_false ).
    commit work and wait.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MAIL_UTIL->CLEAR_ATTRIBUTE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method clear_attribute.
    clear:
    mv_subject      ,
    mv_content_type ,
    mt_content      ,
    mt_recipient    ,
    mo_alv_data     ,
    mt_fieldcat     ,
    mv_attach_name  .
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MAIL_UTIL=>SEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SUBJECT                     TYPE        SO_OBJ_DES
* | [--->] IV_CONTENT_TYPE                TYPE        SO_OBJ_TP (default ='RAW')
* | [--->] IT_CONTENT                     TYPE        SOLI_TAB
* | [--->] IT_RECIPIENT                   TYPE        SOMLRECI1_T
* | [--->] IR_ALV_DATA                    TYPE REF TO DATA(optional)
* | [--->] IT_FIELDCAT                    TYPE        LVC_T_FCAT(optional)
* | [--->] IV_ATTACH_NAME                 TYPE        SO_OBJ_DES(optional)
* | [<---] RV_RESULT                      TYPE        ABAP_BOOL
* | [!CX!] CX_BCS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method send.
    if mo_mail is not bound .
      mo_mail = new zcl_mail_util(  ).
    endif.

    " 2. 记录变量
    mo_mail->set_attribute(
      iv_subject      = iv_subject
      iv_content_type = iv_content_type
      it_content      = it_content
      it_recipient    = it_recipient
      ir_alv_data     = ir_alv_data
      it_fieldcat     = it_fieldcat
      iv_attach_name  = iv_attach_name
    ).

    " 3. 参数检查
    mo_mail->check_parameters(  ).

    " 4. 发送邮件
    rv_result = mo_mail->send_bcs_mail( mo_mail->convert_alv_to_xlsx( ) ).

    " 5. 清除变量
    mo_mail->clear_attribute( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MAIL_UTIL->SET_ATTRIBUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SUBJECT                     TYPE        SO_OBJ_DES
* | [--->] IV_CONTENT_TYPE                TYPE        SO_OBJ_TP
* | [--->] IT_CONTENT                     TYPE        SOLI_TAB
* | [--->] IT_RECIPIENT                   TYPE        SOMLRECI1_T
* | [--->] IR_ALV_DATA                    TYPE REF TO DATA(optional)
* | [--->] IT_FIELDCAT                    TYPE        LVC_T_FCAT(optional)
* | [--->] IV_ATTACH_NAME                 TYPE        SO_OBJ_DES(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_attribute.
    mv_subject      = iv_subject.
    mv_content_type = iv_content_type.
    mt_content      = it_content.
    mt_recipient    = it_recipient.
    mo_alv_data     = ir_alv_data.
    mt_fieldcat     = it_fieldcat.
    mv_attach_name  = iv_attach_name.
  endmethod.
ENDCLASS.