*&---------------------------------------------------------------------*
*& Report ZFIC201
*&---------------------------------------------------------------------*
REPORT ZFIC201.
*&---------------------------------------------------------------------*
*& 使用说明
*& 1) 如果导入中文科目，请在中文环境下操作
*& 2) FS00界面中，不显示左边的navigation tree
*&
*&---------------------------------------------------------------------*
* For ALV display
type-pools: slis.
data: gt_fieldcat type slis_t_fieldcat_alv with header line,
      gs_layout   type slis_layout_alv.

* 处理Tab分割符
class cl_abap_char_utilities definition load.
constants: c_tab type c value cl_abap_char_utilities=>horizontal_tab.

tables:
  ska1, skb1, skat.

data:  gt_bdcdata like bdcdata occurs 0 with header line,       " 用于储存操作信息
       gt_bdcmsgcoll like bdcmsgcoll occurs 0 with header line. " 存储返回信息

* 上传的文件结构须与此内表相同
data: begin of itab occurs 0,
        saknr(010),    " G/L account
        bukrs(004),    " Company code
        glaccount_type(001), " G/L account type
        ktoks(004),    " Account group
        txt20(020),    " Short text
        txt50(050),    " Lont text
        waers(005),    " Account currency
        xsalh(001),    " Only balances in local currency
        mwskz(002),    " Tax category
        xmwno(001),    " Allow posting without tax code
        mitkz(001),    " Recon account type
        xopvw(001),    " Open item management
        KATYP(002),    " Cost element type
        zuawa(003),    " Sort key
        fstag(004),    " Field status group
        xintb(001),    " System posting only
        txt20en(020),   " short text English
        txt50en(050),   " long text English
     end of itab.

* 日志信息
data:gs_data LIKE LINE OF itab.
data: begin of gt_log occurs 0,
        saknr(010),  " G/L account number
        type(1),     " messange type
        txt100(100), " message text
      end of gt_log.


*----------------------------------------------------------------------*
*        SELECTION SCREEN                                              *
*----------------------------------------------------------------------*
selection-screen begin of block blk1 with frame title text01.
* Parameter for file name
parameters: p_file type localfile obligatory default 'C:\会计科目主数据模板BDC.xls'.
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text02.
* Parameters for update mode
parameters: mode_a  radiobutton group rad type c,  " Display All
            mode_n  radiobutton group rad type c,  " Display nothing
            moed_e  radiobutton group rad type c.  " Display error only
selection-screen end of block blk2.

initialization.
  text01 = '导入文件'.
  text02 = '更新模式'.

at selection-screen on value-request for p_file.
* Get file full name using dialog
  call function 'KD_GET_FILENAME_ON_F4'
    exporting
      mask      = p_file
      static    = ' '
    changing
      file_name = p_file.


start-of-selection.
* upload file to internal table itab
*  perform upload_file using p_file changing itab[].
  perform frm_upload_file CHANGING itab[].
  perform check_data.

end-of-selection.
  if gt_log[] is initial.
    perform upload_data.
    perform alv_show.
  else.
    perform alv_show.
  endif.

*&---------------------------------------------------------------------*
*&      Form  write_Log_err
*&---------------------------------------------------------------------*
*       Write error message to log internal table
*----------------------------------------------------------------------*
*      -->SAKNR      text
*      -->MESS_TYPE  text
*      -->MESSAGE    text
*----------------------------------------------------------------------*
form write_log_err using saknr message.

  clear gt_log.
  gt_log-saknr = saknr.
  gt_log-type = 'E'.
  gt_log-txt100 = message.
  append gt_log.

endform.                    "write_Log_err

*&--------------------------------------------------------------------*
*&      Form  check_data
*&--------------------------------------------------------------------*
*  执行数据检查
*  不用检查科目主数据在COA level或者company code是否存在，由系统检查
*---------------------------------------------------------------------*
form check_data.
  data: l_ktopl like ska1-ktopl.  " chart of account
  data: message type string.

* Delete lines where G/L Account is blank
  delete itab where saknr = space.

  loop at itab.
* Add leading zeros for g/l account
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = itab-saknr
      importing
        output = itab-saknr
      exceptions
        others = 1.

* Company code cannot be blank
    if itab-bukrs = space .
      concatenate itab-saknr '公司代码未维护.' into message.
      perform write_log_err using itab-saknr message.
    endif.

*   Get chart of account from company code
    select single ktopl into l_ktopl
      from t001
      where bukrs = itab-bukrs.

    if itab-txt20 = space .
      concatenate itab-saknr '短文本未维护.' into message.
      perform write_log_err using itab-saknr message.
    endif.

    if itab-txt50 = space .
      concatenate itab-saknr '长文本未维护.' into message.
      perform write_log_err using itab-saknr message.
    endif.

    if itab-waers = space .
      concatenate itab-saknr '币别未维护.' into message.
      perform write_log_err using itab-saknr message.
    endif.

    if itab-fstag eq space .
      concatenate itab-saknr '字段状态组未维护.' into message.
      perform write_log_err using itab-saknr message.
    endif.
  endloop.
endform.                    "check_data

*&--------------------------------------------------------------------*
*&      Form  upload_data
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form upload_data.
  data: l_mode.
  data: l_errorinfo type char100.

* Determine Mode
  if mode_a = 'X'.      " Display All
    l_mode = 'A'.
  elseif mode_n = 'X'.  " Display Nothing
    l_mode = 'N'.
  else.
    l_mode = 'E'.       " Display errors
  endif.

  loop at itab.
    clear gt_bdcdata.
    refresh gt_bdcdata.

*   Enter int the new screen
    perform bdc_dynpro      using 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.
    perform bdc_field       using 'BDC_OKCODE' '=ACC_CRE'.
    perform bdc_field       using 'BDC_CURSOR' 'GLACCOUNT_SCREEN_KEY-BUKRS' .

*   G/L account number
    perform bdc_field       using 'GLACCOUNT_SCREEN_KEY-SAKNR' itab-saknr.

*   Company code
    perform bdc_field       using 'GLACCOUNT_SCREEN_KEY-BUKRS' itab-bukrs.

*   Screen
    perform bdc_dynpro      using 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.

    perform bdc_field       using 'BDC_OKCODE' '=2102_GROUP'.

*   Account group
    perform bdc_field       using 'BDC_CURSOR' 'GLACCOUNT_SCREEN_COA-KTOKS'.
    perform bdc_field       using 'GLACCOUNT_SCREEN_COA-KTOKS' itab-ktoks.

*   Account type
    perform bdc_field       using 'GLACCOUNT_SCREEN_COA-GLACCOUNT_TYPE' itab-glaccount_type.

*   Screen
    perform bdc_dynpro      using 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.
    perform bdc_field       using 'BDC_OKCODE' '=TAB02'.

*   TXT20 & TXT50
    perform bdc_field       using 'BDC_CURSOR' 'GLACCOUNT_SCREEN_COA-TXT50_ML'.
    perform bdc_field       using 'GLACCOUNT_SCREEN_COA-TXT20_ML' itab-txt20 .
    perform bdc_field       using 'GLACCOUNT_SCREEN_COA-TXT50_ML' itab-txt50.

*   Screen
    perform bdc_dynpro      using 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.
    perform bdc_field       using 'BDC_OKCODE' '=TAB03'.

*   Currency
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-WAERS'  itab-waers.

*   Only balances in local currency
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-XSALH' itab-xsalh.

*   Tax category
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-MWSKZ' itab-mwskz.

*   Allow posting without tax
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-XMWNO' itab-xmwno.

*   Cost element type
    if not itab-katyp is initial.
      perform bdc_field      using 'GLACCOUNT_SCREEN_CAREA-KATYP' itab-katyp .
    endif.

*   Sor key
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-ZUAWA' itab-zuawa.

*   Reconcilation account type
    if not itab-mitkz is initial.
      perform bdc_field     using 'GLACCOUNT_SCREEN_CCODE-MITKZ' itab-mitkz.
    endif.

*   Open item management
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-XOPVW' itab-xopvw.

*   Sor key
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-ZUAWA' itab-zuawa.

*   Screen
    perform bdc_dynpro      using 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.
    perform bdc_field       using 'BDC_OKCODE' '=TAB04'.

*   Field status group
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-FSTAG' itab-fstag.

*   Post automatically only
    perform bdc_field       using 'GLACCOUNT_SCREEN_CCODE-XINTB' itab-xintb.

*   Screen
    perform bdc_dynpro      using 'SAPLGL_ACCOUNT_MASTER_MAINTAIN' '2001'.
    perform bdc_field       using 'BDC_OKCODE'  '=SAVE'.

*   English text
    perform bdc_field      using 'BDC_CURSOR' 'GLACCOUNT_SCREEN_COA-TXT20_TX(02)'.
    perform bdc_field      using 'GLACCOUNT_SCREEN_COA-LANGU_TX(02)' 'EN'.
    perform bdc_field      using 'GLACCOUNT_SCREEN_COA-TXT20_TX(02)' itab-txt20en.
    perform bdc_field      using 'GLACCOUNT_SCREEN_COA-TXT50_TX(02)' itab-txt50en.

*   调用FS00 tcode
    clear gt_bdcmsgcoll.
    call transaction 'FS00' using gt_bdcdata
         mode  l_mode
         update 'S'  " S为同步更新模式
         messages into gt_bdcmsgcoll.  " 消息保存

*  处理系统返回信息
*  如果处理成功，gt_bdcmsgcoll为空
    if gt_bdcmsgcoll[] is initial.
      clear gt_log.
      gt_log-saknr = itab-saknr.
      concatenate itab-saknr '科目导入成功.' into gt_log-txt100.
      gt_log-type = 'S'.
      append gt_log.
    else.
      " print all error messages using gt_bdcmsgcoll.
      loop at gt_bdcmsgcoll.
        clear l_errorinfo.

        call function 'MESSAGE_TEXT_BUILD'
          exporting
            msgid                     = gt_bdcmsgcoll-msgid
            msgnr                     = gt_bdcmsgcoll-msgnr
            msgv1                     = gt_bdcmsgcoll-msgv1
            msgv2                     = gt_bdcmsgcoll-msgv2
*           MSGV3                     = ' '
*           MSGV4                     = ' '
         importing
           message_text_output       = l_errorinfo .

          perform write_log_err using itab-saknr l_errorinfo.
          clear gt_bdcmsgcoll.
      endloop.
    endif.
  endloop.
endform.                    "upload_data

*----------------------------------------------------------------------*
*        Start new screen
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear gt_bdcdata.
  gt_bdcdata-program  = program.
  gt_bdcdata-dynpro   = dynpro.
  gt_bdcdata-dynbegin = 'X'.
  append gt_bdcdata.
endform.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  clear gt_bdcdata.
  gt_bdcdata-fnam = fnam. " field name
  gt_bdcdata-fval = fval. " field value
  append gt_bdcdata.
endform.                    "BDC_FIELD

*&--------------------------------------------------------------------*
*&      Form  upload_file
*&--------------------------------------------------------------------*
*       上传文件至内表(t_datatab)
*---------------------------------------------------------------------*
form upload_file using in_file type localfile
              changing t_datatab type standard table.

  data: l_filetype type char10.
  data: l_file type string.
  data: l_itab type standard table of itab.

* cl_gui_frontend_services=>gui_upload will call GUI_UPLOAD function
* in which the filename parameter is of type String
* so we need to convert
  l_file = in_file.

  call method cl_gui_frontend_services=>gui_upload
    exporting
      filename            = l_file   " upload file
      filetype            = 'ASC'
      has_field_separator = c_tab  " Has separator
    changing
      data_tab            = t_datatab " import to this intenal table
    exceptions
      file_open_error     = 1
      file_read_error     = 2
      invalid_type        = 3
      no_batch            = 4
      unknown_error       = 5
      others              = 6.

  if sy-subrc <> 0.
    message  '文件上传出错.' TYPE 'E'.
  endif.
endform.                    "Upload_file



*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FIELD_NAME    text
*      -->FIELD_TEXT    text
*      -->FIELD_LENGTH  text
*----------------------------------------------------------------------*
form fieldcat_init using field_name
                         field_text
                         field_length type i.

  data: ls_fieldcat type slis_fieldcat_alv.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = field_name.
  ls_fieldcat-seltext_l = field_text.
  ls_fieldcat-seltext_m = field_text.
  ls_fieldcat-seltext_s = field_text.
  ls_fieldcat-outputlen = field_length.
  append ls_fieldcat to gt_fieldcat.
endform.                    "Fieldcat_init

*&--------------------------------------------------------------------*
*&      Form  ALV_SHOW
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form alv_show.
* Populate fieldcatalog
  perform fieldcat_init using : 'SAKNR' '总帐科目'  10 .
  perform fieldcat_init using : 'TYPE' '类型'  4 .
  perform fieldcat_init using : 'TXT100' '描述' 100.

  call function 'REUSE_ALV_LIST_DISPLAY'
    exporting
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat[]
    tables
      t_outtab           = gt_log
    exceptions
      program_error      = 1
      others             = 2.
endform.                    "output

FORM frm_upload_file  CHANGING ct_data TYPE STANDARD TABLE.


  DATA:lt_file   TYPE STANDARD TABLE OF alsmex_tabline,
       ls_file   TYPE alsmex_tabline,
       lv_filenm LIKE rlgrap-filename.


  FIELD-SYMBOLS: <lfs_value>,
                 <lfs_data> TYPE any.

  ASSIGN ('GS_DATA') TO <lfs_data>.

* File
  lv_filenm = p_file.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_filenm
      i_begin_col             = 1
      i_begin_row             = 2"
      i_end_col               = 18     " 最大列数
      i_end_row               = 10000   "最大行数
    TABLES
      intern                  = lt_file
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
    LOOP AT lt_file INTO ls_file.
      DATA(ls_file3) = ls_file.

      ASSIGN COMPONENT ls_file-col OF STRUCTURE <lfs_data> TO <lfs_value>.  "动态方法将值传到相应的内表
      IF <lfs_value> IS ASSIGNED.
        <lfs_value> = ls_file-value.
      ENDIF.

      AT END OF row.

        IF <lfs_data> IS ASSIGNED.
          APPEND <lfs_data> TO ct_data.
          CLEAR  <lfs_data>.
        ENDIF.
      ENDAT.

      CLEAR: ls_file.

    ENDLOOP.

  ENDCATCH.
ENDFORM.
