*&---------------------------------------------------------------------*
*& Report ZRCA_BDC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrca_bdc NO STANDARD PAGE HEADING.

TABLES: sscrfields,t100.
**====================================================================*
*
**<*> NOTES:
*> Titles and headers: Programmable Logic Data Interface (PLDI)
*> Selection texts:
* DISMOD: * Call mode
* EXT: Extended logic
* FILE1: Data file  (Tab delimited)
* FILE2: Logic Level 1 (Tab delimited
* FILE3: Logic Level 2 (Tab delimited)
* FILE_EXT: Extended logic (Tab delimited)
* SESSION: * Session name
* START: Data start at row
* TCODE: Transaction code
* MASK: Mask

*> Text symbols
* 001: Batch Input (BDC Session)
* 002: PLDI was created on June.27.1997 by budi_rachman@yahoo.com
* 003: Release 3.2
* 004: Call transaction
* 005: Max. 64 columns & 128 characters per cell
* 006: Max. 128 columns & 64 characters per cell
* 007: Max. 30 columns & 40 characters per cell
* 008: Data Structure
* 009: Max. 234 columns & 35 characters per cell
**====================================================================*
*

*$*$*******************************************************************
*$*$*                  Data declarations                              *
*$*$*******************************************************************

* Structure Max.30 cols and 40 chars per cell
DATA: BEGIN OF  t30x040 OCCURS 100,
        b01(40), u01(40), d01(40), i01(40), r01(40),
        b02(40), u02(40), d02(40), i02(40), r02(40),
        b03(40), u03(40), d03(40), i03(40), r03(40),
        b04(40), u04(40), d04(40), i04(40), r04(40),
        b05(40), u05(40), d05(40), i05(40), r05(40),
        b06(40), u06(40), d06(40), i06(40), r06(40),
      END OF t30x040.

* Structure Max.128 cols and 64 chars per cell
DATA: BEGIN OF t128x64 OCCURS 1024,
        b001(64), r001(64), a001(64), c001(64),
        b002(64), r002(64), a002(64), c002(64),
        b003(64), r003(64), a003(64), c003(64),
        b004(64), r004(64), a004(64), c004(64),
        b005(64), r005(64), a005(64), c005(64),
        b006(64), r006(64), a006(64), c006(64),
        b007(64), r007(64), a007(64), c007(64),
        b008(64), r008(64), a008(64), c008(64),
        b009(64), r009(64), a009(64), c009(64),
        b010(64), r010(64), a010(64), c010(64),
        b011(64), r011(64), a011(64), c011(64),
        b012(64), r012(64), a012(64), c012(64),
        b013(64), r013(64), a013(64), c013(64),
        b014(64), r014(64), a014(64), c014(64),
        b015(64), r015(64), a015(64), c015(64),
        b016(64), r016(64), a016(64), c016(64),
        b017(64), r017(64), a017(64), c017(64),
        b018(64), r018(64), a018(64), c018(64),
        b019(64), r019(64), a019(64), c019(64),
        b020(64), r020(64), a020(64), c020(64),
        b021(64), r021(64), a021(64), c021(64),
        b022(64), r022(64), a022(64), c022(64),
        b023(64), r023(64), a023(64), c023(64),
        b024(64), r024(64), a024(64), c024(64),
        b025(64), r025(64), a025(64), c025(64),
        b026(64), r026(64), a026(64), c026(64),
        b027(64), r027(64), a027(64), c027(64),
        b028(64), r028(64), a028(64), c028(64),
        b029(64), r029(64), a029(64), c029(64),
        b030(64), r030(64), a030(64), c030(64),
        b031(64), r031(64), a031(64), c031(64),
        b032(64), r032(64), a032(64), c032(64),
      END OF t128x64.

* Structure Max.64 cols and 128 chars per cell
DATA: BEGIN OF t64x128 OCCURS 1024,
        b001(128), r001(128), a001(128), c001(128),
        b002(128), r002(128), a002(128), c002(128),
        b003(128), r003(128), a003(128), c003(128),
        b004(128), r004(128), a004(128), c004(128),
        b005(128), r005(128), a005(128), c005(128),
        b006(128), r006(128), a006(128), c006(128),
        b007(128), r007(128), a007(128), c007(128),
        b008(128), r008(128), a008(128), c008(128),
        b009(128), r009(128), a009(128), c009(128),
        b010(128), r010(128), a010(128), c010(128),
        b011(128), r011(128), a011(128), c011(128),
        b012(128), r012(128), a012(128), c012(128),
        b013(128), r013(128), a013(128), c013(128),
        b014(128), r014(128), a014(128), c014(128),
        b015(128), r015(128), a015(128), c015(128),
        b016(128), r016(128), a016(128), c016(128),
      END OF t64x128.

* Structure Max.234 cols and 35 chars per cell
DATA: BEGIN OF t234x35 OCCURS 1024,
        f01(35), a01(35),r01(35), a76(35), b01(35), i01(35),
        f02(35), a02(35),r02(35), a77(35), b02(35), i02(35),
        f03(35), a03(35),r03(35), a78(35), b03(35), i03(35),
        f04(35), a04(35),r04(35), a40(35), b04(35), i04(35),
        f05(35), a05(35),r05(35), a41(35), b05(35), i05(35),
        f06(35), a06(35),r06(35), a42(35), b06(35), i06(35),
        f07(35), a07(35),r07(35), a43(35), b07(35), i07(35),
        f08(35), a08(35),r08(35), a44(35), b08(35), i08(35),
        f09(35), a09(35),r09(35), a45(35), b09(35), i09(35),
        f10(35), a10(35),r10(35), a46(35), b10(35), i10(35),
        f11(35), a11(35),r11(35), a47(35), b11(35), i11(35),
        f12(35), a12(35),r12(35), a48(35), b12(35), i12(35),
        f13(35), a13(35),r13(35), a49(35), b13(35), i13(35),
        f14(35), a14(35),r14(35), a50(35), b14(35), i14(35),
        f15(35), a15(35),r15(35), a51(35), b15(35), i15(35),
        f16(35), a16(35),r16(35), a52(35), b16(35), i16(35),
        f17(35), a17(35),r17(35), a53(35), b17(35), i17(35),
        f18(35), a18(35),r18(35), a54(35), b18(35), i18(35),
        f19(35), a19(35),r19(35), a55(35), b19(35), i19(35),
        f20(35), a20(35),r20(35), a56(35), b20(35), i20(35),
        f21(35), a21(35),r21(35), a57(35), b21(35), i21(35),
        f22(35), a22(35),r22(35), a58(35), b22(35), i22(35),
        f23(35), a23(35),r23(35), a59(35), b23(35), i23(35),
        f24(35), a24(35),r24(35), a60(35), b24(35), i24(35),
        f25(35), a25(35),r25(35), a61(35), b25(35), i25(35),
        f26(35), a26(35),r26(35), a62(35), b26(35), i26(35),
        f27(35), a27(35),r27(35), a63(35), b27(35), i27(35),
        f28(35), a28(35),r28(35), a64(35), b28(35), i28(35),
        f29(35), a29(35),r29(35), a65(35), b29(35), i29(35),
        f30(35), a30(35),r30(35), a66(35), b30(35), i30(35),
        f31(35), a31(35),r31(35), a67(35), b31(35), i31(35),
        f32(35), a32(35),r32(35), a68(35), b32(35), i32(35),
        f33(35), a33(35),r33(35), a69(35), b33(35), i33(35),
        f34(35), a34(35),r34(35), a70(35), b34(35), i34(35),
        f35(35), a35(35),r35(35), a71(35), b35(35), i35(35),
        f36(35), a36(35),r36(35), a72(35), b36(35), i36(35),
        f37(35), a37(35),r37(35), a73(35), b37(35), i37(35),
        f38(35), a38(35),r38(35), a74(35), b38(35), i38(35),
        f39(35), a39(35),r39(35), a75(35), b39(35), i39(35),
      END OF t234x35.


FIELD-SYMBOLS <ptr>.

CONSTANTS: eof    VALUE '@',                   "End of file
           dat(3) VALUE 'DAT'.              "Data format

DATA:
  BEGIN OF i_logic1 OCCURS 100,
    name  LIKE bdcdata-fnam,
    value LIKE bdcdata-fval,
  END OF i_logic1.
DATA: i_logic2   LIKE i_logic1 OCCURS 100 WITH HEADER LINE,
      i_logic3   LIKE i_logic1 OCCURS 100 WITH HEADER LINE,
      i_bdctable LIKE bdcdata  OCCURS 100 WITH HEADER LINE,
      text(40),                        "Text 40 chars
      cacah      TYPE i.                    "Jumlah transaksi
*DATA IT_MESS LIKE BDCMSGCOLL OCCURS 0.
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF itab_result OCCURS 0,
        messtyp,"消息类型
        index   TYPE sy-tabix,
        message TYPE string, "消息
        box,
        light   LIKE icon-id,
      END OF itab_result.
TYPE-POOLS : slis.
DATA : fcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA : is_layout TYPE slis_layout_alv.


SELECTION-SCREEN BEGIN OF BLOCK b100 WITH FRAME TITLE TEXT-100.
  PARAMETERS: template LIKE rlgrap-filename.    "template file from SHDB
  PARAMETERS: textfile LIKE rlgrap-filename.
  SELECTION-SCREEN PUSHBUTTON /33(15) butok USER-COMMAND zget.
SELECTION-SCREEN END   OF BLOCK b100.

SELECTION-SCREEN BEGIN OF BLOCK b101 WITH FRAME TITLE TEXT-101.
  PARAMETERS:
    tcode    LIKE tstc-tcode,    "Transaction code
    file2    LIKE rlgrap-filename OBLIGATORY DEFAULT 'C:\',      "Logic-1
    file3    LIKE rlgrap-filename,                               "Logic-2
    file_ext LIKE rlgrap-filename DEFAULT space, "Extended logic
    file1    LIKE rlgrap-filename,           "Data
    mask     DEFAULT '&'.                     "Skip processing

  SELECT-OPTIONS:
    start FOR sy-tabix DEFAULT 1 TO 999999. "Rentang data

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: mode RADIOBUTTON GROUP clbi.
    SELECTION-SCREEN COMMENT  3(10) TEXT-001 .
    PARAMETERS: keep AS CHECKBOX.
    SELECTION-SCREEN COMMENT 15(10) TEXT-s03 FOR FIELD keep.
    SELECTION-SCREEN POSITION 52.
    PARAMETERS: temp RADIOBUTTON GROUP clbi DEFAULT 'X'.
    SELECTION-SCREEN COMMENT  54(20) TEXT-004 .
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN COMMENT  3(29) TEXT-010 .
    PARAMETERS:  session LIKE d0100-mapn DEFAULT sy-uname.

    SELECTION-SCREEN POSITION 52.
    SELECTION-SCREEN COMMENT  54(13) TEXT-011 .
    PARAMETERS: dismod LIKE ibipparms-callmode DEFAULT 'A'.
  SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK b101.

SELECTION-SCREEN BEGIN OF BLOCK btcx WITH FRAME TITLE TEXT-008.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: s30x40 RADIOBUTTON GROUP btcx.
    SELECTION-SCREEN COMMENT  3(79) TEXT-007.
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: s64x128 RADIOBUTTON GROUP btcx.
    SELECTION-SCREEN COMMENT  3(79) TEXT-005.
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: s128x64 RADIOBUTTON GROUP btcx.
    SELECTION-SCREEN COMMENT  3(79) TEXT-006.
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: s234x35 RADIOBUTTON GROUP btcx.
    SELECTION-SCREEN COMMENT  3(79) TEXT-009.
  SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK btcx.

SELECTION-SCREEN BEGIN OF BLOCK btci WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(79) TEXT-002.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK btci.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file1.
  PERFORM call_file USING file1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file2.
  PERFORM call_file USING file2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file_ext.
  PERFORM call_file USING file_ext.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file3.
  PERFORM call_file USING file3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR template.
  PERFORM call_file USING template.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR textfile.
  PERFORM call_file USING textfile.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'ZGET'.
    PERFORM generate_format_file USING template.
  ENDIF.

INITIALIZATION.
  MOVE '生成BDC格式文件' TO butok.
  %_tcode_%_app_%-text = '事务代码'.
  %_file1_%_app_%-text = '数据文件(Tab delimited)'.
  %_file2_%_app_%-text = 'BDC格式文件'.
  %_file3_%_app_%-text = '第二级格式文件'.
  %_file_ext_%_app_%-text = '第三级格式文件'.
  %_mask_%_app_%-text = '数据标示符'.
  %_mode_%_app_%-text = '模式'.
  %_start_%_app_%-text = '记录开始'.
  %_template_%_app_%-text = 'SHDB模板'.
  %_textfile_%_app_%-text = 'BDC格式文件'.
  %_dismod_%_app_%-text = '* 调用模式'.
  %b100000_block_1000 = '生成BDC格式文件'.
  %b101005_block_1000 = '数据导入'.
  %c001015_1000 = '生成会话'.
  %fs03017_1000 = '保留会话'.
  %c004020_1000 = '调用事务'.
  %c010023_1000 = '会话名称'.
  %c011026_1000 = '调用模式'.
  %b008030_block_1000 = 'Data Structure(数据结构)'.
  %c007033_1000 = '最大30列，每列最多40个字符'.
  %c005037_1000 = '最大64列，每列最多128个字符'.
  %c006041_1000 = '最大128列，每列最多64个字符'.
  %c009045_1000 = '最大234列，每列最多35个字符'.
  %b003048_block_1000 = '使用说明'.
  %c002050_1000 = '①SHDB录制②修改字段值,常量加"=",变量设成&1,&2…③下载模板④生成BDC格式文件⑤数据导入'.

*$*$*******************************************************************
*$*$*            Call main routine                                    *
*$*$*******************************************************************

START-OF-SELECTION.

  PERFORM main_program.
  PERFORM call_report.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'ZGET'.
      PERFORM generate_format_file USING template.
  ENDCASE.
*---------------------------------------------------------------------*
*       FORM MAIN_PROGRAM                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM main_program.
*  20090521 added by wellszhang
  IF tcode = '' OR tcode IS INITIAL.
    MESSAGE '事务代码不能为空，请检查!' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF file1 = '' OR file1 IS INITIAL.
    MESSAGE '数据文件不能为空，请检查!' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF temp = 'X'.
    REFRESH: messtab[],itab_result[].
    CLEAR: messtab,itab_result.
  ENDIF.
*  end of modification

* Logic level-1, level-2 and extended logic are being used
  IF file3 NE space AND file2 NE space AND file_ext NE space.
    PERFORM level_pldi USING 3.

* Only logic level-1 is being used
  ELSEIF file3 = space AND file2 NE space.
    PERFORM level_pldi USING 1.

** Logic level-1 and level-2 are being used
  ELSEIF file3 NE space AND file2 NE space.
    PERFORM level_pldi USING 2.

  ELSE.
    tcode = '<?>'.

  ENDIF.

ENDFORM.                    "MAIN_PROGRAM

*---------------------------------------------------------------------*
*       FORM CALL_REPORT                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM call_report.
*  20090521 added by wellszhang
  IF temp = 'X'.
    PERFORM show_alv.
  ENDIF.
*  end of modification
*> Report
  WRITE:/ 'Transaction code:', tcode.
  IF file1 EQ space.
    WRITE:/ 'Logic without data has been executed by', sy-uname.
  ELSEIF mode NE space.
    WRITE:/ 'There are ', cacah LEFT-JUSTIFIED.
    WRITE:  'records have been executed by', sy-uname.
    WRITE:/ 'which used batch input transaction.'.
    WRITE:/ 'Batch input name:', session, '.'.
    WRITE:/ 'Use SM35 for further execution.'.
  ELSE.
    WRITE:/ 'There are ', cacah LEFT-JUSTIFIED.
    WRITE:  'records have been executed by', sy-uname.
    WRITE:/ 'which used online transaction type', dismod, '.'.
  ENDIF.

ENDFORM.                    "CALL_REPORT

*---------------------------------------------------------------------*
*       FORM LEVEL_PLDI                                               *
*---------------------------------------------------------------------*
*       处理导入文件的数据结构（确定列数和列宽度）                                                     *
*---------------------------------------------------------------------*
*  -->  LOGIC_LEVEL                                                   *
*---------------------------------------------------------------------*
FORM level_pldi USING logic_level.

  IF s64x128 = 'X'.
    PERFORM call_pldi TABLES t64x128 USING logic_level.
  ELSEIF s128x64 = 'X'.
    PERFORM call_pldi TABLES t128x64 USING logic_level.
  ELSEIF s234x35 = 'X'.
    PERFORM call_pldi TABLES t234x35 USING logic_level.
  ELSE.
    PERFORM call_pldi TABLES t30x040 USING logic_level.
  ENDIF.

ENDFORM.                    "LEVEL_PLDI

*---------------------------------------------------------------------*
*       FORM CALL_PLDI                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DATA_STRUCT                                                   *
*  -->  LOGIC_LEVEL                                                   *
*---------------------------------------------------------------------*
FORM call_pldi TABLES data_struct USING logic_level.

  PERFORM zgenerate_data_convertion
    TABLES
            data_struct
            start
    USING
            tcode
            file1
            file2
            file3
            session
            mode
            dismod
            logic_level
            start-low
            file_ext
            cacah.

ENDFORM.                    "CALL_PLDI

*---------------------------------------------------------------------*
*       FORM ZGENERATE_DATA_CONVERTION                                *
*---------------------------------------------------------------------*
*       上传数据到内表                                                 *
*---------------------------------------------------------------------*
*  -->  ITAB                                                          *
*  -->  START                                                         *
*  -->  TCODE                                                         *
*  -->  FILEDATA                                                      *
*  -->  FILEBDC                                                       *
*  -->  FILECHILD                                                     *
*  -->  SESSION                                                       *
*  -->  MODE                                                          *
*  -->  DISMOD                                                        *
*  -->  LEVEL                                                         *
*  -->  AWAL                                                          *
*  -->  FILEEXTENDED                                                  *
*  -->  CACAH                                                         *
*---------------------------------------------------------------------*
FORM zgenerate_data_convertion
   TABLES
            itab            "数据内表
            start           "数据转换范围表
   USING
            tcode           "事务码
            filedata        "数据文件路径
            filebdc         "已转换的BDC文件路径
            filechild       "BDC子文件路径？？
            session         "会话名称？？用户名
            mode            "Batch Input (BDC Session)
            dismod          "transaction mode
            level           "逻辑结构层数
            awal            "数据转换开始行数
            fileextended    "Extended logic文件路径
            cacah.          "转换结果数

*######################################################################
*@ Rutin ini berfungsi untuk mengubah file logic dan file data menjadi
*@ bentuk yang dimengerti oleh ABAP, yaitu BDC table.
*######################################################################

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Standar WS_Upload yang mesti ada
*-----------------------------------*

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      filename            = filebdc
      filetype            = dat
    TABLES
      data_tab            = i_logic1
    EXCEPTIONS
      conversion_error    = 1
      file_open_error     = 2
      file_read_error     = 3
      invalid_table_width = 4
      invalid_type        = 5
      no_batch            = 6
      unknown_error       = 7
      OTHERS              = 8.

  IF filedata <> space.
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        filename            = filedata
        filetype            = dat
      TABLES
        data_tab            = itab
      EXCEPTIONS
        conversion_error    = 1
        file_open_error     = 2
        file_read_error     = 3
        invalid_table_width = 4
        invalid_type        = 5
        no_batch            = 6
        unknown_error       = 7
        OTHERS              = 8.
  ELSE.
    ASSIGN COMPONENT 1 OF STRUCTURE itab TO <ptr>.
    <ptr> = '/'. APPEND itab.
*    START = 1 .
  ENDIF.
*-------------------------------------------------*

*if sy-subrc = 0.
* Tahap inisialisasi *
  PERFORM open_prog USING mode session.

* Pengecekan level logic *
  IF level = 1.
    PERFORM level_1 TABLES itab start USING cacah dismod tcode mode.

  ELSEIF level = 2.
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        filename            = filechild
        filetype            = dat
      TABLES
        data_tab            = i_logic2
      EXCEPTIONS
        conversion_error    = 1
        file_open_error     = 2
        file_read_error     = 3
        invalid_table_width = 4
        invalid_type        = 5
        no_batch            = 6
        unknown_error       = 7
        OTHERS              = 8.

    PERFORM level_2 TABLES itab start USING cacah dismod tcode mode awal
 .

  ELSE.
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        filename            = filechild
        filetype            = dat
      TABLES
        data_tab            = i_logic2
      EXCEPTIONS
        conversion_error    = 1
        file_open_error     = 2
        file_read_error     = 3
        invalid_table_width = 4
        invalid_type        = 5
        no_batch            = 6
        unknown_error       = 7
        OTHERS              = 8.

    IF fileextended EQ space.
      CALL FUNCTION 'UPLOAD'
        EXPORTING
          filetype            = dat
        TABLES
          data_tab            = i_logic3
        EXCEPTIONS
          conversion_error    = 1
          file_open_error     = 2
          file_read_error     = 3
          invalid_table_width = 4
          invalid_type        = 5
          no_batch            = 6
          unknown_error       = 7
          OTHERS              = 8.
    ELSE.
      CALL FUNCTION 'WS_UPLOAD'
        EXPORTING
          filename            = fileextended
          filetype            = dat
        TABLES
          data_tab            = i_logic3
        EXCEPTIONS
          conversion_error    = 1
          file_open_error     = 2
          file_read_error     = 3
          invalid_table_width = 4
          invalid_type        = 5
          no_batch            = 6
          unknown_error       = 7
          OTHERS              = 8.
    ENDIF.

    PERFORM level_3 TABLES itab start USING cacah dismod tcode mode awal
 .
  ENDIF.

  PERFORM close_prog USING mode.

ENDFORM.                    "ZGENERATE_DATA_CONVERTION

*---------------------------------------------------------------------*
*       FORM CALL_FILE                                                *
*---------------------------------------------------------------------*
*      调用文件选择窗口，读取文件路径                                                     *
*---------------------------------------------------------------------*
*  -->  FILENAME                                                      *
*---------------------------------------------------------------------*
FORM call_file USING filename.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
*     PROGRAM_NAME  = SYST-REPID
      dynpro_number = syst-dynnr
      field_name    = 'PATH'
    IMPORTING
      file_name     = filename.

ENDFORM.                    "CALL_FILE


**^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^****
*** PLDI (Programmable Logic Data Interface)
*** Fungsi-fungsi standar untuk PLDI
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Konversi tabel 2 kolom menjadi tabel BDC *
*------------------------------------------*

FORM dynpro USING dynbegin name value.

  IF dynbegin =  'X'.
    CLEAR  i_bdctable.
    MOVE: name  TO i_bdctable-program,
          value TO i_bdctable-dynpro ,
          'X'   TO i_bdctable-dynbegin.
    APPEND i_bdctable.

  ELSE.

    CLEAR  i_bdctable.
    MOVE: name    TO i_bdctable-fnam,
          value   TO i_bdctable-fval.
    APPEND i_bdctable.

  ENDIF.

ENDFORM.                    "DYNPRO

*&---------------------------------------------------------------------
*
*&      Form  GENERATE_BDC
*&---------------------------------------------------------------------
*
*  Pemetaan dari logic file dan data file ke BDC map
*
*----------------------------------------------------------------------
*
*  -->  logic file dan data file
*  <--  BDC table
*----------------------------------------------------------------------
*
FORM generate_bdc TABLES logic_t STRUCTURE i_logic1 USING data_t.

  FIELD-SYMBOLS <ptr>.
  DATA nx TYPE i.

  LOOP AT logic_t FROM '2'.

    IF logic_t-name = eof OR
       ( logic_t-name = space AND logic_t-value = space ).
      EXIT.

    ELSEIF logic_t-name(1) = '<' OR logic_t-value = space.
      CONTINUE.

    ELSEIF logic_t-value(1) = '&'.
      nx = logic_t-value+1(4).
      ASSIGN COMPONENT nx OF STRUCTURE data_t TO <ptr>.
      IF <ptr> NE mask.
        PERFORM dynpro USING
          ' ' logic_t-name <ptr>.
      ELSE.
        CONTINUE.
      ENDIF.

    ELSEIF logic_t-value(1) = '='.
      PERFORM dynpro USING
        ' ' logic_t-name logic_t-value+1(131).

    ELSEIF logic_t-name = 'BDC_OKCODE' OR logic_t-name = space.
      PERFORM dynpro USING
        ' ' 'BDC_OKCODE' logic_t-value.

    ELSEIF logic_t-name = 'BDC_CURSOR' OR logic_t-name = 'CURSOR'.
      PERFORM dynpro USING
        ' ' 'BDC_CURSOR' logic_t-value.

    ELSE.
      PERFORM dynpro USING
        'X' logic_t-name logic_t-value.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " GENERATE_BDC


*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Diproses secara On-Line (Call Transaction)
* atau menggunakan Batch Session
*--------------------------------------------*

FORM execute USING mode tcode dismod cacah.
  DATA: exe(100).
  IF mode = 'X'.
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        tcode     = tcode
      TABLES
        dynprotab = i_bdctable.
  ELSE.
    REFRESH messtab[].
    CLEAR messtab.
    CALL TRANSACTION tcode
      USING i_bdctable
      MODE dismod
      UPDATE 'S'
      MESSAGES INTO messtab.
    IF sy-subrc <> 0 .
      cacah = cacah - 1 .
    ENDIF.

    DATA: l_mstring(480).
    itab_result-index = sy-tabix.
    LOOP AT messtab .
      SELECT SINGLE * FROM t100
               WHERE
                 sprsl = messtab-msgspra
                 AND arbgb = messtab-msgid
                 AND msgnr = messtab-msgnr.

      l_mstring = t100-text.
      IF l_mstring CS '&1'.
        REPLACE '&1'  WITH messtab-msgv1(37) INTO l_mstring.
        REPLACE '&2'  WITH messtab-msgv2(12) INTO l_mstring.
        REPLACE '&3'  WITH messtab-msgv3(12) INTO l_mstring.
        REPLACE '&4'  WITH messtab-msgv4(12) INTO l_mstring.
      ELSE.
        REPLACE '&'  WITH messtab-msgv1(12) INTO l_mstring.
        REPLACE '&'  WITH messtab-msgv2(12) INTO l_mstring.
        REPLACE '&'  WITH messtab-msgv3(12) INTO l_mstring.
        REPLACE '&'  WITH messtab-msgv4(12) INTO l_mstring.
      ENDIF.
      CONDENSE l_mstring.
      itab_result-messtyp = messtab-msgtyp.
      itab_result-message = l_mstring.
      IF itab_result-messtyp = 'E' OR itab_result-messtyp = 'A'.
        itab_result-light = '@5C@'.
*        EXIT.
      ELSEIF itab_result-messtyp = 'W'.
        itab_result-light = '@5D@'.
      ELSE.
        itab_result-light = '@5B@'.
      ENDIF.
      APPEND itab_result.
    ENDLOOP.


  ENDIF.
*  IF sy-subrc = 0 AND MODE <> 'X' .
*     EXE = 'Tcode: '.
*    WRITE TCODE TO EXE+7 LEFT-JUSTIFIED.
*    WRITE 'is being executed for record: ' TO EXE+12 LEFT-JUSTIFIED.
*    WRITE CACAH TO EXE+42 LEFT-JUSTIFIED.
*    PERFORM PROSES USING EXE.
*  ENDIF.
ENDFORM.                               " SAVE_IT


*~~~~~~~~~~~~~~*
* Inisialisasi *
*--------------*

FORM open_prog USING mode session.
  DATA: text1(80).

  IF mode = 'X'.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client = sy-mandt
        group  = session
        user   = sy-uname
        keep   = keep.
  ENDIF.

ENDFORM.                    "OPEN_PROG

*~~~~~~~~~*
* Closing *
*---------*

FORM close_prog USING mode.

  IF mode = 'X'.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
  ENDIF.

ENDFORM.                               " CLOSE_PROG

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Indikator sedang melakukan proses *
*-----------------------------------*

FORM proses USING text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = text
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                    "PROSES


*~~~~~~~~~~~~~~~*
* Logic level 1 *
*---------------*
FORM level_1 TABLES itab start USING cacah dismod tcode mode.

  LOOP AT itab.
    IF sy-tabix IN start.
      ASSIGN COMPONENT 1 OF STRUCTURE itab TO <ptr>.
      IF <ptr> = eof. EXIT. ENDIF.
      REFRESH i_bdctable.
      PERFORM generate_bdc TABLES i_logic1 USING itab.
      ADD 1 TO cacah.
      PERFORM execute USING mode tcode dismod cacah.
    ENDIF.
  ENDLOOP.

ENDFORM.                                                    "LEVEL_1

*~~~~~~~~~~~~~~~*
* Logic level 2 *
*---------------*
FORM level_2 TABLES itab start USING cacah dismod tcode mode awal.

  LOOP AT itab.                        "ere sy-tabix in start.
    IF sy-tabix IN start.
      ASSIGN COMPONENT 1 OF STRUCTURE itab TO <ptr>.
      IF <ptr> = eof. EXIT. ENDIF.
      text = 'Processing at row '.
      WRITE sy-tabix TO text+18 LEFT-JUSTIFIED.
      PERFORM proses USING text.

      IF <ptr> NE space.
        IF sy-tabix NE awal.
          PERFORM dynpro USING ' ' 'BDC_OKCODE' '/11'.
          PERFORM execute USING mode tcode dismod cacah.
        ENDIF.
        REFRESH i_bdctable.
        PERFORM generate_bdc TABLES i_logic1 USING itab.
        ADD 1 TO cacah.
      ELSE.
        PERFORM generate_bdc TABLES i_logic2 USING itab.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM dynpro USING ' ' 'BDC_OKCODE' '/11'.
  PERFORM execute USING mode tcode dismod cacah.
ENDFORM.                                                    "LEVEL_2

*~~~~~~~~~~~~~~~~*
* Extended logic *
*----------------*
FORM level_3 TABLES itab start USING cacah dismod tcode mode awal.

  DATA: data_ext(8192), temp(8192),
        sub_item VALUE space.

  LOOP AT itab.

    IF sy-tabix IN start.

      ASSIGN COMPONENT 1 OF STRUCTURE itab TO <ptr>.
** Check End-Of-File
      IF <ptr> = eof.
        EXIT.
      ENDIF.

      text = 'Processing at row '.
      WRITE sy-tabix TO text+18 LEFT-JUSTIFIED.
      PERFORM proses USING text.

** Check if any sub-item data or extended data uses '#' symbol
      IF <ptr> = '#'.                  "->Sub-item or extended-data
        PERFORM generate_bdc TABLES i_logic3 USING itab.
        sub_item = 'X'.     "->Sub-item data flag is activated
        CONTINUE.
      ENDIF.

** Process BDC for header data
      IF <ptr> NE space.
        IF sy-tabix NE awal.
          READ TABLE i_logic3 INDEX 2.
** There is no sub_item data or no extended data with '#' symbol
** or no extended logic
          IF i_logic3 NE space AND sub_item EQ space.
** Read extended data in the last item
            MOVE: itab TO temp,
                  data_ext TO itab.
            PERFORM generate_bdc TABLES i_logic3 USING itab.
            MOVE temp TO itab.
          ELSE.
** There is sub_item data or extended data with '#' symbol
** or no extended logic
** If sub-items are processed, it is saved automatically.
            PERFORM dynpro USING ' ' 'BDC_OKCODE' '/11'.
          ENDIF.
          PERFORM execute USING mode tcode dismod cacah.
          CLEAR sub_item.              "->Reset flag sub-item
        ENDIF.
        REFRESH i_bdctable.
        PERFORM generate_bdc TABLES i_logic1 USING itab.
        ADD 1 TO cacah.
      ELSE.
** Check whether data contains sub_item data
        IF sub_item = 'X'.
** After all sub-items are processed, back to item.
          PERFORM dynpro USING ' ' 'BDC_OKCODE' '/3'.
        ENDIF.
        PERFORM generate_bdc TABLES i_logic2 USING itab.
** Store last data into temporary field
        MOVE itab TO data_ext.
      ENDIF.
    ENDIF.
  ENDLOOP.

  READ TABLE i_logic3 INDEX 2.
  IF i_logic3 NE space AND sub_item EQ space.
** No sub-item data or extended data with '#' symbol
** and there is extended logic
    MOVE: data_ext TO itab.
    PERFORM generate_bdc TABLES i_logic3 USING itab.
  ELSE.
** There is sub-item or extended data, last extended data has '#'
** If sub-items are processed, it is saved automatically.
    PERFORM dynpro USING ' ' 'BDC_OKCODE' '/11'.
  ENDIF.
  PERFORM execute USING mode tcode dismod cacah.
ENDFORM.                                                    "LEVEL_3

*&---------------------------------------------------------------------
*
*&      Form  GENERATE_FORMAT_FILE
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*      -->P_TEMPLATE  text
*----------------------------------------------------------------------
*
FORM generate_format_file USING  template.
  DATA: bdctab TYPE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF outtext OCCURS 0,
          fnam LIKE bdcdata-fnam,
          fval LIKE bdcdata-fval,
        END OF outtext.

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      filename            = template
      filetype            = dat
    TABLES
      data_tab            = bdctab
    EXCEPTIONS
      conversion_error    = 1
      file_open_error     = 2
      file_read_error     = 3
      invalid_table_width = 4
      invalid_type        = 5
      no_batch            = 6
      unknown_error       = 7
      OTHERS              = 8.
  IF sy-subrc NE 0 OR bdctab[] IS INITIAL.
    WRITE: 'ERROR OPEN TEMPLATE FILE, PLEASE CHECK AGAIN!'.
    EXIT.
  ENDIF.
  MOVE 'NAME' TO outtext-fnam.
  MOVE 'VALUE' TO outtext-fval.
  INSERT outtext INDEX 1.
  DELETE bdctab INDEX 1.
  LOOP AT bdctab.
    IF NOT ( bdctab-program IS INITIAL ).
      outtext-fnam = bdctab-program.
      outtext-fval = bdctab-dynpro.
      APPEND outtext.
      CONTINUE.
    ELSEIF ( bdctab-fnam NE 'BDC_CURSOR' AND bdctab-fnam NE 'BDC_SUBSCR' ).
      outtext-fnam = bdctab-fnam.
      outtext-fval = bdctab-fval.
      APPEND outtext.
    ENDIF.
  ENDLOOP.
  CLEAR outtext.
  MOVE '@' TO outtext-fnam.
  APPEND outtext.
  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                  = ' '
*     CODEPAGE = ' '
      filename = textfile
      filetype = 'DAT'
*     MODE     = ' '
*     WK1_N_FORMAT                  = ' '
*     WK1_N_SIZE                    = ' '
*     WK1_T_FORMAT                  = ' '
*     WK1_T_SIZE                    = ' '
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
* IMPORTING
*     FILELENGTH                    =
    TABLES
      data_tab = outtext
*     FIELDNAMES                    =
* EXCEPTIONS
*     FILE_OPEN_ERROR               = 1
*     FILE_WRITE_ERROR              = 2
*     INVALID_FILESIZE              = 3
*     INVALID_TYPE                  = 4
*     NO_BATCH = 5
*     UNKNOWN_ERROR                 = 6
*     INVALID_TABLE_WIDTH           = 7
*     GUI_REFUSE_FILETRANSFER       = 8
*     CUSTOMER_ERROR                = 9
*     OTHERS   = 10
    .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE '生成成功' TYPE 'S'.
  ENDIF.
  LOOP AT outtext.
    WRITE: /(20) outtext-fnam, (20) outtext-fval.
  ENDLOOP.
ENDFORM.                    " GENERATE_FORMAT_FILE
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*20090521 added by WellsZhang
FORM show_alv .
  is_layout-colwidth_optimize = 'X'.
  CLEAR fcat.
  REFRESH fcat.
  PERFORM fcat_built USING 'LIGHT' '结果'.
  PERFORM fcat_built USING 'MESSTYP' '类型'.
  PERFORM fcat_built USING 'INDEX' '索引'.
  PERFORM fcat_built USING 'MESSAGE' '信息'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = 'PF_STATUS_SET'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = 'TOP_OF_PAGE'
*     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND-ALV'
      is_layout          = is_layout
      it_fieldcat        = fcat[]
      i_save             = 'A'
*     I_HTML_HEIGHT_TOP  = 12
    TABLES
      t_outtab           = itab_result
* EXCEPTIONS
*     PROGRAM_ERROR      = 1
*     OTHERS             = 2
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  fcat_built
*&---------------------------------------------------------------------*

FORM fcat_built  USING    p_fieldname p_seltext .
  fcat-fieldname  = p_fieldname.
  fcat-seltext_m  = p_seltext.
  APPEND fcat.
  CLEAR fcat.
ENDFORM.                    " fcat_built
*end of modification