"Description: 序时账
*&---------------------------------------------------------------------*
*& Report ZFIR009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir009.
TABLES: bkpf,acdoca,kna1,cskt.

DATA: is_layout_lvc TYPE lvc_s_layo.
DATA: it_fieldcat_lvc TYPE lvc_t_fcat WITH HEADER LINE.
DATA: it_sort_lvc TYPE lvc_t_sort WITH HEADER LINE.
DATA: g_repid TYPE sy-repid.

DATA: BEGIN OF gs_out,
*****************       常规数据     ************************************
        bukrs     LIKE bkpf-bukrs, " 公司代码
        budat     LIKE bkpf-budat, "  过账日期
        cpudt     LIKE bkpf-cpudt,  " 录入日期
        usnam     LIKE bkpf-usnam, "  制单人
        racct     LIKE acdoca-racct, "  科目编号
        xblnr     LIKE bkpf-xblnr, "  摘要
        kunnr     LIKE acdoca-kunnr, "   客户
        lifnr     LIKE acdoca-lifnr, "  供应商
        anln1     LIKE acdoca-anln1, " 资产
        ltext     LIKE t003t-ltext, " 凭证描述
*        bktxt     LIKE bkpf-bktxt,
        stblg     LIKE bkpf-stblg,
        docln     LIKE acdoca-docln, " 凭证行项目
        hsl       LIKE acdoca-hsl, " 总金额
        name_text LIKE adrp-name_text, " 制单人（描述）
        txt50     LIKE skat-txt50, " 会计科目描述
        drcrk     LIKE acdoca-drcrk, " h/s
        blart     LIKE acdoca-blart, " 凭证类型
        belnr     LIKE bkpf-belnr, "  凭证编号
        bktxt     LIKE bkpf-bktxt,
***************          附加数据        *****************************
        sel       TYPE c,
        zfux      LIKE acdoca-anln1, "辅助项
        zfuxms    LIKE anla-txt50,
        sdmhab    TYPE sdmhab, " 总分类帐科目:借方金额
        sdmsol    TYPE sdmsol, " 总分类帐科目:贷方金额
        rcntr     TYPE acdoca-rcntr, " 成本中心
        ltext2    TYPE cskt-ltext,  " 成本中心描述


      END OF gs_out.
DATA: gt_out LIKE TABLE OF gs_out.


*&----------------------------------------------------------------------&
*&                             选择屏幕
*&----------------------------------------------------------------------&
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_bukrs LIKE bkpf-bukrs OBLIGATORY. "公司代码

  SELECT-OPTIONS:  s_budat FOR bkpf-budat , "过账日期
                   s_cpdut FOR bkpf-cpudt DEFAULT sy-datum,  " 录入日期
                   s_blart FOR bkpf-blart, " 凭证类型
                   s_belnr FOR bkpf-belnr. " 凭证编号

  SELECT-OPTIONS: p_usnam FOR bkpf-usnam NO-EXTENSION NO INTERVALS.  " 制单人

  SELECT-OPTIONS: s_racct FOR acdoca-racct. " 科目编号

  SELECT-OPTIONS: p_xblnr FOR bkpf-xblnr NO-EXTENSION NO INTERVALS, " 摘要
                  p_drcrk FOR acdoca-drcrk NO-EXTENSION NO INTERVALS. " 方向

  SELECT-OPTIONS:  s_kunnr FOR acdoca-kunnr, " 客户
                   s_lifnr FOR acdoca-lifnr, " 供应商
                   s_anln1 FOR acdoca-anln1.  " 资产

SELECTION-SCREEN END OF BLOCK b1.



*&----------------------------------------------------------------------&
*&                             主程序
*&----------------------------------------------------------------------&

START-OF-SELECTION.
  PERFORM check_data.
  PERFORM: get_data.
  PERFORM: alv_disp.

*&----------------------------------------------------------------------&
*&                             FORM GET_DATA
*&----------------------------------------------------------------------&
FORM get_data.

  DATA: ls_out LIKE gs_out,
        lt_out LIKE TABLE OF ls_out.


  SELECT a~docln a~hsl a~racct a~drcrk a~kunnr a~lifnr a~anln1 a~rcntr
         b~bukrs b~budat b~blart b~belnr b~usnam b~xblnr b~bktxt b~cpudt
         b~stblg
    FROM acdoca AS a INNER JOIN bkpf AS b
    ON   a~rbukrs = b~bukrs
    AND a~gjahr = b~gjahr
    AND a~belnr = b~belnr
  "  AND a~rldnr = b~rldnr
    INTO CORRESPONDING FIELDS OF TABLE gt_out

    WHERE b~bukrs = p_bukrs
    AND b~budat IN s_budat
    AND b~cpudt IN s_cpdut
    AND b~blart IN s_blart
    AND b~belnr IN s_belnr
    AND b~usnam IN p_usnam
    AND a~racct IN s_racct
    AND b~xblnr IN p_xblnr
    AND a~drcrk IN p_drcrk
    AND a~kunnr IN s_kunnr
    AND a~lifnr IN s_lifnr
    AND a~anln1 IN s_anln1
    .

  IF gt_out[] IS NOT INITIAL.
    " 凭证类型描述LTEXT
    SELECT blart,ltext  FROM t003t
      INTO TABLE @DATA(lt_t003t)
      WHERE spras = '1'.
    " 从表USR21中取人员编号PERSNUMBER
    SELECT bname,persnumber FROM usr21
      INTO TABLE @DATA(lt_usr21).
    " 表ADRP中取制单人描述NAME_TEXT
    IF lt_usr21[] IS NOT INITIAL.
      SELECT persnumber,name_text FROM adrp
      INTO TABLE @DATA(lt_adrp)
            FOR ALL ENTRIES IN @lt_usr21
            WHERE persnumber = @lt_usr21-persnumber.
    ENDIF.

    " 判断统驭科目
    SELECT saknr,bukrs,mitkz
      FROM skb1 INTO TABLE @DATA(lt_skb1)
       FOR ALL ENTRIES IN @gt_out
      WHERE saknr = @gt_out-racct
      AND bukrs = @gt_out-bukrs.

    SELECT saknr,txt50 FROM skat
      INTO TABLE @DATA(lt_skat)
      FOR ALL ENTRIES IN @gt_out
      WHERE saknr = @gt_out-racct
      AND ktopl = 'ZZZZ'
      AND spras = '1'.

    " 客户名称
    SELECT name1,kunnr FROM kna1
      INTO TABLE @DATA(lt_kna1)
      FOR ALL ENTRIES IN @gt_out
      WHERE kunnr = @gt_out-kunnr.

    " 供应商名称
    SELECT name1,lifnr FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
      FOR ALL ENTRIES IN @gt_out
      WHERE lifnr = @gt_out-lifnr.

    " 资产描述。
    SELECT txt50,bukrs,anln1 FROM anla
      INTO TABLE @DATA(lt_anla)
      FOR ALL ENTRIES IN @gt_out
      WHERE bukrs = @gt_out-bukrs
      AND anln1 = @gt_out-anln1.


    SELECT ltext AS ltext2,kostl
      FROM cskt
      INTO TABLE @DATA(lt_cskt)
      FOR ALL ENTRIES IN @gt_out
      WHERE kostl = @gt_out-rcntr
      AND spras = '1'
      AND kokrs = 'ZZZZ'.

    "排序
    SORT lt_t003t BY blart.
    SORT lt_usr21 BY bname.
    SORT lt_adrp BY persnumber.
    SORT lt_skb1 BY saknr bukrs.
    SORT lt_skat BY saknr.
    SORT lt_kna1 BY kunnr.
    SORT lt_lfa1 BY lifnr.
    SORT lt_anla BY bukrs anln1.


    lt_out[] = gt_out[].
    LOOP AT gt_out INTO gs_out.

      READ TABLE lt_t003t INTO DATA(ls_t003t) WITH KEY blart = gs_out-blart BINARY SEARCH.
      IF sy-subrc = 0.
        gs_out-ltext = ls_t003t-ltext. " 凭证描述
      ENDIF.
      READ TABLE lt_usr21 INTO DATA(ls_usr21) WITH KEY bname = gs_out-usnam BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE lt_adrp INTO DATA(ls_adrp) WITH KEY persnumber = ls_usr21-persnumber BINARY SEARCH.
        IF sy-subrc = 0.
          gs_out-name_text = ls_adrp-name_text.
        ENDIF.
      ENDIF.
      " 判断是否为统驭科目（
      READ TABLE lt_skb1 INTO DATA(ls_skb1) WITH KEY saknr = gs_out-racct bukrs = gs_out-bukrs BINARY SEARCH.

      IF sy-subrc = 0."表SKAT中取：科目描述TXT50
        READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = gs_out-racct BINARY SEARCH.
        IF sy-subrc = 0.
          gs_out-txt50 = ls_skat-txt50.
        ENDIF.
        IF ls_skb1-mitkz = 'D'.  " 对于SKB1-MITKZ=D的， 辅助项ACDOCA-KUNNR
          " SELECT SINGLE name1 INTO gs_out-zfuxms FROM kna1 WHERE kunnr = gs_out-kunnr.
          READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = gs_out-kunnr BINARY SEARCH.
          IF sy-subrc = 0.
            gs_out-zfuxms = ls_kna1-name1.
          ENDIF.
          gs_out-zfux = gs_out-kunnr.

        ELSEIF ls_skb1-mitkz = 'K'.   " 对于SKB1-MITKZ=K的,  辅助项ACDOCA-LIFNR
          " SELECT SINGLE name1 INTO gs_out-zfuxms FROM lfa1 WHERE lifnr = gs_out-lifnr.
          READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = gs_out-lifnr BINARY SEARCH.
          IF sy-subrc = 0.
            gs_out-zfuxms = ls_lfa1-name1.
          ENDIF.
          gs_out-zfux = gs_out-lifnr.

        ELSEIF ls_skb1-mitkz = 'A'.  " 对于SKB1-MITKZ=A的， 辅助项ACDOCA-ANLN1，
          "  SELECT SINGLE txt50 INTO gs_out-zfuxms FROM anla WHERE bukrs = gs_out-bukrs AND anln1 = gs_out-anln1.
          READ TABLE lt_anla INTO DATA(ls_anla) WITH KEY bukrs = gs_out-bukrs anln1 = gs_out-anln1 BINARY SEARCH.
          IF sy-subrc = 0.
            gs_out-zfuxms = ls_anla-txt50.
          ENDIF.
          gs_out-zfux = gs_out-anln1.

        ENDIF.
      ENDIF.

      " 成本中心
      READ TABLE lt_cskt INTO DATA(ls_cskt) WITH KEY  kostl = gs_out-rcntr.
      IF sy-subrc = 0.
        gs_out-ltext2 = ls_cskt-ltext2.
      ENDIF.

      IF gs_out-drcrk = 'S'.
        gs_out-sdmhab = gs_out-hsl.
      ELSEIF gs_out-drcrk = 'H'.
        gs_out-sdmsol = gs_out-hsl.
      ENDIF.


      CLEAR: gs_out-hsl.

      " 行项目是1的才显示总金额，其余行项目显示空
      IF gs_out-docln = '000001'.
        LOOP AT lt_out INTO ls_out WHERE drcrk = 'S' AND belnr = gs_out-belnr.
          gs_out-hsl = ls_out-hsl + gs_out-hsl.
        ENDLOOP.
        gs_out-hsl = abs( gs_out-hsl ).
      ENDIF.


      MODIFY gt_out FROM gs_out.
      CLEAR: gs_out.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DISP
*&---------------------------------------------------------------------*
FORM alv_disp .

  PERFORM: fieldcat.
  g_repid = sy-repid.
  is_layout_lvc-box_fname = 'SEL'.
  is_layout_lvc-cwidth_opt = 'X'.
  is_layout_lvc-zebra = 'X'.

  " 输出前排序。
  SORT gt_out BY bukrs budat belnr docln.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = g_repid
      i_callback_user_command = 'USER_COMMAND'
      is_layout_lvc           = is_layout_lvc
      it_fieldcat_lvc         = it_fieldcat_lvc[]
      it_sort_lvc             = it_sort_lvc[]
      i_default               = 'X'
      i_save                  = 'A'
    TABLES
      t_outtab                = gt_out[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM user_command USING lv_ucomm TYPE syst-ucomm
       pwa_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  " lv_ucomm = sy-ucomm.
  CASE  lv_ucomm.
    WHEN '&IC1'.
      IF pwa_selfield-fieldname = 'BELNR'.
        IF pwa_selfield-value IS NOT INITIAL.
          READ TABLE gt_out INTO gs_out WITH KEY belnr = pwa_selfield-value.
          IF sy-subrc = 0.
            SET PARAMETER ID 'BUK' FIELD gs_out-bukrs.
            SET PARAMETER ID 'GJR' FIELD gs_out-budat+0(4).
            SET PARAMETER ID 'BLN' FIELD gs_out-belnr..
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat .
  DEFINE fieldcat.
    it_fieldcat_lvc-tabname  = 'GT_OUT'.
    it_fieldcat_lvc-fieldname = &1.
    it_fieldcat_lvc-just = &2.
    it_fieldcat_lvc-coltext = &3.
    it_fieldcat_lvc-ref_field = &4.
    it_fieldcat_lvc-ref_table = &5.
    it_fieldcat_lvc-no_zero = 'X'.
    IF it_fieldcat_lvc-fieldname = 'BELNR'.
      it_fieldcat_lvc-emphasize = 'C500'.
    ENDIF.

    APPEND it_fieldcat_lvc .
    CLEAR: it_fieldcat_lvc.
  END-OF-DEFINITION.
  fieldcat:
  'BUKRS'    'C' '公司代码'  'BUKRS' 'BKPF'  ,
  'BUDAT'    'C' '过账日期'  'BUDAT' 'BKPF'  ,
  'CPUDT'    'C' '输入日期'  'CPUDT' 'BKPF'  ,
  'BLART'    'C' '凭证类型'  'BLART' 'BKPF'  ,
  'LTEXT'    'L' '凭证类型描述'  'LTEXT' 'T003T' ,
  'BELNR'    'C' '凭证编码'  'BELNR' 'BKPF'  ,
  'XBLNR'    'L' '凭证摘要'  'XBLNR' 'BKPF'  ,
  'BKTXT'    'L' '凭证抬头文本' 'BKTXT' 'BKPF' ,
  'STBLG'    'C' '冲销凭证' 'STBLG' 'BKPF' ,
  'DOCLN'    'C' '凭证行项目' 'DOCLN' 'ACDOCA'  ,
  'HSL'      'R' '总金额' 'HSL' 'ACDOCA'  ,
  'NAME_TEXT' 'L' '制单人（描述）' 'NAME_TEXT' 'ADRP'  ,
  'RACCT'    'C' '会计科目'  'RACCT' 'ACDOCA'  ,
  'TXT50'    'L' '会计科目描述'  'TXT50' 'SKAT',
  'RCNTR'    'L' '成本中心'      'ACDOCA' 'RCNTR',
  'LTEXT2'   'L' '成本中心描述'  '' '',
  'ZFUX'     'C' '辅助项' ''  '',
  'ZFUXMS'   'L' '辅助项描述' ''  '',
  'SDMHAB'   'R' '借方金额'  'HSL' 'ACDOCA',
  'SDMSOL'   'R' '贷方金额'  'HSL' 'ACDOCA'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_data
*&---------------------------------------------------------------------*
FORM check_data .
  " 权限检查。
  SELECT *
  INTO TABLE @DATA(gt_bukrs)
        FROM t001
        WHERE bukrs = @p_bukrs.

  LOOP AT gt_bukrs INTO DATA(gs_bukrs).
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'ACTVT' FIELD '03'
    ID 'BUKRS' FIELD  gs_bukrs-bukrs.
    IF sy-subrc <> 0.

      MESSAGE '你没有公司' && gs_bukrs-bukrs && '的权限!' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDLOOP.
ENDFORM.

*Text elements
*----------------------------------------------------------
* 001 选择条件


*Selection texts
*----------------------------------------------------------
* P_BUKRS         公司代码
* P_DRCRK         方向
* P_USNAM         制单人
* P_XBLNR         摘要
* S_ANLN1         资产
* S_BELNR         凭证编号
* S_BLART         凭证类型
* S_BUDAT         过账日期
* S_CPDUT D       .
* S_KUNNR         客户
* S_LIFNR         供应商
* S_RACCT         科目编号

