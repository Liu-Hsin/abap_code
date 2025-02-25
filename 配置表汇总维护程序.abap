*&*********************************************************************
*& program name:              [ZSM30]
*& module name :              []
*& apply author:              []
*& author:                    [LiuX]
*& started on:                [11.04.2024 17:19:14]
*& transaction:               []
*& program type:              [Modulpool ]
*& transfer requests:         []
*& program description :      [配置表汇总维护程序]
*&*&*******************************************************************
*& revision log                                                       *
*&                                                                    *
*& log          date                 author       description         *
*& ------      ------------         ---------     -----------         *
*& v1.0       11.04.2024 17:19:14   liux          初稿                *
*&*********************************************************************
PROGRAM zsm30.


DATA: ok_code TYPE sy-ucomm.
DATA: gv_module TYPE string VALUE 'MM'.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABTAR'
CONSTANTS: BEGIN OF c_tabtar,
             tab1 LIKE sy-ucomm VALUE 'TABTAR_FC1',
             tab2 LIKE sy-ucomm VALUE 'TABTAR_FC2',
             tab3 LIKE sy-ucomm VALUE 'TABTAR_FC3',
             tab4 LIKE sy-ucomm VALUE 'TABTAR_FC4',
             tab5 LIKE sy-ucomm VALUE 'TABTAR_FC5',
             tab6 LIKE sy-ucomm VALUE 'TABTAR_FC6',
             tab7 LIKE sy-ucomm VALUE 'TABTAR_FC7',
           END OF c_tabtar.
*&SPWIZARD: DATA FOR TABSTRIP 'TABTAR'
CONTROLS:  tabtar TYPE TABSTRIP.
DATA: BEGIN OF g_tabtar,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSM30',
        pressed_tab LIKE sy-ucomm VALUE c_tabtar-tab1,
      END OF g_tabtar.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABTAR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabtar_active_tab_set OUTPUT.
  tabtar-activetab = g_tabtar-pressed_tab.
  CASE g_tabtar-pressed_tab.
    WHEN c_tabtar-tab1.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'MM'.
    WHEN c_tabtar-tab2.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'SD'.
    WHEN c_tabtar-tab3.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'FI'.
    WHEN c_tabtar-tab4.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'CO'.
    WHEN c_tabtar-tab5.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'PS'.
    WHEN c_tabtar-tab6.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'PP'.
    WHEN c_tabtar-tab7.
      g_tabtar-subscreen = '9001'.
      gv_module          = 'TOOLS'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TABTAR'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabtar_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tabtar-tab1.
      g_tabtar-pressed_tab = c_tabtar-tab1.
    WHEN c_tabtar-tab2.
      g_tabtar-pressed_tab = c_tabtar-tab2.
    WHEN c_tabtar-tab3.
      g_tabtar-pressed_tab = c_tabtar-tab3.
    WHEN c_tabtar-tab4.
      g_tabtar-pressed_tab = c_tabtar-tab4.
    WHEN c_tabtar-tab5.
      g_tabtar-pressed_tab = c_tabtar-tab5.
    WHEN c_tabtar-tab6.
      g_tabtar-pressed_tab = c_tabtar-tab6.
    WHEN c_tabtar-tab7.
      g_tabtar-pressed_tab = c_tabtar-tab7.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.


DATA: BEGIN OF gt_tabname OCCURS 200,
        tabname TYPE vim_name,
        ddtext  TYPE as4text,
      END OF gt_tabname.

DATA:
  block0   TYPE c LENGTH 10,
  more     TYPE c LENGTH 4,
  nor0001  TYPE  vim_name,
  nort0001 TYPE  vim_name,
  nor0002  TYPE  vim_name,
  nort0002 TYPE  vim_name,
  nor0003  TYPE  vim_name,
  nort0003 TYPE  vim_name,
  nor0004  TYPE  vim_name,
  nort0004 TYPE  vim_name,
  nor0005  TYPE  vim_name,
  nort0005 TYPE  vim_name,
  nor0006  TYPE  vim_name,
  nort0006 TYPE  vim_name,
  nor0007  TYPE  vim_name,
  nort0007 TYPE  vim_name,
  nor0008  TYPE  vim_name,
  nort0008 TYPE  vim_name,
  nor0009  TYPE  vim_name,
  nort0009 TYPE  vim_name,
  nor0010  TYPE  vim_name,
  nort0010 TYPE  vim_name,
  nor0011  TYPE  vim_name,
  nort0011 TYPE  vim_name,
  nor0012  TYPE  vim_name,
  nort0012 TYPE  vim_name,
  nor0013  TYPE  vim_name,
  nort0013 TYPE  vim_name,
  nor0014  TYPE  vim_name,
  nort0014 TYPE  vim_name,
  nor0015  TYPE  vim_name,
  nort0015 TYPE  vim_name,
  nor0016  TYPE  vim_name,
  nort0016 TYPE  vim_name,
  nor0017  TYPE  vim_name,
  nort0017 TYPE  vim_name,
  nor0018  TYPE  vim_name,
  nort0018 TYPE  vim_name,
  nor0019  TYPE  vim_name,
  nort0019 TYPE  vim_name,
  nor0020  TYPE  vim_name,
  nort0020 TYPE  vim_name,
  nor0021  TYPE  vim_name,
  nort0021 TYPE  vim_name,
  nor0022  TYPE  vim_name,
  nort0022 TYPE  vim_name,
  nor0023  TYPE  vim_name,
  nort0023 TYPE  vim_name,
  nor0024  TYPE  vim_name,
  nort0024 TYPE  vim_name,
  nor0025  TYPE  vim_name,
  nort0025 TYPE  vim_name,
  nor0026  TYPE  vim_name,
  nort0026 TYPE  vim_name,
  nor0027  TYPE  vim_name,
  nort0027 TYPE  vim_name,
  nor0028  TYPE  vim_name,
  nort0028 TYPE  vim_name,
  nor0029  TYPE  vim_name,
  nort0029 TYPE  vim_name,
  nor0030  TYPE  vim_name,
  nort0030 TYPE  vim_name,
  nor0031  TYPE  vim_name,
  nort0031 TYPE  vim_name,
  nor0032  TYPE  vim_name,
  nort0032 TYPE  vim_name,
  nor0033  TYPE  vim_name,
  nort0033 TYPE  vim_name,
  nor0034  TYPE  vim_name,
  nort0034 TYPE  vim_name,
  nor0035  TYPE  vim_name,
  nort0035 TYPE  vim_name,
  nor0036  TYPE  vim_name,
  nort0036 TYPE  vim_name,
  nor0037  TYPE  vim_name,
  nort0037 TYPE  vim_name,
  nor0038  TYPE  vim_name,
  nort0038 TYPE  vim_name,
  nor0039  TYPE  vim_name,
  nort0039 TYPE  vim_name,
  nor0040  TYPE  vim_name,
  nort0040 TYPE  vim_name,
  nor0041  TYPE  vim_name,
  nort0041 TYPE  vim_name,
  nor0042  TYPE  vim_name,
  nort0042 TYPE  vim_name,
  nor0043  TYPE  vim_name,
  nort0043 TYPE  vim_name,
  nor0044  TYPE  vim_name,
  nort0044 TYPE  vim_name,
  nor0045  TYPE  vim_name,
  nort0045 TYPE  vim_name.
DATA:
  more0046  TYPE  vim_name,
  moret0046 TYPE  vim_name,
  more0047  TYPE  vim_name,
  moret0047 TYPE  vim_name,
  more0048  TYPE  vim_name,
  moret0048 TYPE  vim_name,
  more0049  TYPE  vim_name,
  moret0049 TYPE  vim_name,
  more0050  TYPE  vim_name,
  moret0050 TYPE  vim_name,
  more0051  TYPE  vim_name,
  moret0051 TYPE  vim_name,
  more0052  TYPE  vim_name,
  moret0052 TYPE  vim_name,
  more0053  TYPE  vim_name,
  moret0053 TYPE  vim_name,
  more0054  TYPE  vim_name,
  moret0054 TYPE  vim_name,
  more0055  TYPE  vim_name,
  moret0055 TYPE  vim_name,
  more0056  TYPE  vim_name,
  moret0056 TYPE  vim_name,
  more0057  TYPE  vim_name,
  moret0057 TYPE  vim_name,
  more0058  TYPE  vim_name,
  moret0058 TYPE  vim_name,
  more0059  TYPE  vim_name,
  moret0059 TYPE  vim_name,
  more0060  TYPE  vim_name,
  moret0060 TYPE  vim_name,
  more0061  TYPE  vim_name,
  moret0061 TYPE  vim_name,
  more0062  TYPE  vim_name,
  moret0062 TYPE  vim_name,
  more0063  TYPE  vim_name,
  moret0063 TYPE  vim_name,
  more0064  TYPE  vim_name,
  moret0064 TYPE  vim_name,
  more0065  TYPE  vim_name,
  moret0065 TYPE  vim_name,
  more0066  TYPE  vim_name,
  moret0066 TYPE  vim_name,
  more0067  TYPE  vim_name,
  moret0067 TYPE  vim_name,
  more0068  TYPE  vim_name,
  moret0068 TYPE  vim_name,
  more0069  TYPE  vim_name,
  moret0069 TYPE  vim_name,
  more0070  TYPE  vim_name,
  moret0070 TYPE  vim_name,
  more0071  TYPE  vim_name,
  moret0071 TYPE  vim_name,
  more0072  TYPE  vim_name,
  moret0072 TYPE  vim_name,
  more0073  TYPE  vim_name,
  moret0073 TYPE  vim_name,
  more0074  TYPE  vim_name,
  moret0074 TYPE  vim_name,
  more0075  TYPE  vim_name,
  moret0075 TYPE  vim_name,
  more0076  TYPE  vim_name,
  moret0076 TYPE  vim_name,
  more0077  TYPE  vim_name,
  moret0077 TYPE  vim_name,
  more0078  TYPE  vim_name,
  moret0078 TYPE  vim_name,
  more0079  TYPE  vim_name,
  moret0079 TYPE  vim_name,
  more0080  TYPE  vim_name,
  moret0080 TYPE  vim_name,
  more0081  TYPE  vim_name,
  moret0081 TYPE  vim_name,
  more0082  TYPE  vim_name,
  moret0082 TYPE  vim_name,
  more0083  TYPE  vim_name,
  moret0083 TYPE  vim_name,
  more0084  TYPE  vim_name,
  moret0084 TYPE  vim_name,
  more0085  TYPE  vim_name,
  moret0085 TYPE  vim_name,
  more0086  TYPE  vim_name,
  moret0086 TYPE  vim_name,
  more0087  TYPE  vim_name,
  moret0087 TYPE  vim_name,
  more0088  TYPE  vim_name,
  moret0088 TYPE  vim_name,
  more0089  TYPE  vim_name,
  moret0089 TYPE  vim_name,
  more0090  TYPE  vim_name,
  moret0090 TYPE  vim_name.
*&---------------------------------------------------------------------*
*& Module STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status OUTPUT.
  PERFORM:frm_init_data.
  SET PF-STATUS 'STATUS'.
  SET TITLEBAR  'TITLE'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM:frm_user_command.
  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_9001 OUTPUT.

  IF gt_tabname[] IS NOT INITIAL.
    CASE gv_module.
      WHEN 'TOOLS'.
        block0 = 'Others'.
        PERFORM:frm_set_others_name.
      WHEN OTHERS.
        block0 = gv_module && '模块配置表'.
        PERFORM:frm_set_name USING gv_module.
    ENDCASE.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9001 INPUT.
  ok_code = sy-ucomm.
  PERFORM: frm_user_command_sub.
  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_9002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_9002 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  SET PF-STATUS '9002'.
  PERFORM:frm_set_screen_active.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_9002 INPUT.
  ok_code = sy-ucomm.
  PERFORM: frm_user_command_9002.
  CLEAR sy-ucomm.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form frm_init_data
*&---------------------------------------------------------------------*
FORM frm_init_data .
  IF gt_tabname[] IS INITIAL.
    SELECT tvdir~tabname,dd02t~ddtext FROM tvdir INNER JOIN dd02t ON tvdir~tabname = dd02t~tabname
    INTO TABLE @gt_tabname
    WHERE tvdir~tabname LIKE 'Z%'
    AND ddlanguage = @sy-langu.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_set_name  USING  VALUE(p_mod).
  DATA: lv_index       TYPE numc4,
        lv_screen_name TYPE c LENGTH 10,
        lv_screen_text TYPE c LENGTH 10,
        lv_name1       TYPE c LENGTH 4,
        lv_name2       TYPE c LENGTH 4.
  DATA(lt_tmpe) = gt_tabname[].
  lv_name1 = 'Z' && p_mod && 'T'.
  lv_name2 = 'ZT' && p_mod.
  DELETE lt_tmpe WHERE tabname NS lv_name1 AND tabname NS lv_name2.
  SORT lt_tmpe BY tabname.

  PERFORM:frm_clear_data.

  LOOP AT lt_tmpe INTO DATA(ls_tmpe).
    lv_index = sy-tabix.
    IF sy-tabix <= 45.
      lv_screen_name = 'NOR' && lv_index.
      lv_screen_text = 'NORT' && lv_index.
    ELSE.
      more = 'MORE'.
      lv_screen_name = 'MORE' && lv_index.
      lv_screen_text = 'MORET' && lv_index.
    ENDIF.
    ASSIGN (lv_screen_name) TO FIELD-SYMBOL(<name>).
    IF <name> IS ASSIGNED.
      <name> = ls_tmpe-tabname.
    ENDIF.
    ASSIGN (lv_screen_text) TO FIELD-SYMBOL(<text>).
    IF <text> IS ASSIGNED.
      <text> = ls_tmpe-ddtext.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.
    ASSIGN (screen-name) TO FIELD-SYMBOL(<value>).
    IF <value> IS ASSIGNED AND <value> IS INITIAL.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF more0046 IS INITIAL AND screen-name = 'MORE'.
      screen-active = '0'.
      MODIFY SCREEN.
    ELSEIF more0046 IS NOT INITIAL AND screen-name = 'MORE'..
      screen-active = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_others_name
*&---------------------------------------------------------------------*
FORM frm_set_others_name .
  DATA  lt_tmpe LIKE TABLE OF gt_tabname.
  DATA: lv_index       TYPE numc4,
        lv_screen_name TYPE c LENGTH 10,
        lv_screen_text TYPE c LENGTH 10.

  PERFORM: frm_get_other_tab TABLES lt_tmpe USING 'MM' .
  PERFORM: frm_get_other_tab TABLES lt_tmpe USING 'SD' .
  PERFORM: frm_get_other_tab TABLES lt_tmpe USING 'FI' .
  PERFORM: frm_get_other_tab TABLES lt_tmpe USING 'CO' .
  PERFORM: frm_get_other_tab TABLES lt_tmpe USING 'PS' .
  PERFORM: frm_get_other_tab TABLES lt_tmpe USING 'PP' .

  SORT lt_tmpe BY tabname.
  PERFORM: frm_clear_data.

  LOOP AT lt_tmpe INTO DATA(ls_tmpe).
    lv_index = sy-tabix.
    IF sy-tabix <= 45.
      lv_screen_name = 'NOR' && lv_index.
      lv_screen_text = 'NORT' && lv_index.
    ELSE.
      more = 'MORE'.
      lv_screen_name = 'MORE' && lv_index.
      lv_screen_text = 'MORET' && lv_index.
    ENDIF.
    ASSIGN (lv_screen_name) TO FIELD-SYMBOL(<name>).
    IF <name> IS ASSIGNED.
      <name> = ls_tmpe-tabname.
    ENDIF.
    ASSIGN (lv_screen_text) TO FIELD-SYMBOL(<text>).
    IF <text> IS ASSIGNED.
      <text> = ls_tmpe-ddtext.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.
    ASSIGN (screen-name) TO FIELD-SYMBOL(<value>).
    IF <value> IS ASSIGNED AND <value> IS INITIAL.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF more0046 IS INITIAL AND screen-name = 'MORE'.
      screen-active = '0'.
      MODIFY SCREEN.
    ELSEIF more0046 IS NOT INITIAL AND screen-name = 'MORE'.
      screen-active = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_other_tab
*&---------------------------------------------------------------------*
FORM frm_get_other_tab  TABLES pt_tmpe STRUCTURE gt_tabname USING p_mod .
  DATA: lv_name1 TYPE c LENGTH 4,
        lv_name2 TYPE c LENGTH 4.
  IF pt_tmpe[] IS INITIAL.
    DATA(lt_tmpe) = gt_tabname[].
  ELSE.
    lt_tmpe[] = pt_tmpe[].
  ENDIF.

  lv_name1 = 'Z' && p_mod && 'T'.
  lv_name2 = 'ZT' && p_mod.
  DELETE lt_tmpe WHERE tabname CS lv_name1 OR tabname CS lv_name2.
  pt_tmpe[] = lt_tmpe[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_user_command
*&---------------------------------------------------------------------*
*& text
FORM frm_user_command .
  DATA: lv_code TYPE sy-ucomm.
  lv_code = ok_code.

  CASE lv_code.
    WHEN 'EXIT' OR 'BACK' OR  'CANC'. LEAVE PROGRAM.
    WHEN 'SE11' OR 'SE14' OR 'SE54' OR 'SM30'.
      PERFORM:frm_call_transaction USING lv_code.
    WHEN OTHERS.
      RETURN.
  ENDCASE.
  CLEAR: ok_code.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_clear_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_clear_data .
  CLEAR:
  more,
  nor0001      ,
  nort0001      ,
  nor0002     ,
  nort0002      ,
  nor0003     ,
  nort0003      ,
  nor0004     ,
  nort0004      ,
  nor0005     ,
  nort0005      ,
  nor0006     ,
  nort0006      ,
  nor0007     ,
  nort0007      ,
  nor0008     ,
  nort0008      ,
  nor0009     ,
  nort0009      ,
  nor0010     ,
  nort0010      ,
  nor0011     ,
  nort0011      ,
  nor0012     ,
  nort0012      ,
  nor0013     ,
  nort0013      ,
  nor0014     ,
  nort0014      ,
  nor0015     ,
  nort0015      ,
  nor0016     ,
  nort0016      ,
  nor0017     ,
  nort0017      ,
  nor0018     ,
  nort0018      ,
  nor0019     ,
  nort0019      ,
  nor0020     ,
  nort0020      ,
  nor0021     ,
  nort0021      ,
  nor0022     ,
  nort0022      ,
  nor0023     ,
  nort0023      ,
  nor0024     ,
  nort0024      ,
  nor0025     ,
  nort0025      ,
  nor0026     ,
  nort0026      ,
  nor0027     ,
  nort0027      ,
  nor0028     ,
  nort0028      ,
  nor0029     ,
  nort0029      ,
  nor0030     ,
  nort0030      ,
  nor0031     ,
  nort0031      ,
  nor0032     ,
  nort0032      ,
  nor0033     ,
  nort0033      ,
  nor0034     ,
  nort0034      ,
  nor0035     ,
  nort0035      ,
  nor0036     ,
  nort0036      ,
  nor0037     ,
  nort0037      ,
  nor0038     ,
  nort0038      ,
  nor0039     ,
  nort0039      ,
  nor0040     ,
  nort0040      ,
  nor0041     ,
  nort0041      ,
  nor0042     ,
  nort0042      ,
  nor0043     ,
  nort0043      ,
  nor0044     ,
  nort0044      ,
  nor0045     ,
  nort0045      ,


  more0046      ,
  moret0046     ,
  more0047      ,
  moret0047     ,
  more0048      ,
  moret0048     ,
  more0049      ,
  moret0049     ,
  more0050      ,
  moret0050     ,
  more0051      ,
  moret0051     ,
  more0052      ,
  moret0052     ,
  more0053      ,
  moret0053     ,
  more0054      ,
  moret0054     ,
  more0055      ,
  moret0055     ,
  more0056      ,
  moret0056     ,
  more0057      ,
  moret0057     ,
  more0058      ,
  moret0058     ,
  more0059      ,
  moret0059     ,
  more0060      ,
  moret0060     ,
  more0061      ,
  moret0061     ,
  more0062      ,
  moret0062     ,
  more0063      ,
  moret0063     ,
  more0064      ,
  moret0064     ,
  more0065      ,
  moret0065     ,
  more0066      ,
  moret0066     ,
  more0067      ,
  moret0067     ,
  more0068      ,
  moret0068     ,
  more0069      ,
  moret0069     ,
  more0070      ,
  moret0070     ,
  more0071      ,
  moret0071     ,
  more0072      ,
  moret0072     ,
  more0073      ,
  moret0073     ,
  more0074      ,
  moret0074     ,
  more0075      ,
  moret0075     ,
  more0076      ,
  moret0076     ,
  more0077      ,
  moret0077     ,
  more0078      ,
  moret0078     ,
  more0079      ,
  moret0079     ,
  more0080      ,
  moret0080     ,
  more0081      ,
  moret0081     ,
  more0082      ,
  moret0082     ,
  more0083      ,
  moret0083     ,
  more0084      ,
  moret0084     ,
  more0085      ,
  moret0085     ,
  more0086      ,
  moret0086     ,
  more0087      ,
  moret0087     ,
  more0088      ,
  moret0088     ,
  more0089      ,
  moret0089     ,
  more0090      ,
  moret0090     .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_user_command_sub
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_user_command_sub .
  DATA: lv_code TYPE sy-ucomm.

  lv_code = ok_code.

  CASE lv_code .
    WHEN 'MORE'.
      CALL SCREEN 9002 STARTING AT 2 2 ENDING AT 165 40.
    WHEN OTHERS.
      IF lv_code(3) = 'NOR'.
        PERFORM:frm_skip_sm30 USING lv_code.
      ELSE.
        RETURN.
      ENDIF.
  ENDCASE.
  CLEAR: ok_code.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_set_screen_active
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_screen_active .
  LOOP AT SCREEN.
    ASSIGN (screen-name) TO FIELD-SYMBOL(<value>).
    IF <value> IS ASSIGNED AND <value> IS INITIAL.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_skip_sm30
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_CODE
*&---------------------------------------------------------------------*
FORM frm_skip_sm30  USING  VALUE(pv_code).
  DATA: lv_tab TYPE  dd02v-tabname.
  ASSIGN (pv_code) TO FIELD-SYMBOL(<tabname>).
  IF <tabname> IS NOT INITIAL..
    lv_tab = <tabname>.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action    = 'S'
        view_name = lv_tab.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_call_transaction
*&---------------------------------------------------------------------*
FORM frm_call_transaction  USING    p_lv_code.
  DATA: lv_code TYPE sy-tcode.
  lv_code = p_lv_code.
  CALL TRANSACTION lv_code .
ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_user_command_9002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_user_command_9002 .
  DATA: lv_code TYPE sy-ucomm.
  lv_code = ok_code.
  CASE lv_code .
    WHEN 'OK' OR 'CANCEL' OR 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      IF lv_code(4) = 'MORE'.
        PERFORM:frm_skip_sm30 USING lv_code.
      ELSE.
        RETURN.
      ENDIF.
  ENDCASE.
  CLEAR: ok_code.
ENDFORM.

*GUI Texts
*----------------------------------------------------------
* TITLE --> 配置表汇总维护程序

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.2 - E.G.Mellodew. 1998-2024. Sap Release 755


****************************************************************																																
*   This file was generated by Direct Download Enterprise.     *																																
*   Please do not change it manually.                          *																																
****************************************************************																																
%_DYNPRO																																
ZSM30																																
9000																																
755																																
                40																																
%_HEADER																																
ZSM30                                   9000 9000     37170192 37  0  0 37172  0G 1                              20240524113304																																
%_DESCRIPTION																																
main																																
%_FIELDS																																
TABTAR			169	08	30	00	00	00	  1	  3	J	101	  1	  1		 37	I				  6	 15										
TABTAR_TAB1		CHAR	  4	00	00	08	30	00	  1	  1	I	101	  1	  1		  0	P				  0	  0	102	TABTAR_SCA						MMģ  	                                                                                                                                                                        TABTAR_FC1	
TABTAR_TAB2		CHAR	  4	00	00	08	30	00	  1	  2	I	101	  1	  1		  0	P				  0	  0	103	TABTAR_SCA						SDģ  	                                                                                                                                                                        TABTAR_FC2	
TABTAR_TAB3		CHAR	  4	00	00	08	30	00	  1	  3	I	101	  1	  1		  0	P				  0	  0	104	TABTAR_SCA						FIģ  	                                                                                                                                                                        TABTAR_FC3	
TABTAR_TAB4		CHAR	  4	00	00	08	30	00	  1	  4	I	101	  1	  1		  0	P				  0	  0	105	TABTAR_SCA						COģ  	                                                                                                                                                                        TABTAR_FC4	
TABTAR_TAB5		CHAR	  4	00	00	08	30	00	  1	  5	I	101	  1	  1		  0	P				  0	  0	106	TABTAR_SCA						PSģ  	                                                                                                                                                                        TABTAR_FC5	
TABTAR_TAB6		CHAR	  4	00	00	08	30	00	  1	  6	I	101	  1	  1		  0	P				  0	  0	107	TABTAR_SCA						PPģ  	                                                                                                                                                                        TABTAR_FC6	
TABTAR_TAB7		CHAR	  6	00	00	08	30	00	  1	  7	I	101	  1	  1		  0	P				  0	  0	108	TABTAR_SCA						Others	                                                                                                                                                                        TABTAR_FC7	
TABTAR_SCA			167	00	F0	00	00	00	  3	  4	I	101	  0	  0		 34	B				  3	 12	109									
		CHAR	 20	80	10	00	00	00	255	  1	O	  0	  0	  0		  0					  0	  0								____________________		
%_FLOWLOGIC																																
PROCESS BEFORE OUTPUT.																																
*&SPWIZARD: PBO FLOW LOGIC FOR TABSTRIP 'TABTAR'																																
  MODULE status.																																
																																
  MODULE tabtar_active_tab_set.																																
  CALL SUBSCREEN tabtar_sca																																
    INCLUDING g_tabtar-prog g_tabtar-subscreen.																																
*																																
PROCESS AFTER INPUT.																																
*&SPWIZARD: PAI FLOW LOGIC FOR TABSTRIP 'TABTAR'																																
  CALL SUBSCREEN tabtar_sca.																																
																																
  MODULE tabtar_active_tab_get.																																
																																
  MODULE user_command.																																


****************************************************************																																
*   This file was generated by Direct Download Enterprise.     *																																
*   Please do not change it manually.                          *																																
****************************************************************																																
%_DYNPRO																																
ZSM30																																
9001																																
755																																
                40																																
%_HEADER																																
ZSM30                                   9001I9001     32160192 37  0  0 32162  0G 1                              20240524134425																																
%_DESCRIPTION																																
模块																																
%_FIELDS																																
BLOCK0	C	CHAR	160	80	00	80	30	00	  1	  2		  0	  0	  0		 32	R				  0	  0	101							___________________________________________________________________________________________________________________________________		
MORE		CHAR	 32	00	00	00	30	00	  2	150		  0	  0	  0		 11	P				  0	  0	102							@1E@ 更多...	                                                                                                                                                                        MORE	
NOR0001	C	CHAR	 20	80	00	80	30	00	  3	  6		  0	  0	  0		  0	P				  0	  0	103							____________________	                                                                                                                                                                        NOR0001	
NORT0001	C	CHAR	 60	81	00	80	31	00	  3	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0002		CHAR	 20	80	00	80	30	00	  3	 58		  0	  0	  0		  0	P				  0	  0	104							____________________	                                                                                                                                                                        NOR0002	
NORT0002	C	CHAR	 60	81	00	80	31	00	  3	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0003		CHAR	 20	80	00	80	30	00	  3	110		  0	  0	  0		  0	P				  0	  0	105							____________________	                                                                                                                                                                        NOR0003	
NORT0003	C	CHAR	 60	81	00	80	31	00	  3	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0004		CHAR	 20	80	00	80	30	00	  5	  6		  0	  0	  0		  0	P				  0	  0	106							____________________	                                                                                                                                                                        NOR0004	
NORT0004	C	CHAR	 60	81	00	80	31	00	  5	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0005		CHAR	 20	80	00	80	30	00	  5	 58		  0	  0	  0		  0	P				  0	  0	107							____________________	                                                                                                                                                                        NOR0005	
NORT0005	C	CHAR	 60	81	00	80	31	00	  5	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0006		CHAR	 20	80	00	80	30	00	  5	110		  0	  0	  0		  0	P				  0	  0	108							____________________	                                                                                                                                                                        NOR0006	
NORT0006	C	CHAR	 60	81	00	80	31	00	  5	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0007		CHAR	 20	80	00	80	30	00	  7	  6		  0	  0	  0		  0	P				  0	  0	109							____________________	                                                                                                                                                                        NOR0007	
NORT0007	C	CHAR	 60	81	00	80	31	00	  7	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0008		CHAR	 20	80	00	80	30	00	  7	 58		  0	  0	  0		  0	P				  0	  0	110							____________________	                                                                                                                                                                        NOR0008	
NORT0008	C	CHAR	 60	81	00	80	31	00	  7	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0009		CHAR	 20	80	00	80	30	00	  7	110		  0	  0	  0		  0	P				  0	  0	111							____________________	                                                                                                                                                                        NOR0009	
NORT0009	C	CHAR	 60	81	00	80	31	00	  7	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0010		CHAR	 20	80	00	80	30	00	  9	  6		  0	  0	  0		  0	P				  0	  0	112							____________________	                                                                                                                                                                        NOR0010	
NORT0010	C	CHAR	 60	81	00	80	31	00	  9	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0011		CHAR	 20	80	00	80	30	00	  9	 58		  0	  0	  0		  0	P				  0	  0	113							____________________	                                                                                                                                                                        NOR0011	
NORT0011	C	CHAR	 60	81	00	80	31	00	  9	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0012		CHAR	 20	80	00	80	30	00	  9	110		  0	  0	  0		  0	P				  0	  0	114							____________________	                                                                                                                                                                        NOR0012	
NORT0012	C	CHAR	 60	81	00	80	31	00	  9	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0013		CHAR	 20	80	00	80	30	00	 11	  6		  0	  0	  0		  0	P				  0	  0	115							____________________	                                                                                                                                                                        NOR0013	
NORT0013	C	CHAR	 60	81	00	80	31	00	 11	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0014		CHAR	 20	80	00	80	30	00	 11	 58		  0	  0	  0		  0	P				  0	  0	116							____________________	                                                                                                                                                                        NOR0014	
NORT0014	C	CHAR	 60	81	00	80	31	00	 11	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0015		CHAR	 20	80	00	80	30	00	 11	110		  0	  0	  0		  0	P				  0	  0	117							____________________	                                                                                                                                                                        NOR0015	
NORT0015	C	CHAR	 60	81	00	80	31	00	 11	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0016		CHAR	 20	80	00	80	30	00	 13	  6		  0	  0	  0		  0	P				  0	  0	118							____________________	                                                                                                                                                                        NOR0016	
NORT0016	C	CHAR	 60	81	00	80	31	00	 13	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0017		CHAR	 20	80	00	80	30	00	 13	 58		  0	  0	  0		  0	P				  0	  0	119							____________________	                                                                                                                                                                        NOR0017	
NORT0017	C	CHAR	 60	81	00	80	31	00	 13	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0018		CHAR	 20	80	00	80	30	00	 13	110		  0	  0	  0		  0	P				  0	  0	120							____________________	                                                                                                                                                                        NOR0018	
NORT0018	C	CHAR	 60	81	00	80	31	00	 13	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0019		CHAR	 20	80	00	80	30	00	 15	  6		  0	  0	  0		  0	P				  0	  0	121							____________________	                                                                                                                                                                        NOR0019	
NORT0019	C	CHAR	 60	81	00	80	31	00	 15	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0020		CHAR	 20	80	00	80	30	00	 15	 58		  0	  0	  0		  0	P				  0	  0	122							____________________	                                                                                                                                                                        NOR0020	
NORT0020	C	CHAR	 60	81	00	80	31	00	 15	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0021		CHAR	 20	80	00	80	30	00	 15	110		  0	  0	  0		  0	P				  0	  0	123							____________________	                                                                                                                                                                        NOR0021	
NORT0021	C	CHAR	 60	81	00	80	31	00	 15	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0022		CHAR	 20	80	00	80	30	00	 17	  6		  0	  0	  0		  0	P				  0	  0	124							____________________	                                                                                                                                                                        NOR0022	
NORT0022	C	CHAR	 60	81	00	80	31	00	 17	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0023		CHAR	 20	80	00	80	30	00	 17	 58		  0	  0	  0		  0	P				  0	  0	125							____________________	                                                                                                                                                                        NOR0023	
NORT0023	C	CHAR	 60	81	00	80	31	00	 17	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0024		CHAR	 20	80	00	80	30	00	 17	110		  0	  0	  0		  0	P				  0	  0	126							____________________	                                                                                                                                                                        NOR0024	
NORT0024	C	CHAR	 60	81	00	80	31	00	 17	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0025		CHAR	 20	80	00	80	30	00	 19	  6		  0	  0	  0		  0	P				  0	  0	127							____________________	                                                                                                                                                                        NOR0025	
NORT0025	C	CHAR	 60	81	00	80	31	00	 19	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0026		CHAR	 20	80	00	80	30	00	 19	 58		  0	  0	  0		  0	P				  0	  0	128							____________________	                                                                                                                                                                        NOR0026	
NORT0026	C	CHAR	 60	81	00	80	31	00	 19	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0027		CHAR	 20	80	00	80	30	00	 19	110		  0	  0	  0		  0	P				  0	  0	129							____________________	                                                                                                                                                                        NOR0027	
NORT0027	C	CHAR	 60	81	00	80	31	00	 19	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0028		CHAR	 20	80	00	80	30	00	 21	  6		  0	  0	  0		  0	P				  0	  0	130							____________________	                                                                                                                                                                        NOR0028	
NORT0028	C	CHAR	 60	81	00	80	31	00	 21	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0029		CHAR	 20	80	00	80	00	00	 21	 58		  0	  0	  0		  0	P				  0	  0	131							____________________	                                                                                                                                                                        NOR0029	
NORT0029	C	CHAR	 60	81	00	80	31	00	 21	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0030		CHAR	 20	80	00	80	00	00	 21	110		  0	  0	  0		  0	P				  0	  0	132							____________________	                                                                                                                                                                        NOR0030	
NORT0030	C	CHAR	 60	81	00	80	31	00	 21	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0031		CHAR	 20	80	00	80	00	00	 23	  6		  0	  0	  0		  0	P				  0	  0	133							____________________	                                                                                                                                                                        NOR0031	
NORT0031	C	CHAR	 60	81	00	80	31	00	 23	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0032		CHAR	 20	80	00	80	00	00	 23	 58		  0	  0	  0		  0	P				  0	  0	134							____________________	                                                                                                                                                                        NOR0032	
NORT0032	C	CHAR	 60	81	00	80	31	00	 23	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0033		CHAR	 20	80	00	80	00	00	 23	110		  0	  0	  0		  0	P				  0	  0	135							____________________	                                                                                                                                                                        NOR0033	
NORT0033	C	CHAR	 60	81	00	80	31	00	 23	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0034		CHAR	 20	80	00	80	00	00	 25	  6		  0	  0	  0		  0	P				  0	  0	136							____________________	                                                                                                                                                                        NOR0034	
NORT0034	C	CHAR	 60	81	00	80	31	00	 25	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0035		CHAR	 20	80	00	80	00	00	 25	 58		  0	  0	  0		  0	P				  0	  0	137							____________________	                                                                                                                                                                        NOR0035	
NORT0035	C	CHAR	 60	81	00	80	31	00	 25	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0036		CHAR	 20	80	00	80	00	00	 25	110		  0	  0	  0		  0	P				  0	  0	138							____________________	                                                                                                                                                                        NOR0036	
NORT0036	C	CHAR	 60	81	00	80	31	00	 25	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0037		CHAR	 20	80	00	80	00	00	 27	  6		  0	  0	  0		  0	P				  0	  0	139							____________________	                                                                                                                                                                        NOR0037	
NORT0037	C	CHAR	 60	81	00	80	31	00	 27	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0038		CHAR	 20	80	00	80	00	00	 27	 58		  0	  0	  0		  0	P				  0	  0	140							____________________	                                                                                                                                                                        NOR0038	
NORT0038	C	CHAR	 60	81	00	80	31	00	 27	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0039		CHAR	 20	80	00	80	00	00	 27	110		  0	  0	  0		  0	P				  0	  0	141							____________________	                                                                                                                                                                        NOR0039	
NORT0039	C	CHAR	 60	81	00	80	31	00	 27	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0040		CHAR	 20	80	00	80	00	00	 29	  6		  0	  0	  0		  0	P				  0	  0	142							____________________	                                                                                                                                                                        NOR0040	
NORT0040	C	CHAR	 60	81	00	80	31	00	 29	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0041		CHAR	 20	80	00	80	00	00	 29	 58		  0	  0	  0		  0	P				  0	  0	143							____________________	                                                                                                                                                                        NOR0041	
NORT0041	C	CHAR	 60	81	00	80	31	00	 29	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0042		CHAR	 20	80	00	80	00	00	 29	110		  0	  0	  0		  0	P				  0	  0	144							____________________	                                                                                                                                                                        NOR0042	
NORT0042	C	CHAR	 60	81	00	80	31	00	 29	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0043		CHAR	 20	80	00	80	00	00	 31	  6		  0	  0	  0		  0	P				  0	  0	145							____________________	                                                                                                                                                                        NOR0043	
NORT0043	C	CHAR	 60	81	00	80	31	00	 31	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0044		CHAR	 20	80	00	80	00	00	 31	 58		  0	  0	  0		  0	P				  0	  0	146							____________________	                                                                                                                                                                        NOR0044	
NORT0044	C	CHAR	 60	81	00	80	31	00	 31	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
NOR0045		CHAR	 20	80	00	80	00	00	 31	110		  0	  0	  0		  0	P				  0	  0	147							____________________	                                                                                                                                                                        NOR0045	
NORT0045	C	CHAR	 60	81	00	80	31	00	 31	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
		CHAR	 20	80	10	08	00	00	255	  1	O	  0	  0	  0		  0					  0	  0								____________________		
%_FLOWLOGIC																																
PROCESS BEFORE OUTPUT.																																
  MODULE pbo_9001.																																
*																																
PROCESS AFTER INPUT.																																
  MODULE pai_9001.																																





****************************************************************																																
*   This file was generated by Direct Download Enterprise.     *																																
*   Please do not change it manually.                          *																																
****************************************************************																																
%_DYNPRO																																
ZSM30																																
9002																																
755																																
                40																																
%_HEADER																																
ZSM30                                   9002 9002     31160196 37  0  0 31160  0L 1                              20240524120041																																
%_DESCRIPTION																																
more																																
%_FIELDS																																
BLOCK0		CHAR	160	00	00	00	30	00	  1	  2		  0	  0	  0		 31	R				  0	  0	101							更多的配置表		
MORE0046	C	CHAR	 20	80	00	80	30	00	  2	  6		  0	  0	  0		  0	P				  0	  0	102							____________________	                                                                                                                                                                        MORE0046	
MORET0046	C	CHAR	 60	81	00	80	31	00	  2	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0047		CHAR	 20	80	00	80	30	00	  2	 58		  0	  0	  0		  0	P				  0	  0	103							____________________	                                                                                                                                                                        MORE0047	
MORET0047	C	CHAR	 60	81	00	80	31	00	  2	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0048		CHAR	 20	80	00	80	30	00	  2	110		  0	  0	  0		  0	P				  0	  0	104							____________________	                                                                                                                                                                        MORE0048	
MORET0048	C	CHAR	 60	81	00	80	31	00	  2	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0049		CHAR	 20	80	00	80	30	00	  4	  6		  0	  0	  0		  0	P				  0	  0	105							____________________	                                                                                                                                                                        MORE0049	
MORET0049	C	CHAR	 60	81	00	80	31	00	  4	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0050		CHAR	 20	80	00	80	30	00	  4	 58		  0	  0	  0		  0	P				  0	  0	106							____________________	                                                                                                                                                                        MORE0050	
MORET0050	C	CHAR	 60	81	00	80	31	00	  4	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0051		CHAR	 20	80	00	80	30	00	  4	110		  0	  0	  0		  0	P				  0	  0	107							____________________	                                                                                                                                                                        MORE0051	
MORET0051	C	CHAR	 60	81	00	80	31	00	  4	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0052		CHAR	 20	80	00	80	30	00	  6	  6		  0	  0	  0		  0	P				  0	  0	108							____________________	                                                                                                                                                                        MORE0052	
MORET0052	C	CHAR	 60	81	00	80	31	00	  6	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0053		CHAR	 20	80	00	80	30	00	  6	 58		  0	  0	  0		  0	P				  0	  0	109							____________________	                                                                                                                                                                        MORE0053	
MORET0053	C	CHAR	 60	81	00	80	31	00	  6	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0054		CHAR	 20	80	00	80	30	00	  6	110		  0	  0	  0		  0	P				  0	  0	110							____________________	                                                                                                                                                                        MORE0054	
MORET0054	C	CHAR	 60	81	00	80	31	00	  6	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0055		CHAR	 20	80	00	80	30	00	  8	  6		  0	  0	  0		  0	P				  0	  0	111							____________________	                                                                                                                                                                        MORE0055	
MORET0055	C	CHAR	 60	81	00	80	31	00	  8	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0056		CHAR	 20	80	00	80	30	00	  8	 58		  0	  0	  0		  0	P				  0	  0	112							____________________	                                                                                                                                                                        MORE0056	
MORET0056	C	CHAR	 60	81	00	80	31	00	  8	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0057		CHAR	 20	80	00	80	30	00	  8	110		  0	  0	  0		  0	P				  0	  0	113							____________________	                                                                                                                                                                        MORE0057	
MORET0057	C	CHAR	 60	81	00	80	31	00	  8	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0058		CHAR	 20	80	00	80	30	00	 10	  6		  0	  0	  0		  0	P				  0	  0	114							____________________	                                                                                                                                                                        MORE0058	
MORET0058	C	CHAR	 60	81	00	80	31	00	 10	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0059		CHAR	 20	80	00	80	30	00	 10	 58		  0	  0	  0		  0	P				  0	  0	115							____________________	                                                                                                                                                                        MORE0059	
MORET0059	C	CHAR	 60	81	00	80	31	00	 10	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0060		CHAR	 20	80	00	80	30	00	 10	110		  0	  0	  0		  0	P				  0	  0	116							____________________	                                                                                                                                                                        MORE0060	
MORET0060	C	CHAR	 60	81	00	80	31	00	 10	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0061		CHAR	 20	80	00	80	30	00	 12	  6		  0	  0	  0		  0	P				  0	  0	117							____________________	                                                                                                                                                                        MORE0061	
MORET0061	C	CHAR	 60	81	00	80	31	00	 12	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0062		CHAR	 20	80	00	80	30	00	 12	 58		  0	  0	  0		  0	P				  0	  0	118							____________________	                                                                                                                                                                        MORE0062	
MORET0062	C	CHAR	 60	81	00	80	31	00	 12	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0063		CHAR	 20	80	00	80	30	00	 12	110		  0	  0	  0		  0	P				  0	  0	119							____________________	                                                                                                                                                                        MORE0063	
MORET0063	C	CHAR	 60	81	00	80	31	00	 12	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0064		CHAR	 20	80	00	80	30	00	 14	  6		  0	  0	  0		  0	P				  0	  0	120							____________________	                                                                                                                                                                        MORE0064	
MORET0064	C	CHAR	 60	81	00	80	31	00	 14	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0065		CHAR	 20	80	00	80	30	00	 14	 58		  0	  0	  0		  0	P				  0	  0	121							____________________	                                                                                                                                                                        MORE0065	
MORET0065	C	CHAR	 60	81	00	80	31	00	 14	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0066		CHAR	 20	80	00	80	30	00	 14	110		  0	  0	  0		  0	P				  0	  0	122							____________________	                                                                                                                                                                        MORE0066	
MORET0066	C	CHAR	 60	81	00	80	31	00	 14	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0067		CHAR	 20	80	00	80	30	00	 16	  6		  0	  0	  0		  0	P				  0	  0	123							____________________	                                                                                                                                                                        MORE0067	
MORET0067	C	CHAR	 60	81	00	80	31	00	 16	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0068		CHAR	 20	80	00	80	30	00	 16	 58		  0	  0	  0		  0	P				  0	  0	124							____________________	                                                                                                                                                                        MORE0068	
MORET0068	C	CHAR	 60	81	00	80	31	00	 16	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0069		CHAR	 20	80	00	80	30	00	 16	110		  0	  0	  0		  0	P				  0	  0	125							____________________	                                                                                                                                                                        MORE0069	
MORET0069	C	CHAR	 60	81	00	80	31	00	 16	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0070		CHAR	 20	80	00	80	30	00	 18	  6		  0	  0	  0		  0	P				  0	  0	126							____________________	                                                                                                                                                                        MORE0070	
MORET0070	C	CHAR	 60	81	00	80	31	00	 18	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0071		CHAR	 20	80	00	80	30	00	 18	 58		  0	  0	  0		  0	P				  0	  0	127							____________________	                                                                                                                                                                        MORE0071	
MORET0071	C	CHAR	 60	81	00	80	31	00	 18	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0072		CHAR	 20	80	00	80	30	00	 18	110		  0	  0	  0		  0	P				  0	  0	128							____________________	                                                                                                                                                                        MORE0072	
MORET0072	C	CHAR	 60	81	00	80	31	00	 18	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0073		CHAR	 20	80	00	80	30	00	 20	  6		  0	  0	  0		  0	P				  0	  0	129							____________________	                                                                                                                                                                        MORE0073	
MORET0073	C	CHAR	 60	81	00	80	31	00	 20	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0074		CHAR	 20	80	00	80	00	00	 20	 58		  0	  0	  0		  0	P				  0	  0	130							____________________	                                                                                                                                                                        MORE0074	
MORET0074	C	CHAR	 60	81	00	80	31	00	 20	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0075		CHAR	 20	80	00	80	00	00	 20	110		  0	  0	  0		  0	P				  0	  0	131							____________________	                                                                                                                                                                        MORE0075	
MORET0075	C	CHAR	 60	81	00	80	31	00	 20	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0076		CHAR	 20	80	00	80	00	00	 22	  6		  0	  0	  0		  0	P				  0	  0	132							____________________	                                                                                                                                                                        MORE0076	
MORET0076	C	CHAR	 60	81	00	80	31	00	 22	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0077		CHAR	 20	80	00	80	00	00	 22	 58		  0	  0	  0		  0	P				  0	  0	133							____________________	                                                                                                                                                                        MORE0077	
MORET0077	C	CHAR	 60	81	00	80	31	00	 22	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0078		CHAR	 20	80	00	80	00	00	 22	110		  0	  0	  0		  0	P				  0	  0	134							____________________	                                                                                                                                                                        MORE0078	
MORET0078	C	CHAR	 60	81	00	80	31	00	 22	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0079		CHAR	 20	80	00	80	00	00	 24	  6		  0	  0	  0		  0	P				  0	  0	135							____________________	                                                                                                                                                                        MORE0079	
MORET0079	C	CHAR	 60	81	00	80	31	00	 24	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0080		CHAR	 20	80	00	80	00	00	 24	 58		  0	  0	  0		  0	P				  0	  0	136							____________________	                                                                                                                                                                        MORE0080	
MORET0080	C	CHAR	 60	81	00	80	31	00	 24	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0081		CHAR	 20	80	00	80	00	00	 24	110		  0	  0	  0		  0	P				  0	  0	137							____________________	                                                                                                                                                                        MORE0081	
MORET0081	C	CHAR	 60	81	00	80	31	00	 24	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0082		CHAR	 20	80	00	80	00	00	 26	  6		  0	  0	  0		  0	P				  0	  0	138							____________________	                                                                                                                                                                        MORE0082	
MORET0082	C	CHAR	 60	81	00	80	31	00	 26	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0083		CHAR	 20	80	00	80	00	00	 26	 58		  0	  0	  0		  0	P				  0	  0	139							____________________	                                                                                                                                                                        MORE0083	
MORET0083	C	CHAR	 60	81	00	80	31	00	 26	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0084		CHAR	 20	80	00	80	00	00	 26	110		  0	  0	  0		  0	P				  0	  0	140							____________________	                                                                                                                                                                        MORE0084	
MORET0084	C	CHAR	 60	81	00	80	31	00	 26	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0085		CHAR	 20	80	00	80	00	00	 28	  6		  0	  0	  0		  0	P				  0	  0	141							____________________	                                                                                                                                                                        MORE0085	
MORET0085	C	CHAR	 60	81	00	80	31	00	 28	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0086		CHAR	 20	80	00	80	00	00	 28	 58		  0	  0	  0		  0	P				  0	  0	142							____________________	                                                                                                                                                                        MORE0086	
MORET0086	C	CHAR	 60	81	00	80	31	00	 28	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0087		CHAR	 20	80	00	80	00	00	 28	110		  0	  0	  0		  0	P				  0	  0	143							____________________	                                                                                                                                                                        MORE0087	
MORET0087	C	CHAR	 60	81	00	80	31	00	 28	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0088		CHAR	 20	80	00	80	00	00	 30	  6		  0	  0	  0		  0	P				  0	  0	144							____________________	                                                                                                                                                                        MORE0088	
MORET0088	C	CHAR	 60	81	00	80	31	00	 30	 27		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0089		CHAR	 20	80	00	80	00	00	 30	 58		  0	  0	  0		  0	P				  0	  0	145							____________________	                                                                                                                                                                        MORE0089	
MORET0089	C	CHAR	 60	81	00	80	31	00	 30	 79		  0	  0	  0		 30					  0	  0								____________________________________________________________		
MORE0090		CHAR	 20	80	00	80	00	00	 30	110		  0	  0	  0		  0	P				  0	  0	146							____________________	                                                                                                                                                                        MORE0090	
MORET0090	C	CHAR	 60	81	00	80	31	00	 30	131		  0	  0	  0		 30					  0	  0								____________________________________________________________		
		CHAR	 20	80	10	00	00	00	255	  1	O	  0	  0	  0		  0					  0	  0								____________________		
%_FLOWLOGIC																																
PROCESS BEFORE OUTPUT.																																
  MODULE pbo_9002.																																
*																																
PROCESS AFTER INPUT.																																
  MODULE pai_9002.																															


