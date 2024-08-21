FUNCTION zfm_amount_to_chinese.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_DMBTR) TYPE  FIN_AMOUNT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_DMBTR)
*"----------------------------------------------------------------------

  DATA: scr(30) TYPE c,
        res(60) TYPE c,
        fen(2)  TYPE c.
  DATA: len TYPE i,
        c1  TYPE i,
        c2  TYPE i,
        c3  TYPE i,
        c4  TYPE i.
  DATA: d1(1) TYPE c,
        d2(1) TYPE c,
        d3    TYPE i.
  DATA: digit(2)  TYPE c,
        weight(2) TYPE c.
  DATA: rule1(20) TYPE c VALUE '零壹贰叁肆伍陆柒捌玖'.
  DATA: rule2(30) TYPE c VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  DATA: lv_dmbtr TYPE fin_amount.

  lv_dmbtr = i_dmbtr.
  IF i_dmbtr >= 0.
    lv_dmbtr = i_dmbtr.
  ELSE.
    lv_dmbtr = abs( i_dmbtr ).
  ENDIF.

  scr = lv_dmbtr * 100.
  CONDENSE scr NO-GAPS.
  IF scr = '0'.
    res = '零元'.
  ELSE.
    len = strlen( scr ).
    c1 = 0.
    d1 = '0'.
    CLEAR res.
    DO len TIMES.
      c1 = c1 + 1.
      c2 = len - c1.
      d2 = scr+c2(1) .
      IF d2 = '0'.
        d3 = 0.
      ELSE.
        d3 = d2.
      ENDIF.
      digit = rule1+d3(1) .
      c3 = ( c1 - 1 ) .
      weight = rule2+c3(1) .
      IF d2 = '0'.
        IF c1 = 3.
          digit = ''.
        ELSEIF c1 = 7.
          digit = ''.
          IF len > 10 .
            c4 = len - 10.
            IF scr+c4(4) = '0000'.
              weight = ''.
            ENDIF.
          ENDIF.
        ELSEIF c1 = 11.
          digit = ''.
        ELSEIF d1 = '0'.
          digit = ''.
          weight = ''.
        ELSE.
          weight = ''.
        ENDIF.
      ENDIF.
      CONCATENATE digit weight res INTO res .
      d1 = d2.
    ENDDO.
  ENDIF.
  len = strlen( res ) - 1.
  fen = res+len(1).
  IF fen <> '分' .
    CONCATENATE res '整' INTO e_dmbtr.
  ELSE.
    e_dmbtr = res.
  ENDIF.

  IF i_dmbtr >= 0.
    e_dmbtr = e_dmbtr.
  ELSE.
    e_dmbtr = '负' && e_dmbtr.
  ENDIF.

ENDFUNCTION.
