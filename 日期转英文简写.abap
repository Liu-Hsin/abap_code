FUNCTION zfm_conversion_date_format.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
  DATA:lv_year  TYPE apyear,
       lv_month TYPE fcmnr,
       lv_day   TYPE day_nr,
       ls_t247  TYPE t247.
  DATA:BEGIN OF lt_days OCCURS 0,
      day  TYPE day_nr,
      day_en TYPE text6,
    END OF lt_days.
  DEFINE add_days.
    lt_days-day  = &1.
    lt_days-day_en = &2.
    append lt_days.clear lt_days.
  END-OF-DEFINITION.
  CHECK input IS NOT INITIAL.
  lv_year = input+0(4).
  lv_month = input+4(2).
  lv_day = input+6(2).
  SELECT SINGLE * FROM t247 INTO ls_t247  WHERE spras = 'EN' AND mnr = lv_month.
  add_days: '01'  ' 1st'   ,
            '02'  ' 2nd'   ,
            '03'  ' 3rd'   ,
            '04'  ' 4th'   ,
            '05'  ' 5th'   ,
            '06'  ' 6th'   ,
            '07'  ' 7th'   ,
            '08'  ' 8th'   ,
            '09'  ' 9th'   ,
            '10'  ' 10th'  ,
            '11'  ' 11th'  ,
            '12'  ' 12th'  ,
            '13'  ' 13th'  ,
            '14'  ' 14th'  ,
            '15'  ' 15th'  ,
            '16'  ' 16th'  ,
            '17'  ' 17th'  ,
            '18'  ' 18th'  ,
            '19'  ' 19th'  ,
            '20'  ' 20th'  ,
            '21'  ' 21st'  ,
            '22'  ' 22nd'  ,
            '23'  ' 23rd'  ,
            '24'  ' 24th'  ,
            '25'  ' 25th'  ,
            '26'  ' 26th'  ,
            '27'  ' 27th'  ,
            '28'  ' 28th'  ,
            '29'  ' 29th'  ,
            '30'  ' 30th'  ,
            '31'  ' 31st'  .
  READ TABLE lt_days WITH KEY day = lv_day.
  CONCATENATE ls_t247-ltx+0(3)  lt_days-day_en ',' lv_year INTO output.
ENDFUNCTION.
