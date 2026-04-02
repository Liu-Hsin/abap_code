* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_NORMAL_TOOLS=>ABAP_TIMESTAMP_TO_JAVA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE                        TYPE        SYDATE
* | [--->] IV_TIME                        TYPE        SYUZEIT
* | [--->] IV_MSEC                        TYPE        NUM03 (default =000)
* | [<-()] EV_TIMESTAMP                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD abap_timestamp_to_java.
    DATA:
      lv_date           TYPE sy-datum,
      lv_days_timestamp TYPE timestampl,
      lv_secs_timestamp TYPE timestampl,
      lv_days_i         TYPE i,
      lv_sec_i          TYPE i,
      lv_timestamp      TYPE timestampl,
      lv_dummy          TYPE string.                        "#EC NEEDED

    CONSTANTS:
       lc_day_in_sec TYPE i VALUE 86400.

* Milliseconds for the days since January 1, 1970, 00:00:00 GMT
* one day has 86400 seconds
    lv_date            = '19700101'.
    lv_days_i          = iv_date - lv_date.
* Timestamp for passed days until today in seconds " 时区转换，转为北京时间 ，相差（ 8 * 60 * 60 ）秒
    lv_days_timestamp  = lv_days_i * lc_day_in_sec - ( 8 * 60 * 60 ).

    lv_sec_i          = iv_time.
* Timestamp for time at present day
    lv_secs_timestamp = lv_sec_i.

    lv_timestamp = ( lv_days_timestamp + lv_secs_timestamp ) * 1000.
    ev_timestamp = lv_timestamp.

    SPLIT ev_timestamp AT '.' INTO ev_timestamp lv_dummy.
    ev_timestamp = ev_timestamp + iv_msec.

    SHIFT ev_timestamp RIGHT DELETING TRAILING space.
    SHIFT ev_timestamp LEFT  DELETING LEADING space.

  ENDMETHOD.
