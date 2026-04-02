CLASS ZCL_ABAP_TOOLS DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS convert_timestamp_to_java
      IMPORTING
        !i_date         TYPE sydatum OPTIONAL
        !i_time         TYPE syuzeit OPTIONAL
        !i_msec         TYPE num03 DEFAULT 000
      RETURNING
        VALUE(rv_value) TYPE string .
    CLASS-METHODS general_random_string
      IMPORTING
        !i_length       TYPE i DEFAULT 4
      RETURNING
        VALUE(rv_value) TYPE string .
    CLASS-METHODS add_sm12_entries
      IMPORTING
        !it_seqta TYPE seqta_tt OPTIONAL
      EXPORTING
        !ev_subrc TYPE i .
    CLASS-METHODS del_sm12_entries
      IMPORTING
        !it_seqta TYPE seqta_tt OPTIONAL
      EXPORTING
        !ev_subrc TYPE i .
    CLASS-METHODS create_background_job
      IMPORTING
        !i_jobname      TYPE tbtco-jobname
        !i_username     TYPE syuname
        !i_program_name TYPE rs38m-programm
        !it_rsparams    TYPE rsparams_tt OPTIONAL
      EXCEPTIONS
        program_does_not_exist
        user_does_not_exist
        create_fail .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAP_TOOLS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_TOOLS=>ADD_SM12_ENTRIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SEQTA                       TYPE        SEQTA_TT(optional)
* | [<---] EV_SUBRC                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_sm12_entries.

    CHECK it_seqta[] IS NOT INITIAL.

    CALL FUNCTION 'ENQUEUE_ARRAY'
      TABLES
        enq_array      = it_seqta
      EXCEPTIONS
        argument_error = 1
        foreign_lock   = 2
        own_lock       = 3
        system_failure = 4
        table_overflow = 5
        OTHERS         = 6.

    ev_subrc = sy-subrc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_TOOLS=>CONVERT_TIMESTAMP_TO_JAVA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATE                         TYPE        SYDATUM(optional)
* | [--->] I_TIME                         TYPE        SYUZEIT(optional)
* | [--->] I_MSEC                         TYPE        NUM03 (default =000)
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD convert_timestamp_to_java.
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
* 从 1970-01-01, 00:00:00 GMT 到今天的天数
* one day has 86400 seconds
    lv_date            = '19700101'.
    IF i_date IS INITIAL.
      lv_days_i          = sy-datum - lv_date.
    ELSE.
      lv_days_i          = i_date - lv_date.
    ENDIF.

* 从 19700101 到今天的时间秒数
    lv_days_timestamp  = lv_days_i * lc_day_in_sec .
* 时区转换
    lv_days_timestamp  = lv_days_timestamp - ( 8 * 60 * 60 ).

* 当前时间的时间戳
    IF i_time IS INITIAL.
      lv_sec_i = sy-uzeit.
    ELSE.
      lv_sec_i = i_time.
    ENDIF.
    lv_secs_timestamp  = lv_sec_i.

    lv_timestamp = ( lv_days_timestamp + lv_secs_timestamp ) * 1000.
    rv_value = lv_timestamp.

    SPLIT rv_value AT '.' INTO rv_value lv_dummy.
    rv_value = rv_value + i_msec.

    SHIFT rv_value RIGHT DELETING TRAILING space.
    SHIFT rv_value LEFT  DELETING LEADING space.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_TOOLS=>CREATE_BACKGROUND_JOB
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_JOBNAME                      TYPE        TBTCO-JOBNAME
* | [--->] I_USERNAME                     TYPE        SYUNAME
* | [--->] I_PROGRAM_NAME                 TYPE        RS38M-PROGRAMM
* | [--->] IT_RSPARAMS                    TYPE        RSPARAMS_TT
* | [EXC!] PROGRAM_DOES_NOT_EXIST
* | [EXC!] USER_DOES_NOT_EXIST
* | [EXC!] CREATE_FAIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_background_job.
    SELECT COUNT(*) FROM progdir WHERE name = i_program_name AND state = 'A'.
    IF sy-subrc NE 0.
      RAISE program_does_not_exist.
    ENDIF.

    SELECT COUNT(*) FROM usr01 WHERE bname = i_username.
    IF sy-subrc NE 0.
      RAISE user_does_not_exist.
    ENDIF.

    DATA:lv_job_nr           TYPE tbtco-jobcount,
         lv_job_released     TYPE c,
         lv_job_start_sofort TYPE c,
         lv_xpgtgtsys        TYPE tbtcstep-xpgtgtsys,
         lv_print_parameters TYPE pri_params.

    "打开JOB 通过JOB name 获得JOB号
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = i_jobname
      IMPORTING
        jobcount         = lv_job_nr
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH  sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         RAISING create_fail.
    ENDIF.

    SUBMIT (i_program_name)
        WITH SELECTION-TABLE  it_rsparams
        USER i_username
        VIA JOB i_jobname NUMBER lv_job_nr AND RETURN.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH  sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         RAISING create_fail.
    ENDIF.

    "运行结束  关闭JOB
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_job_nr
        jobname              = i_jobname
        strtimmed            = 'X'
      IMPORTING
        job_was_released     = lv_job_released
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH  sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
         RAISING create_fail.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_TOOLS=>DEL_SM12_ENTRIES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SEQTA                       TYPE        SEQTA_TT
* | [<---] EV_SUBRC                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD del_sm12_entries.

    CHECK it_seqta[] IS NOT INITIAL.

    CALL FUNCTION 'DEQUEUE_ARRAY'
      TABLES
        enq_array      = it_seqta
      EXCEPTIONS
        system_failure = 1
        OTHERS         = 2.

    ev_subrc  = sy-subrc.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_TOOLS=>GENERAL_RANDOM_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LENGTH                       TYPE        I (default =4)
* | [<-()] RV_VALUE                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD general_random_string.
    DATA: albet(80),
          output(255),
          l_alphabetlen TYPE i,
          outputlen     TYPE i.
    CONCATENATE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                'abcdefghijklmnopqrstuvwxyz'
                '1234567890' INTO albet.

    l_alphabetlen = strlen( albet ).

    outputlen = i_length.
    IF outputlen > 255.
      outputlen = 255.
    ENDIF.

    CALL 'RSEC_GEN_PASSWD'
             ID 'ALPHABET'    FIELD albet
             ID 'ALPHABETLEN' FIELD l_alphabetlen
             ID 'OUTPUT'      FIELD output
             ID 'OUTPUTLEN'   FIELD outputlen
             ID 'FORCE_INIT'  FIELD ''.                   "#EC CI_CCALL
    IF sy-subrc <> 0.
* 415(01) In der Kernelroutine RSEC_GEN_PASSWD trat ein Problem auf
    ENDIF.

    DATA : lv_char_string TYPE cl_susr_basic_tools=>ty_char256.
    IF ( count( val = output   sub = `@` ) > 1 ).
      lv_char_string = output.
      output = cl_susr_basic_tools=>eliminate_potential_icons( iv_char_string = lv_char_string ).
    ENDIF.

    rv_value = output.
  ENDMETHOD.
ENDCLASS.