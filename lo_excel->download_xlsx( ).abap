"How I used in the program  -->  lo_excel->download_xlsx( ).

  METHOD download_xlsx.

    DATA: lv_filepath TYPE string,
          lv_length   TYPE i,
          lt_content  TYPE STANDARD TABLE OF tdline.

    IF iv_filename IS INITIAL.

      lv_filepath = file_save_dialog( ).

      IF lv_filepath IS INITIAL.
        RETURN.
      ENDIF.

    ELSE.
      lv_filepath = iv_filename.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = mv_xml
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_content.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = lv_length
        filename                = lv_filepath
        filetype                = 'BIN'
      TABLES
        data_tab                = lt_content
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

  ENDMETHOD. 
