"How I called in the program -->  DATA(lo_excel) = znt_001_cl_001=>upload_xlsx( iv_filename = CONV string( p_fname ) ).  

  METHOD upload_xlsx.

    DATA: lt_records  TYPE solix_tab.

    ro_myself = NEW #( ).

    IF iv_filename IS INITIAL.
      ev_filepath = file_open_dialog( ).
      IF ev_filepath IS INITIAL.
        RETURN.
      ENDIF.

    ELSE.
      IF abap_true NE check_filename( iv_filename = iv_filename ).
        "File extension must be XLSX!
        RAISE EXCEPTION TYPE znt_001_cx_001
                  MESSAGE ID znt_001_cx_001=>mc_message_class
                        TYPE znt_001_cx_001=>mc_message_types-error
                      NUMBER znt_001_cx_001=>mc_messages-file_ext_must_be_xlsx.
      ENDIF.

      ev_filepath = iv_filename.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = ev_filepath
        filetype                = 'BIN'
      IMPORTING
        filelength              = DATA(lv_filelength)
      CHANGING
        data_tab                = lt_records
      EXCEPTIONS
        file_open_error         = 1  " File does not exist and cannot be opened
        file_read_error         = 2  " Error when reading file
        no_batch                = 3  " Cannot execute front-end function in background
        gui_refuse_filetransfer = 4  " Incorrect front end or error on front end
        invalid_type            = 5  " Incorrect parameter FILETYPE
        no_authority            = 6  " No upload authorization
        unknown_error           = 7  " Unknown error
        bad_data_format         = 8  " Cannot Interpret Data in File
        header_not_allowed      = 9  " Invalid header
        separator_not_allowed   = 10 " Invalid separator
        header_too_long         = 11 " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12 " Error when calling data provider
        access_denied           = 13 " Access to file denied.
        dp_out_of_memory        = 14 " Not enough memory in data provider
        disk_full               = 15 " Storage medium is full.
        dp_timeout              = 16 " Data provider timeout
        not_supported_by_gui    = 17 " GUI does not support this
        error_no_gui            = 18 " GUI not available
        OTHERS                  = 19
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_gui_upload_message).

      "& & & &
      RAISE EXCEPTION TYPE znt_001_cx_001
                MESSAGE ID znt_001_cx_001=>mc_message_class
                      TYPE znt_001_cx_001=>mc_message_types-error
                    NUMBER znt_001_cx_001=>mc_messages-generic
                      WITH lv_gui_upload_message.
    ENDIF.

    ro_myself->mv_xml = cl_bcs_convert=>solix_to_xstring( it_solix = lt_records ).

  ENDMETHOD. 
