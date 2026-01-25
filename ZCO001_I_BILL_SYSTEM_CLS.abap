*&---------------------------------------------------------------------*
*& Include          ZCO001_I_BILL_SYSTEM_CLS
*&---------------------------------------------------------------------*
CLASS lcl_main_controller DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      mt_main     TYPE TABLE OF zco001_s_alv_screen,
      mt_excel    TYPE TABLE OF zco001_s_excel,
      mr_excel    TYPE REF TO data,
      mv_filepath TYPE string,

      BEGIN OF ms_alv,
        BEGIN OF s0100,
          cont TYPE REF TO cl_gui_custom_container,
        END OF s0100,

        BEGIN OF s0101,
          itab TYPE STANDARD TABLE OF zco001_s_alv_screen,
          grid TYPE REF TO cl_gui_alv_grid,
          fcat TYPE lvc_t_fcat,
          layo TYPE lvc_s_layo,
          vari TYPE disvariant,
          sort TYPE lvc_t_sort,
        END OF s0101,
      END OF ms_alv.


    CLASS-METHODS:
      end_of_selection,
      get_data,
      pbo IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,
      pai IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,
      ext IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,

      main_alv  IMPORTING VALUE(iv_scrn_alv)  TYPE sy-dynnr
                          VALUE(iv_scrn_cont) TYPE sy-dynnr,
      build_cont IMPORTING VALUE(iv_alv_name) TYPE scrfname
                 RETURNING VALUE(ro_cont)     TYPE REF TO cl_gui_custom_container,
      fill_main_fieldcat IMPORTING VALUE(iv_structure) TYPE string
                                   iv_scrn             TYPE char4
                         RETURNING VALUE(rt_fcat)      TYPE lvc_t_fcat,
      build_grid IMPORTING VALUE(io_cont) TYPE REF TO cl_gui_custom_container
                 RETURNING VALUE(ro_grid) TYPE REF TO cl_gui_alv_grid,
      build_layo IMPORTING VALUE(iv_scrn) TYPE char4
                 RETURNING VALUE(rs_layo) TYPE lvc_s_layo,
      build_vari IMPORTING VALUE(iv_scrn) TYPE char4
                 RETURNING VALUE(rs_vari) TYPE disvariant,
      fill_main_sort IMPORTING VALUE(iv_scrn) TYPE char4
                     RETURNING VALUE(rt_sort) TYPE lvc_t_sort,
      handle_toolbar_alv FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING sender  e_interactive  e_object,
      user_command_alv   FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      refresh_alv IMPORTING io_grid TYPE REF TO cl_gui_alv_grid,
      download_excel_template,
      selection_screen_output,
      excel_upload,
      initial_control,
      fetch_file,
      reach_maintenance_table,
      maintenance_excel_upload,
      save_to_log_table,
      delete_from_log_table,
      call_bapi_acc_document_post,
      bapi_acc_document_rev_post,
      create_table
        IMPORTING iv_stname         TYPE dd02l-tabname OPTIONAL
        EXPORTING et_table_skeleton TYPE REF TO data
        CHANGING  it_fcat           TYPE lvc_t_fcat.

    "Excel Template    ZCO001_S_EXCEL
    "ALV Structure     ZCO001_S_ALV_SCREEN
    "Log Table         ZCO001_T_LOG
    "Maintenance Table ZCO001_T_MAINT
  PRIVATE SECTION.

ENDCLASS.
CLASS lcl_main_controller IMPLEMENTATION.
  METHOD initial_control.
    IF p_run IS NOT INITIAL.
      IF p_fname IS NOT INITIAL.
        excel_upload( ).
      ELSEIF p_fname IS INITIAL.
        MESSAGE s000 DISPLAY LIKE 'E'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD Create_table.
    DATA(lo_table_create) = NEW cl_alv_table_create( ).
    DATA(lv_str_alv) = 'S101'.
    lo_table_create->create_dynamic_table(
      EXPORTING
        it_fieldcatalog           = fill_main_fieldcat( iv_scrn = lv_str_alv iv_structure = gt_structure2 )
      IMPORTING
        ep_table                  = et_table_skeleton
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2 ).

  ENDMETHOD.
  METHOD excel_upload.

    DATA lv_ratio TYPE p LENGTH 3 DECIMALS 2.
    DATA lv_ratio_text TYPE char4.
    DATA lv_decimal TYPE p LENGTH 3 DECIMALS 2.

    IF Mr_Excel IS NOT BOUND.
      DATA(lv_str_alv) = 'S101'.
      DATA(lt_fcat) = fill_main_fieldcat( iv_scrn = lv_str_alv iv_structure = gt_structure2 ).
      create_table(
       IMPORTING
         et_table_skeleton = mr_excel
         CHANGING
         it_fcat           = lt_fcat ).
    ENDIF.


    TRY.
        DATA(lo_excel) = znt_001_cl_001=>upload_xlsx( iv_filename = CONV string( p_fname ) ).

        lo_excel->get_data_by_worksheet_index(
           EXPORTING
             iv_index           = 1
             iv_data_has_header = abap_true
           CHANGING
             cr_data            = mr_excel   ).
      CATCH znt_001_cx_001 INTO DATA(lx_excel).
        MESSAGE i001 DISPLAY LIKE 'E' .
    ENDTRY.
    IF mr_excel IS BOUND.
      mt_excel = CORRESPONDING #( mr_excel->* ).

      SELECT FROM zco001_t_mainten
        INNER JOIN @mt_excel AS mt_excel ON mt_excel~tariff_id_1  = zco001_t_mainten~tariff1
                                        AND mt_excel~tariff_id_2  = zco001_t_mainten~traiff2
                                        AND mt_excel~description  = zco001_t_mainten~description
                                        AND mt_excel~invoice_type = zco001_t_mainten~invoice_type
        FIELDS tariff1,
               traiff2,
               zco001_t_mainten~description,
               zco001_t_mainten~invoice_type,
               zonnet,
               zoffnet,
               international,
               local_data,
               sms,
               short_number,
               simcard,
               roam_voice,
               roam_data,
               roam_sms,
               m2m,
               transmission,
               other,
               interconnection,
               foreign_visitor,
               handset_revenue,
               date_start,
               date_end ORDER BY tariff1, traiff2, zco001_t_mainten~description
        INTO TABLE @DATA(lt_maintenance).

      LOOP AT mt_excel REFERENCE INTO DATA(lr_excel).
        DATA(lv_excel_tabix) = sy-tabix.
        READ TABLE lt_maintenance REFERENCE INTO DATA(lr_maintenance)
           WITH KEY tariff1      = lr_excel->tariff_id_1 traiff2      = lr_excel->tariff_id_2
                    description  = lr_excel->description invoice_type = lr_excel->invoice_type
           BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE s010 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ELSE.
          CASE lr_excel->pnl_group.
            WHEN 'Foreign Visitor Roaming'.
              lv_ratio_text = lr_maintenance->foreign_visitor.
            WHEN 'Handset Revenue'.
              lv_ratio_text = lr_maintenance->handset_revenue.
            WHEN 'Interconnection revenue'.
              lv_ratio_text = lr_maintenance->interconnection.
            WHEN 'International'.
              lv_ratio_text = lr_maintenance->international.
            WHEN 'Local Data'.
              lv_ratio_text = lr_maintenance->local_data.
            WHEN 'M2M'.
              lv_ratio_text = lr_maintenance->m2m.
            WHEN 'Offnet'.
              lv_ratio_text = lr_maintenance->zoffnet.
            WHEN 'Onnet'.
              lv_ratio_text = lr_maintenance->zonnet.
            WHEN 'Other'.
              lv_ratio_text = lr_maintenance->other.
            WHEN 'Roaming Data'.
              lv_ratio_text = lr_maintenance->roam_data.
            WHEN 'Roaming SMS'.
              lv_ratio_text = lr_maintenance->roam_sms.
            WHEN 'Roaming Voice'.
              lv_ratio_text = lr_maintenance->roam_voice.
            WHEN 'Short number services'.
              lv_ratio_text = lr_maintenance->short_number.
            WHEN 'Simcard'.
              lv_ratio_text = lr_maintenance->simcard.
            WHEN 'Simcard Sales'.
              lv_ratio_text = lr_maintenance->simcard.
            WHEN 'SMS'.
              lv_ratio_text = lr_maintenance->sms.
            WHEN 'Transmission'.
              lv_ratio_text = lr_maintenance->transmission.
          ENDCASE.
        ENDIF.

        SPLIT lv_ratio_text AT '%' INTO DATA(lv_left) DATA(lv_right).
        lv_ratio = lv_left / 100.
*        lv_ratio_text = |{ lv_ratio * 100 }|.
*        SPLIT lv_ratio_text AT '.' INTO DATA(lv_left) DATA(lv_right).
*        lv_ratio_text = |{ lv_left }%|.

*        APPEND INITIAL LINE TO lt_log_table REFERENCE INTO DATA(lr_log).
*        lr_log->tariff_id_1        = lr_excel->tariff_id_1.
*        lr_log->tariff_id_2        = lr_excel->tariff_id_2.
*        lr_log->pnl_group          = lr_excel->pnl_group.
*        lr_log->cust_segment       = lr_excel->cust_segment.
*        lr_log->invoice_type       = lr_excel->invoice_type.
*        lr_log->related_report     = lr_excel->related_report.
*        lr_log->related_table      = lr_excel->related_table.
*        lr_log->related_sheet      = lr_excel->related_sheet.
*        lr_log->tariff_type        = lr_excel->tariff_type.
*        lr_log->amount             = lr_excel->amount.
*        lr_log->amount_vat         = lr_excel->amount_vat.
*        lr_log->ratio              = lv_ratio_text.
*        lr_log->distributed_amount = lr_excel->amount * lv_ratio.

        APPEND INITIAL LINE TO mt_main REFERENCE INTO DATA(lr_main).
        IF lr_excel->tariff_id_1 IS NOT INITIAL.
          lr_main->tariff1            = lr_excel->tariff_id_1.
        ELSE.
          MESSAGE s011 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        IF lr_excel->tariff_id_2 IS NOT INITIAL.
          lr_main->tariff2            = lr_excel->tariff_id_2.
        ELSE.
          MESSAGE s012 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        IF lr_excel->description IS NOT INITIAL.
          lr_main->description        = lr_excel->description.
        ELSE.
          MESSAGE s013 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        IF lr_excel->pnl_group IS NOT INITIAL.
          lr_main->pnl_group          = lr_excel->pnl_group.
        ELSE.
          MESSAGE s015 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        lr_main->cust_segment       = lr_excel->cust_segment.
        IF lr_excel->invoice_type IS NOT INITIAL.
          lr_main->invoice_type       = lr_excel->invoice_type.
        ELSE.
          MESSAGE s014 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        lr_main->related_report     = lr_excel->related_report.
        lr_main->related_table      = lr_excel->related_table.
        lr_main->related_sheet      = lr_excel->related_sheet.
        lr_main->tariff_type        = lr_excel->tariff_type.
        lr_main->amount             = lr_excel->amount.
        IF lr_main->invoice_type    = 'Prepaid'.
          lv_decimal = ( 18 / 100 ) + 1.
          lr_main->amount_vat       = lr_main->amount / lv_decimal.
        ELSE.
          lr_main->amount_vat       = lr_excel->amount_vat.
        ENDIF.
        lr_main->ratio              = lv_ratio_text.
        lr_main->distributed_amount = lr_excel->amount_vat * lv_ratio.
        lr_main->icon               = '@09@'.


        DATA(lv_low) = |{ s_time+5(2) }.{ s_time+0(4) }|.
        lr_main->period_year = lv_low.
*        DATA(lv_control_period) = lr_excel->period_year+0(2).
*        IF  lv_control_period = '01' OR lv_control_period = '02' OR lv_control_period = '03' OR lv_control_period = '04'
*          OR lv_control_period = '05' OR lv_control_period = '06' OR lv_control_period = '07' OR lv_control_period = '08'
*          OR lv_control_period = '09'.
*          lr_main->period_year        = lr_excel->period_year.
*        ELSE.
*          IF lv_control_period = '10' OR lv_control_period = '11' OR lv_control_period = '12'.
*            lr_main->period_year        = lr_excel->period_year.
*          ELSE.
*            lr_main->period_year        = |0{ lr_excel->period_year+0(1) }.{ lr_excel->period_year+2(4) }|.
*          ENDIF.
*        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD get_data.

    TYPES: BEGIN OF ty_acdoca_current,
             saknr2 TYPE saknr,
             hsl2   TYPE fins_vhcur12,
           END OF ty_acdoca_current,

           BEGIN OF ty_acdoca_previous,
             skanr1 TYPE saknr,
             hsl1   TYPE fins_vhcur12,
           END OF ty_acdoca_previous,
           BEGIN OF ty_linetype,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE char7,
             high   TYPE char7,
           END OF ty_linetype,
           tt_date TYPE TABLE OF ty_linetype.
    DATA lr_date TYPE tt_date.

    DATA lt_acdoca_current TYPE TABLE OF ty_acdoca_current.
    DATA lt_acdoca_current2 TYPE TABLE OF ty_acdoca_current.
    DATA lt_acdoca_previous TYPE TABLE OF ty_acdoca_previous.
    DATA lt_acdoca_previous2 TYPE TABLE OF ty_acdoca_previous.

    DATA: lv_final_total_zhsl2      TYPE acdoca-hsl,
          lv_final_total_zhsl1      TYPE acdoca-hsl,
          lv_final_total_difference TYPE acdoca-hsl.

    IF p_run IS INITIAL.
      DATA(lv_low) = |{ s_time+5(2) }.{ s_time+0(4) }|.
*      DATA(lv_high) = |{ s_time+0(4) }{ s_time+5(2) }|.
      lr_date = VALUE #( ( sign = 'I' option = 'EQ' low = lv_low  ) ).
      "high = lv_high
      SELECT FROM zco001_T_log
      FIELDS tariff_id_1 AS tariff1,
             tariff_id_2 AS tariff2,
             pnl_group AS pnl_group,
             description AS description,
             cust_segment AS cust_segment,
             invoice_type AS invoice_type,
             related_report AS related_report,
             related_table AS related_table,
             related_sheet AS related_sheet,
             tariff_type AS tariff_type,
             amount AS amount,
             amount_vat AS amount_vat,
             ratio AS ratio,
             distributed_amount AS distributed_amount,
             doc_no AS document_no,
             re_doc_no AS re_document_no,
             posting_date AS document_date,
             period_year AS period_year,
             icon AS icon
        WHERE period_year IN @lr_date
        ORDER BY tariff_id_1, tariff_id_2, pnl_group
        %_HINTS ORACLE 'INDEX("ZCO001_T_LOG","ZCO001_T_LOG~D01")'
      INTO TABLE @DATA(lt_log).

      mt_main = CORRESPONDING #( lt_log  MAPPING tariff1            = tariff1
                                                 tariff2            = tariff2
                                                 cust_segment       = cust_segment
                                                 description        = description
                                                 invoice_type       = invoice_type
                                                 related_report     = related_report
                                                 related_table      = related_table
                                                 related_sheet      = related_sheet
                                                 tariff_type        = tariff_type
                                                 amount             = amount
                                                 amount_vat         = amount_vat
                                                 ratio              = ratio
                                                 distributed_amount = distributed_amount
                                                 document_no        = document_no
                                                 re_document_no     = re_document_no
                                                 document_date      = document_date
                                                 period_year        = period_year
                                                 icon               = icon               ).
    ENDIF.
    SORT mt_main BY tariff1 tariff2 pnl_group.

  ENDMETHOD.
  METHOD end_of_selection.
    CALL SCREEN 0100.
  ENDMETHOD.
  METHOD main_alv.

    DATA lt_exclude TYPE ui_functions.
    FIELD-SYMBOLS : <lo_grid> TYPE REF TO cl_gui_alv_grid,
                    <lo_prnt> TYPE REF TO cl_gui_container,
                    <lo_cont> TYPE REF TO cl_gui_custom_container.

    DATA(lv_str_alv) = 'S' && iv_scrn_alv.
    ASSIGN COMPONENT lv_str_alv OF STRUCTURE ms_alv TO FIELD-SYMBOL(<ls_alv>).
    IF <ls_alv> IS ASSIGNED.
      ASSIGN COMPONENT ms_alv_components-grid OF STRUCTURE <ls_alv> TO <lo_grid>.
      IF <lo_grid> IS ASSIGNED.


        "GRID INITIAL CONTROL if it is bound then flush( ).
        IF <lo_grid> IS NOT BOUND.
          "GRID
          DATA(lv_str_gui) = 'S' && iv_scrn_cont.
          ASSIGN COMPONENT lv_str_gui OF STRUCTURE ms_alv TO FIELD-SYMBOL(<ls_gui>).
          IF <ls_gui> IS ASSIGNED.
            ASSIGN COMPONENT ms_alv_components-cont OF STRUCTURE <ls_gui> TO <lo_cont>.
            IF <lo_cont> IS ASSIGNED.
              <lo_grid> = build_grid( io_cont = <lo_cont> ).
            ENDIF.
          ENDIF.


          "FCAT
          ASSIGN COMPONENT ms_alv_components-fcat OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lt_fcat>).
          IF <lt_fcat> IS ASSIGNED.
            CLEAR : <lt_fcat>.
            <lt_fcat> = fill_main_fieldcat( iv_scrn = iv_scrn_alv iv_structure = gt_structure1 ).
          ENDIF.

          "LAYOUT
          ASSIGN COMPONENT ms_alv_components-layo OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<ls_layo>).
          IF <ls_layo> IS ASSIGNED.
            CLEAR : <ls_layo>.
            <ls_layo> = build_layo( iv_scrn = iv_scrn_alv ).
          ENDIF.

          "VARIANT
          ASSIGN COMPONENT ms_alv_components-vari OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<ls_vari>).
          IF <ls_vari> IS ASSIGNED.
            CLEAR : <ls_vari>.
            <ls_vari> = build_vari( iv_scrn = iv_scrn_alv ).
          ENDIF.

          "SORT
          ASSIGN COMPONENT ms_alv_components-sort OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lt_sort>).
          IF <lt_sort> IS ASSIGNED.
            CLEAR : <lt_sort>.
            <lt_sort> = fill_main_sort( iv_scrn = iv_scrn_alv ).
          ENDIF.


          CHECK sy-subrc IS INITIAL.

          "ALV
          ASSIGN COMPONENT ms_alv_components-itab OF STRUCTURE <ls_alv> TO FIELD-SYMBOL(<lt_itab>).
          IF <lt_itab> IS ASSIGNED.


            ASSIGN mt_main TO <lt_itab>.
            SET HANDLER handle_toolbar_alv FOR ms_alv-s0101-grid."To handle the standard toolbar
            SET HANDLER user_command_alv   FOR ms_alv-s0101-grid.

*            LOOP AT <lt_itab> ASSIGNING FIELD-SYMBOL(<lfs_tab>). "where icon <> '@08@'.
*              ASSIGN COMPONENT 'ICON' OF STRUCTURE <lfs_tab> TO FIELD-SYMBOL(<lfs_value>).
*              IF sy-subrc IS INITIAL.
*                <lfs_value> = '@S_TL_Y@'.
*              ENDIF.
*              UNASSIGN <lfs_value>.
*            ENDLOOP.


            CALL METHOD <lo_grid>->set_table_for_first_display
              EXPORTING
                i_buffer_active               = abap_true
                is_layout                     = <ls_layo>
                i_save                        = 'A'
                it_toolbar_excluding          = lt_exclude
              CHANGING
                it_outtab                     = <lt_itab>
                it_fieldcatalog               = <lt_fcat>
                it_sort                       = <lt_sort>
              EXCEPTIONS
                invalid_parameter_combination = 1
                program_error                 = 2
                too_many_lines                = 3
                OTHERS                        = 4.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.
        ELSE.
          cl_gui_cfw=>flush( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD build_grid.

    CREATE OBJECT ro_grid
      EXPORTING
        i_parent = io_cont.

  ENDMETHOD.
  METHOD build_layo.
*    rs_layo-grid_title = |{ p_explan }|.
    rs_layo-zebra = abap_true.
    rs_layo-sel_mode   = 'A'.
    rs_layo-col_opt = abap_true.
    rs_layo-cwidth_opt = abap_true.
*    rs_layo-ctab_fname = 'SCOL'.

*    CASE iv_scrn.
*      WHEN ms_scr-s0101.
*        rs_layo-no_rowmark = abap_true. "Removes row select in ALV screen.
*    ENDCASE.

  ENDMETHOD.
  METHOD build_vari.

    CASE iv_scrn.
      WHEN ms_scr-s0101.
        rs_vari = VALUE #( report = sy-repid username = sy-uname handle = iv_scrn ).
    ENDCASE.

  ENDMETHOD.
  METHOD fill_main_fieldcat.
    DATA: lv_fname     TYPE lvc_fname,
          lv_offset    TYPE i,
          lv_structure TYPE dd02l-tabname.

*CASE abap_true.
    lv_structure = iv_structure.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = lv_structure
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT rt_fcat REFERENCE INTO DATA(lr_fcat).
      CASE lr_fcat->fieldname.
        WHEN 'ICON'.
          lr_fcat->scrtext_l =
          lr_fcat->scrtext_m =
          lr_fcat->scrtext_s =
          lr_fcat->reptext = TEXT-001."'Document Status'.
          lr_fcat->coltext = TEXT-001."'Document Status'.
*          lr_fcat->tech = abap_true.
        WHEN 'WAERS'.
          lr_fcat->no_out = abap_true.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.
  METHOD fill_main_sort.
  ENDMETHOD.
  METHOD build_cont.

    CREATE OBJECT ro_cont
      EXPORTING
        container_name = iv_alv_name.

  ENDMETHOD.
  METHOD pai.
  ENDMETHOD.
  METHOD pbo.

    DATA(lv_status) = |{ ms_gui-status }{ iv_scrn }|.
    SET PF-STATUS lv_status.

    DATA(lv_title) = |{ ms_gui-title }{ iv_scrn }|.
    SET TITLEBAR lv_title.

    CASE iv_scrn.
      WHEN ms_scr-s0100.
        IF ms_alv-s0100-cont IS NOT BOUND.

          " Build Container
          ms_alv-s0100-cont = build_cont( iv_alv_name = |{ 'F_' }{ iv_scrn }| ).
          main_alv( iv_scrn_cont = iv_scrn  iv_scrn_alv = ms_scr-s0101 ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD ext.

    CASE sy-ucomm.
      WHEN ms_ucomm-back.
        LEAVE TO SCREEN 0.
      WHEN ms_ucomm-leave.
        LEAVE TO SCREEN 0.
      WHEN ms_ucomm-exit.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.
  METHOD handle_toolbar_alv.

    e_object->mt_toolbar[ 10 ]-disabled = abap_false. "To enable sub-total
*    DELETE e_object->mt_toolbar INDEX 10."[ 10 ]-disabled = abap_true. "To desable standard APPEND BUTTON

    "Separator
    APPEND VALUE #( butn_type = '3' disabled  = abap_true ) TO e_object->mt_toolbar.

    IF p_run = abap_true.
      APPEND VALUE #( function  = 'RUN'
                      icon      = icon_detail
                      text      = TEXT-008"'Run'
                      quickinfo = TEXT-002"'Posts selected Excel lines.'
                      disabled  = abap_false ) TO e_object->mt_toolbar.

      APPEND VALUE #( function  = 'DOWNLOAD'
                      icon      = icon_detail
                      text      = TEXT-007"'Download'
                      quickinfo = TEXT-003"'Downloads an Excel template.'
                      disabled  = abap_false ) TO e_object->mt_toolbar.

      APPEND VALUE #( function  = 'SAVE'
                      icon      = icon_detail
                      text      = TEXT-012"'Save'
                      quickinfo = TEXT-013"'Saves selected lines to log table.'
                      disabled  = abap_false ) TO e_object->mt_toolbar.

    ELSEIF p_displa = abap_true.
      APPEND VALUE #( function  = 'RUN'
                      icon      = icon_detail
                      text      = TEXT-008"'Run'
                      quickinfo = TEXT-002"'Applies selected Excel lines to log table.'
                      disabled  = abap_false ) TO e_object->mt_toolbar.

      APPEND VALUE #( function  = 'DOWNLOAD'
                      icon      = icon_detail
                      text      = TEXT-007"'Download'
                      quickinfo = TEXT-003"'Downloads an Excel template.'
                      disabled  = abap_false ) TO e_object->mt_toolbar.
      APPEND VALUE #( function  = 'DELETE'
                      icon      = icon_detail
                      text      = TEXT-014"'Delete'
                      quickinfo = TEXT-015"'Deletes selected lines from log table.'
                      disabled  = abap_false ) TO e_object->mt_toolbar.
    ELSEIF p_revers = abap_true.

      APPEND VALUE #( function  = 'REVERSE'
                       icon      = icon_detail
                       text      = TEXT-006"'Reverse'
                       quickinfo = TEXT-004"'Reverses posts according to selected lines.'
                       disabled  = abap_false ) TO e_object->mt_toolbar.

    ENDIF.

  ENDMETHOD.
  METHOD user_command_alv.

    CASE e_ucomm.
      WHEN 'RUN'.
        call_bapi_acc_document_post( ).
      WHEN 'DOWNLOAD'.
        download_excel_template( ).
      WHEN 'REVERSE'.
        bapi_acc_document_rev_post( ).
      WHEN 'SAVE'.
        save_to_log_table( ).
      WHEN 'DELETE'.
        delete_from_log_table( ).
    ENDCASE.
  ENDMETHOD.
  METHOD refresh_alv.
    io_grid->set_frontend_layout( VALUE #( zebra      = abap_true
                                           cwidth_opt = abap_true
                                           sel_mode   = 'A'
                                        ) ).

    io_grid->refresh_table_display(
          EXPORTING
            is_stable      = VALUE #( row = abap_true col = abap_true )
            i_soft_refresh = abap_true
          EXCEPTIONS
            finished       = 1
            OTHERS         = 2            ).
  ENDMETHOD.
  METHOD download_excel_template.
    DATA(lv_str_alv) = 'S101'.
    DATA(lt_fcat) = fill_main_fieldcat( iv_scrn = lv_str_alv iv_structure = gt_structure2 ).

    create_table(
    IMPORTING
      et_table_skeleton = mr_excel
      CHANGING
      it_fcat           = lt_fcat ).
    IF mr_excel IS BOUND.
      TRY.
          DATA(lo_excel) = znt_001_cl_001=>create_with_internal_table(
                             ir_data          = REF #( mr_excel->* )
                             it_field_catalog = fill_main_fieldcat( iv_scrn = lv_str_alv iv_structure = gt_structure2 ) ).
          lo_excel->download_xlsx( ).
        CATCH znt_001_cx_001 INTO DATA(lx_excel).
          MESSAGE s007 DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Selection_screen_output.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'ID1'.
          p_bukrs = 'AZ01'.
          screen-input = 0.
        WHEN 'ID3'.
          IF p_run = abap_false.
            screen-input = 0.
            screen-invisible = 1.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD fetch_file.
    TRY.
        DATA(lo_excel) = znt_001_cl_001=>upload_xlsx(
                           EXPORTING
                             iv_filename = CONV string( p_fname )
                           IMPORTING
                             ev_filepath = mv_filepath
                         ).
      CATCH znt_001_cx_001 INTO DATA(lx_excel).
        MESSAGE s007 DISPLAY LIKE 'E'.
    ENDTRY.
    p_fname = mv_filepath.
  ENDMETHOD.
  METHOD reach_maintenance_table.
    CALL TRANSACTION 'ZCO002'.
  ENDMETHOD.
  METHOD maintenance_excel_upload.
    CALL TRANSACTION 'ZCO003'.
  ENDMETHOD.
  METHOD save_to_log_table.
    DATA lt_log_table TYPE TABLE OF zco001_t_log.

    ms_alv-s0101-grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).

    IF lt_rows IS NOT INITIAL.
      LOOP AT lt_rows INTO DATA(ls_rows).
        READ TABLE mt_main REFERENCE INTO DATA(lr_main_alv) INDEX ls_rows-index.
        IF sy-subrc IS INITIAL.

          APPEND INITIAL LINE TO lt_log_table REFERENCE INTO DATA(lr_log).
          lr_log->tariff_id_1        = lr_main_alv->tariff1.
          lr_log->tariff_id_2        = lr_main_alv->tariff2.
          lr_log->pnl_group          = lr_main_alv->pnl_group.
          lr_log->description        = lr_main_alv->description.
          lr_log->cust_segment       = lr_main_alv->cust_segment.
          lr_log->invoice_type       = lr_main_alv->invoice_type.
          lr_log->related_report     = lr_main_alv->related_report.
          lr_log->related_table      = lr_main_alv->related_table.
          lr_log->related_sheet      = lr_main_alv->related_sheet.
          lr_log->tariff_type        = lr_main_alv->tariff_type.
          lr_log->amount             = lr_main_alv->amount.
          lr_log->amount_vat         = lr_main_alv->amount_vat.
          lr_log->ratio              = lr_main_alv->ratio.
          lr_log->distributed_amount = lr_main_alv->distributed_amount.
          lr_log->period_year        = lr_main_alv->period_year.
          lr_log->icon               = lr_main_alv->icon.
        ENDIF.
      ENDLOOP.

      MODIFY zco001_t_log FROM TABLE lt_log_table.
      IF sy-subrc IS INITIAL.
        MESSAGE s003.
*        LOOP AT lt_log_table INTO DATA(ls_log_table).
*          READ TABLE mt_main INTO DATA(ls_main)
*                WITH KEY tariff1   = ls_log_table-tariff_id_1
*                         tariff2   = ls_log_table-tariff_id_2
*                         pnl_group = ls_log_table-pnl_group
*         BINARY SEARCH.
*          IF sy-subrc = 0.
*            DELETE mt_main INDEX sy-tabix.
*          ENDIF.
*        ENDLOOP.
      ENDIF.
      CLEAR lt_rows.
*      refresh_alv( io_grid = ms_alv-s0101-grid ).
    ENDIF.

  ENDMETHOD.
  METHOD delete_from_log_table.
    DATA lt_log_table TYPE TABLE OF zco001_t_log.

    ms_alv-s0101-grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).

    IF lt_rows IS NOT INITIAL.
      LOOP AT lt_rows INTO DATA(ls_rows).
        READ TABLE mt_main REFERENCE INTO DATA(lr_main_alv) INDEX ls_rows-index.
        IF sy-subrc IS INITIAL.

          APPEND INITIAL LINE TO lt_log_table REFERENCE INTO DATA(lr_log).
          lr_log->tariff_id_1        = lr_main_alv->tariff1.
          lr_log->tariff_id_2        = lr_main_alv->tariff2.
          lr_log->pnl_group          = lr_main_alv->pnl_group.
          lr_log->description        = lr_main_alv->description.
          lr_log->cust_segment       = lr_main_alv->cust_segment.
          lr_log->invoice_type       = lr_main_alv->invoice_type.
          lr_log->related_report     = lr_main_alv->related_report.
          lr_log->related_table      = lr_main_alv->related_table.
          lr_log->related_sheet      = lr_main_alv->related_sheet.
          lr_log->tariff_type        = lr_main_alv->tariff_type.
          lr_log->amount             = lr_main_alv->amount.
          lr_log->amount_vat         = lr_main_alv->amount_vat.
          lr_log->ratio              = lr_main_alv->ratio.
          lr_log->distributed_amount = lr_main_alv->distributed_amount.
          lr_log->period_year        = lr_main_alv->period_year.
        ENDIF.
      ENDLOOP.

      DELETE zco001_t_log FROM TABLE lt_log_table.
      IF sy-subrc IS INITIAL.
        MESSAGE s004.
        LOOP AT lt_log_table INTO DATA(ls_log_table).
          READ TABLE mt_main INTO DATA(ls_main)
                WITH KEY tariff1     = ls_log_table-tariff_id_1
                         tariff2     = ls_log_table-tariff_id_2
                         pnl_group   = ls_log_table-pnl_group
                         description = ls_log_table-description
         BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE mt_main INDEX sy-tabix.
          ENDIF.
        ENDLOOP.
      ENDIF.
      CLEAR lt_rows.
      refresh_alv( io_grid = ms_alv-s0101-grid ).
    ENDIF.

  ENDMETHOD.
  METHOD call_bapi_acc_document_post.
    DATA lt_log TYPE TABLE OF zco001_t_log.

    DATA lv_total_amount_for_debit TYPE p LENGTH 15 DECIMALS 3.

    DATA lv_day_out            TYPE          datum.
    DATA lv_day_in             TYPE          datum.

    DATA lt_ungrouped_bapi_alv TYPE TABLE OF zco001_s_alv_screen.
    DATA lt_bapi_alv           TYPE TABLE OF zco001_s_alv_screen.

    DATA ls_documentheader     TYPE          bapiache09.

    DATA lt_account_gl         TYPE TABLE OF bapiacgl09.
    DATA lt_currency_amount    TYPE TABLE OF bapiaccr09.
    DATA lt_criteria           TYPE TABLE OF bapiackec9.
    DATA lt_extension1         TYPE TABLE OF bapiacextc.
    DATA lt_extension2         TYPE TABLE OF bapiparex.
    DATA lt_return             TYPE TABLE OF bapiret2.

    DATA   ls_obj_type         TYPE          bapiache09-obj_type.
    DATA   ls_obj_key          TYPE          bapiache09-obj_key.
    DATA   ls_obj_sys          TYPE          bapiache09-obj_sys.

    ms_alv-s0101-grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).

    IF lt_rows IS NOT INITIAL.
      lt_ungrouped_bapi_alv = VALUE #(  FOR ls_row IN lt_rows
                              LET lr_main = REF #( mt_main[ ls_row-index ] )
                              IN ( CORRESPONDING #( lr_main->* ) ) ).

      LOOP AT lt_ungrouped_bapi_alv REFERENCE INTO DATA(lr_ungrouped_bapi_alv)
      GROUP BY ( pnl_group     = lr_ungrouped_bapi_alv->pnl_group
                 cust_segment  = lr_ungrouped_bapi_alv->cust_segment
                 invoice_type  = lr_ungrouped_bapi_alv->invoice_type
                 tariff_type   = lr_ungrouped_bapi_alv->tariff_type )
                 ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_group>).
        APPEND INITIAL LINE TO lt_bapi_alv REFERENCE INTO DATA(lr_bapi_alv).
        LOOP AT GROUP <lfs_group> REFERENCE INTO DATA(lr_main_group).
          lr_bapi_alv->amount             += lr_main_group->amount.
          lr_bapi_alv->amount_vat         += lr_main_group->amount_vat.
          lr_bapi_alv->distributed_amount += lr_main_group->distributed_amount.
          lr_bapi_alv->cust_segment        = lr_main_group->cust_segment.
          lr_bapi_alv->description         = lr_main_group->description.
          lr_bapi_alv->document_date       = lr_main_group->document_date.
          lr_bapi_alv->document_no         = lr_main_group->document_no.
          IF lr_main_group->icon = '@08@'.  "Green
            MESSAGE s017 WITH lr_main_group->tariff1 lr_main_group->tariff2 lr_main_group->pnl_group lr_main_group->description DISPLAY LIKE 'E'.
            RETURN.
          ELSE.
            lr_bapi_alv->icon                = lr_main_group->icon.
          ENDIF.
          lr_bapi_alv->invoice_type        = lr_main_group->invoice_type.
          lr_bapi_alv->period_year         = lr_main_group->period_year.
          lr_bapi_alv->pnl_group           = lr_main_group->pnl_group.
          lr_bapi_alv->ratio               = lr_main_group->ratio.
          lr_bapi_alv->tariff1             = lr_main_group->tariff1.
          lr_bapi_alv->tariff2             = lr_main_group->tariff2.
          lr_bapi_alv->related_report      = lr_main_group->related_report.
          lr_bapi_alv->related_sheet       = lr_main_group->related_sheet.
          lr_bapi_alv->related_table       = lr_main_group->related_table.
          lr_bapi_alv->tariff_type         = lr_main_group->tariff_type.
        ENDLOOP.
      ENDLOOP.

      "BAPI filling begins
      lv_day_in = |{ s_time+0(4) }{ s_time+5(2) }01|.
      CALL FUNCTION 'HR_HR_LAST_DAY_OF_MONTH'
        EXPORTING
          day_in            = lv_day_in
        IMPORTING
          last_day_of_month = lv_day_out
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        MESSAGE s016 DISPLAY LIKE 'E'.
      ENDIF.

      ls_documentheader-bus_act      = 'RFBU'.
      ls_documentheader-header_txt   = |IFRS PA { lv_day_out }|.
      ls_documentheader-doc_type     = 'SA'.
      ls_documentheader-username     = sy-uname.
      ls_documentheader-comp_code    = p_bukrs.
      ls_documentheader-doc_date     = lv_day_out.
      ls_documentheader-pstng_date   = lv_day_out.
      ls_documentheader-fisc_year    = |{ s_time+0(4) }|.
      ls_documentheader-fis_period   = |{ s_time+5(2) }|.
      ls_documentheader-ledger_group = 'A0'.

      DATA(lv_counter_criteria) = 1.
      DATA(lv_counter) = 1.
      LOOP AT lt_bapi_alv REFERENCE INTO DATA(lr_bapi).
        lv_total_amount_for_debit += lr_bapi->distributed_amount.
        DO 4 TIMES.
          APPEND INITIAL LINE TO lt_criteria REFERENCE INTO DATA(lr_criteria).
          lr_criteria->itemno_acc = lv_counter.
          lr_criteria->fieldname  = |WW00{ lv_counter_criteria }|.
          CASE lr_criteria->fieldname.
            WHEN 'WW001'.
              IF lr_bapi->pnl_group       = TEXT-016. "'Short number services'.
                lr_criteria->character    = TEXT-017. "'SHORTNUMBERSERVICE'.
              ELSE.
                lr_criteria->character    = to_upper( lr_bapi->pnl_group ).
              ENDIF.
            WHEN 'WW002'.
              IF lr_bapi->tariff_type     = TEXT-018.  "'Foreign Visitor Roaming'.
                lr_bapi->tariff_type      = TEXT-019.  "'FOREIGN ROAMING'.
              ELSEIF lr_bapi->tariff_type = TEXT-020.  "'Interconnnection revenue'.
                lr_bapi->tariff_type      = TEXT-021.  "'INTCON REVENUE'.
              ELSEIF lr_bapi->tariff_type = TEXT-016.  "'Short number services'.
                lr_criteria->character    = TEXT-017.  "'SHORTNUMBERSERVICE'.
              ELSE.
                lr_criteria->character      = to_upper( lr_bapi->tariff_type ).
              ENDIF.
            WHEN 'WW003'.
              IF lr_bapi->invoice_type    = TEXT-016.  "'Short number services'.
                lr_criteria->character    = TEXT-017.  "'SHORTNUMBERSERVICE'.
              ELSE.
                lr_criteria->character      = to_upper( lr_bapi->invoice_type ).
              ENDIF.
            WHEN 'WW004'.
              IF lr_bapi->cust_segment    = TEXT-016.  "'Short number services'.
                lr_criteria->character    = TEXT-017.  "'SHORTNUMBERSERVICE'.
              ELSE.
                lr_criteria->character      = to_upper( lr_bapi->cust_segment ).
              ENDIF.
          ENDCASE.
          lv_counter_criteria += 1.
        ENDDO.
        lv_counter_criteria = 1.
        APPEND INITIAL LINE TO lt_account_gl REFERENCE INTO DATA(lr_account_gl).
        lr_account_gl->itemno_acc = lv_counter.
        lr_account_gl->gl_account = '6010100110'.
        lr_account_gl->item_text  = 'COPA Correction'.
        APPEND INITIAL LINE TO lt_currency_amount REFERENCE INTO DATA(lr_currency_amount).
        lr_currency_amount->itemno_acc   = lv_counter.
        lr_currency_amount->currency     = 'AZN'.
        lr_currency_amount->currency_iso = 'AZN'.
        lr_currency_amount->amt_doccur   = lr_bapi->distributed_amount.
        lv_counter += 1.
      ENDLOOP.

      APPEND INITIAL LINE TO lt_currency_amount REFERENCE INTO DATA(lr_currency_amount_last).
      lr_currency_amount_last->itemno_acc   = lv_counter.
      lr_currency_amount_last->currency     = 'AZN'.
      lr_currency_amount_last->currency_iso = 'AZN'.
      lr_currency_amount_last->amt_doccur   = lv_total_amount_for_debit * ( -1 ).
      lr_currency_amount_last->amt_base     = lv_total_amount_for_debit * ( -1 ).
      APPEND INITIAL LINE TO lt_account_gl REFERENCE INTO DATA(lr_account_gl_last).
      lr_account_gl_last->itemno_acc = lv_counter.
      lr_account_gl_last->gl_account = '6010100110'.
      lr_account_gl_last->item_text  = 'COPA Correction'.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_documentheader     " Header
*         customercpd    =                       " One-time customer
*         contractheader =                       " Additional Contract Accounts Recievable and Payable Header Line
        IMPORTING                                "
          obj_type       = ls_obj_type           " Reference procedure
          obj_key        = ls_obj_key            " Reference key
          obj_sys        = ls_obj_sys            " Logical system of source document
        TABLES                                   "
          accountgl      = lt_account_gl         " G/L account item
*         accountreceivable =                    " Customer Item
*         accountpayable =                       " Vendor Item
*         accounttax     =                       " Tax item
          currencyamount = lt_currency_amount    " Currency Items
          criteria       = lt_criteria           " CO-PA Account Assignment Characteristics
*         valuefield     =                       " CO-PA Account Assignment Value Fields
          extension1     = lt_extension1         " Container for 'Customer Exit' Parameter
          return         = lt_return             " Return parameter
*         paymentcard    =                       " Payment Card Information
*         contractitem   =                       " Additional Contract Accounts Recieviable and Payable Document Line Item
          extension2     = lt_extension2.        " Reference Structure for BAPI Parameters EXTENSIONIN/EXTENSIONOUT
*         realestate     =                       " Real Estate Account Assignment Data
*         accountwt      =                       " Withholding tax information for FI Interface
      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
          EXPORTING
            it_message = lt_return.                " BAPI Return Table
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        SORT lt_ungrouped_bapi_alv BY tariff1 tariff2 description invoice_type pnl_group. "for binary search
        SORT mt_main by tariff1 tariff2 description invoice_type pnl_group. "for binary search
        LOOP AT lt_ungrouped_bapi_alv REFERENCE INTO DATA(lr_ungrouped_alv).
*        LOOP AT mt_main REFERENCE INTO DATA(lr_main_last) WHERE document_no IS INITIAL ."OR ( TARIFF_ID_1 =   ).
          READ TABLE mt_main REFERENCE INTO DATA(lr_main_last) WITH KEY tariff1 = lr_ungrouped_alv->tariff1
                                                                        tariff2 = lr_ungrouped_alv->tariff2
                                                                        description = lr_ungrouped_alv->description
                                                                        invoice_type = lr_ungrouped_alv->invoice_type
                                                                        pnl_group = lr_ungrouped_alv->pnl_group
                                                                         BINARY SEARCH.
          lr_main_last->document_date    = lv_day_out.
          lr_main_last->document_no      = ls_obj_key+0(10).
          lr_main_last->re_document_no   = ''.
          lr_main_last->icon             = '@08@'.
          APPEND INITIAL LINE TO lt_log REFERENCE INTO DATA(lr_log).
          lr_log->tariff_id_1            = lr_main_last->tariff1               .
          lr_log->tariff_id_2            = lr_main_last->tariff2               .
          lr_log->pnl_group              = lr_main_last->pnl_group             .
          lr_log->description            = lr_main_last->description           .
          lr_log->cust_segment           = lr_main_last->cust_segment          .
          lr_log->invoice_type           = lr_main_last->invoice_type          .
          lr_log->related_report         = lr_main_last->related_report        .
          lr_log->related_table          = lr_main_last->related_table         .
          lr_log->related_sheet          = lr_main_last->related_sheet         .
          lr_log->tariff_type            = lr_main_last->tariff_type           .
          lr_log->amount                 = lr_main_last->amount                .
          lr_log->amount_vat             = lr_main_last->amount_vat            .
          lr_log->ratio                  = lr_main_last->ratio                 .
          lr_log->distributed_amount     = lr_main_last->distributed_amount    .
          lr_log->doc_no                 = lr_main_last->document_no           .
          lr_log->posting_date           = lr_main_last->document_date         .
          lr_log->period_year            = lr_main_last->period_year           .
          lr_log->icon                   = lr_main_last->icon                  .
        ENDLOOP.
*        lt_log = CORRESPONDING #( mt_main ).
        MODIFY zco001_t_log FROM TABLE lt_log.
        IF sy-subrc IS INITIAL.
          refresh_alv( io_grid = ms_alv-s0101-grid ).
          MESSAGE s018.
        ENDIF.

      ENDIF.

      CLEAR lt_rows.

    ENDIF.
    "  @08@      Green
    "  @09@      Yellow
    "  @0A@      Red
    "  @S_TL_Y@  Yellow Old
  ENDMETHOD.
  METHOD bapi_acc_document_rev_post.


    DATA lt_log                    TYPE TABLE OF zco001_t_log.

    DATA lv_total_amount_for_debit TYPE p LENGTH 15 DECIMALS 3.

    DATA lv_day_out                TYPE          datum.
    DATA lv_day_in                 TYPE          datum.

    DATA lt_ungrouped_bapi_alv     TYPE TABLE OF zco001_s_alv_screen.
    DATA lt_bapi_alv               TYPE TABLE OF zco001_s_alv_screen.

    DATA ls_reversal               TYPE          bapiacrev.
    DATA lv_bus_act                TYPE          bapiache09-bus_act.
    DATA lt_return                 TYPE TABLE OF bapiret2.
    DATA lt_return_all             TYPE TABLE OF bapiret2.

    DATA   ls_obj_type             TYPE          bapiacrev-obj_type.
    DATA   ls_obj_key              TYPE          bapiacrev-obj_key.
    DATA   ls_obj_sys              TYPE          bapiacrev-obj_sys.

    ms_alv-s0101-grid->get_selected_rows( IMPORTING et_index_rows = DATA(lt_rows) ).

    IF lt_rows IS NOT INITIAL.
      lt_ungrouped_bapi_alv = VALUE #(  FOR ls_row IN lt_rows
                              LET lr_main = REF #( mt_main[ ls_row-index ] )
                              IN ( CORRESPONDING #( lr_main->* ) ) ).

      LOOP AT lt_ungrouped_bapi_alv REFERENCE INTO DATA(lr_ungrouped_bapi_alv)
      GROUP BY ( document_no    = lr_ungrouped_bapi_alv->document_no )
                 ASCENDING ASSIGNING FIELD-SYMBOL(<lfs_group>).
        APPEND INITIAL LINE TO lt_bapi_alv REFERENCE INTO DATA(lr_bapi_alv).
        LOOP AT GROUP <lfs_group> REFERENCE INTO DATA(lr_main_group).
          lr_bapi_alv->amount             += lr_main_group->amount.
          lr_bapi_alv->amount_vat         += lr_main_group->amount_vat.
          lr_bapi_alv->distributed_amount += lr_main_group->distributed_amount.
          lr_bapi_alv->cust_segment        = lr_main_group->cust_segment.
          lr_bapi_alv->description         = lr_main_group->description.
          lr_bapi_alv->document_date       = lr_main_group->document_date.
          lr_bapi_alv->document_no         = lr_main_group->document_no.
          lr_bapi_alv->re_document_no      = lr_main_group->re_document_no.
          IF lr_main_group->icon     = '@0A@'.  "Red
            MESSAGE s019 WITH lr_main_group->tariff1 lr_main_group->tariff2 lr_main_group->pnl_group lr_main_group->description DISPLAY LIKE 'E'.
            RETURN.
          ELSEIF lr_main_group->icon = '@09@'.  "Yellow
            MESSAGE s020 WITH lr_main_group->tariff1 lr_main_group->tariff2 lr_main_group->pnl_group lr_main_group->description DISPLAY LIKE 'E'.
            RETURN.
          ELSEIF lr_main_group->icon = '@08@'.  "Green
            lr_bapi_alv->icon                = lr_main_group->icon.
          ENDIF.
          lr_bapi_alv->invoice_type        = lr_main_group->invoice_type.
          lr_bapi_alv->period_year         = lr_main_group->period_year.
          lr_bapi_alv->pnl_group           = lr_main_group->pnl_group.
          lr_bapi_alv->ratio               = lr_main_group->ratio.
          lr_bapi_alv->tariff1             = lr_main_group->tariff1.
          lr_bapi_alv->tariff2             = lr_main_group->tariff2.
          lr_bapi_alv->related_report      = lr_main_group->related_report.
          lr_bapi_alv->related_sheet       = lr_main_group->related_sheet.
          lr_bapi_alv->related_table       = lr_main_group->related_table.
          lr_bapi_alv->tariff_type         = lr_main_group->tariff_type.
        ENDLOOP.
      ENDLOOP.

      "BAPI filling begins
      lv_day_in = |{ s_time+0(4) }{ s_time+5(2) }01|.
      CALL FUNCTION 'HR_HR_LAST_DAY_OF_MONTH'
        EXPORTING
          day_in            = lv_day_in
        IMPORTING
          last_day_of_month = lv_day_out
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        MESSAGE s016 DISPLAY LIKE 'E'.
      ENDIF.

      lv_bus_act               = 'RFBU'.
      ls_reversal-obj_type     = 'BKPFF'.
      ls_reversal-username     = sy-uname.
      ls_reversal-comp_code    = p_bukrs.
      ls_reversal-reason_rev   = '01'.
      ls_reversal-pstng_date   = lv_day_out.
      ls_reversal-fis_period   = |{ s_time+5(2) }|.

      LOOP AT lt_bapi_alv ASSIGNING FIELD-SYMBOL(<lfs_bapi_alv>).
        ls_reversal-obj_key_r  = |{ <lfs_bapi_alv>-document_no }{ p_bukrs }{ s_time+0(4) }|.

        CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
          EXPORTING
            reversal = ls_reversal    " Reference information for reversal in Accounting
            bus_act  = lv_bus_act     " Business Transaction
          IMPORTING
            obj_type = ls_obj_type    " Reference procedure
            obj_key  = ls_obj_key     " Reference Key
            obj_sys  = ls_obj_sys     " Logical system of source document
          TABLES
            return   = lt_return.     " Return Parameter
        LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          APPEND LINES OF lt_return TO lt_return_all.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          SORT lt_ungrouped_bapi_alv BY tariff1 tariff2 description invoice_type pnl_group. "for binary search
          SORT mt_main by tariff1 tariff2 description invoice_type pnl_group. "for binary search
          LOOP AT lt_ungrouped_bapi_alv REFERENCE INTO DATA(lr_ungrouped_alv).
*        LOOP AT mt_main REFERENCE INTO DATA(lr_main_last) WHERE document_no IS INITIAL ."OR ( TARIFF_ID_1 =   ).
            READ TABLE mt_main REFERENCE INTO DATA(lr_main_last) WITH KEY tariff1 = lr_ungrouped_alv->tariff1
                                                                          tariff2 = lr_ungrouped_alv->tariff2
                                                                          description = lr_ungrouped_alv->description
                                                                          invoice_type = lr_ungrouped_alv->invoice_type
                                                                          pnl_group = lr_ungrouped_alv->pnl_group
                                                                          BINARY SEARCH.
            lr_main_last->document_date  = lv_day_out.
            lr_main_last->re_document_no = ls_obj_key+0(10).
            lr_main_last->icon           = '@0A@'. "Red
            APPEND INITIAL LINE TO lt_log REFERENCE INTO DATA(lr_log).
            lr_log->tariff_id_1            = lr_main_last->tariff1               .
            lr_log->tariff_id_2            = lr_main_last->tariff2               .
            lr_log->pnl_group              = lr_main_last->pnl_group             .
            lr_log->description            = lr_main_last->description           .
            lr_log->cust_segment           = lr_main_last->cust_segment          .
            lr_log->invoice_type           = lr_main_last->invoice_type          .
            lr_log->related_report         = lr_main_last->related_report        .
            lr_log->related_table          = lr_main_last->related_table         .
            lr_log->related_sheet          = lr_main_last->related_sheet         .
            lr_log->tariff_type            = lr_main_last->tariff_type           .
            lr_log->amount                 = lr_main_last->amount                .
            lr_log->amount_vat             = lr_main_last->amount_vat            .
            lr_log->ratio                  = lr_main_last->ratio                 .
            lr_log->distributed_amount     = lr_main_last->distributed_amount    .
            lr_log->doc_no                 = lr_main_last->document_no           .
            lr_log->re_doc_no              = lr_main_last->re_document_no        .
            lr_log->posting_date           = lr_main_last->document_date         .
            lr_log->period_year            = lr_main_last->period_year           .
            lr_log->icon                   = lr_main_last->icon                  .
          ENDLOOP.

        ENDIF.
      ENDLOOP.

      IF lt_log IS NOT INITIAL.
        MODIFY zco001_t_log FROM TABLE lt_log.
        IF sy-subrc IS INITIAL.
          refresh_alv( io_grid = ms_alv-s0101-grid ).
          MESSAGE s018.
        ENDIF.
      ENDIF.

      IF lt_return_all IS NOT INITIAL.
        CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
          EXPORTING
            it_message = lt_return_all.                " BAPI Return Table
      ENDIF.

      CLEAR lt_rows.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
