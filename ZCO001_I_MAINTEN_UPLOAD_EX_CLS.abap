*&---------------------------------------------------------------------*
*& Include          ZCO001_I_MAINTEN_UPLOAD_EX_CLS
*&---------------------------------------------------------------------*
CLASS lcl_main_controller DEFINITION CREATE PRIVATE FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
      mt_main     TYPE TABLE OF zco001_s_alv_screen,
      mt_excel    TYPE TABLE OF zco001_s_excel_maintenance,
      mr_excel    TYPE REF TO data,
      mv_filepath TYPE string.

    CLASS-METHODS:
      ext IMPORTING VALUE(iv_scrn) TYPE sy-dynnr,

      fill_main_fieldcat IMPORTING VALUE(iv_structure) TYPE string
                                   iv_scrn             TYPE char4
                         RETURNING VALUE(rt_fcat)      TYPE lvc_t_fcat,
      excel_upload,
      initial_control,
      fetch_file,
      reach_maintenance_table,
      download_maintenance_template,
      create_table
        IMPORTING iv_stname         TYPE dd02l-tabname OPTIONAL
        EXPORTING et_table_skeleton TYPE REF TO data
        CHANGING  it_fcat           TYPE lvc_t_fcat.
  PRIVATE SECTION.

ENDCLASS.
CLASS lcl_main_controller IMPLEMENTATION.
  METHOD initial_control.

    IF p_fname IS NOT INITIAL.
      excel_upload( ).
    ELSEIF p_fname IS INITIAL.
      MESSAGE s000 DISPLAY LIKE 'E'.
      LEAVE TO CURRENT TRANSACTION.
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
    DATA lt_maintenance TYPE TABLE OF zco001_t_mainten.
    DATA lv_ratio_text TYPE char4.
    DATA lv_percentage_control TYPE p LENGTH 3 DECIMALS 2.

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


      LOOP AT mt_excel ASSIGNING FIELD-SYMBOL(<lfs_excel>).
        DATA(lv_excel_tabix) = sy-tabix.
        APPEND INITIAL LINE TO lt_maintenance REFERENCE INTO DATA(lr_maintenance).


        lv_percentage_control = <lfs_excel>-zonnet          +
                                <lfs_excel>-zoffnet         +
                                <lfs_excel>-international   +
                                <lfs_excel>-local_data      +
                                <lfs_excel>-sms             +
                                <lfs_excel>-short_number    +
                                <lfs_excel>-simcard         +
                                <lfs_excel>-roam_voice      +
                                <lfs_excel>-roam_data       +
                                <lfs_excel>-roam_sms        +
                                <lfs_excel>-m2m             +
                                <lfs_excel>-transmission    +
                                <lfs_excel>-other           +
                                <lfs_excel>-interconnection +
                                <lfs_excel>-interconnection +
                                <lfs_excel>-handset_revenue.

        IF <lfs_excel>-date_start IS INITIAL OR <lfs_excel>-date_end IS INITIAL.
          MESSAGE s009 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.

        IF lv_percentage_control <> 1 AND lv_percentage_control <> -1.
*          BREAK-POINT.
          MESSAGE s008 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.

        <lfs_excel>-zonnet              = <lfs_excel>-zonnet            * 100.
        <lfs_excel>-zoffnet             = <lfs_excel>-zoffnet           * 100.
        <lfs_excel>-international       = <lfs_excel>-international     * 100.
        <lfs_excel>-local_data          = <lfs_excel>-local_data        * 100.
        <lfs_excel>-sms                 = <lfs_excel>-sms               * 100.
        <lfs_excel>-short_number        = <lfs_excel>-short_number      * 100.
        <lfs_excel>-simcard             = <lfs_excel>-simcard           * 100.
        <lfs_excel>-roam_voice          = <lfs_excel>-roam_voice        * 100.
        <lfs_excel>-roam_data           = <lfs_excel>-roam_data         * 100.
        <lfs_excel>-roam_sms            = <lfs_excel>-roam_sms          * 100.
        <lfs_excel>-m2m                 = <lfs_excel>-m2m               * 100.
        <lfs_excel>-transmission        = <lfs_excel>-transmission      * 100.
        <lfs_excel>-other               = <lfs_excel>-other             * 100.
        <lfs_excel>-interconnection     = <lfs_excel>-interconnection   * 100.
        <lfs_excel>-foreign_visitor     = <lfs_excel>-interconnection   * 100.
        <lfs_excel>-handset_revenue     = <lfs_excel>-handset_revenue   * 100.

        lr_maintenance->mandt           = sy-mandt.
        IF <lfs_excel>-tariff_id_1 IS NOT INITIAL.
          lr_maintenance->tariff1         = <lfs_excel>-tariff_id_1.
        ELSE.
          MESSAGE s011 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        IF <lfs_excel>-tariff_id_2 IS NOT INITIAL.
          lr_maintenance->traiff2         = <lfs_excel>-tariff_id_2.
        ELSE.
          MESSAGE s012 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        IF <lfs_excel>-description IS NOT INITIAL.
          lr_maintenance->description     = <lfs_excel>-description.
        ELSE.
          MESSAGE s013 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        IF <lfs_excel>-invoice_type IS NOT INITIAL.
          lr_maintenance->invoice_type    = <lfs_excel>-invoice_type.
        ELSE.
          MESSAGE s014 WITH lv_excel_tabix DISPLAY LIKE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        lr_maintenance->zonnet          = |{ <lfs_excel>-zonnet          }%|.
        lr_maintenance->zoffnet         = |{ <lfs_excel>-zoffnet         }%|.
        lr_maintenance->international   = |{ <lfs_excel>-international   }%|.
        lr_maintenance->local_data      = |{ <lfs_excel>-local_data      }%|.
        lr_maintenance->sms             = |{ <lfs_excel>-sms             }%|.
        lr_maintenance->short_number    = |{ <lfs_excel>-short_number    }%|.
        lr_maintenance->simcard         = |{ <lfs_excel>-simcard         }%|.
        lr_maintenance->roam_voice      = |{ <lfs_excel>-roam_voice      }%|.
        lr_maintenance->roam_data       = |{ <lfs_excel>-roam_data       }%|.
        lr_maintenance->roam_sms        = |{ <lfs_excel>-roam_sms        }%|.
        lr_maintenance->m2m             = |{ <lfs_excel>-m2m             }%|.
        lr_maintenance->transmission    = |{ <lfs_excel>-transmission    }%|.
        lr_maintenance->other           = |{ <lfs_excel>-other           }%|.
        lr_maintenance->interconnection = |{ <lfs_excel>-interconnection }%|.
        lr_maintenance->foreign_visitor = |{ <lfs_excel>-foreign_visitor }%|.
        lr_maintenance->handset_revenue = |{ <lfs_excel>-handset_revenue }%|.
        lr_maintenance->date_start      =    <lfs_excel>-date_start.
        lr_maintenance->date_end        =    <lfs_excel>-date_end.

        CONDENSE lr_maintenance->zonnet            .
        CONDENSE lr_maintenance->zoffnet           .
        CONDENSE lr_maintenance->international     .
        CONDENSE lr_maintenance->local_data        .
        CONDENSE lr_maintenance->sms               .
        CONDENSE lr_maintenance->short_number      .
        CONDENSE lr_maintenance->simcard           .
        CONDENSE lr_maintenance->roam_voice        .
        CONDENSE lr_maintenance->roam_data         .
        CONDENSE lr_maintenance->roam_sms          .
        CONDENSE lr_maintenance->m2m               .
        CONDENSE lr_maintenance->transmission      .
        CONDENSE lr_maintenance->other             .
        CONDENSE lr_maintenance->interconnection   .
        CONDENSE lr_maintenance->foreign_visitor   .
        CONDENSE lr_maintenance->handset_revenue   .

      ENDLOOP.

      MODIFY zco001_t_mainten FROM TABLE lt_maintenance.
      IF sy-subrc IS INITIAL.
        MESSAGE s005.
      ELSE.
        MESSAGE s006 DISPLAY LIKE 'E'.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD fill_main_fieldcat.
    DATA: lv_fname     TYPE lvc_fname,
          lv_offset    TYPE i,
          lv_structure TYPE dd02l-tabname.

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
        WHEN 'WAERS'.
          lr_fcat->no_out = abap_true.
      ENDCASE.

    ENDLOOP.
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
  METHOD fetch_file.
    TRY.
        DATA(lo_excel) = znt_001_cl_001=>upload_xlsx(
                           EXPORTING
                             iv_filename = CONV string( p_fname )
                           IMPORTING
                             ev_filepath = mv_filepath
                         ).
      CATCH znt_001_cx_001 INTO DATA(lx_excel).
        MESSAGE lx_excel->get_text( ) TYPE icon_warning.
    ENDTRY.
    p_fname = mv_filepath.
  ENDMETHOD.
  METHOD reach_maintenance_table.
    CALL TRANSACTION 'ZCO002'.
  ENDMETHOD.
  METHOD download_maintenance_template.
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
          MESSAGE lx_excel->get_text( ) TYPE icon_warning.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
