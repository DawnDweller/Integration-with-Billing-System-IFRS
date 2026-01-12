
"How I used in the program --> DATA(lo_excel) = znt_001_cl_001=>create_with_internal_table( 

METHOD create_with_internal_table.

    IF abap_true NE check_reference_is_table( ir_data = ir_data ).
      "Data reference is not a table!
      RAISE EXCEPTION TYPE znt_001_cx_001
                MESSAGE ID znt_001_cx_001=>mc_message_class
                      TYPE znt_001_cx_001=>mc_message_types-error
                    NUMBER znt_001_cx_001=>mc_messages-data_ref_is_a_not_table.
    ENDIF.

    ro_myself          = NEW #( ).
    ro_myself->mr_data = ir_data.

    TRY .

        cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
          EXPORTING
            xml_type      = if_salv_bs_xml=>c_type_xlsx
            xml_version   = if_salv_bs_xml=>version_26
            xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
            gui_type      = if_salv_bs_xml=>c_gui_type_gui
            r_result_data = cl_salv_ex_util=>factory_result_data_table( r_data         = ir_data
                                                                        t_fieldcatalog = it_field_catalog
                                                                        s_layout       = is_layout
                                                                        t_sort         = it_sort
                                                                        t_filter       = it_filter
                                                                        t_hyperlinks   = it_hyperlinks )
          IMPORTING
            xml           = ro_myself->mv_xml ).

      CATCH cx_root INTO DATA(lx_root).
        "& & & &
        RAISE EXCEPTION TYPE znt_001_cx_001
                  MESSAGE ID znt_001_cx_001=>mc_message_class
                        TYPE znt_001_cx_001=>mc_message_types-error
                      NUMBER znt_001_cx_001=>mc_messages-generic
                        WITH lx_root->get_text( ).
    ENDTRY.

  ENDMETHOD. 
