```ABAP
*&---------------------------------------------------------------------*
*& Report ZCO001_P_BILL_SYSTEM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCO001_P_BILL_SYSTEM  MESSAGE-ID ZCO001.

INCLUDE ZCO001_I_BILL_SYSTEM_TOP.
INCLUDE ZCO001_I_BILL_SYSTEM_ss.
INCLUDE ZCO001_I_BILL_SYSTEM_cls.
INCLUDE ZCO001_I_BILL_SYSTEM_mdl.

INITIALIZATION.
data(lv_datum) = |{ sy-datum+0(4) }0{ sy-datum+4(2) }|.
s_time = lv_datum.

 pb_maint = TEXT-010.
 pb_ex_up = TEXT-011.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
lcl_main_controller=>fetch_file( ).

AT SELECTION-SCREEN OUTPUT.
lcl_main_controller=>selection_Screen_output( ).

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'MAINT'.
      lcl_main_controller=>reach_maintenance_table( ).
    WHEN 'EX_UP'.
      lcl_main_controller=>maintenance_excel_upload( ).
  ENDCASE.

START-OF-SELECTION.
lcl_main_controller=>initial_control( ).
lcl_main_controller=>get_data( ).

END-OF-SELECTION.
lcl_main_controller=>end_of_selection( ).
```
