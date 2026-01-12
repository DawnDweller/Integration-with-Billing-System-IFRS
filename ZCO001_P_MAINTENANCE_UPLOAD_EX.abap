*&---------------------------------------------------------------------*
*& Report ZCO001_P_MAINTENANCE_UPLOAD_EX
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCO001_P_MAINTENANCE_UPLOAD_EX MESSAGE-ID ZCO001.

INCLUDE ZCO001_I_MAINTEN_UPLOAD_EX_TOP.
INCLUDE ZCO001_I_MAINTEN_UPLOAD_EX_SS.
INCLUDE ZCO001_I_MAINTEN_UPLOAD_EX_CLS.

INITIALIZATION.
 pb_maint = TEXT-010.
 pb_down  = TEXT-011.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
lcl_main_controller=>fetch_file( ).

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'MAINT'.
      lcl_main_controller=>reach_maintenance_table( ).
    WHEN 'DOWN'.
      lcl_main_controller=>download_maintenance_template( ).
  ENDCASE.

START-OF-SELECTION.
lcl_main_controller=>initial_control( ).
