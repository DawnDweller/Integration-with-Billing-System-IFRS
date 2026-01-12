*&---------------------------------------------------------------------*
*& Include          ZCO001_I_BILL_SYSTEM_MDL
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
 lcl_main_controller=>pbo( iv_scrn = sy-dynnr ).
ENDMODULE.

MODULE pai INPUT.
 lcl_main_controller=>pai( iv_scrn = sy-dynnr ).
ENDMODULE.
MODULE ext INPUT.
 lcl_main_controller=>ext( iv_scrn = sy-dynnr ).
ENDMODULE.
