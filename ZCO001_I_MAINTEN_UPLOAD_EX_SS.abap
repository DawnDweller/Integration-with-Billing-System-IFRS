*&---------------------------------------------------------------------*
*& Include          ZCO001_I_MAINTEN_UPLOAD_EX_SS
*&---------------------------------------------------------------------*
TABLES: acdoca, bkpf, mldoc.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-005.
  PARAMETERS: p_fname  LIKE rlgrap-filename MODIF ID id3.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-009.
  SELECTION-SCREEN PUSHBUTTON 1(10) pb_maint USER-COMMAND maint.
   SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN PUSHBUTTON 1(18) pb_down USER-COMMAND down.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
