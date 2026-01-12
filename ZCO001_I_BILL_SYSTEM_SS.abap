*&---------------------------------------------------------------------*
*& Include          ZCO001_I_BILL_SYSTEM_SS
*&---------------------------------------------------------------------*
TABLES: acdoca, bkpf, mldoc.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-005.
  PARAMETERS: p_bukrs TYPE bukrs MODIF ID id1,
              s_time  TYPE jahrper MODIF ID id2 OBLIGATORY, "To assign a search help MATCHCODE OBJECT ZABAP001_SH_DATE,
              p_fname LIKE rlgrap-filename MODIF ID id3.
  PARAMETERS: p_run    TYPE char1 RADIOBUTTON GROUP gr1 USER-COMMAND o DEFAULT 'X',
              p_displa TYPE char1 RADIOBUTTON GROUP gr1,
              p_revers TYPE char1 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-009.
  SELECTION-SCREEN PUSHBUTTON 1(10) pb_maint USER-COMMAND maint.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(20) pb_ex_up USER-COMMAND ex_up.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
