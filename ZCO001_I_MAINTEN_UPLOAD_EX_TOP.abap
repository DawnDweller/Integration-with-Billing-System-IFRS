*&---------------------------------------------------------------------*
*& Include          ZCO001_I_MAINTEN_UPLOAD_EX_TOP
*&---------------------------------------------------------------------*
CONSTANTS:
           gt_structure2 type string value 'ZCO001_S_EXCEL_MAINTENANCE',

           BEGIN OF ms_ucomm,
              back   TYPE sy-ucomm VALUE 'EX001',
              leave  TYPE sy-ucomm VALUE 'EX002',
              exit   TYPE sy-ucomm VALUE 'EX003',
           END OF ms_ucomm.

DATA: lv_titleInfo   TYPE lvc_title.
