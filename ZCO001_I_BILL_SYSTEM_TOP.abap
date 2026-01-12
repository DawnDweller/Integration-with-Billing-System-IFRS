*&---------------------------------------------------------------------*
*& Include ZCO001_I_BILL_SYSTEM_TOP                 - Report ZCO001_P_BILL_SYSTEM
*&---------------------------------------------------------------------*
CONSTANTS:
           gt_structure1 type string value 'ZCO001_S_ALV_SCREEN',
           gt_structure2 type string value 'ZCO001_S_EXCEL',
           BEGIN OF ms_gui,
              status TYPE char7 VALUE 'STATUS_',
              title  TYPE char6 VALUE 'TITLE_',
           END OF ms_gui,
           BEGIN OF ms_scr,
              s0100 TYPE sy-dynnr VALUE '0100',
              s0101 TYPE sy-dynnr VALUE '0101',
           END OF ms_scr,
           BEGIN OF ms_ucomm,
              back   TYPE sy-ucomm VALUE 'EX001',
              leave  TYPE sy-ucomm VALUE 'EX002',
              exit   TYPE sy-ucomm VALUE 'EX003',
           END OF ms_ucomm,

           BEGIN OF ms_alv_components,
              fcat TYPE char10 VALUE 'FCAT',
              grid TYPE char10 VALUE 'GRID',
              cont TYPE char10 VALUE 'CONT',
              layo TYPE char10 VALUE 'LAYO',
              vari TYPE char10 VALUE 'VARI',
              sort TYPE char10 VALUE 'SORT',
              itab TYPE char10 VALUE 'ITAB',
           END OF ms_alv_components.

DATA: lv_titleInfo   TYPE lvc_title.
