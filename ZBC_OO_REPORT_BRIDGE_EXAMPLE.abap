*&---------------------------------------------------------------------*
*& Report  ZCRM_OO_REPORT_BRIDGE_EXAMPLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcrm_oo_report_bridge_example.

***************************************************************************
* Includes
***************************************************************************
INCLUDE zcrm_oo_report_bridge.

***************************************************************************
* Selection screen
***************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS:
      gv_param      AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.

***************************************************************************
* Report class definition
***************************************************************************
CLASS lcl_report DEFINITION INHERITING FROM lcl_report_base.
  PUBLIC SECTION.
    METHODS:
      start REDEFINITION.

ENDCLASS.                    "lcl_report DEFINITION

***************************************************************************
* Report object and event listener creation & assignment
***************************************************************************
set_report_class lcl_report.

***************************************************************************
* Report class implementation
***************************************************************************
CLASS lcl_report IMPLEMENTATION.
  METHOD start.
    WRITE 'Hello world!'.
  ENDMETHOD.
ENDCLASS.                    "lcl_report IMPLEMENTATION
