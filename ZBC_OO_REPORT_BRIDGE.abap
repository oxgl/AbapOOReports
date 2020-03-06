*&---------------------------------------------------------------------*
*&  Include           ZCRM_OO_REPORT_BRIDGE
*&---------------------------------------------------------------------*
*& Developed by: Oxyggen s.r.o.
*& Description:  Bridge for OO reporting
*&               (class definition, selection screen calls to
*&               class method call bridge)
*&               Only selection screen 1000 is supported in some methods
*&---------------------------------------------------------------------*
*& Version history:
*& 1.0
*&  = Initial version
*& 1.1
*&  + Added support for "ON VALUE-REQUEST"
*& 1.2
*&  + Added support for "ON EXIT COMMAND"
*& 1.3
*&  + get_sel_screen_value method is public
*&  + sel_screen_value_request method initializes value from screen
*& 1.4
*&  + ON_SEL_SCREEN_VALIDATION called before ON_SEL_SCREEN_USER_COMMAND
*&  + call SET_SEL_SCREEN_VALID_FOR_UCOMM in ON_INITIALIZATION
*&    to enable validation.
*& 1.5
*&  + using FM DYNP_VALUES_READ instead of RS_SELECTIONSCREEN_READ
*&    for parameters -> supports values longer than 45 chars
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
*       CLASS lcx_validation_error DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_validation_error DEFINITION INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    DATA msgid TYPE symsgid .
    DATA msgno TYPE symsgno .
    DATA msgty TYPE symsgty .
    DATA msgv1 TYPE symsgv .
    DATA msgv2 TYPE symsgv .
    DATA msgv3 TYPE symsgv .
    DATA msgv4 TYPE symsgv .
    DATA parameter TYPE bapi_param .
    DATA row TYPE bapi_line .
    DATA field TYPE bapi_fld .
    DATA system TYPE bapilogsys .
    METHODS constructor
      IMPORTING
        !textid           LIKE textid OPTIONAL
        !previous         LIKE previous OPTIONAL
        !contains_message TYPE abap_bool OPTIONAL           "#EC NEEDED
        !msgid            TYPE symsgid OPTIONAL
        !msgno            TYPE symsgno OPTIONAL
        !msgty            TYPE symsgty OPTIONAL
        !msgv1            TYPE symsgv OPTIONAL
        !msgv2            TYPE symsgv OPTIONAL
        !msgv3            TYPE symsgv OPTIONAL
        !msgv4            TYPE symsgv OPTIONAL
        !parameter        TYPE bapi_param OPTIONAL
        !row              TYPE bapi_line OPTIONAL
        !field            TYPE bapi_fld OPTIONAL
        !system           TYPE bapilogsys OPTIONAL .
    METHODS get_message
      RETURNING
        VALUE(rs_message) TYPE bapiret2 .
    CLASS-METHODS raise_from_sy
      RAISING
        lcx_validation_error .
    CLASS-METHODS raise_from_text
      IMPORTING
        !iv_msgty TYPE symsgty DEFAULT 'E'
        !iv_text  TYPE clike
      RAISING
        lcx_validation_error .
    CLASS-METHODS raise_from_exception
      IMPORTING
        !iv_msgty      TYPE symsgty DEFAULT 'E'
        !iro_exception TYPE REF TO cx_root
      RAISING
        lcx_validation_error .
    METHODS if_message~get_longtext
        REDEFINITION .
    METHODS if_message~get_text
        REDEFINITION .
  PROTECTED SECTION.
    METHODS raise_from_sy_internal
      IMPORTING
        !iv_parameter TYPE clike OPTIONAL
        !iv_row       TYPE clike OPTIONAL
        !iv_field     TYPE clike OPTIONAL
        !iv_system    TYPE clike OPTIONAL
      RAISING
        lcx_validation_error .
ENDCLASS.                    "lcx_validation_error DEFINITION


CLASS lcl_report_helper DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_report_base DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report_base DEFINITION ABSTRACT FRIENDS lcl_report_helper.
  PUBLIC SECTION.
    TYPES:
      tv_selkind             TYPE rsscr_kind.

    CONSTANTS:
      gc_selkind_param       TYPE tv_selkind VALUE 'P',
      gc_selkind_selopt      TYPE tv_selkind VALUE 'S',
      gc_selkind_selopt_low  TYPE tv_selkind VALUE 'l',
      gc_selkind_selopt_high TYPE tv_selkind VALUE 'h'.

    METHODS:
      on_load_of_program,
      on_initialization,
      on_sel_screen_output,
      on_sel_screen_user_command
        IMPORTING
          iv_ucomm TYPE syucomm,                            "#EC NEEDED
      on_sel_screen_exit_command
        IMPORTING
          iv_ucomm TYPE syucomm,                            "#EC NEEDED
      on_sel_screen_validation
        IMPORTING
          iv_ucomm TYPE syucomm OPTIONAL                    "#EC NEEDED
        RAISING
          lcx_validation_error,
      on_sel_screen_value_request
        IMPORTING
          iv_field_name  TYPE string                        "#EC NEEDED
          iv_field_kind  TYPE tv_selkind                    "#EC NEEDED
        CHANGING
          cv_field_value TYPE any,                          "#EC NEEDED
      start ABSTRACT,
      get_sel_screen_value
        IMPORTING
          iv_field_name         TYPE rsscr_name
          iv_field_kind         TYPE tv_selkind DEFAULT gc_selkind_param
        RETURNING
          VALUE(rv_field_value) TYPE dynfieldvalue.

  PROTECTED SECTION.

    DATA:
      gt_ucomm_validation TYPE SORTED TABLE OF syucomm WITH UNIQUE KEY table_line.

    METHODS:
      set_sel_screen_valid_for_ucomm
        IMPORTING
          iv_ucomm  TYPE syucomm
          iv_enable TYPE abap_bool DEFAULT abap_true.



ENDCLASS.                    "lcl_report_base DEFINITION



*----------------------------------------------------------------------*
*       CLASS lcl_report_helper DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      load_of_program
        IMPORTING
          iro_instance TYPE REF TO lcl_report_base,
      initialization,
      sel_screen_output,
      sel_screen_value_request
        IMPORTING
          iv_field_name  TYPE string
          iv_field_kind  TYPE lcl_report_base=>tv_selkind
        CHANGING
          cv_field_value TYPE any,
      sel_screen,
      sel_screen_exit_command,
      start.

  PROTECTED SECTION.
    CLASS-DATA:
      gro_instance    TYPE REF TO lcl_report_base.
ENDCLASS.                    "lcl_report_helper DEFINITION

DEFINE set_report_class.

  LOAD-OF-PROGRAM.
    DATA:
       gro_user_report_instance   TYPE REF TO &1.
    CREATE OBJECT gro_user_report_instance.
    lcl_report_helper=>load_of_program( gro_user_report_instance ).

  INITIALIZATION.
    lcl_report_helper=>initialization( ).

  AT SELECTION-SCREEN OUTPUT.
    lcl_report_helper=>sel_screen_output( ).

  AT SELECTION-SCREEN.
    lcl_report_helper=>sel_screen( ).

  AT SELECTION-SCREEN ON EXIT-COMMAND.
    lcl_report_helper=>sel_screen_exit_command( ).

  START-OF-SELECTION.
    lcl_report_helper=>start( ).

  END-OF-SELECTION.

END-OF-DEFINITION.

DEFINE register_value_req_for_param.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR &1.
    lcl_report_helper=>sel_screen_value_request(
      EXPORTING
        iv_field_name  = '&1'
        iv_field_kind  = lcl_report_base=>gc_selkind_param
      CHANGING
        cv_field_value = &1 ).

END-OF-DEFINITION.

DEFINE register_value_req_for_selopt.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR &1-low.
    lcl_report_helper=>sel_screen_value_request(
      EXPORTING
        iv_field_name  = '&1-LOW'
        iv_field_kind  = lcl_report_base=>gc_selkind_selopt_low
      CHANGING
        cv_field_value = &1-low ).

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR &1-high.
    lcl_report_helper=>sel_screen_value_request(
      EXPORTING
        iv_field_name  = '&1-HIGH'
        iv_field_kind  = lcl_report_base=>gc_selkind_selopt_high
      CHANGING
        cv_field_value = &1-high ).

END-OF-DEFINITION.





*----------------------------------------------------------------------*
*       CLASS lcl_report_helper IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report_helper IMPLEMENTATION.
  METHOD load_of_program.
    gro_instance = iro_instance.
    gro_instance->on_load_of_program( ).
  ENDMETHOD.                    "load_of_program

  METHOD initialization.
    gro_instance->on_initialization( ).
  ENDMETHOD.                    "initialization

  METHOD sel_screen_output.
    gro_instance->on_sel_screen_output( ).
  ENDMETHOD.                    "sel_screen_output

  METHOD sel_screen.
    IF sy-ucomm IS NOT INITIAL.
      DATA(lv_ucomm) = sy-ucomm.

      IF line_exists( gro_instance->gt_ucomm_validation[ table_line = lv_ucomm ] ).
        TRY.
            gro_instance->on_sel_screen_validation( iv_ucomm = lv_ucomm ).
          CATCH lcx_validation_error INTO DATA(lro_validation_error).
            DATA(ls_message) = lro_validation_error->get_message( ).
            MESSAGE ID ls_message-id
                  TYPE 'S'
                NUMBER ls_message-number
                  WITH ls_message-message_v1
                       ls_message-message_v2
                       ls_message-message_v3
                       ls_message-message_v4
               DISPLAY LIKE ls_message-type.
            EXIT.
        ENDTRY.
      ENDIF.

      gro_instance->on_sel_screen_user_command( iv_ucomm = lv_ucomm ).
    ENDIF.
  ENDMETHOD.                    "sel_screen_user_command

  METHOD sel_screen_exit_command.
    IF sy-ucomm IS NOT INITIAL.
      gro_instance->on_sel_screen_exit_command( iv_ucomm = sy-ucomm ).
    ENDIF.
  ENDMETHOD.                    "sel_screen_user_command


  METHOD sel_screen_value_request.
    cv_field_value = gro_instance->get_sel_screen_value(
                        iv_field_name  = CONV #( iv_field_name )
                        iv_field_kind  = iv_field_kind ).

    gro_instance->on_sel_screen_value_request(
      EXPORTING
        iv_field_name  = iv_field_name
        iv_field_kind  = iv_field_kind
      CHANGING
        cv_field_value = cv_field_value ).
  ENDMETHOD.                    "sel_screen_value_request

  METHOD start.

    TRY.
        gro_instance->on_sel_screen_validation( ).
      CATCH lcx_validation_error INTO DATA(lro_validation_error).
        DATA(ls_message) = lro_validation_error->get_message( ).
        MESSAGE ID ls_message-id
              TYPE 'S'
            NUMBER ls_message-number
              WITH ls_message-message_v1
                   ls_message-message_v2
                   ls_message-message_v3
                   ls_message-message_v4
           DISPLAY LIKE ls_message-type.
        EXIT.
    ENDTRY.

    gro_instance->start( ).
  ENDMETHOD.                    "start

ENDCLASS.                    "lcl_report_helper IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_report_base IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_report_base IMPLEMENTATION.

  METHOD on_load_of_program.                                "#EC NEEDED
  ENDMETHOD.                    "on_load_of_program

  METHOD on_initialization.                                 "#EC NEEDED
  ENDMETHOD.                    "on_initialization

  METHOD on_sel_screen_output.                              "#EC NEEDED
  ENDMETHOD.                    "on_sel_screen_output

  METHOD on_sel_screen_user_command.                        "#EC NEEDED
  ENDMETHOD.                    "on_sel_screen_user_command

  METHOD on_sel_screen_exit_command.                        "#EC NEEDED
  ENDMETHOD.                    "on_sel_screen_exit_command

  METHOD on_sel_screen_validation.                          "#EC NEEDED
  ENDMETHOD.                    "on_sel_screen_validation

  METHOD on_sel_screen_value_request.                       "#EC NEEDED
  ENDMETHOD.                    "on_sel_screen_value_request

  METHOD get_sel_screen_value.
    DATA:
      lt_rsselread  TYPE TABLE OF rsselread,
      lt_dynpfields TYPE dynpread_tabtype.

    CASE iv_field_kind.
      WHEN gc_selkind_selopt_low
        OR gc_selkind_selopt.
        APPEND VALUE #(
                  name     = iv_field_name
                  kind     = gc_selkind_selopt
                  position = 'LOW'
               ) TO lt_rsselread.
      WHEN gc_selkind_selopt_high.
        APPEND VALUE #(
                  name     = iv_field_name
                  kind     = gc_selkind_selopt
                  position =  'HIGH'
               ) TO lt_rsselread.
      WHEN OTHERS.
        APPEND VALUE #(
                 fieldname = iv_field_name
               ) TO lt_dynpfields.

    ENDCASE.

    IF lt_rsselread IS NOT INITIAL.
      CALL FUNCTION 'RS_SELECTIONSCREEN_READ'
        EXPORTING
          program     = sy-cprog
          dynnr       = '1000'
        TABLES
          fieldvalues = lt_rsselread.

      READ TABLE lt_rsselread
       ASSIGNING FIELD-SYMBOL(<fss_rsselread>)
        WITH KEY name = iv_field_name.

      IF sy-subrc IS INITIAL.
        rv_field_value = <fss_rsselread>-fieldvalue.
      ENDIF.
    ELSEIF lt_dynpfields IS NOT INITIAL.
      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname               = sy-cprog
          dynumb               = '1000'
        TABLES
          dynpfields           = lt_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          invalid_parameter    = 7
          undefind_error       = 8
          double_conversion    = 9
          stepl_not_found      = 10
          OTHERS               = 11.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_dynpfields ASSIGNING FIELD-SYMBOL(<fss_dynpfield>)
              WITH KEY fieldname = iv_field_name.
        IF sy-subrc IS INITIAL.
          rv_field_value = <fss_dynpfield>-fieldvalue.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_sel_screen_value


  METHOD set_sel_screen_valid_for_ucomm.
    IF iv_enable = abap_true.
      INSERT iv_ucomm INTO TABLE gt_ucomm_validation.
    ELSE.
      DELETE TABLE gt_ucomm_validation
        WITH TABLE KEY table_line = iv_ucomm.
    ENDIF.
  ENDMETHOD.

ENDCLASS.                    "lcl_report_base IMPLEMENTATION




*----------------------------------------------------------------------*
*       CLASS lcx_validation_error IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_validation_error IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    me->msgid     = msgid.
    me->msgno     = msgno.
    me->msgty     = msgty.
    me->msgv1     = msgv1.
    me->msgv2     = msgv2.
    me->msgv3     = msgv3.
    me->msgv4     = msgv4.
    me->parameter = parameter.
    me->row       = row.
    me->field     = field.
    me->system    = system.
  ENDMETHOD.                    "constructor

  METHOD raise_from_sy_internal.

    msgid            = sy-msgid.
    msgno            = sy-msgno.
    msgty            = sy-msgty.
    msgv1            = sy-msgv1.
    msgv2            = sy-msgv2.
    msgv3            = sy-msgv3.
    msgv4            = sy-msgv4.
    parameter        = iv_parameter.
    row              = iv_row.
    field            = iv_field.
    system           = iv_system.

    RAISE EXCEPTION me.

  ENDMETHOD.                    "raise_from_sy_internal

  METHOD raise_from_sy.
    DATA
        lro_validation_error TYPE REF TO lcx_validation_error.

    CREATE OBJECT lro_validation_error.
    lro_validation_error->raise_from_sy_internal( ).
  ENDMETHOD.                    "raise_from_sy


  METHOD raise_from_text.
    DATA:
      lv_text200           TYPE text200,
      lro_validation_error TYPE REF TO lcx_validation_error.

    lv_text200 = iv_text.

    CREATE OBJECT lro_validation_error.

    lro_validation_error->msgty = iv_msgty.
    lro_validation_error->msgid = '0Q'.
    lro_validation_error->msgno = '000'.
    lro_validation_error->msgv1 = lv_text200(50).
    lro_validation_error->msgv2 = lv_text200+50(50).
    lro_validation_error->msgv3 = lv_text200+100(50).
    lro_validation_error->msgv4 = lv_text200+150(50).

    RAISE EXCEPTION lro_validation_error.

  ENDMETHOD.                    "raise_from_text


  METHOD raise_from_exception.

    IF iro_exception IS BOUND.
      raise_from_text(
        iv_msgty = iv_msgty
        iv_text  = iro_exception->get_text( ) ).
    ELSE.
      raise_from_text(
        iv_msgty = iv_msgty
        iv_text  = '-- NO TEXT --' ).
    ENDIF.

  ENDMETHOD.                    "raise_from_exception


  METHOD get_message.
    CLEAR rs_message.
    rs_message-type       = msgty.
    rs_message-id         = msgid.
    rs_message-number     = msgno.
    rs_message-message_v1 = msgv1.
    rs_message-message_v2 = msgv2.
    rs_message-message_v3 = msgv3.
    rs_message-message_v4 = msgv4.
    rs_message-parameter  = parameter.
    rs_message-row        = row.
    rs_message-field      = field.
    rs_message-system     = system.

    rs_message-message    = if_message~get_text( ).
  ENDMETHOD.                    "get_message

  METHOD if_message~get_text.

    MESSAGE ID msgid
          TYPE 'S'
        NUMBER msgno
          WITH msgv1
               msgv2
               msgv3
               msgv4
          INTO result.

  ENDMETHOD.                    "if_message~get_text


  METHOD if_message~get_longtext.
    result = if_message~get_text( ).
  ENDMETHOD.                    "if_message~get_longtext

ENDCLASS.                    "lcx_validation_error IMPLEMENTATION
