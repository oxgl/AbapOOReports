# AbapOOReports
ABAP Object Oriented Reporting

ABAP reports are usually written in non-object oriented blocks of code. Project goal is to create a bridge between this non-OO and OO world. So with this bridge you can create an object oriented report. There are still some limitations.

So how to create an OO report:
1. Create an executable report in ABAP editor
  
2. Include ZBC_OO_REPORT_BRIDGE:
```ABAP   
INCLUDE zcrm_oo_report_bridge.
```

3. Create a selection screen (if needed)
```ABAP   
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
 PARAMETERS:
   gv_param      AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.
```
  
4. Create your own report class definition (let's say lcl_report), inherit from the class lcl_report_base and implement abstract method "start".
```ABAP   
CLASS lcl_report DEFINITION INHERITING FROM lcl_report_base FINAL.
  PUBLIC SECTION.
	 METHODS:
	   start REDEFINITION.
...
```
     
5. Add this single line, this will initialize the "bridge"
```ABAP
set_report_class lcl_report.
```

6. Implement your class:
```ABAP
CLASS lcl_report IMPLEMENTATION.
	METHOD start.
	ENDMETHOD.
...
```
     
You can copy the provided example class ZBC_OO_REPORT_BRIDGE_EXAMPLE. 

To handle selection screen events just redefine the following methods from superclass:
1. on_load_of_program
event is triggered by the ABAP runtime environment when one of the executable programs mentioned above is loaded into the internal session. 

2. on_initialization
method for initializing an executable program
     
3. on_sel_screen_output
triggered by the dynpro event PBO of a selection screen
     
4. on_sel_screen_user_command
first you have to register user command handling using the method SET_SEL_SCREEN_VALID_FOR_UCOMM in method ON_INITIALIZATION:
```ABAP   
set_sel_screen_valid_for_ucomm( 'FC01' ).
```
     
5. on_sel_screen_exit_command
called when exit command is called on selection screen
     
6. on_sel_screen_validation
called before report execution (START-OF-SELECTION)
     
7. on_sel_screen_value_request
     you have to register fields using macros:
```ABAP   
	 register_value_req_for_param <param_name>.
	 register_value_req_for_selopt <selopt_name>.
```
to get the current values on selection screen do not read directly the parameter/selopt, but call method get_sel_screen_value
	 

  
