/*DEFINE VARIABLE i AS INTEGER.
i=1.
DO WHILE(i<=10) WITH FRAME f:
    DISPLAY i MODULO 2.
    i = i + 1. //increment necessray in while loop ..otherwise it would be a infinite loop.
END. */   


/*DEFINE VARIABLE i AS INTEGER LABEL "value".
i=1.
DO i=1 TO 10:    //if have to use the last value of loop ..then do not use with frame clause.
    DISPLAY i.
END.  */


/*DEFINE VARIABLE i AS INTEGER LABEL "value".
i=1.
DO i=1 TO 10 WITH FRAME f:    //use frame to display all values between 1 to 10.
    DISPLAY i.
    if(i=5)then leave.
END.      */


//this is just a block..not a loop
/*DEFINE VARIABLE i AS INTEGER LABEL "value".
i=1.
DO:    //if have to use the last value of loop ..then do not use with frame clause.
    DISPLAY i.
    i = i + 1.  
END.    */  



/*DEFINE VARIABLE i AS INTEGER no-undo.
DO i=1 TO 5 WITH FRAME f:
     FIND NEXT customer.                       // here find statemet need to bring the record from databse to record buffer. which is not used in for each loop.
     DISPLAY cust-num NAME.
END.   */


DEFINE VARIABLE i AS INTEGER.
REPEAT i=1 TO 5:
            FIND NEXT customer.           // here find statemet need to bring the record from databse to record buffer.
             DISPLAY cust-num NAME.
END.   

/*FOR EACH customer:                        // for each used to bringing the records from database to record buffer and for iteration also.
    DISPLAY customer WITH 2 COLUMNS.
END. */
