/* Forward-declares, references, and defines a user-defined function */
/* Forward declare doubler() */
FUNCTION doubler RETURNS INTEGER (INPUT parm1 AS INTEGER) FORWARD.
/* Reference doubler() */
DISPLAY "doubler(0)=" doubler(0) SKIP.
DISPLAY "doubler(1)=" doubler(1) SKIP.
DISPLAY "doubler(2)=" doubler(2).
/* Define doubler() */
FUNCTION doubler RETURNS INTEGER.
RETURN (2 * parm1).
END FUNCTION.  


/*DEFINE VARIABLE output1 AS INTEGER no-undo.    
FUNCTION doubler RETURNS INTEGER(INPUT param1 AS INTEGER,OUTPUT paramo AS INTEGER).
    DO:
         paramo = 2 * param1.
         RETURN paramo.
    END.
   
END FUNCTION.

DISPLAY "doubler(6)" doubler(6,output1) SKIP "doubler(2)" doubler(2,OUTPUT1).  */


/*DEFINE VARIABLE output1 AS INTEGER no-undo.    
FUNCTION doubler RETURNS INTEGER(INPUT-OUTPUT param1 AS INTEGER).
    DO:
         RETURN 2 * param1. 
    END.
   
END FUNCTION.
 output1 = 5.
DISPLAY "doubler(6)" doubler(output1) SKIP "doubler(2)" doubler(OUTPUT1). */













        
