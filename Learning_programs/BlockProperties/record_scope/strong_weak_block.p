//Nested weak scope under strong scope
/*DO FOR customer:              //strong scope
 find first customer.            //weak scope - scope raised to DO FOR block
    DISPLAY cust-num name WITH FRAME a.
  ./*  IF cust-num=11 THEN
    DO:
        LEAVE.
    END.    */
 //END.
 //DISP cust-num + 1.
END.       */


//Sequential strong scope and weak scope blocks for same record 

/*FOR EACH customer:		//Weak Scope
 DISPLAY cust-num name.
END.
DO FOR customer:		//Strong scope
 FIND FIRST customer.
 DISPLAY cust-num name.
END.     */
    


//scope raised to procedure
/*REPEAT:
 FIND NEXT customer.                   //free reference
 DISPLAY cust-num name WITH FRAME a.
 LEAVE.
END.
FIND NEXT customer.                //free reference
DISP cust-num NAME WITH FRAME b. */ 


//scope raised to procedure
REPEAT:
 FIND NEXT customer.                   //free reference
 DISPLAY cust-num name WITH FRAME a.
 LEAVE.
END.
REPEAT:
 FIND NEXT  customer.                    //free reference
 DISPLAY cust-num name WITH FRAME b.
END.      
        

