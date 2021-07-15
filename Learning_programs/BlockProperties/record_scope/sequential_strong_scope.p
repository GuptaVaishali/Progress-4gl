/*DO:
 DO FOR customer:
 FIND FIRST customer.
 DISPLAY cust-num name WITH FRAME a.
 END.
 
 DO FOR customer:
 FIND NEXT customer.
 DISPLAY cust-num name WITH FRAME b.
 END.
END.  */  

/*DO FOR customer:          //nested strong scope not possible
 DO FOR customer:
 FIND FIRST customer.
 DISPLAY cust-num name WITH FRAME a.
 END.
 
 DO FOR customer:
 FIND NEXT customer.
 DISPLAY cust-num name WITH FRAME b.
 END.
END.    */     

DO:
 DO FOR customer:       //customer buffer limited to do for block only
 FIND FIRST customer.
 DISPLAY cust-num name.
 END.
 FIND FIRST customer.	//invalid free reference
 DISP cust-num.
END.   
        



