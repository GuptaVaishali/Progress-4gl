/* references an externally-defined user-defined function */
/* define items */
DEFINE VARIABLE myhand AS HANDLE.

/* forward declare doubler() */
FUNCTION doubler RETURNS INTEGER (INPUT parm1 AS INTEGER) IN myhand.

/* run the procedure that doubler() */
RUN d:/vaishali/BlockProperties/external_userfunc/external_user_func1.p PERSISTENT SET myhand.

/* reference doubler() */
DISPLAY "doubler(1)=" doubler(1) SKIP
"doubler(5)=" doubler(5) skip     
"doubler(17)=" doubler(17) SKIP.

/* delete the procedure that defines doubler */
DELETE PROCEDURE(myhand).
