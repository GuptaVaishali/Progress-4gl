/*DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE qh AS HANDLE  NO-UNDO.
DEFINE VARIABLE bh AS HANDLE  NO-UNDO.
DEFINE VARIABLE fh AS HANDLE  NO-UNDO EXTENT 10.

CREATE BUFFER bh FOR TABLE "Customer".
CREATE QUERY qh.

qh:SET-BUFFERS(bh).
qh:QUERY-PREPARE("FOR EACH Customer").
qh:QUERY-OPEN().
qh:GET-FIRST().

DISPLAY bh:NAME.

REPEAT ix = 1 TO 10:
  fh[ix] = bh:BUFFER-FIELD(ix).
  DISPLAY fh[ix]:NAME STRING(fh[ix]:BUFFER-VALUE).
END.

qh:QUERY-CLOSE().
bh:BUFFER-RELEASE().
DELETE OBJECT bh.
DELETE OBJECT qh.  */

/*****************************************************************************************************/

DEF VAR ctablename AS CHAR NO-UNDO.
DEFINE VARIABLE qh AS HANDLE  NO-UNDO.
DEFINE VARIABLE hbuf AS HANDLE NO-UNDO.
DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE fh AS HANDLE  NO-UNDO EXTENT 10.

SET ctablename.
DO TRANSACTION:
CREATE QUERY qh.
IF ctablename = "customer" THEN
DO:
   hbuf = BUFFER Customer:HANDLE. 
   qh:SET-BUFFERS(hbuf).
   qh:QUERY-PREPARE("FOR EACH Customer where createddate < (today - 10)").
END.
ELSE IF ctablename = "order" THEN
DO:
   hbuf = BUFFER order:HANDLE. 
   qh:SET-BUFFERS(hbuf).
   qh:QUERY-PREPARE("FOR EACH order where createddate < (today - 10)").
END.  

DEF VAR isAvailable AS LOGICAL NO-UNDO.

qh:QUERY-OPEN().
isAvailable = qh:GET-NEXT(EXCLUSIVE-LOCK,NO-WAIT).
MESSAGE isAvailable
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

//why this locked record is available????
/*IF hbuf:AVAILABLE THEN
DO:
   DISPLAY hbuf:NAME.
//   REPEAT ix = 1 TO 5:
      //  fh[ix] = hbuf:BUFFER-FIELD(ix).
        UPDATE NAME.
      //  DISPLAY fh[ix]:NAME STRING(fh[ix]:BUFFER-VALUE).
//   END.
END.       */
//ELSE

IF isAvailable THEN
DO:
    IF hbuf:LOCKED THEN
    DO:
        MESSAGE "locked record"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        qh:GET-NEXT().
    END.
    REPEAT ix = 1 TO 5:
          fh[ix] = hbuf:BUFFER-FIELD(ix).
          DISPLAY fh[ix]:NAME STRING(fh[ix]:BUFFER-VALUE).
    END.
END.
ELSE
    MESSAGE "Records not available"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.    
END.
//FIND configuration WHERE tablename = hbuf.




/*****************************************************************************************************************/
/*DEFINE VARIABLE hbuf AS HANDLE NO-UNDO.
DEFINE VARIABLE htab AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tt2 NO-UNDO
  FIELD x AS CHARACTER.

CREATE TEMP-TABLE htab.
htab:TEMP-TABLE-PREPARE( "dynTT" ).

/* Obtaining the default buffer for a table */
hbuf = BUFFER Customer:HANDLE.     /* For a database table */
hbuf = BUFFER tt2:HANDLE.          /* For a static temp-table */
hbuf = htab:DEFAULT-BUFFER-HANDLE. /* For a dynamic temp-table */     */

/**********************************************************************************************************************/

/*DEFINE VARIABLE qh AS WIDGET-HANDLE.
DEFINE VARIABLE numvar AS INTEGER INITIAL 10.

CREATE QUERY qh.
qh:SET-BUFFERS(BUFFER customer:HANDLE).
qh:QUERY-PREPARE("FOR EACH customer WHERE custnum < " + string(numvar)).
qh:QUERY-OPEN.
 
REPEAT WITH FRAME y:
    qh:GET-NEXT().
    IF qh:QUERY-OFF-END THEN LEAVE.
          DISPLAY custnum
                         name      FORMAT "x(30)"
                         city         FORMAT "X(20)".
    END.

qh:QUERY-CLOSE().
DELETE OBJECT qh.   */



