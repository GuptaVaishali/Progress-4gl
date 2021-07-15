//Connecting Database
CONNECT D:\vaishali\Sports1.db NO-ERROR.

RUN D:\vaishali\DatabaseAccess&Trigger\NextCustName.p.
//pre requisite
//proserve D:/vaishali/Sports1 -n 5

FIND NEXT customer NO-LOCK NO-ERROR.
DISP cust-num NAME.


//Checking If connected or not
IF CONNECTED("sports1") THEN
  DO:
    DISPLAY "DataBase Connected".
    //Run Script
    //....
  END.
ELSE
    MESSAGE "Unable to Connect to DataBase" VIEW-AS ALERT-BOX.
    

//Disconnect DataBase
DISCONNECT sports1 NO-ERROR.

//pro sports1 -ld firstdb
//Progress assumes that database is already connected and ignores the request.


//slide 5

CONNECT D:\vaishali\Sports1.db NO-ERROR.
DEF VAR x as INT.
//Num-DBS Returns the number of connected databases.
DO x = 1 TO NUM-DBS WITH DOWN:
DISPLAY PDBNAME(x) LABEL "Physical Database"
LDBNAME(x) LABEL "Logical Name"
DBTYPE(x) LABEL "Database Type" 
DBRESTRICTIONS(x) LABEL "Restrictions"                         
SDBNAME(LDBNAME(x)) LABEL "Schema Holder DB".
END. 


//slide 6
//Database must be connected in order to make alias.
CONNECT D:\vaishali\Sports1.db NO-ERROR.

//Creating Alias (When want to use different logical database name)
CREATE ALIAS sDb FOR DATABASE Sports1 NO-ERROR.

IF CONNECTED("sDb") THEN
DO:
    RUN D:\vaishali\DatabaseAccess&Trigger\NextCustName.p.
    //Delete Alias
    DELETE ALIAS sDb.
END.


//slide 7
DEF VAR a AS DEC.
DEF VAR b AS DEC.

DISP SKIP "Enter values for a and b".
//prompt-for
PROMPT-FOR a b.
DISP a b.

//assign
ASSIGN a b.
DISP a + b LABEL "Sum".

DEF VAR c AS DEC INITIAL 5.

//set
SET c WITH FRAME f.

//update
UPDATE c WITH FRAME f1.
      

DEF BUTTON bdisplay.
DEF FRAME b1 bdisplay.
ON CHOOSE OF bdisplay IN FRAME b1 DO:
    DISP "Button is Clicked".
END.
//enable
ENABLE bdisplay WITH FRAME b1.
WAIT-FOR GO OF FRAME b1.

PROMPT-FOR Customer.cust-num.
//find
FIND customer USING cust-num NO-LOCK NO-ERROR.
DISPLAY NAME cust-num SKIP.

//for each
FOR EACH customer WHERE cust-num < 5 NO-LOCK:
    DISP cust-num NAME.
END.

//define query
DEF QUERY cust FOR customer.
//open query
OPEN QUERY cust FOR EACH cust WHERE cust-num < 5.
//get
GET FIRST cust.
DISP cust-num NAME.
//RELEASE cust.
CLOSE QUERY cust.
//DISP cust-num NAME.


//define query
DEF QUERY cust FOR customer.
//open query
OPEN QUERY cust FOR EACH cust WHERE cust-num < 5.
//get
    REPEAT:
        GET NEXT cust.
        IF QUERY-OFF-END('cust') THEN LEAVE.
        DISP cust-num NAME.
    END.
   
//RELEASE cust.
CLOSE QUERY cust.
//DISP cust-num NAME.


//slide 9

//insert
INSERT customer.

//create
CREATE customer.
DISP customer.
PROMPT-FOR customer.
ASSIGN customer.


//insert = create + update
CREATE customer.
UPDATE customer.


FOR EACH customer:
DISP name.
END.

//delete
FIND LAST customer.
DELETE customer.
//DISP customer.



//slide 10
PROMPT-FOR Order.Order-Num.
//find
FIND Order NO-LOCK USING Order.Order-Num.
DISPLAY Order.
//OF
FIND Customer OF Order NO-LOCK.
DISPLAY customer. 

FIND FIRST customer WHERE cust-num > 5.
DISPLAY cust-num name.
PAUSE.
FIND NEXT customer WHERE cust-num > 1.
DISPLAY cust-num name.

//The value of a single component primary index for the record you want.
//This option is not supported for the OPEN QUERY statement:
FIND Customer 1. 
DISP cust-num name.

//The cust-num field is the only primary index of the Customer table.
FIND Customer NO-LOCK WHERE Customer.cust-num = 1.                               
DISP cust-num name.

/*Returns a TRUE value if a record is found that meets the
specified FIND criteria; otherwise it returns FALSE.
CAN-FIND does not make the record available to the procedure
*/

DISP CAN-FIND(FIRST Customer WHERE Customer.State = "NH").


//without each 1 result only
FOR customer WHERE name = "vaishali":
DISP cust-num NAME.
END.

//works like find statement
/*FOR FIRST customer WHERE country = "india".
DISP cust-num NAME.
END.   */

//each
FOR EACH customer WHERE name = "vaishali":
DISP cust-num NAME.
END.


//This statement retrieves only the Name and Balance fields of the Customer table:
FOR EACH customer FIELDS (name balance): DISPLAY name balance. 


//This statement retrieves all fields of the Customer table except the Name and Balance fields:
FOR EACH Customer EXCEPT (Name Balance):
  DISPLAY Customer EXCEPT Customer.Name Customer.Balance.

  
//define query
DEF QUERY cust FOR customer.
//open query
OPEN QUERY cust FOR EACH cust WHERE cust-num < 5 .
//get
GET FIRST cust.
DISP cust-num NAME.
//RELEASE cust.
CLOSE QUERY cust.
//DISP cust-num NAME.


//PRESELECT 
/* This code displays all customers in descending order */
DO PRESELECT EACH customer:
    FIND LAST customer. /* last position in list */
    DISPLAY cust-num name WITH FRAME a DOWN.
    REPEAT:
        FIND PREV customer. /* move backward through list */
        DOWN WITH FRAME a.
        DISPLAY cust-num name WITH FRAME a.
    END.   
END.


//slide 14
//BY
FOR EACH Customer BY Customer.Credit-Limit BY Customer.NAME.
DISP NAME Credit-Limit.
END.

//BREAK BY
FOR EACH Customer BREAK BY Customer.State:
  DISPLAY Customer.State Customer.Name 
    Customer.Credit-Limit (TOTAL BY state).
END. 


DEFINE VARIABLE iCount AS INT NO-UNDO.

FOR EACH customer NO-LOCK BY state:
iCount = iCount + 1.
DISP state iCount.
END.

FOR EACH customer NO-LOCK BREAK BY state:
    IF FIRST-OF(state) THEN
    DO:
        iCount = 0.
    END.
    iCount = iCount + 1.
    IF LAST-OF(state) THEN
    DO:
        DISP state iCount.
    END.
END.


//USE-INDEX
PROMPT-FOR Customer.cust-num.
FIND Customer USING Customer.cust-num NO-LOCK NO-ERROR.
DISP cust-num NAME.

//use-index
FIND FIRST customer NO-LOCK NO-ERROR.
DISPLAY cust-num name country postal-code.
PAUSE.
FIND NEXT customer NO-LOCK NO-ERROR.
DISPLAY cust-num name country postal-code.
PAUSE.
FIND FIRST customer USE-INDEX countrypost NO-LOCK NO-ERROR.
DISPLAY cust-num name country postal-code.
PAUSE.
FIND NEXT customer USE-INDEX countrypost NO-LOCK NO-ERROR.
DISPLAY cust-num name country postal-code.

//This will give error as state is not an index field.
FOR EACH customer USE-INDEX state:
    DISP cust-num NAME state.
END.


//slide 15
//aggregate
FOR EACH Customer NO-LOCK BREAK BY Customer.state:
  DISPLAY Customer.state Customer.Balance (TOTAL BY Customer.state).
END. 

//ACCUMULATE
FOR EACH Customer NO-LOCK:
  ACCUMULATE Customer.Credit-Limit (AVERAGE COUNT MAXIMUM).
END.

DISPLAY "MAX-CREDIT STATISTICS FOR ALL CUSTOMERS:" SKIP(2)
        "AVERAGE =" (ACCUM AVERAGE Customer.Credit-Limit) SKIP(1)
        "MAXIMUM =" (ACCUM MAXIMUM Customer.Credit-Limit) SKIP(1)
        "NUMBER OF CUSTOMERS =" (ACCUM COUNT Customer.Credit-Limit) SKIP(1)
        WITH NO-LABELS. 


//slide 16
/*
QUERY has following Characteristics:

For an index-sorted query, Progress can use the index to order records without a results
list.

For a presorted query, Progress must read all the records, sort them,
and build the complete results list before any records are fetched.

You can force Progress to build a preselected results list by specifying
PRESELECT on the OPEN QUERY, DO, or REPEAT statement.
*/
DEF VAR oCount AS INT.
DEFINE QUERY qname FOR order.
OPEN QUERY qname FOR EACH order USE-INDEX Order-Num.
oCount = NUM-RESULTS("qname").
DISP oCount.
  
/*DEF VAR oCount AS INT.
DEFINE QUERY qname FOR order SCROLLING.
OPEN QUERY qname FOR EACH order.
//GET FIRST qname.
//DISP Order-Num.
oCount = NUM-RESULTS("qname").
DISP oCount.  */

/*Progress also builds a complete results list when you open a query 
with a sort condition that cannot be resolved using a single index  */
DEF VAR iCount AS INT.
DEFINE QUERY qname FOR customer.
OPEN QUERY qname FOR EACH customer BY state.
iCount = NUM-RESULTS("qname").
DISP iCount.

DEF VAR oCount AS INT.
DEFINE QUERY qname FOR order.
OPEN QUERY qname PRESELECT EACH order BY Order-Num.
oCount = NUM-RESULTS("qname").
DISP oCount.

//slide 17
//SCROLLING

/*
A query is scrolling if you specify SCROLLING in the DEFINE QUERY statement
or if you define a browse for the query. You can use the REPOSITION statement
to change your current position within the results list.
For a non-scrolling query, you can only move sequentially through the rows by using the
FIRST, LAST, NEXT, and PREV options of the GET statement.
*/

DEFINE QUERY q FOR customer SCROLLING.
DEFINE VARIABLE rid AS ROWID. /* to save the ROWID of cust 4 */
OPEN QUERY q FOR EACH customer.
GET NEXT q. /* gets cust no. 1 */
GET NEXT q. /* gets cust no. 2 */
/* query is positioned ON cust 2 */

GET PREV q. /* gets cust no. 1 */
REPOSITION q FORWARD 0. /* query is positioned BETWEEN cust 1 and 2 */
GET NEXT q. /* gets cust no. 2 */

/* query is positioned ON cust 2 */
REPOSITION q FORWARD 1. /* query is positioned BETWEEN cust 3 and 4 */
GET NEXT q. /* gets cust no. 4 */
rid = ROWID(cust). /* query is positioned ON cust 4 */

REPOSITION q BACKWARD 2. /* query is positioned BETWEEN cust 2 and 3 */
GET PREV q. /* gets cust no. 2 */

REPOSITION q TO ROWID(rid). /* query is positioned BETWEEN cust 3 and 4 */ 
GET NEXT q. /* gets cust no. 4 */
DISP cust-num NAME.


//slide 17
//ROWID
//fetches the first customer record, and if it has a balance,
//refetches it to lock it for update

DEFINE VARIABLE custrid AS ROWID.
FIND FIRST customer NO-LOCK.
custrid = ROWID(customer).
IF balance > 0 THEN DO:
FIND customer WHERE ROWID(customer) = custrid EXCLUSIVE-LOCK.
UPDATE customer.
END.

//Cant store directly in temp-table
DEFINE TEMP-TABLE ttrid FIELD ridfld AS CHARACTER.
FOR EACH customer FIELDS (balance) WHERE balance = 0 NO-LOCK:
    CREATE ttrid.
    ASSIGN ttrid.ridfld = STRING(ROWID(customer)).
END.
DO TRANSACTION:
    FOR EACH ttrid:
        FIND customer WHERE ROWID(customer) = TO-ROWID(ttrid.ridfld).
        DELETE customer.
    END.
END.         

//slide 19


/*OF table 
Relates record to one other table specified by a table or buffer name (table).
The relationship is based on common field names between 
and table
that also participate in a UNIQUE index for either record or table.
*/
PROMPT-FOR Order.Order-Num.
FIND Order NO-LOCK USING Order.Order-Num.

FIND Customer OF Order NO-LOCK.
DISPLAY NAME. 


//slide 20
//join
/*OPEN QUERY Query1 FOR EACH Table1, EACH Table2 OUTER-JOIN
WHERE Field1 = Field3
*/ 
DEFINE VAR i AS INT NO-UNDO.
DEFINE QUERY qname FOR customer, order.
OPEN QUERY qname PRESELECT EACH customer,EACH order OUTER-JOIN WHERE customer.cust-num= order.cust-num 
AND customer.cust-num=1.
DISP NUM-RESULTS("qname").
REPEAT i=1 TO NUM-RESULTS("qname"):
GET NEXT qname.
DISP customer.cust-num NAME Order-Num.
END.


/*
DEFINE VAR i AS INT.
FIND customer WHERE cust-num=1.
FOR EACH order OF customer:
DISP customer.cust-num Order-Num.
i= i + 1.
END.
DISP i.
*/

//slide 23
FOR EACH Item NO-LOCK WHERE Item.Cat-Description CONTAINS "ski":
  DISPLAY Item.Item-Name Item.Cat-Description VIEW-AS EDITOR SIZE 60
   BY 15.
END.

FOR EACH customer WHERE comments CONTAINS "all*".
DISP NAME comments VIEW-AS EDITOR SIZE 60 BY 15.
END.

//CONTAINS
FOR EACH customer WHERE comments CONTAINS "at & all*".
DISP NAME comments VIEW-AS EDITOR SIZE 60 BY 15.
END.

//The CONTAINS option is not allowed in a FIND statement

//Word indexes are case insensitive unless a field
//participating in the word index is case sensitive.

//slide 26
//put-byte
/* You must run this procedure against a non-Progress sports database. */
DEFINE VAR i AS INT NO-UNDO.
DEFINE VAR a AS INT NO-UNDO.
FIND cust WHERE cust-num = 27.
i = 1.
REPEAT:
a = GETBYTE(RAW(name),i).
DISPLAY a.
IF a = ? THEN LEAVE.
i = i + 1.
END.

//get-byte
/* You must run this procedure against a non-Progress sports database. */
DEFINE VAR r3 AS RAW.
FIND FIRST cust.
r3 = RAW(name).
DISPLAY LENGTH(r3) name WITH DOWN. /* length before change */
DOWN.
LENGTH(r3) = 2.
DISPLAY LENGTH(r3) name. /* length after change */


//slide 31

TRIGGER PROCEDURE FOR Create OF Customer.
/* Automatically Increment Customer Number using Next-Cust-Num Sequence */
ASSIGN Customer.Cust-Num =NEXT-VALUE(Next-Cust-Num).


/*
finds outstanding invoices of the customer and checks to see if they are paid.
If not, the trigger returns an error and the deletion fails.
If so, the trigger deletes all of the customer’s orders.
*/
TRIGGER PROCEDURE FOR DELETE OF customer.
FOR EACH invoice OF customer:
    IF invoice.amount > invoice.total-paid + invoice.adjustment THEN
    DO:
        MESSAGE "Outstanding unpaid invoice exists. Cannot delete.".
        RETURN ERROR.
    END.
END.
FOR EACH order OF customer:
DELETE order.
END.


//assign
TRIGGER PROCEDURE FOR ASSIGN OF customer.cust-num OLD VALUE oldcust.
FOR EACH order WHERE order.cust-num = oldcust:
    order.cust-num = customer.cust-num.
END.

//replication
TRIGGER PROCEDURE FOR REPLICATION-CREATE OF Customer.
CREATE Replication.
ASSIGN
Replication.Entry-Id = NEXT-VALUE(Replication-entry)
Replication.Table-Name = "Customer"
Replication.Task-Id = DBTASKID(LDBNAME(BUFFER Replication))
Replication.Repl-Event = "Create".
RAW-TRANSFER Customer TO Replication.Record.


TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Customer.
CREATE Replication.
ASSIGN
Replication.Entry-Id = NEXT-VALUE(Replication-entry)
Replication.Table-Name = "Customer"
Replication.Task-Id = DBTASKID(LDBNAME(BUFFER Replication))
Replication.Repl-Event = "Delete"
Replication.Key-Val = STRING(Customer.Cust-num).

/* custwrr.p */
TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Customer OLD BUFFER oldbuf.
CREATE Replication.
ASSIGN
Replication.Entry-Id = NEXT-VALUE(Replication-entry)
Replication.Table-Name = "Customer"
Replication.Task-Id = DBTASKID(LDBNAME(BUFFER Replication))
Replication.Repl-Event = "Write"
Replication.Key-Val = STRING(IF NEW(Customer) THEN Customer.Cust-num
ELSE oldbuf.Cust-num).
RAW-TRANSFER Customer TO Replication.Record.


//session
ON FIND OF customer DO:
FIND salesrep OF customer.
IF sales-rep NE userid THEN
RETURN ERROR.
END.

//This trigger tests whether you modified the Sales–rep field.
//If so, it adds a comment to the customer record.

ON WRITE OF customer OLD BUFFER ocust DO:
IF customer.sales-rep <> ocust.sales-rep
THEN DO:
FIND salesrep OF customer NO-LOCK.
customer.comments = customer.comments + " Salesrep changed to " +
salesrep.rep-name + " on " + STRING(TODAY).
END.
END.

/* Progress does not support this */
ON REPLICATION-WRITE OF customer DO:
    /*Some action */
END.

//create trigger is fired after create statement executed means After the customer record is created, craete trigger is fired.
//Delete trigger is fired when delete statement executed, firing time - trigger is executed before deletion of customer.if validation of delete trigger fails, record is not deleted.
//Assign trigger is executed on table.field not on tables.while write trigger is executed on table record.
//write trigger is executed before writing the record to the database so, we can do validations in write trigger block,if validation succeeds, record is written to database otherwise returns error.
//write trigger is executed when record is released, or end of procedure statement.
//whenever asssign trigger is executed , after that write trigger is also executed.
//"FIND triggers do not fire in response to the CAN-FIND function." However, when CAN-FIND is used with a ROWID in the query, then the FIND database trigger is still fired.
//Find trigger does not execute if find statement does not satisfy the full search condition i.e. where clause.
//All the session trigger executes before schema trigger but in case of find trigger, schema trigger executes before session trigger.
//There are no session triggers related to database replication.
//only find trigger executes on the statement of "disable triggers for load of customer".
//All session and schema triggers get disabled for that table for load option.
//only Find Trigger does not execute on the statement of "disable triggers for dump of customer".
/*if we check schema trigger overide as yes , then if we use override option in session trigger,then the session trigger functionality will override the functionality of schema trigger,
in other words, schema trigger will not execute, only session trigger will execute.*/
