/*
    PURPOSE - IMPORTING DATA FROM CUSTDATA.CSV FILE AND ROLLBACK DATA IN DB.
    CREATION DATE - 8-JUN-2021
    AUTHOR NAME - VAISHALI GUPTA
*/

/*************************DEFINING TEMP-TABLE************************************/ 
DEFINE TEMP-TABLE ttCustomer NO-UNDO FIELD custnum LIKE customer.cust-num VALIDATE
                                     FIELD country LIKE customer.country.

/****************IMPORT DATA FROM CUSTDATA.CSV FILE INTO TEMP-TABLE*****************/                                     
INPUT FROM D:\D&H_Project\rollbackScript\custdata.csv.

REPEAT: 
    CREATE ttCustomer.
    IMPORT DELIMITER "," ttCustomer.
END.

INPUT CLOSE.

/*********ROLLBACKING OF DATA INTO DB AND PUTTING ERRORS AND REQUIRED INFORMATION INTO IMPORTLOGFILE.TXT********/
OUTPUT TO D:\D&H_Project\rollbackScript\importlogfile.txt.
DEFINE VARIABLE viCount AS INTEGER NO-UNDO.

PUT "LAST RUN DATE :" SPACE(1) TODAY SKIP.
PUT "LAST RUN TIME :" SPACE(1) STRING(TIME,"HH:MM:SS") SKIP.
PUT "File Name :" SPACE(1) "importData.p" SKIP(2).

FOR EACH ttCustomer NO-LOCK:
    FIND customer WHERE customer.cust-num = ttCustomer.custnum EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE customer THEN
        IF LOCKED customer THEN
            PUT "CUSTOMER" ttCustomer.custnum SPACE(1) "IS LOCKED BY ANOTHER USER".
        ELSE
            PUT "CUSTOMER NO" ttCustomer.custnum SPACE(1) "RECORD IS NOT AVAILABLE".
    ELSE
    DO:
        customer.country = ttCustomer.country.
        PUT "CUSTOMER NO" customer.cust-num SPACE(1) "HAS BEEN ROLLBACKED FROM CANADA TO USA" SKIP.
        viCount = viCount + 1.
    END.     
END.     
PUT viCount AT 1 SPACE(1) "RECORDS HAS BEEN UPDATED SUCCESSFULLY".
MESSAGE viCount "RECORDS HAS BEEN ROLLBACKED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  
OUTPUT CLOSE.

