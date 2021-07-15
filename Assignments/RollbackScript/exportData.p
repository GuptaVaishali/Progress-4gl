/*
    PURPOSE - EXPORTING DATA INTO CUSTDATA.CSV FILE AND CHANGE CUSTOMER CITY FROM "USA" TO "CANADA" IN DB.
    CREATION DATE - 8-JUN-2021
    AUTHOR NAME - VAISHALI GUPTA
*/

/*********EXPORTING DATA FROM DB INTO CUSTDATA.CSV FILE****************/
OUTPUT TO D:\D&H_Project\rollbackScript\custdata.csv.
DEFINE BUFFER buCustomer FOR customer.

FOR EACH customer WHERE country = "usa" NO-LOCK:
    EXPORT DELIMITER "," customer.cust-num customer.country.
END.

OUTPUT CLOSE.

/**********CHANGING CONTENT OF CUSTOMER CITY FROM "USA" TO "CANADA" AND STROING ERRORS AND REQUIRED INFORMATION INTO "EXPORTLOGFILE.TXT" FILE***********/
OUTPUT TO D:\D&H_Project\rollbackScript\exportlogfile.txt.  
DEFINE VARIABLE viCount AS INTEGER NO-UNDO.

PUT "LAST RUN DATE :" SPACE(1) TODAY SKIP.
PUT "LAST RUN TIME :" SPACE(1) STRING(TIME,"HH:MM:SS") SKIP.
PUT "File Name :" SPACE(1) "exportData.p" SKIP(2).

FOR EACH customer WHERE customer.country = "usa" NO-LOCK:
    FIND buCustomer WHERE ROWID(buCustomer) = ROWID(customer) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE buCustomer THEN DO:
        IF LOCKED buCustomer THEN
            PUT "CUSTOMER" customer.cust-num SPACE(1) "IS LOCKED BY ANOTHER USER".
    END.
    ELSE DO:
        buCustomer.country = "canada".
        PUT "CUSTOMER NO" customer.cust-num SPACE(1) "HAS BEEN UPDATED FROM USA TO CANADA" SKIP.
        viCount = viCount + 1.
    END.
END.
PUT viCount AT 1 SPACE(1) "RECORDS HAS BEEN UPDATED SUCCESSFULLY".
MESSAGE viCount "RECORDS HAS BEEN UPDATED SUCCESSFULLY" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

OUTPUT CLOSE.  


