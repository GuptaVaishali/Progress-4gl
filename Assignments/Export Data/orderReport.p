DEFINE INPUT PARAMETER piLCustNum LIKE customer.cust-num NO-UNDO.
DEFINE INPUT PARAMETER piHCustNum LIKE customer.cust-num NO-UNDO.

DEFINE INPUT PARAMETER pcFileType  AS   CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pdStartDate AS   DATE      NO-UNDO.
DEFINE INPUT PARAMETER pdEndDate   AS   DATE      NO-UNDO.

DEFINE VARIABLE lOnScreen AS LOGICAL NO-UNDO.

MESSAGE piLCustNum piHCustNum pcFileType pdStartDate pdEndDate VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
IF pcFileType = "pdf" THEN 
    OUTPUT TO D:\ExportData\report.pdf.
ELSE IF pcFileType = "excel" THEN
    OUTPUT TO D:\ExportData\report.csv.
ELSE IF pcFileType = "text file" THEN
    OUTPUT TO D:\ExportData\report.txt.
ELSE
DO:
   OS-COMMAND NO-WAIT "D:\ExportData\report.txt". 
   lOnScreen = TRUE.
END.
   
IF lOnScreen = FALSE THEN
DO:   
   FOR EACH customer WHERE cust-num GE piLCustNum AND cust-num LE piHCustNum:
        PUT "/************* Customer Information *****************/" SKIP(1). 
        EXPORT DELIMITER "," "Cust-Num" "Name" "Country" "Address" "City" "State" "Phone".
        EXPORT DELIMITER "," customer.cust-num customer.NAME customer.country customer.address customer.city customer.state customer.phone. 
        
        FOR EACH order WHERE order.cust-num = customer.cust-num AND order-date GE pdStartDate AND order-date LE pdEndDate:
             PUT SKIP(1) "/************ Order Information ********************/" SKIP(1). 
             
             EXPORT DELIMITER "," "order-num" "cust-num" "order-date" "ship-date" "promise-date" "carrier" "Instructions" "PO" "Terms" "Sales-Rep". 
             EXPORT DELIMITER "," order.order-num order.cust-num order.order-date order.ship-date order.promise-date order.carrier order.Instructions 
                      order.PO order.Terms order.Sales-rep.
                      
             PUT SKIP(1) "/*********** OrderLine Information **************/" SKIP(1).  
             EXPORT DELIMITER "," "Order-Num" "Line-Num" "Price" "Qty" "Discount" "Extended-Price" "BackOrder".
             
             FOR EACH order-line WHERE order-line.order-num = order.order-num:
                    EXPORT DELIMITER "," order-line.order-num order-line.line-num order-line.item-num order-line.price order-line.qty order-line.discount
                           order-line.extended-price order-line.backOrder.
             END.
        END.         
       PUT SKIP(2).   
   END. 
END.
