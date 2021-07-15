/*
    PURPOSE       - EXPORTING WEEKLY BASIS ORDER DETAILS.
    CREATION DATE - 14-JULY-2021
    AUTHOR NAME   - VAISHALI GUPTA
*/

/*********EXPORTING DATA FROM DB INTO REPORT.CSV FILE****************/

OUTPUT TO D:\DNH_Project\TaskScheduler\report.csv.

 /************************* HEADER STATEMENT *********************************/
//    EXPORT DELIMITER "," "order-num" "cust-num" "order-date" "ship-date" "promise-date" "carrier" "Instructions" "PO" "Terms" "Sales-Rep".

FOR EACH order WHERE order-date > TODAY - 7:
    PUT "************ Order Information ********************" SKIP(1). 

    /************************* HEADER STATEMENT *********************************/
    EXPORT DELIMITER "," "order-num" "cust-num" "order-date" "ship-date" "promise-date" "carrier" "Instructions" "PO" "Terms" "Sales-Rep".
              
    EXPORT DELIMITER "," order.order-num order.cust-num order.order-date order.ship-date order.promise-date order.carrier order.Instructions 
          order.PO order.Terms order.Sales-rep.
          
    PUT SKIP(1) "*********** OrderLine Information **************" SKIP(1).  
    EXPORT DELIMITER "," "Order-Num" "Line-Num" "Price" "Qty" "Discount" "Extended-Price" "BackOrder".
     
    FOR EACH order-line WHERE order-line.order-num = order.order-num:
           EXPORT DELIMITER "," order-line.order-num order-line.line-num order-line.item-num order-line.price order-line.qty order-line.discount
                   order-line.extended-price order-line.backOrder.
    END.
    PUT SKIP(1).   
      
END.   

OUTPUT CLOSE.
