DEFINE VARIABLE line-data AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE temp-cust NO-UNDO LIKE Customer.
DEFINE VARIABLE read-data AS INT NO-UNDO.
DEFINE VARIABLE last-custNum  LIKE customer.custNum NO-UNDO.
DEFINE INPUT PARAMETER fill_In_Path AS CHARACTER .


INPUT FROM value(fill_In_Path).

IMPORT line-data.




IF line-data = "CustNum,Country,Name,Address,Address2,City,State,PostalCode,Contact,Phone,SalesRep,CreditLimit,Balance,Terms,Discount,Comments,Fax,EmailAddress"
THEN 
DO:
    REPEAT:
                  
         IMPORT UNFORMATTED line-data. // temp-cust.CustNum temp-cust.Country temp-cust.NAME temp-cust.Address temp-cust.Address2 temp-cust.City temp-cust.State temp-cust.PostalCode temp-cust.Contact temp-cust.Phone temp-cust.SalesRep temp-cust.CreditLimit temp-cust.Balance temp-cust.Terms temp-cust.Discount temp-cust.Comments temp-cust.Fax temp-cust.EmailAddress.
         IF ENTRY(3,line-data) NE "" THEN
         DO:
                 
                  CREATE temp-cust.
                  
                  ASSIGN temp-cust.custNum = INT(ENTRY(1,line-data))    
                   temp-cust.Country = (ENTRY(2,line-data))        
                   temp-cust.NAME  = (ENTRY(3,line-data))          
                   temp-cust.Address  = (ENTRY(4,line-data))       
                   temp-cust.Address2 = (ENTRY(5,line-data))       
                   temp-cust.City = (ENTRY(6,line-data))           
                   temp-cust.State  = (ENTRY(7,line-data))         
                   temp-cust.PostalCode  = (ENTRY(8,line-data))    
                   temp-cust.Contact = (ENTRY(9,line-data))     
                   temp-cust.Phone = (ENTRY(10,line-data))      
                   temp-cust.SalesRep  = (ENTRY(11,line-data))     
                   temp-cust.CreditLimit = DECIMAL(ENTRY(12,line-data))
                   temp-cust.Balance = DECIMAL(ENTRY(13,line-data))   
                   temp-cust.Terms = (ENTRY(14,line-data))            
                   temp-cust.Discount = DECIMAL(ENTRY(15,line-data))   
                   temp-cust.Comments = (ENTRY(16,line-data))          
                   temp-cust.Fax = (ENTRY(17,line-data))               
                  temp-cust.EmailAddress = (ENTRY(18,line-data)).
         END.
    END.
END.
ELSE
DO:
    MESSAGE "File Format Is Not Correct" VIEW-AS ALERT-BOX.
    ASSIGN READ-DATA = 1.
END.
INPUT CLOSE.

IF read-data = 0 THEN
DO:
   FOR EACH temp-cust : 
         
        IF temp-cust.NAME NE "" THEN
        DO:
        
              IF CAN-FIND(FIRST customer WHERE customer.custNum = temp-cust.CustNum) THEN
              DO:
                FIND  customer EXCLUSIVE-LOCK WHERE customer.CustNum = temp-cust.CustNum.
                ASSIGN  
                        customer.Country = temp-cust.Country
                        customer.Name = temp-cust.Name
                        customer.Address = temp-cust.Address
                        customer.Address2 = temp-cust.Address2
                        customer.City = temp-cust.City
                        customer.State = temp-cust.State
                        customer.PostalCode = temp-cust.PostalCode
                        customer.Contact = temp-cust.Contact
                        customer.Phone = temp-cust.Phone
                        customer.SalesRep = temp-cust.SalesRep
                        customer.CreditLimit = temp-cust.CreditLimit
                        customer.Balance = temp-cust.Balance
                        customer.Terms = temp-cust.Terms
                        customer.Discount = temp-cust.Discount
                        customer.Comments = temp-cust.Comments
                        customer.Fax = temp-cust.Fax
                        customer.EmailAddress = temp-cust.EmailAddress.
                        MESSAGE "Record Updated with Cust Num: " STRING(customer.custNum) VIEW-AS ALERT-BOX.
              END.
              ELSE
              DO:   
                    IF (temp-cust.custNum) = 0 THEN
                    DO:
                          FIND LAST customer NO-LOCK NO-ERROR.
                          IF AVAILABLE(customer) THEN
                            last-custNum=customer.custNum + 1.
                           ELSE
                            last-custNum = 1.
                          
                    END.
                    ELSE
                        last-custNum = temp-cust.custNum.
                    CREATE customer.
                    ASSIGN
                    customer.CustNum = last-custNum
                    customer.Country = temp-cust.Country
                    customer.Name = temp-cust.Name
                    customer.Address = temp-cust.Address
                    customer.Address2 = temp-cust.Address2
                    customer.City = temp-cust.City
                    customer.State = temp-cust.State
                    customer.PostalCode = temp-cust.PostalCode
                    customer.Contact = temp-cust.Contact
                    customer.Phone = temp-cust.Phone
                    customer.SalesRep = temp-cust.SalesRep
                    customer.CreditLimit = temp-cust.CreditLimit
                    customer.Balance = temp-cust.Balance
                    customer.Terms = temp-cust.Terms
                    customer.Discount = temp-cust.Discount
                    customer.Comments = temp-cust.Comments
                    customer.Fax = temp-cust.Fax
                    customer.EmailAddress = temp-cust.EmailAddress.
                    MESSAGE "Record Added, with cust num: " STRING(customer.custNum) VIEW-AS ALERT-BOX.
              END.
        END.
     END.                
END.
            
