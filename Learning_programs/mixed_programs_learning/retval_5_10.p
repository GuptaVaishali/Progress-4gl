DEFINE VARIABLE i AS INTEGER.
FORM
customer.cust-num customer.name
WITH FRAME name-frame DOWN.
FIND FIRST customer.
DISPLAY cust-num name WITH FRAME name-frame.
DO i = cust-num + 1 TO CURRENT-VALUE(next-cust-num):                               
DOWN WITH FRAME name-frame.
RUN retstr_5_10.p (BUFFER customer, i).
DISPLAY i @ customer.cust-num         
RETURN-VALUE @ customer.name WITH FRAME name-frame.
END.
