DISPLAY "Display something" WITH FRAME aaa.
REPEAT WITH FRAME bbb:
    PROMPT-FOR customer.cust-num WITH FRAME aaa.
    FIND customer USING cust-num.
    DISPLAY cust-num NAME.        //use frame bbb
END.
