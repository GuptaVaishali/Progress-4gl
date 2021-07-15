FOR EACH customer:
    FOR EACH order OF customer:
        IF AVAILABLE order THEN
        DO:
             DISPLAY order.
        END.
        ELSE
            MESSAGE "order not available" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
END.
