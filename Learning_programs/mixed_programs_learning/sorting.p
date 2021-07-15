/*FOR EACH customer BY state:       //By sorts the records..it does not matter whether the field is indexed or not
    DISP cust-num state.
END.  */

/*FOR EACH customer USE-INDEX state:   //use-index also sorts the records but on the basis of indexed fields.
DISP cust-num state.                   //does not matter whether the field is primary index or not.
END.  */                              //it just should be index field..like in customer table we have 4 index fields.

/*FOR EACH customer BY NAME:
    DISP cust-num NAME.
END.  */

FOR EACH customer USE-INDEX NAME:
    DISP cust-num NAME.
END.

