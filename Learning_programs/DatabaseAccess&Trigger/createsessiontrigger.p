ON CREATE OF customer DO:
    MESSAGE "create session trigger"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

CREATE customer. //first create session trigger then create schema trigger
