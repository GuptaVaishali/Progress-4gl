CLASS Dog INHERITS Animal:

    METHOD PUBLIC OVERRIDE VOID displayInfo():
        SUPER:displayInfo().
        MESSAGE "Dog class" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END METHOD.
    
END CLASS.
