&GLOBAL-DEFINE Table customer
&GLOBAL-DEFINE Field name
&GLOBAL-DEFINE where-clause where {&Field} <> "vaishali gupta"

FIND NEXT {&Table} {&where-clause}.
DISPLAY {&Table}.
