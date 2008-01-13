type variant
    = VFunction of (variant -> variant)
    | VList of variant list
    | VString of string
    | VNumber of float
    | VUnit;;

