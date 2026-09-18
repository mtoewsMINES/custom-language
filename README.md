**Overview**\
This is a recursive descent parser built to interpret and evaluate programs written in a language defined by the following custom grammar:

```
<prog> -> 
    | <stmt> <prog>
    | "{" <prog> "}"
    | ""
<stmt> ->
    | <ptype> <ident> "->" <exp> ";"
    | <ident> "->" <exp> ";"
    | "if" <exp> <prog> "else" <prog>
    | "if" <exp> <prog>
    | "print" <exp>
    | <ident> "." "append" "(" <exp> ")"
    | <ident> "." "reverse" "(" ")"
    | "while" <exp> <prog>
    | "func <ident> "(" <param> ")" <prog>
    | <ident> "(" <list> ")"
    | <exp>
<exp> ->
    | <bop>
<bop> ->
    | <term> <bop'>
<bop'> ->
    | "+" <term> <bop'>
    | "-" <term> <bop'>
    | "||" <term> <bop'>
    | ""
<term> ->
    | <factor> <term'>
<term'> ->
    | "*" -> <factor> <term'>
    | "/" -> <factor> <term'>
    | "&&" -> <factor> <term'>
    | ""
<factor> ->
    | "(" <exp> ")"
    | <ident>
    | <ident> "[" int "]"
    | <ident> "." "length" "(" ")"
    | "[" <list> "]"
    | <literal>
    | <uop>
<uop> ->
    | "!" <exp>
<list> ->
    | <literal> "," <list>
    | <literal>
    | ""
<literal> ->
    | int | string | bool
<param> ->
    | <ptype> <ident> "," <param>
    | <ptype> <ident>
    | ""

<ptype> -> "int" <adt> | "string" <adt> | "bool" <adt>
<adt> -> "list" | ""
<ident> -> alphanumeric characters
```
