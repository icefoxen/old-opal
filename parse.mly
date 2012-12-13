%{
(* parse.mly
   Parser for... something.


*)

open Backend


%}

%token EOF
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE 
%token COLON 
%token METHOD CLASS ELSE END IF THEN ELIF
%token NULL
%token <int> INT
%token <string> SYMBOL
%token <char> CHAR
%token <string> COMMENT
%token <string> STRING
/*%token <float> FLOAT
%token <bool> BOOLEAN
*/

%type <Backend.syntree list> main
%start main

%%

main:  
	  /* EMPTY */ 
	  	{[]}
	| exprlist
	  	{$1}
	;


exprlist:
          expr
                { [$1] }
        | expr exprlist
                { $1 :: $2 }
        ;


expr:
          value
                { Value( $1 ) }
        | methodexpr
                { $1 }
        | classexpr
                { $1 }
        | callexpr
                { $1 }
        | ifexpr
                { $1 }
        | COMMENT
                { Comment( $1 ) }
        ;

value:
          INT
                { Int( $1 ) }
        | varexpr
                { Var( $1 ) }
/*
        | CHAR
                { Char( $1 ) }
        | STRING
                { String( $1 ) }
        | LBRACE RBRACE
                { Array( [||] ) }
        | LBRACE exprlist RBRACE
                { Array( (Array.of_list $2) ) }
*/
        ;


varexpr:
          SYMBOL
                { $1 }
        ;

callexpr:
          LPAREN expr expr exprlist RPAREN
                { Apply( $2, $3, $4 ) }
        | LPAREN expr expr RPAREN
                { Apply( $2, $3, [] ) }
        ;


methodexpr:
          METHOD varexpr varexpr arglist COLON exprlist END
                { MethodDef( $2, $3, $4, $6 ) }
        | METHOD varexpr varexpr COLON exprlist END
                { MethodDef( $2, $3, [], $5 ) }
        ;

classexpr:
          CLASS varexpr varexpr END
                { ClassDef( $2, $3, [] ) }
        | CLASS varexpr varexpr COLON arglist END
                { ClassDef( $2, $3, $5 ) }
        ;


arglist:
          SYMBOL
                { [$1] }
        | SYMBOL arglist
                { $1 :: $2 }
        ;

/*
lambdaexpr:
          FUN arglist COLON exprlist END
                { Lambda( $2, $4 ) }
        | FUN COLON exprlist END
                { Lambda( [], $3 ) }
        ;
*/


ifexpr:
          IF expr THEN exprlist END
                { If( $2, $4, [] ) }
        | IF expr THEN exprlist ELSE exprlist END
                { If( $2, $4, $6 ) }
        | IF expr THEN exprlist eliflist END
                { If( $2, $4, [$5] ) }
        ;

elifpart:
          ELIF expr THEN exprlist
                { ($2,$4) }
        ;

eliflist:
          elifpart
                {let t, b = $1 in If( t, b, [] )}
        | elifpart ELSE exprlist
                {let t, b = $1 in If( t, b, $3 )}
        | elifpart eliflist
                {let t, b = $1 in If( t, b, [$2] )}
        ;

%%

