%{
  open Assignment6
%}

%token LP RP EOL IFF COMMA EOF CUT FAIL LSQ RSQ CON
%token <bool> BOOL
%token <string> VAR
%token <string> ID
%start main             /* the entry point */
%type <Assignment6.clause> main
%%
main:
    EOL {(Node("male",[(Node("john",[]))]),[])}
    |clausee EOL { $1 }
    | EOF {(Node("file_end",[]),[])}
;
clausee:
    term { ($1,[]) }
    | term IFF termlist { ($1,$3) }
;

term:
    ID { Node($1,[])}
    | VAR { V($1) }
    | CUT {Cut}
    | FAIL {Fail}
    | ID LP termlist RP { Node($1,$3)}

termlist:
     term     { [$1] }
    | term COMMA termlist { $1::$3 }
;