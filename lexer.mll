{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     
  | ['.' ]        { EOL }
  | [',' ]        { COMMA }
  | '!'      { CUT }
  | "fail"  {FAIL}
  | '('                                                              { LP }
  | ')'                                                              { RP }
  | ":-"                                                             { IFF }
  | 'T'                                                              { BOOL (true) }
  | 'F'                                                              { BOOL (false) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as ided          { VAR (ided) }
  | ['a'-'z' '0'-'9' '[']['a'-'z' 'A'-'Z' '0'-'9' '_' ''' '|' ']' ';' '[']*    as ided          { ID (ided) }
| eof {EOF}