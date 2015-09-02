%{
    (* Put OCaml helper functions here *)
    open Logoturtle
%}

%token <float> FLOAT
%token <string> ID
%token <string> PARAM
%token <bool>   BOOL
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token OR
%token AND
%token NOT
%token LESS
%token GREATER
%token EQUAL
%token NOTEQUAL
%token LESSEQUAL
%token GREATEREQUAL
%token IF
%token STOP
%token PENUP
%token PENDOWN
%token SETPENCOLOR
%token SETPENSIZE
%token TO
%token END
%token FORWARD
%token BACK
%token LEFT
%token RIGHT
%token REPEAT
%token REPCOUNT
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token EOF

%start <Logoturtle.command list> prog

(* the order of the following is important to define precedence *)
%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL NOTEQUAL
%nonassoc LESS LESSEQUAL GREATER GREATEREQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%%

prog:
  | EOF                       { [] }
  | cmds = command_list; EOF  { cmds }
  ;

command_list: cmds = rev_command_list { List.rev cmds }
rev_command_list:
  | c = command; { [c] }
  | cmds = rev_command_list; single_cmd = command; { single_cmd :: cmds }
    ;

expr:
  | b = base     { b }
  | a = arith    { a }
  | b = boolean  { b }

base:
  | REPCOUNT                          { Var "repcount" }
  | p = PARAM                         { Var p }
  | b = BOOL                          { Bool b }
  | f = FLOAT                         { Number f }
  | LEFT_PAREN; e = expr; RIGHT_PAREN { e }
  ;

arith:
  | e1 = expr; PLUS;   e2 = expr  { Plus  (e1, e2)  }
  | e1 = expr; MINUS;  e2 = expr  { Minus (e1, e2)  }
  | e1 = expr; TIMES;  e2 = expr  { Times (e1, e2)  }
  | e1 = expr; DIVIDE; e2 = expr  { Divide (e1, e2) }
  | MINUS; e = expr               { Negate e        } %prec TIMES
  ;

boolean:
  | NOT; e = expr;                     { Not e }
  | e1 = expr; LESS;         e2 = expr { Less (e1, e2) }
  | e1 = expr; GREATER;      e2 = expr { Greater (e1, e2) }
  | e1 = expr; LESSEQUAL;    e2 = expr { LessEq (e1, e2) }
  | e1 = expr; GREATEREQUAL; e2 = expr { GreaterEq (e1, e2) }
  | e1 = expr; EQUAL;        e2 = expr { Equal (e1, e2) }
  | e1 = expr; NOTEQUAL;     e2 = expr { NEqual (e1, e2) }
  | e1 = expr; OR;           e2 = expr { Or (e1, e2) }
  | e1 = expr; AND;          e2 = expr { And (e1, e2 ) }
  ;

command:
  |  STOP               { Stop }
  |  PENDOWN            { Pendown }
  |  PENUP              { Penup }
  |  SETPENCOLOR; e = expr { Setpencolor e }
  |  SETPENSIZE; e = expr { Setpensize e }
  |  FORWARD; e = expr  { Forward e }
  |  BACK; e = expr     { Back e }
  |  RIGHT; e = expr    { Right e }
  |  LEFT; e = expr     { Left  e }
  |  REPEAT; e = expr; LEFT_BRACKET; cmds = command_fields; RIGHT_BRACKET { Repeat (e, cmds) }
  |  name = ID; args = args_list; { Call (name, args) }
  |  TO; name = ID; params = params_list; cmds = command_fields; END { Proc (name, params, cmds) }
  |  IF; e = expr; LEFT_BRACKET; cmds = command_fields; RIGHT_BRACKET { If (e, cmds) }
  ;

command_fields: cmd = rev_command_fields { List.rev cmd };

rev_command_fields:
  | (* empty *) { [] }
  | cmds = rev_command_fields; c = command  { c :: cmds }

args_list: args = rev_args_list { List.rev args }
rev_args_list:
  | (* empty *) { [] }
  | args = rev_args_list; e = expr { e :: args }

params_list: params = rev_params_list { List.rev params }
rev_params_list:
  | (* empty *) { [] }
  | params = rev_params_list; p = PARAM { p :: params }
