%{
    (* Put OCaml helper functions here *)
    open Logoturtle
%}

%token <float> FLOAT
%token <string> ID
%token <string> PARAM
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token FORWARD
%token RIGHT
%token LEFT
%token REPEAT
%token TO
%token END
%token EOF

%start <Logoturtle.command list> prog
%%

prog:
  | EOF                       { [] }
  | cmds = command_list; EOF  { cmds }
  ;

command_list: cmds = rev_command_list { List.rev cmds }
rev_command_list:
  | c = command; { [c] }
  | cmds = command_list; cmd = command; { cmd :: cmds }
    ;

value:
  | x = FLOAT  { Number x }
  | p = PARAM  { Var p   }

command:
  |  FORWARD; v = value  { Forward v }
  |  RIGHT; v = value    { Right v }
  |  LEFT; v = value     { Left  v }
  |  REPEAT; i = value; LEFT_BRACKET; cmd = command_fields; RIGHT_BRACKET { Repeat (i, cmd) }
  |  name = ID; args = args_list; { Call (name, args) }
  |  TO; name = ID; params = params_list; cmds = command_fields; END { Proc (name, params, cmds) }
  ;

command_fields: cmd = rev_command_fields { List.rev cmd };

rev_command_fields:
  | (* empty *) { [] }
  | cmds = rev_command_fields; c = command  { c :: cmds }

args_list: args = rev_args_list { List.rev args }
rev_args_list:
  | (* empty *) { [] }
  | args = rev_args_list; v = value { v :: args }

params_list: params = rev_params_list { List.rev params }
rev_params_list:
  | (* empty *) { [] }
  | params = rev_params_list; p = PARAM { p :: params }
