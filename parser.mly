%{
    (* Put OCaml helper functions here *)
%}

%token <int> INT
%token <float> FLOAT
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token FORWARD
%token TURN
%token REPEAT
%token EOF

%start <Logoturtle.command option> prog
%%

prog:
  | EOF         { None }
  | c = command { Some c }
  ;


command:
  |  TURN; x = FLOAT     { Turn x }
  |  FORWARD; x = FLOAT  { Forward x }
  |  REPEAT; i = INT; LEFT_BRACKET; cmd = command_fields; RIGHT_BRACKET { Repeat (i, cmd) }
  ;

command_fields: cmd = rev_command_fields { List.rev cmd };

rev_command_fields:
  | (* empty *) { [] }
  | cmds = rev_command_fields; c = command  { c :: cmds }
