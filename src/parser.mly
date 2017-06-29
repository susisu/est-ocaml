%token <float> NUM
%token <string> ID
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COMMA
%token LET
%token EQUAL
%token IN
%token LEFT_PAREN
%token RIGHT_PAREN
%token EOF

%start <unit Term.t> toplevel

%%

number:
  num = NUM { Term.Lit ((), Value.Num num) }

variable:
  name = ID { Term.Var ((), name) }

vector:
  LEFT_BRACKET; elems = separated_list(COMMA, term); RIGHT_BRACKET { Term.Vec ((), elems) }


aterm:
  | t = number                        { t }
  | t = variable                      { t }
  | t = vector                        { t }
  | LEFT_PAREN; t = term; RIGHT_PAREN { t }

application:
  | t = aterm                       { t }
  | func = application; arg = aterm { Term.App ((), func, arg) }


binding:
  LET; name = ID; EQUAL; expr = term; IN; body = term { Term.Let ((), name, expr, body) }


term:
  | t = application { t }
  | t = binding     { t }

toplevel:
  t = term; EOF { t }
