%{
open Term
%}

%token <Lexing.position * float> NUM
%token <Lexing.position * string> ID
%token <Lexing.position> LBRACKET RBRACKET COMMA
%token <Lexing.position> PLUS MINUS
%token <Lexing.position> TIMES FRAC MOD
%token <Lexing.position> POWER CARET
%token <Lexing.position> APPEND EXCL
%token <Lexing.position> LET EQUAL IN
%token <Lexing.position> LPAREN RPAREN
%token <Lexing.position> EOF

%left PLUS MINUS
%left TIMES FRAC MOD
%left POWER CARET
%left APPEND
%nonassoc UNARY
%left EXCL

%start <Lexing.position Term.t> toplevel

%%

num:
  i = NUM { Lit (fst i, Value.Num (snd i)) }

var:
  i = ID { Var (fst i, snd i) }

vec:
  p = LBRACKET; elems = separated_list(COMMA, term); RBRACKET { Vec (p, elems) }


aterm:
  | t = num                            { t }
  | t = var                            { t }
  | t = vec                            { t }
  | LPAREN; t = term; RPAREN           { t }
  | lhs = aterm; p = EXCL; rhs = aterm { App (p, App (p, Var (p, "_!_"),  lhs), rhs) }

app:
  | t = aterm               { t }
  | func = app; arg = aterm { App (get_info func, func, arg) }


op:
  | t = app                         { t }
  | lhs = op; p = PLUS;  rhs = op   { App (p, App (p, Var (p, "_+_"),  lhs), rhs) }
  | lhs = op; p = MINUS; rhs = op   { App (p, App (p, Var (p, "_-_"),  lhs), rhs) }
  | lhs = op; p = TIMES; rhs = op   { App (p, App (p, Var (p, "_*_"),  lhs), rhs) }
  | lhs = op; p = FRAC;  rhs = op   { App (p, App (p, Var (p, "_/_"),  lhs), rhs) }
  | lhs = op; p = MOD;   rhs = op   { App (p, App (p, Var (p, "_%_"),  lhs), rhs) }
  | lhs = op; p = POWER; rhs = op   { App (p, App (p, Var (p, "_**_"), lhs), rhs) }
  | lhs = op; p = CARET; rhs = op   { App (p, App (p, Var (p, "_^_"),  lhs), rhs) }
  | lhs = op; p = APPEND; rhs = op  { App (p, App (p, Var (p, "_@_"),  lhs), rhs) }
  | p = PLUS;  arg = op %prec UNARY { App (p, Var (p, "+_"), arg) }
  | p = MINUS; arg = op %prec UNARY { App (p, Var (p, "-_"), arg) }


bind:
  p = LET; i = ID; EQUAL; expr = term; IN; body = term { Let (p, snd i, expr, body) }


term:
  | t = op   { t }
  | t = bind { t }

toplevel:
  t = term; EOF { t }
