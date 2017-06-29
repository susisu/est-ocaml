%{
open Term
%}

%token <float> NUM
%token <string> ID
%token LBRACKET RBRACKET COMMA
%token PLUS MINUS
%token TIMES FRAC MOD
%token POWER CARET
%token EXCL
%token LET EQUAL IN
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS
%left TIMES FRAC MOD
%left POWER CARET
%left EXCL
%nonassoc UNARY

%start <unit Term.t> toplevel

%%

num:
  num = NUM { Lit ((), Value.Num num) }

var:
  name = ID { Var ((), name) }

vec:
  LBRACKET; elems = separated_list(COMMA, term); RBRACKET { Vec ((), elems) }


aterm:
  | t = num                  { t }
  | t = var                  { t }
  | t = vec                  { t }
  | LPAREN; t = term; RPAREN { t }

app:
  | t = aterm               { t }
  | func = app; arg = aterm { App ((), func, arg) }


op:
  | t = app                      { t }
  | left = op; PLUS;  right = op { App ((), App ((), Var ((), "_+_"),  left), right) }
  | left = op; MINUS; right = op { App ((), App ((), Var ((), "_-_"),  left), right) }
  | left = op; TIMES; right = op { App ((), App ((), Var ((), "_*_"),  left), right) }
  | left = op; FRAC;  right = op { App ((), App ((), Var ((), "_/_"),  left), right) }
  | left = op; MOD;   right = op { App ((), App ((), Var ((), "_%_"),  left), right) }
  | left = op; POWER; right = op { App ((), App ((), Var ((), "_**_"), left), right) }
  | left = op; CARET; right = op { App ((), App ((), Var ((), "_^_"),  left), right) }
  | left = op; EXCL;  right = op { App ((), App ((), Var ((), "_!_"),  left), right) }
  | PLUS;  arg = op %prec UNARY  { App ((), Var ((), "+_"), arg) }
  | MINUS; arg = op %prec UNARY  { App ((), Var ((), "-_"), arg) }


bind:
  LET; name = ID; EQUAL; expr = term; IN; body = term { Let ((), name, expr, body) }


term:
  | t = op   { t }
  | t = bind { t }

toplevel:
  t = term; EOF { t }
