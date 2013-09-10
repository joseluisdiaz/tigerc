/*
 * A Parser for tiger grammar in bison.
 */
%{
package tiger.parser
import tiger.Tree._

%}


%token YYEOF
%token<String> LITERAL ID
%token<Integer> NRO
%token TYPE ARRAY OF VAR FUNCTION
%token LET IN END IF THEN ELSE WHILE DO FOR TO BREAK
%token PTO DOSP DOSPIG COMA PCOMA IGUAL PI PD CI CD LI LD
%token AMPER PIPE MENOR MENIG MAYOR MAYIG DIST
%token MAS MENOS POR DIV NIL

%type<Exp> exp
%type<Exp> prog
%type<Ty> ty
%type<String> id
%type<Field> tyfield
%type<List[Field]> tyflds
%type<List[Exp]> explist
%type<List[(Symbol, Exp)]> rec_fields 
%type<List[Exp]> args 
%type<Var> l_value
%type<Dec> dec
%type<Dec> vardec
%type<Dec> fundec 
%type<List[Dec]> decs 

%nonassoc THEN
%left ELSE
%nonassoc DOSPIG
%left PIPE
%left AMPER
%nonassoc IGUAL MENOR MENIG MAYOR MAYIG DIST
%left MAS MENOS
%left POR DIV
%nonassoc DO
%nonassoc OF

%start prog

%%

prog: exp YYEOF      { $$ = $1; }
    ;

exp : NRO                               { $$ = IntExp($1, line); }
    | PI PD                             { $$ = UnitExp(line); }
    | NIL                               { $$ = NilExp(line); }
    | LITERAL                           { $$ = StringExp($1, line); }
    | BREAK                             { $$ = BreakExp(line); }
    | l_value                           { $$ = VarExp($1, line); }
    | l_value DOSPIG exp                { $$ = AssignExp($1, $3, line); }
    | PI exp PCOMA explist PD           { $$ = SeqExp( $2 :: $4, line); }
    | exp PIPE exp                      { $$ = IfExp($1, IntExp(1, line), Some($3), line); }
    | exp AMPER exp                     { $$ = IfExp($1, $3, Some(IntExp(0, line)), line); }
    | exp IGUAL exp			{ $$ = OpExp($1, EqOp(), $3, line); }
    | exp MENOR exp			{ $$ = OpExp($1, LtOp(), $3, line); }
    | exp MENIG exp			{ $$ = OpExp($1, LeOp(), $3, line); }
    | exp MAYOR exp			{ $$ = OpExp($1, GtOp(), $3, line); }
    | exp MAYIG exp			{ $$ = OpExp($1, GeOp(), $3, line); }
    | exp DIST exp			{ $$ = OpExp($1, NeqOp(), $3, line); }
    | exp MAS exp			{ $$ = OpExp($1, PlusOp(), $3, line); }
    | exp MENOS exp			{ $$ = OpExp($1, MinusOp(), $3, line); }
    | exp POR exp			{ $$ = OpExp($1, TimesOp(), $3, line); }
    | exp DIV exp			{ $$ = OpExp($1, DivideOp(), $3, line); }
    | MENOS exp				{ $$ = OpExp(IntExp(0, line), MinusOp(), $2, line); }
    | PI exp PD                         { $$ = $2; }
    | id PI args PD	                { $$ = CallExp($1, $3, line); }
    | IF exp THEN exp	                { $$ = IfExp($2, $4, None, line); }
    | IF exp THEN exp ELSE exp          { $$ = IfExp($2, $4, Some($6), line); }
    | WHILE exp DO exp	                { $$ = WhileExp($2, $4, line); }
    | FOR id DOSPIG exp TO exp DO exp   { $$ = ForExp($2, false, $4, $6, $8, line); }
    | LET decs IN END		        { $$ = LetExp($2, UnitExp(line), line); }
    | LET decs IN exp END	        { $$ = LetExp($2, $4, line); }
    | LET decs IN exp PCOMA explist END { $$ = LetExp($2, SeqExp($4::$6, line), line); }
    | l_value CI exp CD OF exp		{ $$ = ArrayExp(nombre($1), $3, $6, line); }
    | id LI rec_fields LD	        { $$ = RecordExp($3, $1, line); }
    ;

explist: exp PCOMA explist	{ $$ = $1::$3; }
       | exp			{ $$ = List($1); }
       ;

rec_fields : id IGUAL exp COMA rec_fields { $$ = ($1, $3) :: $5 ; }
           | id IGUAL exp		  { $$ = List( ($1, $3) ); }
           |				  { $$ = Nil; }	
	   ;

decs : dec decs				{ $$ = $1 :: $2; }
     |					{ $$ = Nil; }
     ;

dec : TYPE id IGUAL ty		        { $$ = TypeDecs(List(TypeDec($2, $4, line))); }
    | vardec				{ $$ = $1; }
    | fundec				{ $$ = $1; }
    ;

ty : id					{ $$ = NameTy($1); }		
   | LI tyflds LD			{ $$ = RecordTy($2); }
   | ARRAY OF id			{ $$ = ArrayTy($3); }
   ;

id : ID					{ $$ = $1; }
   ;

tyflds : tyfield COMA tyflds            { $$ = $1 :: $3; }
       | tyfield			{ $$ = List($1); }
       |			        { $$ = Nil; }
       ;

vardec : VAR id DOSPIG exp              { $$ = VarDec($2, false, None, $4, line); }
       | VAR id DOSP id DOSPIG exp	{ $$ = VarDec($2, false, Some($4), $6, line); }
       ;

fundec : FUNCTION id PI tyflds PD IGUAL exp         { $$ = FunctionDecs(List(FunctionDec($2, $4, None, $7, line))); }
       | FUNCTION id PI tyflds PD DOSP id IGUAL exp { $$ = FunctionDecs(List(FunctionDec($2, $4, Some($7), $9, line))); }
       ;

tyfield : id DOSP id		{ $$ = Field($1, false, NameTy($3)); }
	;

args : exp COMA args		{ $$ = $1 :: $3; }
     | exp			{ $$ = List($1); }
     |				{ $$ = List(); }
     ;

l_value : id				{ $$ = SimpleVar($1); }
	| l_value PTO id		{ $$ = FieldVar($1, $3); }
	| l_value CI exp CD		{ $$ = SubscriptVar($1, $3); }
	;

%%

  def fundeLFunTipos(d: Dec, p: List[Dec]) = (d, p) match {
  case (TypeDecs(List(dt)), TypeDecs(hdt)::t ) => TypeDecs(dt::hdt) :: t
  case (FunctionDecs(List(dt)), FunctionDecs(hdt)::t ) => FunctionDecs(dt :: hdt)::t
  case (d1, dl) => d1::dl 
  }
	

  def nombre(s:Var) = s match {
    case SimpleVar(x) => x
    case _ => throw new Error("Imposible que no sea SimpleVar!")
  }
   
  def yyerror(s:String) = println(s);
  
  def line(): Int = scanner.getLineNumber()

  var scanner : TigerScanner = null

  def createScanner(in: java.io.InputStream) = {
    scanner = new TigerScanner(in);
    yyreset(scanner);
  }

