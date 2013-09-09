package tiger.parser

import tiger.parser.TigerTokens._

%%

%class TigerScanner
%type TigerTokens.YYToken
%implements Iterator[TigerTokens.YYToken]

%line

%{

  // These features are added to the Scanner class
  var lookahead : TigerTokens.YYToken = null;
   
  override def hasNext() : Boolean = { 
    if (lookahead == null) lookahead = yylex();
    lookahead match {
      case x:TigerTokens.YYEOF => false;
      case x:TigerTokens.YYToken => true;
    }
  };
  
  override def next() : TigerTokens.YYToken = {
    if (lookahead == null) lookahead = yylex();
    var result : TigerTokens.YYToken = lookahead;
    lookahead = null;
    result
  };
  
  def getLineNumber() : Int = yyline+1;

  var tigerStr = new StringBuffer();

  var ncom = 0


%}

SPC = [ \t\r]
MN = [a-z]
L = [A-Za-z]
LDU = [A-Za-z0-9_]
D = [0-9]
   
NEWLINE = [\n]

%state STRING COM

%%

   /*  Tokens */
<YYINITIAL> {
  "/*"                          { ncom = 1; yybegin(COM); }
  \"                            { tigerStr.setLength(0); yybegin(STRING); }
  
  {D}+                          { return NRO(Integer.parseInt(yytext)) }
  "."                           { return PTO() }
  ":"                           { return DOSP() }
  ":="                          { return DOSPIG() }
  ","                           { return COMA() }
  ";"                           { return PCOMA() }
  "="                           { return IGUAL() }
  "("                           { return PI() }
  ")"                           { return PD() }
  "["                           { return CI() }
  "]"                           { return CD() }
  "{"                           { return LI() }
  "}"                           { return LD() }
  "&"                           { return AMPER() }
  "|"                           { return PIPE() }
  "<"                           { return MENOR() }
  "<="                          { return MENIG() }
  ">"                           { return MAYOR() }
  ">="                          { return MAYIG() }
  "<>"                          { return DIST() }
  "+"                           { return MAS() }
  "-"                           { return MENOS() }
  "*"                           { return POR() }
  "/"                           { return DIV() }

  /* keywords */
  "type"                        { return  TYPE()     }
  "array"                       { return  ARRAY()    }
  "of"                          { return  OF()       }
  "var"                         { return  VAR()      }
  "function"                    { return  FUNCTION() }
  "let"                         { return  LET()      }
  "in"                          { return  IN()       }
  "end"                         { return  END()      }
  "if"                          { return  IF()       }
  "then"                        { return  THEN()     }
  "else"                        { return  ELSE()     }
  "while"                       { return  WHILE()    }
  "do"                          { return  DO()       }
  "for"                         { return  FOR()      }
  "to"                          { return  TO()       }
  "break"                       { return  BREAK()    }
  "nil"                         { return  NIL()      }

  {MN}+                         { return ID(yytext) }
  {L} {LDU}*                    { return ID(yytext) }

  {SPC}+                        {                }
  {NEWLINE}                     { /* Ignorar */  }
  <<EOF>>                       { return TigerTokens.YYEOF(); }
}

<STRING> {
  <<EOF>>                       { throw new Error("String incompleto") }
  \"                            { yybegin(YYINITIAL);
                                   return TigerTokens.LITERAL(tigerStr.toString()); }
  [^\n\r\"\\]+                  { tigerStr.append( yytext ); }
  \\t                           { tigerStr.append('\t'); }
  \\n                           { tigerStr.append('\n'); }

  \\r                           { tigerStr.append('\r'); }
  \\\"                          { tigerStr.append('\"'); }
  \\                            { tigerStr.append('\\'); }

 }

<COM> {
    <<EOF>>             { throw new Error("Commentario incompleto") }
    "*/"                { ncom = ncom - 1; if (ncom == 0) { yybegin(YYINITIAL) } }
    "/*"                { ncom = ncom + 1; }
    .|\n                { /* ignorar */ }
}

