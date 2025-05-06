import java_cup.runtime.Symbol;
%%

%public
%class lexico

%line
%column

%cup

%{
  private Symbol symbol(int type) {
    return new Symbol(type, yyline + 1, yycolumn + 1, yytext());
  }
%}

DIGITO = [0-9]
LETRA = [a-zA-Z]
ESPACIO = [ \t\r\n]
ID = {LETRA}({LETRA}|{DIGITO})*
NUMERO = {DIGITO}+

%%

"+" | "-" | "*" | "/" | "^" | "%" {return symbol(sym.ARITMETICOS);}

"<" | "<=" | ">" | ">=" | "==" | "!=" {return symbol(sym.COMPARACION);}

"=" {return symbol(sym.ASIGNACION);}

"o_o" | ">_<" | "<_<" | ">_>" | "XD" | ":)" | ":D" | ":x" | ":^)" | ":]" | ":´(" | ">:(" | "D:" | ";)" | ":/" | ":S" | ":|" | ":$" | "<3" | "</3" | "v.v" | ";-;" | "x_x" | ">.<" | ">_<" | "^m^" | ":-)" | ":-]" | ":-}" | ":->" | ":-(" | ":-o" | "(=" | "[:" | "^m^" | ";)" | ";3" | ";D" | "</3" | "<3" | "o_o" | "DX" | "TuT" | "TwT" | "UwU" | "v.v" | "x_x" {return symbol(sym.EMOJI);}

"FALSE" | "False" | "false" | "TRUE" | "True" | "true" {return symbol(sym.BOOLEANO);}

"for" | "For" | "FOR" {return symbol(sym.bucleCONTADOR);}

"case" | "Case" | "CASE" {return symbol(sym.casoCONDICIONAL);}

"raise" | "Raise" | "RAISE" | "throw" | "Throw" | "THROW" {return symbol(sym.CHECADOR);}

"class" | "Class" | "CLASS" {return symbol(sym.CLASS);}

"do" | "Do" | "DO" {return symbol(sym.controlBUCLE);}

"]" {return symbol(sym.corDER);}

"[" {return symbol(sym.corIZQ);}

"struct" | "Struct" | "STRUCT" {return symbol(sym.defineESTRUCTURA);}

"def" | "Def" | "DEF" | "func" {return symbol(sym.defineFUNCION);}

":" {return symbol(sym.dosPUNTOS);}

"delete" | "Delete" | "DELETE" {return symbol(sym.ELIMINA);}

"async" | "Async" | "ASYNC" | "await" | "Await" | "AWAIT" | "sync" | "Sync" | "SYNC" | "synchronized" | "Synchronized" | "SYNCHRONIZED" {return symbol(sym.ESPERA);}

"--" | "++" {return symbol(sym.INCREDECRE);}

"try" | "Try" | "TRY" {return symbol(sym.intentaERROR);}

"}" {return symbol(sym.llaveDER);}

"{" {return symbol(sym.llaveIZQ);}

"catch" | "Catch" | "CATCH" | "finally" | "Finally" | "FINALLY" {return symbol(sym.manejaERROR);}

"print" | "Print" | "PRINT" | "printf" | "Printf" | "PRINTF" {return symbol(sym.muestraTEXTO);}

"null" | "Null" | "NULL" {return symbol(sym.NULO);}

"!" | "&&" | "|" | "and" | "And" | "AND" | "not" | "Not" | "NOT" | "or" | "Or" | "OR" | "xor" | "Xor" | "XOR" {return symbol(sym.operLOGICOS);}

")" {return symbol(sym.parDER);}

"(" {return symbol(sym.parIZQ);}

"if" | "If" | "IF" | "while" | "While" | "WHILE" {return symbol(sym.preguntaCONDICIONAL);}

";" {return symbol(sym.puntoCOMA);}

"break" | "Break" | "BREAK" {return symbol(sym.rompeCONDICIONAL);}

"switch" | "Switch" | "SWITCH" {return symbol(sym.seleccionCONDICIONAL);}

"continue" | "Continue" | "CONTINUE" {return symbol(sym.siguienteCICLO);}

"else" | "Else" | "ELSE" {return symbol(sym.sinoCONDICIONAL);} 

"slice" | "Slice" | "SLICE" {return symbol(sym.tipoDatoArregloCorte);}

"bool" | "Bool" | "BOOL" | "boolean" | "Boolean" | "BOOLEAN" {return symbol(sym.tipoDatoBool);}

"byte" | "Byte" | "BYTE" {return symbol(sym.tipoDatoByte);}

"char" | "Char" | "CHAR" | "character" | "Character" | "CHARACTER" {return symbol(sym.tipoDatoCaracter);}

"enum" | "Enum" | "ENUM" {return symbol(sym.tipoDatoENUM);}

"const" | "Const" | "CONST" | "final" | "Final" | "FINAL" | "val" | "Val" | "VAL" {return symbol(sym.tipoDatoINMU);}

"array" | "Array" | "ARRAY" | "list" | "List" | "LIST" {return symbol(sym.tipoDatoLista);}

"bigint" | "Bigint" | "BIGINT" | "int" | "Int" | "INT" | "int16" | "Int16" | "INT16" | "int32" | "Int32" | "INT32" | "int64" | "Int64" | "INT64" | "int8" | "Int8" | "INT8" | "integer" | "Integer" | "INTEGER" | "long" | "Long" | "LONG" | "short" | "Short" | "SHORT" | "ulong" | "Ulong" | "ULONG" | "ushort" | "Ushort" | "USHORT" {return symbol(sym.tipoDatoNUM);}

"decimal" | "Decimal" | "DECIMAL" | "double" | "Double" | "DOUBLE" | "float" | "Float" | "FLOAT" {return symbol(sym.tipoDatoNumDecimal);}

"number" | "Number" | "NUMBER" {return symbol(sym.tipoDatoNUMERO);}

"set" | "Set" | "SET" {return symbol(sym.tipoDatoSET);}

"frozenset" | "Frozenset" | "Frozenset" {return symbol(sym.tipoDatoSetInmu);}

"symbol" | "Symbol" | "SYMBOL" {return symbol(sym.tipoDatoSimbolo);}

"str" | "Str" | "STR" | "string" | "String" | "STRING" | "text" | "Text" | "TEXT" | "varchar" | "Varchar" | "VARCHAR" {return symbol(sym.tipoDatoTEXT);}

"tuple" | "Tuple" | "TUPLE" {return symbol(sym.tipoDatoTupla);}

"let" | "Let" | "LET" | "var" | "Var" | "VAR" {return symbol(sym.tipoDatoVARIABLE);}

"include" {return symbol(sym.tomaARCHIVO);}

"pub" | "public" | "Public" | "PUBLIC" | "private" | "Private" | "PRIVATE" | "protected" | "Protected" | "PROTECTED" {return symbol(sym.VISIBILIDAD);}

"void" {return symbol(sym.VOID);}

"," {return symbol(sym.COMA);}

"'" {return symbol(sym.COMILLA);}

{NUMERO} { return symbol(sym.NUMERO); }

{ID} { return symbol(sym.ID); }

{ESPACIO} { /* Se ignoran */ }

.       { System.err.println("Error: Caracter no válido '" + yytext() + "'"); }