import java_cup.runtime.Symbol;

%%
%public
%class lexico
%line
%column
%cup

DIGITO = [0-9]
LETRA = [a-zA-Z]
ESPACIO = [ \t\r\n]
ID = {LETRA}({LETRA}|{DIGITO})*
NUMERO = {DIGITO}+

%%

"+" | "-" | "*" | "/" | "^" | "%" {return new Symbol(sym.ARITMETICOS);}

"<" | "<=" | ">" | ">=" | "==" | "!=" {return new Symbol(sym.COMPARACION);}

"=" {return new Symbol(sym.ASIGNACION);}

"o_o" | ">_<" | "<_<" | ">_>" | "XD" | ":)" | ":D" | ":x" | ":^)" | ":]" | ":´(" | ">:(" | "D:" | ";)" | ":/" | ":S" | ":|" | ":$" | "<3" | "</3" | "v.v" | ";-;" | "x_x" | ">.<" | ">_<" | "^m^" | ":-)" | ":-]" | ":-}" | ":->" | ":-(" | ":-o" | "(=" | "[:" | "^m^" | ";)" | ";3" | ";D" | "</3" | "<3" | "o_o" | "DX" | "TuT" | "TwT" | "UwU" | "v.v" | "x_x" {return new Symbol(sym.EMOJI);}

"FALSE" | "False" | "false" | "TRUE" | "True" | "true" {return new Symbol(sym.BOOLEANO);}

"for" | "For" | "FOR" {return new Symbol(sym.bucleCONTADOR);}

"case" | "Case" | "CASE" {return new Symbol(sym.casoCONDICIONAL);}

"raise" | "Raise" | "RAISE" | "throw" | "Throw" | "THROW" {return new Symbol(sym.CHECADOR);}

"class" | "Class" | "CLASS" {return new Symbol(sym.CLASS);}

"do" | "Do" | "DO" {return new Symbol(sym.controlBUCLE);}

"]" {return new Symbol(sym.corDER);}

"[" {return new Symbol(sym.corIZQ);}

"struct" | "Struct" | "STRUCT" {return new Symbol(sym.defineESTRUCTURA);}

"def" | "Def" | "DEF" | "func" {return new Symbol(sym.defineFUNCION);}

":" {return new Symbol(sym.dosPUNTOS);}

"delete" | "Delete" | "DELETE" {return new Symbol(sym.ELIMINA);}

"async" | "Async" | "ASYNC" | "await" | "Await" | "AWAIT" | "sync" | "Sync" | "SYNC" | "synchronized" | "Synchronized" | "SYNCHRONIZED" {return new Symbol(sym.ESPERA);}

"--" | "++" {return new Symbol(sym.INCREDECRE);}

"try" | "Try" | "TRY" {return new Symbol(sym.intentaERROR);}

"}" {return new Symbol(sym.llaveDER);}

"{" {return new Symbol(sym.llaveIZQ);}

"catch" | "Catch" | "CATCH" | "finally" | "Finally" | "FINALLY" {return new Symbol(sym.manejaERROR);}

"print" | "Print" | "PRINT" | "printf" | "Printf" | "PRINTF" {return new Symbol(sym.muestraTEXTO);}

"null" | "Null" | "NULL" {return new Symbol(sym.NULO);}

"!" | "&&" | "|" | "and" | "And" | "AND" | "not" | "Not" | "NOT" | "or" | "Or" | "OR" | "xor" | "Xor" | "XOR" {return new Symbol(sym.operLOGICOS);}

")" {return new Symbol(sym.parDER);}

"(" {return new Symbol(sym.parIZQ);}

"if" | "If" | "IF" | "while" | "While" | "WHILE" {return new Symbol(sym.preguntaCONDICIONAL);}

";" {return new Symbol(sym.puntoCOMA);}

"break" | "Break" | "BREAK" {return new Symbol(sym.rompeCONDICIONAL);}

"switch" | "Switch" | "SWITCH" {return new Symbol(sym.seleccionCONDICIONAL);}

"continue" | "Continue" | "CONTINUE" {return new Symbol(sym.siguienteCICLO);}

"else" | "Else" | "ELSE" {return new Symbol(sym.sinoCONDICIONAL);} 

"slice" | "Slice" | "SLICE" {return new Symbol(sym.tipoDatoArregloCorte);}

"bool" | "Bool" | "BOOL" | "boolean" | "Boolean" | "BOOLEAN" {return new Symbol(sym.tipoDatoBool);}

"byte" | "Byte" | "BYTE" {return new Symbol(sym.tipoDatoByte);}

"char" | "Char" | "CHAR" | "character" | "Character" | "CHARACTER" {return new Symbol(sym.tipoDatoCaracter);}

"enum" | "Enum" | "ENUM" {return new Symbol(sym.tipoDatoENUM);}

"const" | "Const" | "CONST" | "final" | "Final" | "FINAL" | "val" | "Val" | "VAL" {return new Symbol(sym.tipoDatoINMU);}

"array" | "Array" | "ARRAY" | "list" | "List" | "LIST" {return new Symbol(sym.tipoDatoLista);}

"bigint" | "Bigint" | "BIGINT" | "int" | "Int" | "INT" | "int16" | "Int16" | "INT16" | "int32" | "Int32" | "INT32" | "int64" | "Int64" | "INT64" | "int8" | "Int8" | "INT8" | "integer" | "Integer" | "INTEGER" | "long" | "Long" | "LONG" | "short" | "Short" | "SHORT" | "ulong" | "Ulong" | "ULONG" | "ushort" | "Ushort" | "USHORT" {return new Symbol(sym.tipoDatoNUM);}

"decimal" | "Decimal" | "DECIMAL" | "double" | "Double" | "DOUBLE" | "float" | "Float" | "FLOAT" {return new Symbol(sym.tipoDatoNumDecimal);}

"number" | "Number" | "NUMBER" {return new Symbol(sym.tipoDatoNUMERO);}

"set" | "Set" | "SET" {return new Symbol(sym.tipoDatoSET);}

"frozenset" | "Frozenset" | "Frozenset" {return new Symbol(sym.tipoDatoSetInmu);}

"Symbol" | "Symbol" | "SYMBOL" {return new Symbol(sym.tipoDatoSimbolo);}

"str" | "Str" | "STR" | "string" | "String" | "STRING" | "text" | "Text" | "TEXT" | "varchar" | "Varchar" | "VARCHAR" {return new Symbol(sym.tipoDatoTEXT);}

"tuple" | "Tuple" | "TUPLE" {return new Symbol(sym.tipoDatoTupla);}

"let" | "Let" | "LET" | "var" | "Var" | "VAR" {return new Symbol(sym.tipoDatoVARIABLE);}

"include" {return new Symbol(sym.tomaARCHIVO);}

"pub" | "public" | "Public" | "PUBLIC" | "private" | "Private" | "PRIVATE" | "protected" | "Protected" | "PROTECTED" {return new Symbol(sym.VISIBILIDAD);}

"void" {return new Symbol(sym.VOID);}

"," {return new Symbol(sym.COMA);}

"'" {return new Symbol(sym.COMILLA);}

{NUMERO} { return new Symbol(sym.NUMERO); }

{ID} { return new Symbol(sym.ID); }

{ESPACIO} { /* Se ignoran */ }

.       { System.err.println("Error: Caracter no válido '" + yytext() + "'"); }