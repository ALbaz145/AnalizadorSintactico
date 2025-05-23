import java_cup.runtime.Symbol;

%%
%public
%class lexico
%unicode
%line
%column
%cup

%{
    private java_cup.runtime.Symbol symbol(int type) {
        return new java_cup.runtime.Symbol(type, yyline, yycolumn);
    }
    private java_cup.runtime.Symbol symbol(int type, Object value) {
        return new java_cup.runtime.Symbol(type, yyline, yycolumn, value);
        }
%}


DIGITO = [0-9]
LETRA = [a-zA-Z]
ESPACIO = [ \t\r\n]
ID = {LETRA}({LETRA}|{DIGITO})*
NUMERO = {DIGITO}+

%%

"+" | "-" | "*" | "/" | "^" | "%" {return new Symbol(sym.ARITMETICOS, yyline+1, yycolumn+1, yytext());}

"<" | "<=" | ">" | ">=" | "==" | "!=" {return new Symbol(sym.COMPARACION, yyline+1, yycolumn+1, yytext());}

"=" {return new Symbol(sym.ASIGNACION, yyline+1, yycolumn+1, yytext());}

"o_o" | ">_<" | "<_<" | ">_>" | "XD" | ":)" | ":D" | ":x" | ":^)" | ":]" | ":´(" | ">:(" | "D:" | ";)" | ":/" | ":S" | ":|" | ":$" | "<3" | "</3" | "v.v" | ";-;" | "x_x" | ">.<" | ">_<" | "^m^" | ":-)" | ":-]" | ":-}" | ":->" | ":-(" | ":-o" | "(=" | "[:" | "^m^" | ";)" | ";3" | ";D" | "</3" | "<3" | "o_o" | "DX" | "TuT" | "TwT" | "UwU" | "v.v" | "x_x" {return new Symbol(sym.EMOJI, yyline+1, yycolumn+1, yytext());}

"FALSE" | "False" | "false" | "TRUE" | "True" | "true" {return new Symbol(sym.BOOLEANO, yyline+1, yycolumn+1, yytext());}

"for" | "For" | "FOR" {return new Symbol(sym.bucleCONTADOR, yyline+1, yycolumn+1, yytext());}

"case" | "Case" | "CASE" {return new Symbol(sym.casoCONDICIONAL, yyline+1, yycolumn+1, yytext());}

"raise" | "Raise" | "RAISE" | "throw" | "Throw" | "THROW" {return new Symbol(sym.CHECADOR, yyline+1, yycolumn+1, yytext());}

"class" | "Class" | "CLASS" {return new Symbol(sym.CLASS, yyline+1, yycolumn+1, yytext());}

"do" | "Do" | "DO" {return new Symbol(sym.controlBUCLE, yyline+1, yycolumn+1, yytext());}

"]" {return new Symbol(sym.corDER, yyline+1, yycolumn+1, yytext());}

"[" {return new Symbol(sym.corIZQ, yyline+1, yycolumn+1, yytext());}

"struct" | "Struct" | "STRUCT" {return new Symbol(sym.defineESTRUCTURA, yyline+1, yycolumn+1, yytext());}

"def" | "Def" | "DEF" | "func" {return new Symbol(sym.defineFUNCION, yyline+1, yycolumn+1, yytext());}

":" {return new Symbol(sym.dosPUNTOS, yyline+1, yycolumn+1, yytext());}

"delete" | "Delete" | "DELETE" {return new Symbol(sym.ELIMINA, yyline+1, yycolumn+1, yytext());}

"async" | "Async" | "ASYNC" | "await" | "Await" | "AWAIT" | "sync" | "Sync" | "SYNC" | "synchronized" | "Synchronized" | "SYNCHRONIZED" {return new Symbol(sym.ESPERA, yyline+1, yycolumn+1, yytext());}

"--" | "++" {return new Symbol(sym.INCREDECRE, yyline+1, yycolumn+1, yytext());}

"try" | "Try" | "TRY" {return new Symbol(sym.intentaERROR, yyline+1, yycolumn+1, yytext());}

"}" {return new Symbol(sym.llaveDER, yyline+1, yycolumn+1, yytext());}

"{" {return new Symbol(sym.llaveIZQ, yyline+1, yycolumn+1, yytext());}

"catch" | "Catch" | "CATCH" | "finally" | "Finally" | "FINALLY" {return new Symbol(sym.manejaERROR, yyline+1, yycolumn+1, yytext());}

"print" | "Print" | "PRINT" | "printf" | "Printf" | "PRINTF" {return new Symbol(sym.muestraTEXTO, yyline+1, yycolumn+1, yytext());}

"null" | "Null" | "NULL" {return new Symbol(sym.NULO, yyline+1, yycolumn+1, yytext());}

"!" | "&&" | "|" | "and" | "And" | "AND" | "not" | "Not" | "NOT" | "or" | "Or" | "OR" | "xor" | "Xor" | "XOR" {return new Symbol(sym.operLOGICOS, yyline+1, yycolumn+1, yytext());}

")" {return new Symbol(sym.parDER, yyline+1, yycolumn+1, yytext());}

"(" {return new Symbol(sym.parIZQ, yyline+1, yycolumn+1, yytext());}

"if" | "If" | "IF" {return new Symbol(sym.preguntaCONDICIONAL, yyline+1, yycolumn+1, yytext());}

"while" | "While" | "WHILE" {return new Symbol(sym.preguntaWHILE, yyline+1, yycolumn+1, yytext());} 

";" {return new Symbol(sym.puntoCOMA, yyline+1, yycolumn+1, yytext());}

"break" | "Break" | "BREAK" {return new Symbol(sym.rompeCONDICIONAL, yyline+1, yycolumn+1, yytext());}

"switch" | "Switch" | "SWITCH" {return new Symbol(sym.seleccionCONDICIONAL, yyline+1, yycolumn+1, yytext());}

"continue" | "Continue" | "CONTINUE" {return new Symbol(sym.siguienteCICLO, yyline+1, yycolumn+1, yytext());}

"else" | "Else" | "ELSE" {return new Symbol(sym.sinoCONDICIONAL, yyline+1, yycolumn+1, yytext());} 

"slice" | "Slice" | "SLICE" {return new Symbol(sym.tipoDatoArregloCorte, yyline+1, yycolumn+1, yytext());}

"bool" | "Bool" | "BOOL" | "boolean" | "Boolean" | "BOOLEAN" {return new Symbol(sym.tipoDatoBool, yyline+1, yycolumn+1, yytext());}

"byte" | "Byte" | "BYTE" {return new Symbol(sym.tipoDatoByte, yyline+1, yycolumn+1, yytext());}

"char" | "Char" | "CHAR" | "character" | "Character" | "CHARACTER" {return new Symbol(sym.tipoDatoCaracter, yyline+1, yycolumn+1, yytext());}

"enum" | "Enum" | "ENUM" {return new Symbol(sym.tipoDatoENUM, yyline+1, yycolumn+1, yytext());}

"const" | "Const" | "CONST" | "final" | "Final" | "FINAL" | "val" | "Val" | "VAL" {return new Symbol(sym.tipoDatoINMU, yyline+1, yycolumn+1, yytext());}

"array" | "Array" | "ARRAY" | "list" | "List" | "LIST" {return new Symbol(sym.tipoDatoLista, yyline+1, yycolumn+1, yytext());}

"bigint" | "Bigint" | "BIGINT" | "int" | "Int" | "INT" | "int16" | "Int16" | "INT16" | "int32" | "Int32" | "INT32" | "int64" | "Int64" | "INT64" | "int8" | "Int8" | "INT8" | "integer" | "Integer" | "INTEGER" | "long" | "Long" | "LONG" | "short" | "Short" | "SHORT" | "ulong" | "Ulong" | "ULONG" | "ushort" | "Ushort" | "USHORT" {return new Symbol(sym.tipoDatoNUM, yyline+1, yycolumn+1, yytext());}

"decimal" | "Decimal" | "DECIMAL" | "double" | "Double" | "DOUBLE" | "float" | "Float" | "FLOAT" {return new Symbol(sym.tipoDatoNumDecimal, yyline+1, yycolumn+1, yytext());}

"number" | "Number" | "NUMBER" {return new Symbol(sym.tipoDatoNUMERO, yyline+1, yycolumn+1, yytext());}

"set" | "Set" | "SET" {return new Symbol(sym.tipoDatoSET, yyline+1, yycolumn+1, yytext());}

"frozenset" | "Frozenset" | "Frozenset" {return new Symbol(sym.tipoDatoSetInmu, yyline+1, yycolumn+1, yytext());}

"Symbol" | "Symbol" | "SYMBOL" {return new Symbol(sym.tipoDatoSimbolo, yyline+1, yycolumn+1, yytext());}

"str" | "Str" | "STR" | "string" | "String" | "STRING" | "text" | "Text" | "TEXT" | "varchar" | "Varchar" | "VARCHAR" {return new Symbol(sym.tipoDatoTEXT, yyline+1, yycolumn+1, yytext());}

"tuple" | "Tuple" | "TUPLE" {return new Symbol(sym.tipoDatoTupla, yyline+1, yycolumn+1, yytext());}

"let" | "Let" | "LET" | "var" | "Var" | "VAR" {return new Symbol(sym.tipoDatoVARIABLE, yyline+1, yycolumn+1, yytext());}

"include" {return new Symbol(sym.tomaARCHIVO, yyline+1, yycolumn+1, yytext());}

"pub" | "public" | "Public" | "PUBLIC" | "private" | "Private" | "PRIVATE" | "protected" | "Protected" | "PROTECTED" {return new Symbol(sym.VISIBILIDAD, yyline+1, yycolumn+1, yytext());}

"void" {return new Symbol(sym.VOID, yyline+1, yycolumn+1, yytext());}

"," {return new Symbol(sym.COMA, yyline+1, yycolumn+1, yytext());}

"'" {return new Symbol(sym.COMILLA, yyline+1, yycolumn+1, yytext());}

"." {return new Symbol(sym.PUNTO, yyline+1, yycolumn+1, yytext());}

"import" | "IMPORT" {return new Symbol(sym.IMPORTACION, yyline+1, yycolumn+1, yytext());}

"return" | "RETURN" {return new Symbol(sym.RETORNA, yyline+1, yycolumn+1, yytext());}

"interface" | "INTERFACE" {return new Symbol(sym.INTERFAZ, yyline+1, yycolumn+1, yytext());}

\"([^\"\\]|\\.)*\" {return new Symbol(sym.STRING, yyline+1, yycolumn+1, yytext());}

"forEach" | "foreach" {return new Symbol(sym.PARA, yyline+1, yycolumn+1, yytext());}

{NUMERO} { return new Symbol(sym.NUMERO, yyline+1, yycolumn+1, yytext()); }

{ID} { return new Symbol(sym.ID, yyline+1, yycolumn+1, yytext()); }

{ESPACIO} { /* Se ignoran */ }

[ \t\n\r]+      { /* Ignorar */ }

"//".*                             { /* ignora comentarios de línea */ }
"/*"([^*]|[\\*][^/])*"*/"         { /* ignora comentarios multilínea */ }

. {
    System.err.println("ERROR léxico en línea " + (yyline + 1) + 
                      ", columna " + (yycolumn + 1) + 
                      ": Símbolo inválido '" + yytext() + "'");
    return symbol(sym.error); // Token genérico de error
}