import java.io.FileReader;

public class Main {
    public static void main(String[] args) {
        try {
            // Crear analizador léxico
            lexico lex = new lexico(new FileReader("texto.txt"));
            // Crear analizador sintáctico
            parser sintactico = new parser(lex);
            // Ejecutar análisis
            sintactico.parse();
            System.out.println("Análisis completado sin errores");
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }
}
