import java.io.*;
import java_cup.runtime.Symbol;

public class Main {
    public static void main(String[] args) {
        try {
            Reader input = new BufferedReader(new FileReader("input\texto.txt"));
            lexico lexer = new lexico(input);
            parser parser = new parser(lexer);
            parser.parse();
            System.out.println("correcto");
        } catch (Exception e) {
            System.err.println("Error");
            e.printStackTrace();
        }
    }
}
