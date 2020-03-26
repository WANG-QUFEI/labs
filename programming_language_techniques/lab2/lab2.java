import CMM.Yylex;
import CMM.Absyn.Program;
import CMM.parser;

import java.io.FileReader;
import java.io.IOException;

public class lab2 {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Usage: lab2 <SourceFile>");
            System.exit(1);
        }
        Yylex l = null;
        try {
            l = new Yylex(new FileReader(args[0]));
            parser p = new parser(l);
            Program parse_tree = p.pProgram();
            new TypeChecker().typeCheck(parse_tree);
            new Interpreter().interpret(parse_tree);
        } catch (TypeException e) {
            System.out.println("TYPE ERROR");
            System.err.println(e.toString());
            System.exit(1);
        } catch (InterpreterException e) {
            System.out.println("INTERPRETER ERROR");
            System.err.println(e.toString());
            System.exit(1);
        } catch (IOException e) {
            System.err.println(e.toString());
            System.exit(1);
        } catch (Throwable e) {
            System.out.println("SYNTAX ERROR");
            System.out.println("At line " + l.line_num()
                    + ", near \"" + l.buff() + "\" :");
            System.out.println("     " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
