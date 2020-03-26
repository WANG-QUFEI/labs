import CMM.Absyn.PDefs;
import CMM.Absyn.Program;

public class Interpreter {
    public void interpret(Program p) {
        PDefs pdefs = (PDefs) p;
        pdefs.accept(InterpreterVisitor.PROGRAM_VISITOR, Env.emptyEnv());
    }
}
