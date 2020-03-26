import CMM.Absyn.PDefs;
import CMM.Absyn.Program;

public class TypeChecker {
	/**
	 * Type check whole program as sequence of functions.
	 * @param program
	 */
	public void typeCheck(Program program) {
		PDefs pdefs = (PDefs) program;
		pdefs.accept(TypeCheckVisitor.PROGRAM_VISITOR, Env.emptyEnv());
	}
}