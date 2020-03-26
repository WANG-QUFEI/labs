import Fun.Absyn.*;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Objects;
import java.util.Set;

public class InterpreterUtil {
    public static Exp functionTransformation(DDef dDef) {
        Objects.requireNonNull(dDef);
        LinkedList<String> stack = new LinkedList<>();
        for (String var : dDef.listident_) {
            stack.push(var);
        }
        return build(stack, dDef.exp_);
    }

    private static Exp build(LinkedList<String> varList, Exp exp) {
        Exp tmp = exp;
        while (!varList.isEmpty()) {
            String topElem = varList.pop();
            tmp = new EAbs(topElem, tmp);
        }
        return tmp;
    }

    public static Set<String> freeVariables(Exp exp) {
        Set<String> set = new HashSet<>();
        Objects.requireNonNull(exp);
        if (exp instanceof EVar) {
            EVar eVar = (EVar) exp;
            set.add(eVar.ident_);
        } else if (exp instanceof EApp) {
            EApp eApp = (EApp) exp;
            set.addAll(freeVariables(eApp.exp_1));
            set.addAll(freeVariables(eApp.exp_2));
        } else if (exp instanceof EAdd) {
            EAdd eAdd = (EAdd) exp;
            set.addAll(freeVariables(eAdd.exp_1));
            set.addAll(freeVariables(eAdd.exp_2));
        } else if (exp instanceof ESub) {
            ESub eSub = (ESub) exp;
            set.addAll(freeVariables(eSub.exp_1));
            set.addAll(freeVariables(eSub.exp_2));
        } else if (exp instanceof ELt) {
            ELt eLt = (ELt) exp;
            set.addAll(freeVariables(eLt.exp_1));
            set.addAll(freeVariables(eLt.exp_2));
        } else if (exp instanceof EIf) {
            EIf eIf = (EIf) exp;
            set.addAll(freeVariables(eIf.exp_1));
            set.addAll(freeVariables(eIf.exp_2));
            set.addAll(freeVariables(eIf.exp_3));
        } else if (exp instanceof EAbs) {
            EAbs eAbs = (EAbs) exp;
            set.addAll(freeVariables(eAbs.exp_));
            set.remove(eAbs.ident_);
        }
        return set;
    }
}
