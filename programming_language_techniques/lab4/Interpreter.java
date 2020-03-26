import java.util.*;

import Fun.Absyn.*;
import Fun.PrettyPrinter;

public class Interpreter {
    private final ProgramVisitor programVisitor = new ProgramVisitor();
    private final ExpVisitor expVisitor = new ExpVisitor();
    final Strategy strategy;

    public Interpreter(Strategy strategy) {
        this.strategy = strategy;
    }

    public void interpret(Program p) {
        Integer val = p.accept(programVisitor, null);
        System.out.println(val);
    }

    private class ProgramVisitor implements Program.Visitor<Integer, Object> {
        @Override
        public Integer visit(Prog p, Object arg) {
            if (p.main_ == null) {
                throw new RuntimeException("'main' function is missing");
            }
            Env env = Env.emptyEnv();
            //extend env with function definitions
            for (Def def : p.listdef_) {
                env.update((DDef) def);
            }
            //check variable bounding in function definition
            for (Def def : p.listdef_) {
                DDef dDef = (DDef) def;
                String functionName = dDef.ident_;
                Val functionValue = env.lookUp(functionName);
                checkVariableBounding(functionValue.exp, env, functionName);
            }
            //check variable bounding in 'main'
            DMain dMain = (DMain) p.main_;
            checkVariableBounding(dMain.exp_, env,"main");
            Val val = dMain.exp_.accept(expVisitor, env);
            if (val.exp instanceof EInt) {
                EInt result = (EInt) val.exp;
                return result.integer_;
            } else {
                throw new RuntimeException("'main' function can not evaluate to integer.");
            }
        }

        private void checkVariableBounding(Exp exp, Env env, String functionName) {
            Set<String> freeVariables = InterpreterUtil.freeVariables(exp);
            for (String var : freeVariables) {
                if (env.lookUp(var) == null) {
                    throw new RuntimeException("variable [" + var + "] is not bound in function [" + functionName + "]");
                }
            }
        }
    }

    private class ExpVisitor implements Exp.Visitor<Val, Env> {
        @Override
        public Val visit(EVar p, Env env) {
            Val val = env.lookUp(p.ident_);
            return val.exp.accept(this, env.retainFunctionStorage(val.env.localVarStorage));
        }

        @Override
        public Val visit(EInt p, Env env) {
            return new Val(p, env);
        }

        @Override
        public Val visit(EApp p, Env env) {
            Val functionVal = p.exp_1.accept(this, env);
            if (!(functionVal.exp instanceof EAbs)) {
                throw new RuntimeException("can not apply argument to expression:\n\t" + PrettyPrinter.print(p.exp_1));
            }
            EAbs eAbs = (EAbs) functionVal.exp;
            Val argVal;
            if (Interpreter.this.strategy == Strategy.CallByName) {
                argVal = new Val(p.exp_2, env.retainLocalVarStorage());
            } else {
                argVal = p.exp_2.accept(this, env);
            }
            Env newEnv = Env.copyFrom(env.functionStorage, functionVal.env.localVarStorage);
            newEnv.update(eAbs.ident_, argVal);
            return eAbs.exp_.accept(this, newEnv);
        }

        @Override
        public Val visit(EAdd p, Env env) {
            Val val1 = p.exp_1.accept(this, env);
            if (! (val1.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to integer:\n" + PrettyPrinter.print(p.exp_1));
            }
            Val val2 = p.exp_2.accept(this, env);
            if (! (val2.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to integer:\n" + PrettyPrinter.print(p.exp_2));
            }
            EInt eInt1 = (EInt) val1.exp;
            EInt eInt2 = (EInt) val2.exp;
            return new Val(new EInt(eInt1.integer_ + eInt2.integer_), env);
        }

        @Override
        public Val visit(ESub p, Env env) {
            Val val1 = p.exp_1.accept(this, env);
            if (! (val1.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to integer:\n\t" + PrettyPrinter.print(p.exp_1));
            }
            Val val2 = p.exp_2.accept(this, env);
            if (! (val2.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to integer:\n\t" + PrettyPrinter.print(p.exp_2));
            }
            EInt eInt1 = (EInt) val1.exp;
            EInt eInt2 = (EInt) val2.exp;
            return new Val(new EInt(eInt1.integer_ - eInt2.integer_), env);
        }

        @Override
        public Val visit(ELt p, Env env) {
            Val val1 = p.exp_1.accept(this, env);
            if (! (val1.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to integer:\n\t" + PrettyPrinter.print(p.exp_1));
            }
            Val val2 = p.exp_2.accept(this, env);
            if (! (val2.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to integer:\n\t" + PrettyPrinter.print(p.exp_2));
            }
            EInt eInt1 = (EInt) val1.exp;
            EInt eInt2 = (EInt) val2.exp;
            int bool = eInt1.integer_ < eInt2.integer_ ? 1 : 0;
            return new Val(new EInt(bool), env);
        }

        @Override
        public Val visit(EIf p, Env env) {
            Val condition = p.exp_1.accept(this, env);
            if (!(condition.exp instanceof EInt)) {
                throw new RuntimeException("expression can not evaluate to true of false:\n\t" + PrettyPrinter.print(p.exp_1));
            }
            EInt conditionVal = (EInt) condition.exp;
            if (conditionVal.integer_ == 1) {
                return p.exp_2.accept(this, env);
            } else {
                return p.exp_3.accept(this, env);
            }
        }

        @Override
        public Val visit(EAbs p, Env env) {
            return new Val(p, env.retainLocalVarStorage());
        }
    }

    private static class Env {
        Map<String, Val> functionStorage;
        Map<String, Val> localVarStorage;

        public Env(Map<String, Val> functionStorage, Map<String, Val> localVarStorage) {
            this.functionStorage = functionStorage;
            this.localVarStorage = localVarStorage;
        }

        static Env emptyEnv() {
            return new Env(new HashMap<>(), new HashMap<>());
        }

        static Env copyFrom(Map<String, Val> functionStorage, Map<String, Val> localVarStorage) {
            Map<String, Val> copyFunStorage = new HashMap<>(functionStorage);
            Map<String, Val> copyLocalStorage = new HashMap<>(localVarStorage);
            return new Env(copyFunStorage, copyLocalStorage);
        }

        Env retainFunctionStorage(Map<String, Val> localVarStorage) {
            return copyFrom(this.functionStorage, localVarStorage);
        }

        Env retainLocalVarStorage() {
            return copyFrom(new HashMap<>(), this.localVarStorage);
        }

        void update(DDef dDef) {
            if (functionStorage.containsKey(dDef.ident_)) {
                throw new RuntimeException("multiple definition of function [" + dDef.ident_ + "]");
            }
            functionStorage.put(dDef.ident_, new Val(InterpreterUtil.functionTransformation(dDef), Env.emptyEnv()));
        }

        void update(String ident, Val val) {
            localVarStorage.put(ident, val);
        }

        Val lookUp(String identifier) {
            Val result = localVarStorage.get(identifier);
            if (result == null) {
                result = functionStorage.get(identifier);
            }
            return result;
        }
    }

    private static class Val {
        Exp exp;
        Env env;

        public Val(Exp exp, Env env) {
            this.exp = exp;
            this.env = env;
        }
    }
}
