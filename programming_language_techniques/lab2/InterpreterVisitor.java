import CMM.Absyn.*;
import CMM.PrettyPrinter;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

public class InterpreterVisitor {
    public static final String MAIN = "main";
    public static final String VAR_RETURN = ":return";
    public static final ProgramVisitor PROGRAM_VISITOR = new ProgramVisitor();
    public static final DefVisitor DEF_VISITOR = new DefVisitor();
    public static final StmVisitor STM_VISITOR = new StmVisitor();
    public static final ExpVisitor EXP_VISITOR = new ExpVisitor();

    public static class ProgramVisitor implements Program.Visitor<Object, Env<Val>> {

        @Override
        public Object visit(PDefs pDefs, Env<Val> env) {
            if (pDefs.listdef_.size() > 0) {
                //1. add program function definitions
                DFun mainFunction = null;
                for (Def def : pDefs.listdef_) {
                    DFun fun = (DFun) def;

                    env.getFunDefinitions().put(fun.id_, fun);
                    if (fun.id_.equals(MAIN)) {
                        mainFunction = fun;
                    }
                }
                //2. evaluate main() function
                if (mainFunction != null) {
                    env.newBlock();
                    mainFunction.accept(DEF_VISITOR, env);
                    env.exitBlock();
                }
            } else {
                throw new InterpreterException("empty program");
            }
            return null;
        }
    }

    public static class DefVisitor implements Def.Visitor<Val, Env<Val>> {
        @Override
        public Val visit(DFun p, Env<Val> env) {
            Val val = null;
            if (!TypeCheckTool.isListEmpty(p.liststm_)) {
                boolean hasReturnValue = !(p.type_ instanceof Tvoid);
                if (hasReturnValue) {
                    env.addVarValueCurrentContext(VAR_RETURN, new VUndefined());
                }
                for (Stm stm : p.liststm_) {
                    stm.accept(STM_VISITOR, env);
                    if (env.checkReturn()) {
                        break;
                    }
                }
                if (hasReturnValue) {
                    val = env.findReturnVal();
                    if (val == null || val instanceof VVoid || val instanceof  VUndefined) {
                        throw new InterpreterException("invocation of function has no return type!\n" + PrettyPrinter.print(p));
                    }
                }
            }
            return val;
        }
    }

    public static class StmVisitor implements Stm.Visitor<Val, Env<Val>> {
        @Override
        public Val visit(SExp p, Env<Val> env) {
            return p.exp_.accept(EXP_VISITOR, env);
        }

        @Override
        public Val visit(SDecl p, Env<Val> env) {
            Val defaultVal = EvalTool.getDefaultValByType(p.type_);
            for (String id : p.listid_) {
                env.addVarValueCurrentContext(id, defaultVal);
            }
            return defaultVal;
        }

        @Override
        public Val visit(SInit p, Env<Val> env) {
            Val defaultVal = EvalTool.getDefaultValByType(p.type_);
            //do variable declaration
            env.addVarValueCurrentContext(p.id_, defaultVal);
            Val val = p.exp_.accept(EXP_VISITOR, env);
            val = EvalTool.coercionIfNeed(val, p.type_);
            env.updateVarValue(p.id_, val);
            return val;
        }

        @Override
        public Val visit(SReturn p, Env<Val> env) {
            Val val = p.exp_.accept(EXP_VISITOR, env);
            env.updateVarValue(VAR_RETURN, val);
            return val;
        }

        @Override
        public Val visit(SWhile p, Env<Val> env) {
            Val expVal = p.exp_.accept(EXP_VISITOR, env);
            if (EvalTool.isBooleanTrue(expVal)) {
                env.newBlock();
                p.stm_.accept(this, env);
                env.exitBlock();
                p.accept(this, env);
            }
            return null;
        }

        @Override
        public Val visit(SBlock p, Env<Val> env) {
            Val val = null;
            env.newBlock();
            for (Stm stm : p.liststm_) {
                Val v = stm.accept(this, env);
                if (stm instanceof SReturn) {
                    val = v;
                    break;
                } else if (env.checkReturn()) {
                    break;
                }
            }
            env.exitBlock();
            return val;
        }

        @Override
        public Val visit(SIfElse p, Env<Val> env) {
            Val val = p.exp_.accept(EXP_VISITOR, env);
            if (EvalTool.isBooleanTrue(val)) {
                env.newBlock();
                p.stm_1.accept(this, env);
                env.exitBlock();
            } else {
                env.newBlock();
                p.stm_2.accept(this, env);
                env.exitBlock();
            }
            return null;
        }
    }

    public static class ExpVisitor implements Exp.Visitor<Val, Env<Val>> {
        private static final Scanner scanner = new Scanner(System.in);

        @Override
        public Val visit(EInt p, Env<Val> env) {
            return EvalTool.eval(p);
        }

        @Override
        public Val visit(EDouble p, Env<Val> env) {
            return EvalTool.eval(p);
        }

        @Override
        public Val visit(ETrue p, Env<Val> env) {
            return EvalTool.eval(p);
        }

        @Override
        public Val visit(EFalse p, Env<Val> env) {
            return EvalTool.eval(p);
        }

        @Override
        public Val visit(EId p, Env<Val> env) {
            Val value = env.findVariableValue(p.id_);
            if (EvalTool.uninitializedVal(value)) {
                throw new InterpreterException("uninitialized variable, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
            }
            return value;
        }

        @Override
        public Val visit(EApp p, Env<Val> env) {
            Val val;
            //1. find function definition
            DFun dFun = env.getFunDefinitions().get(p.id_);
            if (dFun != null) {
                ListArg argList = dFun.listarg_;
                ListExp expList = p.listexp_;
                List<Val> valList = new ArrayList<>();
                Iterator<Arg> argIterator = argList.iterator();
                //2. evaluate each expression to call the function
                for (Exp exp : expList) {
                    Val subVal = exp.accept(this, env);
                    valList.add(subVal);
                }
                //3. assign expression evaluation result to each formal parameter in a new context;
                env.newBlock();
                int idx = 0;
                while (argIterator.hasNext()) {
                    Arg arg = argIterator.next();
                    ADecl aDecl = (ADecl) arg;
                    env.addVarValueCurrentContext(aDecl.id_, EvalTool.coercionIfNeed(EvalTool.copy(valList.get(idx++)), aDecl.type_));
                }
                //4. execute function
                val = dFun.accept(DEF_VISITOR, env);
                env.exitBlock();
            } else {
                val = evalBuiltInFunction(p, env);
            }
            return val;
        }

        private Val evalBuiltInFunction(EApp eApp, Env<Val> env) {
            Val val = new VVoid();
            String funcId = eApp.id_;
            ListExp listexp = eApp.listexp_;
            Exp exp;
            Val expVal;
            //Scanner scanner = new Scanner(System.in);
            switch (funcId) {
                case "printInt":
                    exp = listexp.get(0);
                    expVal = exp.accept(EXP_VISITOR, env);
                    int i = ((VInteger) expVal).integer_;
                    System.out.println(i);
                    break;
                case "printDouble":
                    exp = listexp.get(0);
                    expVal = exp.accept(EXP_VISITOR, env);
                    expVal = EvalTool.coercionIfNeed(expVal, new Tdouble());
                    double d = ((VDouble) expVal).double_;
                    System.out.println(d);
                    break;
                case "readInt":
                    while (scanner.hasNext()) {
                        String next = scanner.next();
                        try {
                            int intInput = Integer.parseInt(next);
                            val = new VInteger(intInput);
                            break;
                        } catch (NumberFormatException nfe) {
                        }
                    }
                    break;
                case "readDouble":
                    while (scanner.hasNext()) {
                        String next = scanner.next();
                        try {
                            double doubleInput = Double.parseDouble(next);
                            val = new VDouble(doubleInput);
                            break;
                        } catch (NumberFormatException nfe) {
                        }
                    }
                    break;
                default:
                    throw new InterpreterException("Type check hit missing error, undefined function: " + funcId);
            }
            return val;
        }

        @Override
        public Val visit(EPIncr p, Env<Val> env) {
            Val val = env.findVariableValue(p.id_);
            if (val == null) {
                throw new InterpreterException("Can not identify variable value, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
            }
            Val increment = EvalTool.IncrementOrDecrement(val, true);
            env.updateVarValue(p.id_, increment);
            return val;
        }

        @Override
        public Val visit(EPDecr p, Env<Val> env) {
            Val val = env.findVariableValue(p.id_);
            if (val == null) {
                throw new InterpreterException("Can not identify variable value, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
            }
            Val decrement = EvalTool.IncrementOrDecrement(val, false);
            env.updateVarValue(p.id_, decrement);
            return val;
        }

        @Override
        public Val visit(EIncr p, Env<Val> env) {
            Val val = env.findVariableValue(p.id_);
            if (val == null) {
                throw new InterpreterException("Can not identify variable value, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
            }
            Val increment = EvalTool.IncrementOrDecrement(val, true);
            env.updateVarValue(p.id_, increment);
            return increment;
        }

        @Override
        public Val visit(EDecr p, Env<Val> env) {
            Val val = env.findVariableValue(p.id_);
            if (val == null) {
                throw new InterpreterException("Can not identify variable value, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
            }
            Val decrement = EvalTool.IncrementOrDecrement(val, false);
            env.updateVarValue(p.id_, decrement);
            return decrement;
        }

        @Override
        public Val visit(ETimes p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.TIMES);
        }

        @Override
        public Val visit(EDiv p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.DIV);
        }

        @Override
        public Val visit(EPlus p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.PLUS);
        }

        @Override
        public Val visit(EMinus p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.MINUS);
        }

        @Override
        public Val visit(ELt p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.LT);
        }

        @Override
        public Val visit(EGt p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.GT);
        }

        @Override
        public Val visit(ELtEq p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.LtEq);
        }

        @Override
        public Val visit(EGtEq p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.GtEq);
        }

        @Override
        public Val visit(EEq p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.Eq);
        }

        @Override
        public Val visit(ENEq p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            Val v2 = p.exp_2.accept(EXP_VISITOR, env);
            return EvalTool.eval(v1, v2, BOperator.NEq);
        }

        @Override
        public Val visit(EAnd p, Env<Val> env) {
            Val v1 = p.exp_1.accept(this, env);
            if (!EvalTool.isBooleanTrue(v1)) {
                return new VInteger(0);
            } else {
                return p.exp_2.accept(this, env);
            }
        }

        @Override
        public Val visit(EOr p, Env<Val> env) {
            Val v1 = p.exp_1.accept(EXP_VISITOR, env);
            if (EvalTool.isBooleanTrue(v1)) {
                return new VInteger(1);
            } else {
                return p.exp_2.accept(EXP_VISITOR, env);
            }
        }

        @Override
        public Val visit(EAss p, Env<Val> env) {
            Val val = p.exp_.accept(EXP_VISITOR, env);
            if (val instanceof VVoid || val instanceof VUndefined) {
                throw new InterpreterException("interpreter can not evaluate value with no specific type\n" + PrettyPrinter.print(p));
            }
            env.updateVarValue(p.id_, val);
            return val;
        }
    }
}
