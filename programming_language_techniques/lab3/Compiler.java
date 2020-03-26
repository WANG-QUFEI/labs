import CMM.Absyn.*;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;

public class Compiler {
    // The output of the compiler is a list of strings.
    LinkedList<String> output;
    private Env env;
    private final ProgramGenerator programGenerator = new ProgramGenerator();
    private final FunctionGenerator functionGenerator = new FunctionGenerator();
    private final StmGenerator stmGenerator = new StmGenerator();
    private final ExpGenerator expGenerator = new ExpGenerator();


    // Compile C-- AST to a .j source file (returned as String).
    // name should be just the class name without file extension.
    public String compile(String name, CMM.Absyn.Program p) {
        // Initialize output
        output = new LinkedList<>();

        // Output boilerplate
        output.add(".class public " + name + "\n");
        output.add(".super java/lang/Object\n");
        output.add("\n");
        output.add(".method public <init>()V\n");
        output.add("  .limit locals 1\n");
        output.add("\n");
        output.add("  aload_0\n");
        output.add("  invokespecial java/lang/Object/<init>()V\n");
        output.add("  return\n");
        output.add("\n");
        output.add(".end method\n");
        output.add("\n");
        output.add(".method public static main([Ljava/lang/String;)V\n");
        output.add("  .limit locals 1\n");
        output.add("  .limit stack  1\n");
        output.add("\n");
        output.add("  invokestatic " + name + "/main()I\n");
        output.add("  pop\n");
        output.add("  return\n");
        output.add("\n");
        output.add(".end method\n");
        output.add("\n");

        // TODO: compile AST, appending to output.
        env = new Env(new HashMap<>(), new LinkedList<>(), name);
        p.accept(programGenerator, null);
        // Concatenate strings in output to .j file content.
        StringBuilder jtext = new StringBuilder();
        for (String s : output) {
            jtext.append(s);
        }
        return jtext.toString();
    }

    private static class Env {
        private String fileName;
        private DFun currentFunction;
        private HashMap<String, FunType> functionSignature;
        private LinkedList<HashMap<String, Integer>> contexts;
        private LinkedList<Integer> blockExitAddress;
        private int addressCounter;
        private int labelCounter;
        private int stackCounter;
        private int localsLimit;
        private int stackLimit;

        public Env(HashMap<String, FunType> functionSignature, LinkedList<HashMap<String, Integer>> contexts, String fileName) {
            this.fileName = fileName;
            this.functionSignature = functionSignature;
            this.contexts = contexts;
            this.blockExitAddress = new LinkedList<>();
            this.addressCounter = 0;
            this.labelCounter = 0;
            this.stackCounter = 0;
            this.localsLimit = 0;
            this.stackLimit = 0;
        }

         void augmentDefaultFunctions() {
            Type intType = new Tint();
            Type doubleType = new Tdouble();
            Type voidType = new Tvoid();
            FunType f1 = new FunType(Collections.singletonList(intType), voidType, "Runtime");
            FunType f2 = new FunType(Collections.singletonList(doubleType), voidType, "Runtime");
            FunType f3 = new FunType(Collections.emptyList(), intType, "Runtime");
            FunType f4 = new FunType(Collections.emptyList(), doubleType, "Runtime");

             functionSignature.put("printInt", f1);
             functionSignature.put("printDouble", f2);
             functionSignature.put("readInt", f3);
             functionSignature.put("readDouble", f4);
        }

        void refresh() {
            currentFunction = null;
            contexts.clear();
            blockExitAddress.clear();
            addressCounter = 0;
            labelCounter = 0;
            localsLimit = 0;
            stackLimit = 0;
            stackCounter = 0;
        }

        void extend(DFun functionDefinition) {
            functionSignature.put(functionDefinition.id_, FunType.getFromDefinition(functionDefinition, fileName));
        }

        void extend(String varId, int varSize) {
            if (contexts.isEmpty()) {
                newBlock();
            }
            contexts.peek().put(varId, addressCounter);
            addressCounter += varSize;
            if (addressCounter > localsLimit) {
                localsLimit = addressCounter;
            }
        }

        FunType lookupFunction(String functionId) {
            return functionSignature.get(functionId);
        }

        int lookup(String var) {
            for (HashMap<String, Integer> map : contexts) {
                if (map.containsKey(var)) {
                    return map.get(var);
                }
            }
            return -1;
        }

        String newLabel() {
            return "L" + labelCounter++;
        }

        void newBlock() {
            contexts.push(new HashMap<>());
            blockExitAddress.push(addressCounter);
        }

        void exitBlock() {
            contexts.pop();
            addressCounter = blockExitAddress.pop();
        }

        void stackChange(int i) {
            stackCounter += i;
            if (stackLimit < stackCounter) {
                stackLimit = stackCounter;
            }
        }
    }

    private class ProgramGenerator implements Program.Visitor<Object, Object> {
        @Override
        public Object visit(PDefs p, Object arg) {
            if (!TypeCheckTool.isListEmpty(p.listdef_)) {
                addFunctionSignatures(p.listdef_);
                for (Def def : p.listdef_) {
                    env.refresh();
                    env.currentFunction = (DFun) def;
                    def.accept(functionGenerator, arg);
                }
            }
            return null;
        }

        private void addFunctionSignatures(ListDef listDef) {
            env.augmentDefaultFunctions();
            for (Def def : listDef) {
                DFun functionDefinition = (DFun) def;
                env.extend(functionDefinition);
            }
        }
    }

    private class FunctionGenerator implements Def.Visitor<Object, Object> {
        @Override
        public Object visit(DFun p, Object arg) {
            //1. generate boilerplate code for function head.
            StringBuilder stringBuilder = new StringBuilder();
            String head = CodeGenerationTool.functionHead(p);
            //2. extend env with function formal argument variables
            extendArgumentVariables(p.listarg_);
            //3. generate code for the statements.
            boolean hasReturnStm = false;
            if (!TypeCheckTool.isListEmpty(p.liststm_)) {
                for (Stm stm : p.liststm_) {
                    stm.accept(stmGenerator, stringBuilder);
                    if (stm instanceof SReturn) {
                        hasReturnStm = true;
                    }
                }
            }
            if (p.type_ instanceof Tvoid && !hasReturnStm) {
                stringBuilder.append(" return\n");
            }
            output.add(CodeGenerationTool.updateHead(head, env.localsLimit, env.stackLimit));
            output.add(stringBuilder.toString());
            output.add(".end method\n\n");
            return null;
        }

        private void extendArgumentVariables(ListArg listArg) {
            if (!TypeCheckTool.isListEmpty(listArg)) {
                for (Arg arg : listArg) {
                    ADecl adecl = (ADecl) arg;
                    env.extend(adecl.id_, CodeGenerationTool.variableSize(adecl.type_));
                }
            }
        }
    }

    private class StmGenerator implements Stm.Visitor<Object, StringBuilder> {
        @Override
        public Object visit(SExp p, StringBuilder arg) {
            p.exp_.accept(expGenerator, arg);
            ETyped eTyped = (ETyped) p.exp_;
            int typeSize = CodeGenerationTool.getTypeSize(eTyped.type_);
            if (typeSize == 1) {
                arg.append(" pop\n");
                env.stackChange(-1);
            } else if (typeSize == 2) {
                arg.append(" pop2\n");
                env.stackChange(-2);
            }
            return null;
        }

        @Override
        public Object visit(SDecl p, StringBuilder arg) {
            int size = CodeGenerationTool.variableSize(p.type_);
            for (String s : p.listid_) {
                env.extend(s, size);
            }
            return null;
        }

        @Override
        public Object visit(SInit sInit, StringBuilder arg) {
            env.extend(sInit.id_, CodeGenerationTool.variableSize(sInit.type_));
            sInit.exp_.accept(expGenerator, arg);
            ETyped eTyped = (ETyped) sInit.exp_;
            if (CodeGenerationTool.convertIntToDouble(sInit.type_, eTyped.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            int address = env.lookup(sInit.id_);
            arg.append(CodeGenerationTool.storeVariableValue(sInit.type_, address));
            if (sInit.type_ instanceof Tint || sInit.type_ instanceof Tbool) {
                env.stackChange(-1);
            } else {
                env.stackChange(-2);
            }
            return null;
        }

        @Override
        public Object visit(SReturn p, StringBuilder arg) {
            Type functionReturnType = env.currentFunction.type_;
            ETyped eTyped = (ETyped) p.exp_;
            p.exp_.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(functionReturnType, eTyped.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            TypeCode typeCode = TypeCode.convertType(functionReturnType);
            switch (typeCode) {
                case CDouble:
                    arg.append(" dreturn\n");
                    break;
                case CInt:
                case CBool:
                    arg.append(" ireturn\n");
                    break;
                case CVoid:
                    arg.append(" return\n");
            }
            return null;
        }

        @Override
        public Object visit(SWhile p, StringBuilder arg) {
            String testLabel = env.newLabel();
            String endLabel = env.newLabel();
            arg.append(testLabel).append(":\n");
            p.exp_.accept(expGenerator, arg);
            arg.append(" ifeq    ").append(endLabel).append("\n");
            env.newBlock();
            p.stm_.accept(stmGenerator, arg);
            env.exitBlock();
            arg.append(" goto    ").append(testLabel).append("\n");
            arg.append(endLabel).append(":\n");
            return null;
        }

        @Override
        public Object visit(SBlock p, StringBuilder arg) {
            env.newBlock();
            for (Stm stm : p.liststm_) {
                stm.accept(stmGenerator, arg);
            }
            env.exitBlock();
            return null;
        }

        @Override
        public Object visit(SIfElse p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            String labelFalse = env.newLabel();
            p.exp_.accept(expGenerator, arg);
            arg.append(" ifeq    ").append(labelFalse).append("\n");
            env.newBlock();
            p.stm_1.accept(stmGenerator, arg);
            env.exitBlock();
            //TODO test effect
            if (!CodeGenerationTool.endWithReturnStm(arg.toString())) {
                arg.append(" goto    ").append(labelTrue).append("\n");
            }
            arg.append(labelFalse).append(":\n");
            env.newBlock();
            p.stm_2.accept(stmGenerator, arg);
            env.exitBlock();
            arg.append(labelTrue).append(":\n");
            return null;
        }
    }

    private class ExpGenerator implements Exp.Visitor<Object, StringBuilder> {
        @Override
        public Object visit(EInt p, StringBuilder arg) {
            arg.append(CodeGenerationTool.compileInt(p.integer_));
            env.stackChange(1);
            return null;
        }

        @Override
        public Object visit(EDouble p, StringBuilder arg) {
            arg.append(CodeGenerationTool.compileDouble(p.double_));
            env.stackChange(2);
            return null;
        }

        @Override
        public Object visit(ETrue p, StringBuilder arg) {
            arg.append(" iconst_1\n");
            env.stackChange(1);
            return null;
        }

        @Override
        public Object visit(EFalse p, StringBuilder arg) {
            arg.append(" iconst_0\n");
            env.stackChange(1);
            return null;
        }

        @Override
        public Object visit(EId p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EApp p, StringBuilder arg) {
            FunType funType = env.lookupFunction(p.id_);
            int idx = 0;
            for (Exp exp : p.listexp_) {
                ETyped eTyped = (ETyped) exp;
                eTyped.accept(expGenerator, arg);
                if (CodeGenerationTool.convertIntToDouble(funType.getArgs().get(idx++), eTyped.type_)) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
            }
            arg.append(CodeGenerationTool.invokeFunction(funType, p.id_));
            env.stackChange(-CodeGenerationTool.stackShrinkSizeAfterFunctionInvocation(funType));
            return null;
        }

        @Override
        public Object visit(EPIncr p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EPDecr p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EIncr p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EDecr p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(ETimes p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EDiv p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EPlus p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EMinus p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(ELt p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            ETyped l = (ETyped) p.exp_1;
            ETyped r = (ETyped) p.exp_2;
            boolean b1 = l.type_ instanceof Tint;
            boolean b2 = r.type_ instanceof Tint;
            if (b1 && b2) {
                arg.append(" iconst_1\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                r.accept(expGenerator, arg);
                arg.append(" if_icmplt    ").append(labelTrue).append("\n");
                env.stackChange(-2);
                arg.append(" pop\n").append(" iconst_0\n").append(labelTrue).append(":\n");
            } else {
                arg.append(" iconst_0\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                if (b1) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                r.accept(expGenerator, arg);
                if (b2) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                arg.append(" dcmpg\n");
                arg.append(" ifge    ").append(labelTrue).append("\n");
                env.stackChange(-4);
                arg.append(" pop\n");
                arg.append(" iconst_1\n");
                arg.append(labelTrue).append(":\n");
            }
            return null;
        }

        @Override
        public Object visit(EGt p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            ETyped l = (ETyped) p.exp_1;
            ETyped r = (ETyped) p.exp_2;
            boolean b1 = l.type_ instanceof Tint;
            boolean b2 = r.type_ instanceof Tint;
            if (b1 && b2) {
                arg.append(" iconst_1\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                r.accept(expGenerator, arg);
                arg.append(" if_icmpgt    ").append(labelTrue).append("\n");
                env.stackChange(-2);
                arg.append(" pop\n").append(" iconst_0\n").append(labelTrue).append(":\n");
            } else {
                arg.append(" iconst_0\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                if (b1) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                r.accept(expGenerator, arg);
                if (b2) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                arg.append(" dcmpg\n");
                arg.append(" ifle    ").append(labelTrue).append("\n");
                env.stackChange(-4);
                arg.append(" pop\n");
                arg.append(" iconst_1\n");
                arg.append(labelTrue).append(":\n");
            }
            return null;
        }

        @Override
        public Object visit(ELtEq p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            ETyped l = (ETyped) p.exp_1;
            ETyped r = (ETyped) p.exp_2;
            boolean b1 = l.type_ instanceof Tint;
            boolean b2 = r.type_ instanceof Tint;
            if (b1 && b2) {
                arg.append(" iconst_1\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                r.accept(expGenerator, arg);
                arg.append(" if_icmple    ").append(labelTrue).append("\n");
                env.stackChange(-2);
                arg.append(" pop\n").append(" iconst_0\n").append(labelTrue).append(":\n");
            } else {
                arg.append(" iconst_0\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                if (b1) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                r.accept(expGenerator, arg);
                if (b2) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                arg.append(" dcmpg\n");
                arg.append(" ifgt    ").append(labelTrue).append("\n");
                env.stackChange(-4);
                arg.append(" pop\n");
                arg.append(" iconst_1\n");
                arg.append(labelTrue).append(":\n");
            }
            return null;
        }

        @Override
        public Object visit(EGtEq p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            ETyped l = (ETyped) p.exp_1;
            ETyped r = (ETyped) p.exp_2;
            boolean b1 = l.type_ instanceof Tint;
            boolean b2 = r.type_ instanceof Tint;
            if (b1 && b2) {
                arg.append(" iconst_1\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                r.accept(expGenerator, arg);
                arg.append(" if_icmpge    ").append(labelTrue).append("\n");
                env.stackChange(-2);
                arg.append(" pop\n").append(" iconst_0\n").append(labelTrue).append(":\n");
            } else {
                arg.append(" iconst_0\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                if (b1) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                r.accept(expGenerator, arg);
                if (b2) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                arg.append(" dcmpg\n");
                arg.append(" iflt    ").append(labelTrue).append("\n");
                env.stackChange(-4);
                arg.append(" pop\n");
                arg.append(" iconst_1\n");
                arg.append(labelTrue).append(":\n");
            }
            return null;
        }

        @Override
        public Object visit(EEq p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            ETyped l = (ETyped) p.exp_1;
            ETyped r = (ETyped) p.exp_2;
            boolean b1 = (l.type_ instanceof Tint || l.type_ instanceof Tbool);
            boolean b2 = (r.type_ instanceof Tint || r.type_ instanceof Tbool);
            if (b1 && b2) {
                arg.append(" iconst_1\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                r.accept(expGenerator, arg);
                arg.append(" if_icmpeq    ").append(labelTrue).append("\n");
                env.stackChange(-2);
                arg.append(" pop\n").append(" iconst_0\n").append(labelTrue).append(":\n");
            } else {
                arg.append(" iconst_0\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                if (b1) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                r.accept(expGenerator, arg);
                if (b2) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                arg.append(" dcmpg\n");
                arg.append(" ifne    ").append(labelTrue).append("\n");
                env.stackChange(-4);
                arg.append(" pop\n");
                arg.append(" iconst_1\n");
                arg.append(labelTrue).append(":\n");
            }
            return null;
        }

        @Override
        public Object visit(ENEq p, StringBuilder arg) {
            String labelTrue = env.newLabel();
            ETyped l = (ETyped) p.exp_1;
            ETyped r = (ETyped) p.exp_2;
            boolean b1 = (l.type_ instanceof Tint || l.type_ instanceof Tbool);
            boolean b2 = (r.type_ instanceof Tint || r.type_ instanceof Tbool);
            if (b1 && b2) {
                arg.append(" iconst_1\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                r.accept(expGenerator, arg);
                arg.append(" if_icmpne    ").append(labelTrue).append("\n");
                env.stackChange(-2);
                arg.append(" pop\n").append(" iconst_0\n").append(labelTrue).append(":\n");
            } else {
                arg.append(" iconst_0\n");
                env.stackChange(1);
                l.accept(expGenerator, arg);
                if (b1) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                r.accept(expGenerator, arg);
                if (b2) {
                    arg.append(" i2d\n");
                    env.stackChange(1);
                }
                arg.append(" dcmpg\n");
                arg.append(" ifeq    ").append(labelTrue).append("\n");
                env.stackChange(-4);
                arg.append(" pop\n");
                arg.append(" iconst_1\n");
                arg.append(labelTrue).append(":\n");
            }
            return null;
        }

        @Override
        public Object visit(EAnd p, StringBuilder arg) {
            String labelFalse = env.newLabel();
            arg.append(" iconst_0\n");
            env.stackChange(1);
            p.exp_1.accept(expGenerator, arg);
            arg.append(" ifeq    ").append(labelFalse).append("\n");
            env.stackChange(-1);
            p.exp_2.accept(expGenerator, arg);
            arg.append(" ifeq    ").append(labelFalse).append("\n");
            env.stackChange(-1);
            arg.append(" pop\n").append(" iconst_1\n");
            arg.append(labelFalse).append(":\n");
            return null;
        }

        @Override
        public Object visit(EOr p, StringBuilder arg) {
            String trueLabel = env.newLabel();
            arg.append(" iconst_1\n");
            env.stackChange(1);
            p.exp_1.accept(expGenerator, arg);
            arg.append(" ifne    ").append(trueLabel).append("\n");
            env.stackChange(-1);
            p.exp_2.accept(expGenerator, arg);
            arg.append(" ifne    ").append(trueLabel).append("\n");
            env.stackChange(-1);
            arg.append(" pop\n").append(" iconst_0\n");
            arg.append(trueLabel).append(":\n");
            return null;
        }

        @Override
        public Object visit(EAss p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(EConv p, StringBuilder arg) {
            return null;
        }

        @Override
        public Object visit(ETyped p, StringBuilder arg) {
            if (p.exp_ instanceof EId) {
                generateEId(p, arg);
            } else if (p.exp_ instanceof EPIncr) {
                generateEPincr(p, arg);
            } else if (p.exp_ instanceof EPDecr) {
                generateEPDecr(p, arg);
            } else if (p.exp_ instanceof EIncr) {
                generateEIncr(p, arg);
            } else if (p.exp_ instanceof EDecr) {
                generateEDecr(p, arg);
            } else if (p.exp_ instanceof ETimes) {
                generateETimes(p, arg);
            } else if (p.exp_ instanceof EDiv) {
                generateEDiv(p, arg);
            } else if (p.exp_ instanceof EPlus) {
                generateEPlus(p, arg);
            } else if (p.exp_ instanceof EMinus) {
                generateEMinus(p, arg);
            } else if (p.exp_ instanceof EAss) {
                generateEAss(p, arg);
            } else {
                p.exp_.accept(expGenerator, arg);
            }
            return null;
        }

        private void generateEId(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EId identityExp = (EId) p.exp_;
            int address = env.lookup(identityExp.id_);
            int typeSize = CodeGenerationTool.getTypeSize(type);
            arg.append(CodeGenerationTool.loadVariable(address, typeSize));
            env.stackChange(typeSize);
        }

        private void generateEPincr(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EPIncr epIncr = (EPIncr) p.exp_;
            int address = env.lookup(epIncr.id_);
            if (type instanceof Tint) {
                arg.append(CodeGenerationTool.loadVariable(address, 1));
                arg.append(" iinc ").append(address).append(" 1\n");
                env.stackChange(1);
            } else {
                arg.append(CodeGenerationTool.loadVariable(address, 2));
                arg.append(" dup2\n dconst_1\n dadd\n");
                arg.append(CodeGenerationTool.storeVariableValue(type, address));
                env.stackChange(6);
                env.stackChange(-4);
            }
        }

        private void generateEPDecr(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EPDecr epIncr = (EPDecr) p.exp_;
            int address = env.lookup(epIncr.id_);
            if (type instanceof Tint) {
                arg.append(CodeGenerationTool.loadVariable(address, 1));
                arg.append(" iinc ").append(address).append(" -1\n");
                env.stackChange(1);
            } else {
                arg.append(CodeGenerationTool.loadVariable(address, 2));
                arg.append(" dup2\n dconst_1\n dsub\n");
                arg.append(CodeGenerationTool.storeVariableValue(type, address));
                env.stackChange(6);
                env.stackChange(-4);
            }
        }

        private void generateEIncr(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EIncr epIncr = (EIncr) p.exp_;
            int address = env.lookup(epIncr.id_);
            if (type instanceof Tint) {
                arg.append(" iinc ").append(address).append(" 1\n");
                arg.append(CodeGenerationTool.loadVariable(address, 1));
                env.stackChange(1);
            } else {
                arg.append(CodeGenerationTool.loadVariable(address, 2));
                arg.append(" dconst_1\n dadd\n dup2\n");
                arg.append(CodeGenerationTool.storeVariableValue(type, address));
                env.stackChange(4);
                env.stackChange(-2);
            }
        }

        private void generateEDecr(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EDecr epIncr = (EDecr) p.exp_;
            int address = env.lookup(epIncr.id_);
            if (type instanceof Tint) {
                arg.append(" iinc ").append(address).append(" -1\n");
                arg.append(CodeGenerationTool.loadVariable(address, 1));
                env.stackChange(1);
            } else {
                arg.append(CodeGenerationTool.loadVariable(address, 2));
                arg.append(" dconst_1\n dsub\n dup2\n");
                arg.append(CodeGenerationTool.storeVariableValue(type, address));
                env.stackChange(4);
                env.stackChange(-2);
            }
        }

        private void generateETimes(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            ETimes eTimes = (ETimes) p.exp_;
            ETyped exp1Typed = (ETyped) eTimes.exp_1;
            ETyped exp2Typed = (ETyped) eTimes.exp_2;
            exp1Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp1Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            exp2Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp2Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            if (type instanceof Tint) {
                arg.append(" imul\n");
                env.stackChange(-1);
            } else {
                arg.append(" dmul\n");
                env.stackChange(-2);
            }
        }

        private void generateEDiv(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EDiv eDiv = (EDiv) p.exp_;
            ETyped exp1Typed = (ETyped) eDiv.exp_1;
            ETyped exp2Typed = (ETyped) eDiv.exp_2;
            exp1Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp1Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            exp2Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp2Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            if (type instanceof Tint) {
                arg.append(" idiv\n");
                env.stackChange(-1);
            } else {
                arg.append(" ddiv\n");
                env.stackChange(-2);
            }
        }

        private void generateEPlus(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EPlus eDiv = (EPlus) p.exp_;
            ETyped exp1Typed = (ETyped) eDiv.exp_1;
            ETyped exp2Typed = (ETyped) eDiv.exp_2;
            exp1Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp1Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            exp2Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp2Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            if (type instanceof Tint) {
                arg.append(" iadd\n");
                env.stackChange(-1);
            } else {
                arg.append(" dadd\n");
                env.stackChange(-2);
            }
        }

        private void generateEMinus(ETyped p, StringBuilder arg) {
            Type type = p.type_;
            EMinus eDiv = (EMinus) p.exp_;
            ETyped exp1Typed = (ETyped) eDiv.exp_1;
            ETyped exp2Typed = (ETyped) eDiv.exp_2;
            exp1Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp1Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            exp2Typed.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(type, exp2Typed.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            if (type instanceof Tint) {
                arg.append(" isub\n");
                env.stackChange(-1);
            } else {
                arg.append(" dsub\n");
                env.stackChange(-2);
            }
        }

        private void generateEAss(ETyped p, StringBuilder arg) {
            Type declaredType = p.type_;
            EAss eAss = (EAss) p.exp_;
            ETyped subTExp = (ETyped) eAss.exp_;
            subTExp.accept(expGenerator, arg);
            if (CodeGenerationTool.convertIntToDouble(declaredType, subTExp.type_)) {
                arg.append(" i2d\n");
                env.stackChange(1);
            }
            int address = env.lookup(eAss.id_);
            if (declaredType instanceof Tint || declaredType instanceof Tbool) {
                arg.append(" dup\n");
                env.stackChange(1);
                env.stackChange(-1);
            } else {
                arg.append(" dup2\n");
                env.stackChange(2);
                env.stackChange(-2);
            }
            arg.append(CodeGenerationTool.storeVariableValue(declaredType, address));
        }
    }



}
