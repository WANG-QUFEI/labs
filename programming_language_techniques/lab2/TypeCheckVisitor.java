import CMM.Absyn.*;
import CMM.PrettyPrinter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class TypeCheckVisitor {
	
	public static final String MAIN = "main";
	public static final ProgramVisitor PROGRAM_VISITOR = new ProgramVisitor();
	public static final DefVisitor DEF_VISITOR = new DefVisitor();
	public static final ExpTypeInferVisitor EXP_TYPE_INFER_VISITOR = new ExpTypeInferVisitor();
	public static final StmVisitor STM_VISITOR = new StmVisitor();
	public static final ArgVisitor ARG_VISITOR = new ArgVisitor();
	
	public static class ProgramVisitor implements Program.Visitor<Object, Env<Type>> {
		/**
		 * type check program.
		 */
		@Override
		public Object visit(PDefs pdefs, Env<Type> env) {
			if (pdefs.listdef_.size() > 0) {
				env.augmentDefaultFunctions();
				for (Def def : pdefs.listdef_) {
					env.addFunctionSignature((DFun) def);
				}
				checkMain(env);
				for (Def def : pdefs.listdef_) {
					def.accept(DEF_VISITOR, env);
				}
			} else {
				throw new TypeException("empty program");
			}
			return null;
		}
		
		private void checkMain(Env<Type> env) {
			/*//Get the environment with the most function information.
			Env<Type> env = envList.getLast();*/
			HashMap<String, FunType> funSignatures = env.getFunSignatures();
			if (!funSignatures.containsKey(MAIN)) {
				throw new TypeException("program must contain function 'int main()'");
			}
			FunType funType = funSignatures.get(MAIN);
			if (!TypeCheckTool.isListEmpty(funType.getArgs())) {
				throw new TypeException("'main' function can not have arguments");
			}
			if (TypeCode.convertType(funType.getVal()) != TypeCode.CInt) {
				throw new TypeException("'main' function must have return type 'int'");
			}
		}
		
		/**
		 *  extend environment with a function definition
		 * @param envOld old environment
		 * @param dfun function definition
		 * @return environment
		 */
		private Env<Type> extend(Env<Type> envOld, DFun dfun) {
			Env<Type> env = Env.emptyEnv();
			env.getFunSignatures().putAll(envOld.getFunSignatures());
			env.addFunctionSignature(dfun);
			return env;
		}

		
	}
	
	public static class DefVisitor implements Def.Visitor<Env<Type>, Env<Type>> {
		public Env<Type> visit(DFun dfun, Env<Type> env) {
			//1. check function type
	    	dfun.type_.accept(new TypeVisitor(), dfun);
	    	env.newBlock();
	    	//2. check function arguments
	    	FunWithEnv fwe = new FunWithEnv(env, dfun);
	        for (Arg arg: dfun.listarg_) {
	        	ADecl adecl = (ADecl) arg;
				adecl.accept(ARG_VISITOR, fwe);
	        }
	        env.setCurrentFunctionScope(dfun);
	        //3. check function statements
	        for (Stm stm: dfun.liststm_){
	        	stm.accept(STM_VISITOR, env);
	        }
	        env.exitBlock();
	        return env;
	    }
    }
	
	public static class TypeVisitor implements Type.Visitor<DFun, DFun> {

		@Override
		public DFun visit(Tbool p, DFun arg) {
			return arg;
		}

		@Override
		public DFun visit(Tdouble p, DFun arg) {
			return arg;
		}

		@Override
		public DFun visit(Tint p, DFun arg) {
			return arg;
		}

		@Override
		public DFun visit(Tvoid p, DFun arg) {
			return arg;
		}
		
	}
	
	public static class ArgVisitor implements Arg.Visitor<Env<Type>, FunWithEnv> {
		@Override
		public Env<Type> visit(ADecl p, FunWithEnv funWithEnv) {
			String functionId = funWithEnv.getFun().id_;
			//1. argument in function can't have type void.
			if (TypeCode.convertType(p.type_) == TypeCode.CVoid) {
				throw new TypeException("Argument with name [" + p.id_ + "] can not have type 'void' in  function [" + functionId + "] argument list!\n" + PrettyPrinter.print(p));
			}
			//2. argument with the same can only appears once
			if (funWithEnv.getDeclTimes().get(p.id_) > 0) {
				throw new TypeException("Argument with name [" + p.id_ + "] can not appear more than once in  function [" + functionId + "] argument list!\n" + PrettyPrinter.print(p));
			}
			funWithEnv.getDeclTimes().put(p.id_, 1);
			//3. extend environment with p
			funWithEnv.getEnv().addFunctionArgument(p);
			return funWithEnv.getEnv();
		}
	}
	
	public static class StmVisitor implements Stm.Visitor<Env<Type>, Env<Type>> {

		@Override
		public Env<Type> visit(SExp p, Env<Type> arg) {
			p.exp_.accept(EXP_TYPE_INFER_VISITOR, arg);
			return arg;
		}

		@Override
		public Env<Type> visit(SDecl p, Env<Type> arg) {
			//1. can not have void type
			if (TypeCode.convertType(p.type_) == TypeCode.CVoid) {
				throw new TypeException("Can not declare void type for variables, id: " + p.listid_ + "\n" + PrettyPrinter.print(p));
			}
			//2. for each variable, check it has not declared in the same context before, and add it to the context.
			for (String varId : p.listid_) {
				if (arg.checkVarDeclaredCurrentContext(varId)) {
					throw new TypeException("Variable already declared, id: " + varId + "\n" + PrettyPrinter.print(p));
				}
				arg.addVarTypeCurrentContext(varId, p.type_);
			}
			return arg;
		}

		@Override
		public Env<Type> visit(SInit p, Env<Type> arg) {
			//1. can not have void type
			TypeCode typeCodeDeclare = TypeCode.convertType(p.type_);
			if (arg.checkVarDeclaredCurrentContext(p.id_)) {
				throw new TypeException("variable already declared, id: " + p.id_+ "\n" + PrettyPrinter.print(p));
			}
			if (typeCodeDeclare == TypeCode.CVoid) {
				throw new TypeException("Can not declare void type for variable in statements, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			arg.addVarTypeCurrentContext(p.id_, p.type_);
			TypeCode typeCode = p.exp_.accept(EXP_TYPE_INFER_VISITOR, arg);
			if (!TypeCheckTool.isTypeMatch(typeCodeDeclare, typeCode)) {
				throw new TypeException("type doesn't match, expected [" + typeCodeDeclare.getPrintStr() + "], actual [" + typeCode.getPrintStr() +"], id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			return arg;
		}

		@Override
		public Env<Type> visit(SReturn p, Env<Type> arg) {
			DFun functionScope = arg.getCurrentFunctionScope();
			Type returnType = functionScope.type_;
			TypeCode returnTypeConverted = TypeCode.convertType(returnType);
			TypeCode expType = p.exp_.accept(EXP_TYPE_INFER_VISITOR, arg);
			if (TypeCheckTool.isTypeMatch(returnTypeConverted, expType)) {
				return arg;
			} else {
				throw new TypeException("statement doesn't return right type, expected [" + returnTypeConverted.getPrintStr() + "], actual [" + expType.getPrintStr() +"], function id: " + functionScope.id_ + "\n" + PrettyPrinter.print(p));
			}
		}

		@Override
		public Env<Type> visit(SWhile p, Env<Type> arg) {
			TypeCode expType = p.exp_.accept(EXP_TYPE_INFER_VISITOR, arg);
			if (!TypeCheckTool.isBoolType(expType)) {
				throw new TypeException("condition expression of While must be bool type.\n" + PrettyPrinter.print(p));
			}
			arg.newBlock();
			p.stm_.accept(this, arg);
			arg.exitBlock();
			return arg;
		}

		@Override
		public Env<Type> visit(SBlock p, Env<Type> arg) {
			arg.newBlock();
			for (Stm stm : p.liststm_)  {
				stm.accept(this, arg);
			}
			arg.exitBlock();
			return arg;
		}

		@Override
		public Env<Type> visit(SIfElse p, Env<Type> arg) {
			TypeCode expType = p.exp_.accept(EXP_TYPE_INFER_VISITOR, arg);
			if (!TypeCheckTool.isBoolType(expType)) {
				throw new TypeException("Condition expression in IfElse statement must be bool type\n " + PrettyPrinter.print(p));
			}
			arg.newBlock();
			p.stm_1.accept(this, arg);
			arg.exitBlock();
			arg.newBlock();
			p.stm_2.accept(this, arg);
			arg.exitBlock();
			return arg;
		}
		
	}
	
	public static class ExpTypeInferVisitor implements Exp.Visitor<TypeCode, Env<Type>> {

		@Override
		public TypeCode visit(EInt p, Env<Type> arg) {
			return TypeCode.CInt;
		}

		@Override
		public TypeCode visit(EDouble p, Env<Type> arg) {
			return TypeCode.CDouble;
		}

		@Override
		public TypeCode visit(ETrue p, Env<Type> arg) {
			return TypeCode.CBool;
		}

		@Override
		public TypeCode visit(EFalse p, Env<Type> arg) {
			return TypeCode.CBool;
		}

		@Override
		public TypeCode visit(EId p, Env<Type> env) {
			Type type = env.findVariableType(p.id_);
			if (type == null) {
				throw new TypeException("Undefined variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			return TypeCode.convertType(type);
		}

		@Override
		public TypeCode visit(EApp p, Env<Type> env) {
			//1. check function declaration.
			FunType functionType = env.findFunctionType(p.id_);
			if (functionType == null) {
				throw new TypeException("Undefined function: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			Type type = functionType.getVal();
			TypeCode functionReturnType = TypeCode.convertType(type);
			//2. check formal arguments and actual arguments
			if (TypeCheckTool.sizeMatch(functionType.getArgs(), p.listexp_)) {
				if (!TypeCheckTool.isListEmpty(functionType.getArgs())) {
					List<TypeCode> typeList = new ArrayList<>();
					for (Exp exp : p.listexp_) {
						TypeCode typeCode = exp.accept(this, env);
						typeList.add(typeCode);
					}
					for (int i = 0; i < typeList.size(); ++i) {
						Type parameterDeclaredType = functionType.getArgs().get(i);
						TypeCode parameterActualType = typeList.get(i);
						TypeCode parameterDeclaredTypeCode = TypeCode.convertType(parameterDeclaredType);
						if (!TypeCheckTool.isTypeMatch(parameterDeclaredTypeCode, parameterActualType)) {
							throw new TypeException("function call argument type does not match, expect [" + parameterDeclaredTypeCode.getPrintStr() + "], actual [" + parameterActualType.getPrintStr() + "], invalid argument position: " + (i+1) + "\n" + PrettyPrinter.print(p));
						}
					}
				}
				return functionReturnType;
			} else {
				throw new TypeException("function call parameter size does not match, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
		}

		@Override
		public TypeCode visit(EPIncr p, Env<Type> env) {
			Type type = env.findVariableType(p.id_);
			if (type == null) {
				throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			TypeCode convertType = TypeCode.convertType(type);
			if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
				throw new TypeException("invalid operation, '++' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			return convertType;
		}

		@Override
		public TypeCode visit(EPDecr p, Env<Type> env) {
			Type type = env.findVariableType(p.id_);
			if (type == null) {
				throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			TypeCode convertType = TypeCode.convertType(type);
			if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
				throw new TypeException("invalid operation, '--' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			return convertType;
		}

		@Override
		public TypeCode visit(EIncr p, Env<Type> env) {
			Type type = env.findVariableType(p.id_);
			if (type == null) {
				throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			TypeCode convertType = TypeCode.convertType(type);
			if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
				throw new TypeException("invalid operation, '++' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			return convertType;
		}

		@Override
		public TypeCode visit(EDecr p, Env<Type> env) {
			Type type = env.findVariableType(p.id_);
			if (type == null) {
				throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			TypeCode convertType = TypeCode.convertType(type);
			if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
				throw new TypeException("invalid operation, '--' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			return convertType;
		}

		@Override
		public TypeCode visit(ETimes p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(t1, t2);
			if (higherNumericType == null) {
				throw new TypeException("invalid operation, '*' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return higherNumericType;
		}

		@Override
		public TypeCode visit(EDiv p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(t1, t2);
			if (higherNumericType == null) {
				throw new TypeException("invalid operation, '/' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return higherNumericType;
		}

		@Override
		public TypeCode visit(EPlus p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(t1, t2);
			if (higherNumericType == null) {
				throw new TypeException("invalid operation, '+' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return higherNumericType;
		}

		@Override
		public TypeCode visit(EMinus p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(t1, t2);
			if (higherNumericType == null) {
				throw new TypeException("invalid operation, '-' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return higherNumericType;
		}

		@Override
		public TypeCode visit(ELt p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if (!TypeCheckTool.isNumericType(t1) || !TypeCheckTool.isNumericType(t2)) {
				throw new TypeException("invalid operation, '<' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return TypeCode.CBool;
		}

		@Override
		public TypeCode visit(EGt p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if (!TypeCheckTool.isNumericType(t1) || !TypeCheckTool.isNumericType(t2)) {
				throw new TypeException("invalid operation, '>' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return TypeCode.CBool;
		}

		@Override
		public TypeCode visit(ELtEq p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if (!TypeCheckTool.isNumericType(t1) || !TypeCheckTool.isNumericType(t2)) {
				throw new TypeException("invalid operation, '<=' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return TypeCode.CBool;
		}

		@Override
		public TypeCode visit(EGtEq p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if (!TypeCheckTool.isNumericType(t1) || !TypeCheckTool.isNumericType(t2)) {
				throw new TypeException("invalid operation, '>=' operator can only be used on int or double\n" + PrettyPrinter.print(p));
			}
			return TypeCode.CBool;
		}

		@Override
		public TypeCode visit(EEq p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if ((TypeCheckTool.isNumericType(t1) && TypeCheckTool.isNumericType(t2)) || (TypeCheckTool.isBoolType(t1) && TypeCheckTool.isBoolType(t2))) {
				return TypeCode.CBool;
			} else {
				throw new TypeException("invalid operation, '==' operator can only be used on (int,double) or bool\n" + PrettyPrinter.print(p));
			}
		}

		@Override
		public TypeCode visit(ENEq p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if ((TypeCheckTool.isNumericType(t1) && TypeCheckTool.isNumericType(t2)) || (TypeCheckTool.isBoolType(t1) && TypeCheckTool.isBoolType(t2))) {
				return TypeCode.CBool;
			} else {
				throw new TypeException("invalid operation, '!=' operator can only be used on (int,double) or bool\n" + PrettyPrinter.print(p));
			}
		}

		@Override
		public TypeCode visit(EAnd p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if (TypeCheckTool.isBoolType(t1) && TypeCheckTool.isBoolType(t2)) {
				return TypeCode.CBool;
			} else {
				throw new TypeException("invalid operation, '&&' operator can only be used on bool.\n" + PrettyPrinter.print(p));
			}
		}

		@Override
		public TypeCode visit(EOr p, Env<Type> arg) {
			TypeCode t1 = p.exp_1.accept(this, arg);
			TypeCode t2 = p.exp_2.accept(this, arg);
			if (TypeCheckTool.isBoolType(t1) && TypeCheckTool.isBoolType(t2)) {
				return TypeCode.CBool;
			} else {
				throw new TypeException("invalid operation, '||' operator can only be used on bool\n" + PrettyPrinter.print(p));
			}
		}

		@Override
		public TypeCode visit(EAss p, Env<Type> env) {
			Type variableType = env.findVariableType(p.id_);
			if (variableType == null) {
				throw new TypeException("undeclared variable, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
			TypeCode expType = p.exp_.accept(this, env);
			TypeCode variableTypeConverted = TypeCode.convertType(variableType);
			if (TypeCheckTool.isTypeMatch(variableTypeConverted, expType)) {
				return variableTypeConverted;
			} else {
				throw new TypeException("assignment type doesn't match, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
			}
		}	
	}
}
