import CMM.Absyn.*;
import CMM.PrettyPrinter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class TypeChecker {
	/**
	 * Type check whole program as sequence of functions.
	 * @param program the program to be type checked
	 */
	public Program typeCheck(Program program) {
		return program.accept(TypeCheckVisitor.PROGRAM_VISITOR, EnvForTypeCheck.emptyEnv());
	}

	private static class TypeCheckVisitor {

		private static final String MAIN = "main";
		private static final ProgramVisitor PROGRAM_VISITOR = new ProgramVisitor();
		private static final DefVisitor DEF_VISITOR = new DefVisitor();
		private static final ExpVisitor EXP_VISITOR = new ExpVisitor();
		private static final StmVisitor STM_VISITOR = new StmVisitor();
		private static final ArgVisitor ARG_VISITOR = new ArgVisitor();

		private static class ProgramVisitor implements Program.Visitor<Program, EnvForTypeCheck> {

			@Override
			public Program visit(PDefs pdefs, EnvForTypeCheck env) {
				if (pdefs.listdef_.size() > 0) {
					ListDef typeAnnotatedListDef = new ListDef();
					env.augmentDefaultFunctions();
					for (Def def : pdefs.listdef_) {
						env.addFunctionSignature((DFun) def);
					}
					checkMain(env);
					for (Def def : pdefs.listdef_) {
						typeAnnotatedListDef.add(def.accept(DEF_VISITOR, env));
					}
					return new PDefs(typeAnnotatedListDef);
				} else {
					throw new TypeException("empty program");
				}
			}

			private void checkMain(EnvForTypeCheck env) {
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


		}

		private static class DefVisitor implements Def.Visitor<Def, EnvForTypeCheck> {

			@Override
			public Def visit(DFun dfun, EnvForTypeCheck env) {
				Type typeAnnotatedFunctionType;
				String typeAnnotatedFunctionId = dfun.id_;
				ListArg typeAnnotatedFunctionListArg = new ListArg();
				ListStm typeAnnotatedFunctionListStm = new ListStm();
				//1. check function type
				typeAnnotatedFunctionType = dfun.type_.accept(new TypeVisitor(), dfun);
				env.newBlock();
				//2. check function arguments
				FunWithEnv fwe = new FunWithEnv(env, dfun);
				for (Arg arg: dfun.listarg_) {
					typeAnnotatedFunctionListArg.add(arg.accept(ARG_VISITOR, fwe));
				}
				env.setCurrentFunctionScope(dfun);
				//3. check function statements
				for (Stm stm: dfun.liststm_){
					typeAnnotatedFunctionListStm.add(stm.accept(STM_VISITOR, env));
				}
				env.exitBlock();
				return new DFun(typeAnnotatedFunctionType, typeAnnotatedFunctionId, typeAnnotatedFunctionListArg, typeAnnotatedFunctionListStm);
			}
		}

		public static class TypeVisitor implements Type.Visitor<Type, DFun> {

			@Override
			public Type visit(Tbool p, DFun arg) {
				return p;
			}

			@Override
			public Type visit(Tdouble p, DFun arg) {
				return p;
			}

			@Override
			public Type visit(Tint p, DFun arg) {
				return p;
			}

			@Override
			public Type visit(Tvoid p, DFun arg) {
				return p;
			}

		}

		public static class ArgVisitor implements Arg.Visitor<Arg, FunWithEnv> {
			@Override
			public Arg visit(ADecl p, FunWithEnv funWithEnv) {
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
				return p;
			}
		}

		private static class StmVisitor implements Stm.Visitor<Stm, EnvForTypeCheck> {

			@Override
			public Stm visit(SExp p, EnvForTypeCheck arg) {
				return new SExp(p.exp_.accept(EXP_VISITOR, arg));
			}

			@Override
			public Stm visit(SDecl p, EnvForTypeCheck env) {
				//1. can not have void type
				if (TypeCode.convertType(p.type_) == TypeCode.CVoid) {
					throw new TypeException("can not declare void type for variables, id: " + p.listid_ + "\n" + PrettyPrinter.print(p));
				}
				//2. for each variable, check it has not declared in the same context before, and add it to the context.
				for (String varId : p.listid_) {
					if (env.checkVarDeclaredCurrentContext(varId)) {
						throw new TypeException("variable already declared, id: " + varId + "\n" + PrettyPrinter.print(p));
					}
					env.addVarTypeCurrentContext(varId, p.type_);
				}
				return p;
			}

			@Override
			public Stm visit(SInit p, EnvForTypeCheck env) {
				if (env.checkVarDeclaredCurrentContext(p.id_)) {
					throw new TypeException("variable already declared, id: " + p.id_+ "\n" + PrettyPrinter.print(p));
				}
				TypeCode typeCodeDeclare = TypeCode.convertType(p.type_);
				if (typeCodeDeclare == TypeCode.CVoid) {
					throw new TypeException("can not declare void type for variable in statements, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				env.addVarTypeCurrentContext(p.id_, p.type_);
				ETyped eTyped = p.exp_.accept(EXP_VISITOR, env);
				TypeCode typeCodeExp = TypeCode.convertType(eTyped.type_);
				if (!TypeCheckTool.isTypeMatch(typeCodeDeclare, typeCodeExp)) {
					throw new TypeException("type doesn't match, expected [" + typeCodeDeclare.getPrintStr() + "], actual [" + typeCodeExp.getPrintStr() +"], id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				return new SInit(p.type_, p.id_, eTyped);
			}

			@Override
			public Stm visit(SReturn p, EnvForTypeCheck arg) {
				DFun functionScope = arg.getCurrentFunctionScope();
				Type returnType = functionScope.type_;
				TypeCode returnTypeConverted = TypeCode.convertType(returnType);
				ETyped eTyped = p.exp_.accept(EXP_VISITOR, arg);
				TypeCode expType = TypeCode.convertType(eTyped.type_);
				if (TypeCheckTool.isTypeMatch(returnTypeConverted, expType)) {
					return new SReturn(eTyped);
				} else {
					throw new TypeException("statement doesn't return right type, expected [" + returnTypeConverted.getPrintStr() + "], actual [" + expType.getPrintStr() +"], function id: " + functionScope.id_ + "\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public Stm visit(SWhile p, EnvForTypeCheck env) {
				ETyped eTyped = p.exp_.accept(EXP_VISITOR, env);
				TypeCode expType = TypeCode.convertType(eTyped.type_);
				if (!TypeCheckTool.isBoolType(expType)) {
					throw new TypeException("condition expression of While must be bool type.\n" + PrettyPrinter.print(p));
				}
				env.newBlock();
				Stm stm = p.stm_.accept(this, env);
				env.exitBlock();
				return new SWhile(eTyped, stm);
			}

			@Override
			public Stm visit(SBlock p, EnvForTypeCheck arg) {
				ListStm listStm = new ListStm();
				arg.newBlock();
				for (Stm stm : p.liststm_)  {
					listStm.add(stm.accept(this, arg));
				}
				arg.exitBlock();
				return new SBlock(listStm);
			}

			@Override
			public Stm visit(SIfElse p, EnvForTypeCheck arg) {
				ETyped eTyped = p.exp_.accept(EXP_VISITOR, arg);
				TypeCode expType = TypeCode.convertType(eTyped.type_);
				if (!TypeCheckTool.isBoolType(expType)) {
					throw new TypeException("condition expression in IfElse statement must be bool type\n " + PrettyPrinter.print(p));
				}
				arg.newBlock();
				Stm stm1 = p.stm_1.accept(this, arg);
				arg.exitBlock();
				arg.newBlock();
				Stm stm2 = p.stm_2.accept(this, arg);
				arg.exitBlock();
				return new SIfElse(eTyped, stm1, stm2);
			}
		}

		private static class ExpVisitor implements Exp.Visitor<ETyped, EnvForTypeCheck> {
			@Override
			public ETyped visit(EInt p, EnvForTypeCheck arg) {
				return TypeCheckTool.typeAnnotatedExpression(p, TypeCode.CInt);
			}

			@Override
			public ETyped visit(EDouble p, EnvForTypeCheck arg) {
				return TypeCheckTool.typeAnnotatedExpression(p, TypeCode.CDouble);
			}

			@Override
			public ETyped visit(ETrue p, EnvForTypeCheck arg) {
				return TypeCheckTool.typeAnnotatedExpression(p, TypeCode.CBool);
			}

			@Override
			public ETyped visit(EFalse p, EnvForTypeCheck arg) {
				return TypeCheckTool.typeAnnotatedExpression(p, TypeCode.CBool);
			}

			@Override
			public ETyped visit(EId p, EnvForTypeCheck env) {
				Type type = env.findVariableType(p.id_);
				if (type == null) {
					throw new TypeException("Undefined variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(p, TypeCode.convertType(type));
			}

			@Override
			public ETyped visit(EApp p, EnvForTypeCheck env) {
				//1. check function declaration.
				FunType functionType = env.findFunctionType(p.id_);
				if (functionType == null) {
					throw new TypeException("Undefined function: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				Type type = functionType.getVal();
				TypeCode returnType = TypeCode.convertType(type);
				//2. check formal arguments and actual arguments
				if (TypeCheckTool.sizeMatch(functionType.getArgs(), p.listexp_)) {
					if (!TypeCheckTool.isListEmpty(functionType.getArgs())) {
						List<Exp> typedExpList = new ArrayList<>();
						List<TypeCode> actualTypeList = new ArrayList<>();
						for (Exp exp : p.listexp_) {
							ETyped eTyped = exp.accept(this, env);
							actualTypeList.add(TypeCode.convertType(eTyped.type_));
							typedExpList.add(eTyped);
						}
						for (int i = 0; i < actualTypeList.size(); ++i) {
							Type parameterDeclaredType = functionType.getArgs().get(i);
							TypeCode parameterActualType = actualTypeList.get(i);
							TypeCode parameterDeclaredTypeCode = TypeCode.convertType(parameterDeclaredType);
							if (!TypeCheckTool.isTypeMatch(parameterDeclaredTypeCode, parameterActualType)) {
								throw new TypeException("function call argument type does not match, expect [" + parameterDeclaredTypeCode.getPrintStr() + "], actual [" + parameterActualType.getPrintStr() + "], invalid argument position: " + (i+1) + "\n" + PrettyPrinter.print(p));
							}
						}
						//replace argument expressions with type annotated expressions.
						p.listexp_.clear();
						p.listexp_.addAll(typedExpList);
					}
					return TypeCheckTool.typeAnnotatedExpression(p, returnType);
				} else {
					throw new TypeException("function call parameter size does not match, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public ETyped visit(EPIncr p, EnvForTypeCheck env) {
				Type type = env.findVariableType(p.id_);
				if (type == null) {
					throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				TypeCode convertType = TypeCode.convertType(type);
				if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
					throw new TypeException("invalid operation, '++' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				return new ETyped(p, type);
			}

			@Override
			public ETyped visit(EPDecr p, EnvForTypeCheck env) {
				Type type = env.findVariableType(p.id_);
				if (type == null) {
					throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				TypeCode convertType = TypeCode.convertType(type);
				if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
					throw new TypeException("invalid operation, '--' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(p, convertType);
			}

			@Override
			public ETyped visit(EIncr p, EnvForTypeCheck env) {
				Type type = env.findVariableType(p.id_);
				if (type == null) {
					throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				TypeCode convertType = TypeCode.convertType(type);
				if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
					throw new TypeException("invalid operation, '++' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(p, convertType);
			}

			@Override
			public ETyped visit(EDecr p, EnvForTypeCheck env) {
				Type type = env.findVariableType(p.id_);
				if (type == null) {
					throw new TypeException("Undeclared variable: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				TypeCode convertType = TypeCode.convertType(type);
				if (convertType != TypeCode.CInt && convertType != TypeCode.CDouble) {
					throw new TypeException("invalid operation, '--' operator can only be used on int,double, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(p, convertType);
			}

			@Override
			public ETyped visit(ETimes p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(TypeCode.convertType(eTyped1.type_), TypeCode.convertType(eTyped2.type_));
				if (higherNumericType == null) {
					throw new TypeException("invalid operation, '*' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				ETimes eTimes = new ETimes(eTyped1, eTyped2);
				return TypeCheckTool.typeAnnotatedExpression(eTimes, higherNumericType);
			}

			@Override
			public ETyped visit(EDiv p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(TypeCode.convertType(eTyped1.type_), TypeCode.convertType(eTyped2.type_));
				if (higherNumericType == null) {
					throw new TypeException("invalid operation, '/' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				EDiv eDiv = new EDiv(eTyped1, eTyped2);
				return TypeCheckTool.typeAnnotatedExpression(eDiv, higherNumericType);
			}

			@Override
			public ETyped visit(EPlus p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(TypeCode.convertType(eTyped1.type_), TypeCode.convertType(eTyped2.type_));
				if (higherNumericType == null) {
					throw new TypeException("invalid operation, '+' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				EPlus ePlus = new EPlus(eTyped1, eTyped2);
				return TypeCheckTool.typeAnnotatedExpression(ePlus, higherNumericType);
			}

			@Override
			public ETyped visit(EMinus p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				TypeCode higherNumericType = TypeCheckTool.toHigherNumericType(TypeCode.convertType(eTyped1.type_), TypeCode.convertType(eTyped2.type_));
				if (higherNumericType == null) {
					throw new TypeException("invalid operation, '-' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				EMinus eMinus = new EMinus(eTyped1, eTyped2);
				return TypeCheckTool.typeAnnotatedExpression(eMinus, higherNumericType);
			}

			@Override
			public ETyped visit(ELt p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if (!TypeCheckTool.isNumericType(TypeCode.convertType(eTyped1.type_)) || !TypeCheckTool.isNumericType(TypeCode.convertType(eTyped2.type_))) {
					throw new TypeException("invalid operation, '<' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(new ELt(eTyped1, eTyped2), TypeCode.CBool);
			}

			@Override
			public ETyped visit(EGt p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if (!TypeCheckTool.isNumericType(TypeCode.convertType(eTyped1.type_)) || !TypeCheckTool.isNumericType(TypeCode.convertType(eTyped2.type_))) {
					throw new TypeException("invalid operation, '>' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(new EGt(eTyped1, eTyped2), TypeCode.CBool);
			}

			@Override
			public ETyped visit(ELtEq p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if (!TypeCheckTool.isNumericType(TypeCode.convertType(eTyped1.type_)) || !TypeCheckTool.isNumericType(TypeCode.convertType(eTyped2.type_))) {
					throw new TypeException("invalid operation, '<=' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(new ELtEq(eTyped1, eTyped2), TypeCode.CBool);
			}

			@Override
			public ETyped visit(EGtEq p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if (!TypeCheckTool.isNumericType(TypeCode.convertType(eTyped1.type_)) || !TypeCheckTool.isNumericType(TypeCode.convertType(eTyped2.type_))) {
					throw new TypeException("invalid operation, '>=' operator can only be used on int or double\n" + PrettyPrinter.print(p));
				}
				return TypeCheckTool.typeAnnotatedExpression(new EGtEq(eTyped1, eTyped2), TypeCode.CBool);
			}

			@Override
			public ETyped visit(EEq p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if ((TypeCheckTool.isNumericType(TypeCode.convertType(eTyped1.type_)) && TypeCheckTool.isNumericType(TypeCode.convertType(eTyped2.type_)))
						|| (TypeCheckTool.isBoolType(TypeCode.convertType(eTyped1.type_)) && TypeCheckTool.isBoolType(TypeCode.convertType(eTyped2.type_)))) {
					return TypeCheckTool.typeAnnotatedExpression(new EEq(eTyped1, eTyped2), TypeCode.CBool);
				} else {
					throw new TypeException("invalid operation, '==' operator can only be used on (int,double) or bool\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public ETyped visit(ENEq p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if ((TypeCheckTool.isNumericType(TypeCode.convertType(eTyped1.type_)) && TypeCheckTool.isNumericType(TypeCode.convertType(eTyped2.type_)))
						|| (TypeCheckTool.isBoolType(TypeCode.convertType(eTyped1.type_)) && TypeCheckTool.isBoolType(TypeCode.convertType(eTyped2.type_)))) {
					return TypeCheckTool.typeAnnotatedExpression(new ENEq(eTyped1, eTyped2), TypeCode.CBool);
				} else {
					throw new TypeException("invalid operation, '!=' operator can only be used on (int,double) or bool\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public ETyped visit(EAnd p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if (TypeCheckTool.isBoolType(TypeCode.convertType(eTyped1.type_)) && TypeCheckTool.isBoolType(TypeCode.convertType(eTyped2.type_))) {
					return TypeCheckTool.typeAnnotatedExpression(new EAnd(eTyped1, eTyped2), TypeCode.CBool);
				} else {
					throw new TypeException("invalid operation, '&&' operator can only be used on bool.\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public ETyped visit(EOr p, EnvForTypeCheck arg) {
				ETyped eTyped1 = p.exp_1.accept(this, arg);
				ETyped eTyped2 = p.exp_2.accept(this, arg);
				if (TypeCheckTool.isBoolType(TypeCode.convertType(eTyped1.type_)) && TypeCheckTool.isBoolType(TypeCode.convertType(eTyped2.type_))) {
					return TypeCheckTool.typeAnnotatedExpression(new EOr(eTyped1, eTyped2), TypeCode.CBool);
				} else {
					throw new TypeException("invalid operation, '||' operator can only be used on bool\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public ETyped visit(EAss p, EnvForTypeCheck env) {
				Type variableType = env.findVariableType(p.id_);
				if (variableType == null) {
					throw new TypeException("undeclared variable, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
				TypeCode variableTypeConverted = TypeCode.convertType(variableType);
				ETyped eTyped = p.exp_.accept(this, env);
				if (TypeCheckTool.isTypeMatch(variableTypeConverted, TypeCode.convertType(eTyped.type_))) {
					EAss eAss = new EAss(p.id_, eTyped);
					return new ETyped(eAss, variableType);
				} else {
					throw new TypeException("assignment type doesn't match, id: " + p.id_ + "\n" + PrettyPrinter.print(p));
				}
			}

			@Override
			public ETyped visit(EConv p, EnvForTypeCheck arg) {
				return null;
			}

			@Override
			public ETyped visit(ETyped p, EnvForTypeCheck arg) {
				return null;
			}
		}
	}
}