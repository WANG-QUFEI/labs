import CMM.Absyn.ADecl;
import CMM.Absyn.Arg;
import CMM.Absyn.DFun;
import CMM.Absyn.Type;

import java.util.ArrayList;
import java.util.List;

;
public class FunType {
	private List<Type> args;
	private Type val;
	private String className;
	
	public FunType(List<Type> args, Type val) {
		super();
		this.args = args;
		this.val = val;
		this.className = "";
	}

	public FunType(List<Type> args, Type val, String className) {
		this.args = args;
		this.val = val;
		this.className = className;
	}

	public static FunType getFromDefinition(DFun functionDefinition) {
		FunType funType = new FunType(new ArrayList<>(), functionDefinition.type_);
		if (functionDefinition.listarg_ != null) {
			for (Arg arg : functionDefinition.listarg_) {
				funType.args.add(((ADecl) arg).type_);
			}
		}
		return funType;
	}

	public static FunType getFromDefinition(DFun functionDefinition, String className) {
		FunType funType = new FunType(new ArrayList<>(), functionDefinition.type_, className);
		if (functionDefinition.listarg_ != null) {
			for (Arg arg : functionDefinition.listarg_) {
				funType.args.add(((ADecl) arg).type_);
			}
		}
		return funType;
	}

	public List<Type> getArgs() {
		return args;
	}

	public Type getVal() {
		return val;
	}

	public void setVal(Type val) {
		this.val = val;
	}

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}

}
