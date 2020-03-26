import java.util.ArrayList;
import java.util.List;

import CMM.Absyn.ADecl;
import CMM.Absyn.Arg;
import CMM.Absyn.DFun;
import CMM.Absyn.Type;;
public class FunType {
	private List<Type> args;
	private Type val;
	
	public FunType(List<Type> args, Type val) {
		super();
		this.args = args;
		this.val = val;
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

	

	public List<Type> getArgs() {
		return args;
	}

	public void setArgs(List<Type> args) {
		this.args = args;
	}

	public Type getVal() {
		return val;
	}

	public void setVal(Type val) {
		this.val = val;
	}
	
	

}
