import CMM.Absyn.ADecl;
import CMM.Absyn.Arg;
import CMM.Absyn.DFun;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author wqf
 *  Utility class for carry environment and function info.
 */
public class FunWithEnv {
	private EnvForTypeCheck env;
	private DFun fun;
	private Map<String, Integer> declTimes;
	public FunWithEnv(EnvForTypeCheck env, DFun fun) {
		super();
		this.env = env;
		this.fun = fun;
		declTimes = new HashMap<>();
		if (!TypeCheckTool.isListEmpty(fun.listarg_)) {
			for (Arg arg : fun.listarg_) {
				ADecl adecl = (ADecl) arg;
				declTimes.put( adecl.id_, 0);
			}
		}
	}
	public EnvForTypeCheck getEnv() {
		return env;
	}
	public DFun getFun() {
		return fun;
	}
	public Map<String, Integer> getDeclTimes() {
		return declTimes;
	}
}
