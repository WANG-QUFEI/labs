import java.util.HashMap;
import java.util.Map;

import CMM.Absyn.ADecl;
import CMM.Absyn.Arg;
import CMM.Absyn.DFun;

/**
 * 
 * @author wqf
 *  Utility class for carry environment and function info.
 */
public class FunWithEnv {
	private Env env;
	private DFun fun;
	private Map<String, Integer> declTimes;
	public FunWithEnv(Env env, DFun fun) {
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
	public Env getEnv() {
		return env;
	}
	public void setEnv(Env env) {
		this.env = env;
	}
	public DFun getFun() {
		return fun;
	}
	public void setFun(DFun fun) {
		this.fun = fun;
	}
	public Map<String, Integer> getDeclTimes() {
		return declTimes;
	}
	public void setDeclTimes(Map<String, Integer> declTimes) {
		this.declTimes = declTimes;
	}
	
}
