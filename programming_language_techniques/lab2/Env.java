import CMM.Absyn.*;
import CMM.PrettyPrinter;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * Environment object for type checking
 */
public class Env<V> {
	private HashMap<String, FunType> funSignatures;
	private HashMap<String, DFun> funDefinitions;
	private LinkedList<HashMap<String, V>> contexts;
	private DFun currentFunctionScope;
	
	public Env(HashMap<String, FunType> funSignatures, LinkedList<HashMap<String, V>> contexts, HashMap<String, DFun> funDefinitions) {
		super();
		this.funSignatures = funSignatures;
		this.funDefinitions = funDefinitions;
		this.contexts = contexts;
		this.currentFunctionScope = null;
	}
	
	public void checkInitialized() {
		if (funSignatures == null || funDefinitions == null || contexts == null) {
			throw new RuntimeException("Environment object has not initialized.");
		}
	}
	
	public void augmentDefaultFunctions() {
		checkInitialized();
		Type intType = new Tint();
		Type doubleType = new Tdouble();
		Type voidType = new Tvoid();
		FunType f1 = new FunType(Collections.singletonList(intType), voidType);
		FunType f2 = new FunType(Collections.singletonList(doubleType), voidType);
		FunType f3 = new FunType(Collections.emptyList(), intType);
		FunType f4 = new FunType(Collections.emptyList(), doubleType);
		
		funSignatures.put("printInt", f1);
		funSignatures.put("printDouble", f2);
		funSignatures.put("readInt", f3);
		funSignatures.put("readDouble", f4);
	}

	public static Env emptyEnv() {
		return new Env(new HashMap<>(), new LinkedList<>(), new HashMap<>());
	}
	
	public void addFunctionSignature(DFun funDefinition) {
		checkInitialized();
		if (this.funSignatures.containsKey(funDefinition.id_)) {
			throw new TypeException("function [ " + funDefinition.id_ + " ] already exists.\n" + PrettyPrinter.print(funDefinition));
		}
		this.funSignatures.put(funDefinition.id_, FunType.getFromDefinition(funDefinition));
	}
	
	public void addFunctionArgument(ADecl adelc) {
		checkInitialized();
		this.contexts.peek().put(adelc.id_, (V) adelc.type_);
	}

	private V findContextValById(String id) {
		V p = null;
		Iterator<HashMap<String, V>> iterator = contexts.iterator();
		while (iterator.hasNext()) {
			HashMap<String, V> next = iterator.next();
			if (next.containsKey(id)) {
				p = next.get(id);
				break;
			}
		}
		return p;
	}
	
	public V findVariableType(String id) {
		return findContextValById(id);
	}

	public V findVariableValue(String id) {
		return findContextValById(id);
	}

	public V findReturnVal() {
		checkInitialized();
		return contexts.peek().get(InterpreterVisitor.VAR_RETURN);
	}

	public boolean checkReturn() {
		checkInitialized();
		V v = findContextValById(InterpreterVisitor.VAR_RETURN);
		return (v != null && !(v instanceof VUndefined) && !(v instanceof VVoid));
	}
	
	public boolean checkVarDeclaredCurrentContext(String varId) {
		checkInitialized();
		boolean result = contexts.peek().containsKey(varId);
		return result;
	}
	
	public void addVarTypeCurrentContext(String varId, V varType) {
		checkInitialized();
		contexts.peek().put(varId, varType);
	}
	
	public FunType findFunctionType(String id) {
		FunType funType;
		funType = this.funSignatures.get(id);
		return funType;
	}

	public void updateVarValue(String varId, V varVal) {
		Iterator<HashMap<String, V>> iterator = contexts.iterator();
		while (iterator.hasNext()) {
			HashMap<String, V> next = iterator.next();
			if (next.containsKey(varId)) {
				V v = next.get(varId);
				next.put(varId, (V)EvalTool.coercionIfNeed((Val) varVal, v.getClass()));
				break;
			}
		}
	}

	public void addVarValueCurrentContext(String id, V val) {
		checkInitialized();
		contexts.peek().put(id, val);
	}

	public void newBlock() {
		this.contexts.push(new HashMap<>());
	}
	
	public void exitBlock() {
		this.contexts.pop();
	}

	public HashMap<String, FunType> getFunSignatures() {
		return funSignatures;
	}

	public HashMap<String, DFun> getFunDefinitions() {
		return funDefinitions;
	}

	public DFun getCurrentFunctionScope() {
		return currentFunctionScope;
	}

	public void setCurrentFunctionScope(DFun currentFunctionScope) {
		this.currentFunctionScope = currentFunctionScope;
	}
}
