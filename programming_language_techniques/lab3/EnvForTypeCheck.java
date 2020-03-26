import CMM.Absyn.*;
import CMM.PrettyPrinter;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * Environment object for type checking
 */
public class EnvForTypeCheck {
	private HashMap<String, FunType> funSignatures;
	private LinkedList<HashMap<String, Type>> contexts;
	private DFun currentFunctionScope;
	
	public EnvForTypeCheck(HashMap<String, FunType> funSignatures, LinkedList<HashMap<String, Type>> contexts) {
		super();
		this.funSignatures = funSignatures;
		this.contexts = contexts;
		this.currentFunctionScope = null;
	}
	
	public void checkInitialized() {
		if (funSignatures == null || contexts == null) {
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

	public static EnvForTypeCheck emptyEnv() {
		return new EnvForTypeCheck(new HashMap<>(), new LinkedList<>());
	}
	
	public void addFunctionSignature(DFun funDefinition) {
		checkInitialized();
		if (this.funSignatures.containsKey(funDefinition.id_)) {
			throw new TypeException("function [ " + funDefinition.id_ + " ] already exists.\n" + PrettyPrinter.print(funDefinition));
		}
		this.funSignatures.put(funDefinition.id_, FunType.getFromDefinition(funDefinition));
	}
	
	public void addFunctionArgument(ADecl adecl) {
		checkInitialized();
		this.contexts.peek().put(adecl.id_, adecl.type_);
	}
	
	public Type findVariableType(String id) {
		Type p = null;
		for (HashMap<String, Type> next : contexts) {
			if (next.containsKey(id)) {
				p = next.get(id);
				break;
			}
		}
		return p;
	}
	
	public boolean checkVarDeclaredCurrentContext(String varId) {
		checkInitialized();
		return contexts.peek().containsKey(varId);
	}
	
	public void addVarTypeCurrentContext(String varId, Type varType) {
		checkInitialized();
		contexts.peek().put(varId, varType);
	}
	
	public FunType findFunctionType(String id) {
		FunType funType;
		funType = this.funSignatures.get(id);
		return funType;
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

	public DFun getCurrentFunctionScope() {
		return currentFunctionScope;
	}

	public void setCurrentFunctionScope(DFun currentFunctionScope) {
		this.currentFunctionScope = currentFunctionScope;
	}
}
