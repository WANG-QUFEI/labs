import CMM.Absyn.*;

import java.util.Objects;

public class CodeGenerationTool {
    public static final String JVM_TYPE_INT = "I";
    public static final String JVM_TYPE_DOUBLE = "D";
    public static final String JVM_TYPE_BOOLEAN = "Z";
    public static final String JVM_TYPE_VOID= "V";
    public static final int SHORT_ADDRESS_LIMIT = 3;

    public static String functionHead(DFun p) {
        StringBuilder sb = new StringBuilder(".method public static ");
        sb.append(p.id_).append("(");
        if (!TypeCheckTool.isListEmpty(p.listarg_)) {
            for (Arg arg : p.listarg_) {
                ADecl adecl = (ADecl) arg;
                TypeCode typeCode = TypeCode.convertType(adecl.type_);
                sb.append(jvmTypeSymbol(typeCode));
            }
        }
        sb.append(")");
        TypeCode returnType = TypeCode.convertType(p.type_);
        sb.append(jvmTypeSymbol(returnType));
        sb.append("\n .limit locals ${local_limit}\n .limit stack ${stack_limit}\n");
        return sb.toString();
    }

    public static String updateHead(String head, Integer localLimit, Integer stackLimit) {
        Objects.requireNonNull(head);
        Objects.requireNonNull(localLimit);
        Objects.requireNonNull(stackLimit);
        return head.replace("${local_limit}", localLimit.toString()).replace("${stack_limit}", stackLimit.toString());
    }

    public static int variableSize(Type varType) {
        Objects.requireNonNull(varType);
        int result;
        TypeCode typeCode = TypeCode.convertType(varType);
        switch (typeCode) {
            case CBool:
            case CInt:
                result = 1;
                break;
            case CDouble:
                result = 2;
                break;
            default:
                throw new RuntimeException("unknown variable type");
        }
        return result;
    }

    public static boolean convertIntToDouble(Type declaredType, Type actualType) {
        Objects.requireNonNull(declaredType);
        Objects.requireNonNull(actualType);
        TypeCode declaredTypeCode = TypeCode.convertType(declaredType);
        TypeCode actualTypeCode = TypeCode.convertType(actualType);
        return declaredTypeCode == TypeCode.CDouble && actualTypeCode == TypeCode.CInt;
    }

    public static String storeVariableValue(Type varType, int address) {
        StringBuilder stringBuilder = new StringBuilder();
        TypeCode typeCode = TypeCode.convertType(varType);
        if (typeCode == TypeCode.CInt || typeCode == TypeCode.CBool) {
            stringBuilder.append(" istore");
        } else if (typeCode == TypeCode.CDouble) {
            stringBuilder.append(" dstore");
        }
        if (address <= SHORT_ADDRESS_LIMIT) {
            stringBuilder.append("_").append(address).append("\n");
        } else {
            stringBuilder.append("    ").append(address).append("\n");
        }
        return stringBuilder.toString();
    }

    public static String compileInt(int i) {
        if (i == -1) {
            return " iconst_m1\n";
        } else if (i >= 0 && i <= 5) {
            return " iconst_" + i + "\n";
        } else if (i >= (-1 << 7) && i <= (1 << 7) - 1) {
            return " bipush    " + i + "\n";
        } else if (i >= (-1 << 15) && i <= (1 << 15) - 1) {
            return " sipush    " + i + "\n";
        } else {
            return " ldc    " + i + "\n";
        }
    }

    public static String compileDouble(double d) {
        if (d == 0.0) {
            return " dconst_0\n";
        } else if (d == 1.0) {
            return " dconst_1\n";
        } else {
            return " ldc2_w    " + d + "\n";
        }
    }

    public static String invokeFunction(FunType funType, String functionName) {
        StringBuilder sb = new StringBuilder();
        sb.append(" invokestatic ").append(funType.getClassName()).append("/");
        sb.append(functionName).append("(");
        for (Type type : funType.getArgs()) {
            sb.append(jvmTypeSymbol(TypeCode.convertType(type)));
        }
        sb.append(")").append(jvmTypeSymbol(TypeCode.convertType(funType.getVal()))).append("\n");
        return sb.toString();
    }

    public static String loadVariable(int address, int typeSize) {
        StringBuilder sb = new StringBuilder();
        if (typeSize == 1) {
            sb.append(" iload");
        } else if (typeSize == 2) {
            sb.append(" dload");
        } else {
            throw new RuntimeException("unknown typeSize: " + typeSize);
        }
        if (address >= 0 && address <= 3) {
            sb.append("_").append(address).append("\n");
        } else {
            sb.append("    ").append(address).append("\n");
        }
        return sb.toString();
    }

    public static int stackShrinkSizeAfterFunctionInvocation(FunType funType) {
        int result = 0;
        for (Type type : funType.getArgs()) {
            result += getTypeSize(type);
        }
        return result - getTypeSize(funType.getVal());
    }

    public static int getTypeSize(Type type) {
        int result = 0;
        if (type instanceof Tint || type instanceof Tbool) {
            result = 1;
        } else if (type instanceof Tdouble) {
            result = 2;
        } else if (!(type instanceof Tvoid)) {
            throw new RuntimeException("unknown type " + type.getClass());
        }
        return result;
    }

    public static boolean endWithReturnStm(String code) {
        boolean result = false;
        int idx = code.lastIndexOf("return");
        if (idx != -1) {
            if (code.substring(idx).matches("return\\s+")) {
                result = true;
            }
        }
        return result;
    }

    private static String jvmTypeSymbol(TypeCode typeCode) {
        Objects.requireNonNull(typeCode);
        String s;
        switch (typeCode) {
            case CInt:
                s = JVM_TYPE_INT;
                break;
            case CDouble:
                s = JVM_TYPE_DOUBLE;
                break;
            case CBool:
                s = JVM_TYPE_BOOLEAN;
                break;
            case CVoid:
                s = JVM_TYPE_VOID;
                break;
            default:
                s = "";
        }
        return s;
    }
}
