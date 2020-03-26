import CMM.Absyn.*;

public class EvalTool {
    private static final Val d_void_val = new VVoid();
    private static final Val d_int_val = new VInteger(-1);
    private static final Val d_double_val = new VDouble(-1.0);
    private static final Val d_bool_val = new VInteger(-1);

    public static boolean isBooleanTrue(Val val) {
        boolean b = false;
        if (val instanceof VInteger) {
            b = ((VInteger) val).integer_ != 0;
        }
        return b;
    }

    public static Val copy(Val val) {
        Val result = val;
        if (val instanceof VInteger || val instanceof VDouble) {
            if (val instanceof VInteger) {
                result = new VInteger(((VInteger) val).integer_);
            } else {
                result = new VDouble(((VDouble) val).double_);
            }
        }
        return result;
    }

    public static Val eval(Exp exp) {
        Val v = null;
        if (exp instanceof EInt) {
            EInt e = (EInt) exp;
            v = new VInteger(e.integer_);
        } else if (exp instanceof EDouble) {
            EDouble e = (EDouble) exp;
            v = new VDouble(e.double_);
        } else if (exp instanceof EFalse) {
            v = new VInteger(0);
        } else if (exp instanceof ETrue) {
            v = new VInteger(1);
        }
        return v;
    }

    public static Val IncrementOrDecrement(Val o, boolean b) {
        Val result;
        if (o instanceof VInteger) {
            VInteger vInteger = (VInteger) o;
            if (b) {
                result = new VInteger(vInteger.integer_ + 1);
            } else {
                result = new VInteger(vInteger.integer_ - 1);
            }
        } else if (o instanceof VDouble) {
            VDouble vDouble = (VDouble)o;
            if (b) {
                result = new VDouble(vDouble.double_ + 1);
            } else {
                result = new VDouble(vDouble.double_ - 1);
            }
        } else {
            throw new RuntimeException("type checker missing error, incr/decr type not match");
        }
        return result;
    }

    public static Val eval(Val v1, Val v2, BOperator operator) {
        Val val;
        if (isNumericValue(v1) && isNumericValue(v2)) {
            if (v1 instanceof VInteger && v2 instanceof VInteger) {
                val = integerOperation(((VInteger) v1).integer_, ((VInteger) v2).integer_, operator);
            } else {
                val = doubleOperation(toDouble(v1), toDouble(v2), operator);
            }
        } else {
            throw new RuntimeException("Type checker hit missing error, '*' operand type not match");
        }
        return val;
    }

    public static boolean isNumericValue(Val val) {
        return val instanceof VInteger || val instanceof VDouble;
    }

    private static Val integerOperation(int v1, int v2, BOperator operator) {
        Val val;
        switch (operator) {
            case PLUS:
                val = new VInteger(v1 + v2);
                break;
            case MINUS:
                val = new VInteger(v1 - v2);
                break;
            case TIMES:
                val = new VInteger(v1 * v2);
                break;
            case DIV:
                val = new VInteger(v1 / v2);
                break;
            default:
                val = evalComparison(v1, v2, operator);
        }
        return val;
    }

    private static Val doubleOperation(double v1, double v2, BOperator operator) {
        Val val;
        switch (operator) {
            case PLUS:
                val = new VDouble(v1 + v2);
                break;
            case MINUS:
                val = new VDouble(v1 - v2);
                break;
            case TIMES:
                val = new VDouble(v1 * v2);
                break;
            case DIV:
                val = new VDouble(v1 / v2);
                break;
            default:
                val = evalComparison(v1, v2, operator);
        }
        return val;
    }

    private static Val evalComparison(double d1, double d2, BOperator operator) {
        Val val = null;
        switch (operator) {
            case LT:
                if (d1 < d2) {
                    val = new VInteger(1);
                } else {
                    val = new VInteger(0);
                }
                break;
            case GT:
                if (d1 > d2) {
                    val = new VInteger(1);
                } else {
                    val = new VInteger(0);
                }
                break;
            case LtEq:
                if (d1 <= d2) {
                    val = new VInteger(1);
                } else {
                    val = new VInteger(0);
                }
                break;
            case GtEq:
                if (d1 >= d2) {
                    val = new VInteger(1);
                } else {
                    val = new VInteger(0);
                }
                break;
            case Eq:
                if (d1 == d2) {
                    val = new VInteger(1);
                } else {
                    val = new VInteger(0);
                }
                break;
            case NEq:
                if (d1 != d2) {
                    val = new VInteger(1);
                } else {
                    val = new VInteger(0);
                }
                break;
        }
        return val;
    }

    public static Val coercionIfNeed(Val val, Type type) {
        if (type instanceof Tdouble && val instanceof VInteger) {
            VInteger vInteger = (VInteger) val;
            return new VDouble(vInteger.integer_.doubleValue());
        } else {
            return val;
        }
    }

    public static Val coercionIfNeed(Val val, Class<?> clz) {
        if (clz == VDouble.class && val instanceof VInteger) {
            VInteger vInteger = (VInteger) val;
            return new VDouble(vInteger.integer_.doubleValue());
        } else {
            return val;
        }
    }

    public static Val getDefaultValByType(Type type) {
        if (type instanceof Tint) {
            return d_int_val;
        } else if (type instanceof Tdouble) {
            return d_double_val;
        } else if (type instanceof Tvoid) {
            return d_void_val;
        } else if (type instanceof Tbool) {
            return d_bool_val;
        } else {
            return new VUndefined();
        }
    }

    public static boolean uninitializedVal(Val val) {
        return val == null || val == d_bool_val || val == d_int_val || val == d_double_val || val == d_void_val;
    }

    private static double toDouble(Val val) {
        double d;
        if (val instanceof VInteger) {
            d = ((VInteger) val).integer_;
        } else {
            d = ((VDouble) val).double_;
        }
        return d;
    }

}
