import java.util.List;

public class TypeCheckTool {
	public static boolean sizeMatch(List<?> l1, List<?> l2) {
		if (l1 == null && l2 == null) {
			return true;
		} else if (l1 != null && l2 != null) {
			return l1.size() == l2.size();
		} else {
			return false;
		}
	}
	
	public static boolean isListEmpty(List<?> l) {
		return l == null || l.size() == 0;
	}
	
	public static boolean isNumericType(TypeCode typeCode) {
		return typeCode == TypeCode.CInt || typeCode == TypeCode.CDouble;
	}

	public static boolean isDoubleType (TypeCode typeCode) {
		return typeCode == TypeCode.CDouble;
	}
	
	public static TypeCode toHigherNumericType(TypeCode t1, TypeCode t2) {
		TypeCode t = null;
		if (isNumericType(t1) && isNumericType(t2)) {
			if (isDoubleType(t1) || isDoubleType(t2)) {
				t = TypeCode.CDouble;
			} else {
				t = TypeCode.CInt;
			}
		}
		return t;
	}
	
	public static boolean isBoolType(TypeCode typeCode) {
		return typeCode == TypeCode.CBool;
	}
	
	public static boolean isTypeMatch(TypeCode t1, TypeCode t2) {
		if (t1 == t2) {
			return true;
		} else {
			if (t1 == TypeCode.CDouble && t2 == TypeCode.CInt) {
				return true;
			} else {
				return false;
			}
		}
	}
	
}
