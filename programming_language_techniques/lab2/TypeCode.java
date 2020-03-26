import CMM.Absyn.Tbool;
import CMM.Absyn.Tdouble;
import CMM.Absyn.Tint;
import CMM.Absyn.Tvoid;
import CMM.Absyn.Type;
public enum TypeCode {
	CInt("int"),
	CDouble("double"),
	CBool("bool"),
	CVoid("void");

	private String printStr;

	TypeCode(String printStr) {
		this.printStr = printStr;
	}

	public String getPrintStr() {
		return printStr;
	}

	public static TypeCode convertType(Type type) {
		if (type == null) {
			throw new RuntimeException("type can not be null");
		}
		if (type.getClass() == Tint.class) {
			return CInt;
		}
		if (type.getClass() == Tvoid.class) {
			return CVoid;
		}
		if (type.getClass() == Tdouble.class) {
			return CDouble;
		}
		if (type.getClass() == Tbool.class) {
			return CBool;
		}
		throw new RuntimeException("No matched type");
	}
}
