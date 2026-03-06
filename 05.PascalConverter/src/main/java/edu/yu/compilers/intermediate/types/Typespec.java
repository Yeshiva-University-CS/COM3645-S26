package edu.yu.compilers.intermediate.types;

import java.util.List;

import edu.yu.compilers.intermediate.symbols.SymTable;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;

public class Typespec {
    private final Form form; // type form
    private SymTableEntry identifier; // type identifier

    // ENUMERATION
    private List<SymTableEntry> enumerationConstants;

    // SUBRANGE
    private Typespec subrangeBaseType;
    private int subrangeMinValue;
    private int subrangeMaxValue;

    // ARRAY
    private Typespec arrayIndexType;
    private Typespec arrayElementType;
    private int arrayElementCount;

    // RECORD
    private SymTable recordSymTable;
    private String recordTypePath;

    static public Typespec newFunctionType(SymTableEntry entry) {
        Typespec type = new Typespec(Form.FUNCTION);
        type.setIdentifier(entry);
        return type;
    }

    public Typespec(Form form) {
        this.form = form;
    }

    public Form getForm() {
        return form;
    }

    public SymTableEntry getIdentifier() {
        return identifier;
    }

    public void setIdentifier(SymTableEntry identifier) {
        this.identifier = identifier;
    }

    /**
     * Return the base type of this type. For SUBRANGE, returns the subrange's base type.
     * Otherwise returns this type itself.
     */
    public Typespec baseType() {
        return form == Form.SUBRANGE ? subrangeBaseType : this;
    }

    // ENUMERATION getters/setters

    public List<SymTableEntry> getEnumerationConstants() {
        return enumerationConstants;
    }

    public void setEnumerationConstants(List<SymTableEntry> constants) {
        this.enumerationConstants = constants;
    }

    // SUBRANGE getters/setters

    public Typespec getSubrangeBaseType() {
        return subrangeBaseType;
    }

    public void setSubrangeBaseType(Typespec type) {
        this.subrangeBaseType = type;
    }

    public int getSubrangeMinValue() {
        return subrangeMinValue;
    }

    public void setSubrangeMinValue(int value) {
        this.subrangeMinValue = value;
    }

    public int getSubrangeMaxValue() {
        return subrangeMaxValue;
    }

    public void setSubrangeMaxValue(int value) {
        this.subrangeMaxValue = value;
    }

    // ARRAY getters/setters

    public Typespec getArrayIndexType() {
        return arrayIndexType;
    }

    public void setArrayIndexType(Typespec type) {
        this.arrayIndexType = type;
    }

    public Typespec getArrayElementType() {
        return arrayElementType;
    }

    public void setArrayElementType(Typespec type) {
        this.arrayElementType = type;
    }

    public int getArrayElementCount() {
        return arrayElementCount;
    }

    public void setArrayElementCount(int count) {
        this.arrayElementCount = count;
    }

    // RECORD getters/setters

    public SymTable getRecordSymTable() {
        return recordSymTable;
    }

    public void setRecordSymTable(SymTable symTable) {
        this.recordSymTable = symTable;
    }

    public String getRecordTypePath() {
        return recordTypePath;
    }

    public void setRecordTypePath(String path) {
        this.recordTypePath = path;
    }

    /**
     * Return the element base type of an array (the non-array leaf element type).
     */
    public Typespec getArrayBaseType() {
        Typespec elemType = arrayElementType;
        while (elemType != null && elemType.getForm() == Form.ARRAY) {
            elemType = elemType.getArrayElementType();
        }
        return elemType;
    }

    /**
     * Return true if this is a structured type (ARRAY or RECORD).
     */
    public boolean isStructured() {
        return form == Form.ARRAY || form == Form.RECORD;
    }

    public enum Form {
        SCALAR, ENUMERATION, SUBRANGE, ARRAY, RECORD, FUNCTION, DYNAMIC;

        public String toString() {
            return super.toString().toLowerCase();
        }
    }

    @Override
    public String toString() {
        return form.toString() + (identifier != null ? " " + identifier.getName() : "");
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (!(obj instanceof Typespec type)) {
            return false;
        } else {
            return form == type.form && (identifier == null ? type.identifier == null : identifier.equals(type.identifier));
        }
    }

}
