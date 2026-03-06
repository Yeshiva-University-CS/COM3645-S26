package edu.yu.compilers.intermediate.types;

import edu.yu.compilers.intermediate.symbols.Predefined;

public class TypeChecker {

    public static boolean isFunction(Typespec type) { return type.getForm() == Typespec.Form.FUNCTION; }

    public static boolean isDynamic(Typespec type) {
        return type.equals(Predefined.noneType);
    }

    public static boolean atLeastOneIsDynamic(Typespec type1, Typespec type2) {
        return isDynamic(type1) || isDynamic(type2);
    }

    public static boolean isInteger(Typespec type) {
        return type.equals(Predefined.integerType);
    }

    public static boolean areIntegers(Typespec type1, Typespec type2) {
        return isInteger(type1) && isInteger(type2);
    }

    public static boolean isReal(Typespec type) {
        return type.equals(Predefined.realType);
    }

    public static boolean areReals(Typespec type1, Typespec type2) {
        return isReal(type1) && isReal(type2);
    }

    public static boolean isString(Typespec type) {
        return type.equals(Predefined.stringType);
    }

    public static boolean areStrings(Typespec type1, Typespec type2) {
        return isString(type1) && isString(type2);
    }

    public static boolean isBoolean(Typespec type) {
        return type.equals(Predefined.booleanType);
    }

    public static boolean areBooleans(Typespec type1, Typespec type2) {
        return isBoolean(type1) && isBoolean(type2);
    }

    public static boolean isNumeric(Typespec type) {
        return isInteger(type) || isReal(type);
    }

    public static boolean areNumeric(Typespec type1, Typespec type2) {
        return isNumeric(type1) && isNumeric(type2);
    }

    public static boolean areAssignmentCompatible(Typespec lhsType, Typespec rhsType) {
        return lhsType.equals(rhsType)
                || atLeastOneIsDynamic(lhsType, rhsType)
                || isReal(lhsType) && isInteger(rhsType);
    }

    public static boolean areComparable(Typespec leftType, Typespec rightType) {
        return atLeastOneIsDynamic(leftType, rightType) ||
                areNumeric(leftType, rightType) ||
                areStrings(leftType, rightType);
    }

    public static boolean areEquatable(Typespec leftType, Typespec rightType) {
        return atLeastOneIsDynamic(leftType, rightType) || leftType.equals(rightType);
    }

    public static boolean supportsAdd(Typespec type) {
        return isDynamic(type) ||
                isNumeric(type) ||
                isString(type);
    }

    public static boolean supportsSubtract(Typespec type) {
        return isDynamic(type) || isNumeric(type);
    }

    public static boolean supportsMultDiv(Typespec type) {
        return isDynamic(type) || isNumeric(type);
    }

    public static boolean canAdd(Typespec leftType, Typespec rightType) {
        return atLeastOneIsDynamic(leftType, rightType) ||
                areNumeric(leftType, rightType) ||
                areStrings(leftType, rightType);
    }

    public static boolean canSubtract(Typespec leftType, Typespec rightType) {
        return atLeastOneIsDynamic(leftType, rightType) || areNumeric(leftType, rightType);
    }

    public static boolean canMultDiv(Typespec leftType, Typespec rightType) {
        return atLeastOneIsDynamic(leftType, rightType) || areNumeric(leftType, rightType);
    }

    public static boolean atLeastOneIsReal(Typespec type, Typespec type2) {
        return isReal(type) || isReal(type2);
    }

    // Pascal-specific aliases used by the Pascal semantic analyzer

    /** Alias for areIntegers — both operands are integer. */
    public static boolean areBothInteger(Typespec type1, Typespec type2) {
        return areIntegers(type1, type2);
    }

    /** Alias for atLeastOneIsReal. */
    public static boolean isAtLeastOneReal(Typespec type1, Typespec type2) {
        return atLeastOneIsReal(type1, type2);
    }

    /** Alias for areStrings — both operands are string. */
    public static boolean areBothString(Typespec type1, Typespec type2) {
        return areStrings(type1, type2);
    }

    /** Alias for isNumeric — type is integer or real. */
    public static boolean isIntegerOrReal(Typespec type) {
        return isNumeric(type);
    }

    /** Returns true if type is a char (Pascal character) type. */
    public static boolean isChar(Typespec type) {
        return type.equals(Predefined.charType);
    }

    /**
     * Returns true if two types are compatible for comparison operators
     * (relational: <, >, <=, >=). Compatible means both numeric, both chars,
     * both booleans, or both the same enumeration type.
     */
    public static boolean areComparisonCompatible(Typespec type1, Typespec type2) {
        if (atLeastOneIsDynamic(type1, type2)) return true;
        if (areNumeric(type1, type2)) return true;
        if (areStrings(type1, type2)) return true;
        if (isChar(type1) && isChar(type2)) return true;
        if (areBooleans(type1, type2)) return true;
        // Same enumeration type
        if (type1.getForm() == Typespec.Form.ENUMERATION && type1.equals(type2)) return true;
        return false;
    }
}
