/**
 * <h1>Predefined</h1>
 * <p>Enter the predefined Pascal types, identifiers, and constants
 * into the symbol table.</p>
 * <p>Adapted from:</p>
 * <p>Copyright (c) 2020 by Ronald Mak</p>
 * <p>For instructional purposes only.  No warranties.</p>
 */

package edu.yu.compilers.intermediate.symbols;

import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.CONSTANT;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.PROCEDURE;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.TYPE;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.RoutineCode.BUILT_IN;
import static edu.yu.compilers.intermediate.types.Typespec.Form.*;

import edu.yu.compilers.intermediate.types.Typespec;

public class Predefined {
    // Predefined types.
    public static Typespec integerType;
    public static Typespec realType;
    public static Typespec booleanType;
    public static Typespec charType;
    public static Typespec stringType;
    public static Typespec noneType;

    // Predefined identifiers.
    public static SymTableEntry integerId;
    public static SymTableEntry realId;
    public static SymTableEntry booleanId;
    public static SymTableEntry charId;
    public static SymTableEntry stringId;
    public static SymTableEntry falseId;
    public static SymTableEntry trueId;
    public static SymTableEntry noneId;

    // Standard routine identifiers.
    public static SymTableEntry writeId;
    public static SymTableEntry writelnId;
    public static SymTableEntry readId;
    public static SymTableEntry readlnId;

    /**
     * Initialize a symbol table stack with predefined identifiers.
     *
     * @param symTableStack the symbol table stack to initialize.
     */
    public static void initialize(SymTableStack symTableStack) {
        initializeTypes(symTableStack);
        initializeConstants(symTableStack);
        initializeStandardRoutines(symTableStack);
    }

    /**
     * Initialize the predefined types.
     *
     * @param symTableStack the symbol table stack to initialize.
     */
    private static void initializeTypes(SymTableStack symTableStack) {
        // Type integer.
        integerId = symTableStack.enterLocal("integer", TYPE);
        integerType = new Typespec(SCALAR);
        integerType.setIdentifier(integerId);
        integerId.setType(integerType);

        // Type real.
        realId = symTableStack.enterLocal("real", TYPE);
        realType = new Typespec(SCALAR);
        realType.setIdentifier(realId);
        realId.setType(realType);

        // Type boolean.
        booleanId = symTableStack.enterLocal("boolean", TYPE);
        booleanType = new Typespec(SCALAR);
        booleanType.setIdentifier(booleanId);
        booleanId.setType(booleanType);

        // Type char.
        charId = symTableStack.enterLocal("char", TYPE);
        charType = new Typespec(SCALAR);
        charType.setIdentifier(charId);
        charId.setType(charType);

        // Type string.
        stringId = symTableStack.enterLocal("string", TYPE);
        stringType = new Typespec(SCALAR);
        stringType.setIdentifier(stringId);
        stringId.setType(stringType);

        // Undefined type.
        noneId = symTableStack.enterLocal("none", TYPE);
        noneType = new Typespec(DYNAMIC);
        noneType.setIdentifier(noneId);
        noneId.setType(noneType);
    }

    /**
     * Initialize the predefined constants.
     *
     * @param symTabStack the symbol table stack to initialize.
     */
    private static void initializeConstants(SymTableStack symTabStack) {
        // Boolean enumeration constant false.
        falseId = symTabStack.enterLocal("false", CONSTANT);
        falseId.setType(booleanType);
        falseId.setValue(false);

        // Boolean enumeration constant true.
        trueId = symTabStack.enterLocal("true", CONSTANT);
        trueId.setType(booleanType);
        trueId.setValue(true);
    }

    /**
     * Initialize the standard procedures and functions.
     *
     * @param symTableStack the symbol table stack to initialize.
     */
    private static void initializeStandardRoutines(SymTableStack symTableStack) {
        writeId   = enterStandardProcedure(symTableStack, "write");
        writelnId = enterStandardProcedure(symTableStack, "writeln");
        readId    = enterStandardProcedure(symTableStack, "read");
        readlnId  = enterStandardProcedure(symTableStack, "readln");
    }

    private static SymTableEntry enterStandardProcedure(SymTableStack stack, String name) {
        SymTableEntry entry = stack.enterLocal(name, PROCEDURE);
        entry.setRoutineCode(BUILT_IN);
        return entry;
    }
}
