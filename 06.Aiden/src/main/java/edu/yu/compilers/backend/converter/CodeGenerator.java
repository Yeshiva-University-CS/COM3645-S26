package edu.yu.compilers.backend.converter;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Hashtable;

import edu.yu.compilers.intermediate.ast.BaseASTVisitor;
import edu.yu.compilers.intermediate.ast.Program;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;

/**
 * Abstract base class for converter code generators.
 */
public abstract class CodeGenerator extends BaseASTVisitor<Object> {

    protected static final Hashtable<String, String> typeNameTable;

    static {
        typeNameTable = new Hashtable<>();
        typeNameTable.put("integer", "int");
        typeNameTable.put("real", "double");
        typeNameTable.put("boolean", "boolean");
        typeNameTable.put("string", "String");
        typeNameTable.put("none", "Void");
    }

    protected final SymTableEntry programId;
    protected final String programName;
    protected final StringWriter outputWriter;
    protected final CodeEmitter code;

    protected CodeGenerator(SymTableEntry programId) {
        this.programId = programId;
        this.programName = programId.getName();
        this.outputWriter = new StringWriter();
        this.code = new CodeEmitter(new PrintWriter(outputWriter));
    }

    public String convert(Program program) {
        visitProgram(program);
        code.lfIfNeeded();
        return outputWriter.toString();
    }

    public String getOutput() {
        return outputWriter.toString();
    }
}
