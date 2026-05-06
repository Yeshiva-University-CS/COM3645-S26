package edu.yu.compilers.backend.converter;

import edu.yu.compilers.intermediate.ast.Program;
import edu.yu.compilers.intermediate.ast.Stmt;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;

public class JavaCodeGenerator extends CodeGenerator {

    private final String javaClassName;

    public JavaCodeGenerator(SymTableEntry programId) {
        super(programId);
        this.javaClassName = programName;
    }

    public JavaCodeGenerator(SymTableEntry programId, String javaClassName) {
        super(programId);
        this.javaClassName = javaClassName;
    }

    @Override
    public Object visitProgram(Program program) {
        code.emitLine("public class " + javaClassName);
        code.emitLine("{");
        code.indent();
        code.emitLine("public static void main(String[] args)");
        code.emitLine("{");
        code.indent();
        for (Stmt s : program.getStatements()) {
            visit(s);
        }
        code.dedent();
        code.emitLine("}");
        code.dedent();
        code.emitLine("}");
        return null;
    }

}
