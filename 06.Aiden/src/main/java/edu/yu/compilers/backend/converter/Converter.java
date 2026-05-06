package edu.yu.compilers.backend.converter;

import edu.yu.compilers.intermediate.ast.Program;

/**
 * Convert programs using an injected code generator.
 */
public class Converter {

    private final CodeGenerator codeGenerator;

    public Converter(CodeGenerator codeGenerator) {
        this.codeGenerator = codeGenerator;
    }

    public String convert(Program program) {
        return codeGenerator.convert(program);
    }
}
