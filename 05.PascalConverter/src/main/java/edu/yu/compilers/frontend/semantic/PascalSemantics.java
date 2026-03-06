package edu.yu.compilers.frontend.semantic;

import antlr4.PascalParser;
import edu.yu.compilers.intermediate.symbols.CrossReferencer;
import edu.yu.compilers.intermediate.symbols.Predefined;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableStack;

import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.PROGRAM;

/**
 * Top-level Pascal semantic analyzer.
 * Handles program-level visitors and exposes the public API.
 */
public class PascalSemantics extends PascalDeclarationSemantics {

    private SymTableEntry programId;

    public PascalSemantics() {
        super(new SymTableStack(), new SemanticErrorHandler());
        Predefined.initialize(symTableStack);
    }

    public SymTableEntry getProgramId() {
        return programId;
    }

    public int getErrorCount() {
        return error.getCount();
    }

    public void printSymbolTableStack() {
        CrossReferencer crossReferencer = new CrossReferencer();
        crossReferencer.print(symTableStack);
    }

    @Override
    public Object visitProgram(PascalParser.ProgramContext ctx) {
        visit(ctx.programHeader());
        visit(ctx.block().declarations());
        visit(ctx.block().compoundStatement());

        return null;
    }

    @Override
    public Object visitProgramHeader(PascalParser.ProgramHeaderContext ctx) {
        PascalParser.ProgramIdentifierContext idCtx = ctx.programIdentifier();
        String programName = idCtx.IDENTIFIER().getText(); // don't shift case

        programId = symTableStack.enterLocal(programName, PROGRAM);
        programId.setRoutineSymTable(symTableStack.push());

        symTableStack.setProgramId(programId);
        symTableStack.getLocalSymTable().setOwner(programId);

        idCtx.entry = programId;
        return null;
    }
}
