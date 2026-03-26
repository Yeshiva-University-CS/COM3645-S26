package edu.yu.compilers.frontend.semantic;

import antlr4.AidenBaseVisitor;
import antlr4.AidenParser;
import antlr4.AidenParser.ProgramStartContext;
import edu.yu.compilers.intermediate.symbols.CrossReferencer;
import edu.yu.compilers.intermediate.symbols.Predefined;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind;
import edu.yu.compilers.intermediate.symbols.SymTableStack;

public class Semantics extends AidenBaseVisitor<Void> {
    private final SymTableStack symTableStack;
    private final SemanticErrorHandler error;

    public Semantics() {
        this.symTableStack = new SymTableStack();
        Predefined.initialize(symTableStack);
        this.error = new SemanticErrorHandler();
    }

    public SymTableStack getSymTableStack() {
        return symTableStack;
    }

    public int getErrorCount() {
        return error.getCount();
    }

    public void printSymbolTableStack() {
        CrossReferencer crossReferencer = new CrossReferencer();
        crossReferencer.print(symTableStack);
    }

    @Override
    public Void visitProgramStart(ProgramStartContext ctx) {
        SymTableEntry entry = symTableStack.enterLocal("__program__", Kind.PROGRAM);
        entry.setRoutineSymTable(symTableStack.push());
        entry.appendLineNumber(ctx.start.getLine());
        symTableStack.setProgramId(entry);
        symTableStack.getLocalSymTable().setOwner(entry);
        ctx.entry = entry;

        for (AidenParser.DeclarationContext decl : ctx.declaration()) {
            visit(decl);
        }

        symTableStack.pop();

        return null;
    }

    /*
     * Report errors using SemanticErrorHandler with the listed Code.
     *
     * REDECLARED_IDENTIFIER
     * UNDECLARED_IDENTIFIER
     * TYPE_MISMATCH
     * INVALID_ASSIGNMENT
     * IMMUTABLE_ASSIGNMENT
     * MISPLACED_RETURN
     * ARGUMENT_COUNT_MISMATCH
     * INVALID_FUNCTION
     */
}
