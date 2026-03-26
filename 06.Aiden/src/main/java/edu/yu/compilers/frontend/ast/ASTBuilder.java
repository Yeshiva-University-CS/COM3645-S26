package edu.yu.compilers.frontend.ast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.antlr.v4.runtime.tree.ParseTree;

import antlr4.AidenBaseVisitor;
import antlr4.AidenParser.DeclarationContext;
import antlr4.AidenParser.ProgramStartContext;
import edu.yu.compilers.intermediate.ast.Program;
import edu.yu.compilers.intermediate.ast.Stmt;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableStack;

/**
 * ASTBuilder class that visits the ANTLR4 parse tree and builds an AST.
 * It transforms the concrete syntax tree into an abstract syntax tree
 * that is used for further processing. This class assumes that semantic
 * analysis has already been performed.
 */
public class ASTBuilder extends AidenBaseVisitor<Object> {

    public static Program build(ParseTree tree, SymTableStack symTableStack) {
        ASTFactory.setSymbolTableStack(symTableStack);
        ASTBuilder builder = new ASTBuilder();
        builder.visit(tree);
        return builder.getProgam();
    }

    private SymTableEntry programEntry = null;

    // A program is a just a list of statements
    private final List<Stmt> programStatements = new ArrayList<>();

    // A map from a function to the AST representing its body
    // This is used to store the body of a function while visiting its declaration
    private final HashMap<SymTableEntry, Stmt.Block> funcBodyMap = new HashMap<>();

    private ASTBuilder() {

    }

    private Program getProgam() {
        return ASTFactory.createProgram(programEntry, programStatements);
    }

    // =============================
    // Program and declarations
    // =============================

    @Override
    public Void visitProgramStart(ProgramStartContext ctx) {
        programEntry = ctx.entry;

        for (DeclarationContext declCtx : ctx.declaration()) {
            Stmt result = (Stmt) visit(declCtx);
            if (!(result instanceof Stmt.Empty)) {
                programStatements.add(result);
            }
        }

        return null;
    }

}
