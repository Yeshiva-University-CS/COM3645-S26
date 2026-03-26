package edu.yu.compilers.backend.interpreter;

import edu.yu.compilers.intermediate.ast.BaseASTVisitor;
import edu.yu.compilers.intermediate.ast.Program;
import edu.yu.compilers.intermediate.ast.Stmt;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;

/**
 * Execute programs.
 */
public class Executor extends BaseASTVisitor<Object>
{
    private int executionCount = 0;     // count of executed statements
    private long elapsedTime = 0L;          // elapsed time in ms
    private final SymTableEntry programId;      // program identifier's symbol table entry
    private final RuntimeStack runtimeStack;  // runtime stack
    private final RuntimeErrorHandler error;  // runtime error handler
    
    public Executor(SymTableEntry programId)
    {
        this.programId = programId;
        runtimeStack = new RuntimeStack();
        error = new RuntimeErrorHandler();
    }
    
    public int getExecutionCount() {
        return executionCount;
    }

    public long getElapsedTime() {
        return elapsedTime;
    }

    public RuntimeErrorHandler getErrorHandler() {
        return error;
    }

    @Override
    public Object visitProgram(Program program) {
        long startTime = System.currentTimeMillis();

        StackFrame programFrame = new StackFrame(programId);
        runtimeStack.push(programFrame);

        for (Stmt s : program.getStatements()) {
            executionCount++;
            visit(s);
        }

        runtimeStack.pop();
        elapsedTime = System.currentTimeMillis() - startTime;

        return null;
    }

}
