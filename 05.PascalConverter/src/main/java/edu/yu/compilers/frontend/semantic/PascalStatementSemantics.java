package edu.yu.compilers.frontend.semantic;

import java.util.HashMap;
import java.util.HashSet;

import antlr4.PascalParser;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableStack;
import edu.yu.compilers.intermediate.types.TypeChecker;
import edu.yu.compilers.intermediate.types.Typespec;
import edu.yu.compilers.intermediate.symbols.Predefined;

import static edu.yu.compilers.frontend.semantic.SemanticErrorHandler.Code.*;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.*;
import static edu.yu.compilers.intermediate.types.Typespec.Form.ENUMERATION;
import static edu.yu.compilers.intermediate.types.Typespec.Form.SCALAR;
import static edu.yu.compilers.intermediate.types.Typespec.Form.SUBRANGE;

/**
 * Statement visitor methods for Pascal semantic analysis.
 */
abstract class PascalStatementSemantics extends PascalExpressionSemantics {

    protected PascalStatementSemantics(SymTableStack symTableStack, SemanticErrorHandler error) {
        super(symTableStack, error);
    }

    @Override
    public Object visitAssignmentStatement(PascalParser.AssignmentStatementContext ctx) {
        visitChildren(ctx);
        return null;
    }

    @Override
    public Object visitLhs(PascalParser.LhsContext ctx) {
        PascalParser.VariableContext varCtx = ctx.variable();
        visit(varCtx);
        ctx.type = varCtx.type;

        return null;
    }

    @Override
    public Object visitCaseStatement(PascalParser.CaseStatementContext ctx) {
        PascalParser.ExpressionContext exprCtx = ctx.expression();
        visit(exprCtx);
        Typespec exprType = exprCtx.type;
        Typespec.Form exprTypeForm = exprType.getForm();

        if (((exprTypeForm != SCALAR) && (exprTypeForm != ENUMERATION) && (exprTypeForm != SUBRANGE))
                || (exprType == Predefined.realType)) {
            error.flag(TYPE_MISMATCH, exprCtx);
            exprType = Predefined.integerType;
        }

        HashSet<Object> constants = new HashSet<>();
        PascalParser.CaseBranchListContext branchListCtx = ctx.caseBranchList();

        for (PascalParser.CaseBranchContext branchCtx : branchListCtx.caseBranch()) {
            PascalParser.CaseConstantListContext constListCtx = branchCtx.caseConstantList();
            PascalParser.StatementContext stmtCtx = branchCtx.statement();

            if (constListCtx != null) {
                for (PascalParser.CaseConstantContext caseConstCtx : constListCtx.caseConstant()) {
                    PascalParser.ConstantContext constCtx = caseConstCtx.constant();
                    Object constValue = visit(constCtx);

                    caseConstCtx.type = constCtx.type;
                    caseConstCtx.value = null;

                    if ((constCtx.type == Predefined.integerType) || (constCtx.type.getForm() == ENUMERATION)) {
                        caseConstCtx.value = (Integer) constValue;
                    } else if (constCtx.type == Predefined.charType) {
                        caseConstCtx.value = (Character) constValue;
                    } else if (constCtx.type == Predefined.stringType) {
                        caseConstCtx.value = (String) constValue;
                    }

                    constants.add(caseConstCtx.value);
                }
            }

            if (stmtCtx != null) visit(stmtCtx);
        }

        return null;
    }

}
