package edu.yu.compilers.frontend.semantic;

import antlr4.PascalParser;
import edu.yu.compilers.intermediate.symbols.Predefined;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableStack;
import edu.yu.compilers.intermediate.types.TypeChecker;
import edu.yu.compilers.intermediate.types.Typespec;

import static edu.yu.compilers.frontend.semantic.SemanticErrorHandler.Code.*;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.*;

/**
 * Expression and factor visitor methods for Pascal semantic analysis.
 */
abstract class PascalExpressionSemantics extends PascalSemanticsBase {

    protected PascalExpressionSemantics(SymTableStack symTableStack, SemanticErrorHandler error) {
        super(symTableStack, error);
    }

    @Override
    public Object visitExpression(PascalParser.ExpressionContext ctx) {
        PascalParser.SimpleExpressionContext simpleCtx1 = ctx.simpleExpression().get(0);
        visit(simpleCtx1);

        Typespec simpleType1 = simpleCtx1.type;
        ctx.type = simpleType1;

        PascalParser.RelOpContext relOpCtx = ctx.relOp();

        if (relOpCtx != null) {
            PascalParser.SimpleExpressionContext simpleCtx2 = ctx.simpleExpression().get(1);
            visit(simpleCtx2);

            Typespec simpleType2 = simpleCtx2.type;
            if (!TypeChecker.areComparisonCompatible(simpleType1, simpleType2)) {
                error.flag(INCOMPATIBLE_COMPARISON, ctx);
            }

            ctx.type = Predefined.booleanType;
        }

        return null;
    }

    @Override
    public Object visitSimpleExpression(PascalParser.SimpleExpressionContext ctx) {
        int count = ctx.term().size();
        PascalParser.SignContext signCtx = ctx.sign();
        boolean hasSign = signCtx != null;
        PascalParser.TermContext termCtx1 = ctx.term().get(0);

        if (hasSign) {
            String sign = signCtx.getText();
            if (!sign.equals("+") && !sign.equals("-")) {
                error.flag(INVALID_SIGN, signCtx);
            }
        }

        visit(termCtx1);
        Typespec termType1 = termCtx1.type;

        for (int i = 1; i < count; i++) {
            String op = ctx.addOp().get(i - 1).getText().toLowerCase();
            PascalParser.TermContext termCtx2 = ctx.term().get(i);
            visit(termCtx2);
            Typespec termType2 = termCtx2.type;

            if (op.equals("or")) {
                if (!TypeChecker.isBoolean(termType1)) {
                    error.flag(TYPE_MUST_BE_BOOLEAN, termCtx1);
                }
                if (!TypeChecker.isBoolean(termType2)) {
                    error.flag(TYPE_MUST_BE_BOOLEAN, termCtx2);
                }
                if (hasSign) {
                    error.flag(INVALID_SIGN, signCtx);
                }
                termType2 = Predefined.booleanType;
            } else if (op.equals("+")) {
                if (TypeChecker.areBothInteger(termType1, termType2)) {
                    termType2 = Predefined.integerType;
                } else if (TypeChecker.isAtLeastOneReal(termType1, termType2)) {
                    termType2 = Predefined.realType;
                } else if (TypeChecker.areBothString(termType1, termType2)) {
                    if (hasSign) error.flag(INVALID_SIGN, signCtx);
                    termType2 = Predefined.stringType;
                } else {
                    if (!TypeChecker.isIntegerOrReal(termType1)) {
                        error.flag(TYPE_MUST_BE_NUMERIC, termCtx1);
                        termType2 = Predefined.integerType;
                    }
                    if (!TypeChecker.isIntegerOrReal(termType2)) {
                        error.flag(TYPE_MUST_BE_NUMERIC, termCtx2);
                        termType2 = Predefined.integerType;
                    }
                }
            } else { // -
                if (TypeChecker.areBothInteger(termType1, termType2)) {
                    termType2 = Predefined.integerType;
                } else if (TypeChecker.isAtLeastOneReal(termType1, termType2)) {
                    termType2 = Predefined.realType;
                } else {
                    if (!TypeChecker.isIntegerOrReal(termType1)) {
                        error.flag(TYPE_MUST_BE_NUMERIC, termCtx1);
                        termType2 = Predefined.integerType;
                    }
                    if (!TypeChecker.isIntegerOrReal(termType2)) {
                        error.flag(TYPE_MUST_BE_NUMERIC, termCtx2);
                        termType2 = Predefined.integerType;
                    }
                }
            }

            termType1 = termType2;
        }

        ctx.type = termType1;
        return null;
    }

    @Override
    public Object visitTerm(PascalParser.TermContext ctx) {
        int count = ctx.factor().size();
        PascalParser.FactorContext factorCtx1 = ctx.factor().get(0);

        visit(factorCtx1);
        Typespec factorType1 = factorCtx1.type;

        for (int i = 1; i < count; i++) {
            String op = ctx.mulOp().get(i - 1).getText().toLowerCase();
            PascalParser.FactorContext factorCtx2 = ctx.factor().get(i);
            visit(factorCtx2);
            Typespec factorType2 = factorCtx2.type;

            switch (op) {
                case "*":
                    if (TypeChecker.areBothInteger(factorType1, factorType2)) {
                        factorType2 = Predefined.integerType;
                    } else if (TypeChecker.isAtLeastOneReal(factorType1, factorType2)) {
                        factorType2 = Predefined.realType;
                    } else {
                        if (!TypeChecker.isIntegerOrReal(factorType1)) {
                            error.flag(TYPE_MUST_BE_NUMERIC, factorCtx1);
                            factorType2 = Predefined.integerType;
                        }
                        if (!TypeChecker.isIntegerOrReal(factorType2)) {
                            error.flag(TYPE_MUST_BE_NUMERIC, factorCtx2);
                            factorType2 = Predefined.integerType;
                        }
                    }
                    break;
                case "/":
                    if (TypeChecker.areBothInteger(factorType1, factorType2)
                            || TypeChecker.isAtLeastOneReal(factorType1, factorType2)) {
                        factorType2 = Predefined.realType;
                    } else {
                        if (!TypeChecker.isIntegerOrReal(factorType1)) {
                            error.flag(TYPE_MUST_BE_NUMERIC, factorCtx1);
                            factorType2 = Predefined.integerType;
                        }
                        if (!TypeChecker.isIntegerOrReal(factorType2)) {
                            error.flag(TYPE_MUST_BE_NUMERIC, factorCtx2);
                            factorType2 = Predefined.integerType;
                        }
                    }
                    break;
                case "div":
                case "mod":
                    if (!TypeChecker.isInteger(factorType1)) {
                        error.flag(TYPE_MUST_BE_INTEGER, factorCtx1);
                        factorType2 = Predefined.integerType;
                    }
                    if (!TypeChecker.isInteger(factorType2)) {
                        error.flag(TYPE_MUST_BE_INTEGER, factorCtx2);
                        factorType2 = Predefined.integerType;
                    }
                    break;
                case "and":
                    if (!TypeChecker.isBoolean(factorType1)) {
                        error.flag(TYPE_MUST_BE_BOOLEAN, factorCtx1);
                        factorType2 = Predefined.booleanType;
                    }
                    if (!TypeChecker.isBoolean(factorType2)) {
                        error.flag(TYPE_MUST_BE_BOOLEAN, factorCtx2);
                        factorType2 = Predefined.booleanType;
                    }
                    break;
            }

            factorType1 = factorType2;
        }

        ctx.type = factorType1;
        return null;
    }

    @Override
    public Object visitVariableFactor(PascalParser.VariableFactorContext ctx) {
        PascalParser.VariableContext varCtx = ctx.variable();
        visit(varCtx);
        ctx.type = varCtx.type;
        return null;
    }

    @Override
    public Object visitNumberFactor(PascalParser.NumberFactorContext ctx) {
        PascalParser.NumberContext numberCtx = ctx.number();
        PascalParser.UnsignedNumberContext unsignedCtx = numberCtx.unsignedNumber();

        if (unsignedCtx.integerConstant() != null) {
            ctx.type = Predefined.integerType;
        } else {
            ctx.type = Predefined.realType;
        }

        return null;
    }

    @Override
    public Object visitCharacterFactor(PascalParser.CharacterFactorContext ctx) {
        ctx.type = Predefined.charType;
        return null;
    }

    @Override
    public Object visitStringFactor(PascalParser.StringFactorContext ctx) {
        ctx.type = Predefined.stringType;
        return null;
    }

    @Override
    public Object visitFunctionCallFactor(PascalParser.FunctionCallFactorContext ctx) {
        PascalParser.FunctionCallContext callCtx = ctx.functionCall();
        PascalParser.FunctionNameContext nameCtx = callCtx.functionName();
        PascalParser.ArgumentListContext listCtx = callCtx.argumentList();
        String name = callCtx.functionName().getText().toLowerCase();
        SymTableEntry functionId = symTableStack.lookup(name);
        boolean badName = false;

        ctx.type = Predefined.integerType;

        if (functionId == null) {
            error.flag(UNDECLARED_IDENTIFIER, nameCtx);
            badName = true;
        } else if (functionId.getKind() != FUNCTION) {
            error.flag(NAME_MUST_BE_FUNCTION, nameCtx);
            badName = true;
        }

        if (badName) {
            if (listCtx != null) {
                for (PascalParser.ArgumentContext argCtx : listCtx.argument()) {
                    visit(argCtx);
                }
            }
        } else {
            checkCallArguments(listCtx, functionId.getRoutineParameters());
            ctx.type = functionId.getType();
        }

        nameCtx.entry = functionId;
        nameCtx.type = ctx.type;
        return null;
    }

    @Override
    public Object visitNotFactor(PascalParser.NotFactorContext ctx) {
        PascalParser.FactorContext factorCtx = ctx.factor();
        visit(factorCtx);
        Typespec factorType = factorCtx.type;

        if (!TypeChecker.isBoolean(factorType)) {
            error.flag(TYPE_MUST_BE_BOOLEAN, factorCtx);
        }

        ctx.type = Predefined.booleanType;
        return null;
    }

    @Override
    public Object visitParenthesizedFactor(PascalParser.ParenthesizedFactorContext ctx) {
        PascalParser.ExpressionContext exprCtx = ctx.expression();
        visit(exprCtx);
        ctx.type = exprCtx.type;
        return null;
    }

    @Override
    public Object visitVariable(PascalParser.VariableContext ctx) {
        PascalParser.VariableIdentifierContext varIdCtx = ctx.variableIdentifier();
        visit(varIdCtx);
        ctx.entry = varIdCtx.entry;
        ctx.type = variableDatatype(ctx, varIdCtx.type);
        return null;
    }

    @Override
    public Object visitVariableIdentifier(PascalParser.VariableIdentifierContext ctx) {
        String variableName = ctx.IDENTIFIER().getText().toLowerCase();
        SymTableEntry variableId = symTableStack.lookup(variableName);

        if (variableId != null) {
            int lineNumber = ctx.getStart().getLine();
            ctx.type = variableId.getType();
            ctx.entry = variableId;
            variableId.appendLineNumber(lineNumber);

            SymTableEntry.Kind kind = variableId.getKind();
            switch (kind) {
                case TYPE, PROGRAM, PROGRAM_PARAMETER, PROCEDURE, UNDEFINED ->
                        error.flag(INVALID_VARIABLE, ctx);
                default -> {
                }
            }
        } else {
            error.flag(UNDECLARED_IDENTIFIER, ctx);
            ctx.type = Predefined.integerType;
        }

        return null;
    }
}
