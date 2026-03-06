package edu.yu.compilers.frontend.semantic;

import java.util.List;

import antlr4.PascalBaseVisitor;
import antlr4.PascalParser;
import edu.yu.compilers.intermediate.symbols.Predefined;
import edu.yu.compilers.intermediate.symbols.SymTable;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableStack;
import edu.yu.compilers.intermediate.types.TypeChecker;
import edu.yu.compilers.intermediate.types.Typespec;

import static edu.yu.compilers.frontend.semantic.SemanticErrorHandler.Code.*;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.*;
import static edu.yu.compilers.intermediate.types.Typespec.Form.*;

/**
 * Abstract base class for Pascal semantic analysis.
 * Holds shared state (symbol table stack, error handler) and utility methods
 * used across expression, statement, and declaration visitors.
 */
abstract class PascalSemanticsBase extends PascalBaseVisitor<Object> {

    protected final SymTableStack symTableStack;
    protected final SemanticErrorHandler error;

    protected PascalSemanticsBase(SymTableStack symTableStack, SemanticErrorHandler error) {
        this.symTableStack = symTableStack;
        this.error = error;
    }

    /**
     * Return the default value for a data type.
     *
     * @param type the data type.
     * @return the default value.
     */
    protected static Object defaultValue(Typespec type) {
        type = type.baseType();

        if (type == Predefined.integerType) return 0;
        else if (type == Predefined.realType) return 0.0f;
        else if (type == Predefined.booleanType) return Boolean.FALSE;
        else if (type == Predefined.charType) return '#';
        else /* string */                         return "#";
    }

    /**
     * Determine whether an expression is a variable only
     * (a single variable factor with no operators).
     *
     * @param exprCtx the ExpressionContext.
     * @return true if it is a variable expression only.
     */
    protected boolean expressionIsVariable(PascalParser.ExpressionContext exprCtx) {
        if (exprCtx.simpleExpression().size() == 1) {
            PascalParser.SimpleExpressionContext simpleCtx = exprCtx.simpleExpression().get(0);
            if (simpleCtx.term().size() == 1) {
                PascalParser.TermContext termCtx = simpleCtx.term().get(0);
                if (termCtx.factor().size() == 1) {
                    return termCtx.factor().get(0) instanceof PascalParser.VariableFactorContext;
                }
            }
        }
        return false;
    }

    /**
     * Perform semantic operations on procedure and function call arguments.
     *
     * @param listCtx    the ArgumentListContext.
     * @param parameters the list of formal parameters.
     */
    protected void checkCallArguments(PascalParser.ArgumentListContext listCtx,
                                      List<SymTableEntry> parameters) {
        int paramsCount = parameters.size();
        int argsCount = listCtx != null ? listCtx.argument().size() : 0;

        if (paramsCount != argsCount) {
            error.flag(ARGUMENT_COUNT_MISMATCH, listCtx);
            return;
        }

        for (int i = 0; i < paramsCount; i++) {
            PascalParser.ArgumentContext argCtx = listCtx.argument().get(i);
            PascalParser.ExpressionContext exprCtx = argCtx.expression();
            visit(exprCtx);

            SymTableEntry paramId = parameters.get(i);
            Typespec paramType = paramId.getType();
            Typespec argType = exprCtx.type;

            if (paramId.getKind() == REFERENCE_PARAMETER) {
                if (expressionIsVariable(exprCtx)) {
                    if (paramType != argType) {
                        error.flag(TYPE_MISMATCH, exprCtx);
                    }
                } else {
                    error.flag(ARGUMENT_MUST_BE_VARIABLE, exprCtx);
                }
            } else if (!TypeChecker.areAssignmentCompatible(paramType, argType)) {
                error.flag(TYPE_MISMATCH, exprCtx);
            }
        }
    }

    /**
     * Determine the datatype of a variable that can have array/field modifiers.
     *
     * @param varCtx  the VariableContext.
     * @param varType the variable's datatype without modifiers.
     * @return the datatype after applying modifiers.
     */
    protected Typespec variableDatatype(PascalParser.VariableContext varCtx, Typespec varType) {
        Typespec type = varType;

        for (PascalParser.ModifierContext modCtx : varCtx.modifier()) {
            if (modCtx.indexList() != null) {
                // Array subscripts
                PascalParser.IndexListContext indexListCtx = modCtx.indexList();
                for (PascalParser.IndexContext indexCtx : indexListCtx.index()) {
                    if (type.getForm() == ARRAY) {
                        PascalParser.ExpressionContext indexExprCtx = indexCtx.expression();
                        visit(indexExprCtx);
                        Typespec indexType = indexExprCtx.type;
                        Typespec arrayIndexType = type.getArrayIndexType();

                        if (!TypeChecker.areAssignmentCompatible(arrayIndexType, indexType)) {
                            error.flag(TYPE_MISMATCH, indexCtx);
                        }

                        type = type.getArrayElementType().baseType();
                    } else {
                        error.flag(TOO_MANY_SUBSCRIPTS, indexCtx);
                    }
                }
            } else if (modCtx.field() != null) {
                // Record field access
                PascalParser.FieldContext fieldCtx = modCtx.field();
                String fieldName = fieldCtx.IDENTIFIER().getText().toLowerCase();

                if (type.getForm() == RECORD) {
                    SymTable recordSymTable = type.getRecordSymTable();
                    SymTableEntry fieldId = recordSymTable.lookup(fieldName);

                    if (fieldId != null) {
                        type = fieldId.getType();
                        fieldCtx.entry = fieldId;
                        fieldCtx.type = type;
                    } else {
                        error.flag(INVALID_FIELD, fieldCtx);
                    }
                } else {
                    error.flag(INVALID_FIELD, fieldCtx);
                }
            }
        }

        return type;
    }

    /**
     * Return the number of values in a type (for array sizing).
     *
     * @param type the type (ENUMERATION or SUBRANGE expected).
     * @return the count of values.
     */
    protected int typeCount(Typespec type) {
        if (type.getForm() == ENUMERATION) {
            return type.getEnumerationConstants().size();
        } else { // SUBRANGE
            return type.getSubrangeMaxValue() - type.getSubrangeMinValue() + 1;
        }
    }

    /**
     * Create a new record type and enter it into the symbol table.
     *
     * @param recordTypeSpecCtx the RecordTypespecContext.
     * @param recordTypeName    the name for the record type.
     * @return the symbol table entry of the record type.
     */
    protected SymTableEntry createRecordType(PascalParser.RecordTypespecContext recordTypeSpecCtx,
                                             String recordTypeName) {
        PascalParser.RecordTypeContext recordTypeCtx = recordTypeSpecCtx.recordType();
        Typespec recordType = new Typespec(RECORD);

        SymTableEntry recordTypeId = symTableStack.enterLocal(recordTypeName, TYPE);
        recordTypeId.setType(recordType);
        recordType.setIdentifier(recordTypeId);

        String recordTypePath = createRecordTypePath(recordType);
        recordType.setRecordTypePath(recordTypePath);

        SymTable recordSymTable = createRecordSymTable(recordTypeCtx.recordFields(), recordTypeId);
        recordType.setRecordSymTable(recordSymTable);

        recordTypeCtx.entry = recordTypeId;
        recordTypeSpecCtx.type = recordType;

        return recordTypeId;
    }

    /**
     * Create the fully qualified type pathname of a record type.
     *
     * @param recordType the record type.
     * @return the pathname.
     */
    private String createRecordTypePath(Typespec recordType) {
        SymTableEntry recordId = recordType.getIdentifier();
        SymTableEntry parentId = recordId.getSymTable().getOwner();
        String path = recordId.getName();

        while ((parentId.getKind() == TYPE) && (parentId.getType().getForm() == RECORD)) {
            path = parentId.getName() + "$" + path;
            parentId = parentId.getSymTable().getOwner();
        }

        path = parentId.getName() + "$" + path;
        return path;
    }

    /**
     * Create the symbol table for a record type.
     *
     * @param ctx     the RecordFieldsContext.
     * @param ownerId the symbol table entry of the owner.
     * @return the new symbol table.
     */
    private SymTable createRecordSymTable(PascalParser.RecordFieldsContext ctx,
                                          SymTableEntry ownerId) {
        SymTable recordSymTable = symTableStack.push();
        recordSymTable.setOwner(ownerId);
        visit(ctx.variableDeclarationsList());
        recordSymTable.resetVariables(RECORD_FIELD);
        symTableStack.pop();
        return recordSymTable;
    }
}
