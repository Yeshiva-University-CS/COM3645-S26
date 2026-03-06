package edu.yu.compilers.frontend.semantic;

import java.util.ArrayList;

import antlr4.PascalParser;
import edu.yu.compilers.intermediate.symbols.Predefined;
import edu.yu.compilers.intermediate.symbols.SymTable;
import edu.yu.compilers.intermediate.symbols.SymTableEntry;
import edu.yu.compilers.intermediate.symbols.SymTableStack;
import edu.yu.compilers.intermediate.types.Typespec;

import static edu.yu.compilers.frontend.semantic.SemanticErrorHandler.Code.*;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.Kind.*;
import static edu.yu.compilers.intermediate.symbols.SymTableEntry.RoutineCode.DECLARED;
import static edu.yu.compilers.intermediate.types.Typespec.Form.ARRAY;
import static edu.yu.compilers.intermediate.types.Typespec.Form.ENUMERATION;
import static edu.yu.compilers.intermediate.types.Typespec.Form.SCALAR;
import static edu.yu.compilers.intermediate.types.Typespec.Form.SUBRANGE;

/**
 * Declaration visitor methods for Pascal semantic analysis.
 */
abstract class PascalDeclarationSemantics extends PascalStatementSemantics {

    protected PascalDeclarationSemantics(SymTableStack symTableStack, SemanticErrorHandler error) {
        super(symTableStack, error);
    }

    @Override
    public Object visitConstantDefinition(PascalParser.ConstantDefinitionContext ctx) {
        PascalParser.ConstantIdentifierContext idCtx = ctx.constantIdentifier();
        String constantName = idCtx.IDENTIFIER().getText().toLowerCase();
        SymTableEntry constantId = symTableStack.lookupLocal(constantName);

        if (constantId == null) {
            PascalParser.ConstantContext constCtx = ctx.constant();
            Object constValue = visit(constCtx);

            constantId = symTableStack.enterLocal(constantName, CONSTANT);
            constantId.setValue(constValue);
            constantId.setType(constCtx.type);

            idCtx.entry = constantId;
            idCtx.type = constCtx.type;
        } else {
            error.flag(REDECLARED_IDENTIFIER, ctx);

            idCtx.entry = constantId;
            idCtx.type = Predefined.integerType;
        }

        constantId.appendLineNumber(ctx.getStart().getLine());
        return null;
    }

    @Override
    public Object visitConstant(PascalParser.ConstantContext ctx) {
        if (ctx.IDENTIFIER() != null) {
            String constantName = ctx.IDENTIFIER().getText().toLowerCase();
            SymTableEntry constantId = symTableStack.lookup(constantName);

            if (constantId != null) {
                SymTableEntry.Kind kind = constantId.getKind();
                if ((kind != CONSTANT) && (kind != ENUMERATION_CONSTANT)) {
                    error.flag(INVALID_CONSTANT, ctx);
                }

                ctx.type = constantId.getType();
                ctx.value = constantId.getValue();

                constantId.appendLineNumber(ctx.getStart().getLine());
            } else {
                error.flag(UNDECLARED_IDENTIFIER, ctx);

                ctx.type = Predefined.integerType;
                ctx.value = 0;
            }
        } else if (ctx.characterConstant() != null) {
            ctx.type = Predefined.charType;
            ctx.value = ctx.getText().charAt(1);
        } else if (ctx.stringConstant() != null) {
            String pascalString = ctx.stringConstant().STRING().getText();
            String unquoted = pascalString.substring(1, pascalString.length() - 1);
            ctx.type = Predefined.stringType;
            ctx.value = unquoted.replace("''", "'").replace("\"", "\\\"");
        } else { // number
            if (ctx.unsignedNumber().integerConstant() != null) {
                ctx.type = Predefined.integerType;
                ctx.value = Integer.parseInt(ctx.getText());
            } else {
                ctx.type = Predefined.realType;
                ctx.value = Float.parseFloat(ctx.getText());
            }
        }

        return ctx.value;
    }

    @Override
    public Object visitTypeDefinition(PascalParser.TypeDefinitionContext ctx) {
        PascalParser.TypeIdentifierContext idCtx = ctx.typeIdentifier();
        String typeName = idCtx.IDENTIFIER().getText().toLowerCase();
        SymTableEntry typeId = symTableStack.lookupLocal(typeName);

        PascalParser.TypeSpecificationContext typespecCtx = ctx.typeSpecification();

        if (typespecCtx instanceof PascalParser.RecordTypespecContext) {
            typeId = createRecordType((PascalParser.RecordTypespecContext) typespecCtx, typeName);
        } else if (typeId == null) {
            visit(typespecCtx);

            typeId = symTableStack.enterLocal(typeName, TYPE);
            typeId.setType(typespecCtx.type);
            typespecCtx.type.setIdentifier(typeId);
        } else {
            error.flag(REDECLARED_IDENTIFIER, ctx);
        }

        idCtx.entry = typeId;
        idCtx.type = typespecCtx.type;

        typeId.appendLineNumber(ctx.getStart().getLine());
        return null;
    }

    @Override
    public Object visitRecordTypespec(PascalParser.RecordTypespecContext ctx) {
        String recordTypeName = SymTable.generateUnnamedName();
        createRecordType(ctx, recordTypeName);
        return null;
    }

    @Override
    public Object visitSimpleTypespec(PascalParser.SimpleTypespecContext ctx) {
        visit(ctx.simpleType());
        ctx.type = ctx.simpleType().type;
        return null;
    }

    @Override
    public Object visitTypeIdentifierTypespec(PascalParser.TypeIdentifierTypespecContext ctx) {
        visit(ctx.typeIdentifier());
        ctx.type = ctx.typeIdentifier().type;
        return null;
    }

    @Override
    public Object visitTypeIdentifier(PascalParser.TypeIdentifierContext ctx) {
        String typeName = ctx.IDENTIFIER().getText().toLowerCase();
        SymTableEntry typeId = symTableStack.lookup(typeName);

        if (typeId != null) {
            if (typeId.getKind() != TYPE) {
                error.flag(INVALID_TYPE, ctx);
                ctx.type = Predefined.integerType;
            } else {
                ctx.type = typeId.getType();
            }

            typeId.appendLineNumber(ctx.start.getLine());
        } else {
            error.flag(UNDECLARED_IDENTIFIER, ctx);
            ctx.type = Predefined.integerType;
        }

        ctx.entry = typeId;
        return null;
    }

    @Override
    public Object visitEnumerationTypespec(PascalParser.EnumerationTypespecContext ctx) {
        Typespec enumType = new Typespec(ENUMERATION);
        ArrayList<SymTableEntry> constants = new ArrayList<>();
        int value = -1;

        for (PascalParser.EnumerationConstantContext constCtx : ctx.enumerationType().enumerationConstant()) {
            PascalParser.ConstantIdentifierContext constIdCtx = constCtx.constantIdentifier();
            String constantName = constIdCtx.IDENTIFIER().getText().toLowerCase();
            SymTableEntry constantId = symTableStack.lookupLocal(constantName);

            if (constantId == null) {
                constantId = symTableStack.enterLocal(constantName, ENUMERATION_CONSTANT);
                constantId.setType(enumType);
                constantId.setValue(++value);

                constants.add(constantId);
            }

            constIdCtx.entry = constantId;
            constIdCtx.type = enumType;

            constantId.appendLineNumber(ctx.getStart().getLine());
        }

        enumType.setEnumerationConstants(constants);
        ctx.type = enumType;

        return null;
    }

    @Override
    public Object visitSubrangeTypespec(PascalParser.SubrangeTypespecContext ctx) {
        Typespec type = new Typespec(SUBRANGE);
        PascalParser.SubrangeTypeContext subCtx = ctx.subrangeType();
        PascalParser.ConstantContext minCtx = subCtx.constant().get(0);
        PascalParser.ConstantContext maxCtx = subCtx.constant().get(1);

        Object minObj = visit(minCtx);
        Object maxObj = visit(maxCtx);

        Typespec minType = minCtx.type;

       if (((minType.getForm() != ENUMERATION)) || (minType == Predefined.realType) || (minType == Predefined.stringType)) {
            error.flag(INVALID_CONSTANT, minCtx);
            minType = Predefined.integerType;
            minObj = 0;
        }

        int minValue;
        int maxValue;

        if (minType == Predefined.integerType) {
            minValue = (Integer) minObj;
            maxValue = (Integer) maxObj;
        } else if (minType == Predefined.charType) {
            minValue = (Character) minObj;
            maxValue = (Character) maxObj;
        } else { // enumeration constants
            minValue = (Integer) minCtx.value;
            maxValue = (Integer) maxCtx.value;
        }

        type.setSubrangeBaseType(minType);
        type.setSubrangeMinValue(minValue);
        type.setSubrangeMaxValue(maxValue);

        ctx.type = type;
        return null;
    }

    @Override
    public Object visitArrayTypespec(PascalParser.ArrayTypespecContext ctx) {
        Typespec arrayType = new Typespec(ARRAY);
        PascalParser.ArrayTypeContext arrayCtx = ctx.arrayType();
        PascalParser.ArrayDimensionListContext listCtx = arrayCtx.arrayDimensionList();

        ctx.type = arrayType;

        int count = listCtx.simpleType().size();
        for (int i = 0; i < count; i++) {
            PascalParser.SimpleTypeContext simpleCtx = listCtx.simpleType().get(i);
            visit(simpleCtx);
            arrayType.setArrayIndexType(simpleCtx.type);
            arrayType.setArrayElementCount(typeCount(simpleCtx.type));

            if (i < count - 1) {
                Typespec elementType = new Typespec(ARRAY);
                arrayType.setArrayElementType(elementType);
                arrayType = elementType;
            }
        }

        visit(arrayCtx.typeSpecification());
        Typespec elementType = arrayCtx.typeSpecification().type;
        arrayType.setArrayElementType(elementType);

        return null;
    }

    @Override
    public Object visitVariableDeclarations(PascalParser.VariableDeclarationsContext ctx) {
        PascalParser.TypeSpecificationContext typeCtx = ctx.typeSpecification();
        visit(typeCtx);

        PascalParser.VariableIdentifierListContext listCtx = ctx.variableIdentifierList();

        for (PascalParser.VariableIdentifierContext idCtx : listCtx.variableIdentifier()) {
            int lineNumber = idCtx.getStart().getLine();
            String variableName = idCtx.IDENTIFIER().getText().toLowerCase();
            SymTableEntry variableId = symTableStack.lookupLocal(variableName);

            if (variableId == null) {
                variableId = symTableStack.enterLocal(variableName, VARIABLE);
                variableId.setType(typeCtx.type);

                SymTable symTable = variableId.getSymTable();
                if (symTable.getNestingLevel() > 1) {
                    variableId.setSlotNumber(symTable.nextSlotNumber());
                }

                idCtx.entry = variableId;
            }

            variableId.appendLineNumber(lineNumber);
        }

        return null;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Object visitRoutineDefinition(PascalParser.RoutineDefinitionContext ctx) {
        PascalParser.FunctionHeadContext funcCtx = ctx.functionHead();
        PascalParser.ProcedureHeadContext procCtx = ctx.procedureHead();
        PascalParser.RoutineIdentifierContext idCtx;
        PascalParser.ParametersContext parameters;
        boolean functionDefinition = funcCtx != null;
        Typespec returnType = null;
        String routineName;

        if (functionDefinition) {
            idCtx = funcCtx.routineIdentifier();
            parameters = funcCtx.parameters();
        } else {
            idCtx = procCtx.routineIdentifier();
            parameters = procCtx.parameters();
        }

        routineName = idCtx.IDENTIFIER().getText().toLowerCase();
        SymTableEntry routineId = symTableStack.lookupLocal(routineName);

        if (routineId != null) {
            error.flag(REDECLARED_IDENTIFIER, ctx.getStart().getLine(), routineName);
            return null;
        }

        routineId = symTableStack.enterLocal(routineName, functionDefinition ? FUNCTION : PROCEDURE);
        routineId.setRoutineCode(DECLARED);
        idCtx.entry = routineId;

        SymTableEntry parentId = symTableStack.getLocalSymTable().getOwner();
        parentId.appendSubroutine(routineId);

        routineId.setRoutineSymTable(symTableStack.push());
        idCtx.entry = routineId;

        SymTable symTable = symTableStack.getLocalSymTable();
        symTable.setOwner(routineId);

        if (parameters != null) {
            ArrayList<SymTableEntry> parameterIds =
                    (ArrayList<SymTableEntry>) visit(parameters.parameterDeclarationsList());
            routineId.setRoutineParameters(parameterIds);

            for (SymTableEntry paramId : parameterIds) {
                paramId.setSlotNumber(symTable.nextSlotNumber());
            }
        }

        if (functionDefinition) {
            PascalParser.TypeIdentifierContext typeIdCtx = funcCtx.typeIdentifier();
            visit(typeIdCtx);
            returnType = typeIdCtx.type;

            if (returnType.getForm() != SCALAR) {
                error.flag(INVALID_RETURN_TYPE, typeIdCtx);
                returnType = Predefined.integerType;
            }

            routineId.setType(returnType);
            idCtx.type = returnType;
        } else {
            idCtx.type = null;
        }

        visit(ctx.block().declarations());

        if (functionDefinition) {
            SymTableEntry assocVarId = symTableStack.enterLocal(routineName, VARIABLE);
            assocVarId.setSlotNumber(symTable.nextSlotNumber());
            assocVarId.setType(returnType);
        }

        visit(ctx.block().compoundStatement());
        routineId.setExecutable(ctx.block().compoundStatement());

        symTableStack.pop();
        return null;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Object visitParameterDeclarationsList(PascalParser.ParameterDeclarationsListContext ctx) {
        ArrayList<SymTableEntry> parameterList = new ArrayList<>();

        for (PascalParser.ParameterDeclarationsContext dclCtx : ctx.parameterDeclarations()) {
            ArrayList<SymTableEntry> parameterSublist = (ArrayList<SymTableEntry>) visit(dclCtx);
            parameterList.addAll(parameterSublist);
        }

        return parameterList;
    }

    @Override
    public Object visitParameterDeclarations(PascalParser.ParameterDeclarationsContext ctx) {
        SymTableEntry.Kind kind = ctx.VAR() != null ? REFERENCE_PARAMETER : VALUE_PARAMETER;
        PascalParser.TypeIdentifierContext typeCtx = ctx.typeIdentifier();

        visit(typeCtx);
        Typespec paramType = typeCtx.type;

        ArrayList<SymTableEntry> parameterSublist = new ArrayList<>();

        PascalParser.ParameterIdentifierListContext paramListCtx = ctx.parameterIdentifierList();
        for (PascalParser.ParameterIdentifierContext paramIdCtx : paramListCtx.parameterIdentifier()) {
            int lineNumber = paramIdCtx.getStart().getLine();
            String paramName = paramIdCtx.IDENTIFIER().getText().toLowerCase();
            SymTableEntry paramId = symTableStack.lookupLocal(paramName);

            if (paramId == null) {
                paramId = symTableStack.enterLocal(paramName, kind);
                paramId.setType(paramType);

            } else {
                error.flag(REDECLARED_IDENTIFIER, paramIdCtx);
            }

            paramIdCtx.entry = paramId;
            paramIdCtx.type = paramType;

            parameterSublist.add(paramId);
            paramId.appendLineNumber(lineNumber);
        }

        return parameterSublist;
    }
}
