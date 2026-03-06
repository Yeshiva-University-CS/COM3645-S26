package edu.yu.compilers;

import antlr4.PascalLexer;
import antlr4.PascalParser;
import edu.yu.compilers.backend.converter.Converter;
import edu.yu.compilers.frontend.SyntaxErrorHandler;
import edu.yu.compilers.frontend.semantic.PascalSemantics;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.FileInputStream;
import java.io.InputStream;

public class PascalCC {
    public static void main(String[] args) throws Exception {
        if (args.length != 2) {
            System.out.println("USAGE: pascalCC {-convert|-compile} sourceFileName");
            return;
        }

        String option = args[0];
        String sourceFileName = args[1];

        if (!option.equalsIgnoreCase("-convert") && !option.equalsIgnoreCase("-compile")) {
            System.out.println("ERROR: Invalid option.");
            System.out.println("   Valid options: -convert, or -compile");
            return;
        }

        // Create the input stream.
        InputStream source = new FileInputStream(sourceFileName);

        // Create the character stream from the input stream.
        CharStream cs = CharStreams.fromStream(source);

        // Custom syntax error handler.
        SyntaxErrorHandler syntaxErrorHandler = new SyntaxErrorHandler();

        // Create a lexer which scans the character stream to create a token stream.
        PascalLexer lexer = new PascalLexer(cs);
        lexer.removeErrorListeners();
        lexer.addErrorListener(syntaxErrorHandler);
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // Create a parser which parses the token stream.
        PascalParser parser = new PascalParser(tokens);

        // Pass 1: Check syntax and create the parse tree.
        parser.removeErrorListeners();
        parser.addErrorListener(syntaxErrorHandler);
        ParseTree tree = parser.program();

        int errorCount = syntaxErrorHandler.getCount();
        if (errorCount > 0) {
            System.out.printf("\nThere were %d syntax errors.\n", errorCount);
            System.out.println("Object file not created or modified.");
            return;
        }

        // Pass 2: Semantic operations.
        PascalSemantics pass2 = new PascalSemantics();
        pass2.visit(tree);

        errorCount = pass2.getErrorCount();
        if (errorCount > 0) {
            System.out.printf("\nThere were %d semantic errors.\n", errorCount);
            System.out.println("Object file not created or modified.");
            return;
        }

        if (option.equalsIgnoreCase("-convert")) {
            // Pass 3: Convert from Pascal to Java.
            Converter pass3 = new Converter();
            String objectCode = (String) pass3.visit(tree);
            System.out.println(objectCode);
        } else {
            // -compile: TBD
            System.out.print("\nPASS 3 Compile: TBD\n\n");
        }
    }
}
