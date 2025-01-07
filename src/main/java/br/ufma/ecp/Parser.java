package br.ufma.ecp;

import br.ufma.ecp.VMWriter.Command;
import br.ufma.ecp.VMWriter.Segment;
import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType;
import static br.ufma.ecp.token.TokenType.AND;
import static br.ufma.ecp.token.TokenType.ASTERISK;
import static br.ufma.ecp.token.TokenType.BOOLEAN;
import static br.ufma.ecp.token.TokenType.CHAR;
import static br.ufma.ecp.token.TokenType.CLASS;
import static br.ufma.ecp.token.TokenType.COMMA;
import static br.ufma.ecp.token.TokenType.CONSTRUCTOR;
import static br.ufma.ecp.token.TokenType.DO;
import static br.ufma.ecp.token.TokenType.DOT;
import static br.ufma.ecp.token.TokenType.ELSE;
import static br.ufma.ecp.token.TokenType.EQ;
import static br.ufma.ecp.token.TokenType.FALSE;
import static br.ufma.ecp.token.TokenType.FIELD;
import static br.ufma.ecp.token.TokenType.FUNCTION;
import static br.ufma.ecp.token.TokenType.GT;
import static br.ufma.ecp.token.TokenType.IDENT;
import static br.ufma.ecp.token.TokenType.IF;
import static br.ufma.ecp.token.TokenType.INT;
import static br.ufma.ecp.token.TokenType.LBRACE;
import static br.ufma.ecp.token.TokenType.LBRACKET;
import static br.ufma.ecp.token.TokenType.LET;
import static br.ufma.ecp.token.TokenType.LPAREN;
import static br.ufma.ecp.token.TokenType.LT;
import static br.ufma.ecp.token.TokenType.METHOD;
import static br.ufma.ecp.token.TokenType.MINUS;
import static br.ufma.ecp.token.TokenType.NOT;
import static br.ufma.ecp.token.TokenType.NULL;
import static br.ufma.ecp.token.TokenType.NUMBER;
import static br.ufma.ecp.token.TokenType.OR;
import static br.ufma.ecp.token.TokenType.PLUS;
import static br.ufma.ecp.token.TokenType.RBRACE;
import static br.ufma.ecp.token.TokenType.RBRACKET;
import static br.ufma.ecp.token.TokenType.RETURN;
import static br.ufma.ecp.token.TokenType.RPAREN;
import static br.ufma.ecp.token.TokenType.SEMICOLON;
import static br.ufma.ecp.token.TokenType.SLASH;
import static br.ufma.ecp.token.TokenType.STATIC;
import static br.ufma.ecp.token.TokenType.STRING;
import static br.ufma.ecp.token.TokenType.THIS;
import static br.ufma.ecp.token.TokenType.TRUE;
import static br.ufma.ecp.token.TokenType.VAR;
import static br.ufma.ecp.token.TokenType.VOID;
import static br.ufma.ecp.token.TokenType.WHILE;

public class Parser {

    private VMWriter vmWriter = new VMWriter();
    private int ifLabelNum = 0 ;
    private int whileLabelNum = 0;

    private SymbolTable symTable = new SymbolTable();

    private static class ParseError extends RuntimeException {}

    private Scanner scan;
    private Token currentToken;
    private Token peekToken;
    private StringBuilder xmlOutput = new StringBuilder();
    private String className = "";

    public String VMOutput() {
        return vmWriter.vmOutput();
    }

    public void parseLet() {
        var isArray = false;
        printNonTerminal("letStatement");
        expectPeek(LET);
        expectPeek(IDENT);

        var symbol = symTable.resolve(currentToken.lexeme);

        if(peekTokenIs(LBRACKET)){
            expectPeek(LBRACKET);
            parseExpression();

            vmWriter.writePush(kind2Segment(symbol.kind()), symbol.index());
            vmWriter.writeArithmetic(Command.ADD);

            expectPeek(RBRACKET);

            isArray = true;
        }

        expectPeek(EQ);
        parseExpression();

        if (isArray) {

            vmWriter.writePop(Segment.TEMP, 0);    // push result back onto stack
            vmWriter.writePop(Segment.POINTER, 1); // pop address pointer into pointer 1
            vmWriter.writePush(Segment.TEMP, 0);   // push result back onto stack
            vmWriter.writePop(Segment.THAT, 0);    // Store right hand side evaluation in THAT 0.

        } else {
            vmWriter.writePop(kind2Segment(symbol.kind()), symbol.index());
        }

        expectPeek(SEMICOLON);
        printNonTerminal("/letStatement");
    }

    public void parseSubroutineCall(){
        var nArgs = 0;
        var name = currentToken.lexeme;
        var symbol = symTable.resolve(name);
        String functionName;

        if (peekTokenIs(LPAREN)) { // method call in current class
            expectPeek(LPAREN);
            vmWriter.writePush(Segment.POINTER, 0);
            nArgs = parseExpressionList() + 1;
            expectPeek(RPAREN);
            functionName = className + "." + name;
        } else {
            expectPeek(DOT);
            expectPeek(IDENT);
            var methodName = currentToken.lexeme;
            
            if (symbol != null) { // method call on object
                functionName = symbol.type() + "." + methodName;
                vmWriter.writePush(kind2Segment(symbol.kind()), symbol.index());
                nArgs = 1;
            } else { // function or constructor call
                functionName = name + "." + methodName;
            }

            expectPeek(LPAREN);
            nArgs += parseExpressionList();
            expectPeek(RPAREN);
        }

        vmWriter.writeCall(functionName, nArgs);
    }

    public void parseIf() {
        printNonTerminal("ifStatement");
        var labelTrue = "IF_TRUE" + ifLabelNum;
        var labelFalse = "IF_FALSE" + ifLabelNum;
        var labelEnd = "IF_END" + ifLabelNum;

        ifLabelNum++;

        expectPeek(IF);
        expectPeek(LPAREN);
        parseExpression();
        expectPeek(RPAREN);

        vmWriter.writeIf(labelTrue);
        vmWriter.writeGoto(labelFalse);
        vmWriter.writeLabel(labelTrue);

        expectPeek(LBRACE);
        parseStatements();
        expectPeek(RBRACE);
        if (peekTokenIs(ELSE)){
            vmWriter.writeGoto(labelEnd);
        }

        vmWriter.writeLabel(labelFalse);

        if (peekTokenIs(ELSE))
        {
            expectPeek(ELSE);
            expectPeek(LBRACE);
            parseStatements();
            expectPeek(RBRACE);
            vmWriter.writeLabel(labelEnd);
        }

        printNonTerminal("/ifStatement");
    }

    public void parseDo() {
        printNonTerminal("doStatement");
        expectPeek(DO);
        expectPeek(IDENT);
        parseSubroutineCall();
        expectPeek(SEMICOLON);
        vmWriter.writePop(Segment.TEMP, 0);
        printNonTerminal("/doStatement");
     }

    public void parseSubroutineDec() {
        printNonTerminal("subroutineDec");

        ifLabelNum = 0;
        whileLabelNum = 0;

        symTable.startSubroutine();

        expectPeek(CONSTRUCTOR, FUNCTION, METHOD);
        var subroutineType = currentToken.type;

        if (subroutineType == METHOD) {
            symTable.define("this", className, SymbolTable.Kind.ARG);
        }

        if (peekTokenIs(VOID)) {
            expectPeek(VOID);
        } else {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
        }

        expectPeek(IDENT);
        var functionName = className + "." + currentToken.lexeme;

        expectPeek(LPAREN);
        parseParameterList();
        expectPeek(RPAREN);
        parseSubroutineBody(functionName, subroutineType);
        printNonTerminal("/subroutineDec");
    }

    public void parseClassVarDec() {
        printNonTerminal("classVarDec");
        expectPeek(STATIC, FIELD);

        SymbolTable.Kind kind = SymbolTable.Kind.STATIC;
        if (currentTokenIs(FIELD))
            kind = SymbolTable.Kind.FIELD;

        // 'int'| 'char'| 'boolean'| className
        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        expectPeek(IDENT);

        String type = currentToken.lexeme;
        String name = currentToken.lexeme;

        symTable.define(name, type, kind);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);

            name = currentToken.lexeme;
            symTable.define(name, type, kind);
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/classVarDec");
    }

    public Parser(byte[] input) {
        scan = new Scanner(input);
        nextToken();
    }

    private void nextToken() {
        currentToken = peekToken;
        peekToken = scan.nextToken();
    }


    public void parse () {
        printNonTerminal("class");
        expectPeek(CLASS);
        expectPeek(IDENT);
        className = currentToken.lexeme;
        expectPeek(LBRACE);

        while (peekTokenIs(STATIC) || peekTokenIs(FIELD)) {
            parseClassVarDec();
        }
        while (peekTokenIs(CONSTRUCTOR) || peekTokenIs(FUNCTION) || peekTokenIs(METHOD)) {
            parseSubroutineDec();
        }
        expectPeek(RBRACE);
        printNonTerminal("/class");
    }

    // funções auxiliares
    public String XMLOutput() {
        return xmlOutput.toString();
    }

    private void printNonTerminal(String nterminal) {
        xmlOutput.append(String.format("<%s>\r\n", nterminal));
    }


    boolean peekTokenIs(TokenType type) {
        return peekToken.type == type;
    }

    boolean currentTokenIs(TokenType type) {
        return currentToken.type == type;
    }

    private void expectPeek(TokenType... types) {
        for (TokenType type : types) {
            if (peekToken.type == type) {
                expectPeek(type);
                return;
            }
        }

        throw error(peekToken, "Expected a statement");

    }

    private void expectPeek(TokenType type) {
        if (peekToken.type == type) {
            nextToken();
            xmlOutput.append(String.format("%s\r\n", currentToken.toString()));
        } else {
            throw error(peekToken, "Expected "+type.name());
        }
    }


    private static void report(int line, String where,
                               String message) {
        System.err.println(
                "[line " + line + "] Error" + where + ": " + message);
    }


    private ParseError error(Token token, String message) {
        if (token.type == TokenType.EOF) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" + token.lexeme + "'", message);
        }
        return new ParseError();
    }

    static public boolean isOperator(String op) {
        return op != null && "+-*/&|<>=~".contains(op);
    }

    void parseExpression() {
        printNonTerminal("expression");
        parseTerm ();
        while (isOperator(peekToken.lexeme)) {
            var ope = peekToken.type;
            expectPeek(peekToken.type);
            parseTerm();
            compileOperators(ope);
        }
        printNonTerminal("/expression");
    }

    private Segment kind2Segment(SymbolTable.Kind kind) {
        if (kind == SymbolTable.Kind.STATIC)
            return Segment.STATIC;
        if (kind == SymbolTable.Kind.FIELD)
            return Segment.THIS;
        if (kind == SymbolTable.Kind.VAR)
            return Segment.LOCAL;
        if (kind == SymbolTable.Kind.ARG)
            return Segment.ARG;
        return null;
    }

    public void parseTerm(){
        printNonTerminal("term");
        switch (peekToken.type){
            case NUMBER:
                expectPeek(TokenType.NUMBER);
                vmWriter.writePush(Segment.CONST, Integer.parseInt(currentToken.lexeme));
                break;
            case STRING:
                expectPeek(TokenType.STRING);
                var strValue = currentToken.lexeme;
                vmWriter.writePush(Segment.CONST, strValue.length());
                vmWriter.writeCall("String.new", 1);
                for (int i = 0; i < strValue.length(); i++) {
                    vmWriter.writePush(Segment.CONST, strValue.charAt(i));
                    vmWriter.writeCall("String.appendChar", 2);
                }
                break;
            case FALSE:
            case NULL:
            case TRUE:
                expectPeek(TokenType.FALSE, TokenType.NULL, TokenType.TRUE);
                vmWriter.writePush(Segment.CONST, 0);
                if (currentToken.type == TRUE)
                    vmWriter.writeArithmetic(Command.NOT);
                break;
            case THIS:
                expectPeek(TokenType.THIS);
                vmWriter.writePush(Segment.POINTER, 0);
                break;
            case IDENT:
                expectPeek(TokenType.IDENT);
                var sym = symTable.resolve(currentToken.lexeme);

                if (peekTokenIs(LBRACKET)) {
                    expectPeek(LBRACKET);
                    parseExpression();
                    vmWriter.writePush(kind2Segment(sym.kind()), sym.index());
                    vmWriter.writeArithmetic(Command.ADD);
                    expectPeek(RBRACKET);
                    vmWriter.writePop(Segment.POINTER, 1);
                    vmWriter.writePush(Segment.THAT, 0);
                } else if (peekTokenIs(DOT) || peekTokenIs(LPAREN)) {
                    parseSubroutineCall();
                } else {
                    vmWriter.writePush(kind2Segment(sym.kind()), sym.index());
                }
                break;
            case LPAREN:
                expectPeek(LPAREN);
                parseExpression();
                expectPeek(RPAREN);
                break;
            case MINUS:
            case NOT:
                expectPeek(MINUS, NOT);
                var op = currentToken.type;
                parseTerm();
                if (op == MINUS)
                    vmWriter.writeArithmetic(Command.NEG);
                else
                    vmWriter.writeArithmetic(Command.NOT);
                break;
        }
        printNonTerminal("/term");
    }

    public int parseExpressionList() {
        printNonTerminal("expressionList");

        var nArgs = 0;

        if (!peekTokenIs(RPAREN)) // verifica se tem pelo menos uma expressao
        {
            parseExpression();
            nArgs = 1;
        }

        // procurando as demais
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            parseExpression();
            nArgs++;
        }

        printNonTerminal("/expressionList");
        return nArgs;
    }

    public void parseStatements() {
        printNonTerminal("statements");
        while (peekToken.type == LET || peekToken.type == IF ||
                peekToken.type == WHILE || peekToken.type == DO ||
                peekToken.type == RETURN) {
            parseStatement();
        }
        printNonTerminal("/statements");
    }

    public void parseStatement() {
        switch (peekToken.type) {
            case LET:
                parseLet();
                break;
            case IF:
                parseIf();
                break;
            case DO:
                parseDo();
                break;
            case WHILE:
                parseWhile();
                break;
            case RETURN:
                parseReturn();
                break;
            default:
                throw error(peekToken, "Expected a statement");
        }
    }

    public void parseWhile() {
        printNonTerminal("whileStatement");

        var labelTrue = "WHILE_EXP" + whileLabelNum;
        var labelFalse = "WHILE_END" + whileLabelNum;
        whileLabelNum++;

        vmWriter.writeLabel(labelTrue);

        expectPeek(WHILE);
        expectPeek(LPAREN);
        parseExpression();

        vmWriter.writeArithmetic(Command.NOT);
        vmWriter.writeIf(labelFalse);

        expectPeek(RPAREN);
        expectPeek(LBRACE);
        parseStatements();

        vmWriter.writeGoto(labelTrue); // Go back to labelTrue and check condition
        vmWriter.writeLabel(labelFalse); // Breaks out of while loop because ~(condition) is true

        expectPeek(RBRACE);
        printNonTerminal("/whileStatement");
    }

   public void parseReturn() {
        printNonTerminal("returnStatement");
        expectPeek(RETURN);
        if (!peekTokenIs(SEMICOLON)) {
            parseExpression();
        } else {
            vmWriter.writePush(Segment.CONST, 0);
        }
        expectPeek(SEMICOLON);
        vmWriter.writeReturn();

        printNonTerminal("/returnStatement");
    }

    private void parseParameterList() {
        printNonTerminal("parameterList");

        SymbolTable.Kind kind = SymbolTable.Kind.ARG;

        if (!peekTokenIs(RPAREN)) {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
            String type = currentToken.lexeme;

            expectPeek(IDENT);
            String name = currentToken.lexeme;
            symTable.define(name, type, kind);

            while (peekTokenIs(COMMA)) {
                expectPeek(COMMA);
                expectPeek(INT, CHAR, BOOLEAN, IDENT);
                type = currentToken.lexeme;

                expectPeek(IDENT);
                name = currentToken.lexeme;

                symTable.define(name, type, kind);
            }
        }
        printNonTerminal("/parameterList");
    }

    private void parseSubroutineBody(String functionName, TokenType subroutineType) {
        printNonTerminal("subroutineBody");
        expectPeek(LBRACE);
        while (peekTokenIs(VAR)) {
            parseVarDec();
        }
        var nlocals = symTable.varCount(SymbolTable.Kind.VAR);

        vmWriter.writeFunction(functionName, nlocals);

        if (subroutineType == CONSTRUCTOR) {
            vmWriter.writePush(Segment.CONST, symTable.varCount(SymbolTable.Kind.FIELD));
            vmWriter.writeCall("Memory.alloc", 1);
            vmWriter.writePop(Segment.POINTER, 0);
        }

        if (subroutineType == METHOD) {
            vmWriter.writePush(Segment.ARG, 0);
            vmWriter.writePop(Segment.POINTER, 0);
        }

        parseStatements();
        expectPeek(RBRACE);
        printNonTerminal("/subroutineBody");
    }

    private void parseVarDec() {
        printNonTerminal("varDec");
        expectPeek(VAR);

        SymbolTable.Kind kind = SymbolTable.Kind.VAR;

        // 'int'| 'char'| 'boolean'| className
        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        expectPeek(IDENT);

        String type = currentToken.lexeme;
        String name = currentToken.lexeme;

        symTable.define(name, type, kind);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);

            name = currentToken.lexeme;
            symTable.define(name, type, kind);
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/varDec");
    }

    public void compileOperators(TokenType type) {

        if (type == ASTERISK) {
            vmWriter.writeCall("Math.multiply", 2);
        } else if (type == SLASH) {
            vmWriter.writeCall("Math.divide", 2);
        } else {
            vmWriter.writeArithmetic(typeOperator(type));
        }
    }

    private Command typeOperator(TokenType type) {
        if (type == PLUS)
            return Command.ADD;
        if (type == MINUS)
            return Command.SUB;
        if (type == LT)
            return Command.LT;
        if (type == GT)
            return Command.GT;
        if (type == EQ)
            return Command.EQ;
        if (type == AND)
            return Command.AND;
        if (type == OR)
            return Command.OR;
        return null;
    }
}