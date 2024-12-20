package br.ufma.ecp;

import br.ufma.ecp.token.Token;
import static br.ufma.ecp.token.TokenType.*;
import br.ufma.ecp.token.TokenType;

public class Parser {

    public void parseLet() {
        printNonTerminal("letStatement");
        expectPeek(LET);
        expectPeek(IDENT);
        if(peekTokenIs(LBRACKET)){
            expectPeek(LBRACKET);
            parseExpression();
            expectPeek(RBRACKET);
        }
        expectPeek(EQ);
        parseExpression();
        expectPeek(SEMICOLON);
        printNonTerminal("/letStatement");
    }

    public void parseSubroutineCall(){
        expectPeek(IDENT);
        if (peekTokenIs(DOT)) {
            expectPeek(DOT);
            expectPeek(IDENT);
        }
        expectPeek(LPAREN);
        parseExpressionList();
        expectPeek(RPAREN);
    }

    public void parseIf() {
        printNonTerminal("ifStatement");
        expectPeek(IF);
        expectPeek(LPAREN);
        parseExpression();
        expectPeek(RPAREN);
        expectPeek(LBRACE);
        parseStatements();
        expectPeek(RBRACE);

        if (peekTokenIs(ELSE)) {
            expectPeek(ELSE);
            expectPeek(LBRACE);
            parseStatements();
            expectPeek(RBRACE);
        }
        printNonTerminal("/ifStatement");
    }

    public void parseDo() {
        printNonTerminal("doStatement");
        expectPeek(DO);
        parseSubroutineCall();
        expectPeek(SEMICOLON);
        printNonTerminal("/doStatement");
     }

    public void parseSubroutineDec() {
        printNonTerminal("subroutineDec");
        expectPeek(CONSTRUCTOR, FUNCTION, METHOD);
        if (peekTokenIs(VOID)) {
            expectPeek(VOID);
        } else {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
        }
        expectPeek(IDENT);
        expectPeek(LPAREN);
        parseParameterList();
        expectPeek(RPAREN);
        parseSubroutineBody();
        printNonTerminal("/subroutineDec");
    }

    public void parseClassVarDec() {
        printNonTerminal("classVarDec");
        expectPeek(STATIC, FIELD);
        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        expectPeek(IDENT);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/classVarDec");
    }

    private static class ParseError extends RuntimeException {}

    private Scanner scan;
    private Token currentToken;
    private Token peekToken;
    private StringBuilder xmlOutput = new StringBuilder();

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
            expectPeek(peekToken.type);
            parseTerm();
        }
        printNonTerminal("/expression");
    }

    public void parseTerm(){
        printNonTerminal("term");
        switch (peekToken.type){
            case NUMBER:
                expectPeek(TokenType.NUMBER);
                break;
            case STRING:
                expectPeek(TokenType.STRING);
                break;
            case FALSE:
            case NULL:
            case TRUE:
                expectPeek(TokenType.FALSE, TokenType.NULL, TokenType.TRUE);
                break;
            case THIS:
                expectPeek(TokenType.THIS);
                break;
            case IDENT:
                expectPeek(TokenType.IDENT);
                if (peekTokenIs(DOT)) {
                    expectPeek(DOT);
                    expectPeek(IDENT);
                    if (peekTokenIs(LPAREN)) {
                        expectPeek(LPAREN);
                        parseExpressionList();
                        expectPeek(RPAREN);
                    }
                } else if (peekTokenIs(LPAREN)) {
                    expectPeek(LPAREN);
                    parseExpressionList();
                    expectPeek(RPAREN);
                } else if (peekTokenIs(LBRACKET)) {
                    expectPeek(LBRACKET);
                    parseExpression();
                    expectPeek(RBRACKET);
                }
                break;
            case LPAREN:
                expectPeek(LPAREN);
                parseExpression();
                expectPeek(RPAREN);
                break;
            case MINUS:
            case NOT:
                expectPeek(peekToken.type);
                parseTerm();
                break;
            default:
                throw error(peekToken, "term expected");
        }

        printNonTerminal("/term");
    }

    public void parseExpressionList() {
        printNonTerminal("expressionList");
        if (!peekTokenIs(RPAREN)) {
            parseExpression();
            while (peekTokenIs(COMMA)) {
                expectPeek(COMMA);
                parseExpression();
            }
        }
        printNonTerminal("/expressionList");
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

    private void parseStatement() {
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
        expectPeek(WHILE);
        expectPeek(LPAREN);
        parseExpression();
        expectPeek(RPAREN);
        expectPeek(LBRACE);
        parseStatements();
        expectPeek(RBRACE);
        printNonTerminal("/whileStatement");
    }

    public void parseReturn() {
        printNonTerminal("returnStatement");
        expectPeek(RETURN);
        if (!peekTokenIs(SEMICOLON)) {
            parseExpression();
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/returnStatement");
    }

    private void parseParameterList() {
        printNonTerminal("parameterList");
        if (!peekTokenIs(RPAREN)) {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
            expectPeek(IDENT);
            while (peekTokenIs(COMMA)) {
                expectPeek(COMMA);
                expectPeek(INT, CHAR, BOOLEAN, IDENT);
                expectPeek(IDENT);
            }
        }
        printNonTerminal("/parameterList");
    }

    private void parseSubroutineBody() {
        printNonTerminal("subroutineBody");
        expectPeek(LBRACE);
        while (peekTokenIs(VAR)) {
            parseVarDec();
        }
        parseStatements();
        expectPeek(RBRACE);
        printNonTerminal("/subroutineBody");
    }

    private void parseVarDec() {
        printNonTerminal("varDec");
        expectPeek(VAR);
        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        expectPeek(IDENT);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/varDec");
    }

}