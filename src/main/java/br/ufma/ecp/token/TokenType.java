package br.ufma.ecp.token;

import java.util.List;

public enum TokenType {
    // symbols
    LPAREN, RPAREN,
    LBRACE, RBRACE,
    LBRACKET, RBRACKET,
    COMMA, SEMICOLON, DOT,
    PLUS, MINUS, ASTERISK, SLASH,
    AND, OR, NOT,
    LT, GT, EQ, N,
    // Literals.
    NUMBER,
    STRING,
    IDENT,
    // keywords
    WHILE, CLASS, CONSTRUCTOR, FUNCTION,
    METHOD, FIELD, STATIC, VAR, INT,
    CHAR, BOOLEAN, VOID, TRUE, FALSE,
    NULL, THIS, LET, DO, IF, ELSE, RETURN,
    EOF,
    ILLEGAL;

    static public boolean isSymbol(char c) {
        String symbols = "{}()[].,;+-*/&|<>=~";
        return symbols.indexOf(c) > -1;
    }

    static public boolean isKeyword(TokenType type) {
        List<TokenType> keywords
                = List.of(
                METHOD,
                WHILE,
                IF,
                CLASS,
                CONSTRUCTOR
        );
        return keywords.contains(type);
    }

}
