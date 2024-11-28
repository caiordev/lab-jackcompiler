package br.ufma.ecp.token;
public class Token {

    public final TokenType type;
    public final String lexeme;
    final int line;

    public Token(TokenType type, String lexeme, int line){
        this.type = type;
        this.lexeme = lexeme;
        this.line = line;
    }

    public String toString() {
        var type = this.type.toString();

        return "<"+ type +">" + lexeme + "</"+ type + ">";
    }



}
