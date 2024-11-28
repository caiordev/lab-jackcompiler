package br.ufma.ecp;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.Test;

import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType;



public class ScannerTest extends TestSupport {


    @Test
    public void testSimple () {
        String input = "45  + if + \"ola mundo\" - 876";
        Scanner scan = new Scanner (input.getBytes());
        for (Token tk = scan.nextToken(); tk.type != TokenType.EOF; tk = scan.nextToken()) {
            System.out.println(tk);
        }
    }
    
    @Test
    public void testScannerWithSquareGame() throws IOException {
        var input = fromFile("Square/SquareGame.jack");
        var expectedResult =  fromFile("Square/SquareGameT.xml");

        var scanner = new Scanner(input.getBytes(StandardCharsets.UTF_8));
        var result = new StringBuilder();
        
        result.append("<tokens>\r\n");

        for (Token tk = scanner.nextToken(); tk.type !=TokenType.EOF; tk = scanner.nextToken()) {
            result.append(String.format("%s\r\n",tk.toString()));
        }

        result.append("</tokens>\r\n");
        
        assertEquals(expectedResult, result.toString());
    }

    @Test
    public void testScannerVoidKeyword() throws IOException {
        var input = "void";
        var expectedResult =  """ 
        <keyword> void </keyword>
        """;

        var scanner = new Scanner(input.getBytes(StandardCharsets.UTF_8));
        var result = new StringBuilder();

        for(Token tk = scanner.nextToken(); tk.type != TokenType.EOF; tk = scanner.nextToken()) {
            result.append(String.format("%s\n",tk.toString()));
        }

        assertEquals(expectedResult, result.toString());
    }


    @Test
    public void testScannerWithSquare() throws IOException {
        var input = fromFile("Square/Square.jack");
        var expectedResult =  fromFile("Square/SquareT.xml");

        var scanner = new Scanner(input.getBytes(StandardCharsets.UTF_8));
        var result = new StringBuilder();
        
        result.append("<tokens>\r\n");

        for (Token tk = scanner.nextToken(); tk.type !=TokenType.EOF; tk = scanner.nextToken()) {
            result.append(String.format("%s\r\n",tk.toString()));
        }
        
        result.append("</tokens>\r\n");
        System.out.println(result.toString());
        assertEquals(expectedResult, result.toString());
    }

    @Test
    public void testScannerWithMain() throws IOException {
        var input = fromFile("Square/Main.jack");
        var expectedResult =  fromFile("Square/MainT.xml");

        var scanner = new Scanner(input.getBytes(StandardCharsets.UTF_8));
        var result = new StringBuilder();

        result.append("<tokens>\r\n");

        for (Token tk = scanner.nextToken(); tk.type !=TokenType.EOF; tk = scanner.nextToken()) {
            result.append(String.format("%s\r\n",tk.toString()));
        }

        result.append("</tokens>\r\n");
        System.out.println(result.toString());
        assertEquals(expectedResult, result.toString());
    }

    
    
}