package br.ufma.ecp;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class VmWriterTest {
  @Test
    public void testInt () {
        var input = """
            10
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 10       
                    """;
            assertEquals(expected, actual);
    }

    @Test
    public void testSimpleExpression () {
        var input = """
            10 + 30
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 10
                push constant 30
                add       
                    """;
            assertEquals(expected, actual);
    }
    @Test
    public void testLiteralString () {
        var input = """
            "OLA"
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 3
                call String.new 1
                push constant 79
                call String.appendChar 2
                push constant 76
                call String.appendChar 2
                push constant 65
                call String.appendChar 2
                    """;
            assertEquals(expected, actual);
    }
    @Test
    public void testFalse () {
        var input = """
            false
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 0       
                    """;
            assertEquals(expected, actual);
    }

    @Test
    public void testNull () {
        var input = """
            null
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 0       
                    """;
            assertEquals(expected, actual);
    }


    @Test
    public void testTrue () {
        var input = """
            true
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 0
                not       
                    """;
            assertEquals(expected, actual);
    }


    @Test
    public void testThis () {
        var input = """
            this
            """;
        
        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push pointer 0
                    """;
            assertEquals(expected, actual);
    }

    @Test
    public void testNot () {
        var input = """
            ~ false
            """;

        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 0   
                not    
                    """;
        assertEquals(expected, actual);
    }

    @Test
    public void testMinus () {
        var input = """
            - 10
            """;

        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseExpression();
        String actual = parser.VMOutput();
        String expected = """
                push constant 10   
                neg    
                    """;
        assertEquals(expected, actual);
    }

    @Test
    public void testReturn () {
        var input = """
            return;
            """;

        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseStatement();
        String actual = parser.VMOutput();
        String expected = """
                push constant 0
                return       
                    """;
        assertEquals(expected, actual);
    }

    @Test
    public void testReturnExpr () {
        var input = """
            return 10;
            """;

        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseStatement();
        String actual = parser.VMOutput();
        String expected = """
                push constant 10
                return       
                    """;
        assertEquals(expected, actual);
    }

    @Test
    public void testIf () {
        var input = """
            if (false) {
                return 10;
            } else {
                return 20;
            }
            """;

        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseStatement();
        String actual = parser.VMOutput();
        String expected = """
            push constant 0
            if-goto IF_TRUE0
            goto IF_FALSE0
            label IF_TRUE0
            push constant 10
            return
            goto IF_END0
            label IF_FALSE0
            push constant 20
            return
            label IF_END0 
                    """;
        assertEquals(expected, actual);
    }

    @Test
    public void testWhile () {
        var input = """
            while (false) {
                return 10;
            } 
            """;

        var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
        parser.parseStatement();
        String actual = parser.VMOutput();
        String expected = """
label WHILE_EXP0
push constant 0
not
if-goto WHILE_END0
push constant 10
return
goto WHILE_EXP0
label WHILE_END0
                    """;
        assertEquals(expected, actual);
    }

  @Test
  public void testSimpleFunctions () {
    var input = """
            class Main {
 
                function int soma(int x, int y) {
                        return  30;
                 }
                
                 function void main() {
                        var int d;
                        return;
                  }
                
                }
            """;;
    var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
    parser.parse();
    String actual = parser.VMOutput();
    String expected = """
            function Main.soma 0
            push constant 30
            return
            function Main.main 1
            push constant 0
            return    
                """;
    assertEquals(expected, actual);
  }

  @Test
  public void testSimpleFunctionWithVar () {
    var input = """
            class Main {

                 function int funcao() {
                        var int d;
                        return d;
                  }
                
                }
            """;;
    var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
    parser.parse();
    String actual = parser.VMOutput();
    String expected = """
            function Main.funcao 1
            push local 0
            return
            """;
    assertEquals(expected, actual);
  }

  @Test
  public void testLet () {
    var input = """
            class Main {
            
              function void main() {
                  var int x;
                  let x = 42;
                  return;
              }
            }
            """;;
    var parser = new Parser(input.getBytes(StandardCharsets.UTF_8));
    parser.parse();
    String actual = parser.VMOutput();
    String expected = """
            function Main.main 1
            push constant 42
            pop local 0
            push constant 0
            return
                """;
    assertEquals(expected, actual);
  }
}
