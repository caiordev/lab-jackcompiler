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
}
