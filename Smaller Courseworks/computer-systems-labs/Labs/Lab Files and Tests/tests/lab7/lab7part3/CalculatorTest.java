import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.sound.sampled.BooleanControl.Type;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;


class CalculatorTest {

	@Test
	@DisplayName("Check the number of methods")
	void methodTest() {
		Calculator cal = new Calculator();
		Method[] methods = cal.getClass().getDeclaredMethods();
		
		
		ArrayList<String> types = new ArrayList<String> ();
		
		for(int i = 0; i<methods.length; i++) {
			assertEquals("x", methods[0].getName().toString(), "Check that there are no new method names");
			types.add(methods[i].getParameterTypes()[0].toString());
			System.out.println(methods[i].getParameterTypes()[0].toString());
		}
		
		assertEquals(3, methods.length, "Check that there are only three methods");
		
		assertEquals(3, types.size(), "Checks correct number of parameters");
		
		assertTrue(types.contains("double"), "Checks there is a double parameter");
		assertTrue(types.contains("class java.lang.Double"), "Checks there is a Double parameter");
		assertTrue(types.contains("class java.lang.String"), "Checks there is a String parameter");
		
	}

	@Test
	@DisplayName("Test adding")
	void addingTest() {
		Calculator cal = new Calculator();
		assertEquals(new Double(17), cal.x("12 + 5"));
	}
	
	@Test
	@DisplayName("Test negative adding")
	void negativeAddingTest() {
		Calculator cal = new Calculator();
		assertEquals(new Double(7), cal.x("5 + 2"));
		assertEquals(new Double(-3), cal.x("-1 + -2"));
	}
	
	@Test
	@DisplayName("Test multiplication")
	void multiplicationTest() {
		Calculator cal = new Calculator();
		assertEquals(new Double(60), cal.x("12 x 5"));
	}

	@Test
	@DisplayName("Test negative multiplication")
	void negativeMultiplicationTest() {
		Calculator cal = new Calculator();
		assertEquals(new Double(15), cal.x("5 x 3"));
		assertEquals(new Double(15), cal.x("-5 x -3"));
	}
	
	@Test
	@DisplayName("Check the functionality of the calculator is correct")
	void malformedTest() {
		Calculator cal = new Calculator();
		assertEquals(null, cal.x("12 [ 3"));
	}
	
}
