import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TestCalculatorTest {

	@Test
	@DisplayName("Test calculatorTest parses correctly")
	void onetest() {
		TestCalculator uut = new TestCalculator();

		TestCal testCal = new TestCal(new Double(17), new Double(60), null);
		assertTrue(uut.testParser(testCal));

		testCal = new TestCal(new Double(12), new Double(60), null);
		assertFalse(uut.testParser(testCal));

		testCal = new TestCal(new Double(17), new Double(61), null);
		assertFalse(uut.testParser(testCal));

		testCal = new TestCal(new Double(17), new Double(60), new Double(1));
		assertFalse(uut.testParser(testCal));
	
	}

	@Test
	@DisplayName("Test calculatorTest uses correct method when parsing")
	void parsingTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testParser(spy);

		assertTrue(spy.stringXCalled, "correct method called");
		assertFalse(spy.primitiveDoubleXCalled, "method should not be called");
		assertFalse(spy.doubleXCalled, "method should not be called");
	}

	@Test
	@DisplayName("Test calculatorTest uses correct method when adding")
	void addingTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testAdd(spy);

		assertTrue(spy.doubleXCalled, "correct method called");
		assertFalse(spy.primitiveDoubleXCalled, "method should not be called");
		assertFalse(spy.stringXCalled, "method should not be called");
	}

	@Test
	@DisplayName("Test calculatorTest uses correct method when multiplying")
	void multiplyingTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testMultiplication(spy);

		assertTrue(spy.primitiveDoubleXCalled, "correct method called");
		assertFalse(spy.doubleXCalled, "method should not be called");
		assertFalse(spy.stringXCalled, "method should not be called");
	}

	@Test
	@DisplayName("Test calculatorTest uses positive numbers when adding")
	void addingPositiveTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testAdd(spy);

		assertTrue(spy.positiveArg, "positive numbers used");
	}

	@Test
	@DisplayName("Test calculatorTest uses negative numbers when adding")
	void addingNegativeTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testAdd(spy);

		assertTrue(spy.negativeArg, "negative numbers used");
	}

	@Test
	@DisplayName("Test calculatorTest uses positive numbers when multiplying")
	void multiplyingPositiveTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testMultiplication(spy);

		assertTrue(spy.positiveArg, "positive numbers used");
	}

	@Test
	@DisplayName("Test calculatorTest uses negative numbers when multiplying")
	void multiplyingNegativeTest() {
		TestCalculator uut = new TestCalculator();

		CalculatorSpy spy = new CalculatorSpy();

		Boolean result = uut.testMultiplication(spy);

		assertTrue(spy.negativeArg, "negative numbers used");
	}
}

class TestCal extends Calculator{
	
	Double a, b, c;
	
	public TestCal(Double a, Double b, Double c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}
	
	public Double x(String x){
		switch(x.replace(" ", "")) {
		case "12+5": return a;
		case "12x5": return b;
		case "12[3": return c;
		}
		return null;
		
	}
	
}

class CalculatorSpy extends Calculator {

	boolean stringXCalled;
	boolean primitiveDoubleXCalled;
	boolean doubleXCalled;
	boolean positiveArg;
	boolean negativeArg;

	public CalculatorSpy() {
		stringXCalled = false;
		primitiveDoubleXCalled = false;
		doubleXCalled = false;
		positiveArg = false;
		negativeArg = false;
	}

	public Double x(String expression) {
		stringXCalled = true;
		return 0.0;
	}

	public Double x(double operand) {
		primitiveDoubleXCalled = true;

		if ((operand >= 0.0) || (x >= 0.0)) {
			positiveArg = true;
		}
		if ((operand < 0.0) || (x < 0.0)) {
			negativeArg = true;
		}

		return 0.0;
	}

	public Double x(Double operand) {
		doubleXCalled = true;

		if ((operand >= 0.0) || (x >= 0.0)){
			positiveArg = true;
		}
		if ((operand < 0.0) || (x < 0.0)){
			negativeArg = true;
		}

		return 0.0;
	}
}

class InaccurateCalculator extends Calculator{
	
	Double a;
	
	public InaccurateCalculator(Double a) {
		this.a = a;
	}
	
	public Double x(String x){
		
		Pattern pattern = Pattern.compile("\\d+|[\\-]\\d+");
		List<Double> list = new ArrayList<Double>();
		Matcher m = pattern.matcher(x);
		
		while (m.find()) {
			list.add(new Double(m.group()));
		}
		
		if(x.contains(" + ") & list.size()==2){
			return new Double(a + list.get(0) + list.get(1));
			
		} else if(x.contains(" * ") & list.size()==2){
			return new Double(a + list.get(0) * list.get(1));
		} else if(x.contains(" - ") & list.size()==2){
			return new Double(a + list.get(0) - list.get(1));
		} else if(x.contains(" / ") & list.size()==2){
			return new Double(a + list.get(0) / list.get(1));
		} else {
			return null;
		}
	}
}
