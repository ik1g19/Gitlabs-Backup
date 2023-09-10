import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class OutputCapturer {
	private PrintStream origOut;

	private ByteArrayOutputStream outputStream;
	
	public void start()
	{
		this.origOut = System.out;
		this.outputStream = new ByteArrayOutputStream();
		PrintStream ps = new PrintStream(this.outputStream);
		System.setOut(ps);	
	}
	
	public String getOutput() {
		System.out.flush();
		return this.outputStream.toString().replaceAll("\\r\\n", "\n").replaceAll("\\r", "\n");
	}
	public void stop() {
		System.setOut(this.origOut);
	}
}

@DisplayName("Lab 1 part 2")
	class GuessingGameTest {
	OutputCapturer outputHarness;

	@BeforeEach
	public void setup() {
		this.outputHarness = new OutputCapturer();
		this.outputHarness.start();
	}

	@AfterEach
	public void tearDown() {
		this.outputHarness.stop();
	}
	
	@Test
	@DisplayName("Tests when the guess was equal")
	public void testEqual() {
		Integer[] randomNumbers = { new Integer(5) };
		String[] inputs = { new String("5") };
		Toolbox.setTestingData(randomNumbers, inputs);
		
		GuessingGame.main(null);
		
		String out = outputHarness.getOutput();
		assertTrue(out.toLowerCase().contains("right"), "Should say guess was right");
		assertFalse(out.toLowerCase().contains("low"), "Should not say low");
		assertFalse(out.toLowerCase().contains("high"), "Should not say high");
	}

	@Test
	@DisplayName("Tests when the guess was too low")
	public void testLow() {

		Integer[] randomNumbers = { new Integer(5) };
		String[] inputs = { new String("1") };
		Toolbox.setTestingData(randomNumbers, inputs);
		GuessingGame.main(null);
		
		String out = outputHarness.getOutput();
		assertFalse(out.toLowerCase().contains("right"), "Should not say right");
		assertTrue(out.toLowerCase().contains("low"), "Should say low");
		assertFalse(out.toLowerCase().contains("high"), "Should not say high");	}

	@Test
	@DisplayName("Tests when the guess was too high")
	public void testHigh() {

		Integer[] randomNumbers = { new Integer(5) };
		String[] inputs = { new String("10") };
		Toolbox.setTestingData(randomNumbers, inputs);
		GuessingGame.main(null);
		
		String out = outputHarness.getOutput();
		assertFalse(out.toLowerCase().contains("right"), "Should not say right");
		assertFalse(out.toLowerCase().contains("low"), "Should not say low");
		assertTrue(out.toLowerCase().contains("high"), "Should say high");	
	}
}
