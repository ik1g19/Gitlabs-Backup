import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
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

class TesterTest {
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
	@DisplayName("Tests opening the lab door")
	public void testMain(){
		String[] args = null;

		Tester.main(args);
		
		String output = outputHarness.getOutput();
		
		String expecteda = "The door won't open!";
		String expectedb = "The door is open.";
		
		String[] tests = output.split("Trying to open the door to Research Labs...");
		
		assertTrue(tests[1].contains(expecteda), "Tests whether the door is locked");
		assertTrue(tests[2].contains(expectedb), "Tests whether the door is open");
		
	}

}
