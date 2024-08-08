import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
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

@DisplayName("Lab 1 part 1")
class HelloTest {
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
	@DisplayName("Test printing of \"Hello World!\"")
	public void testMain(){
		try {
			Hello.main(null);
		} catch (Exception e) {
			fail("threw an exception");
		}

		String output = outputHarness.getOutput();
		String expected = "Hello World!";
		
		assertNotNull(output, "Tests that there is something printed to the terminal");
		assertTrue(output.startsWith(expected), "Tests that \"Hello World!\" has printed");
	}

}
