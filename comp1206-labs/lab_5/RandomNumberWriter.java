import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.io.RandomIO;
import java.util.Arrays;
import java.util.Random;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.FileWriter;
import java.io.Writer;
import java.nio.ByteBuffer;

//class writes 10000 integers using char and byte streams
public class RandomNumberWriter implements RandomIO {
	//random number generator member variable
	private Random generator;

	//constructor initialises generator with given seed
	RandomNumberWriter(long seed) {
		generator = new Random(seed);
	}
	
	//writes 10000 integers using a byte based stream
	public void writeRandomByte(String filename) {
		
		try {
		//getting the file ready
		File file = new File(filename);
	
		//creating the byte based stream 
		OutputStream out = new FileOutputStream(file);
		
		//writing 10000 random integers to the file
		for (int i = 0; i < 10000; i++) {
		
			//generating a new int
			int num = generator.nextInt(100000);
			
			//converting the int to a 4 byte byte array
			byte b[] = ByteBuffer.allocate(4).putInt(num).array();
			
			//writing the byte array to the file
			out.write(b);
			
		}
		
		//closing the byte stream
		out.close();
		} catch (Exception e) {
			System.out.println(e);
		}
	}
	
	//writes 10000 integers using a char based stream
	public void writeRandomChars(String filename) {
		try {
			//getting the file ready
			File file = new File(filename);
		
			//creating the character based stream 
			Writer out = new FileWriter(file);
			
			//writing 10000 random integers to the file
			for (int i = 0; i < 10000; i++) {
			
				//converts the integer to a string
				String num = Integer.toString(generator.nextInt(100000));
				
				//writes the string to the file
				out.write(num + System.getProperty("line.separator"));
				
			}
			
			//closing the character stream
			out.close();
			} catch (Exception e) {
				System.out.println(e);
			}
	}
}
