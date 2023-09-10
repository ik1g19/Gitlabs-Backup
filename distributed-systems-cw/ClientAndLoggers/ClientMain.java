import java.io.File;
import java.io.IOException;

public class ClientMain {
	
	public static void main(String[] args) throws Exception{
		
		final int cport = Integer.parseInt(args[0]);
		int timeout = Integer.parseInt(args[1]);
			
		File downloadFolder = new File("downloads");
		if (!downloadFolder.exists())
			if (!downloadFolder.mkdir()) throw new RuntimeException("Cannot create download folder (folder absolute path: " + downloadFolder.getAbsolutePath() + ")");
		
		
		testClient(cport, timeout, downloadFolder);
		
	}
	
	
	public static void testClient(int cport, int timeout, File downloadFolder) {
		Client client = null;
		
		try {
			
			client = new Client(cport, timeout, Logger.LoggingType.ON_FILE_AND_TERMINAL);
		
			try { client.connect(); } catch(IOException e) { e.printStackTrace(); return; }
			
//			try { list(client); } catch(IOException e) { e.printStackTrace(); }
			
			try { client.store(new File("test.txt")); } catch(IOException e) { e.printStackTrace(); }

			try { client.store(new File("test2.txt")); } catch(IOException e) { e.printStackTrace(); }

			try {Thread.sleep(1000);} catch (InterruptedException e) {System.out.println(e);}
			
//			try { client.store(new File("Clipboard01.pdf")); } catch(IOException e) { e.printStackTrace(); }
//
//			try { client.store(new File("Clipboard01.jpg")); } catch(IOException e) { e.printStackTrace(); }
//
			String list[] = null;
			try { list = list(client); } catch(IOException e) { e.printStackTrace(); }
//
//			try { client.load("test.txt", downloadFolder); } catch(IOException e) { e.printStackTrace(); }
			
//			if (list != null)
//				for (String filename : list)
//					try { client.remove(filename); } catch(IOException e) { e.printStackTrace(); }
//			try { client.remove("test.txt"); } catch(IOException e) { e.printStackTrace(); }
			
//			try { list(client); } catch(IOException e) { e.printStackTrace(); }
			
		} finally {
			if (client != null) try { client.disconnect(); } catch(Exception e) { e.printStackTrace(); }
		}
	}

	public static String[] list(Client client) throws IOException, NotEnoughDstoresException {
		System.out.println("Retrieving list of files...");
		String list[] = client.list();
		
		System.out.println("Ok, " + list.length + " files:");
		int i = 0; 
		for (String filename : list)
			System.out.println("[" + i++ + "] " + filename);
		
		return list;
	}
	
}
