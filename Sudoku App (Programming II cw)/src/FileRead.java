import java.io.*;

public class FileRead {
    BufferedReader reader;

    public FileRead(String path) {
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(path),"ISO-8859-1"));
            reader.mark(0);
        } catch (IOException e) {
            System.err.println(e);
        }
    }

    public String getLine() {
        String line = "";

        try {
            line = reader.readLine();
        } catch (IOException e) {
            System.err.println(e);
        }

        return line;
    }
}
