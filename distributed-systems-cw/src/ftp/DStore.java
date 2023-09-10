package ftp;

import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DStore extends Server {

    int cport;
    String file_folder;


    /**
     * @desc constructs a client
     * @param port port to listen on
     * @param cport controller port to talk to
     * @param timeout timeout (ms)
     * @param file_folder where to store data locally
     */
    public DStore(int port, int cport, int timeout, String file_folder) {

        this.port = port;
        this.cport = cport;
        this.timeout = timeout;
        this.file_folder = file_folder;


        Socket controller = connectToController();


        if (controller != null) {
            for(File file: new File(file_folder).listFiles()) file.delete();
            send("JOIN " + port, controller, "Controller");
            handleRequest(readSocket(controller), controller);
            start();
        }
        else threadIDErr("Unable to connect to Controller");

    }



    public Socket connectToController() {
        Socket controller = null;

        Boolean joined = false;

        for (int i = 0; (i < 10) && !joined; i++) {
            try { controller = new Socket("localhost",cport); joined = true; }
            catch (IOException e) {
                threadIDErr(e.getMessage());

                try {threadIDErr("Retrying Connection..."); Thread.sleep(1000);}
                catch (InterruptedException exc) {threadIDErr(exc.getMessage());}
            }
        }

        return controller;
    }



    public static void main(String args[]) {
        String numberArgs[] = Arrays.copyOf(args, args.length-1);
        Stream<String> str = Arrays.stream(numberArgs);

        List<Integer> intArgs = str.map(x -> {return Integer.parseInt(x);})
                .collect(Collectors.toList());


        DStore dStore = new DStore(intArgs.get(0), intArgs.get(1), intArgs.get(2), args[3]);
    }



    @Override
    protected void handleRequest(String request, Socket client) {
        String args[] = request.split(" ");


        String command = args[0];

        if (command.equals("LIST")) {
            Socket controller = client;

            File folder = new File(file_folder);

            String fileMessage = "empty";


            if (folder.listFiles().length > 0) {
                fileMessage = Arrays.stream(folder.listFiles()).
                        map(x -> x.getName() + " " + x.length()).
                        collect(Collectors.joining("|"));
            }


            send("LIST " + fileMessage, controller, "Controller");


            String response = readSocket(controller);
            if (response.equals("JOINED")) threadIDOutput("Successfully joined Controller");


            closeConnection("Controller", controller);
        }

        else if (command.equals("STORE")) {
                String filename = args[1];
                Long filesize = Long.parseLong(args[2]);

                send("ACK", client, "Client");


                try {
                    receiveFile(client, file_folder + "\\" + filename);
                } catch (IOException e) {
                    threadIDErr(e.getMessage());
                }


                Socket controller = connectToController();

                if (controller != null) {
                    send("STORE_ACK " + filename, controller, "Controller");
                } else threadIDErr("Unable to connect to Controller");

                closeConnection("Controller", controller);
        }

        else if (command.equals("LOAD_DATA")) {
            String filename = args[1];

            try {
                sendFile(client, file_folder + "\\" + filename);
            } catch (IOException e) {
                threadIDErr(e.getMessage());
            }
        }

        else if (command.equals("REMOVE")) {
            String filename = args[1];


            if (deleteFile(file_folder + "\\" + filename)) threadIDOutput("Deleted file " + filename);
            else threadIDErr("Failed to delete file " + filename);


            Socket controller = connectToController();

            if (controller != null) {
                send("REMOVE_ACK " + filename, controller, "Controller");
            }
            else threadIDErr("Unable to connect to Controller");

            closeConnection("Controller", controller);
        }
    }



    public void receiveFile(Socket client, String destination) throws IOException {
        InputStream in = client.getInputStream();

        byte[] buf = new byte[1000]; int buflen;

        File outputFile = new File(destination);
        FileOutputStream out = new FileOutputStream(outputFile);


        threadIDOutput("Starting file Write...");

        while ((buflen = in.read(buf)) != -1) {
            out.write(buf,0,buflen);
        }

        threadIDOutput("Finished file Write");


        //in.close();
        out.close();

    }



    public void sendFile(Socket client, String filepath) throws IOException {
        File inputFile = new File(filepath);
        FileInputStream inf = new FileInputStream(inputFile);

        OutputStream out = client.getOutputStream();

        byte[] buf = new byte[1000]; int buflen;

        threadIDOutput("Starting file Send");

        while ((buflen = inf.read(buf)) != -1) {
            out.write(buf, 0, buflen);
        }

        threadIDOutput("Finished file Send");


        inf.close();
        //client.close();
        //out.close();
    }



    public Boolean deleteFile(String filepath) {
        File file = new File(filepath);

        return file.delete();
    }

}
