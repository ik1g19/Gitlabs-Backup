package ftp;

import java.io.*;
import java.net.*;

public abstract class Server {

    protected int port;
    protected int timeout;



    protected void start() {
        try {
            ServerSocket ss = new ServerSocket(port);
            while (true) {
                try {
                    System.out.println("Waiting for Connection...");
                    final Socket client = ss.accept();
                    new Thread(new Runnable() {
                        public void run() {
                            try {
                                threadIDOutput("Connection Accepted");
                                Boolean closed = false;
                                while (!closed) {
                                    String request = readSocket(client);
                                    if (request == null) closed = true;
                                    else handleRequest(request, client);
                                }
                                closeConnection("Client", client);
                            } catch (Exception e) {
                                threadIDErr("Exception thrown: " + e.getMessage());
                            }

                            threadIDOutput("Thread Stopping");
                        }
                    }).start();
                } catch (Exception e) {
                    System.out.println("error " + e);
                }
            }
        } catch (Exception e) {
            System.out.println("error " + e);
        }
    }



    protected abstract void handleRequest(String request, Socket client);



    protected void send(String msg, Socket socket, String destinationName) {
        try {
            PrintWriter out = new PrintWriter(socket.getOutputStream());
            out.println(msg);
            out.flush();
            threadIDOutput("Sent: \"" + msg + "\" to " + destinationName + " on port: " + socket.getPort());
        }
        catch (IOException e) {
            threadIDErr("Error: " + e);
        }
    }

    protected void send(String msg, String hostname, int port, String destinationName) {
        try {
            Socket socket = new Socket(hostname, port);
            PrintWriter out = new PrintWriter(socket.getOutputStream());
            out.println(msg);
            out.flush();
            threadIDOutput("Sent: \"" + msg + "\" to " + destinationName);
        }
        catch (IOException e) {
            threadIDErr("Error: " + e);
        }
    }



    protected String readSocket(Socket socket) {
        String request = null;

        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            request = in.readLine();
            threadIDOutput("Recieved: " + request);
        }
        catch (IOException e) {
            threadIDErr("Read Socket Error: " + e);
        }

        return request;
    }



    protected void threadIDOutput(String output) {
        System.out.println("Thread ID: " + Thread.currentThread().getId() + " " + output);
    }

    protected void threadIDErr(String output) {
        System.err.println("Thread ID: " + Thread.currentThread().getId() + " " + output);
    }



    protected void closeConnection(String name, Socket client) {
        try {client.close(); threadIDOutput(name + " Socket Closed");} catch (IOException e) {threadIDErr(e.getMessage());}
    }

}