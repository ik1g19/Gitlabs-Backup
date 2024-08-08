package ftp;

import java.io.IOException;
import java.net.Socket;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Controller extends Server {

    private int r;
    private int rbPeriod;
    private int nextID = 0;

    private DStoreIndex dStoreIndex;
    private FileIndex fileIndex;


    /**
     * @desc constructs a controller
     * @param cport to listen on
     * @param r replication factor
     * @param timeout timeout (ms)
     * @param rbPeriod rebalance period (ms)
     */
    public Controller(int cport, int r, int timeout, int rbPeriod) throws IOException {
        this.port = cport;
        this.r = r;
        this.timeout = timeout;
        this.rbPeriod = rbPeriod;

        dStoreIndex = new DStoreIndex();
        fileIndex = new FileIndex();

        start();
    }



    public static void main(String args[]) {
        Stream<String> str = Arrays.stream(args);

        List<Integer> intArgs = str.map(x -> {return Integer.parseInt(x);})
                .collect(Collectors.toList());

        try {
            Controller ctrl = new Controller(intArgs.get(0), intArgs.get(1), intArgs.get(2), intArgs.get(3));
        } catch (IOException e) {
            System.out.println("IOException " + e.getMessage());
        }
    }



    @Override
    protected void handleRequest(String request, Socket client) {
        String args[] = request.split(" ");


        String command = args[0];

        if (command.equals("JOIN")) {
            Socket dStoreSock = client;

            Integer port = Integer.parseInt(args[1]);


            rebalance();


            DStoreConnection dStore;


            if (!files.equals("empty")) {
                List<DStoreFile> dStoreFiles = Arrays.stream(files.split("\\|")).
                        map(x -> x.split(" ")).
                        map(x -> new DStoreFile(x[0], Long.parseLong(x[1]))).
                        collect(Collectors.toList());


                dStore = new DStoreConnection(dStoreFiles, port, nextID);
                dStoreFiles.stream().forEach(x -> x.addDstore(dStore));
            }
            else dStore = new DStoreConnection(port, nextID);


            dStoreIndex.addDStore(dStore);

            threadIDOutput("New Dstore (ID: " + nextID + ") successfully joined");

            nextID++;


            send("JOINED", dStoreSock, "DataStore");
        }

        else if (command.equals("STORE")) {
            String filename = args[1];
            Long filesize = Long.parseLong(args[2]);

            DStoreFile file = fileIndex.addFile(filename, filesize);

            file.setStoreInProgress(true);


            List<DStoreConnection> dStores = dStoreIndex.getFirstN(r);

            dStores.stream().
                    forEach(x -> {
                        x.addFile(file);
                        file.addDstore(x);
                    });

            String ports = dStores.stream().
                    map(x -> Integer.toString(x.getPort())).
                    collect(Collectors.joining(" "));


            file.setAcksQuota(r);

            send("STORE_TO " + ports, client, "Client");


            Boolean fileStored = false;
            while (!fileStored) {
                if (file.storeAckCheck()) {
                    file.setStoreInProgress(false);
                    fileStored = true;

                    threadIDOutput("Store of file " + filename + " complete");

                    send("STORE_COMPLETE", client, "Client");
                }
            }
        }

        else if (command.equals("STORE_ACK")) {
            String filename = args[1];
            DStoreFile file = fileIndex.get(filename);

            file.storeAck();
        }

        else if (command.equals("LOAD")) {
            String filename = args[1];

            DStoreFile file = fileIndex.get(filename);
            int dStorePort = file.getDstore().getPort();

            send("LOAD_FROM " + dStorePort + " " + file.getFilesize(), client, "Client");
        }

        else if (command.equals("REMOVE")) {
            String filename = args[1];
            DStoreFile file = fileIndex.get(filename);

            file.setRemoveInProgress(true);


            DStoreIndex dStores = file.getDStores();

            dStores.entrySet().stream().
                    forEach(x -> {
                        Socket dStore = connectToDStore(x.getValue());

                        if (dStore != null) {
                            send("REMOVE " + filename, dStore, "DataStore");
                        } else threadIDErr("Unable to connect to DataStore");

                        closeConnection("DataStore", dStore);


                        x.getValue().removeFile(filename);
                    });


            Boolean fileRemoved = false;
            while (!fileRemoved) {
                if (file.removeAckCheck()) {
                    fileIndex.remove(filename);


                    file.setRemoveInProgress(false);
                    fileRemoved = true;


                    threadIDOutput("Remove of file " + filename + " complete");

                    send("REMOVE_COMPLETE", client, "Client");
                }
            }
        }

        else if (command.equals("REMOVE_ACK")) {
            String filename = args[1];
            DStoreFile file = fileIndex.get(filename);

            file.removeAck();
        }

        else if (command.equals("LIST")) {
            String filenames = fileIndex.entrySet().stream().
                    map(x -> x.getValue().getFilename()).
                    collect(Collectors.joining(" "));

            send("LIST " + filenames, client, "Client");
        }
    }



    public Socket connectToDStore(DStoreConnection dStoreConnection) {
        Socket dStore = null;

        Boolean joined = false;

        for (int i = 0; (i < 10) && !joined; i++) {
            try { dStore = new Socket("localhost", dStoreConnection.getPort()); joined = true; }
            catch (IOException e) {
                threadIDErr(e.getMessage());

                try {threadIDErr("Retrying Connection..."); Thread.sleep(1000);}
                catch (InterruptedException exc) {threadIDErr(exc.getMessage());}
            }
        }

        return dStore;
    }



    public void rebalance() {
        Integer nODS = dStoreIndex.entrySet().size();

        Boolean rCopies = fileIndex.entrySet().stream().
                map(x -> x.getValue().getDStores().size() == r).
                reduce(true, (a,b) -> a && b);


        List<DStoreConnection> lightDStores = dStoreIndex.entrySet().stream().
                map(x -> x.getValue()).
                filter(x -> {
                    Integer nOF = x.getFileIndex().size();
                    return nOF < Math.floor(r * nOF / nODS);
                }).
                collect(Collectors.toList());


        List<DStoreConnection> bloatedDStores = dStoreIndex.entrySet().stream().
                map(x -> x.getValue()).
                filter(x -> {
                    Integer nOF = x.getFileIndex().size();
                    return nOF < Math.ceil(r * nOF / nODS);
                }).
                collect(Collectors.toList());


        if (rCopies && lightDStores.isEmpty() && bloatedDStores.isEmpty()) {

        }
        else {
            DStoreConnection dStore = bloatedDStores.get(0);
            String firstFilename = (String) dStore.getFileIndex().values().toArray()[0];
            dStore.removeFile(firstFilename);

            DStoreConnection dStore2 = lightDStores.get(0);
            dStore2.addFile();
        }
    }

}