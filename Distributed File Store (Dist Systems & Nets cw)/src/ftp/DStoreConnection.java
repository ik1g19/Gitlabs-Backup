package ftp;

import java.util.List;
import java.util.stream.Collectors;

public class DStoreConnection {

    private FileIndex file_index;
    private int port;
    private int id;



//    public DStoreConnection(List<String[]> files, int port, int id) {
//        this.port = port;
//        this.id = id;
//
//        List<DStoreFile> dStoreFiles = files.stream()
//                .map(x -> new DStoreFile(x[0],Long.parseLong(x[1])))
//                .collect(Collectors.toList());
//
//        file_index = new FileIndex(dStoreFiles);
//    }

    public DStoreConnection(int port, int id) {
        this.port = port;
        this.id = id;

        file_index = new FileIndex();
    }

    public DStoreConnection(List<DStoreFile> dStoreFiles, int port, int id) {
        this.port = port;
        this.id = id;

        file_index = new FileIndex(dStoreFiles);
    }



    public void addFile(String filename, Long filesize) { DStoreFile file = file_index.addFile(filename,filesize); }

    public void addFile(DStoreFile file) { file_index.put(file.getFilename(),file); }



    public void removeFile(String filename) { file_index.remove(filename); }



    public int getPort() { return port; }



    public int getID() { return id; }



    public FileIndex getFileIndex() { return file_index; }

}
