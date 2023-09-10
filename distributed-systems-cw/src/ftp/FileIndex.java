package ftp;

import java.util.HashMap;
import java.util.List;

public class FileIndex extends HashMap<String, DStoreFile> {

    public FileIndex() {}

    public FileIndex(List<DStoreFile> list) {
        list.stream().forEach(x -> put(x.getFilename(),x));
    }



    public DStoreFile addFile(String filename, Long filesize) {
        DStoreFile file = new DStoreFile(filename,filesize);
        put(filename,file);
        return file;
    }

}
