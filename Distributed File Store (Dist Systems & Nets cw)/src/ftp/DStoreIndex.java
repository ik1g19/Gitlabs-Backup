package ftp;

import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

public class DStoreIndex extends HashMap<Integer, DStoreConnection> {

    public DStoreIndex() {}



    public DStoreConnection addDStore(List<DStoreFile> files, int port, Integer id) {
        DStoreConnection dStore = new DStoreConnection(files,port,id);
        put(id, dStore);
        return dStore;
    }

    public DStoreConnection addDStore(DStoreConnection dStore) {
        put(dStore.getID(), dStore);
        return dStore;
    }



    public List<DStoreConnection> getFirstN(int n) {
        return entrySet().stream()
                .map(x -> x.getValue())
                .limit(n)
                .collect(Collectors.toList());
    }



    public DStoreConnection getFirstAvailable() {
        return entrySet().stream()
                .map(x -> x.getValue())
                .collect(Collectors.toList())
        .get(0);
    }

}
