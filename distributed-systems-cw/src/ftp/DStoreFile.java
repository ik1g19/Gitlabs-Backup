package ftp;

public class DStoreFile {

    private String filename;
    private Long filesize;

    private boolean storeInProgress;
    private boolean removeInProgress;

    private int storeAcksQuota;
    private int storeAcks = 0;

    private int removeAcksQuota;
    private int removeAcks = 0;

    private DStoreIndex dStoreIndex;



    public DStoreFile(String filename, Long filesize) {
        this.filename = filename;
        this.filesize = filesize;

        dStoreIndex = new DStoreIndex();
    }



    public boolean isStoreInProgress() {return storeInProgress;}
    public boolean isRemoveInProgress() {return removeInProgress;}



    public void setStoreInProgress(Boolean store) { storeInProgress = store; }
    public void setRemoveInProgress(Boolean remove) { removeInProgress = remove; }



    public String getFilename() { return filename; }
    public Long getFilesize() { return filesize; }



    public void setAcksQuota(int quota) { storeAcksQuota = quota; removeAcksQuota = quota; }

    public void setStoreAcksQuota(int quota) { storeAcksQuota = quota; }
    public int getStoreAcks() { return storeAcks; }



    public void setRemoveAcksQuota(int quota) { removeAcksQuota = quota; }
    public int getStoreAcksQuota() { return storeAcksQuota; }



    public int storeAck() { return storeAcks++; }
    public Boolean storeAckCheck() { return storeAcks == storeAcksQuota; }



    public int removeAck() { return removeAcks++; }
    public Boolean removeAckCheck() { return removeAcks == removeAcksQuota; }



    public void addDstore(DStoreConnection dStore) { dStoreIndex.put(dStore.getID(),dStore); }



    public DStoreConnection getDstore() {
        return dStoreIndex.getFirstAvailable();
    }



    public DStoreIndex getDStores() { return dStoreIndex; }

}
