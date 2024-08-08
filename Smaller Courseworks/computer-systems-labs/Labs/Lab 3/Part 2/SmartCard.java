class SmartCard{
    private String owner;
    
    private Boolean staffStatus;
    
    //sets the owner of the card and sets the default staff status to false
    public SmartCard(String owner) {
        this.owner = owner;
        staffStatus = false;
    }
    
    //returns the owners name
    public String getOwner() {
        return owner;
    }
    
    //returns whether the card owner is staff or not
    public Boolean isStaff() {
        return staffStatus;
    }
    
    //sets a card to either a staff card or a stduent card
    public void setStaff(Boolean status) {
        staffStatus = status;
    }
}