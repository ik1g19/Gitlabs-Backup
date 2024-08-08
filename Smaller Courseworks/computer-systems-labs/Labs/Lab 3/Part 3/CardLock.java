class CardLock {
    //instance variable to hold the object for the last swiped card
    private SmartCard card;
    //boolean to hold whether student access is allowed
    private Boolean studentAccess;
    
    //a new card lock will have student access not allowed
    public CardLock() {
        studentAccess = false;
    }
    
    //stores the last swiped card in the card object
    public void swipeCard(SmartCard card) {
        this.card = card;
    }
    
    //returns the most recently swiped card
    public SmartCard getLastCardSeen() {
        return this.card;
    }
    
    //if it is a staff card the door will always unlock, otherwise the door
    //will only unlock if student access is allowed
    public Boolean isUnlocked() {
        if (card.isStaff())
            return true;
        else if (studentAccess == true)
            return true;
        else
            return false;
    }
    
    //with every call student access is inverted
    public void toggleStudentAccess() {
        studentAccess = !studentAccess;
    }
}