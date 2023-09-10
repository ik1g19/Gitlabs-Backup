/*
transport is an interface as all forms of transport will increase and decrease in velocity,
but in different ways
 */

interface Transport {
    //increases the velocity of the transport vehicle
    public void increaseVelocity();

    //decreases the velocity of the transport vehicle
    public void decreaseVelocity();
}
