/*
car is a class as it needs to be instantiated, it is a refuelable form of transport and a road vehicle
so it implements both refuelable and roadvehicle
 */
public class Car implements Refuelable, RoadVehicle{
    //turns the ignition on the car
    public void turnIgnition() {}


    //implementation for the refueling of the car
    public void refuel() {}


    //implementation to increase and decrease the velocity of the car
    public void increaseVelocity() {}

    public void decreaseVelocity() {}


    //implementation for replacing the wheels of a car
    public void replaceWheels() {}
}
