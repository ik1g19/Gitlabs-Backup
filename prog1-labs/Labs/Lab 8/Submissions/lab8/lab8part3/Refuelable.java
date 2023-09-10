/*
refuelable is an interface as all refuelable forms of transport will need to refuel,
but each will do so in a different way
 */
interface Refuelable extends Transport {
    //refuels the vehicle
    public void refuel();
}
