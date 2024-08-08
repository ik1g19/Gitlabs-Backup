/*
roadvehicle is an interface as all road vehicles will need to replace their wheels,
but this will be done in a different way for each vehicle
 */
interface RoadVehicle extends Transport{
    //replaces the wheels
    public void replaceWheels();
}
