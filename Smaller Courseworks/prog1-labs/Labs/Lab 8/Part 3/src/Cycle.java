/*
cycle is an abstract class as all cycles will need to replace their chain and are likely to be done so in the same way,
a cycle is a road vehicle so it implements roadvehicle
 */
public abstract class Cycle implements RoadVehicle{
    //replaces the chain of a cycle
    public void replaceChain() {};
}
