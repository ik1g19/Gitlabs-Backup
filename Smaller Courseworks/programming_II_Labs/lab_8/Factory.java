import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.Seat;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.SeatFactory;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.Tuple;

public class Factory implements SeatFactory {
    @Override
    public Tuple<Class<? extends Seat>, Class<? extends Seat>> getSeats() {
        Tuple<Class<? extends Seat>, Class<? extends Seat>> seats =
                new Tuple<>(Seat1.class, Seat2.class);

        return seats;
    }
}
