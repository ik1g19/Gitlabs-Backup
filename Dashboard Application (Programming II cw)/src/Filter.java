package src;

import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.HBox;

abstract class Filter {

    public abstract void load(); // Need implementation
    public abstract void filter(); // Need implementation
    public abstract void tearDown(); // Need implementation

    public Scene done() {
        BorderPane bp = new BorderPane();
        FlowPane fp = new FlowPane();
        HBox title = new HBox();

        Image tick = new Image("/resources/tick.png");
        Label done = new Label("Done");

        fp.setAlignment(Pos.CENTER);
        fp.getChildren().add(new ImageView(tick));

        title.setAlignment(Pos.CENTER);
        title.getChildren().add(done);

        bp.setCenter(fp);
        bp.setTop(title);

        return new Scene(bp);
    };

}
