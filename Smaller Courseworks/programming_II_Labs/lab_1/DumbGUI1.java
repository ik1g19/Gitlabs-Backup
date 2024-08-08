import javafx.scene.control.*;
import javafx.application.Application;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.Region;
import javafx.stage.Stage;

public class DumbGUI1 extends Application {
	public void start(Stage stage) {
		
		//creating the pane
		FlowPane pane = new FlowPane(Orientation.HORIZONTAL);
		pane.setAlignment(Pos.CENTER);
		
		
		//adding a textfield
		pane.getChildren().add(new TextField());
		
		
		//inserting a line between the textfield and buttons
		Region newline = new Region();
		newline.setPrefSize(Double.MAX_VALUE, 0.0);
		pane.getChildren().add(newline);
		
		
		//adding buttons
		pane.getChildren().addAll(new Button("Submit"), new Button("Cancel"));
		
		
		//setting the distance between nodes
		pane.setHgap(10);
		pane.setVgap(5);
		
		
		//creating a scene from the pane
		Scene scene = new Scene(pane);
		//creating a stage from the scene
		stage.setScene(scene);
		
		
		//setting the default width and height of the stage
		stage.setWidth(200);
		stage.setHeight(150);
		
		
		//displaying the stage
		stage.show();
		
	}
	
	public static void main(String[] args) {
		launch(args);
	}
}