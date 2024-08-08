import javafx.scene.control.*;
import javafx.geometry.Insets;
import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

public class DumbGUI2 extends Application {
	public void start(Stage stage) {
		
		//creating the overall horizontal box
		HBox calculator = new HBox();
		
		calculator.setSpacing(10);
		calculator.setPadding(new Insets(30, 20, 30, 20));
		
		
		//creating the input text field
		Label variable = new Label("x=");
		
		TextField input = new TextField("1");
		
		calculator.getChildren().addAll(variable, input);
		
		
		//creating the vertical box for the two buttons
		VBox changeValue = new VBox();
		
		Button plusOne = new Button("+1");
		
		Button minusOne = new Button("-1");
		
		changeValue.getChildren().addAll(plusOne, minusOne);
		
		changeValue.setAlignment(Pos.CENTER);
		
		calculator.getChildren().add(changeValue);
		
		
		//creating the function label
		Label function = new Label("Function:");
		
		calculator.getChildren().add(function);
		
		
		//creating the drop down box for the functions
		ChoiceBox functions = new ChoiceBox();
		
		functions.getItems().addAll("X^X", "sin(x)", "cos(x)", "Fibonacci(x)");
		
		functions.setValue("X^X");
		
		calculator.getChildren().add(functions);
		
		
		//creating the output labels and text box
		Label equals = new Label("=");
		
		TextField output = new TextField();
		
		calculator.getChildren().addAll(equals, output);
		
		
		//creating the slider
		Slider slider = new Slider(1, 9, 1);
		
		slider.setShowTickMarks(true);
		
		slider.setShowTickLabels(true);
		
		slider.setMajorTickUnit(2);
		
		slider.setMinorTickCount(1);
		
		calculator.getChildren().add(slider);
		
		
		//adding action event handlers to the buttons 
		plusOne.addEventHandler(ActionEvent.ANY, new ButtonHandler(input, slider, 1.0));
		minusOne.addEventHandler(ActionEvent.ANY, new ButtonHandler(input, slider, -1.0));
		
		
		//adding a listener to the input textfield (uses an anonymous inner class)
		input.textProperty().addListener(new ChangeListener<String>() {
			
			//when the input text field is changed the output textfield is updated according
			//to the selected function
			public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
				int function = functions.getSelectionModel().getSelectedIndex();
				Double inputValue = Double.parseDouble(input.getText());
				
				if (function == 0) output.setText(Double.toString(Math.pow(inputValue, inputValue)));
				else if (function == 1) output.setText(Double.toString(Math.sin(inputValue)));
				else if (function == 2) output.setText(Double.toString(Math.cos(inputValue)));
				else if (function == 3) {
					output.setText(Double.toString(fibonacci(inputValue)));
				};
			}
			
			//a recursive function to calculate the fibonacci sequence
			private Double fibonacci(Double n) {
				if (n.equals(0.0)) return 0.0;
				else if (n.equals(1.0)) return 1.0;
				else return fibonacci(n - 1.0) + fibonacci(n - 2.0);
			}
			
		});
		
		
		//adding a listener to the slider
		slider.valueProperty().addListener(new ChangeListener<Number>() {
			
			//when the slider is changed the incremental buttons are changed accordingly
			public void changed(ObservableValue<? extends Number> observable, Number oldValue, Number newValue) {
				//locks the slider to integer values
				slider.setValue(newValue.intValue());
				
				Double value = slider.getValue();
				
				plusOne.setText("+" + Double.toString(value));
				minusOne.setText("-" + Double.toString(value));
			}
			
		});
		
		
		//aligning contents of the calculator
		calculator.setAlignment(Pos.CENTER);
		
		calculator.setSpacing(20);
		
		//allowing the text boxes to grow and shrink according to the size of the window
		HBox.setHgrow(input, Priority.ALWAYS);
		HBox.setHgrow(output, Priority.ALWAYS);
		
		
		//creating a scene from the pane
		Scene scene = new Scene(calculator);
		
		//creating a stage from the scene
		stage.setScene(scene);
		
		
		//setting the default width and height of the stage
		stage.setWidth(700);
		stage.setHeight(150);
		
		//setting the minimum width and height
		stage.setMinWidth(1000);
		stage.setMinHeight(150);
		
		
		//displaying the stage
		stage.show();
		
	}
	
	public static void main(String[] args) {
		launch(args);
	}
}

class ButtonHandler implements EventHandler<ActionEvent> {
	
	private Slider slider;
	private TextField input;
	private Double action;
	private Boolean increment;
	
	public ButtonHandler(TextField input, Slider slider, Double action) {
		this.slider = slider;
		
		this.input = input;
		
		this.action = action;
		
		//determines if the button is to increment or decrement
		increment = (action > 0) ? true : false;
	}

	public void handle(ActionEvent event) {
		//updating the action value
		action = slider.getValue();
		
		Double value = Double.parseDouble(input.getText());
		
		value += increment.equals(true) ? action : -action;
		
		input.setText(Double.toString(value));
	}

}