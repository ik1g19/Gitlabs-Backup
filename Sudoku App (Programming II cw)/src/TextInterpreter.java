//interprets the given text to provide information to build the grid
//can handle loading from files or direct text
public class TextInterpreter {
    //boolean that stores whether the text was loaded from a file
    private boolean loadedFromFile;

    private FileRead reader;

    private String line;

    private String[] gridLines;
    private Integer currentLineNumber = 0;

    private boolean hasNext = true;

    private String[] locationsInCurrentCage;
    private String targetOfThisCage;

    //keeps track of the current location being accessed in the cage
    private Integer locationCount = 0;
    private boolean hasLocation = true;
    private String currentLocation;

    public TextInterpreter(String text, boolean loadedFromFile) {
        this.loadedFromFile = loadedFromFile;

        //if the text was loaded from a file
        if (loadedFromFile) {
            reader = new FileRead(text);
        }
        //otherwise the grid was given as direct text
        else {
            gridLines = text.split("\n");
        }
    }

    public void processNextLine() {
        //checking if the final line has been processed
        if (loadedFromFile) {
            line = reader.getLine();

            //if the reader returns null then flag hasNext as false
            if (line == null) hasNext = false;
        }
        else {
            //if there are no more lines left in the text then flag hasNext as false
            if (currentLineNumber >= gridLines.length) {
                hasNext = false;
            }
            //otherwise read next line normally
            else {
                line = gridLines[currentLineNumber];
                currentLineNumber++;
            }
        }

        //only process a new line if the end of the file hasn't been reached
        if (hasNext) {
            //fetching all locations in this cage
            String locationsList = returnSpaceSplit(line, 2);
            locationsInCurrentCage = locationsList.split(",");

            //fetching the target of this cage
            targetOfThisCage = returnSpaceSplit(line, 1);
        }
    }


    public void processNextCage() {
        //if there are no more locations left in the cage
        if (locationCount >= locationsInCurrentCage.length) {
            hasLocation = false;
        }
        else {
            currentLocation = locationsInCurrentCage[locationCount];

            locationCount++;
        }
    }


    //returns whether there are any more lines to read
    public boolean hasNext() {
        return hasNext;
    }


    public String getCageTarget() {
        return targetOfThisCage;
    }


    //returns whether there are many more locations in the cage
    public boolean hasLocation() {
        return hasLocation;
    }


    //returns the location of the current tile in the cage
    public Integer getCurrentCageLocation() {
        return Integer.parseInt(currentLocation);
    }


    public void resetLocationCount() {
        //resetting the location count
        locationCount = 0;
        hasLocation = true;
    }


    //fully resets the interpreter
    public void reset() {
        locationCount = 0;
        hasLocation = true;
        currentLineNumber = 0;
        hasNext = true;
    }


    //splits a string by spaces and returns the nth split
    private String returnSpaceSplit(String str, int n) {
        String strSplit[] = str.split("\\s+");
        return strSplit[n-1];
    }
}