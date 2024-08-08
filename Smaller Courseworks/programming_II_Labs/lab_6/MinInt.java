import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.MinimumInArray;

//finds the minimum element in an array recursively
public class MinInt implements MinimumInArray {
    //main method
    public static void main(String[] args){
        int[] arr = {24,52,74,9,34,23,64,34};
        MinInt minInt = new MinInt();
        //outputting the minimum value
        System.out.println("Minimum is: " + minInt.findMin(arr));
    }

    //returns the minimum value given an array
    public int findMin(int[] array) {
        //if the array only contains one element then the element is the minimum
        if (array.length == 1) return array[0];

        //if the first element is smaller than the minimum of the rest of the array, then it is the minimum
        if (array[0] < findMinAux(1, array)) return array[0];
        //otherwise the minimum of the rest of the array is the minimum
        else return findMinAux(1,array);
    }

    //auxillary function to return minimum from an array from a given index onward
    public int findMinAux(int n, int[] array) {
        int[] newArray = new int[array.length-1];
        //creating a new array starting with the nth element of the original array
        System.arraycopy(array,n,newArray,0,array.length-1);
        //returning the minimum of the new array
        return findMin(newArray);
    }
}
