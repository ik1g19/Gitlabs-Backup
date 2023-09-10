import uk.ac.soton.ecs.comp1206.labtestlibrary.datastructure.Tree;
import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.recursion.MinimumInTree;

//finds the minimum element in a tree recursively
public class MinTree implements MinimumInTree {
    //main method
    public static void main(String[] args){
        Tree tree = new Tree( 24,
                new Tree( 45,
                        null ,
                        new Tree(8, null , null) ) ,
                new Tree ( 17,
                        new Tree (74 , null , null ) ,
                        null ) );
        MinTree minTree = new MinTree();
        //outputting the minimum element in the tree
        System.out.println("Minimum is: " + minTree.findMin(tree));
    }

    //returns the minimum element in a tree
    public int findMin(Tree tree) {
        //finds the minimum of the subtrees
        int subtreesMin = findMinSubtrees(tree);

        //if the root is smaller than the minimum element in the subtrees then the root is the minimum element of the tree
        if (tree.getVal() < subtreesMin) return tree.getVal();
        //otherwise the minimum element of the subtrees is the minimum element of the tree
        else return subtreesMin;
    }

    //returns the minimum element of the subtrees of a given tree
    public int findMinSubtrees(Tree tree) {
        //if the tree has no children then the root is the minimum element
        if (tree.left() == null && tree.right() == null) return tree.getVal();
        //if the tree has a right child then the minimum element is tte minimum element in the right subtree
        else if (tree.left() == null && tree.right() != null) return findMin(tree.right());
        //if the tree has a left child then the minimum element is tte minimum element in the left subtree
        else if (tree.left() != null && tree.right() == null) return findMin(tree.left());
        //otherwise if the tree has two children
        //the minimum of the subtrees is the smallest minimum of the left and right subtrees
        else {
            int leftMin = findMin(tree.left());
            int rightMin = findMin(tree.right());

            //if the leftt subtrees minimum is smaller then return the left subtrees minimum
            if (leftMin < rightMin) return leftMin;
            //otherwise return the right subtrees minimum
            else return rightMin;
        }
    }


}
