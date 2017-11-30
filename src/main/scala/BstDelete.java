public class BstDelete {

    static class Tree<T> {
        Tree(T x) {
            value = x;
        }
        T value;
        Tree<T> left;
        Tree<T> right;


    }

    public static String toString(Tree<Integer> t) {
        if (t == null) { return "";}
        String leftTree =  (t.left == null) ? "" : "(" + toString(t.left) + ")";
        String rightTree =  (t.right == null) ? "" : "(" + toString(t.right) + ")";
        return leftTree + "." + t.value + "." + rightTree;
    }

    static private Tree<Integer> deleteMin(Tree<Integer> x) {
        if (x.left == null) return x.right;
        x.left = deleteMin(x.left);
        return x;
    }

    static private Tree<Integer> min(Tree<Integer> x) {
        if (x.left == null) return x;
        else return min(x.left);
    }


    static private Tree<Integer> max(Tree<Integer> x) {
        if (x.right == null) return x;
        else  return max(x.right);
    }

    static private Tree<Integer> deleteMax(Tree<Integer> x) {
        if (x.right == null) return x.left;
        x.right = deleteMax(x.right);
        return x;
    }


    static private Tree<Integer> delete(Tree<Integer> x, int key) {
        if (x == null) return null;

        if      (x.value > key) x.left  = delete(x.left,  key);
        else if (x.value < key) x.right = delete(x.right, key);
        else {
            if (x.right == null) return x.left;
            if (x.left  == null) return x.right;

            Tree<Integer> t = x;
            x = max(t.left);
            x.left = deleteMax(t.left);
            x.right = t.right;
        }
        return x;
    }

    static Tree<Integer> deleteFromBST(Tree<Integer> t, int[] queries) {
        Tree<Integer> result = t;
        for (int query : queries) {
           result = delete(result, query);
        }
        return result;
    }

    public static void main(String[] args) {
       Tree<Integer> root = new Tree<>(2);
        System.out.println(toString(root));
        root = delete(root, 2);
        System.out.println(toString(root));
    }

}
