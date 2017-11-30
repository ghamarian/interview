public class GivenPathSize {
    // Definition for binary tree:
    static class Tree<T> {
        Tree(T x) {
            value = x;
        }

        T value;
        Tree<T> left;
        Tree<T> right;
    }

    static boolean hasPathWithGivenSum(Tree<Integer> root, int s) {
        if(root == null && s == 0){
            return true;
        }
        if(root == null){
            return false;
        }

        if(root.value == s && root.left == null && root.right == null)
            return true;

        return hasPathWithGivenSum(root.left, s - root.value) || hasPathWithGivenSum(root.right, s - root.value);
    }


    public static void main(String[] args) {
        Tree<Integer> root = new Tree<>(8);
        Tree<Integer> right = new Tree<>(3);
        root.right = right;

        System.out.println(hasPathWithGivenSum(root, 8));

    }



}
