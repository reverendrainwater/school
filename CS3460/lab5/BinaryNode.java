

public class BinaryNode
    {
        int key;
        BinaryNode left;
        BinaryNode right;

        BinaryNode(int x)
        {
            key = x;
            left = null;
            right = null;
        }

        BinaryNode(int x, BinaryNode lchild, BinaryNode rchild)
        {
            key = x;
            left = lchild;
            right = rchild;
        }

        public void setKey(int theKey)
        {
            key = theKey;
        }

        public void setLeft (BinaryNode leftChild)
        {
            left = leftChild;
        }

        public void setRight (BinaryNode rightChild)
        {
            right = rightChild;
        }

        public BinaryNode getLeft()
        {
            return left;
        }

        public BinaryNode getRight()
        {
            return right;
        }

        public int getKey()
        {
            return key;
        }
    }