import java.io.IOException;
import java.io.Writer;

class BTreePrint
{
    public static void printTree(Writer out, BinaryNode t) throws IOException
    {	
        if (t.right != null) {
            printTree(out, true, "", t.right);
        }
        printNodeValue(out, t);
        if (t.left != null) {
            printTree(out, false, "", t.left);
        }
    }

    private static void printNodeValue(Writer out, BinaryNode t) throws IOException
    {
        out.write(t.key);
        out.write('\n');
    }

    private static void printTree(Writer out, boolean isRight, String indent, BinaryNode t) throws IOException
    {
        if (t.right != null) {
            printTree(out, true, indent + (isRight ? "        " : " |      "), t.right);
        }
        out.write(indent);
        if (isRight) {
        	out.write(" /");
        } else {
        	out.write(" \\");
        }
        out.write("----- ");
        printNodeValue(out, t);
        if (t.left != null) {
            printTree(out, false, indent + (isRight ? " |      " : "        "), t.left);
        }
    }
}
