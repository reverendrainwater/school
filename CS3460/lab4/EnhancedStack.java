/////////////////////////////////////////////////////////////////////
//
//    IDENTIFICATION DIVISION.
//    PROGRAM-ID. EnhancedStack.java.
//    AUTHOR. Rev Taylor R Rainwater.
//    INSTALLATION. prophet.
//    DATE-WRITTEN. 20.2.2016.
//    DESCRIPTION. An implementation of the EnhancedStack data
//                 data structure.
//
/////////////////////////////////////////////////////////////////////
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class EnhancedStack<T extends Comparable<?super T>> implements Iterable<T>
{   private Node<T> smallest;
    private Node<T> top;
    private int size;
    private int modCount = 0;

    public EnhancedStack()
    {   size = 0;
        top = null;
        smallest = null;
    }

    public T top()
    {
        return top.element;
    }

    public int size()
    {
        return size;
    }

    public boolean isEmpty()
    {
        if(size == 0)
            return true;
        else
            return false;
    }

    public T minimum()
    {
        return smallest.element;
    }

    public void push(T data)
    {
        if(!isEmpty()){
            Node<T> oldNode = top;

            if(data.compareTo(minimum()) <= 0){
                Node<T> temp = new Node<T>(data, oldNode.next, smallest);
                temp.next = oldNode;
                temp.small = smallest;
                smallest = temp;
                top = temp;
            } else {
                Node<T> temp = new Node<T>(data, oldNode.next, smallest);
                temp.next = oldNode;
                temp.small = smallest;
                top = temp;
            }
        } else {
            Node<T> temp = new Node<T>(data, null, smallest);
            temp.next = null;
            smallest = temp;
            top = temp;
        }

        size++;
        modCount++;
    }
    
    public T pop()
    {   T node;
        Node<T> nextNode = top.next;

        if(isEmpty()){
            throw new NoSuchElementException();
        } else {
            node = top.element;
            smallest = top.small;
            top = nextNode;
            size--;
            modCount++;
        }
        return node;
    }

    @Override
    public Iterator<T> iterator()
    {
        return new enhancedIterator();
    }

    public void printAll()
    {
        System.out.println("ALL NODESs:");
        for(Node<T> traverse = top; traverse != null; traverse = traverse.next)
            System.out.println(traverse.element);
    }

    public void printSmallest()
    {
        System.out.println("SMALLEST NODES: ");
        for(Node<T> traverse = top; traverse != null; traverse = traverse.small)
            System.out.println(traverse.element);
    }
    /////////////////////////////////////////////////////////////////////
    //
    //    IDENTIFICATION DIVISION.
    //    CLASS-ID. enhancedIterator.
    //    AUTHOR. Rev Taylor R Rainwater.
    //    SUB-CLASS-OF. EnhancedStack.
    //    DATE-WRITTEN. 20.2.2016.
    //    DESCRIPTION. Iterator for the enhanced stack data structure.
    //
    /////////////////////////////////////////////////////////////////////
    private class enhancedIterator implements Iterator<T>
    {   private Node<T> current = top;
        private int expectedModCount = modCount;

        @SuppressWarnings("unused")
        private boolean okToRemove = false;

        public boolean hasNext()
        {
            return current != null;
        }

        public T next()
        {
            if(expectedModCount != modCount)
                throw new ConcurrentModificationException();
            if(!hasNext())
                throw new NoSuchElementException();

            T nextItem = current.element;
            current = current.next;
            okToRemove = true;
            return nextItem;
        }

        public void remove()
        {
            throw new UnsupportedOperationException();
        }

    }

    /////////////////////////////////////////////////////////////////////
    //
    //    IDENTIFICATION DIVISION.
    //    CLASS-ID. Node.
    //    AUTHOR. Rev Taylor R Rainwater.
    //    SUB-CLASS-OF. EnhancedStack.
    //    DATE-WRITTEN. 20.2.2016.
    //    DESCRIPTION. The common Node class.
    //
    /////////////////////////////////////////////////////////////////////
    private static class Node<T>
    {   T element;
        Node<T> next;
        Node<T> small;

        public Node(T element,Node<T> next, Node<T> small)
        {
            this.small = small;
            this.element = element;
            this.next = next;
        }
    }
}

