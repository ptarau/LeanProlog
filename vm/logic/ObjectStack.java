package vm.logic;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;

/**
Generic Dynamic Stack.
 */
public class ObjectStack implements Stateful {
  private static final long serialVersionUID=222L;
  
  private Object stack[];
  
  private int top;
  
  public static final int SIZE=16; // power of 2
  
  public static final int MINSIZE=1<<15; // power of 2
  
  public ObjectStack(){
    this(SIZE);
  }
  
  public ObjectStack(int size){
    reset();
    stack=new Object[size];
  }
  
  public ObjectStack(ObjectStack other){
    this.top=other.top;
    this.stack=other.stack.clone();
  }
  
  public ObjectStack(Object[] os){
    stack=os;
    top=os.length-1;
  }
  
  final private void reset() {
    top=-1;
  }
  
  public void clear() {
    reset();
    // stack=new Object[stack.length]; // introduces bug at end of bm2
  }
  
  void destroy() {
    reset();
    // Prolog.dump("destroying ObjectStack of length: "+stack.length);
    stack=null;
  }
  
  public final boolean isEmpty() {
    return top<0;
  }
  
  public final void push(Object i) {
    ++top;
    try {
      stack[top]=i;
    } catch(Exception ignore) {
      expand();
      stack[top]=i;
    }
  }
  
  public final Object pop() {
    Object o=stack[top];
    stack[top--]=null;
    return o;
  }
  
  public final Object at(int i) {
    return stack[i];
  }
  
  public final Object set(int i,Object O) {
    return stack[i]=O;
  }
  
  public final Object peek() {
    return stack[top];
  }
  
  public final int size() {
    return top+1;
  }
  
  final int getFree() {
    return stack.length-size();
  }
  
  final int getTop() {
    return top;
  }
  
  final void setTop(int top) {
    for(int i=top+1;i<this.top;i++)
      stack[i]=null; // DO THIS - it prevents memory leak
    this.top=top;
  }
  
  void expand() {
    int l=stack.length;
    Object[] newstack=new Object[l<<1];
    if(PrologGC.trace>=2)
      Prolog.dump("ObjectStack shrinking: "+(l<<1));
    System.arraycopy(stack,0,newstack,0,l);
    stack=newstack;
  }
  
  /**
  * dynamic array operation: shrinks to 1/2 if more than than 3/4 empty
  */
  final void shrink() {
    int l=stack.length;
    if(l<=MINSIZE||top<<2>=l)
      return;
    l=1+(top<<1);
    if(top<MINSIZE)
      l=MINSIZE;
    if(PrologGC.trace>=2)
      Prolog.dump("ObjectStack shrinking: "+l);
    Object[] newstack=new Object[l];
    System.arraycopy(stack,0,newstack,0,top+1);
    stack=newstack;
  }
  
  public final Object[] toArray() {
    Object[] newstack=new Object[top+1];
    System.arraycopy(stack,0,newstack,0,top+1);
    return newstack;
  }
  
  // destructive
  public final ArrayList toArrayList() {
    ArrayList r=new ArrayList(size());
    while(!isEmpty()) {
      r.add(pop());
    }
    return r;
  }
  
  public final LinkedHashSet toSet() {
    LinkedHashSet r=new LinkedHashSet(size());
    reverse();
    while(!isEmpty()) {
      r.add(pop());
    }
    return r;
  }
  
  // destructive - empties this
  public final Iterator toIterator() {
    return toArrayList().iterator();
  }
  
  /*
  public final Vector toVector() {
    Vector r=new Vector();
    while(!isEmpty()) {
      r.add(pop());
    }
    return r;
  }
  
  // destructive - empties this
  public final Enumeration toEnumeration() {
    return toVector().elements();
  }
  */
  
  public final ObjectStack reverse() {
    int l=size();
    int h=l>>1;
    // Prolog.dump("l="+l);
    for(int i=0;i<h;i++) {
      Object temp=stack[i];
      stack[i]=stack[l-i-1];
      stack[l-i-1]=temp;
    }
    return this;
  }
  
  public final void add(Object O) {
    reverse();
    push(O);
    reverse();
  }
  
  /**
   * empties an ObjectStack to a Prolog List
   * //RECLIMIT => ~1000 if used in XBuiltin
   */
  
  public Object toList() {
    return toList(AtomTable.Nil);
  }
  
  public Object toList(Object last) {
    Object r=last;
    while(!isEmpty()) {
      r=new Fun(".",pop(),r);
    }
    return r;
  }
  
  // assumes proper list
  public static ObjectStack fromList(Object xs) {
    ObjectStack os=new ObjectStack();
    if(AtomTable.Nil.equals(xs))
      return os;
    if(!(xs instanceof Fun)) {
      Interact.errmes("bad data in fromList, class="+xs.getClass());
      return os;
    }
    Fun tl=(Fun)xs;
    while(2==tl.args.length&&".".equals(tl.name)) {
      os.push(tl.args[0]);
      Object next=tl.args[1];
      if(AtomTable.Nil.equals(next))
        break;
      
      if(!(next instanceof Fun)) {
        Interact.errmes("bad tail in fromList, class="+xs.getClass()+":"+tl);
        break;
      }
      tl=(Fun)next; // crash if not proper list
    }
    
    return os;
  }
  
  public String toString() {
    if(isEmpty())
      return "[]";
    StringBuffer b=new StringBuffer(top<<2);
    b.append("[");
    for(int i=0;i<=top;i++) {
      if(i==0)
        b.append(""+stack[i]);
      else
        b.append(","+stack[i]);
    }
    b.append("]");
    return b.toString();
  }
}
