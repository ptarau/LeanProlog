package vm.extensions;

/**
 * Wraps a position in the array of keys "pos" and an object "key" together,
 * such that indexing distinguishes them as a unit.
 */
final class Pair implements Comparable,java.io.Serializable {
  private static final long serialVersionUID=222L;
  
  int pos;
  
  Object key;
  
  public Pair(Object key,int pos){
    set(key,pos);
  }
  
  public final void set(Object key,int pos) {
    this.key=key;
    this.pos=pos;
  }
  
  public final boolean equals(Object other) {
    // if(!(other instanceof Pair)) return false;
    Pair p=(Pair)other;
    return (pos==p.pos)&&(key.equals(p.key));
  }
  
  public final int hashCode() {
    return pos^key.hashCode();
  }
  
  public String toString() {
    return pos+":"+key;
  }
  
  public final int compareTo(Object O) {
    Pair P=(Pair)O;
    return this.pos-P.pos;
  }
}
