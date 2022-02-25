package vm.logic;

/**
 * Implements a lightweight external representation of Prolog
 * variables - each Var wraps an int id which makes it unique.
 */
public class Var implements Stateful,Comparable {
  private static final long serialVersionUID=222L;
  
  public Var(int id){
    this.id=id;
  }
  
  final private int id;
  
  final public int getID() {
    return id;
  }
  
  public boolean equals(Object O) {
    return O instanceof Var&&((Var)O).getID()==id;
  }
  
  public int hashCode() {
    return id;
  }
  
  public String toString() {
    return "_j"+id;
  }
  
  public int compareTo(Object O) {
    if(O instanceof Var) {
      Var V=(Var)O;
      return id-V.id;
    } else
      return -1;
  }
}
