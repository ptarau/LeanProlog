package vm.logic;

public class Sym implements Stateful {
  private static final long serialVersionUID=222L;
  
  Object key;
  
  public Object value;
  
  Sym(Object key,Object value){
    this.key=key;
    this.value=value;
  }
  
  Sym(Sym other){
    this.key=other.key;
    this.value=other.value;
  }
  
  // synchronized
  void set(Object value) {
    this.value=value;
  }
  
  // synchronized
  Object get() {
    return this.value;
  }
  
  public String toString() {
    return key+"="+value+"\n";
  }
}