package vm.logic;

import java.util.Iterator;
import java.util.LinkedHashMap;

public class ObjectMap extends LinkedHashMap implements Stateful {
  private static final long serialVersionUID=222L;
  
  public ObjectMap(){
    super();
  }
  
  public ObjectMap(ObjectMap other){
    super(other);
    // super();
    // Prolog.dump(">>>>ObjectMap>>>>>"+other.hashCode()+"<>"+this.hashCode());
    // this.putAll(other);
    
  }
  
  public Iterator getKeys() {
    return this.keySet().iterator();
  }
  
  // RECLIMIT => ~1000 if used in XBuiltin
  public Object toList() {
    Iterator I=getKeys();
    ObjectStack O=new ObjectStack();
    while(I.hasNext()) {
      O.push(I.next());
    }
    return O.toList();
  }
}
