package vm.extensions;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public final class MMap implements java.io.Serializable {
  private static final long serialVersionUID=222L;
  
  private Map map;
  
  static final Map createMap() {
    return new LinkedHashMap();
  }
  
  public static final MMap create() {
    return new MMap();
  }
  
  private static final Set createSet() {
    return new LinkedHashSet();
  }
  
  static final Set copySet(Set S) {
    return new LinkedHashSet(S);
  }
  
  private MMap(){
    this.map=createMap();
  }
  
  public final void clear() {
    map.clear();
  }
  
  public final boolean put(Object key,Object val) {
    Set vals=(Set)map.get(key);
    if(null==vals) {
      vals=createSet();
      map.put(key,vals);
    }
    return vals.add(val);
  }
  
  public final Set get(Object key) {
    Set s=(Set)map.get(key);
    if(null==s)
      s=createSet();
    return s;
  }
  
  public final Iterator valueIterator(Object key) {
    return get(key).iterator();
  }
  
  public final boolean remove(Object key,Object val) {
    Set vals=get(key);
    boolean ok=vals.remove(val);
    if(vals.isEmpty())
      map.remove(key);
    return ok;
  }
  
  public final boolean remove(Object key) {
    return null!=map.remove(key);
  }
  
  public final int size() {
    Iterator I=map.keySet().iterator();
    int s=0;
    while(I.hasNext()) {
      Object key=I.next();
      Set vals=get(key);
      s+=vals.size();
    }
    return s;
  }
  
  public final Set keySet() {
    return map.keySet();
  }
  
  public final Iterator keyIterator() {
    return keySet().iterator();
  }
  
  public final Set values() {
    Iterator I=map.keySet().iterator();
    Set s=createSet();
    while(I.hasNext()) {
      Object key=I.next();
      Set vals=get(key);
      s.addAll(vals);
    }
    return s;
  }
  
  public String toString() {
    return map.toString();
  }
}
