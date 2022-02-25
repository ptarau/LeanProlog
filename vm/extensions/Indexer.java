package vm.extensions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Set;

/**
 * Multiple key Indexer
 */
public class Indexer implements java.io.Serializable {
  private static final long serialVersionUID=222L;
  
  private final MMap mmap;
  
  /**
   * Creates a new empty Indexer. The indexer respects addition order, i.e. get
   * extracts them in chronological order.
   */
  public Indexer(){
    this.mmap=init();
  }
  
  synchronized private final MMap init() {
    return MMap.create();
  }
  
  /**
   * Removes everything from this indexer.
   */
  synchronized public final void clear() {
    this.mmap.clear();
  }
  
  /**
   * Adds a value indexed by an array of keys. Holes in the array, marked by
   * "null" are skipped. This also means that nothing is added if all the keys
   * are null. Note also that, based on the underlying LinkedHashMultimap's
   * contract, adding multiple identical <keys,value> pairs has no effect.
   */
  synchronized public final boolean put(Object[] keys,Object value) {
    int nullCtr=0;
    int l=keys.length;
    for(int i=0;i<l;i++) {
      Object key=keys[i];
      if(null!=key)
        mmap.put(new Pair(key,i),value);
      else
        nullCtr++;
    }
    return l!=nullCtr;
  }
  
  /**
   * Extracts a collection of values matching all the keys. A "null" in the
   * array of keys is interpreted as matching any values. If all the keys are
   * "null", the complete set of (unduplicated) values is returned.
   */
  synchronized public final Set get(Object[] keys) {
    int nullCtr=0;
    Pair keyI=new Pair(null,0);
    int l=keys.length;
    ArrayList maps=new ArrayList();
    for(int i=0;i<l;i++) {
      Object key=keys[i];
      if(null==key) {
        nullCtr++;
        continue;
      }
      keyI.set(key,i);
      Set valsI=mmap.get(keyI);
      if(null!=valsI)
        maps.add(new Pair(valsI,valsI.size()));
    }
    if(nullCtr==l)
      return mmap.values();
    
    l=maps.size();
    if(l>1)
      Collections.sort(maps);
    
    Pair P=(Pair)maps.get(0);
    Set C=(Set)(P.key);
    Set vals=MMap.copySet(C);
    for(int i=1;i<l;i++) {
      Pair PI=(Pair)maps.get(i);
      Set valsI=(Set)PI.key;
      vals.retainAll(valsI);
      if(vals.isEmpty())
        break;
    }
    return vals;
  }
  
  synchronized public boolean remove(Object[] keys,Object value) {
    boolean removed=false;
    for(int i=0;i<keys.length;i++) {
      Object key=keys[i];
      if(null==key)
        continue;
      Pair keyI=new Pair(key,i);
      if(mmap.remove(keyI,value))
        removed=true;
    }
    return removed;
  }
  
  synchronized public final int size() {
    return mmap.size();
  }
  
  synchronized public String toString() {
    return mmap.toString();
  }
  
}
