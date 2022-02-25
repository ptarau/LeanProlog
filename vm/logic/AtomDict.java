package vm.logic;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Fast and portable hash table implementation that provides implicit symbol
 * table functionality by associating a unique integer to each key. This
 * provides chronological order when only additions are made to the table.
 */

public class AtomDict implements Stateful {
  private static final long serialVersionUID=222L;
  
  ObjectStack syms;
  
  private ObjectMap ints;
  
  public AtomDict(){
    this.ints=new ObjectMap();
    this.syms=new ObjectStack();
    syms.push(null);
    // this makes syms size()=1+ints.size()
  }
  
  synchronized public Iterator getSymList(int start,int end) {
    ArrayList atoms=new ArrayList(syms.size());
    if(0==end)
      end=syms.size();
    
    for(int i=start;i<end;i++) {
      Sym s=(Sym)syms.at(i);
      if(null==s) {
        if(i>0&&Interact.verbosity>4)
          Interact.warnmes("getSymList: null sym found, i="+i);
        continue;
      }
      atoms.add(s.key);
    }
    // RECLIMIT => ~1000
    return atoms.iterator();
  }
  
  // zzz
  void freshCopyFrom(AtomDict other) {
    this.ints=new ObjectMap(other.ints);
    /*
    int l=other.syms.size();
    this.syms=new ObjectStack(l);
    this.syms.setTop(other.syms.getTop());
    for(int i=0;i<l;i++) {
      Sym s=(Sym)other.syms.at(i);
      this.syms.set(i,new Sym(s));
    }
    */
    this.syms=new ObjectStack(other.syms);
    // no need for this - attributes shareable
    /*
    // Prolog.dump(">>>>>>>>>HERE");
    int l=this.syms.size();
    int nullctr=0;
    for(int i=1;i<l;i++) {
      Sym s=(Sym)this.syms.at(i);
      // if s is null ugly error here ...
      if(null==s) {
        nullctr++;
      } else
        this.syms.set(i,new Sym(s));
    }
    if(nullctr>0)
      Interact.log(nullctr+" null symbol(s) in fresh copy");
    // Prolog.dump(">>>>>>>>> ="+other.ints.hashCode()+"<>"+ints.hashCode());
    */
  }
  
  public void cloneFrom(AtomDict other) {
    // Prolog.dump(">>>>>>>>> BEFORE cloneFrom="+this.info());
    this.ints=other.ints;
    this.syms=other.syms;
    // Prolog.dump("<<<<<<<<<< AFTER cloneFrom="+this.info());
  }
  
  public void clearAll() {
    this.ints.clear();
    this.syms.clear();
    this.syms.push(null);
  }
  
  public Object i2o(int i) {
    
    int l=syms.size();
    if(i<1||i>=l) {
      if(AtomTable.SYMTRACE) {
        ObjectStack s=new ObjectStack();
        for(int k=Math.max(0,l-10);k<l;k++) {
          Object o=syms.at(k);
          s.push(k+":"+o);
        }
      }
      Interact.println("### i2o: bad index="+i+",should be in 1.."+(l-1)
          +",total ints="+ints.size()+"\ntop syms:\n");
      // return null;
      return "$BADSYM_"+i;
    }
    
    Sym sym=(Sym)syms.at(i);
    if(null==sym)
      return null;
    return sym.key;
  }
  
  public int o2i(Object key) { // this is the actual key, not the sym !!!
  
    // Prolog.dump("key="+key+":"+key.getClass());
    if(null==key) {
      Interact.errmes("o2i: null key");
      return 0;
    }
    Object I=ints.get(key); // this is the sym->int map
    if(null==I)
      return 0;
    return ((Integer)I).intValue();
  }
  
  public Sym o2s(Object key) {
    return (Sym)syms.at(o2i(key));
  }
  
  final static int MAXSYMS=(1<<Defs.SYMBITS)-1;
  
  private final int newSym(Object key,Object value) {
    int ordinal=syms.size();
    if(ordinal>MAXSYMS) {
      Interact.fatal_error("SYMBOL TABLE OVERFLOW AT: ["+ordinal+"/"+MAXSYMS
          +"]=>"+key);
    }
    /*
    else if(0==ordinal%manySYMS) {
      Interact.println("warning: SYMS growing very fast=>["+ordinal+"/"+MAXSYMS
          +"]=>"+key);
    }
    */
    syms.push(new Sym(key,value));
    return ordinal;
  }
  
  final int ensure_added(Object key) {
    return ensure_added(key,AtomTable.Nothing);
  }
  
  final private int ensure_added(Object key,Object value) {
    int o=o2i(key);
    Sym sym=(Sym)syms.at(o);
    if(null!=sym)
      return o;
    o=newSym(key,value);
    ints.put(key,new Integer(o));
    
    if(o!=o2i(key))
      Interact.errmes("ASSERT FAILED: "+o+"!="+o2i(key)+" for: "+key+"="+value
          +"\n"+this);
    return o;
  }
  
  public Object put(Object key,Object value) {
    Sym old=o2s(key);
    if(null==old) {
      ints.put(key,new Integer(newSym(key,value)));
      return null;
    }
    Object oldval=old.value;
    old.value=value;
    return oldval;
  }
  
  final void re_add(int i,Sym sym) {
    syms.set(i,sym);
    ints.put(sym.key,new Integer(i));
  }
  
  public Object get(Object key) {
    Sym sym=o2s(key);
    if(null==sym)
      return null;
    return sym.value;
  }
  
  public Object remove(Object key) {
    // Prolog.dump("remove called ,key="+key+" in: "+this.hashCode());
    if(null==key) {
      Interact.errmes("remove: null key");
      return null;
    }
    Object I=ints.remove(key);
    if(null==I)
      return null;
    int i=((Integer)I).intValue();
    Sym old=(Sym)syms.at(i);
    if(null!=old&&i>0) {
      if(i==syms.getTop()) {
        // Prolog.dump("remove: sym popped, key="+key+" at: "+i+","+info());
        syms.pop();
      } else {
        syms.set(i,null);
      }
    }
    return old.value;
  }
  
  public String toString() {
    return super.toString()+"\n"+syms;
  }
  
  public Iterator getKeys() {
    return ints.getKeys();
  }
  
  public int size() {
    return ints.size();
  }
  
  public String info() {
    long t=Interact.max_memory();
    long u=Interact.used_memory();
    // Prolog.dump("WEIRD:"+syms.peek()); //=no means no sym prop
    return "memK u/t="+u+"/"+t+"\nsize="+size()+",syms="+syms.size()
        +",at top= "+syms.peek();
    
  }
  /*
   * // random tester
   * 
   * int putctr=0;
   * 
   * int remctr=0;
   * 
   * // final static java.math.BigDecimal B=new //
   * java.math.BigDecimal("1234567890.0987654321"); // Object toK(int k) {return
   * B.multiply(new java.math.BigDecimal((double)k));}
   * 
   * Object toK(int k) { return "K"+k; }
   * 
   * void randomOp(boolean grow,int max) { int dice=Math.abs(R.nextInt())%max;
   * int k=0; if(putctr>0) k=Math.abs(R.nextInt())%putctr;
   * 
   * Object K=toK(k); try { switch(dice) { case 0: if(grow)
   * put(toK(putctr),"v"+putctr++); break; case 1: get(K); break; case 2:
   * if(null!=get(K)) { int i=o2i(K); i2o(i); } break; default:
   * 
   * remove(K);
   * 
   * if(R.nextBoolean()) put(K,"u"+k); else remctr++; }
   * 
   * } catch(Exception e) { e.printStackTrace(); // Interact.dump(this);
   * 
   * } }
   * 
   * public void rantest() { Interact.println("BEGIN TESTING");
   * 
   * 
   * // correctness: write a set of assertions
   * 
   * // object in the table do not change their ordinal until removed
   * 
   * // the last value put in the dict is the one that the next get will
   * retrieve
   * 
   * // ordinal stays the same after resizing
   * 
   * // do random operations test random assertions
   * 
   * int maxTest=200000;
   * 
   * for(int i=0;i<maxTest;i++) { randomOp(true,100); }
   * 
   * for(int i=0;i<maxTest;i++) { randomOp(R.nextBoolean(),200); }
   * 
   * for(int i=0;i<maxTest;i++) { randomOp(true,100); }
   * 
   * for(int i=0;i<maxTest;i++) { randomOp(R.nextBoolean(),8000); }
   * 
   * // Interact.println("AFTER: "+this);
   * Interact.println("AFTER: size="+size());
   * Interact.println("END   TESTING: "+size()); }
   * 
   * final static java.util.Random R=new java.util.Random(13);
   */
}
