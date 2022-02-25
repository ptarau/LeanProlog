package vm.extensions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import vm.logic.Fun;
import vm.logic.Interact;
import vm.logic.Stateful;
import vm.logic.Var;

public class DynamicDB {
  private static final long serialVersionUID=222L;
  
  private static Indexer indexer=new Indexer();
  
  private static Map indexSpecs=MMap.createMap();
  
  private static MMap dynamics=MMap.create();
  
  synchronized public static void toFile(String F) {
    Object[] all=new Object[] { indexer, indexSpecs, dynamics };
    Transport.toFile(F,all);
  }
  
  synchronized public static void fromFile(String F) {
    Object[] all=(Object[])Transport.fromFile(F);
    indexer=(Indexer)all[0];
    indexSpecs=(Map)all[1];
    dynamics=(MMap)all[2];
  }
  
  synchronized public static final Iterator allDynamics() {
    Iterator keys=dynamics.keySet().iterator();
    ArrayList a=new ArrayList();
    while(keys.hasNext()) {
      Pair fn=(Pair)keys.next();
      Iterator dbs=dynamics.get(fn).iterator();
      while(dbs.hasNext()) {
        String db=(String)dbs.next();
        a.add(new Fun("$",db,fn.key,new Integer(fn.pos)));
      }
    }
    return a.iterator();
  }
  
  public static final int isDynamic(String Db,String name) {
    Pair key=new Pair(name,0);
    return (dynamics.get(key).contains(Db))?1:0;
  }
  
  public static final int isDynamic(String Db,Fun H) {
    Pair key=new Pair(H.name,H.args.length);
    return (dynamics.get(key).contains(Db))?1:0;
  }
  
  public static final void setDynamic(String Db,String name) {
    Pair key=new Pair(name,0);
    dynamics.put(key,Db);
  }
  
  public static final void setDynamic(String Db,Fun H) {
    Pair key=new Pair(H.name,H.args.length);
    dynamics.put(key,Db);
  }
  
  public static final void removeDynamic(String Db,String name) {
    Pair key=new Pair(name,0);
    dynamics.remove(key,Db);
  }
  
  public static final void removeDynamic(String Db,Fun H) {
    Pair key=new Pair(H.name,H.args.length);
    dynamics.remove(key,Db);
  }
  
  public static final Indexer getIndexer() {
    return indexer;
  }
  
  public static final void index(Fun spec) {
    int l=spec.args.length;
    boolean[] args=new boolean[l+2];
    args[0]=args[1]=true;
    for(int i=0;i<l;i++) {
      args[i+2]=0!=((Integer)spec.args[i]).intValue();
    }
    Pair key=new Pair(spec.name,l);
    indexSpecs.put(key,args);
  }
  
  public static final int is_indexed(Fun T) {
    Object F=T.name;
    int l=T.args.length;
    Pair key=new Pair(F,l);
    return (null==indexSpecs.get(key))?0:1;
  }
  
  // prepend Db + Term
  private final static Object[] prepend(Object Db,Fun T) {
    int l=T.args.length;
    Object[] args=new Object[l+2];
    args[0]=Db;
    args[1]=T.name;
    System.arraycopy(T.args,0,args,2,l);
    return args;
  }
  
  private static final Object[] prepend(Object Db,Var V) {
    return new Object[] { Db, V };
  }
  
  private static final Object[] prepend(Object Db,String V) {
    return new Object[] { Db, V };
  }
  
  public static final Object[] applyIndex(String Db,Fun T) {
    return applyIndex0(prepend(Db,T));
  }
  
  // apply index after prepending db
  public static final Object[] applyIndex(String Db,String S) {
    return applyIndex0(prepend(Db,S));
  }
  
  public static final Object[] applyIndex(String Db,Var V) { // all in a Db
    return applyIndex0(prepend(Db,V));
  }
  
  public static final Object[] applyIndex(Var Db,Fun T) {
    return applyIndex0(prepend(Db,T));
  }
  
  // apply index after prepending db
  public static final Object[] applyIndex(Var Db,String S) {
    return applyIndex0(prepend(Db,S));
  }
  
  public static final Object[] applyIndex(Var Db,Var V) {
    return applyIndex0(prepend(Db,V));
  }
  
  private static final boolean[] default_specs=new boolean[] { true, true };
  
  private static final Object[] applyIndex0(Object[] args) {
    int arity=args.length-2;
    Object F=args[1];
    Pair FN=new Pair(F,arity);
    boolean[] specs=(boolean[])indexSpecs.get(FN);
    if(null==specs)
      specs=default_specs;
    ArrayList a=new ArrayList();
    int lim=args.length;
    if(specs.length<lim)
      lim=specs.length;
    for(int i=0;i<lim;i++) {
      if(!specs[i])
        continue;
      a.add(args[i]);
    }
    return a.toArray();
  }
  
  private static final Object[] fun2keys(Object[] args,boolean nullsOk) {
    // Prolog.dump(new Fun("fun2keys,args));
    int l=args.length;
    Object[] keys=new Object[l];
    for(int i=0;i<l;i++) {
      Object arg=args[i];
      Object key;
      if(arg instanceof Fun) {
        Fun f=(Fun)arg;
        key=f.name;
      } else if(arg instanceof Var) {
        if(nullsOk)
          key=null; // this should be used in get
        else
          return null; // this should be used in put
      } else {
        key=arg;
      }
      keys[i]=key;
    }
    return keys;
  }
  
  // UPDATES
  
  /**
   * adds a Prolog clause, indexed by its head
   */
  // rli?
  public static final int put(Object[] head,Object[] clause,Integer atEnd) {
    Object[] keys=fun2keys(head,false);
    if(null==keys)
      return 0;
    // boolean ok=indexer.put(keys,Rev.wrap(clause));
    boolean ok;
    if(atEnd.intValue()>0)
      ok=indexer.put(keys,clause);
    else
      ok=indexer.put(keys,Rev.wrap(clause));
    return ok?1:0;
  }
  
  /**
   * Removes a specific Prolog clause. Its head is also given to help removing
   * all related indexing information. It is assumed that "get" is first used to
   * extract all the relavant handles, among which, unification is used to
   * select the one to be removed - as in Prolog's retract(_).
   */
  // rli?
  final public static int remove(Object[] head,Object[] clause) {
    // Prolog.dump("remove="+head);
    Object[] keys=fun2keys(head,true);
    boolean ok=indexer.remove(keys,clause);
    if(!ok)
      ok=indexer.remove(keys,Rev.wrap(clause));
    return (ok)?1:0;
  }
  
  final public static void clear() {
    indexer.clear();
    indexSpecs.clear();
    dynamics.clear();
  }
  
  // QUERIES
  
  /**
   * Gets an array Prolog clauses matching a given head.
   */
  
  public static final Iterator get(Object[] head) {
    // Prolog.dump(indexer);
    Object[] keys=fun2keys(head,true);
    Set matches=indexer.get(keys);
    
    // return Arrays.asList(matches.toArray()).iterator();
    // return new ArrayList(matches).iterator();
    // return matches.iterator(); // it is already made from a copySet
    
    Iterator I=matches.iterator();
    ArrayList R=new ArrayList();
    ArrayList A=new ArrayList();
    while(I.hasNext()) {
      Object x=I.next();
      if(x instanceof Rev)
        R.add(Rev.unwrap((Rev)x));
      else
        A.add(x);
    }
    if(R.isEmpty())
      return A.iterator();
    
    Collections.reverse(R);
    R.addAll(A);
    return R.iterator();
  }
  
  public static final Object next(Fun TheI) {
    Iterator I=(Iterator)TheI.args[0];
    if(!I.hasNext())
      return "no";
    return new Fun("the",I.next());
  }
  
  final public static Fun size() {
    Fun f1=new Fun("indexSpecs",new Integer(indexSpecs.size()));
    Fun f2=new Fun("dynamics",new Integer(dynamics.size()));
    Fun f3=new Fun("indexer",new Integer(indexer.size()));
    Fun f=new Fun("size",f1,f2,f3);
    // Interact.pp(f);
    return f;
  }
  
  final public static void xsave(String toFile,String db) {
    Object[] Key=applyIndex(db,new Var(0));
    Iterator I=get(Key);
    ArrayList S=new ArrayList();
    while(I.hasNext()) {
      S.add(I.next());
    }
    Object A=S.toArray();
    Transport.toFile(toFile,A);
  }
  
  final public static Iterator xload(String fromFile) {
    // Object[] Key=applyIndex(db,new Var(0));
    Object[] A=(Object[])Transport.fromFile(fromFile);
    List S=Arrays.asList(A);
    Iterator I=S.iterator();
    // while(I.hasNext()) {
    // indexer.put(Key,I.next());
    // }
    return I;
  }
  
  synchronized final public static void showAll() {
    Interact.pp("\nDYNAMICS:\n"+dynamics+"\n"+"\nINDEX_SPECS:\n"+indexSpecs
        +"\n"+"\nINDEXER:\n"+indexer+"\n");
  }
  
  synchronized public static final Fun db_size() {
    Integer S=new Integer(indexSpecs.size());
    Integer D=new Integer(dynamics.size());
    Integer I=new Integer(indexer.size());
    return new Fun("db_stats",new Fun("indexed",S),new Fun("dynamics",D),
        new Fun("index_records",I));
  }
}

/**
 * tranparently wraps up an object while indicating it
 * should be seen as being added in front of the queue
 * and therefore returned by an iterator in reverse order
 */
class Rev implements Stateful {
  private static final long serialVersionUID=222L;
  
  Object[] val;
  
  Rev(Object[] val){
    this.val=val;
  }
  
  public int hashCode() {
    return val.hashCode();
  }
  
  public boolean equals(Object other) {
    if(other instanceof Rev)
      return val.equals(((Rev)other).val);
    else
      return false;
  }
  
  public String toString() {
    return "@:"+val;
  }
  
  static final Rev wrap(Object[] os) {
    return new Rev(os);
  }
  
  static final Object[] unwrap(Rev rev) {
    return rev.val;
  }
  
}
