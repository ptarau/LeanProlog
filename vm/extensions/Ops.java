package vm.extensions;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import vm.logic.Fun;
import vm.logic.ObjectMap;
import vm.logic.ObjectStack;
import vm.logic.Prolog;

public class Ops extends ObjectMap {
  private static final long serialVersionUID=222L;
  
  SortedSet pris=new TreeSet();
  
  public static final Ops defaultOps=new Ops(0);
  
  /**
   * Creates a table of operators
   * @param init: if 0 adds default
   */
  
  public static final Ops newOps(int init) {
    if(0==init)
      return defaultOps;
    return new Ops(init);
  }
  
  private Ops(int init){
    if(init<2)
      addDefaultOps();
    // otherwise do nothing !!! - TODO - possibly support local ops
  }
  
  public void op(int pri,String assoc,String name) {
    ObjectMap assocs=(ObjectMap)get(name);
    if(null==assocs) {
      if(0==pri)
        return;
      assocs=new ObjectMap();
      put(name,assocs);
    }
    Integer Pri=new Integer(pri);
    pris.add(Pri);
    int l=assoc.length();
    Iterator I=assocs.keySet().iterator();
    String oldassoc=null;
    while(I.hasNext()) {
      String x=(String)I.next();
      // Prolog.dump("OP="+name+":"+assoc+"=?="+x);
      if(l==x.length()&&assoc.equals(x)) {
        oldassoc=x;
        break;
      }
    }
    if(null!=oldassoc)
      assocs.remove(oldassoc);
    // remove its friends as well xfx-xfy-yfx; fx-fy;
    // xf-yf
    if(pri>0) {
      assocs.put(assoc,Pri);
    } else { // Pri=0
      assocs.remove(assoc);
      if(assocs.isEmpty())
        remove(name);
    }
  }
  
  public boolean isOp(String name) {
    return null!=get(name); // assert: ops of pri 0 are removed
  }
  
  public int getPri(String name,String assoc) {
    ObjectMap assocs=(ObjectMap)get(name);
    if(null==assocs)
      return 0;
    Integer Pri=((Integer)assocs.get(assoc));
    if(null==Pri)
      return 0;
    return Pri.intValue();
  }
  
  public Object getPris(String name) {
    ObjectMap assocs=(ObjectMap)get(name);
    if(null==assocs)
      return null;
    ObjectStack Fs=new ObjectStack();
    java.util.Iterator I=assocs.getKeys();
    while(I.hasNext()) {
      String assoc=(String)I.next();
      int pri=getPri(name,assoc);
      if(pri>0)
        Fs.push(new Fun("op",assoc,new Integer(pri)));
    }
    return Fs.toList();
  }
  
  public Object allOps() {
    java.util.Iterator I=getKeys();
    ObjectStack Fs=new ObjectStack();
    while(I.hasNext()) {
      String S=(String)I.next();
      if(null!=getPris(S))
        Fs.push(S);
    }
    return Fs.toList();
  }
  
  public int pred(int pri) {
    Integer Pri=new Integer(pri);
    SortedSet lowerPris=pris.headSet(Pri);
    if(lowerPris.isEmpty())
      return 0;
    Pri=(Integer)lowerPris.last();
    return Pri.intValue();
  }
  
  public int succ(int pri) {
    Integer Pri=new Integer(pri+1);
    SortedSet higherPris=pris.tailSet(Pri);
    if(higherPris.isEmpty())
      return 0;
    Pri=(Integer)higherPris.first();
    return Pri.intValue();
  }
  
  public void addDefaultOps() {
    // op(40,"xfx","::");
    op(50,"xfy",":");
    op(100,"fx","?");
    op(100,"fx","`");
    op(100,"fx","#");
    op(100,"fx","?");
    op(200,"fy","+");
    op(200,"fy","-");
    op(200,"fy","\\");
    op(200,"xfx","**");
    op(200,"xfx","..");
    op(200,"xfy","^");
    op(200,"yfx","<<");
    op(200,"yfx",">>");
    op(400,"yfx","*");
    op(400,"yfx","/");
    op(400,"yfx","//");
    op(400,"yfx","mod");
    op(400,"yfx","div");
    op(400,"yfx","gcd");
    op(500,"yfx","/\\");
    op(500,"yfx","+");
    op(500,"yfx","-");
    op(500,"yfx","xor");
    op(500,"yfx","eq");
    op(500,"yfx","\\/");
    op(600,"xfx","~>");
    op(700,"xfx","<");
    op(700,"xfx","=");
    op(700,"xfx","=..");
    op(700,"xfx","=:=");
    op(700,"xfx","=<");
    op(700,"xfx","==");
    op(700,"xfx","=@=");
    op(700,"xfx","=\\=");
    op(700,"xfx",">");
    op(700,"xfx",">=");
    op(700,"xfx","@<");
    op(700,"xfx","@=<");
    op(700,"xfx","@>");
    op(700,"xfx","@>=");
    op(700,"xfx","\\=");
    op(700,"xfx","\\==");
    op(700,"xfx","\\=@=");
    op(700,"xfx","as");
    op(700,"xfx","is");
    op(700,"xfy","=>");
    op(700,"xfy","<=");
    op(700,"xfy","==>");
    op(700,"xfy","<==");
    op(800,"xfx","@");
    op(900,"fy","\\+");
    op(900,"fy","not");
    // op(990,"xfy","|");
    op(1000,"xfy",",");
    op(1050,"xfy","*->");
    op(1050,"xfy","->");
    op(1100,"xfy",";");
    op(1105,"xfy","|");
    op(1150,"fx","discontiguous");
    op(1150,"fx","dynamic");
    op(1150,"fx","initialization");
    op(1150,"fx","meta_predicate");
    op(1150,"fx","multifile");
    op(1200,"fx",":-");
    op(1200,"fx","?-");
    op(1200,"xfx",":=");
    op(1200,"xfx","-->");
    op(1200,"xfx",":-");
  }
  
  public static void xtest() {
    Ops ops=newOps(0);
    int pri=ops.getPri("=","xfx");
    int lower=ops.pred(pri);
    int higher=ops.succ(pri);
    Prolog.dump("Pris="+ops.pris);
    // Prolog.dump("Ops="+ops);
    Prolog.dump("pri="+pri+",lower="+lower+",higher="+higher);
  }
  
  public String toString() {
    return "total_operators:"+size()+":"+this.hashCode();
  }
}
