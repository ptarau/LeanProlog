package vm.logic;

/**
 * Implements a value lookup hash-table. The value is keyed by two independent
 * key values. The current implementation uses linear probing.
 */
final class Dict extends ObjectMap {
  
  private static final long serialVersionUID=222L;
  
  final static int EMPTY=-1;
  
  /** Access to the current prolog */
  
  Dict(){
  }
  
  Dict(Dict other){
    super(other);
  }
  
  boolean pdef(long pred,int val) {
    boolean old1=null!=get(new Long(pred));
    boolean old2=null!=get(new Integer(val));
    if(old1||old2)
      return false;
    put(new Long(pred),new Integer(val));
    put(new Integer(val),new Long(pred));
    return true;
  }
  
  final int pget(long pred) {
    Object val=get(new Long(pred));
    if(null==val)
      return EMPTY;
    return ((Integer)val).intValue();
  }
  
  final long aget(int addr) {
    Object val=get(new Integer(addr));
    if(null==val)
      return EMPTY;
    return ((Long)val).longValue();
  }
  
  final boolean hdef(long pred,long fun,int val) throws PrologException {
    Pair XY=new Pair(pred,fun);
    if(null!=get(XY))
      return false;
    Integer VAL=new Integer(val);
    put(XY,VAL);
    return true;
  }
  
  // used directly in SWITCH for 1-st arg indexing
  final int hget(long pred,long fun) throws PrologException {
    // long xy=pred;
    // xy=(xy<<32)+fun;
    Pair XY=new Pair(pred,fun);
    Integer VAL=(Integer)get(XY);
    if(null==VAL)
      return Dict.EMPTY;
    return VAL.intValue();
  }
  
  /*
   // the reason we cannot do this is because EXECUTE has been
   // melted together with its target address 
   // EXEC_JUMP_IF etc. all over the code
   // one would need to trail all those changes - ie. reset
   // the instructions to what their were (and possibly resize them?)
  final void hremove(int pred,int fun) throws PrologException {
    hset(pred,fun,atomTable.G_empty);
  }
  */
  
  final int getpred(long pred) throws PrologException {
    return pget(pred);
  }
  
  final long addr2fun(int addr) throws PrologException {
    return aget(addr);
  }
  
  final boolean setpred(long pred,int addr) throws PrologException {
    return pdef(pred,addr);
  }
  
  final boolean do_isEmpty(long val) {
    return val==Dict.EMPTY;
  }
  
} // End class Dict

final class Pair {
  final private long pred;
  
  final private long fun;
  
  Pair(long pred,long fun){
    this.pred=pred;
    this.fun=fun;
  }
  
  public boolean equals(Object that) {
    if(!(that instanceof Pair))
      return false;
    Pair other=(Pair)that;
    return this.pred==other.pred&&this.fun==other.fun;
  }
  
  public int hashCode() {
    return (int)(pred+fun);
  }
}

/*





*/