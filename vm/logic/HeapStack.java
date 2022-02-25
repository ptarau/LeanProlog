package vm.logic;

/**
 * The Prolog Heap. Terms are represented as single integers in the following
 * format
 * 
 * integer: INTTAG, val
 * 
 * atom: FUNTAG ,symbol number, 0
 * 
 * functor: FUNTAG symbol number, arity variable VARTAG==0 heap index
 * 
 * Note that VARTAG is zero so that VAR terms are just heap indexes.
 * 
 * Design note: This represents a reversal in the order of the sub-fields from
 * the C version. For efficient operation in Java, the VAR type must represent a
 * simple heap index. Having the TAG bits at the high order end of the term
 * achieves this.
 */
public class HeapStack extends Defs {
  private static final long serialVersionUID=222L;
  
  static final int MINSIZE=1<<16; // no shrinking to smaller size
  
  /** The heap storage. */
  private long[] cells;
  
  /**
   * The first used element in the heap. heapBase should be set to zero for
   * compatibility with the C version of this program. Otherwise instruction
   * traces will not correspond. Normally BASE should be set to 1 so that
   * element zero is never used. This means that it should be possible to detect
   * zero stores.
   */
  /**
   * The current top of the heap. Unlike the C version, this represents the last
   * used element
   */
  private int heapTop;
  
  private int heapBase;
  
  /** ref/val pair used for dereferencing terms. */
  long xref;
  
  long xval;
  
  /** Access to the current Prolog Shared data. */
  final AtomTable atomTable;
  
  /**
   * Create a new heap.
   */
  
  HeapStack(Prolog prolog,int heapsize){
    // Interact.println("new heap="+heapsize);
    this.atomTable=prolog.atomTable;
    heapBase=0; // Element 0 is never used (except in error), as ++ is performed
    // first in push
    heapTop=heapBase;
    cells=new long[heapsize];
  }
  
  public static long mmTIME=0;
  
  private long oneMMtime=0;
  
  final public void startMMtimer() {
    oneMMtime=System.currentTimeMillis();
  }
  
  final public void endMMtimer() {
    oneMMtime=System.currentTimeMillis()-oneMMtime;
    mmTIME+=oneMMtime;
  }
  
  /**
   * dynamic array operation: heap expander
   */
  void expand() {
    startMMtimer();
    int l=cells.length;
    long[] newstack=new long[l<<1];
    if(PrologGC.trace>=2)
      Prolog.dump("heap expanding: "+(l<<1));
    System.arraycopy(cells,0,newstack,0,l);
    cells=newstack;
    endMMtimer();
  }
  
  /**
   * dynamic array operation: heap reducer timer called elsewhere?
   */
  void shrink() {
    int l=cells.length;
    if(l<=MINSIZE||heapTop<<2>=l)
      return;
    
    // l=1+(heapTop<<1); // this still means shrink to 1/2 of the heap or less
    l=l>>2;
    if(l<MINSIZE)
      l=MINSIZE;
    if(PrologGC.trace>=2)
      Prolog.dump("heap shrinking: "+l);
    long[] newstack=new long[l];
    System.arraycopy(cells,0,newstack,0,heapTop+1);
    cells=newstack;
  }
  
  /**
   * Push a term value onto the heap. First it increments and then it assigns -
   * this means that heapTop points to the last assigned cell and that the next
   * available cell will be at heapTop+1.
   * 
   * @param term
   *          The term to be added.
   * @return The heap index of the added term.
   */
  public final long pushTerm(long xval2) {
    // Interact.println("cells:"+cells);
    if(heapTop+1>=cells.length)
      expand();
    cells[++heapTop]=xval2;
    
    /*
    ++heapTop;
    try {
      cells[heapTop]=t;
    } catch(ArrayIndexOutOfBoundsException ignore) {
      expand();
      cells[heapTop]=t;
    }
    */
    
    return heapTop;
  }
  
  final long newVar() {
    return pushTerm(heapTop+1);
  }
  
  /**
   * Set the heap top the indicated value. This should only be called from
   * ChoicePointStack during a backtrack operation.
   */
  final void setHeapTop(int newTop) {
    heapTop=newTop;
  }
  
  /** Return the current top of the heap. */
  final int getHeapTop() {
    return heapTop;
  }
  
  /** Utility function to push a []/0 term. */
  final void pushNil() {
    pushTerm(atomTable.G_NIL);
  }
  
  /**
   * Utility function to push a ./0 term and a specified term.
   * 
   * @param term
   *          The term to be added.
   */
  final void pushList(long term) {
    pushTerm(heapTop+2); // prevents bug in unify and maybe elsewhere $$ ??
    pushTerm(atomTable.G_DOT);
    pushTerm(term);
  }
  
  /**
   * Pushes sub array of cells to heap
   */
  final void pushCells(long source[],int from,int arity) {
    if(arity+heapTop>=cells.length-1)
      expand();
    System.arraycopy(source,from,cells,heapTop+1,arity);
    heapTop+=arity;
  }
  
  /**
   * Set a heap element to the given term.
   * 
   * @param hRef
   *          The heap index to be set.
   * @param cval
   *          The term to replace the heap element at <code>hRef</code> heap
   *          index.
   */
  final void setRef(long hRef,long cval) {
    cells[xx(hRef)]=cval;
  }
  
  /**
   * Return the term at a given heap index.
   * 
   * @param hRef
   *          The heap index.
   * @return The indicated heap term.
   */
  public final long getRef(long hRef) {
    return cells[xx(hRef)];
  }
  
  /** Completely clear the heap. */
  public void clear() {
    heapTop=heapBase; // this points to invalid data: a var can never be 0;
  }
  
  void destroy() {
    clear();
    // Interact.println("!!!!!!!!!!cells cleared");
    cells=null;
  }
  
  /** Return the count of entries. */
  public final int getUsed() {
    return heapTop+1;
  }
  
  /** Return available heap entries. */
  public final int getFree() {
    return cells.length-heapTop-1;
  }
  
  final void FDEREF() {
    if(isNONVAR(xref))
      xval=xref;
    else {
      deref();
    }
  }
  
  final void deref() {
    // ASSERT isVAR(xref)
    while(isVAR(xval=cells[xx(xref)])&&xref!=xval) { // variable and not
                                                     // unbound.
      xref=xval;
    }
    // ASSERT isVAR(xref) and xval contains either a non-var or an unbound var
    // in any case, cell[xref] contains the same thing as xval
  }
  
  // refer to results as heap.xref and heap.xval !!!
  public final void deref(long xref) {
    this.xref=xref;
    this.xval=xref;
    // ASSERT isVAR(xref)
    deref();
  }
  
  long copyTermFrom(HeapStack M,long t) {
    EncodedTerm ET=M.encodedCopy(t);
    t=decodedCopy(ET);
    ET.destroy();
    return t;
  }
  
  EncodedTerm encodedCopy(long t) {
    int h=heapTop;
    // Prolog.dump(heapTop+":orig to encode:"+termToString(t));
    long ct=copyTerm(t);
    // Prolog.dump(heapTop+":copy to encode:"+termToString(ct));
    EncodedTerm T;
    if(isATOMIC(ct))
      T=new EncodedTerm(ct);
    else
      T=new EncodedTerm(cells,xx(ct),heapTop+1);
    // Prolog.dump("encodedTerm:"+T+"size:"+T.size()+"="+(heapTop-h));
    
    heapTop=h;
    return T;
  }
  
  int decodedCopy(EncodedTerm T) {
    // if(T.size()==2) return T.getRef(1);
    while(getFree()<=T.size())
      expand();
    // expand
    int ct=heapTop+1;
    heapTop=T.decodeTerm(ct,cells);
    return ct;
  }
  
  private ObjectMap varTable;
  
  final long copyTerm(long t) {
    if(getFree()<=getUsed())
      expand();
    int currentTop=heapTop;
    varTable=new ObjectMap();
    
    long ct=t;
    
    try {
      ct=xcp(t);
    } catch(PrologException e) {
      heapTop=currentTop;
      Interact.warnmes("copy_term warning: recursion overflow, returned: "
          +AtomTable.S_null);
      ct=atomTable.G_null;
    }
    varTable=null;
    
    return ct;
  }
  
  private final long xcp(long xref) throws PrologException {
    LongStack copyStack=new LongStack();
    
    long handle=newVar();
    
    copyStack.push(xref); // term to be copied
    copyStack.push(handle); // where to put the copy
    
    while(!copyStack.isEmpty()) {
      
      long ct=copyStack.pop(); // where to put the copy
      xref=copyStack.pop(); // term to be copied
      
      // dereference
      long xval;
      
      if(isVAR(xref)) {
        deref(xref);
        xref=this.xref;
        xval=this.xval;
      } else
        xval=xref;
      
      // handle simple terms
      
      long cval=copySimple(xval);
      
      if(cval!=0) {
        setRef(ct,cval);
      } else {
        // handle compound terms
        int arity=GETARITY(xval);
        long h=pushTerm(xval);
        setRef(ct,h);
        /*
         * for (int i=1; i<arity+1; i++) { h=newVar(); copyStack.push(xref+i);
         * // term to be copied copyStack.push(h); // where to put the copy }
         */
        for(int i=arity;i>0;i--) {
          // newVar(); // just because it possibly expands heap
          copyStack.push(xref+i); // term to be copied
          copyStack.push(h+i); // where to put the copy
        }
        heapTop+=arity;
        if(heapTop+1>=cells.length)
          expand();
      }
    }
    return handle;
  }
  
  final long copySimple(long xval) {
    
    // handle variables
    if(isVAR(xval)) {
      Long Key=new Long(xval);
      Object XVal=varTable.get(Key);
      Long Val;
      if(null==XVal) {
        long newvar=newVar();
        Val=new Long(newvar);
        varTable.put(Key,Val);
      } else
        Val=(Long)XVal;
      return Val.longValue();
    }
    
    // handle integers
    if(isINTEGER(xval))
      return xval;
    
    // handle atoms
    int arity=GETARITY(xval);
    if(arity==0)
      return xval;
    
    return 0;
  }
  
  final int termHash(long t) {
    
    int h=-3;
    
    try {
      h=xh(t);
    } catch(Exception e) {
      Interact.warnmes("term_hash: resource overflow on very large term");
    }
    
    // Interact.println("hcode="+h);
    return h;
  }
  
  private final int xh(long xref) throws PrologException {
    LongStack copyStack=new LongStack();
    copyStack.push(xref); // term to be hashed
    int hcode=0;
    
    while(!copyStack.isEmpty()) {
      
      xref=copyStack.pop();
      
      // dereference
      long xval;
      
      if(isVAR(xref)) {
        deref(xref);
        xref=this.xref;
        xval=this.xval;
      } else
        xval=xref;
      
      // Prolog.dump("hash_term: "+dumpCell(xval));
      
      if(Defs.isVAR(xval)) {
        return -1;
      }
      
      int k=0;
      
      if(Defs.isINTEGER(xval)) {
        k=Defs.OUTPUT_INT(xval);
      } else if(Defs.isIDENTIFIER(xval)) {
        int arity=GETARITY(xval);
        Object s=atomTable.getSym(xval);
        k=s.hashCode()+3*arity;
        for(int i=arity;i>0;i--) {
          copyStack.push(xref+i); // term to be hashed
        }
      } else {
        Interact.warnmes("hash_term BAD TAG: "+dumpCell(xval));
        return -2;
      }
      
      k=Math.abs(k);
      k=k&((1<<29)-1);
      
      hcode=(hcode<<4)+k;
      
    }
    
    hcode=hcode&((1<<29)-1);
    hcode=Math.abs(hcode);
    
    return hcode;
  }
  
  public static final int checkDepth(int depth) throws PrologException {
    if(depth>Interact.RECURSION_DEPTH)
      throw new PrologException(
          "recursion depth limit reached in I/O opertion, max="
              +Interact.RECURSION_DEPTH);
    else
      return depth+1;
  }
  
  final String codes2atom(long l,long vl) {
    // get heap address of start of list.
    StringBuffer sbuf=new StringBuffer();
    while(vl==atomTable.G_DOT) {
      deref(++l);
      long carVal=xval;
      if(isINTEGER(carVal))
        sbuf=sbuf.append((char)OUTPUT_INT(carVal));
      else {
        return null;
      }
      deref(++l);
      l=xref;
      vl=xval;
    }
    if(vl!=atomTable.G_NIL) {
      return null;
    }
    return sbuf.toString();
  }
  
  final int atom2codes(String sbuf) {
    int r=getHeapTop()+1;
    for(int i=0;i<sbuf.length();i++) {
      pushList(INPUT_INT(sbuf.charAt(i)));
    }
    pushNil();
    return r;
  }
  
  /**
   * returns the string image of a cell
   */
  private String dumpCell0(long x) {
    String sbuf;
    if(Defs.isIDENTIFIER(x))
      sbuf=atomTable.getSym(x)+"/"+GETARITY(x);
    else
      sbuf=Defs.showCell(x);
    return sbuf;
  }
  
  String dumpCell(long x) {
    try {
      return dumpCell0(x);
    } catch(Exception e) {
      return "bad_cell#"+x;
    }
  }
  
  /**
   * returns a human readable representation of a heap cell at a given index as
   * a String
   */
  String cellToString(long i) {
    String sbuf;
    try {
      long x=cells[xx(i)];
      sbuf=dumpCell(x);
    } catch(Exception e) {
      sbuf="bad_cell_address#"+i;
    }
    return sbuf;
  }
  
  /*
   * simple representation of a reference as String overridden in Machine with a
   * recursive printing of a term
   */
  public String termToString(long xref) {
    return cellToString(xref);
  }
  
  public void dump() {
    int limit=20;
    dump((heapTop>limit)?(heapTop-limit):0,heapTop);
  }
  
  public void dump(int from,int to) {
    if(Prolog.DEBUG) {
      Prolog.dump("HEAP used:"+heapTop+" max:"+cells.length);
      for(int i=from;i<to&&i<=heapTop;i++) {
        if(cells[i]!=0) {
          Prolog.dump("["+i+"]="+cellToString(i));
        }
      }
      Prolog.dump("END HEAP.");
    }
  }
  
  final void addHeapKeepers(ObjectStack syms,AtomTable keepers) {
    
    for(int h=0;h<=heapTop;h++) {
      long f=cells[h];
      cells[h]=addKeeper(f,syms,keepers);
    }
    
  }
  
  /**
   * adds a root - containing a heap reference to live data
  */
  final long addKeeper(long f,ObjectStack syms,AtomTable keepers) {
    if(Defs.isIDENTIFIER(f)) {
      int o=Defs.GETSYMNO(f);
      Object O=syms.at(o);
      if(null!=O) {
        Sym S=(Sym)O;
        // if(S.ordinal!=o) Interact.errmes("bad symbol="+o+":"+S);
        // if(null==keepers.get(S.key)) {
        o=keepers.ensure_added(S.key);
        f=Defs.PUTSYMNO(f,o);
        // Prolog.dump("keepers: sym="+o+":\n"+S+"=>"+keepers.o2s(S.key)+keepers.info());
        // }
      }
    }
    return f;
  }
  
} // End class HeapStack

