package vm.logic;

import java.math.BigDecimal;

/**
 * term constructor and deconstructor
 */
public class TermConverter implements ITerm,OTerm,Stateful {
  
  private static final long serialVersionUID=222L;
  
  final HeapStack heap;
  
  final AtomTable atomTable;
  
  private final ObjectMap varTable,occTable;
  
  public TermConverter(HeapStack heap){
    this.heap=heap;
    this.atomTable=heap.atomTable;
    this.varTable=new ObjectMap();
    this.occTable=new ObjectMap();
  }
  
  public void clear() {
    varTable.clear();
    occTable.clear();
  }
  
  public String getVarName(int xval) {
    String s="_"+xval;
    if(null==occTable)
      return s;
    Object O=occTable.get(new Integer(xval));
    if(null==O||!(O instanceof String))
      return s;
    String name=(String)O;
    
    return name;
  }
  
  // ITerm interface implementation:
  
  /**
   * part of ITerm interface: creates a Var
   */
  public long internalizeVar(Object name) {
    Object VX=varTable.get(name);
    Long V;
    if(null!=VX) {
      V=(Long)VX;
      occTable.remove(name);
      return V.intValue();
    }
    long v=heap.newVar();
    V=new Long(v);
    if(hasName(name)) {
      varTable.put(name,V);
      occTable.put(name,name);
      occTable.put(V,name);
    }
    return v;
  }
  
  boolean hasName(Object O) {
    return !O.equals("_");
  }
  
  /**
   * Part of ITerm interface: creates a constant. Internalizes the string,
   * assuming a different Prolog as a client. A slightly faster put_fun(Object
   * name, int arity) + put_args(int[] args) could optimize the case when
   * symbols are shared.
   */
  public long internalizeConst(Object O) throws PrologException {
    return putFun(O,0);
  }
  
  /**
   * part of ITerm interface: creates an integer
   */
  public long internalizeInt(int i) {
    return heap.pushTerm(Defs.INPUT_INT(i));
  }
  
  /**
   * Builds a list of chars representation of a string. Main purpose: avoid
   * internalizing "transient" constants.
   */
  public final long internalizeString(String s) throws PrologException {
    return heap.atom2codes(s);
  }
  
  /**
   * Part of ITerm interface: creates a compound term It assumes args are
   * already built and collected in the args array in this implementation we
   * push refs to them after building the functor. As a result, this interface
   * accomodates the usual bottom-up term building of the WAM and can be used to
   * attach other Prologs with different term representation as well as to
   * devices like term readers.
   */
  public long internalizeFun(Object f,long[] args) throws PrologException {
    long fun=putFun(f,args.length);
    if(args.length>=Defs.SYMBITS)
      throw new PrologException("arity too high in internalizeFun: "+f+"/"
          +args.length);
    putArgs(args);
    return fun;
  }
  
  // the next 2 operations are not part of the interface - still using them
  // makes
  // code more uniform and portable
  
  final public long putFun(Object f,int arity) throws PrologException {
    // $string G_STRING gets here added to atomTable
    
    long fun=atomTable.newFunctor(f,arity);
    
    return heap.pushTerm(fun);
  }
  
  final public void putArgs(long[] args) {
    // can be made faster with arrayCopy if needed - to be provided as an
    // operation by heap
    for(int i=0;i<args.length;i++) {
      heap.pushTerm(args[i]);
    }
  }
  
  static public Object bigList() {
    ObjectStack s=new ObjectStack();
    for(int i=0;i<50000;i++) {
      s.push(new Integer(i));
    }
    return s.toList();
  }
  
  final public long internalizeList(Fun F) throws PrologException {
    ObjectStack xs=new ObjectStack();
    Object last=null;
    for(;;) {
      xs.push(F.args[0]);
      Object tl=F.args[1];
      if(!(tl instanceof Fun)) {
        last=tl;
        break;
      }
      F=(Fun)tl;
      if(!(2==F.args.length&&".".equals(F.name))) {
        last=tl;
        break;
      }
    }
    if(null==last)
      throw new PrologException("error in internalizing list");
    long list=putObjectInternal(last);
    while(!xs.isEmpty()) {
      Object X=xs.pop();
      long x=putObjectInternal(X);
      list=cons(x,list);
      
    }
    return list;
  }
  
  final private long cons(long x,long xs) throws PrologException {
    long list=putFun(".",2);
    heap.pushTerm(x);
    heap.pushTerm(xs);
    return list;
  }
  
  /**
   * Internalizes an external object - Fun, VAr, String etc.
   */
  public long toInternal(Object O) throws PrologException {
    clear();
    return putObjectInternal(O);
  }
  
  // should depend on RECURSION_DEPTH
  final private long putObjectInternal(Object O) throws PrologException {
    // Interact.println("result="+O);
    long res;
    if(null==O)
      res=heap.atomTable.G_null;
    else if(O instanceof Integer)
      res=internalizeInt(((Integer)O).intValue());
    else if(O instanceof Var) {
      res=internalizeVar(O);
    } else if(O instanceof Fun) {
      Fun F=(Fun)O;
      int l=F.args.length;
      long[] iargs=new long[l];
      if(1==l&&"$string".equals(F.name)) { // G_STRING
        res=internalizeString((String)F.args[0]); // atom2codes
      } else if(2==l&&".".equals(F.name)) {
        res=internalizeList(F);
      } else {
        for(int i=0;i<F.args.length;i++) {
          iargs[i]=putObjectInternal(F.args[i]);
        }
        res=internalizeFun(F.name,iargs);
      }
    } else
      res=internalizeConst(O);
    return res;
  }
  
  /**
   * Returns an external term in a form specified by OTerm
   */
  
  public Object toExternal(long ref) {
    return toExternal(ref,this);
  }
  
  public Object toExternal(long ref,OTerm O) {
    // Interact.dump("EXTERNALIZING: "+ref);
    // if(ref==33)
    // trace=true;
    try {
      return externalizeTerm(ref,O);
    } catch(Exception e) {
      e.printStackTrace();
      return null;
    }
  }
  
  // boolean trace=false;
  
  // should depend on RECURSION_DEPTH
  private Object externalizeTerm(long xref,OTerm O) throws PrologException {
    // if(trace)
    // Interact.dump("externalizeTerm: "+xref);
    if(0==xref)
      convertError("externalizeTerm: variable cannot be 0");
    long xval;
    if(Defs.isVAR(xref)) {
      heap.deref(xref); // assumes this is a VAR
      xref=heap.xref;
      xval=heap.xval;
    } else
      xval=xref;
    Object R;
    if(Defs.isVAR(xval))
      R=O.externalizeVar(xval);
    else if(Defs.isINTEGER(xval))
      R=O.externalizeInt(Defs.OUTPUT_INT(xval));
    else if(Defs.isIDENTIFIER(xval)) {
      
      Object f=atomTable.getSym(xval);
      int arity=Defs.GETARITY(xval);
      
      if(0==arity) {
        if(null==f||AtomTable.S_null.equals(f))
          R=null;
        else
          R=O.externalizeConst(f);
      } else if(xval==atomTable.G_STRING&&1==arity) { // $string
        long arg=xref+1;
        heap.deref(arg);
        String S=heap.codes2atom(heap.xref,heap.xval);
        // if(trace) Interact.dump("externalizeTerm:"+S);
        if(null==S)
          S=(String)AtomTable.S_null;
        R=O.externalizeConst(S); // we externalize it here as arg. of a Fun etc.
      } else if(xval==atomTable.G_NUMBER&&1==arity) { // big integer or decimal
        // $number
        
        long arg=xref+1;
        heap.deref(arg);
        if(Defs.isINTEGER(heap.xval)) {
          R=O.externalizeInt(Defs.OUTPUT_INT(heap.xval));
        } else { // $$$ use downConvert here ???
          Object X=atomTable.getSym(heap.xval);
          Number N=null;
          R=null;
          
          if(X instanceof String) {
            // Prolog.dump("<<<EXPECTED: $number/1: "+f+"/"+arity+">>>");
            String S=(String)X;
            try {
              N=new java.math.BigInteger(S);
            } catch(Exception e) {
              try {
                N=new java.math.BigDecimal(S);
              } catch(Exception ee) {
              }
            }
          }
          if(null!=N)
            try {
              R=O.externalizeConst(N);
            } catch(Exception e) {
              N=null;
            }
          if(null==N) {
            Interact.errmes("expected number in: "+heap.termToString(xref)
                +",got: <"+X+">");
            R=O.externalizeConst(X);
          }
        }
      } else if(2==arity&&atomTable.G_DOT==xval) {
        R=externalizeList(xref,O); // TODO
      }
      
      else { // RECLIMIT
        Object[] args=new Object[arity];
        for(int i=0;i<arity;i++) {
          args[i]=externalizeTerm(xref+1+i,O);
        }
        R=O.externalizeFun(f,args);
      }
    } else {
      R=null;
      convertError("bad data in putTerm:"+xref+"->"+xval);
    }
    return R;
  }
  
  final public Object externalizeList(long xref,OTerm O) throws PrologException {
    ObjectStack xs=new ObjectStack();
    Object last=null;
    for(;;) {
      Object X=externalizeTerm(xref+1,O);
      xs.push(X);
      long tl=xref+2;
      long xval;
      if(Defs.isVAR(tl)) {
        heap.deref(tl); // assumes this is a VAR
        xref=heap.xref;
        xval=heap.xval;
      } else
        xval=xref;
      
      if(xval!=atomTable.G_DOT) {
        last=externalizeTerm(xref,O);
        ;
        break;
      }
      
    }
    if(null==last)
      throw new PrologException("error in internalizing list");
    if(!AtomTable.Nil.equals(last)) {
      last=new Fun(".",xs.pop(),last);
    } else {
      last=AtomTable.Nil;
    }
    return xs.toList(last);
  }
  
  public static final Object downConvert(Object NumOb) {
    String S=NumOb.toString();
    Number N=null;
    
    try {
      N=Integer.valueOf(S);
      int i=N.intValue();
      if(Math.abs(i)>=Defs.MAXINT)
        N=null;
    } catch(Exception e) {
    }
    
    if(null==N)
      try {
        N=new java.math.BigInteger(S);
      } catch(Exception e) {
        try {
          BigDecimal D=new java.math.BigDecimal(S);
          N=D.stripTrailingZeros();
        } catch(Exception ee) {
        }
      }
    return (null!=N)?N:S;
  }
  
  private void convertError(String mes) {
    Interact.errmes(mes);
  }
  
  // OTerm operations
  
  public Object externalizeVar(long i) throws PrologException {
    return new Var(Defs.xx(i));
  }
  
  public Object externalizeConst(Object c) {
    // if(c instanceof java.math.BigInteger)
    // Prolog.dump("ext..."+c+":"+c.getClass());
    return c;
  }
  
  public Object externalizeInt(int i) {
    return new Integer(i);
  }
  
  public Object externalizeFun(Object f,Object[] args) {
    // Prolog.dump("fun$$$:"+f);
    return new Fun(f,args);
  }
  
}
