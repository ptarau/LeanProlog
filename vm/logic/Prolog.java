package vm.logic;

/**
 * Implements a Prolog code space associated to byte code file loaded in a
 * CodeStore and an AtomTable - returning unique integers for various Java
 * Objects - in particular for Java Strings naming functors and constants
 */
public final class Prolog implements Stateful {
  
  private static final long serialVersionUID=222L;
  
  // if !null extensions become available
  // could also be called via reflection - so that it detects presence of
  // extensions
  
  final static Extender extender=new vm.extensions.XBuiltins();
  
  // final static Extender extender=null;
  
  static final boolean DEBUG=false;
  
  /*
  static private Prolog defaultProlog=null;
  
  public static final Prolog getInitialProlog() {
    if(null==defaultProlog) {
      defaultProlog=newProlog(null);
    }
    return defaultProlog;
  }
  
  public static final void setInitialProlog(Prolog prolog) {
    defaultProlog=prolog;
  }
  */
  
  /**
   * de facto constructor - constructs and returns Prolog class as a result of
   * loading a bytecode file or stream -- returns null if fails. If arg is a
   * stream, also integrates default wam.bp file. The returned Prolog object can
   * be used to activate engines.
   */
  
  // load(, fload( => this is really the entry point
  
  /**
   * de facto constructor for Prolog objects from files streams or by cloning.
   */
  
  public static final int IDENTITY=0;
  
  public static final int CLONE=1;
  
  public static final int DEFAULT=2;
  
  public static final int BPFILE=3;
  
  public static final int STREAM=4;
  
  /**
   * creates new Prolog containg code area and symbol table
   * to be shared among a set of engines
   */
  // relevant for bg( and i_server(
  synchronized public static final Prolog newProlog(int op,
      Object wamFileOrStreamOrProlog) {
    // Prolog.dump("op="+op+":"+wamFileOrStreamOrProlog);
    switch(op) { // 0
      case IDENTITY: {
        Prolog P=(Prolog)wamFileOrStreamOrProlog;
        P.atomTable.logicThreadCount++;
        return P;
      }
      case CLONE: { // 1
        // Prolog.dump("op="+op+":"+wamFileOrStreamOrProlog);
        Prolog P=(Prolog)wamFileOrStreamOrProlog;
        return new Prolog(P);
      }
      case DEFAULT: // 2
        try {
          return new Prolog(noProlog); // uses default lwam.bp
        } catch(Exception e) {
          Interact.errmes("Unable to create DEFAULT Prolog",e);
          return null;
        }
      case BPFILE: // 3
      case STREAM: { // 4
        try {
          return new Prolog(wamFileOrStreamOrProlog);
        } catch(PrologException e) {
          Interact.errmes(
              "failing to construct Prolog instance from bytecode source: "
                  +wamFileOrStreamOrProlog,e);
          return null;
        }
      }
      default:
        Interact.errmes("bad opcode in newProlog: "+op+","
            +wamFileOrStreamOrProlog);
        return null;
    }
  }
  
  public final int RUNSTART;
  
  public final static Object noProlog=new Object();
  
  /**
   * Deep Clone of other Prolog
   */
  private Prolog(Prolog other){
    // Interact.println("new Prolog from=>"+other);
    
    // engineTable=new ObjectMap();
    
    atomTable=other.atomTable.freshCopy(this);
    
    atomTable.frozen=true;
    
    dict=new Dict(); // should be after init_constants
    
    codeStore=other.codeStore; // uses atomTable, constants, dict
    
    // RUNTIME
    RUNSTART=other.RUNSTART;
    
    atomTable.frozen=false;
    
    startTime=System.currentTimeMillis();
  }
  
  /**
   * Creates runtime system and loads byte code file pr stream given as
   * argument. A null is interpreted as a request to use defaults.
   * 
   */
  private Prolog(Object wamFileOrStream) throws PrologException{
    // Prolog.dump("Prolog constructor:"+wamFileOrStream);
    if(null==wamFileOrStream||noProlog==wamFileOrStream)
      wamFileOrStream=Interact.PROLOG_BYTECODE_FILE;
    
    // engineTable=new ObjectMap();
    
    atomTable=new AtomTable(this); // needed for code, dict etc.
    atomTable.frozen=true;
    
    atomTable.init_constants(); // should be after new AtomTable
    
    dict=new Dict(); // should be after init_constants
    
    codeStore=new CodeStore(atomTable,dict); // uses atomTable, constants, dict
    
    load(wamFileOrStream);
    
    // RUNTIME
    RUNSTART=atomTable.size()+1; // this is final !!!
    
    atomTable.frozen=false;
    
    startTime=System.currentTimeMillis();
  }
  
  final Dict dict;
  
  public final AtomTable atomTable;
  
  // private final ObjectMap engineTable;
  
  final CodeStore codeStore;
  
  public static final ObjectMap gvarTable=new ObjectMap();
  
  public static final ObjectMap lvarTable=new ObjectMap();
  
  synchronized public static final Object put(Object K,Object V) {
    return gvarTable.put(K,V);
  }
  
  synchronized public static final Object get(Object K) {
    return gvarTable.get(K);
  }
  
  synchronized public static final Object remove(Object K) {
    return gvarTable.remove(K);
  }
  
  private static final ObjectMap getDict(Object Id) {
    ObjectMap M=(ObjectMap)lvarTable.get(Id);
    if(null==M)
      M=new ObjectMap();
    lvarTable.put(Id,M);
    return M;
  }
  
  synchronized public static final Object lput(Object Id,Object K,Object V) {
    return getDict(Id).put(K,V);
  }
  
  synchronized public static final Object lget(Object Id,Object K) {
    return getDict(Id).get(K);
  }
  
  synchronized public static final Object lremove(Object Id,Object K) {
    ObjectMap M=getDict(Id);
    Object old=getDict(Id).remove(K);
    if(M.isEmpty())
      lvarTable.remove(Id);
    return old;
  }
  
  final long startTime;
  
  /**
   * loads from file name/url String or BufferedReader
   */
  synchronized public final boolean load(Object fNameOrStream)
      throws PrologException {
    boolean ok=false;
    try {
      ok=codeStore.load(fNameOrStream);
    } catch(Exception e) {
      throw new PrologException("Error in loading: "+fNameOrStream+"=>"+e);
    }
    
    return ok;
  }
  
  public static final void dump(Object s) {
    Interact.println("!!!"+s);
  }
  
  public String toString() {
    return "prolog_"+hashCode();
  }
}
