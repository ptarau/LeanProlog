package vm.logic;

/**
 * Implements a symbol table as a hash-table.
 */
public final class AtomTable extends AtomDict {
  private static final long serialVersionUID=222L;
  
  final static boolean SYMTRACE=false; //
  
  final static boolean SYMTEST=false;
  
  private static boolean SYMGC=true;
  
  public boolean symgc_flag=false; // inhibits SYMGC dynamically if false
  
  public static final int set_symgc(int on_off) {
    int old=SYMGC?1:0;
    SYMGC=(1==on_off)?true:false;
    return old;
  }
  
  final static int MANYSYMS=1<<24; // was 18 for 32 bits %Paul
  
  // was 16 for 32 bit version %%Paul
  public final static int SYMGC_DELTA=1<<22; // min size increase to wait
                                             // between SYMGC
  
  // private final static int ENGINE_GC_DELTA=1<<8; // min new engines
  // triggering
  // gc
  
  // private static final int ENGINE_GC_MIN=1<<6;
  
  /* Above this symgc_flag is always
    set. Ideally at 1<<23.
  */
  private static int SYMGC_FORCE=1<<28; // 23 for 32 bit %%Paul
  
  public static void set_symgc_force(Integer I) {
    SYMGC_FORCE=I.intValue();
  }
  
  // DYNAMICS
  
  public int logicThreadCount=0;
  
  // private int ENGINES_BEFORE=0;
  
  private final int SYMGC_MIN; // init + delta
  
  private Prolog prolog;
  
  private int SYSEND=0;
  
  void setSYSEND() {
    SYSEND=size();
  }
  
  public int getSYSEND() {
    return SYSEND;
  }
  
  /**
   * Create a new functor term with the specified arity.
   */
  final long newFunctor(Object name,int arity) {
    if(arity>Defs.MAXARITY||null==name) {
      Interact.errmes(
          "Arity limit exceeded or bad name, name="+name+", arity="+arity);
    }
    int f=addObject(name);
    // Interact.println("added:"+name+",as:"+f);
    return Defs.addArity(f,arity);
  }
  
  synchronized final Object getSym(long atomTerm) {
    return i2o(Defs.GETSYMNO(atomTerm));
  }
  
  /**
   * Creates an integer term or a new functor depending on the format of
   * <code>name</code>.
   */
  final long inputTerm(String name,int arity)
      throws PrologException,PrologException {
    try {
      int i=Integer.parseInt(name); // $$$ $number ???
      return Defs.INPUT_INT(i);
    } catch(NumberFormatException e) {
      return newFunctor(name,arity);
    }
  }
  
  // constants
  
  long G_true;
  
  long G_fail;
  
  public static final Object S_null="$null";
  
  public static final Object Nil="[]";
  
  public static final Object Nothing="no";
  
  public static final Object Yes="yes";
  
  long G_null; /* representation for Java's null */
               
  long G_empty;
  
  long G_predmark;
  
  long G_addrmark;
  
  long G_undefined;
  
  long G_metacall;
  
  long G_prolog_main;
  
  long G_NIL;
  
  long G_DOT;
  
  long G_DIF;
  
  long G_STRING; // $string
  
  long G_NUMBER; // $number
  
  long G_VAR;
  
  long G_THE;
  
  long G_NO;
  
  void init_constants() throws PrologException {
    G_true=newFunctor("true",0); // should be the first
    G_fail=newFunctor("fail",0);
    G_null=newFunctor(S_null,0);
    G_empty=newFunctor("$empty",0);
    
    G_prolog_main=newFunctor("run",3);
    G_predmark=newFunctor("predmark",0);
    G_addrmark=newFunctor("addrmark",0);
    G_undefined=newFunctor("$undefined",3);
    G_metacall=newFunctor("metacall",2);
    
    G_NIL=newFunctor("[]",0);
    G_DOT=newFunctor(".",2);
    G_DIF=newFunctor("-",2);
    // special functors - to force various external encodings
    
    G_THE=newFunctor("the",1);
    G_NO=newFunctor("no",0);
    
    G_STRING=newFunctor("$string",1);
    
    G_VAR=newFunctor("$VAR",1);
    
    G_NUMBER=newFunctor("$number",1);
  }
  
  private long oneMMtime=0;
  
  public static long SYMGCtime=0;
  
  private long oneSYMGCtime=0;
  
  final public void startSYMGCtimer() {
    oneSYMGCtime=System.currentTimeMillis();
  }
  
  final public void endSYMGCtimer() {
    oneSYMGCtime=System.currentTimeMillis()-oneSYMGCtime;
    AtomTable.SYMGCtime+=oneSYMGCtime;
  }
  
  final public void startMMtimer() {
    oneMMtime=System.currentTimeMillis();
  }
  
  final public void endMMtimer() {
    oneMMtime=System.currentTimeMillis()-oneMMtime;
    HeapStack.mmTIME+=oneMMtime;
  }
  
  boolean frozen=false;
  
  int prevSize=0;
  
  synchronized private final boolean maybeTrim() {
    if(frozen)
      return false;
      
    if(mtUnSafe()) {
      return false;
    }
    
    int currSize=size();
    
    if(currSize<SYMGC_MIN)
      return false;
      
    if(currSize<prevSize+SYMGC_DELTA&&currSize<SYMGC_FORCE) {
      return false;
    }
    
    /*
    Iterator engines=prolog.getEngines();
    long h=0L;
    
    while(engines.hasNext()) {
      LogicEngine E=(LogicEngine)engines.next();
      
      if(E.isStopped)
        continue;
      h+=E.getHeapTop();
      h+=E.trailSize();
      h+=E.choiceMem();
    }
    if(currSize<h>>2||h<prevData+SYMGC_DELTA) {
      callCtr=0;
      if(SYMTRACE)
        Interact.warnmes("SYMGC too costly, total data size="+prevData+"->"+h
            +",syms="+currSize);
      if(Interact.memory_usage_is_high())
        Interact.log("memory usage high when delaying costly symgc");
      
      return false;
    }
    
    prevData=h;
    */
    
    this.prevSize=Math.min(MANYSYMS,currSize);
    
    if(SYMTRACE)
      Interact.log("maybeTrim(): TURNING SYMGC on: "+info());
      
    return true;
    
    // Runtime.getRuntime().gc();
  }
  
  final private boolean mtUnSafe() { //
    // SYMGC=true has no effect if this happens
    // it has effect on bg(
    // return prolog.engineCount()>2&&Thread.currentThread().isDaemon();
    boolean postpone=this.logicThreadCount>0; // set by LogicThread
    /* // too much trace ...
    if(SYMTRACE&&postpone)
      Prolog.dump("### SYMGC: postponed by mtUnSafe, logicThreadCount="
          +this.logicThreadCount);
    */
    return postpone;
  }
  
  /**
   * this and addObject are the only synchronzed methods
   */
  synchronized public final void force_trimSyms(LogicEngine currentEngine) {
    if(frozen)
      return;
      
    if(mtUnSafe()) {
      symgc_flag=false;
      return;
    }
    
    if(SYMTRACE) {
      Interact.log("### SYMGC: calling from: "+Thread.currentThread());
      Prolog.dump("### SYMGC: calling from: "+Thread.currentThread());
    }
    // Interact.println("### SYMGC: before="+info());
    frozen=true; // in case of MT
    trimSyms(currentEngine);
    symgc_flag=false;
    frozen=false;
    // Interact.println("### SYMGC: after= "+info());
  }
  
  AtomTable(Prolog prolog){
    this.prolog=prolog;
    SYMGC_MIN=prolog.RUNSTART+SYMGC_DELTA;
    frozen=true;
  }
  
  synchronized AtomTable freshCopy(Prolog newProlog) {
    if(this.prolog==newProlog)
      return this; // ENGINE REFACTORING
    AtomTable other=null;
    try {
      other=(AtomTable)this.clone();
      // reset DYNAMICS
      other.logicThreadCount=0;
      other.symgc_flag=false;
      // other.ENGINES_BEFORE=0;
      other.prolog=newProlog;
      other.freshCopyFrom(this);
    } catch(Exception e) {
      Interact.errmes("error in freshCopy(), prolog="+newProlog,e);
      return null;
    }
    
    // Prolog.dump("freshCopy: "+this.hashCode()+"<>"+other.hashCode());
    // other.put("fooo","baar");
    // Prolog.dump("FOOO="+this.get("fooo"));
    return other;
  }
  
  /**
   * this and force_trimSyms are the only synchronzed methods - nothing
   * else can directly change the AtomTable
   */
  synchronized final int addObject(Object O) { // THIS IS WHERE SYMGC IS
    // SCHEDULED
    int i=ensure_added(O);
    if(SYMGC&&!symgc_flag) {
      symgc_flag=maybeTrim();
      if(SYMTRACE&&symgc_flag)
        Interact.log("### SYMGC: turning symgc_flag ON "+info());
    }
    return i;
  }
  
  /**
   * Actual SYMGC method.
   */
  
  private final void trimSyms(LogicEngine currentEngine) {
    String bef="";
    if(SYMGC&&(Interact.verbosity>3||SYMTRACE)) {
      bef="BEFORE symbol gc: logicThreads="+logicThreadCount+","+info();
      // symtest("before symgc"); // this does not make sense - engines might
      // have been removed
    }
    startSYMGCtimer();
    
    // add load time syms
    AtomTable keepers=new AtomTable(this.prolog);
    for(int i=1;i<prolog.RUNSTART;i++) {
      keepers.syms.push(null);
      keepers.re_add(i,(Sym)syms.at(i));
    }
    
    // show some debug info
    if(SYMTRACE)
      showTopSyms(keepers);
      
    // scan the heaps, also add protected Engines
    // can an engine that refers to itself escape SYMGC? - bug?
    ObjectStack engines=currentEngine.getEngines();
    
    if(SYMTRACE)
      Interact.dump("engines="+engines);
      
    for(int i=0;i<engines.size();i++) {
      LogicEngine E=(LogicEngine)engines.at(i);
      if(SYMTRACE) {
        if(Interact.verbosity>3)
          Prolog.dump("trimSyms: engine="+E+",isStopped="+E.isStopped);
      }
      if(E.isStopped)
        continue;
      /*
      if(E.isProtected())
        keepers.ensure_added(E);
      boolean present=null!=keepers.o2s(E);
      */
      E.addKeeperSyms(syms,keepers); // $$$THIS IS WHERE WE COLLECT THEM !!!
      /*
      boolean added=!present&&null!=keepers.o2s(E);
      
      if(added&&!E.isProtected()) {
        // Prolog.dump("ABOUT TO REMOVE SELF REFERENTIAL ENGINE: "+E);
        keepers.remove(E); // self referential engine
      }
      */
    }
    
    // remove unreachable Engines
    // killDeadEngines(keepers);
    
    // replace syms and ints in the AtomDict from keepers
    cloneFrom(keepers);
    
    // ENGINES_BEFORE=prolog.engineCount();
    endSYMGCtimer();
    
    // show some debug info
    if(SYMGC&&(Interact.verbosity>3||SYMTRACE)) {
      String aft="AFTER  symbol gc: logicThreads="+logicThreadCount+","+info()
          +"\n";
      Interact.println(bef);
      Interact.println(aft);
      Interact.log(bef);
      Interact.log(aft);
      symtest("after symgc");
    }
    
  }
  
  /*
  // kills unreacheables + stopped
  private void killDeadEngines(AtomTable keepers) {
    ObjectStack deadEngines=new ObjectStack();
    Iterator engines=prolog.getEngines();
    while(engines.hasNext()) {
      LogicEngine E=(LogicEngine)engines.next();
      if(E.isProtected())
        continue;
      else if(E.isStopped) {
        deadEngines.push(E);
      } else if(null==keepers.o2s(E)) {
        // Prolog.dump("UNREACHEABLE ENGINE: "+E);
        deadEngines.push(E);
      }
    }
    for(int i=0;i<deadEngines.size();i++) {
      LogicEngine E=(LogicEngine)deadEngines.at(i);
      killDeadEngine(E);
      keepers.remove(E);
    }
  }
  */
  
  /*
  private void killDeadEngine(LogicEngine E) {
    // if(SYMTRACE)
    // Prolog.dump("------------------removing dead engine="+E);
    prolog.removeEngine(E);
    E.stop(); // also stops but it does not remove !!!
  }
  */
  
  private void showTopSyms(AtomTable keepers) {
    
    Prolog.dump("KEEPERS SIZE="+keepers.size()+"/"+keepers.syms.size());
    int WINSIZE=3;
    int l=syms.size();
    int k=Math.max(1,l-WINSIZE);
    for(int i=k;i<l;i++) {
      // keepers.syms.push(null);
      // keepers.re_add(i,(Sym)syms.at(i)); // index error - gap - obviously
      // keepers.ensure_added((Sym)syms.at(i)); // ineffective
      Object o=syms.at(i);
      String s=(null==o)?"null":o.toString();
      if(s.length()<80)
        Prolog.dump("SYM "+i+":"+s);
    }
    
  }
  
  /*
  public String info() {
    return prolog.enginfo()+",RUNSTART="+prolog.RUNSTART+",threads="
        +Thread.activeCount()+","+super.info();
  }
  */
  synchronized public String rinfo() {
    int i=prolog.RUNSTART; // or =1
    int l=syms.size();
    StringBuffer sbuf=new StringBuffer("syms.size()="+l+"\n");
    if(SYMTRACE) {
      for(;i<l;i++) {
        Object sym=i2o(i);
        if(null==sym)
          sym="(null)";
        sbuf.append(i+":<"+sym+">\n");
      }
    } else
      sbuf.append(""+(l-i));
    return "RUNTIME SYMS:\n"+sbuf.toString();
  }
  
  public final void symtest(Object mes) {
    if(!SYMTEST)
      return;
    int l=this.syms.size();
    StringBuffer buf=new StringBuffer();
    int ctr=0;
    for(int i=1;i<l;i++) {
      if(null==syms.at(i)) {
        ctr++;
        buf.append(i+" ");
      }
    }
    if(0==ctr)
      return;
    mes=mes+"!!! SYMTEST "+ctr+"/"+l+" null symbol(s) at: "+buf+" !!!\n";
    Interact.log(mes);
    Interact.dump(mes);
  }
} // end class AtomTable
