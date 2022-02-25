package vm.logic;

/**
 * Implements Heap garbage collection - to be applied to a given Machine
 * Algorithm: simplified mark & sweep - Copyright (C) Paul Tarau  2001
 */
final class PrologGC {
  private static final long serialVersionUID=222L;
  
  /**
  * change this to true to trigger extensive GC tracing
  */
  public static int trace=0;
  
  public static final int minGC=HeapStack.MINSIZE;
  
  public static long gcTIME=0;
  
  private long oneGCtime=0;
  
  // private final static boolean profile=true;
  
  final void startGCtimer() {
    oneGCtime=System.currentTimeMillis();
  }
  
  final void endGCtimer() {
    oneGCtime=System.currentTimeMillis()-oneGCtime;
    gcTIME+=oneGCtime;
  }
  
  /**
   * The constructor gets a reference to a machine's registers
   * choice point stack, trail and heap. Various methods defined
   * in this class will perform GC - by possibly updating
   */
  public PrologGC(int arity,long[] regs,ChoicePointStack choice,
      TrailStack trail,HeapStack heap){
    
    this.arity=arity;
    this.regs=regs;
    this.choice=choice;
    this.heap=heap;
    this.trail=trail;
  }
  
  private int arity;
  
  private long[] regs;
  
  private HeapStack heap;
  
  private TrailStack trail;
  
  private ChoicePointStack choice;
  
  // private IntStack roots;
  
  private LongStack border;
  
  private boolean[] marked;
  
  private int mark_ctr;
  
  // a subset of recongnizers - should be all the GC needs
  
  /**
   * checks if a cell is a variable (heap reference)
   */
  final static boolean isVar(long cell) {
    return Defs.isVAR(cell);
  }
  
  /**
   * Checks if a cell is atomic - i.e. integer or symbolic constant
   */
  final static boolean isAtomic(long cell) {
    return Defs.isATOMIC(cell);
  }
  
  /**
   * check if the cell starts a compound term - use
   * getArity(cell) to see how many cells follow this
   */
  final static boolean isCompound(long cell) {
    return Defs.isCOMPOUND(cell);
  }
  
  /**
   * returns the arity of a functor cell indicating
   * the beginning of a compound term
   */
  final static int getArity(long cell) {
    return Defs.GETARITY(cell);
  }
  
  /**
   * main garbage collector call
  */
  public boolean collect() throws PrologException {
    int before=heap.getUsed();
    if(before<minGC)
      return true;
    startGCtimer();
    
    if(trace>=2)
      Prolog.dump("running gc, heap used="+before);
    if(trace==3) {
      dumpChoice();
      dumpRegs();
      dumpTrail(20);
    }
    
    border=new LongStack(); // grey cells
    findRoots(); // initializes border (grey cell set to roots)
    marked=new boolean[heap.getUsed()]; // allocates mark array
    mark_ctr=mark(); // do marking
    border=null; // no need for border from now on
    
    if(trace==3) {
      Prolog.dump(Interact.NL+"TOTAL MARKED="+mark_ctr+"/"+heap.getHeapTop());
      dumpMarkedHeap(30,true);
    }
    sweep(); // sweep and copy to area above top then relocate back and update
             // stacks
    if(trace==3) {
      Prolog.dump(Interact.NL+"======================:"+mark_ctr+"/"
          +heap.getUsed());
      dumpChoice();
      dumpRegs();
      dumpTrail(20);
      dumpMarkedHeap(30,false);
      Prolog.dump(Interact.NL+"TOTAL MARKED="+mark_ctr+"/"+heap.getUsed());
    }
    marked=null; // no need for marks anymore
    
    choice.shrinkAll();
    
    endGCtimer();
    
    if(trace>=1) {
      Prolog.dump("GC: words collected="+(before-heap.getUsed())+", free="
          +heap.getFree()+" gc time="+oneGCtime+"ms"+" ,total GC="+gcTIME
          +", trail="+trail.getTop()+", choice="+choice.getTop());
      Runtime r=Runtime.getRuntime();
      // r.gc();
      Prolog
          .dump("Java Memory: total="+r.totalMemory()+" free="+r.freeMemory());
    }
    return true;
  }
  
  /**
   * collects all roots
   */
  void findRoots() throws PrologException {
    // add registers in choicepoints
    for(int i=0;i<choice.size();i++) {
      long[] cregs=((ChoicePointStackEntry)choice.at(i)).regs;
      for(int j=0;j<cregs.length;j++) {
        addRoot(cregs[j]);
      }
    }
    // add argument registers
    for(int i=1;i<=arity;i++) {
      addRoot(regs[i]);
    }
    
    // adds heap[0]=0 - used to redirect dead trail cells
    addRoot(0);
  }
  
  /**
   * adds a root - containing a heap reference to live data
   */
  final void addRoot(long regs2) {
    if(isVar(regs2))
      border.push(regs2);
  }
  
  /**
   * Iterative marking algorithm
   */
  int mark() throws PrologException {
    // iterative depth first marking phase
    int ctr=0;
    while(!border.isEmpty()) {
      long x=border.pop();
      if(isVar(x)) {
        int ix=Defs.xx(x);
        // if(x>=heap.getUsed())
        // Interact.assertion("bad index="+x+"should be <"+heap.getUsed()+"free:"+heap.getFree());
        if(!marked[ix]) {
          long v=heap.getRef(x);
          // if(v==0 && x!=0) Interact.assertion("bad null cell at index="+x);
          marked[ix]=true;
          ++ctr;
          if(isVar(v))
            border.push(v);
          else if(isCompound(v)) {
            for(int i=1;i<=getArity(v);i++) {
              border.push(x+i);
            }
          }
        }
      }
    }
    
    if(trace>=2)
      Prolog.dump("mark_ctr before marking tops: "+ctr);
    
    // further marking:
    // artificially mark cells referenced by H and T in choice points
    // this largely simplifies sweep
    for(int i=0;i<choice.size();i++) {
      ChoicePointStackEntry cp=((ChoicePointStackEntry)choice.at(i));
      /*
      if(!(cp.heapTop>=0 && cp.heapTop<heap.getUsed())) 
        Prolog.dump("bad heapTop="+cp.heapTop+" in choice point:"+i);
      */
      int h=cp.heapTop;
      if(!marked[h]) {
        heap.setRef(h,0);
        marked[h]=true;
        ++ctr;
      } // needs to be zeroed if unmarked!!!
      
      /*
      //if(!(cp.trailTop>=-1 && cp.trailTop<heap.getUsed())) 
      //   Interact.assertion("bad trailTop="+cp.trailTop+" in choice point:"+i);
      if(-1==cp.trailTop) continue;
      int t=trail.at(cp.trailTop);
      
      //if(!(t>=0 && t<heap.getUsed())) 
      //Prolog.dump("bad trailTop ref="+t+" in choice point:"+i);
      h=heap.getRef(t);
      if(!marked[h]) {marked[h]=true; ++ctr;}
      */
    }
    
    if(trace>=2)
      Prolog.dump("mark_ctr after marking tops: "+ctr);
    
    // Test validity of marking - by zeroing all unmarked data
    // for(int i=0; i<heap.getUsed(); ++i) {
    // if(!marked[i]) heap.setRef(i,0);
    // }
    
    return ctr;
  }
  
  final long slideBack(long x) {
    if(isVar(x)) {
      // if(!(x<heap.getUsed()))
      // Interact.assertion("slideBack: "+x+" expected< "+heap.getUsed());
      x=heap.getRef(x);
      // if(!(x<=mark_ctr))
      // avaIO.assertion("slideBack: "+x+" expected <= "+mark_ctr);
    }
    return x;
  }
  
  void sweep() {
    checkUpperHeap();
    int to=liftMarked();
    relocateHeap(to);
    relocateChoicePoints();
    relocateRegs();
    relocateTrail();
    slideBackHeap(to);
  }
  
  /**
   *  makes sure there's enough space to lift marked cells to upper heap
   */
  void checkUpperHeap() {
    if(mark_ctr>=heap.getFree()) {
      if(trace>0)
        Prolog.dump("GC: marked="+mark_ctr+"free="+heap.getFree());
      heap.expand(); // make sure we have room
      if(trace>0)
        Prolog.dump("GC: expanded, now free="+heap.getFree());
    }
  }
  
  /**
   * Copies each marked cell to the upper heap and set forward links
   * from old heap cells (vars and nonvars!) to copied new cells
   * note: variables in new cells still point to old addresses
   * therefore on more pass is needed to fix them - see relocateHeap(...)
   */
  int liftMarked() {
    int to=heap.getUsed(); // first free after heap.getTop()
    for(int from=0;from<heap.getUsed();++from) {
      if(marked[from]) {
        heap.setRef(to,heap.getRef(from));
        heap.setRef(from,to-heap.getUsed()); // where it will be after
                                             // relocation
        ++to;
      }
    }
    // if(mark_ctr!=to-heap.getUsed())
    // Interact.assertion("marked="+mark_ctr+"<>swept="+(to-heap.getUsed()));
    return to;
  }
  
  /**
   * Redirects new var cells to point where they should, by following
   * forwarding pointers in old variable cells to relocated new variables.
   * Note: the heap still needs to be slided - but this is delayed
   * until forwarding pointers in old cells are not be needed anymore.
   */
  void relocateHeap(int to) {
    for(int h=heap.getUsed();h<to;++h) {
      long oldref=heap.getRef(h);
      if(isVar(oldref)) {
        // if x is a var in the old heap then it points where the new landed
        long newref=heap.getRef(oldref); // forwarding points to future location
                                         // after sliding
        heap.setRef(h,newref);
      }
    }
  }
  
  /**
   * Relocates choicePoints (heapTops and regs)
   * using lower heap as a fast relocation table.
   */
  
  void relocateChoicePoints() {
    for(int i=0;i<choice.size();i++) {
      ChoicePointStackEntry cp=((ChoicePointStackEntry)choice.at(i));
      cp.heapTop=Defs.xx(slideBack(cp.heapTop));
      
      long[] cregs=cp.regs;
      for(int j=0;j<cregs.length;j++) {
        cregs[j]=slideBack(cregs[j]);
      }
    }
  }
  
  /**
   * Relocates registers in regs[1..arity]
   * using lower heap as a fast relocation table.
   */
  void relocateRegs() {
    for(int j=1;j<=arity;j++) {
      regs[j]=slideBack(regs[j]);
    }
  }
  
  void relocateTrail() {
    relocateTrail(0,trail.getTop());
  }
  
  /** Relocates trail and sets all dead trail cell to point to 
   *  dummy unbound variable at address 0
   *  This makes relocating the to of the trail pointers 
   *  in choicpoints unnecessary
  */
  
  void relocateTrail(int min,int max) {
    for(int i=min;i<=max;++i) {
      int h=trail.at(i);
      if(!marked[h])
        trail.update(i,0);
      else
        trail.update(i,Defs.xx(slideBack(h)));
    }
  }
  
  /** 
   * Slides heap from upper to lower area.
   * This should be last thing to do - as relocation
   * table in lower heap will be overwritten.
   */
  void slideBackHeap(int to) {
    for(int h=heap.getUsed();h<to;h++) {
      heap.setRef(h-heap.getUsed(),heap.getRef(h));
    }
    
    // set top of the heap to new value;
    heap.setHeapTop(to-heap.getUsed()-1);
  }
  
  /**
   * prints out heap cells showing which are marked
   */
  void dumpMarkedHeap(int max,boolean onlyMarked) throws PrologException {
    Prolog.dump("\nHEAP: "+heap.getUsed());
    int ctr=0;
    if(max==0)
      max=heap.getUsed();
    for(int i=0;ctr<max&&i<heap.getUsed();i++) {
      if(onlyMarked&&!marked[i])
        continue;
      String m=(!marked[i])?" ":"*";
      Prolog.dump("["+ctr+"] "+m+"_"+i+"=>"+heap.cellToString(i));
      ++ctr;
    }
  }
  
  /**
   * prints out choice point information
   */
  void dumpChoice() throws PrologException {
    Prolog.dump("\nCHOICE: "+choice.getTop());
    for(int i=0;i<choice.size();i++) {
      ChoicePointStackEntry cp=(ChoicePointStackEntry)choice.at(i);
      Prolog.dump("\nchoice["+i+"]: H="+cp.heapTop+" T="+cp.trailTop+" P="
          +cp.instrPtr);
      long[] cregs=cp.regs;
      for(int j=0;j<cregs.length;j++) {
        Prolog.dump("cregs["+j+"]="+heap.dumpCell(cregs[j]));
      }
    }
  }
  
  /**
   * prints out registers
   */
  void dumpRegs() throws PrologException {
    Prolog.dump(Interact.NL+"REGS: "+arity);
    for(int i=1;i<=arity;i++) {
      Prolog.dump("["+i+"] "+heap.dumpCell(regs[i]));
    }
  }
  
  /**
   * prints out trail cells
   */
  void dumpTrail(int max) {
    dumpTrail(trail,heap,max);
  }
  
  static void dumpTrail(TrailStack trail,HeapStack heap,int max) {
    Prolog.dump(Interact.NL+"TRAIL: "+trail.getTop());
    if(0==max)
      max=trail.getTop();
    for(int i=0;i<max&&i<=trail.getTop();++i) {
      Prolog.dump("["+i+"] "+trail.at(i)+"=>"+heap.cellToString(trail.at(i)));
    }
  }
}
