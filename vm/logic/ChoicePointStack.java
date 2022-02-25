package vm.logic;

/**
 * Implements the BinWAM engine's choice point stack
 */
final class ChoicePointStack extends ObjectStack {
  private static final long serialVersionUID=222L;
  
  private HeapStack heap;
  
  private TrailStack trail;
  
  ChoicePointStack(HeapStack heap){
    super();
    this.heap=heap;
  }
  
  final void setTrail(TrailStack trail) {
    this.trail=trail;
  }
  
  final int SAVED_H() {
    return ((ChoicePointStackEntry)peek()).heapTop;
  }
  
  final int SAVED_TR() {
    return ((ChoicePointStackEntry)peek()).trailTop;
  }
  
  final int SAVED_P() {
    return ((ChoicePointStackEntry)peek()).instrPtr;
  }
  
  final int setSAVED_P(int instrPtr) {
    return ((ChoicePointStackEntry)peek()).instrPtr=instrPtr;
  }
  
  /**
   * Adds a choicepoint to the stack.
   */
  final int addChoicePoint(int instrPtr,long[] regs,int regCount) {
    push(new ChoicePointStackEntry(heap.getHeapTop(),trail.getTop(),instrPtr,
        regs,regCount));
    return getTop();
  }
  
  void expand() {
    heap.startMMtimer();
    super.expand();
    heap.endMMtimer();
  }
  
  /**
   * Aggressive space recovery mechanism - tries to shrink all eligible data
   * areas. By calling this on backtrack, cut, and heap overflow one can make
   * sure that no Prolog engine locks memory resources inadvertently.
   */
  void shrinkAll() {
    heap.startMMtimer();
    trail.tidy();
    // collectTrail();
    trail.shrink();
    this.shrink();
    heap.shrink();
    heap.endMMtimer();
  }
  
  final void setCut(int cutBint) {
    setTop(cutBint);
    shrinkAll(); // shrinks on forward execution - see also gc
  }
  
  final int restoreState(long[] regs,boolean discardChoice) {
    trail.unwindTrail(SAVED_TR());
    heap.setHeapTop(SAVED_H());
    long[] savedRegs=((ChoicePointStackEntry)peek()).regs;
    System.arraycopy(savedRegs,0,regs,1,savedRegs.length);
    
    ((ChoicePointStackEntry)peek()).undo();
    
    if(discardChoice) {
      pop();
      shrinkAll(); // aggressive space recovery
      return getTop();
    }
    
    return getTop()-1;
  }
  
  void destroy() {
    while(!isEmpty()) {
      ChoicePointStackEntry cp=(ChoicePointStackEntry)pop();
      cp.undo();
    }
    super.destroy();
  }
  
} // End class ChoicePointStack
