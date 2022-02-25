package vm.logic;

/**
 * ChoicePoint entry containing heapTop,trailTop,instrPtr fields and register array regs
 */
final class ChoicePointStackEntry implements Stateful {
  private static final long serialVersionUID=222L;
  
  ChoicePointStackEntry(int heapTop,int trailTop,int instrPtr,long[] regs,
      int regCount){
    this.heapTop=heapTop;
    this.trailTop=trailTop;
    this.instrPtr=instrPtr;
    this.regs=new long[regCount];
    System.arraycopy(regs,1,this.regs,0,regCount);
    undoStack=null;
  }
  
  int heapTop;
  
  int trailTop;
  
  int instrPtr;
  
  long[] regs;
  
  ObjectStack undoStack;
  
  final void add(Undoable O) {
    if(null==undoStack)
      undoStack=new ObjectStack();
    undoStack.push(O);
  }
  
  void undo() {
    if(null==undoStack)
      return;
    
    for(int i=undoStack.getTop();i>=0;i--) {
      Undoable O=(Undoable)undoStack.at(i);
      O.undo();
    }
  }
  
  public String toString() {
    return "(H="+heapTop+",TR="+trailTop+",P="+instrPtr+",regs="+regs.length
        +")";
  }
} // End class ChoicePointStackEntry