package vm.logic;

/**
Instructs the current machine to apply undo action to a key.
 */
public class Undo implements Undoable, Stateful {
  
  private static final long serialVersionUID = 222L;
  
  private Object key;

  public Undo(Object key) {
    this.key = key;
  }

  public void undo() {
    if(null!=key) {}
    key=null;
  }
  
  /**
   * cleans left over objects
   */
  public void finalize() {
    undo();
  }
}