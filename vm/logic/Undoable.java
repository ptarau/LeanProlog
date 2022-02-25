package vm.logic;

/**
Java data subject to undo action on backtracking should implement this.
 */
public interface Undoable {
  public static final long serialVersionUID = 222L;
  
  public void undo();
}