package vm.logic;
import java.io.Serializable;
/**
Interface that enables full use of Serialization and Cloning for 
saving/duplicating the state of all Prolog classes.
 */
public interface Stateful extends Serializable, Cloneable {
  public static final long serialVersionUID = 222L;
}
