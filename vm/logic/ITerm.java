package vm.logic;

/**
 * allows building integer represented terms on a heap in a uniform way
 */
public interface ITerm {
  public static final long serialVersionUID=222L;
  
  public long internalizeVar(Object id) throws PrologException;
  
  public long internalizeConst(Object c) throws PrologException;
  
  public long internalizeString(String s) throws PrologException;
  
  public long internalizeInt(int i) throws PrologException;
  
  public long internalizeFun(Object f,long[] args) throws PrologException;
  
  public long internalizeList(Fun L) throws PrologException;
  
  public long toInternal(Object o) throws PrologException;
}
