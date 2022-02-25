package vm.logic;

/**
 * allows building external terms represented as generic Object refrences in a uniform way
 * in particular, it allows interoperation with Kernel Prolog interpreter
 */
public interface OTerm {
  public static final long serialVersionUID=222L;
  
  public Object externalizeVar(long xval) throws PrologException;
  
  public Object externalizeConst(Object c);
  
  public Object externalizeInt(int i);
  
  public Object externalizeFun(Object f,Object[] args);
  
  public Object toExternal(long i,OTerm O);
}