package vm.logic;

/**
 * Provides a generic Prolog Exception type
 */
public class PrologException extends Exception implements Stateful { // abstract
  private static final long serialVersionUID=222L;
  
  public PrologException(String s){
    super(s);
    if(Interact.verbosity>3)
      Interact.warnmes("PrologException: "+s);
  }
  
  public String toString() {
    String name=getClass().getName();
    int l=name.lastIndexOf(".");
    String Q="\'";
    if(l>0)
      name=name.substring(l+1,name.length());
    return "\'!! Prolog_Error\'(\'"+name+Q+","+Q+getMessage()+Q+"\')";
  }
}
