package vm.logic;

public interface Extender {
  static final long serialVersionUID=222L;
  
  Object xcall(int Op,Object Input,LogicEngine E);
  
  public int obtype_of(Prolog current,Object O);
}
