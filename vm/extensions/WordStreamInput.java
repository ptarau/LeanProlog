package vm.extensions;

import vm.logic.Fun;
import vm.logic.Interactor;

public class WordStreamInput extends IOInteractor {
  private static final long serialVersionUID=222L;
  
  private WordReader T;
  
  public static Interactor new_interactor(String fname) {
    WordStreamInput I=new WordStreamInput(fname);
    if(null==I.T)
      return null;
    return I;
  }
  
  public static Interactor new_interactor(Fun fname) {
    WordStreamInput I=new WordStreamInput(fname);
    return I;
  }
  
  public WordStreamInput(Object fname){
    super(fname);
    this.T=WordReader.newWordReader(fname);
  }
  
  public Object interactor_handle(Object message) {
    Object R=T.getWord();
    if(null==R)
      return "no";
    return new Fun("the",R);
  }
  
  public void stop_interactor() {
    super.stop_interactor();
    T=null;
  }
}
