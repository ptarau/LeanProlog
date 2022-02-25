package vm.extensions;

import java.io.BufferedReader;

import vm.logic.Fun;
import vm.logic.Interactor;

public class TokenStreamInput extends IOInteractor {
  private static final long serialVersionUID=222L;
  
  private Tokenizer T;
  
  public static Interactor new_interactor(String fname) {
    TokenStreamInput I=new TokenStreamInput(fname);
    if(null==I.T)
      return null;
    return I;
  }
  
  public static Interactor new_interactor(Fun fname) {
    TokenStreamInput I=new TokenStreamInput(fname);
    if(null==I.T)
      return null;
    return I;
  }
  
  public static Interactor new_interactor(BufferedReader fname) {
    TokenStreamInput I=new TokenStreamInput(fname);
    if(null==I.T)
      return null;
    return I;
  }
  
  public TokenStreamInput(Object fname){
    super(fname);
    this.T=Tokenizer.newTokenizer(fname);
  }
  
  public Object interactor_handle(Object message) {
    Object R=T.getClauseTokens();
    if(null==R)
      return "no";
    return new Fun("the",R);
  }
  
  public void stop_interactor() {
    super.stop_interactor();
    T=null;
  }
}
