package vm.extensions;

import java.io.BufferedReader;
import java.io.CharArrayReader;
import java.io.IOException;

import vm.logic.Interact;
import vm.logic.Interactor;

public class LineStreamInput extends IOInteractor {
  private static final long serialVersionUID=222L;
  
  private BufferedReader in;
  
  private TokenStreamInput tReader=null;
  
  public static Interactor new_interactor(String fname) {
    try {
      return new LineStreamInput(fname);
    } catch(Exception e) {
      Interact.errmes("new_interactor: file error",e);
      return null;
    }
  }
  
  public static Interactor new_interactor(LineStreamOutput source) {
    try {
      return new LineStreamInput(source);
    } catch(Exception e) {
      Interact.errmes("new_interactor: memory file error",e);
      return null;
    }
  }
  
  public LineStreamInput(String fname) throws IOException{
    super(fname);
    this.in=Interact.safeFileReader(fname);
  }
  
  public LineStreamInput(LineStreamOutput S) throws IOException{
    super(S);
    char[] chars=S.toChars();
    if(null==chars)
      throw new IOException("not a memory file"+S);
    this.in=new BufferedReader(new CharArrayReader(chars));
    // Interact.println("LineStreamInput called, in="+in);
  }
  
  public static BufferedReader getReader(LineStreamInput I) {
    // Interact.println("getReader called, in="+I.in);
    return I.in;
  }
  
  public TokenStreamInput getTokenizer() {
    if(null==this.tReader)
      this.tReader=(TokenStreamInput)TokenStreamInput.new_interactor(in);
    return tReader;
  }
  
  public void prompt() throws IOException {
    // do nothing
  }
  
  public String readln() throws IOException {
    return in.readLine();
  }
  
  public int read() throws IOException {
    return in.read();
  }
  
  public void stop_interactor() {
    super.stop_interactor();
    try {
      in.close();
    } catch(IOException e) {
      Interact.errmes("stop_interactor: error in closing input",e);
    }
    in=null;
  }
  
  public String toString() {
    // new Exception("IOInteractor").printStackTrace();
    // return "!!!unexpected in LineStreamInput: "+super.toString();
    return "LineStreamInput_"+hashCode();
  }
}
