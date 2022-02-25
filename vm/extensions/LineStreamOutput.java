package vm.extensions;

import java.io.BufferedWriter;
import java.io.CharArrayWriter;
import java.io.IOException;

import vm.logic.Interact;
import vm.logic.Interactor;

public class LineStreamOutput extends IOInteractor {
  private static final long serialVersionUID=222L;
  
  private BufferedWriter out;
  
  private CharArrayWriter outchars;
  
  public static Interactor new_interactor(String fname) {
    try {
      return new LineStreamOutput(fname);
    } catch(IOException e) {
      Interact.errmes("new interactor failing on output to: "+fname,e);
      return null;
    }
  }
  
  public LineStreamOutput(String fname) throws IOException{
    super(fname);
    if("$mem".equals(fname)) {
      // Prolog.dump("$mem");
      this.outchars=new CharArrayWriter();
      this.out=new BufferedWriter(this.outchars);
    } else {
      boolean append=false;
      if(fname.startsWith("++")) {
        append=true;
        fname=fname.substring(2);
      }
      this.out=Interact.safeFileWriter(fname,append);
    }
  }
  
  public void println(Object O) throws IOException {
    if(null==O)
      O="$null";
    out.write(O.toString());
    out.newLine();
    out.flush();
  }
  
  public void print(Object O) throws IOException {
    if(null==O)
      O="$null";
    out.write(O.toString());
    out.flush();
  }
  
  public void stop_interactor() {
    super.stop_interactor();
    try {
      out.close();
    } catch(IOException e) {
      Interact.errmes("stop_interactor: error in closing output",e);
    }
    out=null;
  }
  
  char[] toChars() {
    if(null!=outchars)
      return outchars.toCharArray();
    return null;
  }
  
  public String toString() {
    if(null!=outchars)
      return outchars.toString();
    return super.toString();
  }
}
