package vm.extensions;

import java.io.BufferedWriter;
import java.io.IOException;

import vm.logic.Interact;
import vm.logic.Interactor;

public class ConsoleIO extends IOInteractor {
  private static final long serialVersionUID=222L;
  
  // using jline
  static final private jline.ConsoleReader in=makeReader();
  
  private static jline.ConsoleReader makeReader() {
    try {
      return new jline.ConsoleReader();
    } catch(Exception e) {
      err("unable to start ConsoleIO");
      return null;
    }
  }
  
  public String readln() throws IOException {
    return in.readLine("");
  }
  
  public int read() throws IOException {
    return in.readVirtualKey();
  }
  
  static final private BufferedWriter out=Interact.tryEncoding(System.out);
  
  private static final IOInteractor defaultConsoleIO=new ConsoleIO("?- ");
  
  /**
   * Will be called using reflection - argument should match _exactly_ type used
   * in Prolog - typically a Prolog atom i.e. a Java String
   */
  public static Interactor new_interactor(String prompt) {
    if(defaultConsoleIO.initiator.equals(prompt))
      return defaultConsoleIO;
    return new ConsoleIO(prompt);
  }
  
  public ConsoleIO(Object prompt){
    super(prompt);
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
  
  public void prompt() throws IOException {
    print(initiator.toString());
  }
  
}
