package vm.logic;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

class CodeLoader implements Stateful {
  private static final long serialVersionUID=222L;
  
  /**
   * loads bytecode from a file or open stream and possibly combines
   * wam file with default bp file that contains builtins, runtime system etc.
   * note that wam files can be compiled separately with wcompile an then concatenated
   */
  static boolean load(Object fnameOrStream,CodeStore codeStore) {
    boolean ok=false;
    if(fnameOrStream instanceof String) {
      ok=fload((String)fnameOrStream,codeStore,true);
      codeStore.setSYSEND();
    } else {
      // merge lwam.bp builtins + code with bytecode from stream
      // note that first the default bp file is loaded (without terminating the
      // file) then, floadfromReader loads from the additional wam file and
      // closes the process
      ok=fload(Interact.PROLOG_BYTECODE_FILE,codeStore,false); /* do not terminate */
      if(ok) {
        codeStore.setSYSEND();
        ok=floadfromReader((BufferedReader)fnameOrStream,codeStore,true); /* terminate */
      }
    }
    return ok;
  }
  
  private static final InputStream url2stream(String f,boolean quiet) {
    // System.err.println("trying URL: "+f);
    InputStream stream=null;
    try {
      URL url=new URL(f);
      stream=url.openStream();
    } catch(MalformedURLException e) {
      if(quiet)
        return null;
      Interact.errmes("bad URL: "+f,e);
    } catch(Exception e) {
      if(quiet)
        return null;
      Interact.errmes("unable to read URL: "+f,e);
    }
    
    return stream;
  }
  
  /**
   * Loads code from a local *.bp file
   */
  private static boolean fload(String fName,CodeStore codeStore,
      boolean terminate_file) {
    BufferedReader R;
    try {
      R=Interact.safeFileReader(fName);
    } catch(Exception e) {
      R=null;
    }
    
    if(null==R) {
      // jar:file:/fullpath/main.jar!/lib/a.jar!/a.resource,
      // String jarPath="/Users/tarau/Desktop/tarau/art/prologL/";
      String jarName="jar:file:"+Interact.PROLOG_JAR_FILE+"!/"+fName;
      InputStream S=url2stream(jarName,false);
      if(null==S)
        return false;
      // R=new InputStreamReader(S);
      R=Interact.tryEncoding(S);
    }
    
    if(null==R) {
      Interact.warnmes("Code file not found as:"+fName);
      return false;
    }
    
    try {
      floadfromReader(R,codeStore,terminate_file);
      R.close();
    } catch(Exception e) {
      Interact.errmes("Error in loading:"+fName,e);
      return false;
    }
    return true;
  }
  
  /**
   * Reads instructions from *.bp Prolog bytecode files
   */
  private static boolean floadfromReader(BufferedReader in,CodeStore codeStore,
      boolean terminate_file) {
    boolean atEof=false;
    int opcode,reg,arity;
    String l="";
    int k=0;
    try {
      for(;;) {
        k++;
        l=in.readLine();
        atEof=(null==l);
        if(atEof)
          break;
        opcode=Integer.parseInt(l);
        k++;
        l=in.readLine();
        atEof=(null==l);
        if(atEof)
          break;
        reg=Integer.parseInt(l);
        k++;
        l=in.readLine();
        atEof=(null==l);
        if(atEof)
          break;
        arity=Integer.parseInt(l);
        k++;
        l=in.readLine();
        atEof=(null==l);
        if(atEof)
          break;
        codeStore.loadInstruction(opcode,reg,l,arity);
        // if (opcode == Defs.END) break;
      }
      // if (opcode != Defs.END)
      // Interact.errmes("Premature end of file during instruction loading.");
    } catch(EOFException eof) {
      atEof=true;
    } catch(Exception e) {
      Interact.errmes("Exception loading instruction at line: "+k+"=><<<"+l
          +">>>",e);
      atEof=false;
    }
    if(terminate_file&&atEof) {
      try {
        codeStore.fake_end();
      } catch(Exception e) {
        Interact.errmes("Exception ending code",e);
        return false;
      }
    }
    return true;
  }
  
}