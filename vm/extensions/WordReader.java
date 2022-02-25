package vm.extensions;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;

import vm.logic.Fun;
import vm.logic.Interact;

/**
 * Reads chars from char streams using the current default encoding
 */
public class WordReader extends StreamTokenizer {
  private static final long serialVersionUID=222L;
  
  public static WordReader newWordReader(Object fname) {
    
    try {
      Reader R;
      if(fname instanceof String) {
        R=Interact.safeFileReader((String)fname);
      } else {
        Fun F=(Fun)fname;
        R=new StringReader((String)F.getArg(1));
      }
      WordReader T=new WordReader(R);
      return T;
      
    } catch(IOException e) {
      e.printStackTrace();
      return null;
    }
  }
  
  public WordReader(Reader reader){
    super(reader);
    resetSyntax();
    eolIsSignificant(true);
    ordinaryChar('.');
    ordinaryChars('!','/'); // 33-47
    ordinaryChars(':','@'); // 55-64
    ordinaryChars('[','`'); // 91-96
    ordinaryChars('{','~'); // 123-126
    wordChars('_','_');
    wordChars('a','z');
    wordChars('A','Z');
    wordChars('0','9');
    slashStarComments(false);
    slashSlashComments(false);
    ordinaryChar('%');
  }
  
  public Object getWord() {
    Object t=null;
    
    int c=TT_EOF;
    try {
      c=nextToken();
      while(Character.isWhitespace(c)&&c!=TT_EOL) {
        c=nextToken();
      }
    } catch(IOException e) {
      Interact.errmes("tokenizer error",e);
      return t;
    }
    
    switch(c) {
      case StreamTokenizer.TT_WORD:
        t=sval;
      break;
      
      case StreamTokenizer.TT_EOL:
        t=""+(char)10;
      break;
      
      case StreamTokenizer.TT_EOF: {
        // t==null
      }
      break;
      
      default: {
        t=""+(char)c;
      }
        
    }
    // Prolog.dump("<<"+c+">>"+M.termToString(t));
    return t;
  }
  
}