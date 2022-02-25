package vm.extensions;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;

import vm.logic.Interact;

public class PreTokenizer extends StreamTokenizer {
  private static final long serialVersionUID=222L;
  
  public PreTokenizer(Reader R) throws IOException{
    super(R);
    resetSyntax();
    whitespaceChars(0,8); // 9 tab, 10 eol excepted
    whitespaceChars(11,31);
    whitespaceChars(127,127);
    ordinaryChars(' ',' '); // 32
    ordinaryChars('!','/'); // 33-47
    ordinaryChars(':','@'); // 55-64
    ordinaryChars('[','`'); // 91-96
    ordinaryChars('{','~'); // 123-126
    wordChars('_','_');
    wordChars('a','z');
    wordChars('A','Z');
    wordChars('0','9');
    // parseNumbers(); // DO NOT DO THIS - long integers are chopped
    // commentChar('%'); // 37
    // quoteChar('\''); // 39 DO NOT DO THIS - we need to know what we quote
    eolIsSignificant(true); // 10 is now spacial
    lowerCaseMode(false);
  }
  
  public static final int TT_FULLSTOP=-10;
  
  public static final int TT_Q=-20;
  
  public static final int TT_DQ=-30;
  
  public static final int TT_ERR=-99;
  
  final public static boolean isSPACE(int t) {
    return Character.isWhitespace(t);
  }
  
  // look ahead
  private final int N() throws IOException {
    int next=super.nextToken();
    super.pushBack();
    return next;
  }
  
  // skip
  private final void S() throws IOException {
    super.nextToken();
  }
  
  // return next
  private final int C() throws IOException {
    return super.nextToken();
  }
  
  void spaces() throws IOException {
    for(;;) {
      int w=N();
      if(TT_EOF==w)
        break;
      if(!isSPACE(w))
        break;
      S();
    }
  }
  
  // override
  public int nextToken() throws IOException {
    
    int t=C();
    if(t==TT_EOF)
      return TT_EOF;
    else if('.'==t) {
      int n=N();
      if(isSPACE(n)||TT_EOL==n||TT_EOF==n) {
        if(TT_EOF!=n) {
          S();
          spaces();
        }
        t=TT_FULLSTOP;
      }
    } else if(isSPACE(t)) {
      spaces();
    } else if('/'==t&&'*'==N()) {
      for(;;) {
        t=C();
        if(TT_EOF==t)
          return TT_ERR;
        if(t=='*'&&'/'==N()) {
          S();
          spaces();
          t=' ';
          break;
        }
      }
    } else if('%'==t) {
      for(;;) {
        t=C();
        if(t==TT_EOL)
          break;
        if(t==TT_EOF)
          break;
      }
    } else if('\''==t)
      t=getQuoted(t,TT_Q);
    else if('"'==t)
      t=getQuoted(t,TT_DQ);
    /* else if("0".equals(sval)) {
      // System.out.println("<at 0'"+sval+">");
      // things to do here to possibly accept 0'<char> syntax - but tricky!!!
    } 
    */
    else {
      // nothing - just defaults TT_WORD or ordinary char
    }
    return t;
    
  }
  
  int getQuoted(int t,int TQ) throws IOException {
    int Q=t;
    StringBuffer sbuf=new StringBuffer();
    for(;;) {
      t=C();
      if(t==TT_EOL)
        return TT_ERR;
      if(t==TT_EOF)
        return TT_ERR;
      if('\\'==t) { // escape
        t=C();
        sbuf.append((char)t);
      } else if(Q==t) { // twice quote
        int n=N();
        if(Q==n) {
          S();
          sbuf.append((char)t);
        } else { // closing quote
          super.sval=sbuf.toString();
          sbuf=null;
          t=TQ;
          break;
        }
      } else if(t==TT_WORD)
        sbuf.append(sval);
      else
        sbuf.append((char)t);
    }
    return TQ;
  }
  
  String show(int t) {
    String s="token="+t;
    String x="";
    switch(t) {
      case TT_WORD:
        x=sval;
      break;
      case TT_Q:
        x="QUOTED "+sval;
      break;
      case TT_DQ:
        x="DQUOTED "+sval;
      break;
      case TT_FULLSTOP:
        x="FULLSTOP ";
      break;
      case TT_ERR:
        x="*** error ***";
      break;
      case TT_EOL:
        x="EOL";
      break;
      case TT_EOF:
        x="EOF";
      break;
      default:
        if(isSPACE(t))
          x="SPACE:"+t;
        else
          x="char "+(char)t;
      break;
    
    }
    return x+"                 ====>  "+s;
  }
  
  public static Object ytest(Object fname) {
    if("y".equals(fname))
      fname="ex.pl";
    if("x".equals(fname))
      fname="progs/simple.pro";
    try {
      PreTokenizer T=new PreTokenizer(Interact.safeFileReader(fname.toString()));
      for(;;) {
        int t=T.nextToken();
        if(TT_EOF==t||TT_ERR==t)
          break;
        System.out.println(T.show(t));
      }
    } catch(Exception e) {
      e.printStackTrace();
    }
    return "got:"+fname;
  }
  
  /*
  public static void main(String[] args) {
    ytest("x");
  }
  */
}
