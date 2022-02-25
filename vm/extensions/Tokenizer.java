package vm.extensions;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;

import vm.logic.AtomDict;
import vm.logic.Fun;
import vm.logic.Interact;
import vm.logic.ObjectStack;
import vm.logic.Var;

public class Tokenizer extends PreTokenizer {
  private static final long serialVersionUID=222L;
  
  ObjectStack S;
  
  LineNumberReader R;
  
  public static Tokenizer newTokenizer(Object fname) {
    
    try {
      Reader R;
      if(fname instanceof Reader) {
        R=(Reader)fname;
      } else if(fname instanceof String) {
        R=Interact.safeFileReader((String)fname);
      } else {
        Fun F=(Fun)fname;
        String S=(String)F.getArg(1);
        if(null!=S&&S.length()>0&&!S.endsWith(".")&&!S.endsWith(". "))
          S=S+".";
        R=new StringReader(S);
      }
      Tokenizer T=new Tokenizer(new LineNumberReader(R));
      return T;
      
    } catch(IOException e) {
      e.printStackTrace();
      return null;
    }
  }
  
  private final static Integer VAR0=new Integer(0);
  
  private final static int MAXVAR=1<<24;
  
  private final static String UNDER="_";
  
  private Tokenizer(LineNumberReader R) throws IOException{
    super(R);
    this.R=R;
  }
  
  public int lineno() {
    return R.getLineNumber();
  }
  
  private static Object sym(Object O) {
    return O.toString();
  }
  
  private static Object var(Object O,AtomDict Vs) {
    // return new Fun("var",O);
    int o=Vs.o2i(O);
    if(o<=0) {
      Vs.put(O,VAR0);
      
      o=Vs.o2i(O);
    } else {
      Integer Ctr=(Integer)Vs.get(O);
      int ctr=Ctr.intValue();
      Vs.put(O,new Integer(ctr+1));
    }
    if(UNDER.equals(O)) {
      Integer Ctr=(Integer)Vs.get(O);
      int ctr=Ctr.intValue();
      o=MAXVAR+ctr;
    }
    return new Fun("var",new Var(o));
  }
  
  private static final Object num(Object O) {
    return new Fun("num",O);
  }
  
  public static final Object str(Object O) {
    return new Fun("str",O);
  }
  
  private static final Object qtd(Object O) {
    return new Fun("qtd",O);
  }
  
  private static final Object classify(String X,AtomDict Vs) {
    if(X.length()>0) {
      char c=X.charAt(0);
      if(Character.isUpperCase(c)||'_'==c)
        return var(X,Vs);
      // String - also for numbers as they can be arbitrary length
      else if(Character.isDigit(c)||'-'==c||'+'==c)
        return num(X);
    }
    return sym(X);
  }
  
  public Object getClauseTokens() {
    if(TT_EOF==ttype)
      return null;
    int line1=this.lineno();
    boolean ok=true;
    AtomDict Vs=new AtomDict();
    this.S=new ObjectStack(); // collects tokens before full stop
    try {
      boolean go=true;
      while(go&&ok) {
        int t=nextToken();
        // Prolog.dump(S+":"+lineno()+":  "+show(t));
        switch(t) {
          case TT_FULLSTOP:
            // Prolog.dump("<<<"+S+":"+lineno()+":  "+show(t)+">>>");
            Object C=S.peek();
            
            if(C instanceof Character) {
              
              char c=((Character)C).charValue();
              
              // Prolog.dump("getClauseTokens: <<<"+(int)c+">>>");
              
              if("\n\t (){}[]!'\"".indexOf(c)>=0)
                go=false;
              else {
                S.push(DOT);
              }
            } else
              go=false;
          // Prolog.dump("getClauseTokens go: <<<"+go+">>>");
          break;
          case TT_EOF:
          case TT_ERR:
            ok=false;
          break;
          case TT_WORD:
            S.push(sval);
          break;
          case TT_Q:
            // S.push(Q);
            // S.push(sval);
            // S.push(Q);
            S.push(qtd(sval));
          break;
          case TT_DQ:
            // S.push(DQ);
            // S.push(sval);
            // S.push(DQ);
            S.push(str(sval));
          break;
          default:
            S.push(new Character((char)t));
        }
      }
    } catch(IOException e) {
      e.printStackTrace();
      ok=false;
    }
    
    ObjectStack Ts=aggregate(S,Vs);
    
    this.S=null;
    if(!ok) {
      if(!Ts.isEmpty()) {
        Interact
            .warnmes("Tokenizer ERROR WHILE EXPECTING END OF A CLAUSE at line="
                +lineno()+", tokens so far:\n==>"+Ts);
      }
      return null;
    } else {
      ObjectStack Xs=getVars(Vs);
      int line2=this.lineno();
      // Prolog.dump("lineno="+line1+":"+line2);
      return new Fun("between",new Integer(line1),new Integer(line2),
          Ts.toList(),Xs.toList());
      // RECLIMIT => ~1000
    }
  }
  
  private final ObjectStack aggregate(ObjectStack Xs,AtomDict Vs) {
    ObjectStack Ys=new ObjectStack();
    StringBuffer s=new StringBuffer();
    
    for(int i=0;i<Xs.size();i++) {
      Object X=Xs.at(i);
      if(X instanceof Fun) {
        s=closeBuffer(s,Ys,"sym",false); // spec
        Ys.push(X);
      } else if(X instanceof String) {
        s=closeBuffer(s,Ys,"sym",false); // spec
        Ys.push(classify((String)X,Vs));
      } else {
        // non-sticky solo-chars
        char c=((Character)X).charValue();
        if(Character.isWhitespace(c))
          c=' ';
        switch(c) {
          case '!':
          case ';':
          case '|':
          case ',':
            s=endWithUnstiky(X,s,Ys);
          break; // String
          
          case '(':
          case '[':
          case '{':
            s=endWithPar(X,s,Ys);
          break; // Character
          
          case ')':
          case ']':
          case '}':
            s=endWithPar(X,s,Ys);
          break; // Character
          
          case ' ': {
            Object P=at(Xs,i+1); // Object U=at(Xs,i-1);
            
            s=closeBuffer(s,Ys,"sym",false);
            if(CharLPAR.equals(P)) {
              Ys.push(OpLPAR);
              i++;
            }
          }
          break;
          case '-':
          case '+': {
            Object SP=at(Xs,i-1);
            Object N1=at(Xs,i+1);
            Object D=at(Xs,i+2);
            Object N2=at(Xs,i+3);
            
            if(isNUM(SP)&&isNUM(N1)&&isSCI(SP)) {
              // "3E-10"
              Ys.pop();
              Ys.push(classify(""+SP+c+N1,Vs));
              i++;
            } else if(SPACE.equals(SP)&&isSCI(N1)&&isNUM(N1)&&isNUM(N2)) {
              // " -3E-5" D=+,-
              Ys.push(classify(""+c+N1+D+N2,Vs));
              i+=3;
            } else if(SPACE.equals(SP)&&DOT.equals(D)&&isNUM(N1)&&isNUM(N2)) {
              // " 3.4E-5"
              if(isSCI(N2)) {
                // Interact.dump("SCI1=>"+N2);
                Object sig=at(Xs,i+4);
                Object exp=at(Xs,i+5);
                Ys.push(classify(""+c+N1+D+N2+sig+exp,Vs));
                i+=5;
              } else {
                Ys.push(classify(""+c+N1+D+N2,Vs));
                i+=3;
              }
            } else if(SPACE.equals(SP)&&isNUM(N1)) {
              Ys.push(classify(""+c+N1,Vs));
              i++;
            }
            
            else
              // sticky symbol-char
              s.append(c);
            
          }
          break;
          case '.': {
            Object Pred=at(Xs,i-1);
            Object Succ=at(Xs,i+1);
            if(isNUM(Pred)&&isNUM(Succ)) {
              if(isSCI(Succ)) {
                // Interact.dump("SCI2=>"+Succ);
                Object sig=at(Xs,i+2);
                Object exp=at(Xs,i+3);
                Ys.pop();
                String Y=Pred+"."+Succ+sig+exp;
                Ys.push(classify(Y,Vs));
                i+=3;
              } else {
                Ys.pop();
                String Y=Pred+"."+Succ;
                Ys.push(classify(Y,Vs));
                ++i;
              }
            } else {
              // accumulate this "."
              s.append(X);
            }
          }
          break;
          default:
            s.append(c);
        }
      }
    }
    return Ys;
  }
  
  final static Character DOT=new Character('.');
  
  final static Character SPACE=new Character(' ');
  
  final static Character CharLPAR=new Character('(');
  
  final static Object OpLPAR="~(";
  
  final public static Object at(ObjectStack Xs,int i) {
    if(i>=0&&i<Xs.size())
      return Xs.at(i);
    else
      return null;
  }
  
  private static final StringBuffer endWithUnstiky(Object X,StringBuffer s,
      ObjectStack Ys) {
    s=closeBuffer(s,Ys,"sym",false);// spec
    Ys.push(sym(X.toString())); // single Character
    return s;
  }
  
  private static final StringBuffer endWithPar(Object X,StringBuffer s,
      ObjectStack Ys) {
    s=closeBuffer(s,Ys,"sym",false);// spec
    // Object Par=new Fun(par,X.toString()); // Prolog assumes this
    Object Par=X;
    Ys.push(Par.toString()); // single Character par
    return s;
  }
  
  private static final StringBuffer closeBuffer(StringBuffer s,ObjectStack Ys,
      String tag,boolean always) {
    if(always||s.length()>0) {
      String X=s.toString(); // .trim();
      if(0==X.length())
        return s;
      Object Y;
      if("str".equals(tag))
        Y=str(X); // list of codes
      else if("qtd".equals(tag))
        Y=qtd(X); // quoted
      else
        Y=sym(X.trim()); // const
      Ys.push(Y);
      s=new StringBuffer();
    }
    return s;
  }
  
  private static final boolean isNUM(Object X) {
    if(null==X||!(X instanceof String))
      return false;
    String x=(String)X;
    if(0==x.length())
      return false;
    char c=x.charAt(0);
    return Character.isDigit(c); // || '-'==c || '+'==c;
  }
  
  private static final boolean isSCI(Object X) {
    if(null==X||!(X instanceof String))
      return false;
    String x=(String)X;
    if(0==x.length())
      return false;
    char c=x.charAt(x.length()-1);
    return c=='E'||c=='e';
  }
  
  private static final ObjectStack getVars(AtomDict Vs) {
    // Object[] names=Vs.toKeys();
    // Object[] occs=Vs.toValues();
    Iterator I=Vs.getKeys();
    ObjectStack R=new ObjectStack();
    // for(int i=0;i<names.length;i++) {
    // R.push(new Fun("var",names[i],new Var(i),occs[i]));
    // }
    
    while(I.hasNext()) {
      Object name=I.next();
      int i=Vs.o2i(name);
      R.push(new Fun("var",name,new Var(i),Vs.get(name)));
    }
    return R;
  }
  
  public static Object xtest(Object fname) {
    if("x".equals(fname))
      fname="progs/optest.pro";
    if("y".equals(fname))
      fname="progs/simplest.pro";
    if("z".equals(fname))
      fname="progs/pbm.pl";
    Tokenizer T=newTokenizer(fname.toString());
    for(;;) {
      Object X=T.getClauseTokens();
      System.out.println("=>"+X);
      System.out.println("-------end tokens---------");
      if(TT_EOF==T.ttype)
        break;
    }
    return "got:"+fname;
  }
}
