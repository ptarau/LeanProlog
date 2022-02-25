package vm.logic;

import java.io.BufferedReader;

public class LogicInteractor extends LogicEngine implements Interactor {
  private static final long serialVersionUID=222L;
  
  public static LogicInteractor new_interactor() {
    return new_interactor0(Prolog.DEFAULT,Prolog.noProlog);
  }
  
  /**
  * shares symbol table with engine given as argument
  */
  
  public static LogicInteractor new_interactor(LogicInteractor E) {
    return new_interactor0(Prolog.IDENTITY,E.prolog);
  }
  
  /**
   * starts from a preexisting Prolog - typically loaded from a file
   */
  
  public static LogicInteractor new_interactor(int op,Prolog prolog) {
    return new_interactor0(op,prolog);
  }
  
  public static LogicInteractor new_interactor(String initFile) {
    return new_interactor0(Prolog.BPFILE,initFile);
  }
  
  public static LogicInteractor new_interactor(BufferedReader memReader) {
    return new_interactor0(Prolog.STREAM,memReader);
  }
  
  public static LogicInteractor new_interactor0(int op,Object O) {
    Prolog P=Prolog.newProlog(op,O);
    // Prolog P=Prolog.newProlog(O);
    return new LogicInteractor(P);
    // new prolog using <initFile.bp> code file
  }
  
  public final static Object call_engine(LogicInteractor E,Object query) {
    // E.protect_engine();
    Object O=E.query_engine(query);
    // E.unprotect_engine(); // no gc can happen - we are in control !!!
    // Prolog.dump("TOP ENGINE DONE:"+E.prolog.engineTable.o2i(E));
    return O;
  }
  
  public final static Object[] bundle_call(LogicInteractor E,Object[] bundle) {
    Var Ok=new Var(0);
    Fun query=new Fun(":-",Ok,new Fun("bundle_call",Ok));
    E.setBundle(bundle);
    Object answer=call_engine(E,query);
    // Prolog.dump("bundle_call=>"+answer);
    if(answer instanceof Fun) {
      Integer Code=(Integer)((Fun)answer).args[0];
      if(Code.intValue()>0)
        return E.getBundle();
    }
    return null;
  }
  
  /**
   * Calls and collects first answer
   */
  public final static Object call_prolog(Prolog P,Object query) {
    LogicInteractor E=new LogicInteractor(P);
    Object O=call_engine(E,query);
    // Prolog.dump("TOP ENGINE DONE:"+E.prolog.engineTable.o2i(E));
    // E.unProtect();
    E.stop(); // ok to stop - creates new each time -- also unprotects
    return O;
  }
  
  final public TermConverter termConverter;
  
  Object lastQuery=null;
  
  LogicEngine cloneLogicEngine() {
    return new LogicInteractor(this.prolog);
  }
  
  public LogicInteractor(Prolog prolog){
    super(prolog);
    termConverter=new TermConverter(this);
  }
  
  /**
   * Overrides - to use termConverter from terms of type Fun, Var etc.
   */
  final long queryTerm(Object query) throws PrologException {
    if(query instanceof EncodedTerm)
      return super.queryTerm(query);
    else
      return termConverter.toInternal(query);
  }
  
  /**
   * Expects (X:-G), returns the(Answer) or no
   */
  final public Object query_engine(Object X,Object G) {
    Fun query=new Fun(":-",X,G);
    return query_engine(query);
  }
  
  final public Object query_engine(Object query) {
    this.lastQuery=query;
    Object answer="no";
    if(load_engine(query))
      answer=ask_interactor();
    return answer;
  }
  
  public static Object getException(Object answer) {
    if(!(answer instanceof Fun))
      return null;
    Object arg=((Fun)answer).args[0];
    if(!(arg instanceof Fun))
      return null;
    Fun exc=(Fun)arg;
    // Prolog.dump("HERE==>"+exc);
    if(!"exception".equals(exc.name))
      return null;
    return exc.args[0];
  }
  
  public final boolean load_engine(Object query) {
    // Prolog.dump(this+"=>load_engine: "+query);
    try {
      INIT_INTERP(query);
      return true;
    } catch(PrologException e) {
      Interact.errmes("error in load_engine():"+this,e);
      stop();
      return false;
    }
  }
  
  public Object ask_interactor() {
    long t=0;
    try {
      t=ask();
    } catch(PrologException e) {
      // ok: t==null
      Interact.warnmes("ask_interactor: PrologException = "+e);
    }
    if(0==t)
      return "no";
    
    Fun T=new Fun("the",termConverter.toExternal(t));
    
    // Prolog.dump("here!!! "+T);
    return T;
  }
  
  public Object tell_interactor(Object XG) {
    if(load_engine(XG))
      return "yes";
    else
      return "no";
  }
  
  // not meant to be called
  public Object interactor_handle(Object message) {
    Prolog.dump("THE ENGINE DOES IT: interactor_handle");
    return null;
  }
  
  public void stop_interactor() {
    stop();
  }
  
  final void cwrite(long xref) {
    Object O=termConverter.toExternal(xref);
    Interact.print(O.toString());
  }
  
  final void cnl() {
    Interact.println("");
  }
  
  /**
   * returns term+vars of the form T-Vs. Note that string should end with "."
   */
  public Object atom_to_term(String S) {
    Var T=new Var(0);
    Var Vs=new Var(1);
    Fun answer=new Fun("-",T,Vs);
    Fun goal=new Fun("atom_to_term",S,T,Vs);
    load_engine(new Fun(":-",answer,goal));
    Object R=ask_interactor();
    if(!(R instanceof Fun))
      return null;
    return ((Fun)R).args[0];
  }
  
  /**
   * returns string representation of T using write
   */
  public String term_to_atom(Object T) {
    Var A=new Var(0);
    Fun goal=new Fun("term_to_atom",T,A);
    load_engine(new Fun(":-",A,goal));
    Object R=ask_interactor();
    if(!(R instanceof Fun))
      return null;
    return (String)(((Fun)R).args[0]);
  }
  
  /**
   * returns a reader using the Prolog parser
   */
  public LogicInteractor openReader(String fname) {
    LogicInteractor E=LogicInteractor.new_interactor(this);
    Var A=new Var(0);
    Fun goal=new Fun("term_of",fname,A);
    E.load_engine(new Fun(":-",A,goal));
    return E;
  }
  
  /**
   * returns a writer using the Prolog writer
   */
  public LogicInteractor openWriter(String fname) {
    LogicInteractor E=LogicInteractor.new_interactor(this);
    Var A=new Var(0);
    Fun goal=new Fun("term_of",fname,A);
    E.load_engine(new Fun(":-",A,goal));
    return E;
  }
  
  /*
     reads a term using termOf 
  */
  
  public Object readTerm() {
    Object R=ask_interactor();
    if(!(R instanceof Fun))
      return null;
    return ((Fun)R).args[0];
  }
  
  // override
  final long xcall(int op,long input) {
    // if(this.isStopped) return 0;
    Object I;
    
    if(op<0) { // -71, -72
      I=ref2bundle(input);
    } else {
      I=termConverter.toExternal(input);
    }
    Object O=null; // "xcall_error";
    
    O=Prolog.extender.xcall(Math.abs(op),I,this);
    
    // Prolog.dump("?????????XCALL: "+I);
    // Prolog.dump("!!!!!!!!!XCALL: "+O+",this="+this);
    
    // if(null!=O && (O instanceof java.math.BigInteger))
    // Prolog.dump("Output="+O+":"+O.getClass());
    try {
      long o;
      
      // if(op<0) {
      // Interact.dump("op="+op+", class="+O+":"+O.getClass());
      // if(!(O instanceof Object[]))
      // Interact.dump("    Output="+O);
      // }
      
      if(op<0&&O instanceof Object[]) {
        o=bundle2ref((Object[])O);
        if(0==o)
          o=termConverter.toInternal(AtomTable.Nothing);
      } else {
        // Interact.dump("EXITING xcall: "+I+"=>"+O);
        o=termConverter.toInternal(O);
      }
      return o;
    } catch(PrologException e) {
      Interact.errmes("unable to internalize result, op="+op,e);
      return 0L;
    } catch(Exception e) {
      if(!isStopped)
        Interact.errmes("error in xcall: "+input);
      return 0L;
    }
  }
  
  public String toString() {
    /*String s="";
     if(null!=lastQuery)
      s="q="+lastQuery;*/
    return "EXT:"+super.toString(); // +"=>"+s;
  }
}
