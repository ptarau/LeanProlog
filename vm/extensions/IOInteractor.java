package vm.extensions;

import java.io.IOException;

import vm.logic.AtomTable;
import vm.logic.Fun;
import vm.logic.Interact;
import vm.logic.Interactor;
import vm.logic.ObjectMap;
import vm.logic.ObjectStack;

public class IOInteractor implements Interactor {
  private static final long serialVersionUID=222L;
  
  public static Interactor new_interactor(Object initiator) {
    // Prolog.dump("IOInteractor: "+initiator);
    return new IOInteractor(initiator);
  }
  
  public static final int READLN=0;
  
  public static final int READTOKS=1;
  
  public static final int WRITE=2;
  
  public static final int NL=3;
  
  public static final int PUT_CODE=4;
  
  public static final int GET_CODE=5;
  
  public static final int READWORDS=6;
  
  public final Object initiator;
  
  // private
  Object message;
  
  public IOInteractor(Object initiator){
    this.initiator=initiator;
  }
  
  static final Fun validate(Object message) {
    Fun cmd=(Fun)message;
    if(!"$cmd".equals(cmd.name)) {
      Interact.errmes("ConsoleIO interactor_handle: bad message="+message);
      return null;
    }
    return cmd;
  }
  
  static final int getOp(Fun cmd) {
    return ((Integer)cmd.args[0]).intValue();
  }
  
  static final Object getParam(Fun cmd) {
    return cmd.args[1];
  }
  
  // client side
  
  /**
   * this method is called by the client and will return whatever this
   * Interactor holds
   */
  public Object ask_interactor() {
    
    // System.out.println("### ask_interactor !!!! ="+message);
    Object mes=interactor_handle(this.message);
    this.message=null;
    return mes;
  }
  
  /**
   * this method is called by the client and will configure this interactor to
   * read something if message is null and write it out otherwise
   */
  public Object tell_interactor(Object message) {
    // System.out.println("\n$$$ message="+message+":"+this);
    this.message=message;
    // maybe also notify a logger about last line read
    // or accumulate various messages in an ObjectStack
    return null;
  }
  
  public void stop_interactor() {
    this.message=null;
  }
  
  // interactor side
  
  static void err(String s) {
    Interact.errmes(s);
  }
  
  public void println(Object O) throws IOException {
    err("IOInteractor: unexpected=println=>"+O);
  }
  
  public void print(Object O) throws IOException {
    err("IOInteractor: unexpected=print=>"+O);
  }
  
  public void prompt() throws IOException {
    err("IOInteractor: unexpected=prompt");
  }
  
  public String readln() throws IOException {
    err("IOInteractor: unexpected=readln");
    return null;
  }
  
  public int read() throws IOException {
    err("IOInteractor: unexpected=read");
    return -1;
  }
  
  /**
   * This is not meant to be called by the client, but implemented by this
   * interactor to handle various messages
   */
  public Object interactor_handle(Object message) {
    // System.out.println("!!! got !!!! ="+message);
    try {
      Fun cmd=validate(message);
      
      // $cmd handled here
      int op=getOp(cmd);
      
      switch(op) {
        case READLN: { // readln
          if(cmd.args.length<=1)
            prompt();
          else {
            String altPrompt=cmd.args[1].toString();
            if(altPrompt!=null&&altPrompt.length()>0)
              print(altPrompt);
          }
          String line=readln();
          if(null==line)
            return AtomTable.Nil;
          // if("".equals(line))
          // return "no"; // AtomTable.Nil;
          if(!"".equals(line)&&line.endsWith("."))
            line=line+" ";
          return string2codes(line);
        }
        case READTOKS: {// tokens
          prompt();
          String line=readln();
          if(null==line) {
            Interact.halt(0);
            return AtomTable.Nil;
          }
          // Prolog.dump("read_toks: <"+line+">");
          int i=line.lastIndexOf(".");
          if(i<line.length()-2)
            line=line+". ";
          Tokenizer T=Tokenizer.newTokenizer(new Fun("$str",line));
          
          Object Ts=T.getClauseTokens();
          // Prolog.dump("read_toks:"+Ts);
          if(null==Ts)
            return new Fun("between",new Integer(0),new Integer(0),
                AtomTable.Nil);
          return Ts;
        }
        case WRITE: // write
          print(cmd.args[1]);
        break;
        case NL: {
          println(""); // nl
        }
        break;
        case PUT_CODE: {
          char c=(char)((Integer)cmd.args[1]).intValue();
          print(""+c);
        }
        break;
        
        case GET_CODE: { // read one char code
          // prompt();
          int c=read();
          return new Integer(c);
        }
        
        case READWORDS: {// tokens
          // prompt();
          String line=readln();
          if(null==line)
            return AtomTable.Nil;
          
          // RECLIMIT => ~1000
          WordReader T=WordReader.newWordReader(new Fun("$str",line));
          ObjectStack S=new ObjectStack();
          Object W;
          while(null!=(W=T.getWord())) {
            S.push(W);
          }
          return S.toList();
        }
        /*
        case WRITE_CODES: // $cmd for writeln can be improved !!!
          print(cmd.args[1]);
        break;
        */
        default:
          Interact.errmes("ConsoleIO interactor_handle: bad message="+message);
      }
    } catch(IOException e) {
      Interact.errmes("interactor_handle",e);
    }
    
    return AtomTable.Nothing;
  }
  
  public Object getInitiator() {
    return this.initiator;
  }
  
  public String toString() {
    return initiator+"_"+getClass().getName()+"_"+hashCode();
    // return getClass().getName()+hashCode();
  }
  
  public static Object string2codes(String line) {
    if(null==line)
      return "no";
    return new Fun("the",new Fun("$string",line));
  }
  
  private static final ObjectMap stream2aliasMap=new ObjectMap();
  
  private static final ObjectMap alias2streamMap=new ObjectMap();
  
  public static void set_alias(Object Stream,Object Alias) {
    stream2aliasMap.put(Stream,Alias);
    alias2streamMap.put(Alias,Stream);
  }
  
  public static void remove_alias(Object StreamOrAlias) {
    Object A=stream2aliasMap.get(StreamOrAlias);
    Object S=alias2streamMap.get(StreamOrAlias);
    if(null==S&&null!=A)
      S=alias2streamMap.get(A);
    else if(null==A&&null!=S)
      A=stream2aliasMap.get(S);
    if(null!=S)
      stream2aliasMap.remove(S);
    if(null!=A)
      alias2streamMap.remove(A);
  }
  
  public static Object stream2alias(Object S) {
    return XBuiltins.the(stream2aliasMap.get(S));
  }
  
  public static Object alias2stream(Object A) {
    return XBuiltins.the(alias2streamMap.get(A));
  }
}
