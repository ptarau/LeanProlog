package vm.logic;

import java.lang.management.ManagementFactory;
import java.util.List;

/**
 * Shows examples of calls form Java to Prolog.
 */
public class Start implements Stateful {
  
  private static final long serialVersionUID=222L;
  
  /**
   * this class embeds a LogicInteractor into your application
   */
  
  /*
   * Main entry point
   */
  
  // Start.java
  
  public static void main(String args[]) {
    
    // System.out.println("REFACTORED WITH LONGS VERSION 1.0");
    
    LogicInteractor E=initProlog(args);
    
    for(;;) {
      Object goal=new Fun(":-","ok","toploop");
      // Object answer=
      LogicInteractor.call_engine(E,goal);
      // Interact.println("Prolog RETURNED:"+answer);
      if((null==Interact.argStack)||Interact.argStack.isEmpty())
        break;
      E.stop();
      E=LogicInteractor.new_interactor(Interact.PROLOG_BYTECODE_FILE);
    }
    Interact.halt(0);
  }
  
  static boolean done=false;
  
  public static void initEnv() {
    if(!done) {
      List inputArgs=ManagementFactory.getRuntimeMXBean().getInputArguments();
      System.out.println("Java VM called with: "+inputArgs);
      Interact.log("\nSTARTING jvm with ARGS="+inputArgs);
      // this is, unfortunately, ignored by the jvm ...
      System.setProperty("file.encoding","UTF-8");
    }
    done=true;
  }
  
  public static LogicInteractor initProlog(String[] args) {
    initEnv();
    
    Interact.args=args;
    Interact.argStack=Interact.extractCmds(args);
    Interact.log("START Prolog Process: "+Interact.argStack);
    Interact.argStack.reverse();
    if(args!=null&&args.length>0) {
      if(args[0].endsWith(".bp")) {
        Interact.PROLOG_BYTECODE_FILE=args[0];
      } else if(args[0].endsWith(".jar")) {
        Interact.PROLOG_JAR_FILE=args[0];
      }
    }
    Interact.println("Starting from "+Interact.PROLOG_JAR_FILE+":"
        +Interact.PROLOG_BYTECODE_FILE+"\nMaxCores="+Interact.max_cores()
        +", MaxMemory="+Interact.available_memory()+"MB, MAXSYMS="
        +AtomDict.MAXSYMS+", MAXARITY="+Defs.MAXARITY+".");
    Interact.println("Default JVM character set is "
        +java.nio.charset.Charset.defaultCharset()+", our default is "
        +Interact.getEncoding()+".");
    
    LogicInteractor E=LogicInteractor
        .new_interactor(Interact.PROLOG_BYTECODE_FILE);
    
    // these 2 actions emulate what the C side does
    Fun goal=new Fun(":-",new Var(1),new Fun("prolog_init",new Var(1)));
    LogicInteractor.call_engine(E,goal);
    
    // eval_cmd_line_args(E); // done in xtop.pl
    
    return E;
  }
  /*
  public static void eval_cmd_line_args(LogicInteractor E) {
    String[] args=Interact.args;
    if(null==args)
      return;
    
    Fun goal;
    for(int i=0;i<args.length;i++) {
      String arg=args[i];
      
      if(i==0&&(arg.endsWith(".bp")||arg.endsWith(".jar")))
        continue;
      
      if(!arg.endsWith("."))
        arg=arg+".";
      Var R=new Var(1);
      goal=new Fun(":-",R,new Fun("call_goal_string",arg,R));
      
      LogicInteractor.call_engine(E,goal);
      
    }
  }
  */
  
  /*
  private static void simpleTop(LogicInteractor E) {
    vm.extensions.ConsoleIO io=(vm.extensions.ConsoleIO)vm.extensions.ConsoleIO
        .new_interactor(">> ");
    try {
      for(;;) {
        io.prompt();
        String line=io.readln();
        Object Line=E.atom_to_term(line);
        io.println("Line=> "+Line);
        Fun TVs=(Fun)Line;
        Fun goal;
        if(TVs.args[0] instanceof String) {
          goal=new Fun(":-","yes",TVs.args[0]);
        } else {
          Fun T=(Fun)TVs.args[0];
          if(TVs.args[1] instanceof Fun) {
            Fun Vs=(Fun)TVs.args[1];
            goal=new Fun(":-",Vs,T);
          } else
            goal=new Fun(":-","yes",T);
        }
        Object answer=LogicInteractor.call_engine(E,goal);
        if(!(answer instanceof Fun)) {
          io.println(answer); // get stuff from Shell
          continue;
        }
        Fun R=(Fun)answer;
        io.println("Result=> "+R.args[0]);
        // for(int i=0;R.args.length()) {
        // }
        
      }
    } catch(Exception e) {
      e.printStackTrace();
    }
  }
  */
}
