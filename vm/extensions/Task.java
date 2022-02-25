package vm.extensions;

import vm.logic.Fun;
import vm.logic.Interact;
import vm.logic.LogicInteractor;
import vm.logic.Stateful;
import vm.logic.Var;

public class Task implements Stateful,Runnable {
  private static final long serialVersionUID=222L;
  
  private final LogicInteractor I;
  
  /*
   * public Task(Object goal) { this(null,goal); }
   */
  
  public Task(int op,Object bpFile,Object goal){
    this.I=prolog_loop(op,bpFile,goal);
  }
  
  synchronized public void task_bg() {
    LogicThread.newThread(this,"task_bg").start();
  }
  
  public void run() {
    Interact.errmes("Task: run is just a stub, must override!");
  }
  
  public static LogicInteractor prolog_loop(int op,Object bpFile,Object goal) {
    // bpFile becomes default embedded lwam.bp if null
    // i_server starts here
    LogicInteractor I=LogicInteractor.new_interactor0(op,bpFile);
    // I.protect_engine();
    Var R=new Var(0);
    // Interact.dump("I="+I);
    Object answer=I.query_engine(R,new Fun("$prolog_immediately",goal,R));
    if(!(answer instanceof Fun)||!"ok".equals(((Fun)answer).args[0])) {
      Interact.errmes("'$prolog_immediately' failed with goal: "+goal);
      return null;
    }
    answer=I.load_engine(new Fun(":-","done","$prolog_loop"));
    return I;
  }
  
  Object[] askProlog(Object[] bundle) {
    // interacts with independently running '$prolog_loop'
    // Interact.dump("ENTERING askProlog, engine="+I);
    I.setBundle(bundle);
    try {
      // Interact.dump("ask: engine="+I+" ENTERING");
      long t=I.ask();
      // Interact.dump("ask: engine="+I+"====>"+t);
      if(0==t) {
        // we should detect here that it was a Prolog exception
        // thrown inside '$prolog_loop'
        return null;
      }
    } catch(Exception e) {
      Interact.dump("askProlog EXCEPTION="+e);
      // e.printStackTrace();
      return null;
    }
    // Interact.dump("BEFORE I.getBunbdle");
    return I.getBundle();
  }
  /*
  public static void stopProlog(LogicInteractor I) {
    I.stop();
  }
  */
}