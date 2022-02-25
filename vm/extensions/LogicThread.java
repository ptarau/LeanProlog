package vm.extensions;

import vm.logic.Interact;
import vm.logic.LogicInteractor;
import vm.logic.Prolog;

public class LogicThread extends LogicInteractor implements Runnable {
  private static final long serialVersionUID=222L;
  
  public static final LogicThread new_interactor(
      Object wamFileOrStreamOrProlog,Hub hub,int clone) {
    LogicThread T=new LogicThread(wamFileOrStreamOrProlog,hub,clone);
    T.protect_engine();
    return T;
  }
  
  public static final ThreadGroup group=new ThreadGroup("logic_thread");
  
  synchronized static final Thread newThread(Runnable R,String info) {
    String name=info+"_"+R.toString();
    Thread T=new Thread(group,R,info+"_"+R.toString());
    T.setDaemon(true);
    // Prolog.dump("starting "+T);
    Interact.log("STARTING Thread "+group.activeCount()+" "+name);
    return T;
  }
  
  private Hub hub;
  
  private Thread myThread;
  
  private LogicThread(Object wamFileOrStreamOrProlog,Hub hub,int clone){
    super(Prolog.newProlog(clone,wamFileOrStreamOrProlog));
    this.hub=hub;
    // Prolog.dump(this.prolog+"<>"+prolog);
  }
  
  // Initiative: client
  // should also contain a static method new_intercator
  // returning an Interactor or null if its private contructor fails
  
  /**
   * Initiative: client
   * data is obtained from this Interactor
   * this usually triggers some computation in this interactor
   * and might involve the interactor calling
   * methods within its initiative
   */
  public Object ask_interactor() {
    // Interact.println("------------>HERE");
    return hub.ask_interactor();
  }
  
  // set up message such that the interactor knows about the hub
  public Object tell_interactor(Object G) {
    // Fun G=new Fun("launch_engine",this.hub,goal);
    if(hub.is_stopped()) {
      return "no";
    }
    Object ok=super.tell_interactor(G);
    if("no".equals(ok))
      return "no";
    Thread T=newThread(this,"tell_interactor_"+G.toString());
    this.myThread=T;
    
    T.start();
    return ok;
  }
  
  /**
   * Initiative: client or the interactor itself
   * Result: 
   *   stops this Interactor
   *   this can discard it or make it available for reuse
   */
  public void stop_interactor() {
    if(null!=myThread) {
      
      myThread.interrupt(); // if caught by Hub - frees others on Hub
      
      stop(); // recovers all engines with this as a root
      endRun();
      myThread.stop(); // the only way to get this right ...
      
    }
    myThread=null;
  }
  
  /**
   * Initiative: this Interactor
   * 
   * Result: 
   *   a(possibly) performs some action on it and
   *   returns some form of acknowledgement
   * 
   *   this method is meant to be private - i.e.
   *   only the interactor should call it
   */
  public Object interactor_handle(Object message) {
    return super.interactor_handle(message);
  }
  
  private final void endRun() {
    this.unprotect_engine();
    prolog.atomTable.logicThreadCount--;
  }
  
  public void run() {
    // see Prolog ... IDENTITY for this, not CLONE !!!
    // prolog.atomTable.logicThreadCount++;
    while(!hub.is_stopped()) {
      Object answer="no";
      try {
        answer=super.ask_interactor();
      } catch(Throwable e) {
        
        // prolog.removeEngine(this);
        stop(); // also stops and removes
        
        if(!hub.is_stopped()) {
          Interact.warnmes("Exception in LogicThread with valid Hub "+this,e);
          hub.tell_interactor("no");
        } else if(e instanceof Exception) {
          Interact.warnmes("Unexpected exception in LogicThread "+this,e);
        }
        
        endRun();
        return; // ends Thread
      }
      // answer=super.ask_interactor();
      if("no".equals(answer))
        break;
      Object maybeError=getException(answer);
      if(null!=maybeError) {
        Interact.warnmes("Exception in LogicThread, bg(..) or bg_clone(..): "
            +maybeError);
        break;
      }
      
      hub.tell_interactor(answer);
    }
    endRun();
    hub.tell_interactor("no");
  }
  
}
