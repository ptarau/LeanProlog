package vm.extensions;

import vm.logic.Interactor;
import vm.logic.ObjectStack;
import vm.logic.Stateful;

/**
 * An Hub is a device which synchronizes
 * Producers and Consumers. Called through Reflection from Prolog.
 */
public class Hub implements Stateful,Interactor {
  private static final long serialVersionUID=222L;
  
  private boolean stopped;
  
  private Object port;
  
  ObjectStack toStop=new ObjectStack();
  
  public Hub(){
    this.port=null;
    this.stopped=false;
  }
  
  synchronized public Object ask_interactor() {
    while(null==port&&!stopped) {
      try {
        wait();
      } catch(InterruptedException e) {
        if(stopped) {
          // Interact.warnmes("Hub caught "+e);
          break;
        }
      }
    }
    Object result=port;
    port=null;
    notifyAll();
    return result;
  }
  
  synchronized public Object tell_interactor(Object T) {
    while(null!=port&&!stopped) {
      try {
        wait();
      } catch(InterruptedException e) {
        if(stopped) {
          // Interact.warnmes("Hub caught "+e);
          break;
        }
      }
    }
    port=T;
    notifyAll();
    return stopped?null:T;
  }
  
  public Object interactor_handle(Object message) {
    return message;
  }
  
  public void stop_interactor() {
    this.stopped=true;
    while(!toStop.isEmpty()) {
      LogicThread T=(LogicThread)toStop.pop();
      T.stop_interactor();
    }
  }
  
  public boolean is_stopped() {
    return this.stopped;
  }
}
