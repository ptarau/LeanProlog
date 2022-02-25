package vm.extensions;

import vm.logic.Interact;
import vm.logic.Prolog;

/**
 * Emulates socket based client/server interaction. Except that it happens 
 * all between internal threads, synchronize using 2 Hub object instead of 
 * sockets.
 */

public class InnerTask extends Task {
  private static final long serialVersionUID=222L;
  
  private Hub serverQ;
  
  public InnerTask(int op,Object bpFile,Object goal){
    super(op,bpFile,goal);
    this.serverQ=new Hub();
  }
  
  public void run() {
    // Interact.errmes("Task: run is just a stub, must override!");
    
    while(serverStep())
      ;
    // Prolog.dump("SERVER_STOPPED: "+this);
  }
  
  public static Object EOT=new Object(); // stop service
  
  public static Object EOS=new Object(); // stop server
  
  synchronized private boolean serverStep() {
    ServicePair service=new ServicePair();
    this.serverQ.tell_interactor(service);
    
    try {
      for(;;) {
        Object in=service.fromClient.ask_interactor();
        // Prolog.dump("THREAD SERVER GOT: "+in+":>"+in.getClass());
        if(EOT==in) { // client disconnected
          service.clear();
          // Prolog.dump("CLIENT THREAD DISCONNECTED: "+this);
          return true; // disconnect and keep going
        }
        
        // this should never happen
        // signaling is done by sendong "stop" to the server
        if(EOS==in) { // server got end of stream
          service.clear();
          this.serverQ.tell_interactor(EOS);
          this.serverQ.stop_interactor();
          this.serverQ=null;
          Prolog.dump("THREAD SERVER got EOS: "+this);
          return false;
        }
        
        Object[] bundle=(Object[])in;
        Object[] bundle0=bundle;
        
        // Interact.dump("THREAD SERVER GOT bundle>>>: =>"+bundle.getClass());
        bundle=askProlog(bundle);
        // Interact.dump("AFTER askProlog"+bundle);
        // should tell something else,
        // not null? client hangs ...
        if(null==bundle) { // server got "stop"
          service.toClient.tell_interactor(bundle0);
          
          this.serverQ.tell_interactor(EOS);
          this.serverQ.stop_interactor();
          service.clear();
          this.serverQ=null;
          // Prolog.dump("THREAD SERVER got null: "+bundle);
          return false;
        }
        service.toClient.tell_interactor(bundle);
      }
    } catch(Exception e) {
      e.printStackTrace();
      return false;
    }
  }
  
  public static Object ask_server(ServicePair service,Object query) {
    try {
      // Prolog.dump("CLIENT SENDING: "+query+ "=>"+query);
      service.fromClient.tell_interactor(query);
      // Prolog.dump("CLIENT SENT: "+query+ "=>"+query);
      
      Object in=service.toClient.ask_interactor();
      // Prolog.dump("CLIENT GOT: "+in+ "=>"+in.getClass());
      // if(in instanceof Object[]) Prolog.dump("HERE "+new
      // PortableTerm((Object[])in,0));
      // if(null==in)
      // return EOS;
      return in;
    } catch(NullPointerException n) {
      // ok - server shut down
      return EOS;
    } catch(Exception e) {
      Interact.errmes("error in ask_local_server on: "+query.getClass(),e);
      return null;
    }
  }
  
  public ServicePair newConnection() {
    ServicePair P=(ServicePair)serverQ.ask_interactor();
    return P;
  }
}

class ServicePair {
  
  ServicePair(){
    this.fromClient=new Hub();
    this.toClient=new Hub();
  }
  
  void disconnect() {
    this.fromClient.tell_interactor(InnerTask.EOT);
  }
  
  void stop() {
    this.fromClient.tell_interactor(InnerTask.EOS);
  }
  
  void clear() {
    this.fromClient=null;
    this.toClient=null;
  }
  
  Hub fromClient;
  
  Hub toClient;
}
