package vm.extensions;

import vm.logic.Interact;

public class OuterService {
  private static final long serialVersionUID=222L;
  
  private Transport server;
  
  private Transport service;
  
  private OuterTask task;
  
  public OuterService(Transport server,Transport service,OuterTask task){
    this.server=server;
    this.service=service;
    this.task=task;
  }
  
  /*
  public void run() {
    // if(!
    serviceStep();
    // )
    // Interact.warnmes("service failed on port="+task.port);
  }
  
  public void bg_run() {
    Thread T=LogicThread.newThread(this);
    T.start();
    Prolog.dump("OuterService.bg_run, started: "+T);
  }
  */

  boolean serviceStep() {
    try {
      for(;;) {
        
        Object[] bundle=(Object[])service.read_from();
        // Interact.dump("SOCKET SERVER GOT: "+bundle+"=>"+bundle.getClass());
        bundle=task.askProlog(bundle);
        // Prolog.dump("SOCKET SERVER COMPUTES =>"+bundle);
        service.write_to(bundle);
        if(null==bundle) { // server got stop
          // Prolog.dump("SOCKET SERVER null bundle");
          service.disconnect();
          server.discontinue();
          // Prolog.dump("SOCKET SERVER STOPS:!!! "+!server.isRunning());
          return false;
        }
        // Interact.dump("SOCKET SERVER SENDS: "+bundle+"=>"+bundle.getClass());
      }
    } catch(NullPointerException e) {
      // this is ok - in means the client closed socket
      // Prolog.dump("CLIENT CLOSE SOCKET");
    } catch(Exception e) {
      Interact.errmes("serviceStep failed",e);
      return false;
    }
    return true;
  }
  
}
