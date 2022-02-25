package vm.extensions;

import vm.logic.Interact;

public class OuterTask extends Task {
  private static final long serialVersionUID=222L;
  
  final int port;
  
  // final int bg_service;
  
  public OuterTask(int port,int op,Object bpFile,Object goal){
    super(op,bpFile,goal);
    this.port=port;
    // this.bg_service=bg_service;
  }
  
  public void run() {
    // Interact.dump("SOCKET SERVER RUNNING AT: "+port);
    try {
      Transport server=new Transport(port);
      while(server.isRunning()) {
        Transport service=Transport.newService(server);
        if(null==service) {
          if(server.isRunning())
            Interact.errmes("error creating new service on port: "+port);
          break;
        }
        OuterService outer=new OuterService(server,service,this);
        if(!outer.serviceStep())
          break;
        /*if(bg_service>0)
          outer.bg_run();
        else
          outer.run();
        */
      }
    } catch(Exception e) {
      Interact.errmes("error creating server on port: "+port);
    }
  }
  
  public static Transport connection(String host,int port,int quickFail) {
    try {
      return new Transport(host,port);
    } catch(Exception e) {
      if(quickFail>0) {
        Interact.errmes("error in creating client on host="+host+",port="+port
            +"=>"+e);
      }
      return null;
    }
  }
  
  /*
   * done by the client
   */
  public static Object ask_server(Transport service,Object query) {
    try {
      service.write_to(query);
      // Interact.dump("CLIENT SENT: "+query+"=>"+query.getClass());
      Object in=service.read_from();
      // Interact.dump("CLIENT GOT: "+in+"=>"+in.getClass());
      // if(in instanceof Object[]) Interact.dump("HERE "+new
      // PortableTerm((Object[])in,0));
      
      return in;
    } catch(Exception e) {
      Interact.errmes("error in ask_server on host="+service.host+",port="
          +service.port+" on: "+query.getClass(),e);
      return null;
    }
  }
  
}
