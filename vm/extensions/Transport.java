package vm.extensions;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;

import vm.logic.Interact;
import vm.logic.LogicEngine;
import vm.logic.Prolog;
import vm.logic.PrologException;
import vm.logic.Stateful;

/**
 * Transport layer: provides socket based client/server interaction
 * Supports various term encodings - from plain ascii to serialized and encrypted
 * Called though Reflection from Prolog.
 */
public class Transport implements Stateful {
  private static final long serialVersionUID=222L;
  
  /**
   * Client
   */
  public Transport(String host,int port,int try_connect) throws PrologException{
    this.host=host;
    this.port=port;
    if(try_connect>0&&!connect()) {
      throw new PrologException("Client creation failure: host="+host+" port="
          +port);
    }
  }
  
  public Transport(String host,int port) throws PrologException{
    this(host,port,1);
  }
  
  /**
   * Server constructor
   */
  public Transport(int port) throws PrologException{
    this.host=null;
    this.port=port;
    try {
      server_socket=new ServerSocket(port);
    } catch(IOException e) {
      server_socket=null;
    }
    
    if(null==server_socket)
      Interact.errmes("Socket Error: server creation failure: "+port);
    else {
      Interact.println("Server listening on port: "+port);
    }
  }
  
  public static int find_free_port(int basePort,int maxtry) {
    int port=0;
    for(int i=0;i<maxtry;i++) {
      try {
        ServerSocket s=new ServerSocket(basePort+i);
        s.close();
      } catch(IOException e) {
        continue;
      }
      
      port=basePort+i;
      break;
    }
    return port;
  }
  
  public boolean isRunning() {
    return null!=server_socket;
  }
  
  public static Transport newService(Transport server) {
    // Interact.println("Creating new service.");
    
    if(!server.isRunning())
      return null;
    
    Transport service=new Transport(server); // blocking happens here
    if(service.server_socket==null)
      return null;
    
    // Interact.println("Returning new service: "+service);
    return service;
  }
  
  /**
   * Service constructor
   */
  private Transport(Transport T){
    this.server_socket=T.server_socket;
    // Prolog.dump("blocking on server socket: "+server_socket);
    if(accept()) { // here is where we block
      // Prolog.dump("accept ok: unblocking on server socket: "+server_socket);
      return;
    }
    this.server_socket=null;
    if(T.isRunning())
      Interact.errmes("Service creation failure on: "+T.server_socket,null);
  }
  
  public String host;
  
  public int port;
  
  transient protected DataInputStream from;
  
  transient protected DataOutputStream to;
  
  transient protected Socket client_socket;
  
  transient protected ServerSocket server_socket;
  
  static final int OTypeTag=-1; // can be any negative number - more tags can be
  
  // used
  
  /**
    Reads from a socket stream. The data format is compatible with BinProlog's C base
    data format <int length,bytes> while allowing for encrypted or unenecrypted serialized
    objects as a transport mechanism.
  */
  synchronized public Object read_from() {
    Object o=null;
    if(null==from) {
      Interact.warnmes("read_from: null socket exception");
      return o;
    }
    try {
      // Prolog.dump("read_from TRACE available bytes:"+"<"+from.available()+"> on "+client_socket);
      int l=from.readInt();
      if(l>=0)
        o=sread_from(from,l);
      else if(OTypeTag==l)
        o=oread_from(from);
      else {
        Interact.warnmes("Bad int data type in read_from: "+l);
      }
    } catch(EOFException e) {
      // Interact.warnmes("EOF read_from: " + e);
    } catch(SocketException e) {
      Interact.warnmes("SocketException in read_from: "+e);
      // ok, the other side just closed
    } catch(IOException e) {
      Interact.warnmes("IOException (not EOF) in read_from: "+e);
    } catch(ClassNotFoundException e) {
      Interact.warnmes("ClassNotFoundExceptionin read_from: "+e);
    }
    /*catch (Throwable e) {
       Interact.errmes("UNEXPECTED ERROR IN read_from()",e);
    }*/
    Interact.traceln("read_from TRACE:"+"<"+o+">");
    if(null==o)
      disconnect();
    return o;
  }
  
  public void bundle_read(LogicEngine e) {
    Object[] o=(Object[])read_from();
    e.setBundle(o);
  }
  
  public void bundle_write(LogicEngine e) {
    Object[] o=e.getBundle();
    write_to(o);
  }
  
  /**
    Writes to a socket stream.
  */
  synchronized public void write_to(Object o) {
    // if(null==to) // closed by the other side
    // return;
    
    try {
      if(o instanceof String)
        swrite_to(to,(String)o);
      else
        owrite_to(to,o);
    } catch(IOException e) {
      Interact.warnmes("IO Exception in write_to: "+e+"<=="+o);
      disconnect();
    }
    Interact.traceln("write_to TRACE:"+"<"+o+">");
  }
  
  static final private String sread_from(DataInputStream f,int length)
      throws IOException {
    byte bs[]=bread_from(f,length);
    String s=new String(bs,Interact.getEncoding());
    return s;
  }
  
  static final private void swrite_to(DataOutputStream f,String s)
      throws IOException {
    if(null==s)
      return;
    byte[] bs=s.getBytes(Interact.getEncoding());
    f.writeInt(bs.length);
    bwrite_to(f,bs);
  }
  
  public static byte[] file2bytes(String fname) throws IOException {
    File file=new File(fname);
    int l=(int)(file.length());
    DataInputStream f=new DataInputStream(new FileInputStream(file));
    byte[] bs=bread_from(f,l);
    f.close();
    return bs;
  }
  
  public static final byte[] bread_from(DataInputStream f,int length)
      throws IOException {
    byte bs[]=new byte[length];
    f.readFully(bs);
    return bs;
  }
  
  public static final void bwrite_to(OutputStream f,byte[] bs)
      throws IOException {
    f.write(bs);
    f.flush();
  }
  
  /**
   *  Reads a serialized object
   */
  static final private Object oread_from(DataInputStream f) throws IOException,
      ClassNotFoundException {
    Object o=(new ObjectInputStream(f)).readObject();
    // Prolog.dump("READ OBJECT: <"+o+">");
    return o;
  }
  
  /**
   * Writes a serialized object
   */
  static final private void owrite_to(DataOutputStream f,Object o)
      throws IOException {
    f.writeInt(OTypeTag);
    f.flush();
    ObjectOutputStream g=new ObjectOutputStream(f);
    // Prolog.dump("WRITE OBJECT: <"+o+">");
    g.writeObject(o);
    g.flush();
  }
  
  static public boolean compress=true;
  
  static public void compressOn() {
    compress=true;
  }
  
  static public void compressOff() {
    compress=false;
  }
  
  static public boolean toFile(String fname,Object O) {
    try {
      ObjectOutputStream f=(compress)?new ObjectOutputStream(
          new DeflaterOutputStream(new FileOutputStream(fname)))
          :new ObjectOutputStream(new FileOutputStream(fname));
      f.writeObject(O);
      f.close();
      return true;
    } catch(IOException e) {
      Interact.errmes("unable to save object to file: "+fname,e);
      return false;
    }
  }
  
  static public Object fromFile(String fname) {
    try {
      ObjectInputStream f=(compress)?new ObjectInputStream(
          new InflaterInputStream(new FileInputStream(fname)))
          :new ObjectInputStream(new FileInputStream(fname));
      Object O=f.readObject();
      f.close();
      return O;
    } catch(Exception e) {
      Interact.errmes("unable to rebuild object from file: "+fname,e);
      return null;
    }
  }
  
  static protected void traceln(String s) {
    Interact.traceln(s);
  }
  
  /**
     Opens read/write streams of a Connector socket
  */
  protected void open_streams() throws IOException {
    from=new DataInputStream(new BufferedInputStream(
        client_socket.getInputStream()));
    to=new DataOutputStream(new BufferedOutputStream(
        client_socket.getOutputStream()));
  }
  
  /** 
  * disconects a client or service socket
  */
  public void disconnect() {
    if(null!=client_socket) {
      try {
        traceln("disconnecting client_socket: "+client_socket);
        client_socket.close();
        from=null;
        to=null;
        client_socket=null;
      } catch(IOException e) {
        Interact.errmes("disconnect: failing to close socket",e);
      }
    }
  }
  
  /** 
   * discontinues future services on this server_socket
   */
  synchronized public void discontinue() {
    // Prolog.dump("disconnecting server_socket: "+server_socket);
    if(null!=server_socket) {
      try {
        traceln("disconnecting server_socket: "+server_socket);
        server_socket.close();
        from=null;
        to=null;
        server_socket=null;
      } catch(IOException e) {
        Interact.errmes("disconnect: failing to close server socket",e);
      }
    }
  }
  
  /**
    Creates a Client socket. Note that it spends some
    time retrying to connect to a Server.
  */
  static int SRETRY=10;
  
  private boolean create_socket() {
    traceln("Creating socket to host="+host+" port="+port);
    boolean ok=false;
    try {
      for(int i=0;i<SRETRY;i++) {
        try {
          client_socket=new Socket(host,port);
          client_socket.setTcpNoDelay(true);
          // client_socket.setSoLinger(false,0);
          break;
        } catch(BindException e) {
          // workaround to OS bug (on various Windows)
          long waitingTime=1000L*(1<<i);
          Prolog.dump("waiting "+waitingTime
              +"ms for broken OS to close old sockets:"+e);
          try {
            Thread.sleep(waitingTime);
          } catch(InterruptedException ie) {
          }
        }
      }
      ok=true;
    } catch(UnknownHostException e) {
      // Interact.errmes("create_socket: bad host="+host+" or port="+port,e);
    } catch(IOException e) {
      // Interact.errmes("error in create_socket",e);
    }
    if(ok)
      traceln("created client_socket: "+client_socket.toString());
    else
      traceln("client: socket creation failure");
    return ok;
  }
  
  /**
    Opens the read/write streams of a Client socket
  */
  private boolean open_connection() {
    boolean ok=false;
    try {
      open_streams();
      ok=true;
    } catch(IOException e) {
    }
    if(ok) {
      traceln("client connected!");
    } else
      Interact.warnmes("client connection failure");
    return ok;
  }
  
  public boolean connect() {
    boolean ok=create_socket()&&open_connection();
    if(!ok)
      disconnect();
    return ok;
  }
  
  private boolean accept() {
    try {
      if(null==client_socket) {
        client_socket=server_socket.accept();
        open_streams();
      }
    } catch(IOException e) {
      // Interact.warnmes("error accepting on server socket:"+server_socket);
      disconnect();
      return false;
    }
    return true;
  }
  
}
