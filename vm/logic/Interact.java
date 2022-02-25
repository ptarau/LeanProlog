package vm.logic;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.CharArrayReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

/**
 * Proposed interaction model:
 * 
 * send(channel/agent,Term,Timeout)
 * receive(channel/agent,Term,Timout)
 */
public class Interact {
  private static final long serialVersionUID=222L;
  
  public static int verbosity=3; // 2 the higher the more verbose
  
  public static int quickfail=3; // the higher the less fault tolerant we are
  
  public static final boolean LOGGING=true;
  
  public static final long MAXLOGSIZE=1L<<22; // ~4MB
  
  public static Integer set_verbosity(Integer v) {
    int oldv=verbosity;
    verbosity=v.intValue();
    return new Integer(oldv);
  }
  
  public static Integer get_verbosity() {
    return new Integer(verbosity);
  }
  
  public static Integer set_quickfail(Integer v) {
    int oldv=quickfail;
    quickfail=v.intValue();
    return new Integer(oldv);
  }
  
  public static Integer get_quickfail() {
    return new Integer(quickfail);
  }
  
  // //1
  
  public static String PROLOG_BYTECODE_FILE="lwam.bp";
  
  public static String PROLOG_JAR_FILE="lprolog.jar";
  
  public static String[] args=null;
  
  public static ObjectStack argStack=null;
  
  public static ObjectStack pathStack=new ObjectStack();
  
  public static final String NL=System.getProperty("line.separator"); // has
  
  // length=2
  
  // on Windows!!!
  
  private static long startTime=System.currentTimeMillis();
  
  // encodings
  
  public static String defaultEncoding="UTF-8"; // same as "UTF8"
  
  public static void setEncoding(String encoding) {
    defaultEncoding=encoding;
  }
  
  public static String getEncoding() {
    return defaultEncoding;
  }
  
  public static final String expand_env(String s) {
    Map envs=System.getenv();
    Iterator I=envs.keySet().iterator();
    while(I.hasNext()) {
      String key=(String)I.next();
      String key1="$"+key;
      String key2="%"+key;
      if(s.contains(key1)) {
        s=s.replace(key1,(String)envs.get(key));
      }
      if(s.contains(key2)) {
        s=s.replace(key2,(String)envs.get(key));
      }
    }
    if(s.contains("~"))
      s=s.replace("~",System.getProperty("user.home"));
    return s;
  }
  
  public static String absolute_file_name(String path) {
    if(null==path)
      return path;
    try {
      // Interact.dump("path BEFORE="+path);
      path=expand_env(path);
      // Interact.dump("path="+path);
      File f=new File(path);
      
      // if(f.exists()) { // breaks compiler if removed?
      path=f.getCanonicalPath();
      
      return path;
      
      // }
      // Interact.dump("the(path)="+Output);
    } catch(Exception e) {
      
      e.printStackTrace();
    }
    return path;
  }
  
  public static BufferedReader safeStringReader(String s) throws IOException {
    if(null==s)
      throw new IOException("safeStringReader called with null string");
    char[] chars=s.toCharArray();
    return new BufferedReader(new CharArrayReader(chars));
  }
  
  public static BufferedReader tryEncoding(InputStream inputStream) {
    return tryEncoding(inputStream,getEncoding());
  }
  
  public static BufferedReader safeFileReader(String s) throws IOException {
    s=absolute_file_name(s);
    InputStream is=new FileInputStream(s);
    return tryEncoding(is);
  }
  
  public static BufferedWriter safeFileAppender(String s) throws IOException {
    return safeFileWriter(s,true);
  }
  
  public static BufferedWriter safeFileWriter(String s) throws IOException {
    return safeFileWriter(s,false);
  }
  
  public static BufferedWriter safeFileWriter(String s,boolean append)
      throws IOException {
    s=absolute_file_name(s);
    OutputStream os=new FileOutputStream(s,append);
    return tryEncoding(os);
  }
  
  public static BufferedWriter tryEncoding(OutputStream outputStream) {
    return tryEncoding(outputStream,getEncoding());
  }
  
  public static BufferedWriter tryEncoding(OutputStream outputStream,
      String encoding) {
    OutputStreamWriter writer;
    try {
      writer=new OutputStreamWriter(outputStream,encoding);
    } catch(Exception e) { // use default if it fails
      warnmes("unable to use character encoding: "+encoding);
      writer=new OutputStreamWriter(outputStream);
    }
    return new BufferedWriter(writer);
  }
  
  public static BufferedReader tryEncoding(InputStream inputStream,
      String encoding) {
    InputStreamReader reader;
    try {
      reader=new InputStreamReader(inputStream,encoding);
    } catch(Exception e) { // use default if it fails
      warnmes("unable to use character encoding: "+encoding);
      reader=new InputStreamReader(inputStream);
    }
    return new BufferedReader(reader);
  }
  
  public static String forceEncodingOf(String str,String badEncoding,
      String goodEncoding) {
    try {
      return new String(str.getBytes(badEncoding),goodEncoding);
    } catch(UnsupportedEncodingException ex) {
      warnmes("unable to force character encoding: "+goodEncoding);
      return str;
    }
  }
  
  public static void add_to_path(String x) {
    pathStack.push(x);
  }
  
  public static Object get_path() {
    return pathStack.toList();
  }
  
  public static void clear_path() {
    pathStack=new ObjectStack();
  }
  
  public static ObjectStack extractCmds(String[] args) {
    ObjectStack s=new ObjectStack();
    if(null==args)
      return s;
    for(int i=0;i<args.length;i++) {
      String a=args[i];
      if(a.endsWith(".bp")||a.endsWith(".jar"))
        continue;
      if(!a.endsWith("."))
        a=a+".";
      s.push(a);
    }
    if(verbosity>3)
      log("lprolog starting with: "+s);
    return s;
  }
  
  public static long time() {
    long t=System.currentTimeMillis();
    long res=t-startTime;
    startTime=t;
    return res;
  }
  
  public static void println(Object O) {
    System.out.println(O.toString());
  }
  
  public static void pp(Object O) {
    System.out.println(O.toString());
  }
  
  public static void traceln(Object O) {
    if(verbosity>3)
      println(O);
  }
  
  public static void print(Object O) {
    System.out.print(O.toString());
  }
  
  public static final void dump(Object s) {
    println("!!!"+s);
  }
  
  public static final void warnmes(int level,String s) {
    if(verbosity>=level) {
      println(s);
      log(s);
    }
  }
  
  public static final void warnmes(String s) {
    warnmes(2,s);
  }
  
  private static final void warnmes(Throwable e,int fail) { // quickfail
    warnmes(e.toString());
    if(fail>=2||verbosity>=3)
      printStackTrace(e);
    if(fail>=3) {
      println("HALTING, quickfail MODE");
      halt(911);
    }
  }
  
  public static final void warnmes(String s,Throwable e) {
    warnmes(s);
    if(verbosity>=1&&null!=e) {
      warnmes(e,2);
    }
  }
  
  public static void printStackTrace(Throwable e) {
    if(verbosity>=1) {
      // CharArrayWriter b=new CharArrayWriter();
      ByteArrayOutputStream b=new ByteArrayOutputStream();
      PrintWriter fb=new PrintWriter(b); // was PrologWriter - for case of IDE
      // redirection
      e.printStackTrace(fb);
      fb.flush();
      warnmes("/*\n"+b.toString()+"\n*/");
      fb.close();
    }
  }
  
  public static final void errmes(String s,Throwable e) {
    warnmes(s);
    if(verbosity>=1&&null!=e) {
      warnmes(e,quickfail);
    }
  }
  
  public static final void errmes(String Mes) {
    errmes("??? "+Mes,(new Exception("error locator")));
  }
  
  public static final void fatal_error(String Mes) {
    quickfail=10;
    errmes("!!! "+Mes,(new Exception("error locator")));
  }
  
  public static long time=System.currentTimeMillis();
  
  final static long compTime(long time) {
    return time-AtomTable.SYMGCtime-HeapStack.mmTIME-PrologGC.gcTIME;
  }
  
  static void endMes(int code) {
    time=System.currentTimeMillis()-time;
    String scode=(0==code)?"":"("+code+")";
    String mes="Prolog execution halted"+scode+". CPU time = "+(time)+" ms\n";
    mes=mes+"Useful computation="+compTime(time)+", symbol gc="
        +AtomTable.SYMGCtime+" ms, expand/shrink="+HeapStack.mmTIME
        +" ms, heap gc="+PrologGC.gcTIME+" ms";
    println(mes);
    if(code>0)
      Interact.log("lprolog ended with error code="+code);
    if(verbosity>2)
      log(mes);
  }
  
  public static final void halt(String mes) {
    errmes("FATAL ERROR:",(new java.lang.Exception(mes)));
    halt(1);
  }
  
  public static final void halt(int code) {
    endMes(code);
    System.exit(code);
  }
  
  public static final byte[] toBytes(Object O) {
    if(null==O)
      return null;
    try {
      ByteArrayOutputStream b=new ByteArrayOutputStream();
      ObjectOutputStream s=new ObjectOutputStream(b);
      s.writeObject(O);
      s.flush();
      byte[] bs=b.toByteArray();
      // for(int i=0;i<bs.length;i++) Interact.print(bs[i]+" ");
      // Prolog.dump("toBytes="+bs.length);
      /*
      if(!O.equals(fromBytes(bs))) {
         warnmes("assertion failed in toBytes <"+O+">");
         return null;
      }
      */
      return bs;
    } catch(Exception e) {
      errmes("error in toBytes on <"+O+">",e);
      return null;
    }
  }
  
  public static final Object fromBytes(byte[] bs) {
    if(null==bs)
      return null;
    
    // for(int i=0;i<bs.length;i++) Interact.print(bs[i]+" ");
    // Prolog.dump("toBytes="+bs.length);
    
    try {
      ByteArrayInputStream b=new ByteArrayInputStream(bs);
      ObjectInputStream s=new ObjectInputStream(b);
      return s.readObject();
    } catch(Exception e) {
      errmes("error in fromBytes",e);
      return null;
    }
    
  }
  
  /*
  
  // test 
  static {
    String s="hello";
    //byte[] bs=toBytes(s);
    for(int i=0;i<bs.length;i++) {
      System.out.print(bs[i]+" ");
    }
    System.out.println();
    System.out.println(""+fromBytes(bs));
  }
  */
  
  private final static long quantum=200;
  
  public static void sleep_ms(long ms) {
    long steps=ms/quantum;
    long first=ms-(steps*quantum);
    try {
      Thread.yield();
      Thread.sleep(first);
      for(int i=0;i<steps;i++) {
        Thread.yield();
        Thread.sleep(quantum);
      }
    } catch(InterruptedException e) {
    }
  }
  
  public static void sleep_ns(int nanos) {
    try {
      Thread.sleep(0,nanos);
    } catch(InterruptedException e) {
    }
  }
  
  public static String logFileName="/tmp/lprolog.log";
  
  private static boolean badLogFile=false;
  
  public static PrintWriter getLogFile() {
    try {
      File logf=new File(logFileName);
      boolean keepFile=true;
      if(logf.length()>MAXLOGSIZE)
        keepFile=false;
      return new PrintWriter(new FileOutputStream(logf,keepFile),true);
    } catch(IOException e) {
      badLogFile=true;
      println("unable to start logging to: "+logFileName);
      return null;
    }
  }
  
  public static String getDateTime() {
    DateFormat dateFormat=new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    Date date=new Date();
    return dateFormat.format(date);
  }
  
  public static BigInteger getTime() {
    long t=System.currentTimeMillis();
    return BigInteger.valueOf(t);
  }
  
  public static final int max_cores() { // in MBytes
    return Runtime.getRuntime().availableProcessors();
  }
  
  public static final long max_memory() { // in MBytes
    return Runtime.getRuntime().maxMemory()>>20;
  }
  
  public static final long used_memory() { // in MBytes
    return (Runtime.getRuntime().totalMemory()-Runtime.getRuntime()
        .freeMemory())>>20;
  }
  
  public static final long available_memory() { // in MBytes
    return max_memory()-used_memory();
  }
  
  static final boolean memory_usage_is_high() {
    return 100*used_memory()>80*max_memory();
  }
  
  public static void log(Object mes) {
    if(!LOGGING)
      return;
    if(badLogFile)
      return;
    if(verbosity>2) {
      PrintWriter logFile=getLogFile();
      if(null!=logFile) {
        String timestamp="=== "+getDateTime()+" === mem="+used_memory()+"/"
            +max_memory();
        StringBuffer buf=new StringBuffer();
        for(int i=0;i<timestamp.length();i++)
          buf.append('-');
        logFile.println(timestamp+"\n"+mes+"\n"+buf+"\n\n");
      }
    }
  }
  
  public static void log_to(String fname,Object s,int withTimeStamp) {
    PrintWriter f=null;
    try {
      File logf=new File(fname);
      boolean keepFile=true;
      if(logf.length()>MAXLOGSIZE)
        keepFile=false;
      f=new PrintWriter(new FileOutputStream(logf,keepFile),true);
      if(withTimeStamp>0)
        f.println("% <<<"+getDateTime()+">>>");
      f.println(s);
      f.close();
    } catch(IOException e) {
      println("unable to start logging to: "+fname);
    }
  }
  
  public static String rbug(String s,String t) {
    try {
      if(s.length()>0&&t.length()>0)
        return s+t;
    } catch(Exception e) {
      Interact.log("error in rbug: "+s+"~"+t+"=>"+e);
      e.printStackTrace();
    }
    return null;
  }
  
  public static int RECURSION_DEPTH=2048; // avoids Java stack overflows
}
