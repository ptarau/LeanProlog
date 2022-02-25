package vm.extensions;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.SequenceInputStream;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.Random;
import java.util.Vector;

import vm.logic.AtomTable;
import vm.logic.Defs;
import vm.logic.Extender;
import vm.logic.Fun;
import vm.logic.Interact;
import vm.logic.Interactor;
import vm.logic.LogicEngine;
import vm.logic.ObjectStack;
import vm.logic.Prolog;
import vm.logic.TermConverter;
import vm.logic.Var;

// import java.math.MathContext; // 5.0++ only

public class XBuiltins implements XDefs,Extender {
  private static final long serialVersionUID=222L;
  
  static final Object the(Object x) {
    if(null==x)
      return AtomTable.Nothing;
    return new Fun("the",x);
  }
  
  /* thread local variables */
  
  private final void lput(Object var,Object val) {
    Prolog.lput(thread_id(),var,val);
  }
  
  private final Object lget(Object var) {
    return Prolog.lget(thread_id(),var);
  }
  
  private final void lremove(Object var) {
    Prolog.lremove(thread_id(),var);
  }
  
  private static String absolute_file_name(String path) {
    return Interact.absolute_file_name(path);
  }
  
  // static MathContext MC=MathContext.DECIMAL128;
  
  static int scale=16;
  
  final static int round=BigDecimal.ROUND_HALF_EVEN;
  
  static int fprecision=0;
  
  static DecimalFormat makeFormat(int decimals) {
    StringBuffer buf=new StringBuffer("#.");
    for(int i=0;i<decimals;i++) {
      buf.append('#');
    }
    return new DecimalFormat(buf.toString());
  }
  
  private static DecimalFormat formatter=makeFormat(fprecision);
  
  final static Object to_number(String S) {
    try {
      return new BigInteger(S);
    } catch(NumberFormatException e) {
      if(S.startsWith("0x")||S.startsWith("0X"))
        return new BigInteger(S.substring(2),16);
      else if(S.startsWith("0b")||S.startsWith("0B"))
        return new BigInteger(S.substring(2),2);
      else if(S.startsWith("0o")||S.startsWith("0O"))
        return new BigInteger(S.substring(2),8);
      else if(S.startsWith("0c")&&S.length()==3)
        return new Integer(S.charAt(2));
      return (new BigDecimal(S)).stripTrailingZeros();
    }
  }
  
  final static String to_string(Object o) {
    if(fprecision>0&&o instanceof Number)
      return formatter.format(o);
    return o.toString();
  }
  
  // static
  Random random=new Random(13);
  
  public static String to_rstring(Object t) {
    StringBuffer buf=new StringBuffer();
    add_canonical(t,buf);
    return buf.toString();
  }
  
  private static final String ensure_escaped(Object o) {
    String s=o.toString();
    s=s.replace("\\","\\\\");
    s=s.replace("\'","\\\'");
    s=s.replace("\"","\\\"");
    return s;
  }
  
  private static final void add_canonical(Object t,StringBuffer buf) {
    if(t instanceof Var) {
      buf.append('_');
      buf.append(((Var)t).getID());
    } else if(t instanceof Number) {
      buf.append(to_string(t));
    } else if(t instanceof Fun) {
      Object name=((Fun)t).name;
      Object[] args=((Fun)t).args;
      int l=args.length;
      // if(1==l&&"$VAR".equals(name)) { // Paulo
      // buf.append("_");
      // buf.append(args[0]);
      // } else
      {
        buf.append("\'");
        buf.append(ensure_escaped(name));
        buf.append("\'(");
        
        for(int i=0;i<l;i++) {
          Object a=args[i];
          if(null==a)
            buf.append("\'$null\'");
          else
            add_canonical(a,buf);
          if(i<l-1)
            buf.append(",");
        }
        buf.append(")");
      }
    } else {
      buf.append("(\'");
      buf.append(ensure_escaped(t));
      buf.append("\')");
    }
  }
  
  private static final long[] callcount=new long[MAXDEF];
  
  private static final void xprofile(int op) {
    callcount[op]=callcount[op]+1;
  }
  
  private static final Object get_xprofile() {
    ObjectStack x=new ObjectStack();
    ObjectStack fs=get_field_names("vm.extensions.XDefs");
    for(int i=0;i<MAXDEF;i++) {
      x.push(new Fun("-",BigInteger.valueOf(callcount[i]),
          // new Fun(":",
          // new Integer(i),
          fs.at(i+1)));
    }
    return x.toList();
  }
  
  Object statistics(Object Input,LogicEngine E) {
    Object Output=null;
    int op=(int)i(Input,1);
    switch(op) {
      case 0: {
        int prec=3;
        long t=System.currentTimeMillis()-Interact.time;
        BigDecimal T=(new BigDecimal(BigInteger.valueOf(t))
            .divide(new BigDecimal(1000.0),prec,round));
        Output=T.stripTrailingZeros();
      }
      break;
      case 1: {
        Output=new Integer(E.prolog.atomTable.size());
      }
      break;
      case 2: {
        Output=new Integer(E.getEngines().size());
      }
      break;
      case 3: {
        ObjectStack I=E.getEngines();
        long h=0L;
        long s=0L;
        long t=0L;
        for(int i=0;i<I.size();i++) {
          LogicEngine e=(LogicEngine)I.at(i);
          h+=e.getUsed();
          s+=e.choiceMem();
          t+=e.trailSize();
        }
        Output=new Fun("heap_stack_trail",BigInteger.valueOf(h),
            BigInteger.valueOf(s),BigInteger.valueOf(t));
      }
      break;
      case 4: {
        Output=new Fun("flags",
            new Fun("gc",E.gc_flag?new Integer(1):new Integer(0)),
            new Fun("symgc",
                E.prolog.atomTable.symgc_flag?new Integer(1):new Integer(0)));
      }
      break;
      case 5: {
        long tmem=Interact.max_memory();
        Output=BigInteger.valueOf(tmem);
      }
      break;
      case 6: {
        long umem=Interact.used_memory();
        Output=BigInteger.valueOf(umem);
      }
      break;
      case 7: {
        int tcnt=Thread.activeCount();
        int lcnt=E.prolog.atomTable.logicThreadCount;
        Output=new Fun("threads",new Fun("logic_in_bg",Integer.valueOf(lcnt)),
            new Fun("total",Integer.valueOf(tcnt)));
      }
      break;
      case 8: {
        Output=DynamicDB.db_size();
      }
      break;
      
      case 9: {
        Output=get_xprofile();
      }
      break;
      
      case 10: {
        Output=new Integer(Interact.max_cores());
      }
      break;
      
      default:
        Interact.errmes("statistics: bad op="+op);
    }
    
    return Output;
  }
  
  final static private Object icompute2(char op,Object Input) {
    Object Output;
    if(all_ints(Input)) {
      long a=i(Input,1);
      long b=i(Input,2);
      Output=computeSmall2(op,a,b);
    } else {
      Number X=b(Input,1);
      Number Y=b(Input,2);
      BigInteger a=(BigInteger)X;
      BigInteger b=(BigInteger)Y;
      Output=computeBig2(op,a,b);
    }
    return Output;
  }
  
  final static private Object computeSmall2(char op,long a,long b) {
    long r;
    switch(op) {
      case '+':
        r=a+b;
      break;
      case '-':
        r=a-b;
      break;
      case '*':
        r=a*b;
      break;
      case 'm':
        r=Math.min(a,b);
      break;
      case 'M':
        r=Math.max(a,b);
      break;
      case 'D':
        r=a/b;
      break;
      case 'd':
        r=a%b;
      break;
      case 'g':
        r=gcd(a,b);
      break;
      case 'i':
        r=modinv(a,b);
      break;
      
      // todo
      
      case 'A': // and
        r=a&b;
      break;
      
      case 'O': // or
        r=a|b;
      break;
      
      case 'X': // xor
        r=a^b;
      break;
      
      case 'E': // eq i.e. ~xor
        r=~(a^b);
      break;
      
      case 'I': // eq i.e. ~xor
        r=(~a)|b;
      break;
      
      case 'L': // eq i.e. ~xor
        r=(~a)&b;
      break;
      
      // todo
      
      default:
        return null;
    }
    return toBigIfOver(r);
  }
  
  final static private Object computeBig2(char op,BigInteger a,BigInteger b) {
    BigInteger r;
    switch(op) {
      case '+':
        r=a.add(b);
      break;
      case '-':
        r=a.subtract(b);
      break;
      case '*':
        r=a.multiply(b);
      break;
      case 'm':
        r=a.min(b);
      break;
      case 'M':
        r=a.max(b);
      break;
      case 'D':
        r=a.divide(b);
      break;
      case 'd':
        r=a.mod(b);
      break;
      case 'g':
        r=a.gcd(b);
      break;
      case 'i':
        r=a.modInverse(b);
      break;
      
      // todo: booleans
      
      case 'A':
        r=a.and(b);
      break;
      
      case 'O':
        r=a.or(b);
      break;
      
      case 'X':
        r=a.xor(b);
      break;
      
      case 'E':
        r=a.xor(b);
        r=r.not();
      break;
      
      case 'I':
        r=a.not();
        r=r.or(b);
      break;
      
      case 'L':
        r=a.not();
        r=r.and(b);
      break;
      
      // todo - booleans
      
      default:
        return null;
    }
    return r;
  }
  
  final static private Object computeDec2(char op,BigDecimal a,BigDecimal b) {
    BigDecimal r;
    switch(op) {
      case '+':
        r=a.add(b);
      break;
      case '-':
        r=a.subtract(b);
      break;
      case '*':
        r=a.multiply(b);
      break;
      case 'm':
        r=a.min(b);
      break;
      case 'M':
        r=a.max(b);
      break;
      
      default:
        return null;
    }
    return r.stripTrailingZeros();
  }
  
  final static private long gcd(long a,long b) {
    return BigInteger.valueOf(a).gcd(BigInteger.valueOf(b)).longValue();
  }
  
  final static private long modinv(long a,long b) {
    return BigInteger.valueOf(a).modInverse(BigInteger.valueOf(b)).longValue();
  }
  
  final static private BigInteger MAXINT=BigInteger.valueOf(Defs.MAXINT);
  
  final static private long maxint=Defs.MAXINT;
  
  final static private boolean all_ints(Object Input) {
    Fun FXs=(Fun)Input;
    int arity=FXs.args.length;
    for(int i=0;i<arity;i++) {
      if(!(FXs.args[i] instanceof Integer))
        return false;
    }
    return true;
  }
  
  final static private long i(Object Input,int n) {
    Fun FXs=(Fun)Input;
    return ((Integer)FXs.getArg(n)).longValue();
  }
  
  // input up conversion
  
  final static private Number b(Object Input,int n) {
    Fun FXs=(Fun)Input;
    Object I=FXs.getArg(n);
    if(I instanceof BigInteger) {
      return (BigInteger)I;
    }
    if(I instanceof BigDecimal) {
      return (BigDecimal)I;
    } else if(I instanceof String)
      return (new BigDecimal((String)I)).stripTrailingZeros();
    else
      return BigInteger.valueOf(((Integer)I).longValue());
  }
  
  public static double d(Object Input,int n) {
    Fun FXs=(Fun)Input;
    Object I=FXs.getArg(n);
    if(I instanceof BigInteger) {
      return ((BigInteger)I).doubleValue();
    }
    if(I instanceof BigDecimal) {
      return ((BigDecimal)I).doubleValue();
    } else if(I instanceof String)
      return Double.parseDouble((String)I);
    else
      return ((Integer)I).doubleValue();
  }
  
  final static private Number upConvertAs(Number X,Number Y) {
    if(X instanceof BigDecimal&&Y instanceof BigInteger)
      Y=new BigDecimal((BigInteger)Y);
    return Y;
  }
  
  // output down conversion
  
  final static private Object toBigIfOver(long b) {
    if(maxint>Math.abs(b))
      return new Integer((int)b);
    return BigInteger.valueOf(b);
    // return String.valueOf(b); // $$ String conv here
  }
  
  final static private Object maybeInteger(Object O) { // $$ String conv here
    
    Object R=O;
    if(O instanceof BigInteger) {
      BigInteger b=(BigInteger)O;
      int rel=MAXINT.compareTo(b.abs());
      if(rel<0) {
        // R=b.toString();
        R=b;
      } else {
        R=new Integer((int)b.longValue());
      }
    }
    
    return R;
  }
  
  /**
   * calls a generic Interactor pseudo-constructor using a reflection mechanism
   * 
   * one can add Prolog extensions by simply implementing the interface
   * interactor and providing a static method like this one
   * 
   * note that Object is usually a String or something the caller knows about -
   * match through reflection should be _exact_
   */
  static Interactor new_interactor(Object Input) {
    // Prolog.dump("new_interactor: Input="+Input);
    
    String interactor=to_s(Input,1);
    Object initializer=o(Input,2);
    Object[] args=new Object[] { initializer };
    boolean quiet=true;
    Interactor I=(Interactor)call_java_class_method(interactor,"new_interactor",
        args,quiet);
        
    if(null==I)
      Interact.errmes("new_interactor failed on: "+Input);
      
    return I;
  }
  
  public final int obtype_of(Prolog current,Object O) {
    if(O instanceof String)
      return LogicEngine.STRINGOB_TYPE;
    if(O instanceof Integer)
      return LogicEngine.SMALLINT_TYPE;
    if(O instanceof BigInteger)
      return LogicEngine.BIGINT_TYPE;
    if(O instanceof BigDecimal)
      return LogicEngine.BIGDEC_TYPE;
    if(O instanceof byte[])
      return LogicEngine.BYTES_TYPE;
    if(O instanceof LogicEngine) {
      LogicEngine other=(LogicEngine)O;
      if(other.prolog==current)
        return LogicEngine.ENGINE_TYPE;
    }
    if(O instanceof Interactor)
      return LogicEngine.INTERACTOR_TYPE;
    return LogicEngine.OTHER_OB_TYPE;
  }
  
  private static final Object o(Object Input,int n) {
    Fun FXs=(Fun)Input;
    return FXs.getArg(n);
  }
  
  private static final String to_s(Object Input,int n) {
    Object O=o(Input,n);
    if(null==O)
      return "$null";
    return O.toString();
  }
  
  static final byte[] bs(Object Input,int n) {
    Object O=o(Input,n).toString();
    return Interact.toBytes(O);
  }
  
  /*
  public static final Object serialize(Object O) {
    byte[] bs=Interact.toBytes(O);
    int l=bs.length;
    // System.out.println(">l="+l);
    int l3=1+l/3;
    ObjectStack s=new ObjectStack(2+l3);
    s.push(new Integer(l));
    // for(int k=0;k<l;k++) {
    // System.out.print(bs[k]+",");
    // }
    // System.out.println();
    for(int i=0;i<l3;i++) {
      int n=0;
      int j=3*i;
      if(j<l) {
        int b=bs[j];
        if(b<0)
          b=256+b;
        // System.out.println("b="+b);
        n=b;
      }
      j++;
      n=(n<<8);
      if(j<l) {
        int b=bs[j];
        if(b<0)
          b=256+b;
        n+=b;
      }
      j++;
      n=(n<<8);
      if(j<l) {
        int b=bs[j];
        if(b<0)
          b=256+b;
        n+=b;
      }
      s.push(new Integer(n));
    }
    return s.toList();
  }
  
  public static final Object unserialize(Object xs) {
    ObjectStack s=ObjectStack.fromList(xs);
    int l=((Integer)s.at(0)).intValue();
    byte[] bs=new byte[l];
    int l3=1+l/3;
    // System.out.println("<l3="+l3);
    for(int i=0;i<l3;i++) {
      int n=((Integer)s.at(i+1)).intValue();
      // System.out.println("n="+n);
      int j=3*i+2;
      if(j<l) {
        bs[j]=(byte)(n&255);
      }
      j--;
      n=n>>>8;
      if(j<l) {
        bs[j]=(byte)(n&255);
      }
      j--;
      n=n>>>8;
      if(j<l) {
        bs[j]=(byte)(n&255);
      }
      // System.out.print(n+",");
    }
    // System.out.println("<=n");
    // for(int k=0;k<l;k++) {
    // System.out.print(bs[k]+",");
    // }
    // System.out.println();
    return Interact.fromBytes(bs);
  }
  
  
  public static int byte_size(Object O) {
    return Interact.toBytes(O).length;
  }
  */
  
  /*
  public static BigInteger bigfact(BigInteger n) {
    if(n.compareTo(BigInteger.ZERO)<=0) {
      return BigInteger.ONE;
    }
    return n.multiply(bigfact(n.subtract(BigInteger.ONE)));
  }
  
  public static BigInteger serbigfact(BigInteger n) {
    if(n.compareTo(BigInteger.ZERO)<=0) {
      return BigInteger.ONE;
    }
    BigInteger r=n.multiply(serbigfact(n.subtract(BigInteger.ONE)));
    return (BigInteger)unserialize(serialize(r));
  }
  
  public static ArrayList fr(BigInteger N) {
    ArrayList Ns=new ArrayList();
    fr1(BigInteger.ONE,N,Ns);
    Collections.reverse(Ns);
    return Ns;
  }
  
  public static void fr1(BigInteger J,BigInteger K,ArrayList Ns) {
    // Interact.pp(J+"+"+K+",Ns="+Ns);
    if(K.compareTo(BigInteger.ZERO)>0) {
      BigInteger KMJ=K.mod(J);
      BigInteger J1=J.add(BigInteger.ONE);
      BigInteger KDJ=K.divide(J);
      fr1(J1,KDJ,Ns);
      Ns.add(KMJ);
    }
  }
  
  public static ArrayList sfr(BigInteger N) {
    ArrayList Ns=new ArrayList();
    sfr1(BigInteger.ONE,N,Ns);
    Collections.reverse(Ns);
    return Ns;
  }
  
  public static void sfr1(BigInteger J,BigInteger K,ArrayList Ns) {
    // Interact.pp(J+"+"+K+",Ns="+Ns);
    if(K.compareTo(BigInteger.ZERO)>0) {
      BigInteger KMJ=K.mod(J);
      BigInteger J1=J.add(BigInteger.ONE);
      BigInteger KDJ=K.divide(J);
      J1=(BigInteger)unserialize(serialize(J1));
      KDJ=(BigInteger)unserialize(serialize(KDJ));
      KMJ=(BigInteger)unserialize(serialize(KMJ));
      sfr1(J1,KDJ,Ns);
      Ns.add(KMJ);
    }
  }
  */
  
  /*
  public static BigInteger rf(ArrayList Ns) {
    Collections.reverse(Ns);
    int l=Ns.size();
    int k=l-1;
    return lf(k,Ns);
  }
  */
  
  /*
  static {
    ArrayList ns=sfr(new BigInteger("2012"));
    Interact.pp("ns="+ns);
    ns=sfr(new BigInteger("12345"));
    Interact.pp("ns="+ns);
    
    long t0=System.currentTimeMillis();
    BigInteger n=new BigInteger("1234").pow(5678);
    ArrayList r=fr(n);
    r=fr(n); // twice to emulate cost of decoding
    long t1=System.currentTimeMillis();
    Interact.pp("time="+(t1-t0)+"->"+r.size());
    
    long t2=System.currentTimeMillis();
    n=new BigInteger("1234").pow(5678);
    ArrayList sr=sfr(n);
    sr=sfr(n); // twice, to emulate effor of decoding
    long t3=System.currentTimeMillis();
    Interact.pp("time="+(t3-t2)+"->"+sr.size());
  }
  */
  
  /*
  static {
   long t0=System.currentTimeMillis();
   BigInteger n=new BigInteger("20000");
   BigInteger r=bigfact(n);
   long t1=System.currentTimeMillis();
   Interact.pp("time="+(t1-t0)+":"+n+"->"+r.toString().length());
   
   long t2=System.currentTimeMillis();
   BigInteger sr=serbigfact(n);
   long t3=System.currentTimeMillis();
   Interact.pp("time="+(t3-t2)+":"+n+"->"+sr.toString().length());
  }
  */
  
  /*
  static {
    Object s=new String("");
    System.out.println("byte size="+byte_size(s));
    s=new ArrayList();
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    s=new HashMap();
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    s=new java.util.LinkedHashMap();
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    
    s=new Integer(0);
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    s=new Double(0);
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    s=new Fun("f",new Integer(0));
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    s=new BigInteger("0");
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    s=new BigDecimal("0.0");
    System.out.println(s.getClass()+",byte size="+byte_size(s));
    //
    Object O=serialize(s);
    System.out.println("serialized="+O);
    Object u=unserialize(O);
    if(u!=null)
      System.out.println(s+"="+u);
  }
  */
  
  /**
   * simple reflection layer for calling static methods of a class
   * 
   * NOTE: requires EXACT signatures !!!
   */
  
  public static final Object call_java_class_method(String className,
      String methodName,Object[] args,boolean quiet) {
    if(!quiet) {
      Prolog.dump("call_java_class_method=> "+className+","+methodName);
      for(int i=0;i<args.length;i++) {
        Prolog.dump("  "+args[i]+":"+args[i].getClass().getName());
      }
    }
    Object result=null;
    int argn=args.length;
    Class[] argTypes=new Class[argn];
    for(int i=0;i<argn;i++) {
      argTypes[i]=args[i].getClass();
    }
    try {
      Class c=Class.forName(className);
      java.lang.reflect.Method m=c.getMethod(methodName,argTypes);
      result=m.invoke(c,args);
    } catch(Exception e) {
      // if(!quiet) {
      Fun T=new Fun(methodName,args);
      Interact.warnmes("reflection error: class="+className+",method+args="+T,
          e);
      // }
    }
    return result;
  }
  
  public static final int system(String cmd) {
    try {
      Process P=Runtime.getRuntime().exec(cmd);
      P.waitFor();
    } catch(Exception e) {
      Interact.errmes("error in system cmd: "+cmd,e);
      return 0;
    }
    return 1;
  }
  
  public static final boolean concatenate_files(ObjectStack fs,String f) {
    // Interact.println("*** FS: "+fs);
    
    try {
      Vector V=new Vector(fs.size());
      for(int i=0;i<fs.size();i++) {
        String fname=(String)fs.at(i);
        if(fname.contains(".DS_Store"))
          continue;
        V.add(new FileInputStream(fname));
        // Interact.println("*** FS: "+fname);
      }
      
      BufferedOutputStream os=new BufferedOutputStream(new FileOutputStream(f));
      BufferedInputStream is=new BufferedInputStream(
          new SequenceInputStream(V.elements()));
      int c;
      while((c=is.read())!=-1) {
        os.write(c);
      }
      os.close();
      is.close();
      return true;
    } catch(Exception e) {
      Interact.warnmes("error in concatenate_files: "+fs+"=>"+f,e);
      return false;
    }
  }
  
  final static int maxvar(Object t) {
    if(t instanceof Var) {
      return ((Var)t).getID();
    } else if(t instanceof Fun) {
      Fun f=(Fun)t;
      int l=f.args.length;
      int m=0;
      for(int i=0;i<l;i++) {
        int a=maxvar(f.args[i]);
        if(a>m)
          m=a;
      }
      return m;
    } else
      return -1;
  }
  
  final static private Object varnumbers(Object t,int m) {
    if(t instanceof Var)
      return t;
    if(t instanceof Fun) {
      Fun f=(Fun)t;
      int l=f.args.length;
      if(1==l&&f.name.equals("$VAR")) {
        int i=((Integer)f.args[0]).intValue();
        return new Var(m+i);
      }
      Fun cf=new Fun(f.name,l);
      for(int i=0;i<f.args.length;i++) {
        cf.args[i]=varnumbers(f.args[i],m);
      }
      return cf;
    } else
      return t;
  }
  
  final static private Object compute2(char op,Object Input) {
    Object Output;
    if(all_ints(Input)) {
      long a=i(Input,1);
      long b=i(Input,2);
      Output=computeSmall2(op,a,b);
    } else {
      // Prolog.dump("$$$: "+Input);
      Number X=b(Input,1);
      Number Y=b(Input,2);
      Number B=upConvertAs(X,Y);
      Number A=upConvertAs(Y,X);
      if(A instanceof BigDecimal) {
        BigDecimal a=(BigDecimal)A;
        BigDecimal b=(BigDecimal)B;
        Output=computeDec2(op,a,b);
      } else {
        BigInteger a=(BigInteger)A;
        BigInteger b=(BigInteger)B;
        Output=computeBig2(op,a,b);
      }
    }
    return Output;
  }
  
  public Object xcall(int Op,Object Input,LogicEngine E) {
    // Prolog.dump("xcall<=== Input op="+Op+":"+Input);
    Object Output=null;
    xprofile(Op);
    try {
      Output=xcall0(Op,Input,E);
      // if(null==Output)
      // Prolog.dump(new Fun("NULL OUTPUT",new Integer(Op),Input));
      
      // if(null==Output) {
      // Output=new Fun("exception",new Integer(Op),"null_output");
      // }
      if(null==Output)
        Output=AtomTable.Nothing;
    } catch(Exception e) {
      Interact.warnmes("error in xcall",e);
      Output=new Fun("exception",new Integer(Op),e.toString());
    }
    // Prolog.dump("xcall===> op="+Op+":"+Output);
    return Output;
  }
  
  private Object xcall0(int Op,Object Input,LogicEngine E) {
    
    Object Output=null;
    
    switch(Op) {
      
      case NEGATE:
        if(all_ints(Input))
          Output=toBigIfOver(-i(Input,1));
        else {
          Number A=b(Input,1);
          if(A instanceof BigInteger)
            Output=((BigInteger)A).negate();
          else
            Output=((BigDecimal)A).negate().stripTrailingZeros();
        }
      break;
      
      case ABS:
        if(all_ints(Input))
          Output=toBigIfOver(Math.abs(i(Input,1)));
        else {
          Number A=b(Input,1);
          if(A instanceof BigInteger)
            Output=((BigInteger)A).abs();
          else
            Output=((BigDecimal)A).abs().stripTrailingZeros();
        }
      break;
      
      case COMPARE:
        if(all_ints(Input)) {
          long dif=i(Input,1)-i(Input,2);
          dif=Long.signum(dif);
          
          Output=new Integer((int)dif);
        } else {
          Number X=b(Input,1);
          Number Y=b(Input,2);
          
          Number B=upConvertAs(X,Y);
          Number A=upConvertAs(Y,X);
          
          if(A instanceof BigDecimal)
            Output=((BigDecimal)A).stripTrailingZeros()
                .compareTo(((BigDecimal)B).stripTrailingZeros());
          else
            Output=((BigInteger)A).compareTo((BigInteger)B);
        }
      break;
      
      case PLUS:
        Output=compute2('+',Input);
      break;
      
      case SUBTRACT:
        Output=compute2('-',Input);
      break;
      
      case MULTIPLY:
        Output=compute2('*',Input);
      break;
      
      case POW:
        if(all_ints(Input)) {
          int y=(int)i(Input,2);
          BigInteger X=BigInteger.valueOf(i(Input,1));
          if(y>=0) {
            Output=X.pow(y);
          } else {
            BigDecimal R=BigDecimal.ONE.divide((new BigDecimal(X.pow(-y))),
                scale,round);
            Output=R.stripTrailingZeros();
          }
        } else {
          Number X=b(Input,1);
          Number Y=b(Input,2);
          double x;
          double y;
          if(X instanceof BigInteger)
            x=((BigInteger)X).doubleValue();
          else
            x=((BigDecimal)X).doubleValue();
          if(Y instanceof BigInteger)
            y=((BigInteger)Y).doubleValue();
          else
            y=((BigDecimal)Y).doubleValue();
            
          double r=Math.pow(x,y);
          
          Output=BigDecimal.valueOf(r).stripTrailingZeros();
        }
      break;
      
      case MIN:
        Output=compute2('m',Input);
      break;
      
      case MAX:
        Output=compute2('M',Input);
      break;
      
      case DIVIDE: {
        BigDecimal A;
        BigDecimal B;
        if(all_ints(Input)) {
          A=BigDecimal.valueOf(i(Input,1));
          B=BigDecimal.valueOf(i(Input,2));
        } else {
          Number X=b(Input,1);
          Number Y=b(Input,2);
          Number Y1=upConvertAs(X,Y);
          Number X1=upConvertAs(Y,X);
          if(X1 instanceof BigDecimal) {
            A=(BigDecimal)X1;
            B=(BigDecimal)Y1;
          } else {
            A=new BigDecimal((BigInteger)X1);
            B=new BigDecimal((BigInteger)Y1);
          }
        }
        int prec=Math.max(A.scale(),B.scale());
        prec=Math.max(prec,scale);
        BigDecimal C=A.divide(B,prec,round);
        C=C.stripTrailingZeros();
        // Interact.dump("C.scale()="+C.scale());
        Output=C;
      }
      break;
      
      case BITCOUNT: {
        int r;
        BigInteger X;
        if(all_ints(Input)) {
          X=BigInteger.valueOf(i(Input,2));
        } else {
          X=(BigInteger)b(Input,2);
        }
        r=(0==i(Input,1)?X.getLowestSetBit():X.bitLength());
        Output=new Integer(r);
      }
      break;
      
      case LSHIFT:
        if(all_ints(Input)) {
          BigInteger X=BigInteger.valueOf(i(Input,1));
          int Y=(int)i(Input,2);
          Output=X.shiftLeft(Y);
        } else {
          Number X=b(Input,1);
          int Y=(int)i(Input,2);
          Output=((BigInteger)X).shiftLeft(Y);
        }
      break;
      
      case RSHIFT:
        if(all_ints(Input)) {
          BigInteger X=BigInteger.valueOf(i(Input,1));
          int y=(int)i(Input,2);
          Output=X.shiftRight(y);
        } else {
          BigInteger X=(BigInteger)b(Input,1);
          int y=(int)i(Input,2);
          Output=X.shiftRight(y);
        }
      break;
      
      case GETBIT: {
        int r;
        if(all_ints(Input)) {
          BigInteger X=BigInteger.valueOf(i(Input,1));
          int y=(int)i(Input,2);
          r=X.testBit(y)?1:0;
        } else {
          BigInteger A=(BigInteger)b(Input,1);
          int y=(int)i(Input,2);
          r=A.testBit(y)?1:0;
        }
        Output=new Integer(r);
      }
      break;
      
      case SETBIT:
        if(all_ints(Input)) {
          BigInteger X=BigInteger.valueOf(i(Input,1));
          int Y=(int)i(Input,2);
          int B=(int)i(Input,3);
          Output=(0==B)?X.clearBit(Y):X.setBit(Y);
        } else {
          BigInteger A=(BigInteger)b(Input,1);
          int y=(int)i(Input,2);
          int b=(int)i(Input,3);
          Output=(0==b)?A.setBit(y):A.clearBit(y);
        }
      break;
      
      case FLIPBIT:
        if(all_ints(Input)) {
          BigInteger X=BigInteger.valueOf(i(Input,1));
          int y=(int)i(Input,2);
          Output=X.flipBit(y);
        } else {
          BigInteger X=(BigInteger)b(Input,1);
          int y=(int)i(Input,2);
          Output=X.flipBit(y);
        }
      break;
      
      case ITE:
        if(all_ints(Input)) {
          int x=(int)i(Input,1);
          int t=(int)i(Input,2);
          int e=(int)i(Input,3);
          // ite(X,T,E, R):-R is xor(/\(X,xor(T,E)),E).
          int te=t^e;
          int xte=x&te;
          Output=new Integer(xte^e);
        } else {
          // Interact.println("HERE:"+Input);
          
          BigInteger X=(BigInteger)b(Input,1);
          BigInteger T=(BigInteger)b(Input,2);
          BigInteger El=(BigInteger)b(Input,3);
          BigInteger TE=T.xor(El);
          BigInteger XTE=X.and(TE);
          // Interact.println("THERE:"+Output);
          
          Output=XTE.xor(El);
        }
      break;
      
      case NOT:
        if(all_ints(Input)) {
          int x=(int)i(Input,1);
          Output=new Integer(~x);
        } else {
          BigInteger X=(BigInteger)b(Input,1);
          Output=X.not();
        }
      break;
      
      case AND:
        Output=icompute2('A',Input);
      break;
      
      case OR:
        Output=icompute2('O',Input);
      break;
      
      case XOR:
        Output=icompute2('X',Input);
      break;
      
      case EQ:
        Output=icompute2('E',Input);
      break;
      
      case IMPL:
        Output=icompute2('I',Input);
      break;
      
      case LESS:
        Output=icompute2('L',Input);
      break;
      
      case DIV:
        Output=icompute2('D',Input);
      break;
      
      case MOD:
        Output=icompute2('d',Input);
      break;
      
      case GCD:
        Output=icompute2('g',Input);
      break;
      
      case MODINV:
        Output=icompute2('i',Input);
      break;
      
      case GC: {
        Output=AtomTable.Yes;
        int gcOp=((int)i(Input,1));
        if(0==gcOp) {
          E.gc_flag=true;
        } else if(1==gcOp) { // symgc
          // Prolog.dump("@@@before symgc: "+e.prolog.atomTable.info());
          E.prolog.atomTable.force_trimSyms(E);
          // Runtime.getRuntime().gc();
          // Prolog.dump("@@@after symgc: "+e.prolog.atomTable.info());
        } else if(2==gcOp) {
          Output=new Integer(AtomTable.set_symgc(0));
        } else if(3==gcOp) {
          Output=new Integer(AtomTable.set_symgc(1));
        } else {
          Interact.warnmes("bad gcOp");
          Output=AtomTable.Nothing;
        }
      }
      break;
      
      case TO_CODES: {
        String S=to_s(Input,1);
        Output=new Fun("$string",S); // G_STRING
      }
      break;
      
      case FROM_CODES:
        Output=to_s(Input,1);
      break;
      
      case TO_NCODES: {
        String S=to_string(o(Input,1));
        Output=new Fun("$string",S); // G_STRING
      }
      break;
      
      case FROM_NCODES:
        // Output=new Fun("$number",o(Input,1));
        Output=TermConverter.downConvert(o(Input,1));
      break;
      
      case HALT:
        Interact.halt((int)i(Input,1));
      break;
      
      case NEW_INTERACTOR:
        Output=new_interactor(Input);
      break;
      
      case ASK_INTERACTOR: { // get for engines
        Interactor I=(Interactor)o(Input,1);
        if(I instanceof LogicEngine) {
          // Interact.dump("ASK_INTERACTOR:"+I+" ASKER="+E);
        }
        Output=I.ask_interactor();
      }
      break;
      
      // ask another interactor to do something
      case TELL_INTERACTOR: { // to_engine
        // Prolog.dump("TELL_INTERACTOR:"+Input);
        Interactor other=(Interactor)o(Input,1);
        Output=other.tell_interactor(o(Input,2));
        if(null==Output)
          Output=AtomTable.Nothing;
      }
      break;
      
      // provided by non-engine interactors
      // interactor_return
      // interactor_handle
      
      case STOP_INTERACTOR: { // stop
        Output=AtomTable.Yes;
        Object Handle=o(Input,1);
        if(Handle instanceof Interactor) {
          Interactor other=(Interactor)Handle;
          other.stop_interactor();
        } else {
          Interact.warnmes("*** stop_interactor applied to: "+Handle);
          Output=AtomTable.Nothing;
        }
      }
      break;
      
      case STATISTICS:
        Output=statistics(Input,E);
      break;
      
      /**
       * limited reflection: jcall(Class,StaticMethodAndArgs,Result)
       * 
       * IMPORTANT: argument types should be EXACT: i.e. if a method is declared
       * to have an argument Object, calling it with a String will fail, even if
       * String is instance of Object. This is, somewhat of an infelicity - but
       * it is fast.
       */
      
      // used for db_ operations, mostly
      case JCALL: {
        String C=to_s(Input,1);
        Object G=o(Input,2);
        String F;
        Object[] args;
        if(G instanceof Fun) {
          Fun FXs=(Fun)G;
          args=FXs.args;
          F=(String)FXs.name;
        } else {
          F=(String)G;
          args=new Object[] {};
        }
        boolean quiet=true; // !verbose
        Output=call_java_class_method(C,F,args,quiet);
        if(null==Output)
          Output=AtomTable.S_null;
      }
      break;
      
      case GVAR_SET: {
        Object var=o(Input,1);
        Object val=o(Input,2);
        Prolog.put(var,val);
      }
      break;
      
      case GVAR_GET: {
        Object var=o(Input,1);
        Object val=Prolog.get(var);
        
        Output=the(val);
        // Prolog.dump("====>"+Output);
      }
      break;
      
      case GVAR_REMOVE: {
        Object var=o(Input,1);
        Prolog.remove(var);
      }
      break;
      
      case RANDOM:
        if(all_ints(Input)) {
          int lim=(int)i(Input,1);
          int r=random.nextInt(lim);
          Output=new Integer(r);
        } else {
          Number Lim=b(Input,1);
          if(Lim instanceof BigInteger) {
            BigInteger B=(BigInteger)Lim;
            Output=(new BigInteger(1+B.bitLength(),random)).mod(B);
          } else
            Interact.warnmes("bad arg in random:"+Lim);
        }
      break;
      
      case SET_RANDOM_SEED:
        if(all_ints(Input)) {
          int seed=(int)i(Input,1);
          random=new Random(seed);
        } else {
          Interact.warnmes("bad arg in set_random_seed:"+Input);
        }
      break;
      
      case SET_PRECISION:
        if(all_ints(Input)) {
          int op=(int)i(Input,1);
          int prec=(int)i(Input,2);
          if(op>0) {
            fprecision=prec;
            formatter=makeFormat(fprecision);
          } else
            scale=prec;
        } else {
          Interact.warnmes("bad arg in set_precision:"+Input);
        }
      break;
      
      case EXISTS_FILE: {
        String F=(String)o(Input,1);
        int ok=0;
        if(!(null==F)) {
          F=absolute_file_name(F);
          File f=new File(F);
          ok=f.exists()?1:0;
          if(ok>0&&f.isDirectory())
            ok=2;
        }
        Output=new Integer(ok);
      }
      break;
      
      case FLOOR:
        if(all_ints(Input))
          Output=i(Input,1);
        else {
          Number A=b(Input,1);
          if(A instanceof BigInteger)
            Output=A;
          else
            Output=((BigDecimal)A).toBigInteger();
        }
      break;
      
      case LOG: {
        double x=d(Input,1);
        double r=Math.log(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case SIN: {
        double x=d(Input,1);
        double r=Math.sin(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case COS: {
        double x=d(Input,1);
        double r=Math.cos(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case TAN: {
        double x=d(Input,1);
        double r=Math.tan(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case ASIN: {
        double x=d(Input,1);
        double r=Math.asin(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case ACOS: {
        double x=d(Input,1);
        double r=Math.acos(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case ATAN: {
        double x=d(Input,1);
        double r=Math.atan(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case ARITH_CONST: {
        int x=(int)i(Input,1);
        double r=0.0;
        switch(x) {
          case 0:
            r=Math.PI;
            Output=BigDecimal.valueOf(r);
          break;
          case 1:
            r=Math.E;
            Output=BigDecimal.valueOf(r);
          break;
          case 2:
            Output=BigDecimal.ONE.divide(BigDecimal.valueOf(10.0).pow(scale));
          break;
        }
      }
      break;
      
      case EXP: {
        double x=d(Input,1);
        double r=Math.exp(x);
        Output=BigDecimal.valueOf(r);
      }
      break;
      
      case XTEST: {
        Object I=o(Input,1);
        Output=xtest(I);
      }
      break;
      
      case NEW_OPS: {
        int init=(int)i(Input,1);
        Output=Ops.newOps(init);
      }
      break;
      
      case GET_PRI: {
        Ops ops=(Ops)o(Input,1);
        String name=to_s(Input,2);
        String assoc=to_s(Input,3);
        Output=ops.getPri(name,assoc);
      }
      break;
      
      case GET_PRIS: {
        Ops ops=(Ops)o(Input,1);
        String name=to_s(Input,2);
        Output=ops.getPris(name);
        if(null==Output)
          Output=AtomTable.Nil;
      }
      break;
      
      case OP_SUCC: {
        Ops ops=(Ops)o(Input,1);
        int pri=(int)i(Input,2);
        Output=ops.succ(pri);
      }
      break;
      
      case OP_PRED: {
        Ops ops=(Ops)o(Input,1);
        int pri=(int)i(Input,2);
        Output=ops.pred(pri);
      }
      break;
      
      case ALL_OPS: {
        Ops ops=(Ops)o(Input,1);
        Output=ops.allOps();
      }
      break;
      
      case OP: {
        Ops ops=(Ops)o(Input,1);
        int pri=(int)i(Input,2);
        String assoc=to_s(Input,3);
        String name=to_s(Input,4);
        ops.op(pri,assoc,name);
      }
      break;
      
      case TO_NUMBER: {
        String S=to_s(Input,1);
        Output=to_number(S);
      }
      break;
      
      case PROTECT: {
        int i=(int)i(Input,1);
        LogicEngine e=E.getEngine(i);
        e.protect_engine();
        Output=AtomTable.Yes;
      }
      break;
      
      case UNPROTECT: {
        int i=(int)i(Input,1);
        LogicEngine e=E.getEngine(i);
        e.protect_engine();
        e.unprotect_engine();
        Output=AtomTable.Yes;
      }
      break;
      
      case SET_VAL: {
        Object var=o(Input,1);
        Object val=o(Input,2);
        // Interact.dump(var+"<="+val);
        lput(var,val);
      }
      break;
      
      case GET_VAL: {
        Object var=o(Input,1);
        Object val=lget(var);
        
        Output=the(val);
        // Prolog.dump("====>"+Output);
      }
      break;
      
      case REMOVE_VAL: {
        Object var=o(Input,1);
        lremove(var);
      }
      break;
      
      case EXPORT: { // sent as - to xcall, now +
        E.setBundle((Object[])Input);
        Output=AtomTable.Yes;
      }
      break;
      
      case IMPORT: { // sent as - to xcall, now +
        Output=E.getBundle();
      }
      break;
      
      case SERVER: { // uses $prolog_loop
        // Interact.dump("SERVER:"+Input);
        int port=(int)i(Input,1);
        int op=(int)i(Input,2);
        Object fileOrProlog=o(Input,3);
        Object goal=o(Input,4);
        
        if(op==Prolog.IDENTITY||op==Prolog.CLONE)
          fileOrProlog=E.prolog;
        Task T=new OuterTask(port,op,fileOrProlog,goal);
        // op controls cloning !!!
        
        T.run();
        // runs forever
        Output=AtomTable.Yes;
      }
      break;
      
      /* networking */
      
      case CONNECTION: {
        String host=to_s(Input,1);
        int port=(int)i(Input,2);
        int quickFail=(int)i(Input,3);
        // Prolog.dump("CONNECTION TO: "+host+":"+port+",quickFail:"+quickFail);
        Output=OuterTask.connection(host,port,quickFail);
      }
      break;
      
      case ASK_SERVER: {
        Transport connection=(Transport)o(Input,1);
        // Prolog.dump("ASK SERVER TO: "+connection.host+":"+connection.port);
        Object[] bundle=E.getBundle();
        
        // Prolog.dump("TO_SERVER: "+new PortableTerm(bundle,0));
        bundle=(Object[])OuterTask.ask_server(connection,bundle);
        // Prolog.dump("FROM_SERVER: "+new PortableTerm(bundle,0));
        
        E.setBundle(bundle);
        Output=AtomTable.Yes;
      }
      break;
      
      case DISCONNECT: {
        Transport connection=(Transport)o(Input,1);
        connection.disconnect();
        Output=AtomTable.Yes;
      }
      break;
      
      case SHM_PUT: {
        String diranme=to_s(Input,1);
        String key=to_s(Input,2);
        Object[] bundle=E.getBundle();
        Output=AtomTable.Nothing;
        if(null==bundle)
          break;
        if(SharedMem.put(diranme,key,bundle))
          Output=AtomTable.Yes;
        else
          E.setBundle(bundle);
      }
      break;
      
      case SHM_GET: {
        Output=AtomTable.Nothing;
        String dirname=to_s(Input,1);
        String key=to_s(Input,2);
        Object value=SharedMem.get(dirname,key);
        if(null==value)
          break;
          
        // Prolog.dump("GOT: "+value+":"+value.getClass());
        Object[] bundle=(Object[])value;
        E.setBundle(bundle);
        Output=AtomTable.Yes;
      }
      break;
      
      case SHM_REMOVE: {
        String dirname=to_s(Input,1);
        String key=to_s(Input,2);
        Output=SharedMem.remove(dirname,key)?AtomTable.Yes:AtomTable.Nothing;
      }
      break;
      
      case SLEEP_MS: {
        long ms=i(Input,1);
        Interact.sleep_ms(ms);
      }
      break;
      
      // inner thread client/server operations
      
      case ISERVER: {
        int op=(int)i(Input,1);
        Object fileOrProlog=o(Input,2);
        Object goal=o(Input,3);
        if(op==Prolog.IDENTITY||op==Prolog.CLONE)
          fileOrProlog=E.prolog;
        Task T=new InnerTask(op,fileOrProlog,goal);
        
        T.task_bg();
        // runs forever
        Output=T;
      }
      break;
      
      /* networking */
      
      case ICONNECTION: {
        InnerTask A=(InnerTask)o(Input,1);
        // Prolog.dump("CONNECTION TO: "+host+":"+port);
        Output=A.newConnection();
      }
      break;
      
      case ASK_ISERVER: {
        ServicePair connection=(ServicePair)o(Input,1);
        
        Object[] bundle=E.getBundle();
        
        // Prolog.dump("TO_SERVER: "+new PortableTerm(bundle,0));
        Object in=InnerTask.ask_server(connection,bundle);
        if(InnerTask.EOS==in) {
          Output=AtomTable.Nothing;
        } else {
          
          bundle=(Object[])in;
          
          // Prolog.dump("FROM_SERVER: "+new PortableTerm(bundle,0));
          
          E.setBundle(bundle);
          Output=AtomTable.Yes;
        }
      }
      break;
      
      case IDISCONNECT: {
        ServicePair connection=(ServicePair)o(Input,1);
        int maybeStop=(int)i(Input,2);
        if(maybeStop==0)
          connection.disconnect();
        else
          connection.stop(); // does not stop the engine !!!
        Output=AtomTable.Yes;
      }
      break;
      
      case TO_BUNDLE: {
        Object[] bundle=E.getBundle();
        Output=bundle;
      }
      break;
      
      case FROM_BUNDLE: {
        // Prolog.dump(Input.getClass());
        // Output=Input;
        E.setBundle((Object[])Input);
        Output=AtomTable.Yes;
      }
      break;
      
      case DIRS_OR_FILES: {
        String dir=to_s(Input,1);
        dir=absolute_file_name(dir);
        File d=new File(dir);
        if(!d.isDirectory())
          break;
        boolean onlyDirs=0==i(Input,2);
        File[] ds=d.listFiles();
        ArrayList s=new ArrayList();
        for(int i=0;i<ds.length;i++) {
          if(onlyDirs&&ds[i].isDirectory())
            s.add(ds[i].getName());
          if(!onlyDirs&&ds[i].isFile())
            s.add(ds[i].getName());
        }
        Output=s.iterator();
      }
      break;
      
      case NEWER_FILE_OF: {
        String s1=to_s(Input,1);
        String s2=to_s(Input,2);
        s1=absolute_file_name(s1);
        s2=absolute_file_name(s2);
        File f1=new File(s1);
        File f2=new File(s2);
        boolean b1=f1.exists();
        boolean b2=f2.exists();
        if(b1&b2) {
          long t1=f1.lastModified();
          long t2=f2.lastModified();
          String winner=t2>=t1?s2:s1;
          Output=the(winner);
        } else
          Output="no";
      }
      break;
      
      case ABSOLUTE_FILE: {
        String path=to_s(Input,1);
        Output=the(absolute_file_name(path));
      }
      break;
      
      case SAVE_PROLOG: {
        // buggy
        String fname=to_s(Input,1);
        Prolog P=E.prolog;
        Prolog.dump(P.atomTable.rinfo());
        int ok=Transport.toFile(fname,P)?1:0;
        Output=new Integer(ok);
      }
      break;
      
      case LOAD_PROLOG: {
        // buggy
        String fname=to_s(Input,1);
        Prolog P=(Prolog)Transport.fromFile(fname);
        Output=P;
        Prolog.dump(P.atomTable.rinfo());
      }
      break;
      
      case TERM_TO_FILE: {
        String key=to_s(Input,1);
        Object[] bundle=E.getBundle();
        Output=AtomTable.Nothing;
        if(null==bundle)
          break;
        if(Transport.toFile(key,bundle))
          Output=AtomTable.Yes;
        else
          E.setBundle(bundle);
      }
      break;
      
      case TERM_FROM_FILE: {
        Output=AtomTable.Nothing;
        String key=to_s(Input,1);
        Object value=Transport.fromFile(key);
        if(null==value)
          break;
          
        // Prolog.dump("GOT: "+value+":"+value.getClass());
        Object[] bundle=(Object[])value;
        E.setBundle(bundle);
        Output=AtomTable.Yes;
      }
      break;
      
      case FILE_OP: {
        boolean ok=false;
        int op=(int)i(Input,1);
        String f=to_s(Input,2);
        // ensures user.dir is used - tested on OS X Lion
        f=absolute_file_name(f);
        if(null==f)
          op=-1;
        switch(op) {
          case 0: // make_directory
            ok=(new File(f)).mkdirs();
          break;
          case 1: {// delete_directory
            File file=new File(f);
            // ok=file.isDirectory()&&file.setWritable(true)&&file.delete();
            ok=file.isDirectory()&&file.delete();
          }
          break;
          case 2: {// delete_file
            File file=new File(f);
            // ok=file.isFile()&&file.setWritable(true)&&file.delete();
            ok=file.isFile()&&file.delete();
          }
          break;
          case 3: // rename_file
            String f2=to_s(Input,3);
            f2=absolute_file_name(f2);
            ok=(new File(f)).renameTo(new File(f2));
          break;
          
          default:
            Interact.warnmes("bad file operation: "+op+" on: "+f);
        }
        Output=ok?new Integer(1):new Integer(0);
      }
      break;
      
      case SYSTEM:
        Output=new Integer(system(to_s(Input,1)));
      break;
      
      case NEW_JAVA_CLASS: {
        String className=to_s(Input,1);
        Output=PrologReflection.new_java_class(className);
      }
      break;
      
      case NEW_JAVA_OBJECT: {
        Class cls=(Class)o(Input,1);
        Object cArgs=o(Input,2);
        Output=PrologReflection.new_java_object(cls,cArgs);
      }
      break;
      
      case INVOKE_JAVA_METHOD: {
        // Prolog.dump("invoke_java_method: "+Input);
        Object maybeClass=o(Input,1);
        Object obj=o(Input,2);
        String methodName=to_s(Input,3);
        Object mArgs=o(Input,4);
        Output=PrologReflection.invoke_java_method(maybeClass,obj,methodName,
            mArgs);
      }
      break;
      
      case GET_JAVA_FIELD_HANDLE: {
        Object obj=o(Input,1);
        String fieldName=to_s(Input,2);
        Output=PrologReflection.get_java_field_handle(obj,fieldName);
      }
      break;
      
      case HUB: {
        Output=new Hub();
      }
      break;
      
      case NEW_LOGIC_THREAD: {
        Hub hub=(Hub)o(Input,1);
        Object goal=o(Input,2);
        
        int clone=(int)i(Input,3);
        
        Object source=o(Input,4);
        if(AtomTable.Nothing.equals(source))
          source=E.prolog; // reuse this prolog by default
        // Prolog.dump("HERE="+E.prolog);
        LogicThread T=null;
        // try {
        T=LogicThread.new_interactor(source,hub,clone);
        hub.toStop.push(T);
        // } catch(Exception e) {
        // e.printStackTrace();
        // break;
        // }
        // Prolog.dump("THERE="+T);
        // if(Interact.verbosity>2)
        // Interact.log(Thread.activeCount()+" THREADS: new started with clone="
        // +clone+" goal="+goal);
        
        // if returned, T can be stopped !!!
        Output=T.tell_interactor(goal); // starts thread
        
      }
      break;
      
      case COMPARE0: {
        Object A=o(Input,1);
        Object B=o(Input,2);
        // this handles recursive comparison of Funs as well
        int cmp=Fun.compare_simple(A,B);
        if(0==cmp)
          Output="=";
        else
          Output=(cmp>0)?">":"<";
      }
      break;
      
      case GET_CMD_LINE_ARGS: {
        int discard=(int)i(Input,1);
        
        ObjectStack s=Interact.extractCmds(Interact.args);
        // unreversed
        if(discard>0)
          Interact.args=null;
        Output=s.toList();
      }
      break;
      
      case POP_CMD_ARG: {
        int op=(int)i(Input,1);
        if(0==op) { // POP
          if((null==Interact.argStack)||Interact.argStack.isEmpty())
            Output=AtomTable.Nothing;
          else
            Output=the(Interact.argStack.pop());
        } else if(1==op) { // PUSH
          Interact.argStack.push(o(Input,2));
        } else if(2==op) { // ADD
          Interact.argStack.add(o(Input,2));
        } else
          Interact.warnmes("bad data in CMD_ARG op => "+Input);
      }
      break;
      
      // uses Iterator
      case CURRENT_SYMBOLS: {
        int userSyms=(int)i(Input,1);
        int start=0;
        int end=E.prolog.atomTable.getSYSEND();
        if(userSyms>0) {
          start=E.prolog.atomTable.getSYSEND();
          end=0; // means end of syms
        }
        Output=E.prolog.atomTable.getSymList(start,end);
      }
      break;
      
      case PATH_OP: {
        int op=(int)i(Input,1);
        switch(op) {
          case 0: {
            if(Interact.pathStack.isEmpty())
              Output=AtomTable.Nothing;
            else
              Output=the(Interact.pathStack.pop());
          }
          break;
          case 1: {
            if(Interact.pathStack.isEmpty())
              Output=AtomTable.Nothing;
            else
              Output=the(Interact.pathStack.peek());
          }
          case 2: {
            String fname=to_s(Input,2);
            Interact.pathStack.push(fname);
          }
          break;
          case 3: {
            String fname=to_s(Input,2);
            Interact.pathStack.add(fname);
          }
          break;
          default:
            Interact.warnmes("bad path_op: "+op);
        }
      }
      break;
      
      case PATH_ELEMENTS: {
        Output=new ObjectStack(Interact.pathStack).reverse().toList();
      }
      break;
      
      case CLEAR_PATH: {
        Interact.pathStack=new ObjectStack();
      }
      break;
      
      case GVARS: {
        // uses Iterator
        Output=Prolog.gvarTable.getKeys();
      }
      break;
      
      case SLEEP_NS: {
        int ns=(int)i(Input,1);
        Interact.sleep_ns(ns);
      }
      break;
      
      // to avoid RECLIMIT on large lists
      case ATOMIC_LIST_CONCAT: {
        Object xs=o(Input,1);
        Object[] os=ObjectStack.fromList(xs).toArray();
        StringBuffer buf=new StringBuffer();
        for(int i=0;i<os.length;i++) {
          buf.append(os[i]);
        }
        Output=buf.toString();
      }
      break;
      
      case OPEN_LIST: {
        ObjectStack answers=new ObjectStack();
        Output=answers;
      }
      break;
      
      case LIST_ADD: {
        ObjectStack answers=(ObjectStack)o(Input,1);
        Object answer=o(Input,2);
        answers.push(answer);
        Output=AtomTable.Yes;
      }
      break;
      
      case CLOSE_LIST: {
        ObjectStack answers=(ObjectStack)o(Input,1);
        Output=answers.toList();
      }
      break;
      
      case PROFILE_ON_OFF: {
        int on=(int)i(Input,1);
        //Runtime.getRuntime().traceMethodCalls(1==on);
      }
      break;
      
      case SYMTEST: {
        E.prolog.atomTable.symtest(o(Input,1));
        // System.out.println("thread="+Thread.currentThread().getId());
      }
      break;
      
      case THREAD_ID: {
        long id=Thread.currentThread().getId();
        Output=BigInteger.valueOf(id);
      }
      break;
      
      case TO_UPPER_CASE: {
        String s=to_s(Input,1);
        Output=s.toUpperCase();
      }
      break;
      
      case TO_LOWER_CASE: {
        String s=to_s(Input,1);
        Output=s.toLowerCase();
      }
      break;
      
      case CONCATENATE_FILES: // concatenate list of files
        ObjectStack Fs=ObjectStack.fromList(o(Input,1));
        String F=to_s(Input,2);
        // Interact.println(Fs+F);
        concatenate_files(Fs,F);
      break;
      
      case SUBST_ATOM: {
        String x=to_s(Input,1);
        String y=to_s(Input,2);
        String s=to_s(Input,3);
        Output=s.replace(x,y);
      }
      break;
      
      case TO_RSTRING: {
        Object t=o(Input,1);
        Output=to_rstring(t);
      }
      break;
      
      case OBJECT_TO_QUOTED_STRING: {
        Object t=o(Input,1);
        t=new Fun("x",t);
        String s=t.toString();
        int l=s.length();
        
        Output=s.substring(2,l-1);
      }
      break;
      
      case GETENV: {
        String key=to_s(Input,1);
        Output=the(System.getenv(key));
      }
      break;
      
      case GETENVS: {
        Output=the(System.getenv().keySet().iterator());
      }
      break;
      
      case EXPAND_ENV: {
        String s=to_s(Input,1);
        Output=Interact.expand_env(s);
      }
      break;
      
      case GET_WORKING_DIRECTORY: {
        String dir=System.getProperty("user.dir");
        if(null==dir)
          dir=".";
        Output=dir;
      }
      break;
      
      case SET_WORKING_DIRECTORY: {
        String s=to_s(Input,1);
        s=absolute_file_name(s);
        File f=new File(s);
        if(f.exists()&&f.isDirectory())
          System.setProperty("user.dir",s);
        else
          s=null;
        Output=the(s);
      }
      break;
      
      case GET_ENGINE_OBJECT: {
        int e=(int)i(Input,1);
        if(0==e)
          Output=the(E);
        else {
          LogicEngine M=E.getEngine(e);
          Output=the(M);
        }
      }
      break;
      
      case IS_ENGINE: {
        int e=(int)i(Input,1);
        if(0==e)
          Output=new Integer(1);
        else {
          LogicEngine M=E.getEngine(e);
          int ok=(null==M)?0:1;
          Output=new Integer(ok);
        }
      }
      break;
      
      case ENGINE_GC: {
        // Interact.dump("current engine "+E);
        LogicEngine root=E.getParent();
        if(null==root)
          root=E;
        ObjectStack Es=LogicEngine.getEnginesBelow(root);
        
        // Interact.dump("parent, child and sibling engines "+Es);
        while(!Es.isEmpty()) {
          LogicEngine M=(LogicEngine)(Es.pop());
          if(M==E||M==root||M.isProtected())
            continue; // no suicide, please !!!
          // Interact.dump("stopping engine "+M);
          M.stop();
        }
      }
      break;
      
      case TO_SET: {
        Object xs=o(Input,1);
        Output=ObjectStack.fromList(xs).toSet();
      }
      break;
      
      case FROM_SET: {
        LinkedHashSet set=(LinkedHashSet)o(Input,1);
        Object[] os=set.toArray();
        ObjectStack s=new ObjectStack(os);
        Output=s.toList();
      }
      break;
      
      case VARNUMBERS: {
        Object t=o(Input,1);
        Output=varnumbers(t,maxvar(t)+1);
      }
      break;
      
      // add break before this !!!
      default:
        Interact.warnmes("bad Op in xcall:"+Op);
    }
    
    if(null==Output)
      return null;
    Output=maybeInteger(Output);
    // just for benchmarking purposes
    // if(Output instanceof Number)
    // unserialize(serialize(Output));
    return Output;
  }
  
  Long thread_id() {
    return new Long(Thread.currentThread().getId());
  }
  
  Object xtest(Object S) {
    // Tokenizer.xtest(S);
    Prolog.dump("input:"+S);
    // Ops.xtest();
    PreTokenizer.ytest(S);
    // ObjectStack s=new ObjectStack();
    // s.push("a");s.push("b");s.push("c");s.push("d");s.push("e");s.push("f");
    // Prolog.dump(s);
    // s.reverse();
    // Prolog.dump(s);
    // Toks.xtest(S);
    return "DONE";
  }
  
  public static final ObjectStack get_field_names(String className) {
    ObjectStack fs=new ObjectStack();
    try {
      Class cls=Class.forName(className);
      Field fieldlist[]=cls.getDeclaredFields();
      for(int i=0;i<fieldlist.length;i++) {
        Field fld=fieldlist[i];
        fs.push(fld.getName());
      }
    } catch(Exception e) {
      Interact.warnmes("error in get_field_names: "+e);
    }
    return fs;
  }
  
}