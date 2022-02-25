package vm.logic;

import java.math.BigDecimal;

/**
 *  Implements external representations of Prolog
 *  compound terms - a functor of the form Symbol / Arity
 *  Uses full hashing on all arguments so if that if is GROUND 
 *  it can be used as a key in an Map
 */
public class Fun implements Stateful,Comparable {
  private static final long serialVersionUID=222L;
  
  /*
  public static Fun fun(Object name,Object arg) {
    return new Fun(name,arg);
  }
  
  public static Var var(int i) {
    return new Var(i);
  }
  */
  
  public Fun(Object name,Object[] args){
    this.name=name;
    this.args=args;
  }
  
  public Fun(Object name,int n){
    this.name=name;
    this.args=new Object[n];
    // for(int i=0;i<n;i++) {
    //
    // }
  }
  
  public Fun(Object name,Object a1){
    this.name=name;
    this.args=new Object[1];
    args[0]=a1;
  }
  
  public Fun(Object name,Object a1,Object a2){
    this.name=name;
    this.args=new Object[2];
    args[0]=a1;
    args[1]=a2;
  }
  
  public Fun(Object name,Object a1,Object a2,Object a3){
    this.name=name;
    this.args=new Object[3];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
  }
  
  public Fun(Object name,Object a1,Object a2,Object a3,Object a4){
    this.name=name;
    this.args=new Object[4];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
    args[3]=a4;
  }
  
  public Fun(Object name,Object a1,Object a2,Object a3,Object a4,Object a5){
    this.name=name;
    this.args=new Object[5];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
    args[3]=a4;
    args[4]=a5;
  }
  
  public Fun(Object name,Object a1,Object a2,Object a3,Object a4,Object a5,
      Object a6){
    this.name=name;
    this.args=new Object[6];
    args[0]=a1;
    args[1]=a2;
    args[2]=a3;
    args[3]=a4;
    args[4]=a5;
    args[5]=a6;
  }
  
  final public Object name;
  
  final public Object[] args;
  
  /** gets arg in 1..arity range */
  public Object getArg(int i) {
    return this.args[i-1];
  }
  
  /** sets arg in in 1..arity range */
  public void setArg(int i,Object A) {
    this.args[i-1]=A;
  }
  
  public int hashCode() {
    return args.length<<8+name.hashCode();
  }
  
  public boolean equals(Object O) {
    if(!(O instanceof Fun))
      return false;
    Fun F=(Fun)O;
    return args.length==F.args.length&&name.equals(F.name);
  }
  
  public int deepHashCode() {
    int l=args.length;
    int k=name.hashCode()^(l<<2);
    for(int i=0;i<l;i++) {
      Object X=args[i];
      int j;
      if(null==X) {
        j=0;
        Interact.warnmes("null arg in Fun="+name+"/"+l+":"+(i+1));
      } else
        j=(X instanceof Fun)?((Fun)X).deepHashCode():X.hashCode();
      k+=(k<<4)+j;
    }
    return k;
  }
  
  public boolean deepEquals(Object O) {
    if(!(O instanceof Fun))
      return false;
    Fun F=(Fun)O;
    if(!(args.length==F.args.length&&name.equals(F.name)))
      return false;
    for(int i=0;i<args.length;i++) {
      if(args[i] instanceof Fun&&F.args[i] instanceof Fun) {
        if(!((Fun)args[i]).deepEquals(F.args[i]))
          return false;
      } else if(!args[i].equals(F.args[i]))
        return false;
    }
    return true;
  }
  
  public static String toQuoted(String s) {
    if(null==s)
      return s;
    char quote='\'';
    
    // empty string needs quotes!
    if(s.length()==0)
      return quote+s+quote;
    
    char a=s.charAt(0);
    
    // check if already quoted
    if(a==quote) {
      if(1==s.length())
        s=quote+s+s+quote; // ' ==> ''''
      return s;
    }
    boolean needsq=false;
    if((Character.isUpperCase(a)||Character.isDigit(a)||'_'==a))
      needsq=true;
    else if(s.equals("!")) { /* without quotes */
    } else
      for(int i=0;i<s.length();i++) {
        char c=s.charAt(i);
        if(Character.isLetterOrDigit(c)||c=='_')
          continue;
        needsq=true;
        break;
      }
    if(needsq) {
      StringBuffer sb=new StringBuffer();
      for(int i=0;i<s.length();i++) {
        char c=s.charAt(i);
        if(quote==c) {
          sb.append(quote); // double it
        }
        sb.append(c);
      }
      s="\'"+sb+"\'";
    }
    return s;
  }
  
  private final static String snull="\'$null\'";
  
  private String maybeNull(Object O) {
    if(null==O)
      return snull;
    if(O instanceof String)
      return toQuoted((String)O);
    return O.toString();
  }
  
  public String toString() {
    StringBuffer buf=new StringBuffer();
    
    if(args.length==2
        &&("/".equals(name)||"-".equals(name)||"+".equals(name)||"="
            .equals(name))) {
      buf.append("(");
      buf.append(maybeNull(args[0]));
      buf.append(" "+name+" ");
      buf.append(maybeNull(args[1]));
      buf.append(")");
    } else if(args.length==2&&".".equals(name)) {
      buf.append('[');
      {
        Object tail=this;
        for(boolean first=true;;) {
          if("[]".equals(tail))
            break;
          if(!(tail instanceof Fun)) {
            buf.append('|');
            buf.append(maybeNull(tail));
            break;
          }
          Fun list=(Fun)tail;
          if(!(list.args.length==2&&".".equals(list.name))) {
            buf.append('|');
            buf.append(maybeNull(tail));
            break;
          } else {
            if(first)
              first=false;
            else
              buf.append(',');
            buf.append(maybeNull(list.args[0]));
            tail=list.args[1];
          }
        }
      }
      buf.append(']');
    } else if(args.length==1&&"$VAR".equals(name)) {
      buf.append("_"+args[0]);
    } else {
      String qname=maybeNull(name);
      buf.append(qname);
      buf.append("(");
      for(int i=0;i<args.length;i++) {
        Object O=args[i];
        buf.append(maybeNull(O));
        if(i<args.length-1)
          buf.append(",");
      }
      buf.append(")");
    }
    return buf.toString();
  }
  
  public int compareTo(Object O) {
    if(O instanceof Fun) {
      Fun F=(Fun)O;
      int cmp=this.args.length-F.args.length;
      if(0!=cmp)
        return cmp;
      cmp=compare_simple(name,F.name);
      if(0!=cmp)
        return cmp;
      for(int i=0;i<args.length;i++) {
        Object A=args[i];
        Object B=F.args[i];
        if(A instanceof Fun)
          cmp=((Fun)A).compareTo(B);
        
        else if(B instanceof Fun)
          cmp=-((Fun)B).compareTo(A);
        
        else
          cmp=compare_simple(A,B);
        if(0!=cmp)
          return cmp;
      }
      return 0; // not found as being different
    } else
      return -1;
  }
  
  public static int compare_simple(Object A,Object B) {
    int cmp=0;
    if(A==null)
      cmp=(B==null)?0:1;
    else if(B==null)
      cmp=(A==null)?0:-1;
    else if(A instanceof Var)
      cmp=((Var)A).compareTo(B);
    else if(B instanceof Var)
      cmp=-((Var)B).compareTo(A);
    else if(A instanceof Comparable&&A.getClass()==B.getClass())
      // fun gets compared here !!!
      cmp=((Comparable)A).compareTo(B);
    else if(A instanceof Number&&B instanceof Number) {
      BigDecimal XA=new BigDecimal(A.toString());
      BigDecimal XB=new BigDecimal(B.toString());
      cmp=XA.compareTo(XB);
    } else if(A instanceof Number) {
      cmp=-1;
    } else if(B instanceof Number) {
      cmp=1;
    } else {
      cmp=A.getClass().getName().compareTo(B.getClass().getName());
    }
    return cmp;
  }
}