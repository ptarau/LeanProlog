package vm.extensions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import vm.logic.Fun;
import vm.logic.Interact;
import vm.logic.PrologException;
import vm.logic.Stateful;

/**
 * Adds reflection based Builtins
 */
public class PrologReflection implements Stateful {
  
  public PrologReflection(){
  }
  
  /* Reflection related */

  public static final Object new_java_class(String className) {
    
    Class C=null;
    try {
      if(null==className)
        return C;
      C=Class.forName(className);
    } catch(Exception e) {
      if("boolean".equals(className))
        C=Boolean.TYPE;
      else if("char".equals(className))
        C=Character.TYPE;
      else if("byte".equals(className))
        C=Byte.TYPE;
      else if("short".equals(className))
        C=Short.TYPE;
      else if("int".equals(className))
        C=Integer.TYPE;
      else if("long".equals(className))
        C=Long.TYPE;
      else if("float".equals(className))
        C=Float.TYPE;
      else if("double".equals(className))
        C=Double.TYPE;
      else if("void".equals(className))
        C=Void.TYPE;
      else {
        Interact.warnmes("cannot create class:"+className,e);
      }
    }
    return C;
  }
  
  private static final Class[] getTypes(Object[] Os) {
    if(null==Os)
      return null;
    Class[] Cs=new Class[Os.length];
    for(int i=0;i<Os.length;i++) {
      Object O=Os[i];
      Class C;
      if(null==O||"$null".equals(O)) {
        Os[i]=null;
        C=null;
        // return null;
      } else if(O instanceof Integer)
        C=Integer.TYPE;
      else if(O instanceof Double)
        C=Double.TYPE;
      else
        C=O.getClass();
      Cs[i]=C;
    }
    return Cs;
  }
  
  private static final Object[] getArgs(Object obj) throws PrologException {
    
    Object[] args;
    if("void".equals(obj))
      args=null;
    else
      args=((Fun)obj).args;
    return args;
  }
  
  public static final Object new_java_object(Class cls,Object cArgs) {
    try {
      if(null==cls)
        return null;
      Object[] args=getArgs(cArgs);
      Class[] types=getTypes(args);
      // Interact.println("constructor args="+args+" types="+types);
      Object O=instantiate(cls,args,types);
      if(null==O)
        return null;
      return O;
    } catch(Throwable e) {
      Interact.errmes("cannot create object in: "+cls,e);
      return null;
    }
  }
  
  public static final Object invoke_java_method(Object maybeClass,Object obj,
      String methodName,Object mArgs) {
    Class cls=null;
    
    Object[] args=null;
    Class[] parameterTypes=null;
    try {
      if(null==obj)
        return null;
      if(null==maybeClass)
        return null;
      if(maybeClass instanceof Class) {
        cls=(Class)maybeClass;
        
      }
      if(null==cls) {
        cls=(obj instanceof Class)?(Class)obj:obj.getClass();
      }
      
      if(null==methodName)
        return null;
      args=getArgs(mArgs);
      parameterTypes=getTypes(args);
      // Interact.println(methodName+": invoke args="+args+" types="+parameterTypes+" obj="+obj);
      Object res=invokeTheMethod(cls,obj,methodName,args,parameterTypes);
      return res;
    } catch(Throwable e) {
      Interact.errmes("exception invoking method: "+methodName+"() on: "+obj,e);
      return null;
    }
  }
  
  public static final Object get_java_field_handle(Object obj,String fieldName) {
    
    try {
      
      if(null==obj)
        return null;
      
      if(null==fieldName)
        return null;
      // Interact.println(fieldName+"in obj="+obj);
      Class C;
      if(obj instanceof Class)
        C=(Class)obj;
      else
        C=obj.getClass();
      Field F=C.getField(fieldName);
      Object res=F;
      return res;
    } catch(Throwable e) {
      Interact.errmes("cannot find field: "+fieldName+"() of: "+obj,e);
      return null;
    }
  }
  
  public final boolean delete_java_class(Object xref) {
    Interact.println("delete_java_class is deprecated: "+xref);
    return true;
  }
  
  public final boolean delete_java_object(Object xref) {
    Interact.println("delete_java_object is deprecated: "+xref);
    return true;
  }
  
  /**
    * Instantiate an Object of Given Class
    * @param currentClass is the Class whose object is instantiated
    * @param args is the array of Objects passed as arguments to the constructor
    * @param parameterTypes is the array of Classes which are the types for args and used to find the constructor we are looking for
    * @return The Object of the currentClass created with the void constructor
    */
  public static final Object instantiate(Class currentClass,Object[] args,
      Class[] parameterTypes) throws Exception {
    Object res=null;
    if(args==null) {
      res=currentClass.newInstance();
    } else {
      try {
        Constructor theConstructor=currentClass.getConstructor(parameterTypes);
        res=theConstructor.newInstance(args);
      } catch(NoSuchMethodException e) {
        boolean found=false;
        Constructor[] theConstructors=currentClass.getConstructors();
        for(int x=0;x<theConstructors.length;x++) {
          if(theConstructors[x].getParameterTypes().length==parameterTypes.length) {
            Interact.traceln("length: "
                +theConstructors[x].getParameterTypes().length);
            Constructor theConstructor=theConstructors[x];
            try {
              res=theConstructor.newInstance(args);
              found=true;
              break;
            } catch(IllegalArgumentException iae) {
              // Prolog.dump("Trying to instantiate ...."+theConstructor,iae);
            } catch(NullPointerException npe) {
              // Prolog.dump("Trying to instantiate ...."+theConstructor,iae);
            }
          }
        }
        if(!found)
          throw e;
      }
    }
    return res;
  }
  
  /**
   * Invokes a Given Method of Given Object with given arguments	
   * @param theCurrent is the object (or class in case of static method) whose method is invoked
   * @param methodName is the name of method to be invoked
   * @param args is the array of Objects passed as arguments to the method
   * @param parameterTypes is the array of Classes which are the types for args and used to find the method we are looking for
   * @return The Object returned by given method
   */
  public static final Object invokeTheMethod(Class theClass,Object theCurrent,
      String methodName,Object[] args,Class[] parameterTypes) throws Exception {
    try {
      return tryMethod(theClass,theCurrent,methodName,args,parameterTypes);
    } catch(Exception e) {
      try {
        if(theCurrent instanceof Class)
          return tryMethod(theClass.getClass(),theClass,methodName,args,
              parameterTypes);
      } catch(Exception ce) {
      }
      Interact.warnmes("method invocation error: CLASS=>"+theClass+", OBJECT=>"
          +theCurrent+", method: "+methodName);
      throw e;
    }
  }
  
  private static final Object tryMethod(Class currentClass,
      Object currentObject,String methodName,Object[] args,
      Class[] parameterTypes) throws Exception {
    
    // Interact.println("entering invoke: CLASS=>"+currentClass+", OBJECT=>"+currentObject+", method: "+methodName);
    
    Object res=null;
    Method theMethod=null;
    
    if(args==null) {
      theMethod=currentClass.getMethod(methodName,(Class[])null);
      res=theMethod.invoke(currentObject,(Object[])null);
    } else {
      try {
        theMethod=currentClass.getMethod(methodName,parameterTypes);
        res=theMethod.invoke(currentObject,args);
      } catch(NoSuchMethodException e) {
        boolean found=false;
        Method[] theMethods=currentClass.getMethods();
        for(int x=0;x<theMethods.length;x++) {
          theMethod=theMethods[x];
          if(methodName.equals(theMethod.getName())
              &&theMethod.getParameterTypes().length==parameterTypes.length) {
            try {
              // Prolog.dump("Trying to invoke ...."+theMethod+"/"+parameterTypes.length);
              res=theMethod.invoke(currentObject,args);
              found=true;
              break;
            } catch(IllegalArgumentException iae) {
              // trying more
              // Prolog.dump("Bad arguments trying to invoke ...."+theMethod+"=>"+iae);
            } catch(NullPointerException npe) {
              // Prolog.dump("Trying to instantiate ...."+theConstructor,iae);
            } catch(InvocationTargetException te) {
              Interact.errmes("error in trying Java method: ",
                  te.getTargetException());
              return res;
            }
          }
        }
        if(!found)
          throw e;
      } catch(InvocationTargetException te) {
        Interact.errmes("error in calling Java method: ",
            te.getTargetException());
        return res;
      }
    }
    return res;
  }
}
