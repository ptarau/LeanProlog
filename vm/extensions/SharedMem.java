package vm.extensions;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.channels.FileLock;
import java.nio.channels.FileLockInterruptionException;

import vm.logic.Interact;

public class SharedMem {
  
  // interface
  
  public static boolean put(String dirname,String key,Object value) {
    byte[] bs=Interact.toBytes(value);
    SharedMem m=newSharedMem(toMemName(dirname,key),bs.length);
    if(null==m)
      return false;
    if(!m.lock())
      return false;
    m.buf.clear();
    m.buf.put(bs);
    // m.buf.force(); // ??
    if(!m.unlock())
      return false;
    return true;
  }
  
  public static String toMemName(String dirname,String key) {
    return dirname+"/"+key+".shm";
  }
  
  public static Object get(String dirname,String key) {
    String fname=toMemName(dirname,key);
    File f=new File(fname);
    boolean first=true;
    while(!f.exists()) {
      if(first)
        Interact.pp("starting to wait for: "+fname);
      first=false;
      Interact.sleep_ms(100);
    }
    
    SharedMem m=getSharedMem(fname);
    
    if(null==m) {
      Interact.errmes("shm_get: no such key, "+key);
      return null;
    }
    if(m.size()>=1L<<31) {
      Interact.errmes("shm_get: size="+m.size()+", too large for "+key);
      return null;
    }
    if(!m.lock()) {
      Interact.errmes("shm_get: lock failed on "+key);
      return null;
    }
    // m.buf.flip(); DO NOT DO THIS !!!
    byte[] bs=new byte[(int)m.size()];
    // Prolog.dump("SharedMem size="+m.size()+"buf="+m.buf);
    m.buf.get(bs);
    if(!m.unlock()) {
      Interact.errmes("shm_get: unlock failed on "+key);
      return null;
    }
    
    Object O=Interact.fromBytes(bs);
    
    // Prolog.dump("HERE="+key+"=>"+O);
    
    return O;
    
  }
  
  public static boolean remove(String dirname,String key) {
    String name=toMemName(dirname,key);
    SharedMem m=getSharedMem(name);
    if(null==m)
      return false;
    if(!m.lock())
      return false;
    // if(!m.unlock()) return false;
    try {
      m.chan.close();
    } catch(IOException e) {
      Interact.errmes("error removing shared memory: "+key,e);
    }
    (new File(name)).delete();
    return true;
  }
  
  // implementation
  
  private final FileChannel chan;
  
  private final MappedByteBuffer buf;
  
  private FileLock memlock;
  
  private static SharedMem newSharedMem(String name,long size) {
    SharedMem m=new SharedMem(name,size);
    if(m.chan==null)
      return null;
    return m;
  }
  
  private static SharedMem getSharedMem(String name) {
    return newSharedMem(name,0L);
  }
  
  private SharedMem(String name,long len){
    this.chan=newChan(name,len);
    if(this.chan!=null) {
      this.buf=newBuf(this.chan,size());
    } else {
      this.buf=null;
    }
  }
  
  private boolean lock() {
    if(null!=this.memlock)
      return true; // already holding it
    while(null==this.memlock) {
      try {
        this.memlock=this.chan.lock();
      } catch(FileLockInterruptionException waitMore) {
        continue;
      } catch(Exception e) {
        Interact.errmes("locking error",e);
        return false;
      }
    }
    return true;
  }
  
  private boolean unlock() {
    if(null==this.memlock)
      return false;
    if(!this.memlock.isValid())
      return false;
    try {
      this.memlock.release();
      this.memlock=null;
      return true;
    } catch(Exception e) {
      Interact.errmes("unlocking error",e);
      return false;
    }
  }
  
  private static FileChannel newChan(String name,long len) {
    try {
      File f=new File(name);
      RandomAccessFile raf=new RandomAccessFile(f,"rw");
      if(len>0)
        raf.setLength(len);
      return raf.getChannel();
    } catch(IOException e) {
      Interact.errmes("unable to create or access shared memory <"+name+">",e);
      return null;
    }
  }
  
  public long size() {
    try {
      return this.chan.size();
    } catch(Exception e) {
      Interact.errmes("error in SharedMem size()",e);
      return -1L;
    }
  }
  
  private static MappedByteBuffer newBuf(FileChannel chan,long len) {
    try {
      MappedByteBuffer fbuf=chan.map(MapMode.READ_WRITE,0,len);
      return fbuf;
    } catch(IOException e) {
      Interact.errmes("shared memory mapping error",e);
      return null;
    }
  }
  
}
