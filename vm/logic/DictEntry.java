package vm.logic;

/**
 * A dictionary entry.
 */
class DictEntry implements Stateful {
  
  private static final long serialVersionUID = 222L;
  
  final int pred;

  final int fun;

  int val; // only this can be updated

  DictEntry(int pred,int fun,int val){
    this.pred=pred;
    this.fun=fun;
    this.val=val;
  }

  DictEntry(DictEntry entry){
    this.pred=entry.pred;
    this.fun=entry.fun;
    this.val=entry.val;

  }

  void xsetVal(int val) {
    this.val=val;
  }

  /** Do the specified keys match this entry? */
  final boolean isFound(int pred,int fun) {
    return(this.pred==pred&&this.fun==fun);
  }

  public String toString() {
    String s="<"+pred+","+fun+">-->"+val;
    s="<"+pred+"/"+Defs.GETARITY(pred)+","
        +fun+"/"+Defs.GETARITY(fun)+">"+"==>"+val;
    return s;
  }
}// End class DictEntry

