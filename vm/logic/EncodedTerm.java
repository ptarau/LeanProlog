package vm.logic;

/**
 * Contains a relocatable term obtained by extracting from a
 * HeapStack a segment <from>..<to> resulting from a copy_term operation.
 */
final class EncodedTerm implements Stateful {
  private static final long serialVersionUID=222L;
  
  private long[] cells;
  
  /**
   *  creates an EncodedTerm from a section of a heap
   *  known to represent, usually after copyTerm, a valid
   *  unique term - that starts at cells[1]
   */
  EncodedTerm(long[] heap,int from,int to){
    cells=new long[to-from+1];
    for(int i=from;i<to;i++) {
      cells[i-from+1]=encodeCell(from,heap[i]);
    }
    // Prolog.dump("EncodedTerm ["+from+".."+to+"]-->"+this);
  }
  
  /**
   * Creates a simple EncodedTerm by putting a given
   * constant as cell[1]
   */
  EncodedTerm(long atomic){
    cells=new long[2];
    // cells[0]=0; self ref variable - not used for now
    cells[1]=atomic;
    // Prolog.dump("EncodedTerm atomic--> "+this);
  }
  
  /**
   * copies the cells from an array 
   * assumed to contain valid cells
   */
  EncodedTerm(long cells[]){
    this.cells=cells;
  }
  
  /**
   * returns the array of cells
   */
  final long[] getCells() {
    return cells.clone(); // could be a copy !!!
  }
  
  final int size() {
    return cells.length;
  }
  
  /**
   * Builds a decoded copy of this term on a given
   * heap (array of ints) starting from given address
   * and returns the end of the term
   */
  final int decodeTerm(int from,long[] heap) {
    int to=from+cells.length-1;
    for(int i=from;i<to;i++) {
      heap[i]=decodeCell(from,cells[i-from+1]);
    }
    return to;
  }
  
  static final long encodeCell(int from,long v) {
    return (Defs.isVAR(v))?v-from+1:v;
  }
  
  static final long decodeCell(int from,long cells2) {
    return (Defs.isVAR(cells2))?cells2+from-1:cells2;
  }
  
  void destroy() {
    cells=null;
  }
  
  public String toString() {
    if(size()==0)
      return "[]";
    StringBuffer buf=new StringBuffer(4*size());
    buf.append("["+Defs.showCell(cells[0]));
    for(int i=1;i<size();i++) {
      buf.append(",");
      buf.append(Defs.showCell(cells[i]));
    }
    buf.append("]");
    return buf.toString();
  }
}