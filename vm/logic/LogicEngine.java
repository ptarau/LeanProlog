package vm.logic;

import java.util.Iterator;

public class LogicEngine extends HeapStack // implements Runnable
{
  private static final long serialVersionUID=222L;
  
  private static int instances=0;
  
  private final int instance;
  
  private int children=0;
  
  private LogicEngine parent=null;
  
  public final LogicEngine getParent() {
    return this.parent;
  }
  
  // private final ObjectMap engineTable=new ObjectMap();
  
  private final ObjectMap handleTable=new ObjectMap();
  
  // synchronized
  int addEngine(LogicEngine E) {
    if(children<0) {
      children=0;
      Interact.warnmes("children counter overflow, reset to 0");
    }
    int e=++children;
    int I=new Integer(e);
    
    handleTable.put(I,E);
    return e;
  }
  
  // synchronized
  public LogicEngine getEngine(int e) {
    Integer I=new Integer(e);
    LogicEngine E=(LogicEngine)handleTable.get(I);
    return E;
  }
  
  // to be called by stop()
  // synchronized
  private void removeEngine(int e) {
    Integer I=new Integer(e);
    LogicEngine E=(LogicEngine)this.handleTable.get(I);
    if(null==E) {
      
      Interact.errmes("removeEngine: pointing to null engine, handle="+I
          +". Are you trying to stop an engine already stopped?");
      
      return;
      
    }
    this.handleTable.remove(I);
    // E.removeChildren();
    ObjectStack s=getEnginesBelow(E);
    for(int i=0;i<s.size();i++) {
      LogicEngine child=(LogicEngine)s.at(i);
      child.stop();
    }
    E.handleTable.clear(); // ensures errors on unexpected refs
  }
  
  LogicEngine getAncestor() {
    LogicEngine p=this;
    while(null!=p.parent) {
      p=p.parent;
    }
    return p;
  }
  
  // synchronized
  public ObjectStack getEngines() {
    LogicEngine a=getAncestor();
    return getEnginesBelow(a);
  }
  
  public static ObjectStack getEnginesBelow(LogicEngine a) {
    ObjectStack es=new ObjectStack();
    getEngines(a,es);
    return es.reverse();
  }
  
  static void getEngines(LogicEngine e,ObjectStack es) {
    es.push(e);
    Iterator H=e.handleTable.keySet().iterator();
    while(H.hasNext()) {
      Integer h=(Integer)H.next();
      LogicEngine child=(LogicEngine)e.handleTable.get(h);
      getEngines(child,es);
    }
  }
  
  LogicEngine(Prolog prolog){
    super(prolog,1<<12); // 4096
    instance=instances++;
    this.prolog=prolog;
    this.codeStore=prolog.codeStore;
    this.dict=prolog.dict;
    
    this.unifyStack=new LongStack(1<<10); // 1024
    this.choice=new ChoicePointStack(this);
    this.trail=new TrailStack(this,choice);
    // establish otherwise circular references for local ease of use.
    this.choice.setTrail(trail);
    this.regs=new long[TEMPARGS+MAXREG+1];
    this.gc_flag=false;
    
  }
  
  public final Prolog prolog;
  
  private final CodeStore codeStore;
  
  private final Dict dict;
  
  private final LongStack unifyStack;
  
  private final ChoicePointStack choice;
  
  private final TrailStack trail;
  
  private EncodedTerm messageBox;
  
  private Object[] ioBundle;
  
  private static final int TEMPARGS=8;
  
  private final long[] regs;
  
  boolean isStopped=false;
  
  // private int currentEngine;
  
  // curInstr used by run() which need to have global access - to simplify
  // conversion ??
  private long curInstr;
  
  private int instrPtr;
  
  private int ires;
  
  /** Work Functor arity */
  private int arity;
  
  private int cutB; // cut back choice point.
  
  private long S;
  
  public boolean gc_flag;
  
  // ******INTERFACE INSTRUCTION OPERATIONS ********************
  private final int REGFIELD() {
    return LCGET(curInstr);
  }
  
  private final int LEFTFIELD() {
    return MCGET(curInstr);
  }
  
  // ******Register manipulation functions *********************
  private final long An() {
    return regs[LCGET(curInstr)];
  }
  
  private final void setAn(long val) {
    regs[LCGET(curInstr)]=val;
  }
  
  private final long Ai() {
    return regs[MCGET(curInstr)];
  }
  
  private final void setAi(long val) {
    regs[MCGET(curInstr)]=val;
  }
  
  /**
   * Get the value in a temporary register.
   * <p>
   * Tempoary regs use a negative offset from the end of the <tt>regs</tt>
   * array. Propose TEMPREGS = regs[regs.length - i];
   * 
   * @param i
   *          Index of the temporary register in the range 1 to TEMPREGS.
   * 
   */
  public final long X(int i) {
    return regs[regs.length-i];
  }
  
  /**
   * Set the value of a temporary register.
   * 
   * @return The register contents.
   */
  private final void setX(int xval,long val) {
    regs[regs.length-xval]=val;
  }
  
  final void INIT_INTERP(Object query) throws PrologException {
    choice.clear();
    trail.clear();
    clear();
    S=0;
    // Interact.println("cells:="+size());
    // Interact.println("regs:="+regs.length);
    regs[2]=newVar();
    
    regs[3]=atomTable.G_true;
    instrPtr=xx(prolog.codeStore.getStartPoint());
    
    // this may consume heap, should be last!!!!!!!!!!!!!
    regs[1]=queryTerm(query);
    
    cutB=choice.addChoicePoint(0,regs,3);
  }
  
  long queryTerm(Object query) throws PrologException {
    return decodedCopy((EncodedTerm)query);
  }
  
  private int op;
  
  final private long runInternal() throws PrologException {
    
    for(;;) {
      
      curInstr=codeStore.GETINSTR(instrPtr,0);
      op=CodeStore.GETINSTR_OP(curInstr);
      
      // prolog.dump("op="+op);
      switch(op) {
        case END:
          // destroy(); - may be reused?
          // Interact.println("END reached by "+this);
          stop();
          return 0;
          
        case UNIFY_VARIABLE:
          if(S!=0) {
            setAn(S++);
            instrPtr++;
            continue;
          }
          // drop thru to next case
        case WRITE_VARIABLE:
          setAn(newVar());
          instrPtr++;
          continue;
          
        case UNIFY_VALUE:
          if(S!=0) {
            if(!unify(An(),S)) {
              FAILURE();
              continue;
            }
            S++;
            instrPtr++;
            continue;
          }
          // drop thru to next case
        case WRITE_VALUE:
          pushTerm(An());
          instrPtr++;
          continue;
          
        case GET_STRUCTURE:
          xref=An();
          FDEREF();
          if(isVAR(xval)) {
            S=0;
            pushTerm(codeStore.GETFUN(instrPtr));
            setRef(xref,getHeapTop());
            trail.trailVarIf(xref);
            instrPtr+=2;
            continue;
          }
          if(xval!=codeStore.GETFUN(instrPtr)) {
            FAILURE();
            continue;
          }
          S=xref+1;
          instrPtr+=2;
          continue;
          
        case PUT_STRUCTURE:
          setAn(pushTerm(codeStore.GETFUN(instrPtr)));
          instrPtr+=2;
          continue;
          
        case MOVE_REG:
          setAn(Ai());
          instrPtr++;
          continue;
          
        case MOVE_REGx2:
          setAn(Ai());
          curInstr=codeStore.GETINSTR(instrPtr,1);
          setAn(Ai());
          instrPtr+=2;
          continue;
          
        case PUT_VARIABLE:
          setAn(newVar());
          setAi(An());
          instrPtr++;
          continue;
          
        case GET_VALUE:
          if(!unify(An(),Ai())) {
            FAILURE();
            continue;
          }
          instrPtr++;
          continue;
          
        case UNIFY_VAR_VAR:
          if(S!=0) {
            setAn(S++);
            setAi(S++);
            instrPtr++;
            continue;
          } // drop thru to next case
        case WRITE_VAR_VAR:
          setAn(newVar());
          setAi(newVar());
          instrPtr++;
          continue;
          
        case UNIFY_VAL_VAR:
          if(S!=0) {
            if(!unify(An(),S)) {
              FAILURE();
              continue;
            }
            S++;
            setAi(S++);
            instrPtr++;
            continue;
          }
          // drop thru to next case
        case WRITE_VAL_VAR:
          pushTerm(An());
          setAi(newVar());
          instrPtr++;
          continue;
          
        case UNIFY_VAL_VAL:
          if(S!=0) {
            if(!unify(An(),S)) {
              FAILURE();
              continue;
            }
            S++;
            if(!unify(Ai(),S)) {
              FAILURE();
              continue;
            }
            S++;
            instrPtr++;
            continue;
          }
          // drop thru to next case
        case WRITE_VAL_VAL:
          pushTerm(An());
          pushTerm(Ai());
          instrPtr++;
          continue;
          
        case UNIFY_VAR_VAL:
          if(S!=0) {
            setAn(S++);
            if(!unify(Ai(),S)) {
              FAILURE();
              continue;
            }
            S++;
            instrPtr++;
            continue;
          }
          // drop thru to next case
        case WRITE_VAR_VAL:
          setAn(newVar());
          pushTerm(Ai());
          instrPtr++;
          continue;
          
        case UNIFY_CONSTANT:
          if(S!=0) {
            xval=getRef(S++);
            xref=xval;
            FDEREF();
            
            if(isVAR(xval)) {
              setRef(xref,codeStore.GETFUN(instrPtr));
              trail.trailVarIf(xref);
            } else if(xval!=codeStore.GETFUN(instrPtr)) {
              FAILURE();
              continue;
            }
            instrPtr+=2;
            continue;
          }
          // drop thru to WRITE_CONSTANT.
          
        case WRITE_CONSTANT:
          pushTerm(codeStore.GETFUN(instrPtr));
          instrPtr+=2;
          continue;
          
        case GET_CONSTANT:
          // Bind VAR in An to constant.
          xref=An();
          FDEREF();
          
          if(isVAR(xval)) {
            setRef(xref,codeStore.GETFUN(instrPtr)); // fill the unb. VAR with
            // the const.
            trail.trailVarIf(xref);
          } else if(xval!=codeStore.GETFUN(instrPtr)) { // if already bound
            // fail
            // if not same value.
            FAILURE();
            continue;
          }
          instrPtr+=2;
          continue;
          
        case PUT_CONSTANT:
          setAn(codeStore.GETFUN(instrPtr));
          instrPtr+=2;
          continue;
          
        case PUSH_CUT:
          pushTerm(INPUT_INT(cutB));
          instrPtr++;
          continue;
          
        case PUT_CUT:
          choice.setCut(cutB);
          instrPtr++;
          continue;
          
        case GET_CUT:
          xref=regs[1];
          FDEREF();
          choice.setCut(OUTPUT_INT(xval));
          instrPtr++;
          continue;
          
        case EXECUTE:
          instrPtr=codeStore.GETLABEL(instrPtr);
          if(gc_flag&&!gc_call())
            return 0;
          cutB=choice.getTop();
          continue;
          
        case PROCEED:
          FAILURE(); // FAILURE() is ok, HERE !!!
          // it just sets the address of next instruction
          // heap address where the answer is found
          // alternatively, 0 means no answer has been computed
          return 1; // address of the answer
          
        case EXEC_JUMP_IF:
          instrPtr=codeStore.GETLABEL(instrPtr);
          // Prolog.dump("JUMP IF:"+instrPtr);
          if(gc_flag&&!gc_call())
            return 0;
          cutB=choice.getTop();
        case JUMP_IF: // drop thru to next case
          xref=regs[1];
          FDEREF();
          // Prolog.dump("JUMP IF:"+xref+"/"+xval);
          if(isVAR(xval)) {
            instrPtr+=2;
            continue;
          }
          {
            int label=codeStore.GETLABEL(instrPtr);
            // System.err.println("JUMP IF:" + label
            // +"?"+codeStore.GETLABEL(label) );
            if(xval!=codeStore.GETFUN(label)) {
              instrPtr+=4;
              continue;
            }
            S=xref+1;
            curInstr=codeStore.GETINSTR(label,2);
            // System.err.println("\tAn: R" + LCGET(curInstr) + "="+ An() + ",
            // Ai:
            // R" + MCGET(curInstr) + "=" + Ai());
            setAn(S++);
            setAi(S++);
            instrPtr=label+3;
            continue;
          }
          
        case GET_UNIFY_VAL_VAR:
          xref=An();
          FDEREF();
          
          if(isVAR(xval)) {
            S=0;
            setRef(xref,pushTerm(codeStore.GETFUN(instrPtr)));
            trail.trailVarIf(xref);
            curInstr=codeStore.GETINSTR(instrPtr,2);
            pushTerm(An());
            setAi(newVar());
            instrPtr+=3;
            continue;
          }
          if(xval!=codeStore.GETFUN(instrPtr)) {
            FAILURE();
            continue;
          }
          S=xref+1;
          curInstr=codeStore.GETINSTR(instrPtr,2);
          if(!unify(An(),S)) {
            FAILURE();
            continue;
          }
          S++;
          setAi(S++);
          instrPtr+=3;
          continue;
          
        case GET_UNIFY_VAL_VAL:
          xref=An();
          FDEREF();
          if(isVAR(xval)) {
            S=0;
            setRef(xref,pushTerm(codeStore.GETFUN(instrPtr)));
            trail.trailVarIf(xref);
            curInstr=codeStore.GETINSTR(instrPtr,2);
            pushTerm(An());
            pushTerm(Ai());
            instrPtr+=3;
            continue;
          }
          if(xval!=codeStore.GETFUN(instrPtr)) {
            FAILURE();
            continue;
          }
          S=xref+1;
          curInstr=codeStore.GETINSTR(instrPtr,2);
          if(!unify(An(),S)) {
            FAILURE();
            continue;
          }
          S++;
          if(!unify(Ai(),S)) {
            FAILURE();
            continue;
          }
          S++;
          instrPtr+=3;
          continue;
          
        case GET_UNIFY_VAR_VAR:
          xref=An();
          FDEREF();
          if(isVAR(xval)) {
            S=0;
            setRef(xref,pushTerm(codeStore.GETFUN(instrPtr)));
            trail.trailVarIf(xref);
            curInstr=codeStore.GETINSTR(instrPtr,2);
            setAn(newVar());
            setAi(newVar());
            instrPtr+=3;
            continue;
          }
          if(xval!=codeStore.GETFUN(instrPtr)) {
            FAILURE();
            continue;
          }
          S=xref+1;
          curInstr=codeStore.GETINSTR(instrPtr,2);
          setAn(S++);
          setAi(S++);
          instrPtr+=3;
          continue;
          
        case GET_UNIFY_VAR_VAL:
          xref=An();
          FDEREF();
          if(isVAR(xval)) {
            S=0;
            setRef(xref,pushTerm(codeStore.GETFUN(instrPtr)));
            trail.trailVarIf(xref);
            curInstr=codeStore.GETINSTR(instrPtr,2);
            setAn(newVar());
            pushTerm(Ai());
            instrPtr+=3;
            continue;
          }
          if(xval!=codeStore.GETFUN(instrPtr)) {
            FAILURE();
            continue;
          }
          S=xref+1;
          curInstr=codeStore.GETINSTR(instrPtr,2);
          setAn(S++);
          if(!unify(Ai(),S)) {
            FAILURE();
            continue;
          }
          S++;
          instrPtr+=3;
          continue;
          
        case PUT_WRITE_VAR_VAR:
          setAn(pushTerm(codeStore.GETFUN(instrPtr)));
          instrPtr+=2;
          curInstr=codeStore.GETINSTR(instrPtr,0);
          setAn(newVar());
          setAi(newVar());
          instrPtr++;
          continue;
          
        case PUT_WRITE_VAL_VAR:
          setAn(pushTerm(codeStore.GETFUN(instrPtr)));
          instrPtr+=2;
          curInstr=codeStore.GETINSTR(instrPtr,0);
          pushTerm(An());
          setAi(newVar());
          instrPtr++;
          continue;
          
        case PUT_WRITE_VAL_VAL:
          setAn(pushTerm(codeStore.GETFUN(instrPtr)));
          instrPtr+=2;
          curInstr=codeStore.GETINSTR(instrPtr,0);
          pushTerm(An());
          pushTerm(Ai());
          instrPtr++;
          continue;
          
        case PUT_WRITE_VAR_VAL:
          setAn(pushTerm(codeStore.GETFUN(instrPtr)));
          instrPtr+=2;
          curInstr=codeStore.GETINSTR(instrPtr,0);
          setAn(newVar());
          pushTerm(Ai());
          instrPtr++;
          continue;
          
        case EXEC_TRY:
          instrPtr=codeStore.GETLABEL(instrPtr);
          cutB=choice.getTop();
          curInstr=codeStore.GETINSTR(instrPtr,0);
        case TRY_ME_ELSE: // drop thru to next case
          arity=REGFIELD();
          // $$$
          if(gc_flag&&!gc_call(0,arity))
            return 0; // moved here - as we know arity
          choice.addChoicePoint(codeStore.GETLABEL(instrPtr),regs,arity);
          instrPtr+=2;
          continue;
          
        case RETRY_ME_ELSE:
          // arity=REGFIELD(); // arity not required as known by choicePoint.
          // backpatch choice so that restoreState will then set instrPtr.
          // $$$ -- THIS TRIGGERS gcbug.pl - in assert+findall
          // arity=REGFIELD();
          // if(gc_flag&&!gc_call(0,arity))
          // return 0; // moved here - as we know arity
          
          choice.setSAVED_P(codeStore.GETLABEL(instrPtr));
          cutB=choice.restoreState(regs,false);
          instrPtr+=2;
          continue;
          
        case TRUST_ME_ELSE:
          // xval=REGFIELD(); // arity not required as known by choicePoint.
          
          // ??? unneeded - except maybe for symgc - NO EFFECT ON SYMGC BUG
          
          // $$$ -- THIS TRIGGERS gcbug.pl - in assert+findall
          // arity=REGFIELD();
          // if(gc_flag&&!gc_call(0,arity))
          // return 0; // moved here - as we know arity
          
          cutB=choice.restoreState(regs,true);
          instrPtr+=2;
          continue;
          
        case TRY_ME_ONLY: /* nop */
        case NONDET:
          instrPtr+=2;
          continue;
          
        case EXEC_SWITCH:
          // $CALL
          instrPtr=codeStore.GETLABEL(instrPtr);
          if(gc_flag&&!gc_call())
            return 0;
          cutB=choice.getTop();
        case SWITCH: // drop thru to next case
          xref=regs[1];
          FDEREF();
          if(isVAR(xval)) {
            instrPtr+=2;
            continue;
          }
          // firstarg indexing here int->int
          instrPtr=xx(dict.hget(codeStore.GETFUN(instrPtr),xval));
          if(dict.do_isEmpty(instrPtr)) {
            FAILURE();
            continue;
          }
          S=xref+1;
          instrPtr+=2;
          continue;
          
        case LOAD_CONSTANT: {
          int ival=REGFIELD();
          setX(ival,codeStore.GETFUN(instrPtr));
          instrPtr+=2;
        }
          continue;
          
        case LOAD_VAL_SHORT:
          // REG contains small integer for immediate use.
          xval=INPUT_INT(REGFIELD());
          // was different in orig C, code - but it looks ok here as is PT
          // xval=REGFIELD(); // was in orig C code
          setX(2,xval);
          IN(1,Ai());
          instrPtr++;
          continue;
          
        case LOAD_VALUEx2:
          IN(1,Ai());
          IN(2,An());
          instrPtr++;
          continue;
          
        case LOAD_VALUE:
          ires=REGFIELD();
          IN(ires,Ai());
          instrPtr++;
          continue;
          
          /*
           * BUG: LOAD_VARIABLE: was missing - problem with voids passed to
           * arith builtins
           * 
           * FIXED: see instruction 50
           * 
           * INSTRUCTIONS SEEM OUT OF ORDER MIGHT IMPACT ON PERFORMANCE
           */
          
        case LOAD_VARIABLE:
          // Prolog.dump("UNEXPECTED LOAD_VARIABLE:"+op); FAILURE();
          ires=REGFIELD();
          // setAn(newVar());
          // setAi(An());
          xval=newVar();
          setX(ires,xval);
          setAi(xval);
          instrPtr++;
          continue;
          
          /* INLINE_PREDS */
          
        case FAIL_0:
          FAILURE();
          continue;
          
          /* OLD SPECIAL BUILTINS */
          
          /*
           * handles a. which gives a(Cont):-'true'(Cont).
           */
        case TRUE_0: // TRUE_0 in C
          cutB=choice.getTop();
          xref=An();
          
          FDEREF();
          arity=GETARITY(xval);
          
          // $CALL
          instrPtr=xx(dict.getpred(xval));
          
          if(dict.do_isEmpty(instrPtr)) {
            // int cont=regs[arity+1];
            long cont=getRef(xref+arity); // BUG - past versions was cont=arity
            // or
            // arity+1
            instrPtr=do_undef(0,xref,cont); // should trim cont here
            continue;
          }
          
          for(int i=1;i<arity+1;i++)
            regs[i]=getRef(xref+i); // was gc bug
          
          if(gc_flag&&!gc_call(0,arity))
            return 0;
          
          continue;
          
          /*
           * handles pred like: a:-X,b. which become a(Cont):-'call'(X,b(Cont)).
           */
        case CALL_1: // CALL_1 in C
          // Interact.println("CALL_1: entering");
          cutB=choice.getTop();
          xref=An();
          FDEREF();
          arity=GETARITY(xval)+1; // inc arity by 1 for bin continution
          
          // $CALL
          instrPtr=xx(dict.getpred(PUTARITY(xval,arity)));
          
          if(dict.do_isEmpty(instrPtr)) {
            long cont=regs[2];
            instrPtr=do_undef(1,xref,cont);
            continue;
          }
          
          regs[arity]=regs[2];
          for(int i=1;i<arity;i++) {
            regs[i]=getRef(xref+i);
          }
          
          if(gc_flag&&!gc_call(0,arity))
            return 0;
          continue;
          
        case APPLY:
          cutB=choice.getTop();
          xval=codeStore.GETFUN(instrPtr);
          apply(xval);
          
          // $ CALL
          instrPtr=xx(dict.getpred(atomTable.G_metacall));
          continue;
          
        case CWRITE_1: // $IO
        {
          xref=regs[0];
          FDEREF();
          cwrite(xref);
        }
          instrPtr++;
        break;
        
        case CNL_0: // $IO
          cnl();
          instrPtr++;
        break;
        
        case IS_COMPILED_1:
          xref=regs[0];
          FDEREF();
          xval=PUTARITY(xval,1+GETARITY(xval));
          if(isCompiled(xval))
            instrPtr++;
          else
            FAILURE();
          continue;
          
        case RETURN_1:
          // Interact.println("RETURN_1 called");
          xref=regs[0];
          instrPtr++;
          return xref;
          
          // ARITH instructions start here 71,72...
          
        case CREATE_ENGINE_3:
        case ENGINE_GET_2:
        case ENGINE_STOP_1:
          engine_op(op);
          continue;
          
        case CURRENT_ENGINE_1:
          // Prolog.dump("current_engine: "+this+
          // "\nengines="+prolog.engineTable.size()+":"+prolog.engineTable);
          OUT(INPUT_INT(engineHandle));
          continue;
          
          /* sends an encoded term to an engine */
        case TO_ENGINE_2:
          to_engine(op,X(1),X(2));
          instrPtr++;
          continue;
          
          /* decodes to fresh heap term the encoded term sent to this engine */
        case FROM_ENGINE_1:
          xval=from_engine();
          OUT(xval);
          continue;
          
        case THIS_CLASS_1:
          ires=0; // signals the current prolog
          OUT(INPUT_INT(ires));
          continue;
          
        case ARG_3:
          arg();
          continue;
          
        case ARITY_2:
          arity();
          continue;
          
        case FUN_3:
          fun();
          continue;
          
        case SUCC_2: // pred == succ negate succ
          succ();
          continue;
          
        case TYPE_OF_2:
          type_of();
          continue;
          
        case ICALL_3:
          if(!icall())
            FAILURE();
          continue;
          
        case ICALL_OP_4:
          if(!icall_op())
            FAILURE();
          continue;
          
        case ICALL_IMPURE_5:
          if(!icall_impure())
            FAILURE();
          continue;
          
        case XCALL_3:
          // if(gc_flag&&!gc_call(0,16))
          // FAILURE(); // does not seem to help with SYMGC bug
          
          if(!call_xcall())
            FAILURE();
          continue;
          
        default:
          warn_mes("*** bad instruction: ["+op+"] ***"+X(1)+":"+X(2));
          stop();
          return 0;
          
      } // end switch
    } // end for
  }
  
  void expand() {
    gc_flag=true;
    super.expand();
  }
  
  void gc_trace(String mes,long pred,int arity) throws PrologException {
    if(PrologGC.trace>1) {
      String data=(pred!=0)?(atomTable.getSym(pred)+"/"+arity):("<?>/"+arity);
      Prolog.dump("GC: "+data+" "+mes+" USED: "+getUsed()+" FREE:"+getFree());
    }
  }
  
  /**
   * calls external GC - passes a handle to this LogicEngine to it
   */
  private boolean gc(int arity) throws PrologException {
    /**
     * call your gc here !!!
     */
    PrologGC collector=new PrologGC(arity,regs,choice,trail,this);
    boolean ok=collector.collect();
    collector=null;
    gc_flag=false;
    
    return ok;
  }
  
  private final boolean gc_call() throws PrologException {
    long pred=dict.addr2fun(instrPtr);
    if(dict.do_isEmpty(pred)) {
      Prolog
          .dump("address does not match known predicate: dropped gc_call at: "
              +instrPtr);
      return true; // we cannot compute arity - wait for next opportunity for
      // gc
    }
    int arity=GETARITY(pred);
    
    return gc_call(pred,arity);
    
  }
  
  boolean gc_call(long pred,int arity) throws PrologException {
    gc_trace("BEFORE:",pred,arity);
    boolean ok=gc(arity);
    gc_trace("AFTER: ",pred,arity);
    if(this.atomTable.symgc_flag) {
      // Interact.dump("this should call force_trimSyms, engine="+this);
      this.atomTable.force_trimSyms(this);
    }
    return ok;
  }
  
  final boolean isCompiled(long PredArity) throws PrologException {
    int instrPtr=xx(dict.getpred(PredArity));
    return !dict.do_isEmpty(instrPtr);
  }
  
  /**
   * handles the case when a predicate undefined at link time first in the body
   * of a clause
   */
  final void apply(long fun) {
    int arity=GETARITY(fun)-1;
    fun=PUTARITY(fun,arity);
    pushTerm(fun);
    int xref=getHeapTop();
    // COPY_CELLS(xref,regs,arity);
    pushCells(regs,1,arity);
    // for(int i=1; i<=arity; i++) pushTerm(regs[i]);
    regs[2]=regs[arity+1];
    regs[1]=xref;
    // Prolog.dump("### APPLY: "+termToString(xref));
  }
  
  final int do_undef(int trim,long closure,long cont) throws PrologException {
    regs[1]=INPUT_INT(trim);
    regs[2]=closure;
    regs[3]=cont;
    long undef=atomTable.G_undefined;
    int ip=xx(dict.getpred(undef));
    // Prolog.dump("### do_undef: "+trim+" xref: "+termToString(closure)+" cont:"+termToString(cont)+" ip: "+ip);
    
    if(dict.do_isEmpty(ip))
      throw new PrologException("undefined predicate: "+termToString(closure));
    
    return ip;
  }
  
  final boolean unify(long v1,long v2) {
    unifyStack.clear();
    unifyStack.push(v1);
    unifyStack.push(v2);
    
    while(!unifyStack.isEmpty()) {
      long t1=unifyStack.pop();
      long t2=unifyStack.pop();
      if(isVAR(t1)) {
        deref(t1);
        t1=xref;
        v1=xval;
      } else
        v1=t1;
      if(isVAR(t2)) {
        deref(t2);
        t2=xref;
        v2=xval;
      } else
        v2=t2;
      if(t1!=t2) {
        if(isVAR(v1)) { /* unb. var. v1 */
          if(isVAR(v2)&&v2>v1) { /* unb. var. v2 */
            setRef(v2,v1);
            trail.trailVarIf(v2);
          } else {
            setRef(v1,isCOMPOUND(v2)?t2:v2);
            trail.trailVarIf(v1);
          }
        } else if(isVAR(v2)) { /* v1 is NONVAR */
          setRef(v2,isCOMPOUND(v1)?t1:v1);
          trail.trailVarIf(v2);
        } else if(v1!=v2) /* both are NONVAR */
          return false;
        else if(isIDENTIFIER(v1)&&((v1=GETARITY(v1))>0)) {
          /* they have the same FUNCTOR, v1==v2 */
          // CHECK(U,ChoiceStk,"unification overflow");
          unifyStack.push(getRef(t1+v1));
          unifyStack.push(getRef(t2+v1));
          for(int i=0;i<v1;i++) {
            unifyStack.push(getRef(t1+i));
            unifyStack.push(getRef(t2+i));
          }
        } // end else if
      } // end t1!=t2
    }
    return true;
  }
  
  /* ERROR HANDLERS */
  public static void warn_mes(String mes) {
    Interact.warnmes(Interact.NL+"Warning: "+mes+".");
  }
  
  public final void OUT(long expr) {
    if(LEFTFIELD()!=0) {
      if(!unify(expr,An())) {
        FAILURE();
        return;
      }
    } else {
      setAn(expr);
    }
    instrPtr++;
  }
  
  public final void FAILURE() {
    instrPtr=choice.SAVED_P();
  }
  
  private final void IN(int I,long Expr) {
    xref=Expr;
    if(isNONVAR(xref))
      setX(I,xref);
    else {
      deref();
      setX(I,isCOMPOUND(xval)?xref:xval);
    }
  } // End IN
  
  public final void add_instr(int target,int op,int reg,String fun,int arity)
      throws PrologException {
    if(op<0||op>MAXOP||null==fun)
      throw new PrologException("unexpected in add_instr("+target+","+op+","
          +reg+","+fun+","+arity+")");
    if(0==target)
      codeStore.loadInstruction(op,reg,fun,arity);
    else
      throw new PrologException("no write_instr in add_instr("+target+","+op
          +","+reg+","+fun+","+arity+")");
  }
  
  // protected and friends are deprecated
  
  private boolean gcProtected=false;
  
  public void protect_engine() {
    this.gcProtected=true;
  }
  
  public void unprotect_engine() {
    this.gcProtected=false;
  }
  
  public boolean isProtected() {
    return this.gcProtected;
  }
  
  public final void dismantle() { // deprecated
    stop();
  }
  
  /**
  * Stops this machine - overrides
  */
  public final void stop() {
    if(isStopped)
      return;
    // op=END;
    // instrPtr=0;
    isStopped=true;
    op=END;
    instrPtr=0;
    // this forbids stop(E),stop(E) as engine is gone...
    // DO NOT DO THIS BECAUSE:
    // - no handle is there to print out the name of the missing engine
    // - repeated stops will try to stop something that does not have a handle
    // - the only solution seems to leave this to the symbol gc
    //
    // Interact.println("@@@ destroy");
    super.destroy();
    unifyStack.destroy();
    choice.destroy();
    trail.destroy();
    
    if(null!=parent)
      parent.removeEngine(engineHandle);
    // undoTable=null;
    
  }
  
  /**
   * Queries an interpreter, returning a term, one answer at a time.
   */
  
  final public long ask() throws PrologException {
    long answer=0;
    if(!isStopped)
      answer=runInternal();
    return answer; // could be reused even if answer==0
  }
  
  boolean isUNBOUND(int ref) {
    this.deref(ref);
    return isVAR(this.xval)&&this.xval==this.xref;
  }
  
  final void to_engine(int op,long e,long t) throws PrologException {
    LogicEngine M=toHandle(op,e);
    
    if(null==M||M.isStopped) // stopped
      throw new PrologException(this+"=>to_engine("+termToString(e)
          +"): stopped or null engine="+e);
    
    if(null!=M.messageBox)
      throw new PrologException(this+"=>to_engine("+termToString(e)
          +"): full messageBox");
    
    M.messageBox=encodedCopy(t);
    // Object[] bundle=ref2term(t).export(1);
    // Prolog.dump("TO_ENGINE: "+new PortableTerm(bundle,0));
  }
  
  final int from_engine() throws PrologException {
    if(isStopped) // stopped
      throw new PrologException("from_engine: "+this+"=>stopped engine");
    
    if(null==messageBox)
      throw new PrologException("from_engine: "+this+"=>empty messageBox found");
    int res=decodedCopy(messageBox); // handleMessage: doit
    messageBox=null;
    return res;
  }
  
  /*
   * public final boolean exportBundle(int cloned) { if(null!=this.ioBundle ||
   * null==this.messageBox) {
   * Interact.warnmes("exportBundle: bad message boxes"); return false; }
   * EncodedTerm T=this.messageBox; this.messageBox=null; // taken!
   * this.ioBundle=(new PortableTerm(T,prolog)).export(cloned); return true; }
   * 
   * public final boolean importBundle(int cloned) { boolean
   * bad1=null==this.ioBundle; boolean bad2=null!=this.messageBox;
   * if(bad1||bad2) {
   * Interact.warnmes("importBundle: bad message boxes:"+bad1+","+bad2); return
   * false; } PortableTerm P=new PortableTerm(this.ioBundle,cloned);
   * this.ioBundle=null; // taken! messageBox=P.outputTo(prolog); return true; }
   */
  
  public final void setBundle(Object[] bundle) {
    this.ioBundle=bundle;
  }
  
  public final Object[] getBundle() {
    return this.ioBundle;
  }
  
  // !!!speed up
  public final Object[] ref2bundle(long x) {
    // Interact.dump("ENTERING ref2bundle");
    EncodedTerm T=encodedCopy(x);
    
    Object[] O=(new PortableTerm(T,prolog)).export(0);
    // Interact.dump("EXITING ref2bundle");
    return O;
  }
  
  public final int bundle2ref(Object[] bundle) {
    // Interact.dump("ENTERING bundle2ref");
    if(null==bundle)
      return 0;
    // Interact.dump("ENTERING PortableTerm");
    PortableTerm P=new PortableTerm(bundle,0);
    // Interact.dump("EXITING PortableTerm");
    EncodedTerm T=P.outputTo(prolog);
    int x=decodedCopy(T);
    // Interact.dump("EXITING bundle2ref");
    return x;
  }
  
  /*
   * private void show_trace() { codeStore.dumpInstruction(0,instrPtr);
   * Interact.warnmes("\tAn: R"+LCGET(curInstr)+"="+An()+", Ai: R"
   * +MCGET(curInstr)+"="+Ai()); }
   */
  
  /**
   * ENGINE OPERATIONS
   */
  
  // override
  LogicEngine cloneLogicEngine() {
    return new LogicEngine(this.prolog);
  }
  
  final private LogicEngine toHandle(int op,long xval) {
    LogicEngine E=toHandle(xval);
    if(null!=E)
      return E;
    
    Interact.errmes("bad engine_op="+op+": null engine handle");
    
    return null;
  }
  
  int engineHandle=0;
  
  /**
   * decode symbolic constant representing engine to handle/ordinal in
   * engineTable
   */
  final LogicEngine toHandle(long xval) {
    LogicEngine E;
    if(isINTEGER(xval)) {
      int e=OUTPUT_INT(xval);
      if(0==e) {
        E=cloneLogicEngine(); // also called by LogicInteractor
        e=addEngine(E); // should not be inherited by LogicThread !!!
        E.engineHandle=e;
        E.parent=this;
        // Interact.dump("toHandle: added engine="+e);
      } else {
        E=getEngine(e);
        if(null==E)
          Interact.warnmes("no engine for handle="+e);
      }
      // } else if(isSYMCONST(s)) {
      // Prolog.dump("toHandle: getSym="+prolog.atomTable.getSym(s));
      // E=(LogicEngine)prolog.atomTable.getSym(s);
    } else
      E=null;
    return E;
  }
  
  final private void engine_op(int op) throws PrologException {
    // Prolog.dump("!!!engine_op:"+op+"=>"+op);
    
    switch(op) {
      case CREATE_ENGINE_3: { // also LOAD_ENGINE
        xval=X(1);
        LogicEngine M=toHandle(op,xval); //
        if(null==M) {
          Interact.warnmes("failure to create new engine");
          
          return;
        }
        long t=X(2);
        EncodedTerm T=encodedCopy(t);
        M.INIT_INTERP(T);
        
        // xval=M.currentEngine;
        
        xval=INPUT_INT(M.engineHandle);
        OUT(xval);
        
        return;
      }
      
      case ENGINE_GET_2: {
        // Prolog.dump("!!!engine_get:"+termToString(X(1)));
        // if(this instanceof Interactor)
        // Interact.dump("INTERACTOR="+this);
        
        LogicEngine E=toHandle(op,X(1));
        
        if(null==E) {
          xval=atomTable.G_NO;
        } else {
          // Prolog.dump("engine_get: engine:"+E+":"+E.getClass());
          try {
            long result=E.ask();
            if(0==result) {
              xval=atomTable.G_NO;
              // Interact.dump("engine_get: STOPPING engine:"+E+":"+E.getClass());
              E.stop(); // WAS BUGGY instead of E.stop() it WAS stop() -
                        // resulting in self-stopped!!!
            } else {
              xref=copyTermFrom(E,result);
              xval=atomTable.G_THE;
              xval=pushTerm(xval); // builds the(X)
              pushTerm(xref);
            }
          }
          /*
           * catch(PrologException ex) { //ex.printStackTrace();
           * xval=atomTable.G_NO; } catch(ClassCastException ex) {
           * //ex.printStackTrace(); xval=atomTable.G_NO; throw new
           * PrologException(
           * "engine_get/2 atempted on unknown object: "+E+":"+E.getClass()); }
           */
          catch(Exception fe) {
            fe.printStackTrace();
            Interact.errmes("error in engine_get, engine="+E+":"
                +E.getClass().getName());
          }
        }
        
        OUT(xval);
        return;
      }
      
      case ENGINE_STOP_1: {
        // Prolog.dump("!!!engine_stop:"+termToString(X(1)));
        xval=X(1);
        if(isINTEGER(xval)) {
          if(0==OUTPUT_INT(xval))
            this.stop();
          else {
            LogicEngine E=toHandle(op,X(1));
            if(null!=E)
              E.stop();
          }
        }
        instrPtr++;
        return;
      }
      
      default: {
        throw new PrologException("bad opcode in engine_op: "+op);
      }
    }
  }
  
  final private void arg() {
    long argNo=X(1);
    if(!isINTEGER(argNo)) {
      warn_mes(xval+"arg/3's 1st arg must be integer");
      FAILURE();
      return;
    }
    argNo=OUTPUT_INT(argNo);
    
    xref=X(2);
    FDEREF();
    
    // this returns the object itself is not compound
    if(0==argNo&&isIDENTIFIER(xval)) {
      xval=PUTARITY(xval,0);
      OUT(xval);
      return;
    }
    
    if(isATOMIC(xval)) {
      FAILURE();
      return;
    }
    if(isVAR(xval)) {
      warn_mes("arg/3's 2nd arg must be nonvar.");
      FAILURE();
      return;
    }
    if(argNo<0||argNo>GETARITY(xval)) {
      /*
       * warn_mes("bad index: "+argNo
       * +", arg/3's 1st argument must be in range 0.."+GETARITY(xval)
       * +" for predicate "+dumpCell(xval));
       */
      FAILURE();
      return;
    }
    xref+=argNo;
    OUT(xref);
  }
  
  final private void arity() {
    xref=X(1);
    FDEREF();
    if(!isIDENTIFIER(xval)) {
      warn_mes(xval+": arity/2's 1st arg must be symbolic");
      // Interact.errmes("arity error");
      FAILURE();
      return;
    }
    ires=GETARITY(xval);
    OUT(INPUT_INT(ires));
  }
  
  final private void fun() {
    // Prolog.dump("fun-here");
    xref=X(1);
    FDEREF();
    if(!isIDENTIFIER(xval)) {
      warn_mes(xval+": fun/3's 1st arg must be symbolic");
      FAILURE();
      return;
    }
    
    if(!isINTEGER(X(2))) {
      warn_mes(prolog.atomTable.getSym(xval)
          +":fun/3's 2nd arg must be integer, bad data="+X(2));
      FAILURE();
      return;
    }
    
    int arity=OUTPUT_INT(X(2));
    if(arity>=MAXARITY) {
      warn_mes(prolog.atomTable.getSym(xval)+"/"+arity
          +": fun/3's 2nd arg arity out of range");
      FAILURE();
      return;
    }
    
    int s=getHeapTop()+1;
    
    xval=PUTARITY(xval,arity);
    pushTerm(xval);
    
    while((arity--)>0) {
      newVar();
    }
    OUT(s);
    return;
  }
  
  final private void succ() {
    if(!isINTEGER(X(1))) {
      warn_mes(xval+":succ/3's 1'st arg must be integer");
      FAILURE();
      return;
    }
    ires=OUTPUT_INT(X(1));
    ires++;
    if(ires>=Defs.MAXINT)
      ires=0; // goes mod MAXINT, always > 0
    OUT(INPUT_INT(ires));
    return;
  }
  
  final static int VAR_TYPE=0;
  
  final static int INT_TYPE=1;
  
  final static int ATOM_TYPE=2;
  
  final static int FUN_TYPE=3;
  
  final static int CODES_TYPE=4;
  
  public final static int BYTES_TYPE=5;
  
  public final static int SMALLINT_TYPE=6;
  
  public final static int BIGINT_TYPE=7;
  
  public final static int BIGDEC_TYPE=8; // FLOAT
  
  public final static int STRINGOB_TYPE=9;
  
  public final static int INTERACTOR_TYPE=10;
  
  public final static int OTHER_OB_TYPE=11;
  
  public final static int ENGINE_TYPE=12;
  
  final static int BAD_TYPE=-1;
  
  final private void type_of() {
    xref=X(1);
    FDEREF();
    int t=BAD_TYPE; // unexpected bad type
    if(isVAR(xval))
      t=VAR_TYPE;
    else if(isINTEGER(xval)) {
      // int e=OUTPUT_INT(xval);
      // Interact.dump("type_of="+e);
      // if(null!=getEngine(e))
      // t=ENGINE_TYPE;
      // else
      t=INT_TYPE;
    } else if(isATOMIC(xval)) {
      Object O=atomTable.getSym(xval);
      if(null!=O) {
        if(null!=Prolog.extender)
          t=Prolog.extender.obtype_of(prolog,O);
        else
          t=ATOM_TYPE;
      } else
        t=BAD_TYPE;
    } else if(isCOMPOUND(xval)) {
      if(atomTable.G_STRING==xval) {
        t=CODES_TYPE;
      } else
        t=FUN_TYPE;
    }
    if(BAD_TYPE==t) {
      warn_mes("type_of: unexpected object of unkonwn type:"+dumpCell(xval));
      FAILURE();
      return;
    }
    OUT(INPUT_INT(t));
    return;
  }
  
  final private boolean call_xcall() throws PrologException {
    // Interact.verbosity=3;
    // Interact.quickfail=4;
    
    if(!isINTEGER(X(1))) {
      warn_mes(xval+":xcall/3's 1'st arg, the operation code, must be integer");
      return false;
    }
    ires=OUTPUT_INT(X(1));
    
    if(null==Prolog.extender) {
      warn_mes("xcall called when no extender available, xcall op="+ires);
      FAILURE();
      return false;
    }
    // override
    long o=xcall(ires,X(2));
    if(0==o) {
      // Prolog.dump("call_external failing="+o);
      return false;
    }
    
    OUT(o);
    
    return true;
  }
  
  void cwrite(long xref) {
    warn_mes("unavailable cwrite/1 called form inner LogicEngine with "+xref);
  }
  
  void cnl() {
    warn_mes("unavailable cnl/0 called form inner LogicEngine");
  }
  
  boolean icall() throws PrologException {
    boolean ok=false;
    if(!isINTEGER(X(1))) {
      warn_mes(xval+":icall/3's 1'st arg, the operation code, must be integer");
      return ok;
    }
    int op=OUTPUT_INT(X(1));
    switch(op) {
    
      case 0: { // new_builtin0
        xref=X(2); // this is the input
        FDEREF(); // not needed if expecting only int, const arg ???
        // GETREF
        // just a stub: do some work here
        OUT(xref); // for now just return it back
        ok=true;
      }
      break;
      
      case 1: { // copy_term
        xref=X(2); // this is the input
        FDEREF();
        EncodedTerm t=encodedCopy(xref);
        xref=decodedCopy(t);
        OUT(xref);
        ok=true;
      }
      break;
      
      case 2: { // term_hash
      
        int res=termHash(X(2));
        ok=res>=0;
        OUT(INPUT_INT(res));
      }
      break;
      
      default: {
        warn_mes("unavailable icall/3 called with Op="+op);
      }
    }
    return ok;
  }
  
  /**
   * implements fast internal extension; note that called built-ins should do an
   * OUT(result)) if successful
   * 
   * success / failure depends on boolean value returned
   */
  boolean icall_op() throws PrologException {
    if(!isINTEGER(X(1))) {
      warn_mes(xval
          +":icall_op/4's 1'st arg, the operation code, must be integer");
      return false;
    }
    
    int op=OUTPUT_INT(X(1));
    boolean ok=false;
    // Prolog.dump("icall_op:"+op+":"+X(2)+":"+X(3));
    switch(op) {
    
      case 0:
        ok=hkey();
      break;
      
      case 1:
        ok=false;
      break;
      
      case 2: { // ncompare
        long a=X(2);
        long b=X(3);
        if(isINTEGER(a)&&isINTEGER(b)) {
          int ia=OUTPUT_INT(a);
          int ib=OUTPUT_INT(b);
          int lim=1<<28;
          if(Math.abs(ia)<lim&&Math.abs(ib)<lim) {
            OUT(INPUT_INT(Integer.signum(ia-ib)));
            ok=true;
          }
        }
      }
      break;
      
      case 3:
      case 4: { // +, -
        long a=X(2);
        long b=X(3);
        if(isINTEGER(a)&&isINTEGER(b)) {
          int ia=OUTPUT_INT(a);
          int ib=OUTPUT_INT(b);
          ia=(3==op)?(ia+ib):(ia-ib);
          if(Math.abs(ia)<(1<<28)) {
            OUT(INPUT_INT(ia));
            ok=true;
          }
        }
      }
      break;
      
      case 5: { // *
        long a=X(2);
        long b=X(3);
        if(isINTEGER(a)&&isINTEGER(b)) {
          int ia=OUTPUT_INT(a);
          int ib=OUTPUT_INT(b);
          long lim=1L<<14;
          if(Math.abs(ia)<lim&&Math.abs(ib)<lim) {
            OUT(INPUT_INT(ia*ib));
            ok=true;
          }
        }
      }
      break;
      
      case 6: {
        xref=det_append(); // uses X(1) and X(2) implicitely.
        if(xref==0) {
          ok=false;
        } else {
          ok=true;
          OUT(xref);
        }
      }
      break;
      
      default: {
        warn_mes("unavailable icall_op called with Op="+op);
      }
    }
    
    return ok;
  }
  
  private final int det_append() throws PrologException {
    int H=getHeapTop()+1;
    int oldH=H;
    xref=X(2);
    if(isVAR(xref)) {
      xval=getRef(xref);
      while(xval==atomTable.G_DOT) {
        pushList(++xref);
        ++xref;
        FDEREF();
      }
    } else
      xval=xref;
    
    if(xval!=atomTable.G_NIL) {
      // throw new
      // PrologException("[] should end 1-st arg of det_append/3, not "
      // +xval);
      setHeapTop(oldH);
      return 0;
    } else {
      /*
       * bug here - maybe unify does not know obout this!
       */
      pushTerm(X(3));
      return H;
    }
  }
  
  /**
   * 
   * returns a simple (possibly transient) hash code not that we do OUT(..) right here and
   * this is propagated up by icall_op - for Strings hasCode is always the same in java 8
   */
  
  final private boolean hkey() {
    if(!isINTEGER(X(2))) {
      warn_mes("arg 1 (mod) in hkey should be int");
      return false;
    }
    int mod=OUTPUT_INT(X(2));
    xref=X(3);
    FDEREF();
    if(isVAR(xval))
      return false;
    int res;
    if(isINTEGER(xval)) {
      res=OUTPUT_INT(xval);
    } else {
      Object O=atomTable.getSym(xval);
      if(null==O) {
        return false;
      } else {
        res=O.hashCode()+GETARITY(xval);
      }
    }
    
    long lres=INPUT_INT(Math.abs(res%mod));
    OUT(lres);
    return true;
  }
  
  /**
   * implements fast internal extension note that called built-ins should do an
   * OUT(result)) if successful
   * 
   * success / failure depends on boolean value returned
   * 
   * typical used for things like swap_arg(I,T,New,Old)
   */
  boolean icall_impure() throws PrologException {
    if(!isINTEGER(X(1))) {
      warn_mes(xval
          +":icall_impure/5's 1'st arg, the operation code, must be integer");
      return false;
    }
    
    int op=OUTPUT_INT(X(1));
    boolean ok=false;
    
    switch(op) {
      case 0:
        ok=change_arg();
      break;
      
      default: {
        warn_mes("unavailable icall_impure/5 called with Op="+op);
      }
    }
    
    return ok;
  }
  
  final boolean change_arg() {
    long argNo=X(2);
    if(!isINTEGER(argNo)) {
      warn_mes(xval+"change_arg/3's 1st arg must be integer");
      return false;
    }
    argNo=OUTPUT_INT(argNo);
    
    xref=X(3);
    FDEREF();
    
    if(isATOMIC(xval)) {
      return false;
    }
    if(isVAR(xval)) {
      warn_mes("change_arg/3's 2nd arg must be nonvar.");
      return false;
    }
    if(argNo<=0||argNo>GETARITY(xval)) {
      warn_mes("bad index: "+argNo
          +", change_arg/3's 1st argument must be in range 1.."+GETARITY(xval)
          +" for predicate "+dumpCell(xval));
      return false;
    }
    xref+=argNo;
    xval=X(4);
    long other=getRef(xref);
    setRef(xref,xval); // swap_arg?
    OUT(other);
    return true;
  }
  
  long xcall(int op,long input) throws PrologException {
    warn_mes("unavailable xcall called form inner LogicEngine, op="+op
        +" ,input="+input);
    return 0;
  }
  
  /*
   * public void trailUndoable(Object O) {
   * ((ChoicePointStackEntry)choice.peek()).add(new Undo(O)); }
   */
  
  // begin symgc SYMGC $$$
  
  final Object i2o(int iobject) {
    return atomTable.i2o(iobject);
  }
  
  public final long choiceMem() {
    long s=0L;
    s+=regs.length;
    for(int i=0;i<choice.size();i++) {
      long[] cregs=((ChoicePointStackEntry)choice.at(i)).regs;
      s+=cregs.length;
    }
    return s;
  }
  
  public final long trailSize() {
    return trail.size();
  }
  
  final void addKeeperSyms(ObjectStack syms,AtomTable keepers) {
    // this.currentEngine=addKeeper(this.currentEngine,syms,keepers);
    addHeapKeepers(syms,keepers);
    // int arity=regs.length-1; // $$$ too much
    int arity=TEMPARGS<<1; // / this is problematic !!! ???
    addOtherKeepers(syms,keepers,choice,regs,arity);
  }
  
  void addOtherKeepers(ObjectStack syms,AtomTable keepers,
      ChoicePointStack choice,long[] regs,int arity) {
    
    // add registers in choicepoints
    
    for(int i=0;i<choice.size();i++) {
      long[] cregs=((ChoicePointStackEntry)choice.at(i)).regs;
      for(int j=0;j<cregs.length;j++) {
        cregs[j]=addKeeper(cregs[j],syms,keepers);
      }
    }
    // add argument registers
    for(int i=1;i<=arity;i++) {
      regs[i]=addKeeper(regs[i],syms,keepers);
    }
    
    // add possibly in use temporary args? //X(1) X(2)...
    int l=regs.length;
    for(int i=1;i<TEMPARGS;i++) {
      regs[l-i]=addKeeper(regs[l-i],syms,keepers);
    }
    
    // probably redundant - does not help with i2o bad index bug
    // currentEngine=addKeeper(currentEngine,syms,keepers);
  }
  
  // end object gc
  
  public String toString() {
    return ":ENG:"+instance+":";
  }
  
}