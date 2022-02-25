package vm.logic;

/**
  Implements everything needed for a Prolog code container - in particular a Prolog class
*/
final class CodeStore extends Defs {
  private static final long serialVersionUID=222L;
  
  private Dict dict;
  
  private AtomTable atomTable;
  
  private int codeTop;
  
  private long curPred;
  
  private int prevInstr; // last instruction.
  
  private int prevLen;
  
  private int prevPrevLen;
  
  private long[] code;
  
  private static final byte[] instructionLength=initInstructionLengths();
  
  CodeStore(AtomTable atomTable,Dict dict) throws PrologException{
    this.dict=dict;
    
    this.atomTable=atomTable;
    
    this.code=new long[1<<16];
    
    SETOP(codeTop,END);
    codeTop+=getInstructionLength(END);
    curPred=atomTable.G_true;
    prevPrevLen=prevLen=2;
    SETOP(codeTop,PROCEED);
    dict.setpred(atomTable.G_true,codeTop);
    codeTop=SETFUN(codeTop,atomTable.G_true);
    codeTop++;
  }
  
  void setSYSEND() {
    atomTable.setSYSEND();
  }
  
  final private long inputTerm(String name,int arity) throws PrologException {
    return atomTable.inputTerm(name,arity);
  }
  
  /*******INTERFACE INSTRUCTION OPERATIONS ********************/
  
  static final int GETINSTR_OP(long instr) {
    return RCGET(instr);
  }
  
  private final void SETOP(int instPtr,int val) {
    code[instPtr]=RCPUT(code[instPtr],val);
  }
  
  private final int GETOP(int instPtr) {
    return RCGET(code[instPtr]);
  }
  
  private final void SETREG(int instPtr,int val) {
    code[instPtr]=LCPUT(code[instPtr],val);
  }
  
  private final int GETREG(int instPtr) {
    return LCGET(code[instPtr]);
  }
  
  private final void SETLEFT(int instPtr,int val) {
    code[instPtr]=MCPUT(code[instPtr],val);
  }
  
  private final int GETARG(int instPtr) {
    return MCGET(code[instPtr]);
  }
  
  final long GETINSTR(int instrPtr,int step) {
    return code[instrPtr+step];
  }
  
  final long GETFUN(int instr) {
    return code[instr+1];
  }
  
  private final int SETFUN(int instr,long val) {
    instr++;
    code[instr]=val;
    return instr;
  }
  
  final int GETLABEL(int instr) {
    return xx(code[instr+1]);
  }
  
  private final void SETLABEL(int instr,int val) {
    code[instr+1]=val;
  }
  
  private final boolean COMPRESS(int reg,int Simple,int DoubleInst,int First,
      int Triple) {
    // return true;
    
    if(1==prevLen&&Simple==GETOP(codeTop-1)) {
      SETOP(codeTop-1,DoubleInst);
      codeTop--;
      SETLEFT(codeTop,reg);
      if(2==prevPrevLen&&First==GETOP(codeTop-2)) {
        SETOP(codeTop-2,Triple);
      }
      return true;
    }
    return false;
    
  }
  
  private final boolean OCOMPRESS(int reg,int Simple,int DoubleInst,int First,
      int Triple) {
    return COMPRESS(reg,Simple,DoubleInst,First,Triple);
  }
  
  int getTop() {
    return codeTop;
  }
  
  /** Return the count of entries.*/
  final int getUsed() {
    return codeTop+1;
  }
  
  int getMax() {
    return code.length;
  }
  
  int getFree() {
    return code.length-codeTop-1;
  }
  
  // get index of prolog_main
  int getStartPoint() throws PrologException {
    int start=Dict.EMPTY; // atomTable.G_empty;
    // try {
    start=dict.getpred(atomTable.G_prolog_main);
    // }
    // catch (PrologException ignore) {
    // }
    if(start==Dict.EMPTY)
      throw new PrologException(Interact.NL
          +"Fatal error: no definition for start point predicate.");
    return start;
  }
  
  private int codeTopBak;
  
  // private int codeTopBakLOADTIME;
  
  void setTopBack() {
    codeTopBak=codeTop;
  }
  
  /*
  void rollback() {
    codeTop=codeTopBak;
  }

  void resetTopBak() {
    codeTopBak=codeTopBakLOADTIME;
  }
  */
  
  /**
   * Loads a bytecode file from a URL or a file or
   * an open stream - also, it prepends wam.bp to it.
   */
  boolean load(Object fNameOrStream) {
    // Interact.println("   begin loading:" + fName);
    
    boolean ok=CodeLoader.load(fNameOrStream,this);
    
    // Interact.println("   finished loading.");
    setTopBack();
    
    // codeTopBakLOADTIME=codeTop;
    
    return ok;
  }
  
  void terminate_file(int reg) throws PrologException {
    skipClause=false; // $$
    SETREG(codeTop,reg);
    codeTop++;
    linkCode(codeTopBak);
    shrink();
    // showCode();
  }
  
  /*
  void showCode() {
    try {
      BufferedWriter out=new BufferedWriter(new FileWriter("code.txt"));
      out.write("version: 3"+"\n");
      for(int i=0;i<codeTop;i++) {
        int op=RCGET(code[i]);
        int ai=MCGET(code[i]);
        int an=LCGET(code[i]);
        out.write("["+i+"] "+op+":"+ai+":"+an);
        switch(op) {
          case EXECUTE:
          case PROCEED:
          case TRY_ME_ELSE:
          case RETRY_ME_ELSE:
          case TRUST_ME_ELSE:
          case TRY_ME_ONLY:
          case NONDET:
          case EXEC_TRY:
          case EXEC_SWITCH:
          case SWITCH:
          case JUMP_IF:
          case EXEC_JUMP_IF:
          // case END:
          // out.write(":["+code[i+1]+"]=>"+(int)code[i+1]);
          break;
          default: // nothing
        }
        out.write("\n");
      }
      
      out.close();
    } catch(IOException e) {
    }
  }
  */
  
  /*
  222 ------> CLAUSE
  0
  1
  $dummy

  223 -----> FIRSTARG
  2
  0
  _

  17 ------> EXECUTE
  0
  1
  true

  0 ------> END
  0
  0
  wam
  */
  
  /*
  ii(clause,?,$end_dummy,1)
  ii(firstarg,?,_/0,2)
  % ii(get,variable,arg(1),var(1-1/2,1/2))
  % ii(put,value,arg(1),var(1-1/2,2/2))
  ii(execute,?,true,1)
  ii(end,?,0,wam)
  */
  
  void fake_end() throws PrologException {
    // int opcode, reg, arity;
    loadInstruction(CLAUSE,0,"$end_dummy",1);
    loadInstruction(FIRSTARG,2,"_",0);
    loadInstruction(EXECUTE,0,"true",1);
    loadInstruction(END,0,"wam",0); // LinkFlag=0
  }
  
  final private void resize_to(int realsize) {
    // int l=code.length;
    // Prolog.dump("@"+code.hashCode()+"resizing at:"+codeTop+":("+l+"):"+realsize);
    long[] newstack=new long[realsize];
    System.arraycopy(code,0,newstack,0,codeTop+1);
    this.code=newstack;
    if(PrologGC.trace>=2)
      Prolog.dump("@"+this.code.hashCode()+"=@"+newstack.hashCode()+"resized:"
          +codeTop+":"+code.length);
  }
  
  final private void expand() {
    if(codeTop+100>code.length)
      resize_to(code.length<<1);
  }
  
  final private void shrink() {
    // if(codeTop+100<code.length>>1) resize_to(code.length>>1);
    // if(Prolog.timeStamp>Prolog.LOADTIME) resize_to(codeTop+2);
    // if (atomTable.timeStamp>Prolog.LOADTIME&&codeTop+100<code.length>>1)
    // resize_to(codeTop+2);
  }
  
  private boolean skipClause=false;
  
  private long gensymCtr=0;
  
  private final String maybeGensymName(String name) {
    if(null!=name&&name.startsWith("$$_do")) {
      name=name+"_"+gensymCtr++;
    }
    return name;
  }
  
  final void loadInstruction(int op,int reg,String name,int arity)
      throws PrologException {
    expand();
    
    // Prolog.dump("x"+op+","+reg+","+arity+","+name+"!!!");
    // Interact.halt("end");
    
    if(null==name)
      Interact.warnmes("unexpected instr:"+codeTop+" Op:"+op+"-->"
          +getInstructionName(op)+" reg:"+reg+" "+name+"/"+arity);
    
    // $$ high risk change - ignores clauses when not grouped
    if(skipClause&&op!=CLAUSE) {
      // Prolog.dump("skiping op="+name); // $$
      return;
    }
    
    SETOP(codeTop,op);
    switch(op) {
      case CLAUSE: {
        skipClause=false; // $$ ##
        name=maybeGensymName(name);
        long pred=inputTerm(name,arity);
        boolean isNewPred=dict.setpred(pred,codeTop);
        if(curPred!=atomTable.G_true)
          SETLABEL(prevInstr,codeTop);
        reg++;
        // System.err.println("Prev instr:" + prevInstr);
        if(isNewPred) {
          if(curPred!=atomTable.G_true&&GETOP(codeTop-2)!=END) {
            switch(GETOP(prevInstr)) {
              case TRY_ME_ELSE: /* begin of single cls */
                // System.err.println("PREV CLAUSE: TRY_ME_ELSE");
                
                for(int p=prevInstr-2;p<=codeTop-4;p++)
                  code[p]=code[p+4];
                
                SETOP(codeTop-4,END);
                SETOP(codeTop-2,END);
              break;
              
              case RETRY_ME_ELSE:
                // System.err.println("PREV CLAUSE: TRUST_ME_ELSE");
                SETOP(prevInstr,TRUST_ME_ELSE);
              break;
              
              default:
                dumpCode(codeTop-10,codeTop+2);
                abortLoad("bad code in backpatching:"+op);
            }
          }
          curPred=pred;
          // System.err.println("CHG: SWITCH:" + codeTop);
          SETOP(codeTop,SWITCH);
          SETREG(codeTop,0);
          codeTop=SETFUN(codeTop,pred);
          codeTop++;
          // System.err.println("ADD: TRY_ME_ELSE:" + codeTop);
          SETOP(codeTop,TRY_ME_ELSE); /* reg = how far is fun=nextcls */
          SETREG(codeTop,arity); /* arity is here, not on the stack */
        } else if(curPred==pred) {
          // System.err.println("CHG: RETRY_ME_ELSE:" + codeTop);
          SETOP(codeTop,RETRY_ME_ELSE);
          SETREG(codeTop,arity);
        } else {
          // dumpCode(codeTop-5, codeTop+5);
          skipClause=true;
          // $$ abortLoad(
          Interact.warnmes(1,"*** Predicate "+name+"/"+arity
              +" leads other group of clauses. Clause ignored!");
          return;
        }
        prevInstr=codeTop;
        codeTop++;
      }
      break;
      
      case FIRSTARG: /* MaxReg-FunFirstarg/Arity */
        if(reg>=MAXREG)
          abortLoad("Load instruction: not enough registers");
        {
          long funval=inputTerm(name,arity);
          if(name.length()>0&&(name.charAt(0)=='_')
              ||!dict.hdef(curPred,funval,codeTop)) {
            int label=dict.getpred(curPred);
            if(label==Dict.EMPTY)
              abortLoad("Load instruction, FIRSTARG: current predicate not found");
            SETOP(label,NONDET);
          }
        }
        codeTop--; /* null effect, as we do codeTop++ later */
      break;
      
      case EXECUTE:
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
      break;
      
      case PUT_VARIABLE:
      case GET_VALUE:
        SETREG(codeTop,reg);
        SETLEFT(codeTop,arity);
      break;
      
      case GET_STRUCTURE:
      case PUT_STRUCTURE:
        SETREG(codeTop,reg);
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
      break;
      
      case UNIFY_VARIABLE:
        if(OCOMPRESS(reg,UNIFY_VALUE,UNIFY_VAL_VAR,GET_STRUCTURE,
            GET_UNIFY_VAL_VAR))
          break;
        if(OCOMPRESS(reg,UNIFY_VARIABLE,UNIFY_VAR_VAR,GET_STRUCTURE,
            GET_UNIFY_VAR_VAR))
          break;
        SETREG(codeTop,reg);
      break;
      
      case UNIFY_VALUE:
        if(OCOMPRESS(reg,UNIFY_VALUE,UNIFY_VAL_VAL,GET_STRUCTURE,
            GET_UNIFY_VAL_VAL))
          break;
        if(OCOMPRESS(reg,UNIFY_VARIABLE,UNIFY_VAR_VAL,GET_STRUCTURE,
            GET_UNIFY_VAR_VAL))
          break;
        SETREG(codeTop,reg);
      break;
      
      case WRITE_VARIABLE:
        if(OCOMPRESS(reg,WRITE_VALUE,WRITE_VAL_VAR,PUT_STRUCTURE,
            PUT_WRITE_VAL_VAR))
          break;
        if(OCOMPRESS(reg,WRITE_VARIABLE,WRITE_VAR_VAR,PUT_STRUCTURE,
            PUT_WRITE_VAR_VAR))
          break;
        SETREG(codeTop,reg);
      break;
      
      case WRITE_VALUE:
        if(OCOMPRESS(reg,WRITE_VALUE,WRITE_VAL_VAL,PUT_STRUCTURE,
            PUT_WRITE_VAL_VAL))
          break;
        if(OCOMPRESS(reg,WRITE_VARIABLE,WRITE_VAR_VAL,PUT_STRUCTURE,
            PUT_WRITE_VAR_VAL))
          break;
        SETREG(codeTop,reg);
      break;
      
      case MOVE_REG:
        if(1==prevLen&&MOVE_REG==GETOP(codeTop-1)
            &&!(1==prevPrevLen&&MOVE_REGx2==GETOP(codeTop-2)))
          SETOP(codeTop-1,MOVE_REGx2);
        SETREG(codeTop,reg);
        SETLEFT(codeTop,arity);
      break;
      
      case LOAD_VALUE:
        if(1==prevLen&&LOAD_VALUE==GETOP(codeTop-1)&&1==GETREG(codeTop-1)
            &&2==reg) {
          SETOP(codeTop-1,LOAD_VALUEx2);
          SETREG(codeTop-1,arity);
          codeTop--;
          break;
        }
        /* An = REG = second,  Ai = LEFT = first */
        SETREG(codeTop,reg);
        SETLEFT(codeTop,arity);
      break;
      
      case LOAD_CONSTANT: {
        // System.err.println("LOAD_VAL_SHORT OP:"+op);
        long small;
        if(1==prevLen&&LOAD_VALUE==GETOP(codeTop-1)&&1==GETREG(codeTop-1)
            &&2==reg) {
          small=inputTerm(name,arity);
          int smallVal=OUTPUT_INT(small);
          if(isINTEGER(small)&&smallVal>=0&&smallVal<MAXREG) {
            op=LOAD_VAL_SHORT;
            SETOP(codeTop-1,op);
            SETREG(codeTop-1,xx((int)small));
            codeTop--;
            // System.err.println("LOAD_VAL_SHORT:"+smallVal);
            break;
          }
        }
      }
        SETREG(codeTop,reg);
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
      break;
      
      case GET_CONSTANT:
      case PUT_CONSTANT:
        
      case UNIFY_CONSTANT:
      case WRITE_CONSTANT:
        SETREG(codeTop,reg);
        codeTop=SETFUN(codeTop,inputTerm(name,arity));
      break;
      
      /*
       * triggers linking
       */
      case END:
        terminate_file(reg);
      break;
      
      case ARITH:
        // Interact.dump("ARITH:"+(GETOP(codeTop)+" "+arity));
        SETOP(codeTop,(GETOP(codeTop)+arity));
        if(reg!=0) {
          SETREG(codeTop,reg);
          SETLEFT(codeTop,name.charAt(0)-'0');
        }
      break;
      
      case INLINE:
      case BUILTIN:
        SETREG(codeTop,reg);
        SETOP(codeTop,(GETOP(codeTop)+arity));
      break;
      
      default:
        SETREG(codeTop,reg);
    }
    // if (codeTop >= codeMax) abortLoad("Code store overflow.");
    prevPrevLen=prevLen;
    prevLen=getInstructionLength(op);
    codeTop++;
  }
  
  private void linkCode(int instr) throws PrologException {
    // Prolog.dump("linking code starting at: "+instr);
    
    if(Prolog.DEBUG)
      showLowLevel();
    
    int label=0;
    long f;
    int instrLen=1;
    for(;instr<codeTop;instr+=instrLen) {
      int op=GETOP(instr);
      // if(op>=MAXOP) {Interact.println(instr+": op:!!!? "+op);instrLen=1;}
      // else
      instrLen=getInstructionLength(op);
      switch(op) {
        case EXECUTE: /* we look to what's next */
          f=GETFUN(instr);
          label=Dict.EMPTY;
          try {
            label=dict.getpred(f);
          } catch(PrologException ignore) {
          } // FIXED - PT
          
          if(dict.do_isEmpty(label)) {
            
            Interact.warnmes(4,"Undefined after :-, code["+instr+".."+codeTop
                +"] ->"+atomTable.getSym(f)+"/"+GETARITY(f));
            
            // abortLoad("Undefined predicate after :-"); //FIXED: this gets
            // caught without message
            
            // dict.hdef(atomTable.G_undefined, f, instr);
            SETOP(instr,APPLY);
            // Interact.println(instr+": label:=>"+GETLABEL(instr));
            continue;
          }
          switch(GETOP(label)) { // still linking EXECUTE - this is where it
          // points to
          
            case TRUE_0:
              SETOP(instr,TRUE_0);
              SETREG(instr,1);
              SETOP(instr+getInstructionLength(TRUE_0),NOP);
              SETREG(instr+getInstructionLength(TRUE_0),0);
            break;
            
            case CALL_1:
              SETOP(instr,CALL_1);
              SETREG(instr,1);
              SETOP(instr+getInstructionLength(CALL_1),NOP);
              SETREG(instr+getInstructionLength(CALL_1),0);
            break;
            
            case NONDET:
              SETOP(instr,EXEC_TRY);
              SETLABEL(instr,label+getInstructionLength(NONDET));
            break;
            
            case TRY_ME_ELSE:
              SETOP(instr,EXEC_TRY);
              SETLABEL(instr,label);
            break;
            
            case JUMP_IF:
              SETOP(instr,EXEC_JUMP_IF);
              SETLABEL(instr,label);
            break;
            
            case SWITCH:
              if(linkSwitch(label,false)) {
                SETOP(instr,EXEC_JUMP_IF);
                SETLABEL(instr,label);
              } else {
                SETOP(instr,EXEC_SWITCH);
                SETLABEL(instr,label);
              }
            break;
            
            default:
              SETLABEL(instr,label);
          }
        break; // end EXECUTE
        
        case SWITCH:
          linkSwitch(instr,true);
        break;
        
        case NONDET: // if first and last -> TRY_ME_ONLY
          label=dict.getpred(GETFUN(instr));
          // System.err.println("here!!!"+getAtomName(label)+getAtomName(atomTable.G_empty));
          if(label!=Dict.EMPTY)
            break; // !!! already linked as a pred, ok as is
          dict.setpred(GETFUN(instr),label+getInstructionLength(NONDET));
          SETOP(label,TRY_ME_ONLY); // because it has just this clause
        break;
        
        default:
          if(0==instrLen) {
            instrLen=1;
          }
      }
    }
  }
  
  private boolean linkSwitch(int p,boolean doit) {
    int jlabel=GETLABEL(p+2);
    int label=jlabel;
    if(TRUST_ME_ELSE==GETOP(label)&&GET_UNIFY_VAR_VAR==GETOP(label+2)) {
      if(doit) {
        SETOP(p,JUMP_IF);
        label+=2;
        SETLABEL(p,label);
      }
      return jlabel!=0;
    }
    return false;
  }
  
  private void abortLoad(String msg) throws PrologException {
    throw new PrologException("Load instruction: "+msg+" code=@"
        +code.hashCode()+"top:"+codeTop);
  }
  
  void dumpCode(int instrPtr) throws PrologException {
    dumpCode(instrPtr-10,instrPtr+10);
  }
  
  void dumpCode(int instrFrom,int instrTo) throws PrologException {
    if(instrFrom<0)
      instrFrom=0;
    
    Prolog.dump("CODE: from:"+instrFrom+" to:"+instrTo+" used:"+codeTop+" of:"
        +code.length+".");
    
    for(int i=instrFrom;i<instrTo&&i<codeTop;i+=getInstructionLength(GETOP(i)))
      dumpInstruction(i,i);
    
    Prolog.dump("end of dump");
  }
  
  void dumpInstruction(int instrCount,int i) {
    int op=GETOP(i);
    String name=getInstructionName(op);
    switch(op) {
      case END:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name);
      break;
      case EXECUTE:
      case NONDET:
      case SWITCH:
      case EXEC_SWITCH:
      case JUMP_IF:
      case EXEC_JUMP_IF:
      case EXEC_TRY:
        Prolog.dump("");
      case TRY_ME_ELSE:
      case RETRY_ME_ELSE:
      case TRUST_ME_ELSE:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name+" "+GETREG(i)
            +"->["+GETFUN(i)+"]");
      break;
      case MOVE_REG: // MOVE INSTRUCTIONS:
      case PUT_VARIABLE:
      case GET_VALUE:
      case LOAD_VALUE:
      case UNIFY_VAR_VAR: // DOUBLE INSTRUCTIONS:
      case WRITE_VAR_VAR:
      case UNIFY_VAL_VAL:
      case WRITE_VAL_VAL:
      case UNIFY_VAR_VAL:
      case WRITE_VAR_VAL:
      case UNIFY_VAL_VAR:
      case WRITE_VAL_VAR:
      case MOVE_REGx2:
      case LOAD_VALUEx2:
      case LOAD_VAL_SHORT:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name+" X"+GETREG(i)
            +",A"+GETARG(i));
      break;
      default:
        Prolog.dump("#"+instrCount+"\t<"+i+"> op:"+op+"/"+name);
        if(0!=GETREG(i))
          Prolog.dump(" X"+GETREG(i));
        if(2==getInstructionLength(op)) {
          long f=GETFUN(i);
          Prolog.dump(" "+atomTable.getSym(f)+"/"+GETARITY(f));
        }
        Prolog.dump("");
    }
  }
  
  private static final int getInstructionLength(int op) {
    if(op<0||(op>NOP&&op<CLAUSE)||op>=MAXOP)
      Interact.warnmes("warning - bad operation: "+op);
    return instructionLength[op];
  }
  
  final static byte[] initInstructionLengths() {
    byte[] ilen=new byte[MAXOP];
    
    // set instruction lengths to 1
    for(int i=0;i<MAXOP;i++) {
      ilen[i]=1;
    }
    // except for:
    ilen[GET_STRUCTURE]=2;
    ilen[PUT_STRUCTURE]=2;
    ilen[UNIFY_CONSTANT]=2;
    ilen[WRITE_CONSTANT]=2;
    ilen[GET_CONSTANT]=2;
    ilen[PUT_CONSTANT]=2;
    ilen[EXECUTE]=2;
    ilen[PROCEED]=2;
    ilen[END]=2;
    ilen[TRY_ME_ELSE]=2;
    ilen[RETRY_ME_ELSE]=2;
    ilen[TRUST_ME_ELSE]=2;
    ilen[TRY_ME_ONLY]=2;
    ilen[NONDET]=2;
    ilen[EXEC_TRY]=2;
    ilen[EXEC_SWITCH]=2;
    ilen[SWITCH]=2;
    ilen[JUMP_IF]=2;
    ilen[EXEC_JUMP_IF]=2;
    ilen[LOAD_CONSTANT]=2;
    ilen[GET_UNIFY_VAR_VAR]=2;
    ilen[GET_UNIFY_VAL_VAL]=2;
    ilen[GET_UNIFY_VAR_VAL]=2;
    ilen[GET_UNIFY_VAL_VAR]=2;
    ilen[PUT_WRITE_VAR_VAR]=2;
    ilen[PUT_WRITE_VAL_VAL]=2;
    ilen[PUT_WRITE_VAR_VAL]=2;
    ilen[PUT_WRITE_VAL_VAR]=2;
    // Interact.println(APPLY+":"+MAXOP);
    // ilen[APPLY] = 2; //$$ not expected to cause problem
    
    return ilen;
  }
  
  final static String getInstructionName(int c) {
    return "op_"+c;
  }
  
} // end class CodeStore
