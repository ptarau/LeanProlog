package vm.logic;

/** Definition of Prolog instruction codes and static data transformer methods.
* This interface describes only WAM instructions.
* The definitions for builtin predicates can be found in {@link BuiltinIDs BuiltinIDs}.
*<p>The Prolog engine defines a set of registers which the following instructions manipulate.
* A small set of temporary registers is also defined. These are indicated by X(i) below.
*<p>Instructions can be divided into one word, two or three word formats.
* The first word (WORD1) has the following layout:
*<p>
* <table border=1>
* <tr><td>REG id<td>ARG id<td>OP id
* <tr><td>11 bits<td>11 bits<td>9 bits
* </table>
*<p>
* Where
*<ul>
*<li>REG is either the index of a WAM register, or an immediate operand(depending on instruction).
*<li>ARG is the index of a WAM register.
*<li>OP is the instruction id, from the list below augmented by the list of builtin operations.
*</ul>
*<p>
* The format of the second, or third instruction words (WORD2, WORD3 respectively) depends on the instruction.
*
*<p>The following notations are used below:
*<table>
*<tr><td>&middot;<td>An()<td>The contents of the register indicated by the REG instruction field.
*<tr><td>&middot;<td>Ai()<td>The contents of the register indicated by the ARG instruction field.
*<tr><td>&middot;<td>newvar()<td>Push a new unbound variable onto the heap.
*<tr><td>&middot;<td>heap()<-expr<td>Push expression onto the heap.
*<tr><td>&middot;<td>currentArg()<td>Heap index for current argument of the current predicate/functor.
*<tr><td>&middot;<td>nextArg()<td>Heap index for argument following currentArg for the current predicate/functor.
*<tr><td>&middot;<td>noCurrentArg()<td>No currentArg is defined at this point.
*<tr><td>&middot;<td>cutB<td>The index of the choice point which should be restored on failure.
*</table>
*
*<p>
* Note:
<br>Some instructions only appear in the compiled format. They are resolved by the
* loader into other runtime instructions (eg CLAUSE).
*<br> Compound instructions do not exist in the compiled format. For efficiency they
* are produced by the loader to represent in compressed form a given sequence of 
* basic instructions.
*
*<p>Unless otherwise noted instruction execution continues with the next compiled instruction.
*/

public class Defs implements Stateful { // abstract
  private static final long serialVersionUID=222L;
  
  public static final int xx(long x) {
    int ix=(int)x;
    // if((long)ix!=x)
    // Interact.errmes("x="+x+"<>"+"ix="+ix);
    return ix;
  }
  
  static final int END=0;
  
  /** Unify unbound An() with currentArg.
  *<p>This is equivalent to An()<-currentArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VARIABLE WRITE_VARIABLE} instead.
  */
  static final int UNIFY_VARIABLE=1;
  
  /** An()<-newvar().
  *<p> Set register An() to point to a new unbound variable allocated from the heap.
  */
  static final int WRITE_VARIABLE=2;
  
  /** Unify An() with currentArg.
  *<p>Fail if An() and currentArg don't unify.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VALUE WRITE_VARLUE} instead.
  */
  static final int UNIFY_VALUE=3;
  
  /** heap()<-An(). */
  static final int WRITE_VALUE=4;
  
  /** Unify currentArg with constant term.
  *<p>The constant term is retrived from WORD2.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_CONSTANT WRITE_CONSTANT} instead.
  */
  static final int UNIFY_CONSTANT=5;
  
  /** heap()<-constant term.
  * The constant term is stored in WORD2.
  */
  static final int WRITE_CONSTANT=6;
  
  /** Bind An() to a constant.
  *<p>If An() dereferences to an unbound variable then bind it to the constant term in WORD2.
  * The variable is optionally trailed.
  *<p> Otherwise fail if the values differ.
  */
  static final int GET_CONSTANT=7;
  
  static final int GET_STRUCTURE=8;
  
  /** An()<-constant term.
  *<p> The constents of the An() register are set to the value of WORD2 which contains
  * a constant term.
  */
  static final int PUT_CONSTANT=9;
  
  /** An()<-heap()<-functor term.
  *<p> A functor term stored in WORD2 is pushed onto the heap. The An() register is set to the new heap index of this functor term.
  */
  static final int PUT_STRUCTURE=10;
  
  /** An()<-Ai().
  * Move contents of Ai() register to An() register.
  */
  static final int MOVE_REG=11;
  
  /** An()<-newVar(), Ai()<-An(). */
  static final int PUT_VARIABLE=12;
  
  /** Bind An() and Ai() registers.
  *<p>Fail if An() and Ai() don't unify. 
  */
  static final int GET_VALUE=13;
  
  /** heap()<-cutB.
  * Push the cutB choice point index onto the heap.
  */
  static final int PUSH_CUT=14;
  
  /** Set the current choice point back to that saved in cutB. */
  static final int PUT_CUT=15;
  
  /** Set the current choice point back to that saved in the dereferenced contents of regs[1]. */
  static final int GET_CUT=16;
  
  /** Transfer control to the instructions for a particular predicate.
  *<br>cutB is set to the current choice point.
  *<br>WORD2 contains the instruction index for the required predicate.
  *<br>At load time the instruction index is set by resolving the predicate name/arity.
  *<br>Execution continues at the specified instruction.
  */
  static final int EXECUTE=17;
  
  static final int PROCEED=18;
  
  /** First clause of a non-deterministic predicate.
  *<p>Create a new choice point.
  *<br>The REG field indicates the arity of the predicate.
  *<br>WORD2 contains the instruction index for the first clause of this predicate.
  *<br>Jump to the new instruction.
  */
  static final int TRY_ME_ELSE=19;
  
  /** Middle clauses of a non-deterministic predicate.
  * Backtrack to try another clause. 
  *<br>The REG field indicates the arity of the predicate - but is not used by this implementation
  * as the choice point retains this value.
  *<br>WORD2 contains the instruction index for the next clause of this predicate.
  *<br>Jump to the new instruction.
  */
  static final int RETRY_ME_ELSE=20;
  
  /** Last clause of a non-deterministic predicate.
  * Backtrack to the choice point prior to the one set by TRY_ME_ELSE.
  * Hence get Tail Recursion Optimization.
  *<br>The REG field indicates the arity of the predicate - but is not used by this implementation
  * as the choice point retains this value.
  *<br>WORD2 contains the instruction index for the last clause of this predicate.
  *<br>Jump to the new instruction.
  */
  static final int TRUST_ME_ELSE=21;
  
  static final int TRY_ME_ONLY=22;
  
  static final int NONDET=23;
  
  /** This instruction combines {@link InstructionIDs#EXECUTE EXECUTE}
  * and {@link InstructionIDs#SWITCH SWITCH} instructions.
  *<p>This instruction is produced by the loader.
  */
  static final int EXEC_SWITCH=24;
  
  /** Transfer control to the clause determined by the value of reg[1].
  * The SWITCH instruction is stored in the code store in the following format:
  *<p>
  *<table border=1>
  *<tr><td>REG unused<td>ARG unused<td>SWITCH
  *<tr><td>WORD1<td colspan=2>predicate id.
  *<tr><td>WORD2<td colspan=2>destination instruction if reg[1]<sup>*</sup> is an unbound variable.
  *</table>
  * * refers to the dereferenced value.
  *<p> If reg[1] is an unbound variable continue execution at the instruction index in WORD2.
  *<p> Otherwise lookup the value stored for the combined predicate id. in WORD1
  * and the dereferenced value of reg[1]. This yields the instruction index of the target clause.
  * Execution continues at this instruction.
  * The operation fails if an unknown value is encountered.
  */
  static final int SWITCH=25;
  
  /** This instruction combines {@link InstructionIDs#EXECUTE EXECUTE} and {@link InstructionIDs#JUMP_IF JUMP_IF} instructions.
  *<p>This instruction is produced by the loader.
  */
  static final int EXEC_JUMP_IF=26;
  
  /** Transfer control depending on the contents of reg[1].
  * The JUMP_IF instruction is stored in the code store in the following format:
  *<p>
  *<table border=1>
  *<tr><td>REG<td>ARG<td>JUMP_IF
  *<tr><td>WORD1<td colspan=2>test term value
  *<tr><td>WORD2<td colspan=2>destination instruction if reg[1]<sup>*</sup> is an unbound variable.
  *<tr><td>WORD3<td colspan=2 rowspan=2>destination instruction if reg[1]<sup>*</sup> equals the test term.
  *		<br>An()<-??, Ai()<-??.
  *</table>
  * * refers to the dereferenced value.
  *<p>Otherwise execution continues with the next instruction.

  */
  static final int JUMP_IF=27;
  
  /** X(REG)<-constant term.
  *<p> A constant term is placed in temporary register X(REG).
  * Here REG indictes the index of a temporary register rather than the usual An() register.
  */
  static final int LOAD_CONSTANT=28;
  
  /** X(REG)<-Ai().
  *<p> The dereferenced value of register Ai() is placed in temporary register X(REG).
  * Here REG indictes the index of a temporary register rather than the usual An() register.
  */
  static final int LOAD_VALUE=29;
  
  static final int GET_UNIFY_VAR_VAR=30;
  
  /** Unify unbound An() with currentArg and unbound Ai() with nextArg.
  *<p>This is equivalent to An()<-currentArg, Ai()<-nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAR_VAR WRITE_VAR_VAR} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAR_VAR=31;
  
  /** An()<-heap()<-functor term, An()<-newvar(), Ai()<-newvar().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAR_VAR WRITE_VAR_VAR}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAR_VAR=32;
  
  /** An()<-newvar(), Ai()<-newvar(). */
  static final int WRITE_VAR_VAR=33;
  
  static final int GET_UNIFY_VAL_VAL=34;
  
  /** Unify An() with currentArg and Ai() with nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAR_VAR WRITE_VAL_VAL} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAL_VAL=35;
  
  /** An()<-heap()<-functor term, heap()<-An(), heap()<-Ai().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAL_VAL WRITE_VAL_VAL}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAL_VAL=36;
  
  /** heap()<-An(), heap()<-Ai(). */
  static final int WRITE_VAL_VAL=37;
  
  static final int GET_UNIFY_VAR_VAL=38;
  
  /** Unify unbound An() with currentArg and Ai() with nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAR_VAL WRITE_VAR_VAL} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAR_VAL=39;
  
  /** An()<-heap()<-functor term, An()<-newvar(), heap()<-Ai().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAR_VAL WRITE_VAR_VAL}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAR_VAL=40;
  
  /** An()<-newvar(), heap()<-Ai(). */
  static final int WRITE_VAR_VAL=41;
  
  static final int GET_UNIFY_VAL_VAR=42;
  
  /** Unify An() with currentArg and unbound Ai() with nextArg.
  *<p>If noCurrentArg perform {@link InstructionIDs#WRITE_VAL_VAR WRITE_VAL_VAR} instead.
  *<p>This instruction is produced by the loader.
  */
  static final int UNIFY_VAL_VAR=43;
  
  /** An()<-heap()<-functor term, heap()<-An(), Ai()<-newvar().
  *<p>This is a combination of {@link InstructionIDs#PUT_STRUCTURE PUT_STRUCTURE}
  * and {@link InstructionIDs#WRITE_VAL_VAR WRITE_VAL_VAR}.
  *<p>WORD2 contains the functor term.
  *<p>WORD3 is formated like WORD1 to provide the An() and Ai() references.
  */
  static final int PUT_WRITE_VAL_VAR=44;
  
  /** heap()<-An(), Ai()<-newvar(). */
  static final int WRITE_VAL_VAR=45;
  
  /** An()<sub>1</sub><-Ai()<sub>1</sub>, An()<sub>2</sub><-Ai()<sub>2</sub>.
  *<p>A pair of MOVE_REG instructions coded as one instruction.
  *<br>Move contents of Ai() register to An() register.
  *<br>The contents of WORD2 are then interpreted in WORD1 format 
  *yielding a new set of register references.
  *<br>The contents of the new Ai() register are moved to the new An() register.
  * @see InstructionIDs#MOVE_REG MOVE_REG
  */
  static final int MOVE_REGx2=46;
  
  /** X(1)<-Ai(), X(2)<-An().
  *<br> The dereferenced value of register Ai() is placed in temporary register X(1).
  *<br> The dereferenced value of register An() is placed in temporary register X(2).
  */
  static final int LOAD_VALUEx2=47;
  
  /** X(1)<-Ai(), X(2)<-a small integer.
  *<br> The dereferenced value of register Ai() is placed in temporary register X(1).
  *<br> A small integer derived from the contents of REG field/An() 
  *is placed in temporary register X(2).
  */
  static final int LOAD_VAL_SHORT=48;
  
  /** This instruction combines {@link InstructionIDs#EXECUTE EXECUTE}
  * and {@link InstructionIDs#TRY_ME_ELSE TRY_ME_ELSE} instructions.
  *<p>This instruction is produced by the loader.
  */
  static final int EXEC_TRY=49;
  
  static final int LOAD_VARIABLE=50;
  
  /**
   * END OF COMMON SEGMENT SHARED WITH BinProlog 11.x
   */
  
  static final int APPLY=63; // not really a builtin with arity -> 63 was 50
  
  static final int BUILTIN=64; // n_builtin
  
  static final int TRUE_0=BUILTIN+0; // 64
  
  static final int CALL_1=BUILTIN+1; // 65
  
  static final int INLINE=66; // was 53
  
  static final int FAIL_0=INLINE+0; // 66
  
  static final int CWRITE_1=INLINE+1; // 67
  
  static final int CNL_0=INLINE+2; // 68
  
  static final int IS_COMPILED_1=INLINE+3; // 69
  
  static final int RETURN_1=INLINE+4; // 70
  
  static final int ARITH=71;
  
  // GAP for strip_cont0 == 70=> ARITH+0
  
  static final int CURRENT_ENGINE_1=ARITH+1; // 72
  
  static final int CREATE_ENGINE_3=ARITH+2; // 73
  
  static final int ENGINE_GET_2=ARITH+3; // 74
  
  static final int ENGINE_STOP_1=ARITH+4; // 75
  
  static final int TO_ENGINE_2=ARITH+5; // 76
  
  static final int FROM_ENGINE_1=ARITH+6; // 77
  
  static final int THIS_CLASS_1=ARITH+7; // 78
  
  static final int ARG_3=ARITH+8;
  
  static final int ARITY_2=ARITH+9;
  
  static final int FUN_3=ARITH+10;
  
  static final int SUCC_2=ARITH+11;
  
  static final int TYPE_OF_2=ARITH+12;
  
  static final int ICALL_3=ARITH+13;
  
  static final int ICALL_OP_4=ARITH+14;
  
  static final int ICALL_IMPURE_5=ARITH+15;
  
  static final int XCALL_3=ARITH+16;
  
  static final int LAST_BUILTIN=220; // 3 other codes after this - used by the
                                     // compiler only !!!
  
  // --------------------------------------------
  
  static final int NOP=LAST_BUILTIN+1;
  
  /**
   * This instruction indicates the start of a new clause in the compiled instructions.
   * During loading this instruction is replaced by {@link InstructionIDs#TRY_ME_ELSE TRY_ME_ELSE},
   * {@link InstructionIDs#RETRY_ME_ELSE RETRY_ME_ELSE} or {@link InstructionIDs#TRUST_ME_ELSE TRUST_ME_ELSE} instructions.
   *<p>This instruction is not present at run time.
   */
  static final int CLAUSE=NOP+1; // 222
  
  static final int FIRSTARG=NOP+2; // 223
  
  /** Maximum value for instruction codes, both WAM and Builtin.*/
  final static int MAXOP=256;
  
  /**
  * "Macros" start here
  */
  
  private static final int INSTR_SIZE=64;
  
  private static final int CELL_SIZE=64;
  
  private static final int ARITYBITS=30;
  
  private static final int REGBITS=22;
  
  private static final int ARGBITS=22;
  
  final static int MAXARITY=(1<<ARITYBITS)-1;
  
  private static final int OPBITS=(INSTR_SIZE-REGBITS-ARGBITS);
  
  // if OPBITS > 10 it will be slow on SPARCs
  
  static final int MAXREG=(1<<12)-1;
  
  /* DATA AREA FORMATS & INSTRUCTION FORMATS */
  
  private static final int TAGBITS=4;
  
  public static final int MAXINT=(1<<28)-1;
  
  // static final int getTagBits() {return TAGBITS;}
  /**********************LOW-LEVEL TERM OPERATIONS*****************/
  // Term representation:
  // int <INTTAG><integer>
  // fun <FUNTAG><Symbol><Arity>
  // var <VARTAG><heap reference> : this is just a heap reference as VARTAG=0
  // |TAG|SYMNO|ARITY| or |TAG|DATA|
  
  private static final int LBITS=TAGBITS;
  
  private static final int RBITS=ARITYBITS;
  
  private static final int MBITS=(CELL_SIZE-LBITS-RBITS);
  
  private static final long RMASK=((1L<<RBITS)-1);
  
  private static final long LMASK=(~0L<<(MBITS+RBITS));
  
  private static final long MMASK=(~LMASK&~RMASK);
  
  static final int SYMBITS=MBITS;
  
  private static final int LGET(long W) {
    return (int)(W>>>(MBITS+RBITS));
  }
  
  private static final long LPUT(long W,long val) {
    return (W&~LMASK)|(val<<(MBITS+RBITS));
  }
  
  private static final int MGET(long W) {
    return (int)((W)<<LBITS>>>(LBITS+RBITS));
  }
  
  private static final long MPUT(long W,long val) {
    return((W&~MMASK)|(val<<(LBITS+RBITS)>>>LBITS));
  }
  
  private static final int RGET(long W) {
    return (int)(W&RMASK);
  }
  
  private static final long RPUT(long W,long val) {
    return (W&~RMASK)|val;
  }
  
  /*******************INTERFACE TERM OPERATIONS ******************/
  
  private static final long PUTTAG(long W,long val) {
    return LPUT(W,val);
  }
  
  static final long PUTSYMNO(long W,long val) {
    return MPUT(W,val);
  }
  
  static final int GETSYMNO(long W) {
    return MGET(W);
  }
  
  static final long PUTARITY(long W,long val) {
    return RPUT(W,val);
  }
  
  static final int GETARITY(long W) {
    return RGET(W);
  }
  
  /* 
   VAR tags are assumed 0 - otherwise it can all change
   */
  static private final long TAGMASK=LMASK;
  
  static private final long VARTAG=PUTTAG(0L,0); // assumed 0
  
  static private final long FUNTAG=PUTTAG(0L,1);
  
  static private final long INTTAG=PUTTAG(0L,2);
  
  // static private final long noTAG = PUTTAG(0,3);
  
  /*************** HIGH-LEVEL TERM OPERATIONS************************/
  static final boolean isNONVAR(long hRef) {
    return (hRef&TAGMASK)!=VARTAG;
  }
  
  // {return isIDENTIFIER(hRef) || isINTEGER(hRef);}
  static final boolean isVAR(long hRef) {
    return (hRef&TAGMASK)==VARTAG;
  }
  
  static final boolean isINTEGER(long hRef) {
    return (hRef&TAGMASK)==INTTAG;
  }
  
  static final boolean isIDENTIFIER(long hRef) {
    return (hRef&TAGMASK)==FUNTAG;
  }
  
  static final boolean isSYMCONST(long hRef) {
    return isIDENTIFIER(hRef)&&GETARITY(hRef)==0;
  }
  
  static final boolean isCOMPOUND(long hRef) {
    return isIDENTIFIER(hRef)&&GETARITY(hRef)!=0;
  }
  
  static final boolean isATOMIC(long hRef) {
    return isINTEGER(hRef)||isSYMCONST(hRef);
  }
  
  // ************** Packing/unpacking integer terms.
  // FIXED UGLY BUG with negative numbers confused with compound terms - uses
  // unsigned right shift >>> PT
  static final long INPUT_INT(long val) {
    return ((val<<TAGBITS)>>>TAGBITS)|INTTAG;
  }
  
  static final int OUTPUT_INT(long val) {
    return (int)((val<<TAGBITS)>>TAGBITS);
  }
  
  static final long addArity(int symno,int arity) {
    return PUTARITY(PUTSYMNO(FUNTAG,symno),arity);
  }
  
  /*************** LOW-LEVEL INSTRUCTION OPERATIONS *****************/
  
  private static final int LCBITS=REGBITS;
  
  private static final int MCBITS=ARGBITS;
  
  private static final int RCBITS=OPBITS;
  
  private static final long RCMASK=((1L<<RCBITS)-1);
  
  private static final long LCMASK=(~0L<<(MCBITS+RCBITS));
  
  private static final long MCMASK=(~LCMASK&~RCMASK);
  
  static final int LCGET(long W) {
    return (int)(W>>>(MCBITS+RCBITS));
  }
  
  static final long LCPUT(long W,long val) {
    return((W&~LCMASK)|(val)<<(MCBITS+RCBITS));
  }
  
  static final int MCGET(long W) {
    return (int)(W<<(LCBITS)>>>(LCBITS+RCBITS));
  }
  
  static final long MCPUT(long W,long val) {
    return (W&~MCMASK)|(val<<(LCBITS+RCBITS)>>>LCBITS);
  }
  
  static final int RCGET(long W) {
    return (int)(W&RCMASK);
  }
  
  static final long RCPUT(long W,long val) {
    return (W&~RCMASK)|val;
  }
  
  /**
     shows content of a cell
   */
  
  static String showCell(long val) {
    String s;
    if(isVAR(val))
      s="_"+val;
    else if(isINTEGER(val))
      s=""+OUTPUT_INT(val);
    else if(isIDENTIFIER(val))
      s="<"+GETSYMNO(val)+">/"+GETARITY(val);
    else
      s="unknown?"+LGET(val)+"#"+val;
    return s;
  }
  
  /**
    shows some low level data
   */
  void showLowLevel() {
    if(true) {
      // long f;
      try {
        Prolog.dump("\nLow level data\n"+"\nminus one:"+INPUT_INT(-1)
            +" compound -1:"+isCOMPOUND(INPUT_INT(-1))+"\nplus one:"
            +INPUT_INT(1)+" compound 1:"+isCOMPOUND(INPUT_INT(1))+"\nminus 2:"
            +INPUT_INT(-2)+" compound -2000:"+isCOMPOUND(INPUT_INT(-2000))+
            // "\nf/1:"+(f=inputTerm("f",1))+
            // " compound f/1:"+isCOMPOUND(f)+" arity:"+GETARITY(f)+
            "\nFUNTAG:"+FUNTAG+" INTTAG:"+INTTAG+"\n");
      } catch(Exception ignore) {
      }
    }
    
  }
}
