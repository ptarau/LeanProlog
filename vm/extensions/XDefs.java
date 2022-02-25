package vm.extensions;

public interface XDefs {
  public static final long serialVersionUID=222L;
  
  public static final int NEGATE=0;
  
  public static final int ABS=1;
  
  public static final int COMPARE=2;
  
  public static final int PLUS=3;
  
  public static final int SUBTRACT=4;
  
  public static final int MULTIPLY=5;
  
  public static final int POW=6;
  
  public static final int MIN=7;
  
  public static final int MAX=8;
  
  public static final int DIVIDE=9;
  
  public static final int BITCOUNT=10;
  
  public static final int LSHIFT=11;
  
  public static final int RSHIFT=12;
  
  public static final int GETBIT=13;
  
  public static final int SETBIT=14; // 0 or 1
  
  public static final int FLIPBIT=15;
  
  // TODO: test boolean ops
  
  public static final int ITE=16;
  
  public static final int NOT=17;
  
  public static final int AND=18; // done
  
  public static final int OR=19;
  
  public static final int XOR=20;
  
  public static final int EQ=21;
  
  // todo
  
  public static final int IMPL=22;
  
  public static final int LESS=23; // ~a /\ b
  
  public static final int DIV=24;
  
  public static final int MOD=25;
  
  public static final int GCD=26;
  
  public static final int MODINV=27;
  
  public static final int GC=28;
  
  public static final int TO_CODES=29;
  
  public static final int FROM_CODES=30;
  
  public static final int TO_NCODES=31;
  
  public static final int FROM_NCODES=32;
  
  public static final int HALT=33;
  
  public static final int NEW_INTERACTOR=34;
  
  public static final int ASK_INTERACTOR=35;
  
  public static final int TELL_INTERACTOR=36;
  
  public static final int STOP_INTERACTOR=37;
  
  public static final int STATISTICS=38;
  
  public static final int JCALL=39;
  
  // local to Prolog - uses dedicated table
  public static final int GVAR_SET=40;
  
  public static final int GVAR_GET=41;
  
  public static final int GVAR_REMOVE=42;
  
  public static final int RANDOM=43;
  
  public static final int SET_RANDOM_SEED=44;
  
  public static final int SET_PRECISION=45;
  
  public static final int EXISTS_FILE=46;
  
  public static final int FLOOR=47;
  
  public static final int LOG=48;
  
  // TODO
  
  public static final int SIN=49;
  
  public static final int COS=50;
  
  public static final int TAN=51;
  
  public static final int ASIN=52;
  
  public static final int ACOS=53;
  
  public static final int ATAN=54;
  
  public static final int ARITH_CONST=55;
  
  public static final int EXP=56;
  
  public static final int XTEST=57;
  
  public static final int NEW_OPS=58;
  
  public static final int GET_PRI=59;
  
  public static final int GET_PRIS=60;
  
  public static final int OP_SUCC=61;
  
  public static final int OP_PRED=62;
  
  public static final int ALL_OPS=63;
  
  public static final int OP=64;
  
  public static final int TO_NUMBER=65;
  
  public static final int PROTECT=66;
  
  public static final int UNPROTECT=67;
  
  // local to a Prolog uses symbol attributes
  public static final int GET_VAL=68;
  
  public static final int SET_VAL=69;
  
  public static final int REMOVE_VAL=70;
  
  public static final int EXPORT=71; // used as -71
  
  public static final int IMPORT=72; // used as -72
  
  public static final int SERVER=73;
  
  public static final int CONNECTION=74;
  
  public static final int ASK_SERVER=75;
  
  public static final int DISCONNECT=76;
  
  public static final int SHM_PUT=77;
  
  public static final int SHM_GET=78;
  
  public static final int SHM_REMOVE=79;
  
  public static final int SLEEP_MS=80;
  
  public static final int ISERVER=81;
  
  public static final int ICONNECTION=82;
  
  public static final int ASK_ISERVER=83;
  
  public static final int IDISCONNECT=84;
  
  public static final int TO_BUNDLE=85;
  
  public static final int FROM_BUNDLE=86;
  
  public static final int DIRS_OR_FILES=87;
  
  public static final int NEWER_FILE_OF=88;
  
  public static final int ABSOLUTE_FILE=89;
  
  public static final int SAVE_PROLOG=90;
  
  public static final int LOAD_PROLOG=91;
  
  public static final int TERM_TO_FILE=92;
  
  public static final int TERM_FROM_FILE=93;
  
  public static final int FILE_OP=94;
  
  public static final int SYSTEM=95;
  
  public static final int NEW_JAVA_CLASS=96;
  
  public static final int NEW_JAVA_OBJECT=97;
  
  public static final int INVOKE_JAVA_METHOD=98;
  
  public static final int GET_JAVA_FIELD_HANDLE=99;
  
  public static final int HUB=100;
  
  public static final int NEW_LOGIC_THREAD=101;
  
  public static final int COMPARE0=102;
  
  public static final int GET_CMD_LINE_ARGS=103;
  
  public static final int POP_CMD_ARG=104;
  
  public static final int CURRENT_SYMBOLS=105;
  
  public static final int PATH_OP=106;
  
  public static final int PATH_ELEMENTS=107;
  
  public static final int CLEAR_PATH=108;
  
  public static final int GVARS=109;
  
  public static final int SLEEP_NS=110;
  
  public static final int ATOMIC_LIST_CONCAT=111;
  
  public static final int OPEN_LIST=112;
  
  public static final int LIST_ADD=113;
  
  public static final int CLOSE_LIST=114;
  
  public static final int PROFILE_ON_OFF=115;
  
  public static final int SYMTEST=116;
  
  public static final int THREAD_ID=117;
  
  public static final int TO_UPPER_CASE=118;
  
  public static final int TO_LOWER_CASE=119;
  
  public static final int CONCATENATE_FILES=120;
  
  public static final int SUBST_ATOM=121;
  
  public static final int TO_RSTRING=122;
  
  public static final int OBJECT_TO_QUOTED_STRING=123;
  
  public static final int GETENV=124;
  
  public static final int GETENVS=125;
  
  public static final int EXPAND_ENV=126;
  
  public static final int GET_WORKING_DIRECTORY=127;
  
  public static final int SET_WORKING_DIRECTORY=128;
  
  public static final int GET_ENGINE_OBJECT=129;
  
  public static final int IS_ENGINE=130;
  
  public static final int ENGINE_GC=131;
  
  public static final int TO_SET=132;
  
  public static final int FROM_SET=133;
  
  public static final int VARNUMBERS=134;
  
  public static final int MAXDEF=135;
  
}
