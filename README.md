# LeanProlog is a lightweight but efficient Java-based Prolog system.

### It is designed to have most Prolog functionality expressed at source level with good performance coming from its optimized (Bin)WAM-based runtime system.

A typical run on any OS:

```
cd bin
java -jar ../lprolog.jar ../lprolog.jar
?-[nrev].
?-go.
?-halt.
```

On a Mac OS X or Ubuntu, the Java version  self-compiles and builds with 

```build.sh```, 

creating ```lprolog.jar``` and a shell to launch it to be put in your PATH

```lprolog.sh```

The same should work on any Unix/Linux but (but may need small adaptations depending on command shells for other systems).

### Convention for third party extensions: 

add them to lib, then customize
the script "to_xjar" to embed them into prolog.jar

Some features:

- reentrant coroutining engines
- threads

See directory ```bin/progs``` for some typical examples that it can run.
