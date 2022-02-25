Lean Prolog quick start:

A typical run on any OS:

java -jar lprolog.jar lprolog.jar
?-[nrev].
?-go.
?-halt.

You can customize lprolog.sh or lprolog.bat
to run with various parameters. In these scripts $1, $2, (or %1, %2)
can be any prolog goals passed on the command line. 

Use current_predicate(P) to list available predicates - the ones
overlapping with ISO prolog are likely to have the same semantics.

See the draft papers in this directory for various implementation
decisions and for some of the capabilities.

Directory progs contains examples of programs know to run with current
version of Lean Prolog.

Enjoy,

Paul Tarau
