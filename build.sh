RUNNABLE=lprolog.sh
export PATH=".:$PATH"
./lgo.sh
echo "LPJAR=`pwd`/lprolog.jar" > "$RUNNABLE"
cat ltemplate.txt >> "$RUNNABLE"
chmod a+x "$RUNNABLE"
echo copy "$RUNNABLE" somewhare in your PATH
