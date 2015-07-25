From http://www.gnu.org/software/coreutils/manual/html_node/cat-invocation.html

#cat

cat copies each file (‘-’ means standard input), or standard input if none are given, to standard output. Synopsis:

`cat [option] [file]…`

The program accepts the following options.

**‘-A’** **‘--show-all’**

  Equivalent to -vET.&nbsp;

**‘-b’** **‘--number-nonblank’**

  Number all nonempty output lines, starting with 1.&nbsp; 
  
**‘-e’**

  Equivalent to -vE.&nbsp;

**‘-E’** **‘--show-ends’**

  Display a ‘$’ after the end of each line.&nbsp;

**‘-n’** **‘--number’**

  Number all output lines, starting with 1. This option is ignored if -b is in effect.&nbsp;

**‘-s’** **‘--squeeze-blank’**

  Suppress repeated adjacent empty lines; output just one empty line instead of several.&nbsp;

**‘-t’**

  Equivalent to -vT.&nbsp;

**‘-T’** **‘--show-tabs’**

  Display TAB characters as ‘^I’.&nbsp;

**‘-u’**

  Ignored; for POSIX compatibility.&nbsp;

**‘-v’** **‘--show-nonprinting’**

  Display control characters except for LFD and TAB using ‘^’ notation and precede characters that have the high bit set with ‘M-’.
