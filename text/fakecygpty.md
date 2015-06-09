# fakecygpty
It is the redistribution of fakecygpty.

http://www.ysnb.net/meadow/meadow-develop/2005/msg00209.html
http://www.ysnb.net/meadow/meadow-develop/2005/msg00367.html


As noted in the Cygwin archives[1], an NTEmacs build doesn’t know how to
present itself as a cygwin tty device when running cygwin commands.  As a
possible workaround, someone smart has come up with `fakecygpty`[2].  When
compiled from within cygwin according to the instructions, you get a nice
little shim program which does present the expected tty interface.  The
command to run then becomes

  c:\Program Files\Emacs\emacs-24.3\bin>fakecygpty ssh my_server

which works nicely for me!

[1](http://cygwin.com/ml/cygwin/2001-11/msg00631.html)
[2](https://github.com/Shougo/fakecygpty/blob/master/fakecygpty.c)

Visit http://www.emacswiki.org/emacs/SshWithNTEmacs for more info. 

# License

GNU General Public License version2
