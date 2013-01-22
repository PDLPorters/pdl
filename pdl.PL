if($^O =~ /mswin32/i) {exit}
open(FOO,">pdl.c");
print FOO <<'nosubs';

/******************************
 * pdl.c - perldl spawner
 * Works around a problem with many unices that you can't use an interpreter
 * to run an interpreter -- so "#!/usr/bin/perldl" won't work.
 * This is a compiled piece of code that launches perldl "directly", 
 * so that the poor kernel's mind isn't blown.
 *
 * If you feed in a single non-switch argument it gets prepended with a 
 * "-" to let perldl know that it's an input file.  That way you can be lazy
 * and say "#!/usr/bin/pdl" at the top of your script.
 * 
 * Don't modify this .c code -- modify the generator, pdl.PL, instead.
 *
 * CED 21-Jul-2004
 */


#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(int argc, char **argv) {
  char perldl[BUFSIZ];
  int pipes[2];
  int pid,i;
  int status;

  if(pipe(pipes)) {perror("pdl (perldl spawn wrapper)"); exit(1);}
  pid = fork();
  if(pid==0) {
    dup2(pipes[1],1);
    dup2(pipes[1],2);
    system("which perldl");
    exit(0);
  }
  read(pipes[0],perldl,BUFSIZ);
  pid = wait(&status);
  if(! WIFEXITED(status) ) {
    fprintf(stderr,"Hmmm... couldn't seem to find perldl anywhere. Quitting.\n");
    goto exit;
  }

  /* Remove trailing newline */
  for(i=0;i<BUFSIZ && perldl[i]; i++) 
    if(perldl[i]=='\n' || perldl[i]=='\r')
        perldl[i]='\0';

  if(argc==2) {
    if(argv[1][0]!='-') {
      char **argv2 = malloc((argc+2)*sizeof(char *));
      int i;

      if(!argv2) 
	goto exit;

      for(i=0;i<argc;i++)
	argv2[i+1]=argv[i];

      argv2[1]="-";
      argv2[0]="perldl";
      argv2[argc+1]=0;

      execv(perldl,argv2);
      fprintf(stderr,"couldn't execv %s\n",perldl);
      goto exit;
    }
  }

  argv[0]="perldl";
  execv(perldl,argv);
  fprintf(stderr,"couldn't execv %s (%d args)\n",perldl,argc);
  goto exit;

exit: 
  perror("pdl (perldl trampoline)");
  exit(-1);
}
nosubs
  ;

use Config;
my $dir = 'blib/bin';
#manually create directory. Older versions of EU::MM (at least as late
#as 6.17 which ships with perl 5.8.6) don't automatically create it.
mkdir $dir unless -e $dir && -d $dir;
`$Config{cc} -o $dir/pdl pdl.c`;

exit 0;
