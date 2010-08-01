use PDL::Config;

do {
print "\nWARNING:
  The TriD Tk interface is has been deprecated
  and is not available for this PDL build.  If
  you desire Tk support, please contact the PDL
  developrs.  We're investigating more portable
  and supportable GUI options to Tk.\n";
exit; } if ($PDL::Config{POGL_WINDOW_TYPE} eq 'glut');

print "This Tk interface has been replaced, the new Tk demo is in
Demos/TkTriD_demo.pm which can be run from the perldl prompt:
pdl> demo Tk3D\n";

print "\nHit <Enter> now to go to the Demo, any other key to exit ";
my $key = <STDIN>;
chomp($key);
exit if($key);
exec("perldl 	<<EOF
use blib
demo Tk3d
EOF");	
1;
