print "This Tk interface has been replaced, the new Tk demo is in
Demos/TkTriD_demo.pm which can be run from the perldl prompt:
perldl> demo Tk3D\n";

print "\nHit <Enter> now to go to the Demo, any other key to exit ";
my $key = <STDIN>;
chomp($key);
exit if($key);
exec("perldl 	<<EOF
use blib
demo Tk3d
EOF");	
1;
