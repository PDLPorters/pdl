use Inline::MakeMaker; # to allow us to install the inlined code !

# Note use of 'WriteInlineMakefile' in place of the normal
# 'WriteMakefile' call! Syntax of args is identical to MakeMaker.
WriteInlineMakefile(
    'NAME'	=> 'PDL::MyInlineMod',
    'VERSION_FROM' => 'MyInlineMod.pm', # finds $VERSION
);
