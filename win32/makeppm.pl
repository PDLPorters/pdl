# PSEUDO Code

# 1
# make sure 'nmake doctest' was run

# 2
# move blib/lib/PDL/HtmlDocs/PDL to blib/html/lib/PDL
# copy pbmplus exes into blib/script

# 3
# make sure PDL.ppd was made
# patch it to include ref to the postint script
#   *and* get the reference to the tar.gz right

# 4
# tar blib *and* the install script (install.ppm) into PDL-ver.tar.gz

# 5
# zip PDL.ppd PDL-ver.tar.gz Readme into pdl-ver-build.zip

