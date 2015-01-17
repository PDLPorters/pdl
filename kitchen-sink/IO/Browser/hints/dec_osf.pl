# Achim Bohnet <ach@mpe.mpg.de>:
#
# /usr/include/curses.h claims that derwin() is part of:
#
#      #ifdef _XOPEN_SOURCE_EXTENDED
#
#      /* These are the ENHANCED CURSES interfaces in X/Open Curses, Issue 4 */
#

$self->{'DEFINE'} .= ' -D_XOPEN_SOURCE_EXTENDED -D_POSIX_C_SOURCE=199506L';
