#!/usr/bin/perl
#
# This tests the new PDL constructor with a string argument.
# There are two goals from the new functionality: (1) allow
# MATLAB to use familiar syntax to create arrays, and
# (2) to allow cut-n-paste of PDL print output as input
# for scripts and programs
#

use Test::More skip_all => 'pdl string constructor tests not implemented';

# Basic 2D array
# pdl> p $a = pdl q[ [ 1, 2, 3 ], [ 4, 5, 6 ] ];
# pdl> p $a = pdl q[ 1 2 3 ; 4 5 6 ]
# pdl> p $a = pdl '[ [ 1, 2, 3 ], [ 4, 5, 6 ] ]';
# 
# [
#  [1 2 3]
#  [4 5 6]
# ]

# Basic 1D array
# pdl> p $b = pdl [ 1, 2, 3, 4, 5, 6 ]
# pdl> p $b = pdl q[ 1 2 3 4 5 6 ]
# pdl> p $b = pdl q[1,2,3,4,5,6]
# [1 2 3 4 5 6] 

# 1D array with signs
# pdl> p $c = pdl [ 7, -2, +5 ]
# pdl> p $c = pdl q[ 7 -2 +5 ]
# pdl> p $c = pdl q[ 7, -2, +5 ]
# [7 -2 5] 

# 1D array with mixed ops and signs
# pdl> p $d = pdl [ 7 - 2, +5 ]
# pdl> p $d = pdl q[ 7 - 2 +5 ]
# [5 5]

# ...another
# pdl> p $d = pdl [ 7, -2 + 5 ]
# pdl> p $d = pdl q[ 7 -2 + 5 ]
# [7 3]

# 1D array with ops, not signs
# pdl> p $d = pdl [ 7 - 2 + 5 ]
# pdl> p $d = pdl q[ 7 - 2 + 5 ]
# 10

# A [2,3,4] shape piddle
# pdl> p $d = pdl [ [ [0, 1], [4, 0], [0, 3] ],
#                   [ [2, 0], [4, 0], [4, 1] ],
#                   [ [0, 1], [3, 2], [1, 4] ],
#                   [ [1, 2], [2, 2], [2, 1] ] ];
# 
# [
#  [
#   [0 1]
#   [4 0]
#   [0 3]
#  ]
#  [
#   [2 0]
#   [4 0]
#   [4 1]
#  ]
#  [
#   [0 1]
#   [3 2]
#   [1 4]
#  ]
#  [
#   [1 2]
#   [2 2]
#   [2 1]
#  ]
# ]
# 
# ...the same, just different formatting...
#
# [
#  [ [0 1] [4 0] [0 3] ]
#  [ [2 0] [4 0] [4 1] ]
#  [ [0 1] [3 2] [1 4] ]
#  [ [1 2] [2 2] [2 1] ]
# ]

# A 3x3 2D array
# pdl> p pdl [ [1, 2, 3], [2, 1, 0], [2, 2, 1] ];
# pdl> p $e = pdl q[ [ 1 2 3 ] ; [ 2 1 0 ] ; [ 2 2 1 ] ]; 
# pdl> p pdl q[  1 2 3 ; 2 1 0 ; 2 2 1 ]  # this should be the same
#
# [
#  [1 2 3]
#  [2 1 0]
#  [2 2 1]
# ]
