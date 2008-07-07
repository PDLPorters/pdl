# HG: remove *roff special syntax from manpage's NAME section
/.SH \"NAME\"/,+1 {
s/\\f(CW\\\*(C`/"/g
s/\\\*(C'\\fR/"/g
}
