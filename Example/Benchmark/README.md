# Usage

```
perl Makefile.PL
make
perl -Mblib time.pl
```

You can edit `Bench.pm` for different parameters / running times if
your machine is very slow or very fast.

Currently only compares XS/C "++" with a PDL one.
