# A Guide to Benchmarking with PDL

## Tasks

This is a collection of benchmarking tasks collected from comparison sites,
such as [plb2](https://github.com/attractivechaos/plb2),
[FPBench](https://github.com/FPBench/FPBench) and
the [Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html).
Yes, we want PDL to be the fastest dog in the race, but more importantly it lets
our developers know when their latest commit has degraded performance.

### Matrix multiplication

From [plb2](https://github.com/attractivechaos/plb2/tree/master/src/perl), it creates
two square matrices and multiplies them together (the inner product, `x`).

This PDL script is more than 65 times faster than their Perl script.
I've tried a few variations to find a faster version, but most gains are within
the timing variation.
