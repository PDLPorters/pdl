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

Initial measurement for PDL 2.095
```
Benchmark 1: ./matrix_multiplication.pl
  Time (mean ± σ):      3.569 s ±  0.063 s    [User: 3.656 s, System: 0.088 s]
  Range (min … max):    3.521 s …  3.710 s    10 runs
```

## Benchmarking

You can run a comparison with the Perl script using [hyperfine](https://github.com/sharkdp/hyperfine)

```
hyperfine --warmup 1 'YOUR_ENV_VAR=1 path/to/matmul.pl 300' 'path/to/matrix_multiplication.pl 300'
```

I recommend low values at first because, using the default (N=1500),
hyperfine takes over 10 minutes to measure the Perl script because it's default
is to run each program 10 times (changed with the **[mMr]** options).
Ideally, you want to run this on a quiet system with few other processes running.

Other benchmarks in pure Perl can be found at [plb2](https://github.com/attractivechaos/plb2/tree/master/src/perl),

### Strategies

If you want to compare two different branches against each other,
consider using something like `--setup 'git checkout HEAD^'` or perhaps
just running hyperfine with the single benchmark while searching for
the offending commit with `git bisect`. Let us know how _you_ do benchmarking.

## Profiling

Use [Devel::NYTProf](https://metacpan.org/pod/Devel::NYTProf) to find out where
your script is spending its time and how many times a line is run. Running the
profiler really slows down your code, so I will run both commands on the same line
and come back later.
```
perl -d:NYTProf matrix_multiplication.pl 500; nytprofhtml --no-flame
```

* [profiling](https://github.com/PDLPorters/pdl/issues/451)
* [perl v java](https://charlesreid1.github.io/perl-vs-java-n-queens-problem.html)
