# AGENTS.md — Perl Data Language (PDL)

Canonical developer documentation lives in `lib/PDL/DeveloperGuide.pod`
(`PDL::DeveloperGuide`) and `lib/PDL/PP.pod` (`PDL::PP`). Keep this file
concise and aligned with `PDL::DeveloperGuide` as the single source of truth.

## Build & Test Commands

```sh
perl Makefile.PL
make -j$(nproc)
make && prove -bv t/core.t  # single test against blib/
make -j$(nproc) test        # full suite against blib/
perl -Mblib -MPDL -E 'say $PDL::VERSION'
```

- Always use `-Mblib`, never bare `-Iblib/lib`.
- Update `MANIFEST` whenever adding or removing tracked files.

## Core Invariants & Pitfalls

- **Column-first indexing, row-major storage:** Dimension 0 (`X` / column) is
  contiguous in memory. Transpose (`->transpose` / `->t` / `->xchg(0,1)`) when
  interfacing with column-major Fortran/OpenGL APIs.
- **Explicit slicing in library code:** Use `$pdl->slice(...)` in modules;
  reserve `PDL::NiceSlice` for interactive shells/scripts.
- **Zero-copy slicing & dataflow:** Slices share memory and propagate changes
  back to parents unless detached with `->sever()`.
- **Design for complex, bad values, and broadcasting:** Use `PDL_IF_BAD` /
  `HandleBad => 1` and consider complex types and broadcasting from the start.
- **Tests:** Use `Test::More` + `Test::PDL` (`is_pdl($got, $expected)`).
- **Debugging:** Use `PDL::Core::set_debugging(1)` (`PDLDEBUG_f`),
  `PDL::Core::pdump($pdl)` + `utils/address-pseudonymise`, and
  `$::PP_VERBOSE = 1` around `pp_def`.

## Satellite Distribution Conventions

- Hard-fail (`die`) in `Makefile.PL` on missing native dependencies (never
  `exit 0`); preserve quotes in `PDL::Core::Dev::PDL_INCLUDE()`.
- License under `perl_5` ("the same terms as Perl itself") with a top-level
  `LICENSE` file.
  For library bindings, consider documenting supported types with `=head1 TYPES` POD.
- Use `PDLPorters/devops` composite GitHub Actions (`ci-dist`,
  `install-dep-<lib>`) for CI.

See `CLAUDE.md` and `lib/PDL/DeveloperGuide.pod` for further details.
