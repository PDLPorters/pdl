# CLAUDE.md — Perl Data Language (PDL)

## Project Scope
Scientific numeric computation engine utilizing the Perl Data Language (PDL) framework.

## Core Design Paradigms
* **Column-First Indexing, Row-Major Storage:** Accessing multidimensional fields maps coordinate variables as `(X, Y, Z)` or `(column, row, plane)`. While memory allocation keeps data row-major (C-contiguous), the 0th dimension ($X$) is the fastest-moving index in memory.
  * *For NumPy users:* Unlike NumPy's `(row, col)` tracking where the last index is contiguous, PDL uses `(col, row)` tracking where the first index is contiguous.
  * *For MATLAB users:* PDL shares the `(col, row)` rapid-variation syntax order but uses a physical C-style row layout instead of Fortran column layout.
* **Zero-Copy Memory Slicing:** Subsets of multi-dimensional `ndarrays` generated via `slice`, `dice`, `mv`, or `xchg` are virtual views that share memory rather than copying it.
* **Dataflow Automation:** Modifications made to a sliced child view automatically propagate backward to update the parent array unless explicitly detached.
* **Native C Broadcasting:** Element-by-element loops scale across high dimensions inside optimized, multi-threaded C processing blocks using PDL broadcasting logic, omitting scalar Perl loops.
* **Auto-Parallelisation via POSIX Threads:** Large calculations automatically split processing across hardware cores via POSIX threads if target ndarrays exceed the active execution threshold. Control the worker environment dynamically via configuration settings:
  * `set_autopthread_targ($count)` sets the maximum concurrent thread execution ceiling (uses all available system CPU cores if the environment variable `PDL_AUTOPTHREAD_TARG` is absent).
  * `set_autopthread_size($size_in_mpe)` sets the baseline parallelisation floor. **Note:** The metric threshold is parsed explicitly in Meg-elements ($2^{20}$ or $1,048,576$ elements). Setting this threshold to 0 removes the floor, forcing parallel execution across all operations regardless of data size.

## Broadcasting Rules
PDL auto-vectorizes dimensions via its broadcasting engine based on the function's native `PDL::PP` signature:
1. **Active vs. Broadcast Dimensions:** The operator's signature defines its "active" target dimensions (e.g., matrix operations consume the first two dimensions). Higher dimensions are treated as implicit broadcast loops managed by the C engine.
2. **Dimension Promoting (Rank Alignment):** If dimension counts mismatch, missing higher dimensions are implicitly appended with a size of 1 until rank quantities align.
3. **Dimension Inflating (Shape Alignment):** Mismatched dimensions pass validation if one operand's dimension size is exactly 1. Dimensions of size 1 are implicitly inflated (cloned/repeated) to match the larger operand's shape footprint without memory duplication.
* Shifting target vectors to dimension 0 via `xchg`, `mv`, or `reorder` isolates them ahead of the broadcast dimension boundary.

## Coding Standards & Project Layout
* **Explicit Slicing for Library Code:** In shared module architectures and internal library design, always use the explicit `$pdl->slice(...)` method over the source-filtered `PDL::NiceSlice` engine syntax. Use NiceSlice syntax exclusively for interactive shell sessions or standalone prototyping scripts.
* **The Modern Module Layout:** New extension features or standalone modules must use the modern layout auto-generated via `pptemplate Modulename`. This creates standard scaffolding with compiled C code bindings driven out of a structured `lib/YourModule.pd` script layout.
* **Prototyping Extensions:** For rapid prototyping, exploration, or short script-internal low-level extensions, declare your compiled routines inline directly via [Inline::Pdlpp](https://metacpan.org/pod/Inline::Pdlpp) blocks. Do not manage raw C file bookkeeping manually.
* **The Interop Trap:** Because PDL stores data C-style but indices column-first, arrays must be transposed via `->transpose` or `->xchg(0,1)` before being fed to external column-major engines like Fortran LAPACK or OpenGL bindings.
* **Explicit Mutation:** Use computed assignments (`.=`) to force data vectorization and write back changes through array slices.
* **Severing Dataflow:** Call `->sever()` explicitly when you want to break dataflow tracking between a child view and its parent structure, protecting parent arrays from side effects.
* **Avoid Nested Iteration:** Never wrap raw ndarrays inside Perl `for` or `while` blocks to inspect individual elements. Keep operations vectorized.

## Bad Value Handling (PDL::Bad)
* Set `$pdl->badflag(1)` on data segments containing invalid/missing measurements to trigger core C conditional branching.
* Keep `badflag(0)` on clean data paths to avoid element-by-element branch performance penalties.

## Ecosystem Extensions
* **Linear Algebra & Transforms:** Use `PDL::LinearAlgebra` (LAPACK) and `PDL::FFTW3` instead of writing custom mathematical routines.
* **Visualisation:** Use `PDL::Graphics::Simple` for 2D diagnostics and `PDL::Graphics::TriD` for OpenGL-accelerated 3D rendering.

## Routine Verification Commands & Testing
* **Interactive Exploration Shell:** Launch the baseline interactive environment via `perldl`. To run the advanced, modern interactive shell utility `pdl2`, ensure you install [PDL::Perldl2](https://metacpan.org/pod/PDL::Perldl2) explicitly first.
* Execute test scripts with PDL headers active: `perl -MPDL path/to/script.pl`
* Run full project test suites via the standard harness: `prove -lv t/`

### Test Suite Standards
Always use [Test::More](https://metacpan.org/pod/Test::More) coupled with [Test::PDL](https://metacpan.org/pod/Test::PDL) for verifying array computations.
* **The `is_pdl` Predicate:** Do not use raw scalar comparisons or loop extractions for multi-dimensional arrays. Use `is_pdl($got, $expected)` to validate type, dimensions, bad values, and content.
* **Approximate Matching:** For floating-point operations where minor calculation variances manifest, configure absolute tolerance constraints globally or via compilation triggers (e.g., `use Test::PDL -atol => 0.01;`) to set floating-point tolerance margins.
