# Useless Performance Optimisations on the BEAM

This repo contains tools and code examples used in my #CodeBEAMSTO '19
talk, exploring the possibilities of optimisations on the BEAM VM's
assembly language layer. (If you're interested, the slides are
available
[here](https://docs.google.com/presentation/d/1E8aFUJTBu7MvBNEEhsJasmGry0XP7Zve0TaNBK9f-1k/edit?usp=sharing).)

So what exactly can you find in this repo? Here's a short inventory of
interesting things:

* [talk_snippets.hrl](talk_snippets.hrl): all the code snippets shown
  on the slides in one `.hrl` file. Don't try to include it in an
  actual module, it won't compile. It's only purpose is to make it
  possible to copy-paste syntax-highlighted source code from there to
  the presentation.
* [bapp/](bapp): source code for my _Beam Assembly PreProcessor_
  script. Writing BEAM assembly by hand is hard, because it's mostly a
  machine-interface. I got frustrated enough to hack together this
  preprocessor that compiled `.bapp` files to `.S` files. All of my
  assembly programs are actually written in `.bapp` format.
* [test/](test): a small framework for performance testing different
  implementations of test cases across a range of different
  arguments. The test cases themselves are also defined in this
  directory.
* [asm/](asm): assembly implementations of the test cases.
* [src/](src): Erlang implementations of the test cases.
* [otp/](otp): a directory containing some modules borrowed from
  [Erlang/OTP](https://github.com/erlang/otp/tree/OTP-22.0-rc1) (OTP
  22.0 rc1, precisely). I compile these modules to assembly and
  generate some statistics on opcode usage from them.

Please keep in mind that the code in this repo is not production
quality. The only guarantee is that it Works On My Machine(tm), your
mileage may vary. A lot of things were quickly hacked together until
they became good enough for my needs, and then I moved backed to
writing slides and practising presenting them. Sorry for all the mess!

## How to build?

Prerequisites:

* You need to have the OTP versions you want to use for building and
  testing the code installed via
  [`kerl`](https://github.com/kerl/kerl).
* The tests want to lock the CPU frequency to a constant value, and
  you need (passwordless) `sudo` and the `cpufrequtils` package for
  that.
* The build and test shell scripts assume you are using Linux. I
  *guess* relying on `cpufrequtils` immediately disqualifies MacOS and
  WSL. And even if you'd remove that, I still have this bad habit of
  accidentally using tools or arguments supported by GNU programs
  only, causing issues on MacOS.

With all that said, you first shall create a `local.mk` file and
define some parameters there that are specific to your environment:

```make
KERL     := ~/bin/kerl            # Location of kerl if it's not in your $PATH
OTP_VSNS := OTP-22.0-rc1 OTP-22.0 # OTP versions to use for build & test
                                  # (defaults to all versions found in kerl)
CPU_FREQ := 2.6GHz                # CPU speed to lock while testing
```

Then just do `make`, and hope for the best!

All build artefacts will go under the `build/` directory, into
separate subdirectories by OTP version (such as `build/OTP-22.0/`).

## BAPP

The goal of BAPP is to make writing BEAM assembly a bit easier. What
it gives you:

* No need to write `module_info/0,1` boilerplate. Nor module name
  attribute (the file name defines the module name instead).
* Symbolic labels (also, auto-converting labels to either `label` or
  `f` tuples based on context).
* Auto-wrapping Erlang terms into appropriate `atom`, `integer`
  etc. tuples.
* Shorter syntax for register names: `x0`, `y123` and similar names
  mean registers, not atoms (you can use the `'x0'` syntax if you want
  to refer to the atom instead).
* Shorter syntax for test statements: just add a `?` in front instead
  of the `test` tuple.
* (Mostly) white-space delimited syntax, whit less commas and dots.
* Easy function syntax, that takes care of inserting `func_info` and
  entry-point `label` ops for you automatically:

```erlang
-exportes([hi/1]).

hi/1 ->
  ?is_eq_exact @its_a_bapp x0 bapp
  move x0 false
  return

  @its_a_bapp
  move x0 true
  return
.
```

What you don't get is any guarantee that the tool would actually work
for opcodes not used in the `.bapp` files of this repo. To properly
compile plain Erlang terms into properly tagged opcode arguments you
need to know what type of arguments the opcode is supposed to work
on. There's information about this in `erlc`, but BAPP doesn't use
that information, it just uses some heuristics that _60% of the time
work all the time_. For the rest of the times there may be some ugly
hacks in place. Or not, no guarantees.

## Test cases

A test case is defined by a module in the `test` directory that
follows the naming convention `<TEST_CASE_NAME>_test`. These modules
also implement the `perftest` behaviour that allows the test cases to
define different parameters for the test (typically for controlling
the complexity of the problem), perform set up and clean up duties and
so on.

Any module found in the code path that follows the naming convention
`<TEST_CASE_NAME>_<IMPLEMENTATION>` is considered to be an
implementation of the corresponding test case. The implementations
have to provide a `run` function that the `perftest` module will call
(the number of arguments is defined by the test case).

The tests need to measure the effects of changing a handful of
assembly instructions only, which would typically make only a few
nanoseconds difference. At this time scale test execution times can be
quite noisy, especially when working on top of a big, complex VM such
as the BEAM. The following measures are maid to minimise the noise:

* The CPU cores are locked to run at a fixed frequency.
* Scheduler threads are pinned to CPU cores.
* The test process is pinned to the last scheduler.
* Before spawning the test process, all but one schedulers are taken
  offline for a brief time. This forces processes away from the test
  process' scheduler, and hopefully it will remain mostly free for the
  time of the tests too.
* The test process runs with maximum priority.
* Immediately before starting the tests the test process yields to
  start with predictively low reduction count.
* Execution time is measured with `os:perf_counter/0`.
* There are 100 warm-up rounds and 1000 actual test runs. Median,
  average and standard distribution of these 1000 runs are calculated.
