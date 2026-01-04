# Some random useful code snippets and notes on debugging tools

## To get a disassembly in the cross-environment:

Not actually a diassembly, it's the input to the assembler. Not a round-trip through the assembler & disassembler.

```lisp
(let ((mezzano.compiler::*trace-asm* t))
  (mezzano.compiler::compile-lambda '<lambda-to-disassemble>))
```

## Tracing optimizer transforms

Bind `mezzano.compiler::*report-after-optimize-passes*`

## Loading up the gdb tools (aarch64)

1. Start qemu with the `-s -S` options (one starts the gdb stub, the other starts stopped)
2. Start gdb
3. `target remote :1234`
4. `source tools/gdb.scm`
5. `gu (load-symbols "../mezzano.map")`
6. do other init stuff
7. `c`

Most of this can be done through options when starting gdb

```sh
aarch64-elf-gdb -ex "source tools/gdb.scm" -ex "gu (load-symbols \"../mezzano.map\")" -ex "target remote :1234"
```

### Useful gdb ops

`display /i $pc` - print current pc & instruction every step

print current pc & function & instruction every step (better than above!)
```gdb
define hook-stop
ltrace
end
```

`gu (break "BOOTLOADER-ENTRY-POINT")` - break on a symbol
`gu (where)` - print current function name
`gu (unwind)` - backtrace
`gu (unwind2)` - backtrace, but try to extract current function name from memory instead of the map file

## Building from the repl

```lisp
; from the MBuild/Mezzano directory
(asdf:load-system :lispos)
(asdf:load-system :lispos-file)
(file-server::spawn-file-server)
(cold-generator:set-up-cross-compiler :architecture :arm64)
(cold-generator::make-image "../../mezzano" :image-size (* 5 1024 1024 1024) :header-path "tools/disk-header.bin")
```

Once the system boots up either:
```lisp
(snapshot-and-exit) ; take a snapshot and terminate the basic repl
```
or:
```lisp
(throw 'mezzano.supervisor::terminate-thread nil) ; just terminate the basic repl
```
or nothing and live with the basic repl hanging around, a snapshot is taken at the end of IPL anyway.

## Low-Level DeBugger (LLDB)

lldb (in system/lldb.lisp) is roughly equivalent to gdb in terms of how it works, in the sense
that it operates directly on threads at the assembly level, as opposed to the "normal" style of
Common Lisp debugger that operates within a thread on conditions, signals, and restarts (plus
maybe some other extra stuff, like backtraces, frame inspection, restarting and resumption, etc)

It has a small suite of building blocks for manipulating threads at a low-level. Primarily
stopping threads, single-stepping at the instruction level, and register inspection.

The only high-level tool provided here is `trace-execution`. It takes a function to call,
and steps through it printing every instruction executed. This was originally developed
for profiling CLOS dispatch. Unlike the statistical profiler, this gives an exact trace
of instructions and functions executed.

## Statistical Profiler

Lives in the `mezzano.profiler` package.

Simple operation:
```lisp
(mezzano.profiler:with-profiling (<options...>)
  <forms-to-profile...>)
```

`:thread t` to sample just the current thread or `:thread nil` for all threads.
`mezzano.supervisor::*profile-sample-during-gc*` to sample when the gc is running.

Returns a profile object which can be passed to `mezzano.profiler:save-profile`.

`(mezzano.profiler:save-profile "profile.txt" <profile> :verbosity :flame-graph)`

Drop "profile.txt" in https://www.speedscope.app/ .

## Ansi-Tests

```lisp
(in-package :cl-user)
(load (merge-pathnames "ansi-test/init.lsp" (user-homedir-pathname)) :verbose t :print t)
(rt:disable-note :nil-vectors-are-strings)
(mapc #'rt:rem-test
      '(cl-test::make-array.23 ; Exhausts memory
        cl-test::make-array.28 ; Stack overflows
        cl-test::print.cons.7 ; missing circularity detection
        ;; These all take ages.
        cl-test::find-all-symbols.1
        cl-test::do-all-symbols.1
        cl-test::do-all-symbols.2
        cl-test::do-all-symbols.3
        cl-test::do-all-symbols.4
        cl-test::bignum.float.compare.7
        cl-test::bignum.float.compare.8
        cl-test::rational.double-float.random.compare.1
        cl-test::rational.long-float.random.compare.1
        ))
;; Since we're evaluating lots, better to avoid the compiler
(setf mezzano.internals::*eval-hook* 'mezzano.full-eval:eval-in-lexenv)
(time (regression-test:do-tests))
```
