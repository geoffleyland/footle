This started out as an attempt to learn rust, and at the same time I thought I'd try to learn
something else I knew nothing about, but sounded fun: writing a parser.  Every now and then I work
on it for a month or so, and then it gets lost for a year, but it comes back as an itch I have to
scratch.

A loooong time later, and it's not got very far (there's no control flow), but it does compile
very simple programs to AArch64.

It has some features:
 * It doesn't use many libraries.  The results would be better and my life would be easier if I
   used the excellent parsing, compiling and many other libraries that are available, but this is
   a learning exercise.  If you look, you'll see it's *obviously* not an implementing exercise.
 * As much as it can, it eats its own dogfood - it can write intermediate representations (up to a
   point) out as valid footle code, and the test suite re-compiles that output and checks it ends
   up the same.  It can even write out the AArch64 it thinks it's creating, and check that against
   an independent disassembly of what it generated.  Eating its own dogfood puts some constraints
   on the language (there have to be multiple returns from blocks) and if I ever get to loops...
   that'll be tricky.
 * It has pretty good error messages: not as good as rust, but it doesn't stop on the first error
   and it shows you the source causing the error.  This came about because I read several excellent
   tutorials on parsers and interpreters and learned a lot, but there were always a few gaps.
   Nice error handling was one of them, so I tried to figure it out.
 * It does some optimisation - constant propagation, limited common sub-expression elimination and
   dead code elimination.  Again, because it wasn't covered in the tutorials I read, I gave it a
   go.
 * It's got a register allocator and instruction scheduler - it knows about register pressure and
   instruction latency and tries to schedule stuff to keep the critical path short and a small
   working register set.
 * It can compile in calls to (two) library functions (`pow` and `sin`)
 * And, if your program is as simple as, say, a + 2 or a^2.5 or even sin(a), it can compile it to
   AArch64!

 It has some big holes:
  * No control flow is pretty significant
  * Only one type (floats), but I think adding bool (and the whole type system for two types) is
    next on the list - any time this decade!

And, if I were to dream, the list of things I'll probably never get to implementing include:
 * An "interpreter" mode that runs interpreted control flow over compiled
   basic blocks.  In a sense, I've achieved that already, because it compiles but doesn't allow
   control flow.
 * A static language dressed up as a dynamic one: types where you want them, and inferred
   everywhere else.
 * Decent static checking.  I find it hard when my dynamic language runs a model for six hours and
   then dies in a type error I made in the output writing.
 * And one day a tracing JIT that runs the same compiler to compile in control flow over multiple
   blocks (I'm a fan of LuaJIT)
 * Multiple dispatch (I'm a fan of Julia)
