# Cobra

![A Cobra](https://upload.wikimedia.org/wikipedia/commons/thumb/9/94/Indian_Cobra.JPG/1920px-Indian_Cobra.JPG)

In this assignment, you'll implement a compiler for a small language with
functions declarations and function calls.
You'll also add some more interesting static well-formedness checks to the compiler.

Of course, the **Cobra** language implements the **C**alling **o**f **B**asic Sub**R**outines with **A**rithmetic.

## The Cobra Language

As usual, we have concrete and abstract syntaxes, along with a specification
of semantics.

### Concrete Syntax

The major addition to Cobra are _function declarations_.  Our programs
are now a sequence of zero or more function declarations, followed by a single
_main expression_.

```
<program> :=
  | <decls> <expr>
  | <expr>

<decls> :=
  | <decl>
  | <decl> <decls>

<decl> :=
  | def <identifier>(<ids>): <expr>
  | def <identifier>(): <expr>

<ids> :=
  | <identifier>
  | <identifier> , <ids>

<expr> :=
  | let <bindings> in <expr>
  | if <expr>: <expr> else: <expr>
  | <binop-expr>

<binop-expr> :=
  | <identifier>
  | <number>
  | true
  | false
  | add1(<expr>)
  | sub1(<expr>)
  | isnum(<expr>)
  | isbool(<expr>)
  | print(<expr>)
  | <identifier>(<exprs>)
  | <identifier>()
  | <expr> + <expr>
  | <expr> - <expr>
  | <expr> * <expr>
  | <expr> < <expr>
  | <expr> > <expr>
  | <expr> == <expr>
  | ( <expr> )

<exprs> :=
  | <expr>
  | <expr> , <exprs>

<bindings> :=
  | <identifier> = <expr>
  | <identifier> = <expr>, <bindings>
```

The second addition is _function applications_, which are written
`<identifier>(<exprs>)`, for example `f(1, 2, 3)`.  This is the syntax for a
_call_ to a function.

Finally, we have added a `print` operation. Calling `print(e)` displays the value
of `e` to the user and then returns the value of `e`. For example `print(1+1)` prints `2` and then _returns_ `2`.  The expression `e` should only be evaluated once, so, for example, this program:

```
print(print(1))
```

Should print `1` exactly twice due to the `print` command, and then once at the end of the program (since the answer is `1`).

### Abstract Syntax

As usual, we have a user-facing syntax and a compiler-facing syntax.  In this
assignment, these definitions have been split into the separate `expr.ml`
file.

```
type prim1 =
  | Add1
  | Sub1
  | Print
  | IsNum
  | IsBool

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal

type expr =
  | ELet of (string * expr) list * expr
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EApp of string * expr list
  | EIf of expr * expr * expr
  | ENumber of int
  | EBool of bool
  | EId of string

type decl =
  | DFun of string * string list * expr

type program =
  | Program of decl list * expr
```

The `EApp` constructor corresponds to function applications, and
the `decl` type respresents function declarations.  A program is a
list of `decls` with a main expression.

### Semantics

There are several distinguishing features of Cobra.  The first is
function applications.  A function application should give the answer we'd get
if we followed the rules for substituting argument values for parameter names.
So, for example:

```
def f(x, y):
  x + y

f(1, 2)
```

Should produce 3.

Your compiler should use the calling convention discussed in lecture, shown
on [these slides](https://github.com/ucsd-cse131-sp17/lectures/blob/master/lecture11/calling.pdf),
to implement this behavior.

There are a number of new _errors_ that can occur now that we have function
declarations and calls.  Your implementation should catch all of these cases
_statically_; that is, before the program runs, in `well_formed_e`:

- A function application with the wrong number of arguments should signal an
  error containing the string "Arity"
- A function application of a non-existent function should signal an error
  containing the string "No such function"
- A function declaration with duplicate names in the argument list should
  report an error containg the string "Duplicate parameter"
- If there are multiple function definitions with the same name, report an
  error containing the string "Duplicate function"

Again, these errors should stop the program from compiling, _not_ happen at
runtime.  See the notes
on `well_formed` below for implementation details.

### Implementation

We have added one new `instruction` called `IComment`, which is useful
for generating assembly files with comments. Thus, the list of instructions

~~~{.ocaml}
[
  IComment ("Store 0 in EAX");
  IMov (Reg(EAX), Const(0))
]
~~~~

corresponds the following assembly:

```
;; Store 0 in EAX
mov eax, 0
```

We have also added a new `arg`, `Label`:

~~~{.ocaml}
[
  IMov (RegOffset(-4, ESP), Label("goto_is_not_harmful"))
  ILabel("goto_is_not_harmful");
  IMov (Reg(EAX), Const(0))
]
~~~

corresponds to (the very silly):

```
mov [esp - 4], goto_is_not_harmful
goto_is_not_harmful:
mov eax, 0
```

You're free to add your own new instructions, though you do not need to
in order to complete the assignment.

### Testing

Add your own tests to the list `myTestList` in the file **`myTests.ml`**. You can use `t` to test
that a program evaluates to a given value and `terr` to test that a program
throws an error containing the given error message.

As usual, we will be evaluating your tests by checking to see how many
bugs your tests uncover in our buggy compilers.

### TODO

In all, you will need to implement the following in **`compile.ml`**:

- The `Prim1` case for `Print`, which should call the C `print` function
  in `main.c`. This call should obey the `C` calling convention, rather than the `Cobra` calling convention.

- `compile_decl`, which compiles a function _declaration_.

- `compile_app`, which compiles function applications (that is, expressions of the form `EApp(f,es)`).

- The `EApp` case of `well_formed_e`, which checks the well-formedness of function applications.

- `well_formed_d`, which checks the well-formedness of function declarations.

### FAQ:

**Why do `well_formed_e` and `well_formed_d` take a list of declarations as an argument?**

Think of this as similar to the environment, but for function declarations.  You can use these
to keep track of what function definitions are available to, for example, report that an `EApp` expression
is using a function that doesn't exist, or using it with the wrong number of arguments.

**Do I have to pass arguments in a particular order?  As in, does the first argument have to be the first one “pushed”?**

You can pick either order, as long as it is consistent and works between the caller and callee.

**How do we generalize from a single argument to multiple?**

In lecture, we introduced the calling convention with single-argument functions.
In this assignment you need to implement this for a list of arguments.

There's a few points to keep track of here:

- At the caller, the generated code needs to store each argument on the stack in the order the callee expects.  This might require using intermediate storage space
- At the callee, the starting stack index and environment will be based on the number of arguments

### Recommended Ways to Start

1.  First, implement (and test!) calls with a single argument.  The code from lecture slides can be helpful here, because the idea of putting 3 words on the stack for the argument, return pointer, and old stack address will translate most directly
2.  Second, implement (and test!) `print`.  This will give you more practice with understanding moving `ESP` around appropriately.
3.  Third, implement (and test!) the new static errors.  This will give you practice with the list parts of the new declaration and `EApp` datatypes.
4.  Fourth, implement (and test!) multi-argument functions.  This will use the practice from the last three steps to generalize single-argument functions to putting multiple values on the stack and using them in the callee.
5.  Fifth, write some complicated tests that use recursion (you should be able to get factorial working, for example), to stress-test your implementation
