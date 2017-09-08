# est
*est* is a simple vector calculator, much simpler than AWK or R.

``` console
$ cat sample.dat
1	2	3
4	5	6
7	8	9
$ est '$0 + 2 * $1' sample.dat
5
14
23
$ est 'sum $0' sample.dat
12
```

## How to build
### Requirements
* OCaml compiler

For macOS:

``` shell
brew install ocaml opam
```

(Opam is a package manager of OCaml, which is recommended.)

Build tools and dependencies can be installed with opam:

* ocamlbuild
* ocamlfind
* menhir
* core
* re2

``` shell
opam install ocamlbuild ocamlfind menhir core re2
```

*TODO: version?*

### Build
``` shell
make
```

An executable will be created at `_build/src/main.native`.

## Usage
### Synopsis
```
est PROGRAM [FILES ...]
```

For program's syntax and language features, see [Language](#language).

If `-` is given for a file, it will read data from the standard input.

### Flags
| name                    | aliases                 | description                          |
| ----------------------- | ----------------------- | ------------------------------------ |
| `-help`                 | `-?`                    | print help text                      |
| `-reader NAME`          | `-r`                    | specify reader                       |
| `-reader-options SEXP`  | `-ro`                   | specify reader options               |
| `-list-readers`         | `-ls-r`, `-ls-readers`  | print list of the available readers  |
| `-printer NAME`         | `-p`                    | specify printer                      |
| `-printer-options SEXP` | `-po`                   | specify printer options              |
| `-list-printers`        | `-ls-p`, `-ls-printers` | print list of the available printers |

*Readers* and *printers*, as their names suggest, are used to read and print data.
See [Readers](#readers) and [Printers](#printers) for detailed information.

These options can also be specified in the config file `~/.estconfig` (see also: [Config file](#config-file)).
Flags always overrides the same options in the config file.

### Readers
*Readers* (not you!) are used to read data from input files.

You can specify a reader by `-reader` flag.
There are two readers available:

* `table` (default)
* `table_ex`

Both readers are basically the same: read a table, each row (line) contains numbers separated by spaces or tabs.
Lines start with `#` will be ignored as comments.

The table in the *n*th file will be assigned to a variable named `$$n` (note that n starts from 0, so the first one is 0th), as a vector of vectors, each component represents a column of the table.
For the first file, `$$` can be also used for the whole table and `$n` for the *n*th column (as used in the first example).

In addition, `table_ex` can read constant values in the input files.
If a comment line starts with `##`, followed by a name and a number (e.g. `## c 3.0e+8`), the number also can be referred to by the name in the program.

Options for the reader can be specified by `-reader-options` flag, in an S-expression.
The default options are as follows:

``` lisp
((strict true)          ; fails if table contains an empty or invalid entry
 (separator (" " "\t")) ; separator(s) used to separate numbers in rows
 (default nan)          ; default value to fill empty or invalid entries
 (transpose false))     ; if true, tables will be transposed
```

All fields are optional and the default values will be used if not specified.
For example, if `-reader-options '((transpose true))'` is specified, tables will be transposed (rows and columns are interchanged), while the default separators will be used.

### Printers
*Printers* are used to print data to the stream.

You may specify a printer by `-printer` flag, but currently there is only one available printer `table`, which is used by default.

Options for the printer can be specified by `-printer-options` flag, in an S-expression.
The default options are as follows:

``` lisp
((strict true)      ; fails if table contains an empty entry
 (separator "\t")   ; separator used to separate numbers in rows
 (precision 8)      ; maximum number of digits of output numbers
 (default nan)      ; default value to fill empty entries
 (transpose false)) ; if true, table will be transposed
```

All fields are optional and the default values will be used if not specified.

### Language
#### Syntax
The syntax of the language is very informally expressed as follows.

```
<term> ::= <literal>
           <identifier>                               -- variable
           "[" <term> "," <term> "," ...  "]"         -- vector
           <term> <term>                              -- application
           "let" <identifier> "=" <term> "in" <term>  -- let-binding
```

Terms can be (or must be, for some cases) parenthesized.
For example, a let-binding must be parenthesized when it is used in an application.

*Literals* are numbers like `1`, `3.14`, `6.626e-34`.

*Identifiers* must start with alphabetical letter or `$`, followed by letters, digits, `$`, `'` or `_`.
All of `x`, `x1`, `Foo_bar'` and `$0` are valid identifiers.

*Applications* are usual function applications, `f x` can be pronounced as "apply a function `f` to the argument `x`".
Arguments are not needed to parenthesized as languages like C.
They are left-associative i.e. `f x y` = `(f x) y`.

*Let-bindings* are used to bind a variable to the first expression in the second (body).
For example, `let x = 1 + 2 in 3 * x` is evaluated to `9`.

In addition, operators can also be used in expressions.
See [Operators](#operators) for the available operators and the precedence between them.

#### Types
There are three types of values:

* number
* function
* vector

A number is an IEEE 754 double precision floating point number (so it can be `nan` or `inf`).

A vector value is an array of zero or more values.
Values in a vector can have any type and can be different to each other.

In the tables below, `*` is used to describe an unknown type, which can be any one of the three types.

#### Predefined constants
| name      | type   | description                      |
| --------- | ------ | -------------------------------- |
| `pi`      | number | pi (= 3.14159...)                |
| `e`       | number | Napier's constant (= 2.71828...) |
| `ln2`     | number | natural logarithm of 2           |
| `ln10`    | number | natural logarithm of 10          |
| `log2e`   | number | base 2 logarithm of e            |
| `log10e`  | number | base 10 logarithm of e           |
| `sqrt2`   | number | square root of 2                 |
| `sqrt1_2` | number | square root of 1/2               |

#### Operators
| arity  | name | type                       | description          | associativity |
| ------ | ---- | -------------------------- | -------------------- | ------------- |
| unary  | `+`  | number → number           |                      |               |
| unary  | `-`  | number → number           |                      |               |
| binary | `+`  | number → number → number |                      | left          |
| binary | `-`  | number → number → number |                      | left          |
| binary | `*`  | number → number → number |                      | left          |
| binary | `/`  | number → number → number |                      | left          |
| binary | `%`  | number → number → number | modulo               | left          |
| binary | `**` | number → number → number | exponentiation       | right         |
| binary | `^`  | number → number → number | (alias for `**`)     | right         |
| binary | `@`  | vector → vector → vector | vector concatenation | left          |
| binary | `!`  | vector → number → *      | index access         | left          |

The arithmetic operators also accept number vectors: `[1, 2, 3] * 4` produces `[4, 8, 12]`, `[1, 2] + [3, 4]` produces `[4, 6]`, etc.
Note that a multiplication of two vectors does not mean inner or outer product.

The precedence of the operators (and application) is as follows.
`!` has the highest precedence, and binary `+` and `-` have the lowest.

1. `!`
2. application
4. unary `+` `-`
3. `@`
5. `**` `^`
6. `*` `/` `%`
7. binary `+` `-`

This means (of course) `1 + 2 * 3` = `1 + (2 * 3)`, not `(1 + 2) * 3`.

#### Functions
| name    | type                       | description                                        |
| ------- | -------------------------- | -------------------------------------------------- |
| `sign`  | number → number           | sign of a number (`+1`, `-1` or `0`)               |
| `abs`   | number → number           | absolute value                                     |
| `round` | number → number           | round to nearest                                   |
| `floor` | number → number           | round down                                         |
| `ceil`  | number → number           | round up                                           |
| `sqrt`  | number → number           | square root                                        |
| `exp`   | number → number           | exponential function                               |
| `expm1` | number → number           | `expm1 x` = `exp x - 1` but more precise           |
| `log`   | number → number           | natural logarithm                                  |
| `log1p` | number → number           | `log1p x` = `log (1 + x)` but more precise         |
| `log2`  | number → number           | Base 2 logarithm                                   |
| `log10` | number → number           | Base 10 logarithm                                  |
| `sin`   | number → number           | sine                                               |
| `cos`   | number → number           | cosine                                             |
| `tan`   | number → number           | tangent                                            |
| `asin`  | number → number           | arcsine (-pi/2 to pi/2)                            |
| `acos`  | number → number           | arccosine (0 to pi)                                |
| `atan`  | number → number           | arctangent (-pi/2 to pi/2)                         |
| `sinh`  | number → number           | hyperbolic sine                                    |
| `cosh`  | number → number           | hyperbolic cosine                                  |
| `tanh`  | number → number           | hyperbolic tangent                                 |
| `log_`  | number → number → number | `log_ b a` = base b logarithm of a                 |
| `atan2` | number → number → number | `atan2 y x` = arctangent of y/x (-pi to pi)        |
| `len`   | vector → number           | vector length                                      |
| `fst`   | vector → *                | first component of a vector                        |
| `take`  | number → vector → vector | take first n components of a vector                |
| `drop`  | number → vector → vector | drop first n components of a vector                |
| `sum`   | vector → number           | sum of a number vector                             |
| `prod`  | vector → number           | product of a number vector                         |
| `max`   | vector → number           | maximum value in a number vector (`-inf` if empty) |
| `min`   | vector → number           | minimum value in a number vector (`inf` if empty)  |
| `avg`   | vector → number           | average                                            |
| `var`   | vector → number           | variance                                           |
| `sd`    | vector → number           | standard deviation                                 |
| `se`    | vector → number           | standard error                                     |
| `cov`   | vector → vector → number | covariance                                         |
| `cor`   | vector → vector → number | Pearson correlation coefficient                    |

The mathematical functions which takes numbers also accept number vectors.
For example, `sign [2, -3, 0]` produces `[1, -1, 0]`.

### Config file
The config file `~/.estconfig` is convenient to specify the options that you commonly use.
The options in the config file are written in S-expression.

This is a template of the config file (`;` is used to comment out a line).

```lisp
(
 ; (reader table)  ; default reader
 ; (printer table) ; default printer
 (reader_options (
  (table (
   ; (strict true)          ; fails if table contains an empty or invalid entry
   ; (separator (" " "\t")) ; separator(s) used to separate numbers in rows
   ; (default nan)          ; default value to fill empty or invalid entries
   ; (transpose false))     ; if true, tables will be transposed
  ))
  (table_ex (
   ; (strict true)          ; fails if table contains an empty or invalid entry
   ; (separator (" " "\t")) ; separator(s) used to separate numbers in rows
   ; (default nan)          ; default value to fill empty or invalid entries
   ; (transpose false))     ; if true, tables will be transposed
  ))
 ))
 (printer_options (
  (table (
   ; (strict true)      ; fails if table contains an empty entry
   ; (separator "\t")   ; separator used to separate numbers in rows
   ; (precision 8)      ; maximum number of digits of output numbers
   ; (default nan)      ; default value to fill empty entries
   ; (transpose false)) ; if true, table will be transposed
  ))
 ))
)
```

Note that command line flags always overrides the same options in the config file (see also: [Flags](#flags)).

## License
[MIT License](http://opensource.org/licenses/mit-license.php)

## Author
Susisu ([GitHub](https://github.com/susisu), [Twitter](https://twitter.com/susisu2413))
