# scala-labo-redos

> A vulnerable regex ([ReDoS][]) checker.

[ReDoS]: https://en.wikipedia.org/wiki/ReDoS

## Supported Features of Regular Expression Syntax

This tool supports to analyze a subset of ECMA-262 (JavaScript) regular expression.
Detailed supported features are listed on the below table.

Meanings of status symbols:

  - `○`: this feature is completely supported.
  - `△`: this feature is supported, but partially.
  - `×`: this feature is not yed supported, but it is planned to support.

| Feature                            | Status  |
|:-----------------------------------|:-------:|
| ignore-case (`/.../i`) flag        | ×       |
| multiline (`/.../m`) flag          | ×       |
| unicode (`/.../u`) flag            | ×       |
| dot-all (`/.../s`) flag            | ○       |
| concatenation (`/ab/`)             | ○       |
| alternation (`/a|b/`)              | ○       |
| `*` repetition (`/a*/`)            | ○       |
| `*?` repetition (`/a*?/`)          | ○       |
| `+` repetition (`/a+/`)            | ○       |
| `+?` repetition (`/a+?/`)          | ○       |
| `?` choice (`/a?/`)                | ○       |
| `??` choice (`/a??/`)              | ○       |
| `{n}` repetition (`/a{n}/`)        | △(1)    |
| `{n,}` repetition (`/a{n,}/`)      | △(1)    |
| `{n,}?` repetition (`/a{n,}?/`)    | △(1)    |
| `{n,m}` repetition (`/a{n,m}/`)    | △(1)    |
| `{n,m}?` repetition (`/a{n,m}?/`)  | △(1)    |
| single character (`/c/`)           | △(2)    |
| character class (`/[c-d]/`)        | ×       |
| dot (`/./`)                        | ○       |
| line-begin assertion (`/^/`)       | △(3)    |
| line-end assertion (`/$/`)         | △(3)    |
| word boundary assertion (`/\b/`)   | ×       |
| look-ahead assertion (`/(?=a)/`)   | ×       |
| look-behind assertion (`/(?<=a)/`) | ×       |
| back reference (`/\1/`)            | ×       |

(1): These types repetitions are always expanded to the regular expression with `n`-times repetition.
  In general, it is useful for reasonable analysis to consider it as `*` repetition when `n` is large.
  However this case is not yet supported.

(2): An ignore-case flag is not yet supported.

(3): Currently it is only supported when it is placed at the begin/end of patterns.
