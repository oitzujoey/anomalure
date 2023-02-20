# Anomalure

A simple experimental lisp

## Goals

* Minimize the keyword count.
* Remain untyped, even with abstractions such as numbers and records.
* Provide similar or better extensibility than Common Lisp.

## Language

The core language is lambda calculus, but function calls are explicit.

| Form | Description |
|:-|:-|
| (lambda name body) | Lambda. |
| (funcall function argument) | Function call. |

