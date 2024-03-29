Probably `(quotient x 3)` is nicer than `(floor (/ x 3))`

There's a pattern in these puzzles, which is: given a list of things and a
carry, make a new list where each element is the combination of the carry and
the next value in the list; and the carry is the next value (or a function
thereof). It's like for/fold/list. Or, possibly, it's a generator. 

My solutions tend to be rather verbose. At the same time, I often find myself
regretting not adding helper functions earlier. 

Thoughts on improvement:
- try more top-down programming (writing the final answer first, applied to the
  tests, noting that the test will fail)
- try to be more consistent with naming from the start. Eg, I make use of
  structures for types; so, for operations on types of `things`, start all
  functions with `thing-`
- try to reach for `fold` or `for` before `let`
- just don't worry about memory: stick to immutable structures, and prefer lists
  (which support fold!)
- prefer `match` over `car` and `cadr`, especially for the output of
  `regexp-match`, as it indicates the kinds of structures that are expected.

mbutterick simply converted numbers to strings when necessary! Maybe I should
stop worrying about performance.

The pattern I'm looking for may be what Haskell calls `scanl`.

`in-slice` is a thing. It peels off n elements of a list at a time.





