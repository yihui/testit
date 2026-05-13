Short `%==%` failure shows full LHS/RHS display.

```r
msg = tryCatch(
  assert('short', { (list(a = 1, b = 2) %==% list(a = 1, b = 99)) }),
  error = conditionMessage
)
cat(msg, sep = '\n')
```
```
-- Assertion failed: short --
   list(a = 1, b = 2) (LHS) ==>
   List of 2
    $ a: num 1
    $ b: num 2
   ----------
   List of 2
    $ a: num 1
    $ b: num 99
   <== (RHS) list(a = 1, b = 99)
   list(a = 1, b = 2) %==% list(a = 1, b = 99) is not TRUE but FALSE at <text>#2
```

Long `%==%` failure shows mini_diff instead.

```r
x = as.list(setNames(1:15, paste0('item', 1:15)))
y = x; y$item5 = 99; y$item12 = 88
msg = tryCatch(
  assert('long', { (x %==% y) }),
  error = conditionMessage
)
cat(msg, sep = '\n')
```
```
-- Assertion failed: long --
   x (- LHS) vs y (+ RHS):
     ...
      $ item2 : int 2
      $ item3 : int 3
      $ item4 : int 4
   -  $ item5 : int 5
   +  $ item5 : num 99
      $ item6 : int 6
      $ item7 : int 7
      $ item8 : int 8
      $ item9 : int 9
      $ item10: int 10
      $ item11: int 11
   -  $ item12: int 12
   +  $ item12: num 88
      $ item13: int 13
      $ item14: int 14
      $ item15: int 15
   x %==% y is not TRUE but FALSE at <text>#4
```
