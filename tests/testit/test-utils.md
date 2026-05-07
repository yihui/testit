This block checks deletion-style mini_diff output.

```r
cat(mini_diff(c('a', 'b', 'c'), c('a', 'c')), sep = '\n')
```

```
  a
- b
  c
```

This block checks insertion-style mini_diff output.

```r
cat(mini_diff(c('a', 'c'), c('a', 'b', 'c')), sep = '\n')
```

```
  a
+ b
  c
```

This block checks replacement-style mini_diff output.

```r
cat(mini_diff(c('a'), c('b')), sep = '\n')
```

```
- a
+ b
```

This block checks that mini_diff emits an ellipsis for skipped context.

```r
x1 = paste0('L', 1:16)
x2 = x1
x2[c(3, 13)] = c('X', 'Y')
cat(mini_diff(x1, x2), sep = '\n')
```

```
  L1
  L2
- L3
+ X
  L4
  L5
  L6
  ...
  L10
  L11
  L12
- L13
+ Y
  L14
  L15
  L16
```
