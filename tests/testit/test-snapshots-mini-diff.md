This block checks deletion-style mini_diff output.

```r
capture.output(mini_diff(c('a', 'b', 'c'), c('a', 'c')))
```

```
[1] "  a " "- b " "  c "
```

This block checks insertion-style mini_diff output.

```r
capture.output(mini_diff(c('a', 'c'), c('a', 'b', 'c')))
```

```
[1] "  a " "+ b " "  c "
```

This block checks replacement-style mini_diff output.

```r
capture.output(mini_diff(c('a'), c('b')))
```

```
[1] "- a " "+ b "
```

This block checks that mini_diff emits an ellipsis for skipped context.

```r
x1 = paste0('L', 1:16)
x2 = x1
x2[c(3, 13)] = c('X', 'Y')
any(capture.output(mini_diff(x1, x2)) == '  ...')
```

```
[1] TRUE
```
