```r
mini_diff(c('a', 'b', 'c'), c('a', 'c'))
```

```
  a
- b
  c
```

```r
mini_diff(c('a', 'c'), c('a', 'b', 'c'))
```

```
  a
+ b
  c
```

```r
mini_diff(c('a'), c('b'))
```

```
- a
+ b
```

```r
x1 = paste0('L', 1:16)
x2 = x1
x2[c(3, 13)] = c('X', 'Y')
mini_diff(x1, x2)
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
