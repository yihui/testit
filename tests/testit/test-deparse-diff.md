Test deparse_diff() via equ_info() when str() is identical for both sides.
These tests require `digits17` deparse control (R >= 3.5.0).

```r
if (getRversion() < '3.5.0') cat('skip')
```
```
```

Tiny numeric difference (str() shows the same, deparse reveals the diff):

```r
cat(equ_info(1.0000000000001, 1.0000000000002), sep = '\n')
```

```
x (LHS) ==>
 num 1
----------
 num 1
<== (RHS) y

Detailed diff (- LHS, + RHS):
- 1.0000000000000999
+ 1.0000000000002001
```

When str() already differs, no detailed diff is shown:

```r
cat(equ_info('hello', 'world'), sep = '\n')
```

```
x (LHS) ==>
 chr "hello"
----------
 chr "world"
<== (RHS) y
```

Large vector where only a deep element differs (invisible to str()):

```r
x = as.double(1:200)
y = x; y[100] = y[100] * (1 + 1e-14)
cat(equ_info(x, y), sep = '\n')
```

```
x (LHS) ==>
 num [1:200] 1 2 3 4 5 6 7 8 9 10 ...
----------
 num [1:200] 1 2 3 4 5 6 7 8 9 10 ...
<== (RHS) y

Detailed diff (- LHS, + RHS):
  ...
  97
  98
  99
- 100
+ 100.00000000000099
  101
  102
  103
```

Data frame where one cell differs (invisible to str()):

```r
x = data.frame(a = 1:3, b = c(1.1, 2.2, 3.3))
y = x; y$b[2] = 2.2000000000001
cat(equ_info(x, y), sep = '\n')
```

```
x (LHS) ==>
'data.frame':	3 obs. of  2 variables:
 $ a: int  1 2 3
 $ b: num  1.1 2.2 3.3
----------
'data.frame':	3 obs. of  2 variables:
 $ a: int  1 2 3
 $ b: num  1.1 2.2 3.3
<== (RHS) y

Detailed diff (- LHS, + RHS):
  structure(list(a = 1:3
  b = c(1.1000000000000001
- 2.2000000000000002
+ 2.2000000000001001
  3.2999999999999998))
- class = "data.frame"
  row.names = c(NA
- -3L))
+ -3L)
+ class = "data.frame")
```

Large diff hits max_diff and returns partial output:

```r
x = as.double(1:200)
y = x; y[20:200] = y[20:200] * (1 + 1e-14)
cat(equ_info(x, y), sep = '\n')
```

```
x (LHS) ==>
 num [1:200] 1 2 3 4 5 6 7 8 9 10 ...
----------
 num [1:200] 1 2 3 4 5 6 7 8 9 10 ...
<== (RHS) y

Detailed diff (- LHS, + RHS):
  ...
  17
  18
  19
- 20
+ 20.000000000000199
- 21
+ 21.00000000000021
- 22
+ 22.00000000000022
- 23
+ 23.000000000000231
- 24
+ 24.000000000000242
- 25
+ 25.000000000000249
- 26
+ 26.000000000000259
- 27
+ 27.00000000000027
- 28
+ 28.000000000000281
- 29
+ 29.000000000000291
- 30
+ 30.000000000000298
- 31
+ 31.000000000000309
- 32
+ 32.00000000000032
- 33
+ 33.000000000000327
- 34
+ 34.000000000000341
- 35
+ 35.000000000000348
- 36
+ 36.000000000000362
- 37
+ 37.000000000000369
- 38
+ 38.000000000000377
- 39
+ 39.000000000000391
- 40
+ 40.000000000000398
- 41
+ 41.000000000000412
- 42
+ 42.000000000000419
- 43
+ 43.000000000000426
- 44
+ 44.000000000000441
```
