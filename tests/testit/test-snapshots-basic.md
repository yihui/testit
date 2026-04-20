This block checks simple vector printing output.

```r
1:5
```

```
[1] 1 2 3 4 5
```

This block checks tabular printing from a data frame.

```r
data.frame(x = 1:3, y = letters[1:3])
```

```
  x y
1 1 a
2 2 b
3 3 c
```

This block checks mixed cat()/print() output ordering.

```r
cat('Line 1\n')
print('Text output')
cat('Line 2\n')
```

```
Line 1
[1] "Text output"
Line 2
```
