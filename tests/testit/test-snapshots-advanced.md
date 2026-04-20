This block checks nested backticks in output text.

`````r
cat("Use `code` for inline code\n")
cat("````r for R code blocks\n")
`````

`````
Use `code` for inline code
````r for R code blocks
`````

This block checks stabilized environment printing.

`````r
e <- new.env()
print(e)
`````

`````
<environment: ...>
`````
