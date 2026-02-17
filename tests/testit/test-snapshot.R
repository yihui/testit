library(testit)

# Test snapshot function with simple output
snapshot('simple_vector', {
  print(1:5)
})

# Test snapshot with data frame
snapshot('data_frame', {
  print(data.frame(x = 1:3, y = letters[1:3]))
})

# Test snapshot with multiple outputs
snapshot('multiple_outputs', {
  cat('Line 1\n')
  print('Text output')
  cat('Line 2\n')
})

# Test snapshot with structured data
snapshot('structured_data', {
  list(
    numbers = 1:5,
    letters = letters[1:5],
    matrix = matrix(1:6, nrow = 2)
  )
})
