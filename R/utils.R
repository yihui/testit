# an internal environment to store objects
.env = new.env(parent = emptyenv())

# has the package been installed once in test_pkg()?
.env$installed = FALSE

# find an available dir
available_dir = function(dirs) {
  for (i in dirs) {
    if (utils::file_test('-d', i)) return(i)
  }
  stop('none of the directories exists:\n', paste(utils::formatUL(dirs), collapse = '\n'))
}

# tailored for assert(): extract the expression that is likely to be useful for
# diagnostics if possible
deparse_key = function(expr) {
  x = deparse(expr, width.cutoff = 100L)
  if ((n <- length(x)) <= 1) return(x)
  # if expression is in {}, fetch the line n-1, otherwise use the first line
  paste(x[1], '....', if (x[n] == '}') sub('^\\s*', '', x[n - 1L]))
}

# whether every element of x is strictly TRUE
all_true = function(x) {
  is.logical(x) && length(x) && !any(is.na(x)) && all(x)
}

insert_identical = function() {
  rstudioapi::insertText(text = ' %==% ')
}

# This function is a modification of base::sys.source.  It allows to specify
# the top-level environment, which is by default "envir" (the same as in
# base::sys.source), but for package testing it is desirable to use the
# package namespace to mimic the environment structure used when packages
# are running. This function assumes that chdir = FALSE and keep.source = TRUE.
sys.source2 = function(file, envir, top.env = as.environment(envir)) {
  oop = options(keep.source = TRUE, topLevelEnvironment = top.env)
  on.exit(options(oop), add = TRUE)

  lines = readLines(file, warn = FALSE, encoding = 'UTF-8')
  srcfile = srcfilecopy(file, lines, file.mtime(file), isFile = TRUE)
  exprs = parse(text = lines, srcfile = srcfile, encoding = 'UTF-8')

  if (length(exprs) == 0L) return()
  owd = setwd(dirname(file)); on.exit(setwd(owd), add = TRUE)
  for (i in seq_along(exprs)) eval(exprs[i], envir)
}

# Clean output to remove unstable elements like bytecode addresses
clean_output = function(lines) {
  # Remove addresses like <bytecode: 0x...>, <environment: 0x...>, <pointer: 0x...>
  gsub('<(bytecode|environment|pointer): 0x[0-9a-f]+>', '<\\1: ...>', lines)
}

# Find the longest backtick sequence in text
find_longest_backticks = function(text) {
  matches = gregexpr('`+', text, perl = TRUE)
  max_len = 0
  for (m in matches) {
    if (m[1] != -1) {
      lens = attr(m, 'match.length')
      max_len = max(max_len, max(lens))
    }
  }
  max_len
}

# Parse markdown file to extract code blocks
parse_snapshot_md = function(file) {
  lines = readLines(file, warn = FALSE, encoding = 'UTF-8')
  
  blocks = list()
  i = 1
  while (i <= length(lines)) {
    line = lines[i]
    # Check if this line starts a code fence
    if (grepl('^`{3,}', line)) {
      # Extract fence and language
      fence_match = regmatches(line, regexpr('^`+', line))
      fence = fence_match[1]
      lang = trimws(sub('^`+', '', line))
      
      # Collect lines until we find the closing fence
      code_lines = character()
      i = i + 1
      while (i <= length(lines)) {
        if (lines[i] == fence) {
          # Found closing fence
          break
        }
        code_lines = c(code_lines, lines[i])
        i = i + 1
      }
      
      # Determine block type
      block_type = if (tolower(lang) == 'r') 'r' else 'output'
      
      blocks[[length(blocks) + 1]] = list(
        type = block_type,
        content = paste(code_lines, collapse = '\n')
      )
    }
    i = i + 1
  }
  
  # Find longest backtick sequence in all content
  all_content = paste(sapply(blocks, function(b) b$content), collapse = '\n')
  max_backticks = find_longest_backticks(all_content)
  
  list(blocks = blocks, max_backticks = max_backticks)
}

# Run snapshot tests from markdown files
run_snapshot_tests = function(md_files, envir, update = FALSE) {
  for (md_file in md_files) {
    cat('Running snapshot tests from:', basename(md_file), '\n')
    
    parsed = parse_snapshot_md(md_file)
    blocks = parsed$blocks
    max_backticks = parsed$max_backticks
    
    if (length(blocks) == 0) next
    
    # Process blocks in pairs: R code block followed by output block
    i = 1
    updated = FALSE
    new_blocks = list()
    
    while (i <= length(blocks)) {
      block = blocks[[i]]
      
      if (block$type == 'r') {
        # Execute R code and capture output
        output_lines = tryCatch({
          capture.output({
            exprs = parse(text = block$content)
            for (expr in exprs) {
              result = withVisible(eval(expr, envir = envir))
              # Only auto-print if result is visible and non-NULL
              if (result$visible && !is.null(result$value)) {
                print(result$value)
              }
            }
          })
        }, error = function(e) {
          c(paste('Error:', conditionMessage(e)))
        })
        
        # Clean output
        output_lines = clean_output(output_lines)
        output_text = paste(output_lines, collapse = '\n')
        
        # Add R block to new blocks
        new_blocks[[length(new_blocks) + 1]] = list(type = 'r', content = block$content)
        
        # Check if next block is output block
        if (i + 1 <= length(blocks) && blocks[[i + 1]]$type == 'output') {
          expected_text = blocks[[i + 1]]$content
          
          if (update) {
            # Update mode: replace expected with actual
            new_blocks[[length(new_blocks) + 1]] = list(type = 'output', content = output_text)
            updated = TRUE
            i = i + 2
          } else {
            # Compare mode
            if (!identical(output_text, expected_text)) {
              cat('\n')
              cat('Snapshot test failed in:', md_file, '\n')
              cat('R code:\n')
              cat(block$content, '\n')
              cat('\n')
              
              # Try to use diff command
              diff_available = system2('which', 'diff', stdout = FALSE, stderr = FALSE) == 0
              
              if (diff_available) {
                # Write to temp files and use diff
                tmp_expected = tempfile()
                tmp_actual = tempfile()
                writeLines(expected_text, tmp_expected)
                writeLines(output_text, tmp_actual)
                
                cat('Diff:\n')
                system2('diff', c('-u', tmp_expected, tmp_actual), stdout = TRUE, stderr = TRUE)
                
                unlink(c(tmp_expected, tmp_actual))
              } else {
                # Fallback: show expected vs actual
                cat('Expected:\n')
                cat(expected_text, '\n')
                cat('\nActual:\n')
                cat(output_text, '\n')
              }
              
              stop('Snapshot test failed. Set R_TESTIT_UPDATE_SNAPSHOTS=true to update.', call. = FALSE)
            }
            
            new_blocks[[length(new_blocks) + 1]] = list(type = 'output', content = expected_text)
            i = i + 2
          }
        } else {
          # No expected output block, add one in update mode
          if (update) {
            new_blocks[[length(new_blocks) + 1]] = list(type = 'output', content = output_text)
            updated = TRUE
          }
          i = i + 1
        }
      } else {
        # Non-R block (orphaned output block), keep as-is
        new_blocks[[length(new_blocks) + 1]] = block
        i = i + 1
      }
    }
    
    # Write updated markdown if in update mode
    if (update && updated) {
      # Use n+3 backticks for all code blocks if n backticks are found inside any block
      out_fence_len = max(3, max_backticks + 3)
      out_fence = paste(rep('`', out_fence_len), collapse = '')
      
      out_lines = character()
      for (block in new_blocks) {
        if (block$type == 'r') {
          out_lines = c(out_lines, paste0(out_fence, 'r'))
        } else {
          out_lines = c(out_lines, out_fence)
        }
        out_lines = c(out_lines, block$content)
        out_lines = c(out_lines, out_fence)
        out_lines = c(out_lines, '')  # Empty line between blocks
      }
      
      writeLines(out_lines, md_file)
      cat('Updated snapshot file:', md_file, '\n')
    }
  }
}
