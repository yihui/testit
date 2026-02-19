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

# Create fence for code blocks based on content
make_fence = function(text) {
  ms = gregexpr('`+', text, perl = TRUE)
  n = max(2, unlist(lapply(ms, attr, 'match.length')))
  strrep('`', n + 1)
}

# Parse markdown file to extract code blocks
parse_snapshot_md = function(file) {
  lines = readLines(file, warn = FALSE, encoding = 'UTF-8')
  
  # Find all fence lines
  fence_idx = grep('^```', lines)
  if (length(fence_idx) == 0) return(list(blocks = list(), lines = lines))
  
  # Pair up opening and closing fences
  blocks = list()
  i = 1
  while (i < length(fence_idx)) {
    open_idx = fence_idx[i]
    close_idx = fence_idx[i + 1]
    
    # Extract fence and language
    open_line = lines[open_idx]
    lang = trimws(sub('^```+', '', open_line))
    
    # Extract content between fences
    if (close_idx > open_idx + 1) {
      content_lines = lines[(open_idx + 1):(close_idx - 1)]
    } else {
      content_lines = character(0)
    }
    
    # Determine block type
    block_type = if (tolower(lang) == 'r') 'r' else 'output'
    
    blocks[[length(blocks) + 1]] = list(
      type = block_type,
      content = content_lines
    )
    
    i = i + 2
  }
  
  list(blocks = blocks, lines = lines)
}

#' Run snapshot tests from markdown files
#'
#' Execute snapshot tests from markdown files containing R code blocks and
#' expected output blocks. This function is called automatically by
#' \code{\link{test_pkg}()} for files matching \code{test-*.md}, but can also
#' be run manually.
#' @param md_files Character vector of paths to markdown files
#' @param envir Environment in which to evaluate R code
#' @param update Logical; if \code{TRUE}, update snapshot files with actual
#'   output instead of comparing. Can also be set via the environment variable
#'   \code{R_TESTIT_UPDATE_SNAPSHOTS=true}.
#' @return Invisible \code{NULL}. Stops with an error if any snapshot test fails.
#' @export
#' @examples
#' \dontrun{
#' # Manually run snapshot tests
#' test_snaps('test-output.md', globalenv(), update = FALSE)
#' 
#' # Update snapshots
#' test_snaps('test-output.md', globalenv(), update = TRUE)
#' }
test_snaps = function(md_files, envir, update = FALSE) {
  for (md_file in md_files) {
    cat('Running snapshot tests from:', basename(md_file), '\n')
    
    parsed = parse_snapshot_md(md_file)
    blocks = parsed$blocks
    lines = parsed$lines
    
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
              # Only auto-print if result is visible
              if (result$visible) {
                print(result$value)
              }
            }
          })
        }, error = function(e) {
          c(paste('Error:', conditionMessage(e)))
        })
        
        # Clean output
        output_lines = clean_output(output_lines)
        
        # Add R block to new blocks
        new_blocks[[length(new_blocks) + 1]] = list(type = 'r', content = block$content)
        
        # Check if next block is output block
        if (i + 1 <= length(blocks) && blocks[[i + 1]]$type == 'output') {
          expected_lines = blocks[[i + 1]]$content
          
          if (update) {
            # Update mode: replace expected with actual
            new_blocks[[length(new_blocks) + 1]] = list(type = 'output', content = output_lines)
            updated = TRUE
            i = i + 2
          } else {
            # Compare mode
            if (!identical(output_lines, expected_lines)) {
              cat('\n')
              cat('Snapshot test failed in:', md_file, '\n')
              cat('R code:\n')
              cat(block$content, sep = '\n')
              cat('\n')
              
              # Try to use diff command
              diff_available = system2('which', 'diff', stdout = FALSE, stderr = FALSE) == 0
              
              if (diff_available) {
                # Write to temp files and use diff
                tmp_expected = tempfile()
                tmp_actual = tempfile()
                writeLines(expected_lines, tmp_expected)
                writeLines(output_lines, tmp_actual)
                
                cat('Diff:\n')
                diff_output = system2('diff', c('-u', tmp_expected, tmp_actual), 
                                     stdout = TRUE, stderr = TRUE)
                cat(diff_output, sep = '\n')
                
                unlink(c(tmp_expected, tmp_actual))
              } else {
                # Fallback: show expected vs actual
                cat('Expected:\n')
                cat(expected_lines, sep = '\n')
                cat('\nActual:\n')
                cat(output_lines, sep = '\n')
              }
              
              stop('Snapshot test failed. Set R_TESTIT_UPDATE_SNAPSHOTS=true to update.', call. = FALSE)
            }
            
            new_blocks[[length(new_blocks) + 1]] = list(type = 'output', content = expected_lines)
            i = i + 2
          }
        } else {
          # No expected output block, add one
          new_blocks[[length(new_blocks) + 1]] = list(type = 'output', content = output_lines)
          if (!update) {
            # In non-update mode, this is a new snapshot
            updated = TRUE
          } else {
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
    
    # Write updated markdown if needed
    if (updated) {
      # Determine fence to use
      all_content = unlist(lapply(new_blocks, function(b) b$content))
      fence = make_fence(all_content)
      
      out_lines = character()
      for (block in new_blocks) {
        if (block$type == 'r') {
          out_lines = c(out_lines, paste0(fence, 'r'))
        } else {
          out_lines = c(out_lines, fence)
        }
        out_lines = c(out_lines, block$content)
        out_lines = c(out_lines, fence)
        out_lines = c(out_lines, '')  # Empty line between blocks
      }
      
      writeLines(out_lines, md_file)
      cat('Updated snapshot file:', md_file, '\n')
    }
  }
}
