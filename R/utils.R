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

# get fence for code blocks based on content
get_fence = function(text, extra = FALSE) {
  ms = gregexpr('^`+', text, perl = TRUE)
  n = max(unlist(lapply(ms, attr, 'match.length')))
  if (extra && n >= 3) n = n + 1
  strrep('`', max(n, 3))
}

# Parse markdown file to extract code blocks
parse_snapshot = function(lines, file) {
  # Find all fence lines
  idx = grepl(r <- sprintf('^%s\\s*(\\{r\\})?\\s*', get_fence(lines)), lines)
  if (sum(idx) %% 2 != 0) stop('Unmatched code fences in ', error_loc(file))
  # Change TRUE to FALSE for idx elements at even positions and their next
  # elements to TRUE to mark the start of the next block
  fences = which(idx)
  i = seq_len(length(fences)/2) * 2
  idx[fences[i]] = FALSE
  idx[fences[i] + 1] = TRUE
  # Split lines into code, output, and text blocks
  N = seq_along(lines)
  blocks = split(data.frame(lines, N), cumsum(idx[N]))
  lapply(blocks, function(b) {
    n = nrow(b)
    if (n < 2 || !grepl(r, b[1, 1])) list(type = 'text', content = b[, 1]) else {
      list(type = gsub('^```+|\\s+', '', b[1, 1]), content = b[-c(1, n), 1], line = b[1, 2])
    }
  })
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
  update = isTRUE(as.logical(update))
  for (f in md_files) {
    raw_lines = readLines(f, warn = FALSE, encoding = 'UTF-8')
    blocks = parse_snapshot(raw_lines, f)
    new_blocks = list(); changed = TRUE
    pos = NULL  # record the first line of the first failed block for error reporting

    # Process blocks in pairs: R code block followed by output block
    N = length(blocks)
    for (i in seq_len(N)) {
      block = blocks[[i]]
      new_blocks[[length(new_blocks) + 1]] = block  # Add current block to new_blocks
      if (block$type != '{r}') next

      out = capture_output(block$content, envir)
      # look for the next output block k
      k = NULL
      if (i + 1 <= N) for (j in (i + 1):N) {
        if (blocks[[j]]$type == '') {
          k = j; break
        }
      }
      if (is.null(k)) {
        # no output block, add one
        new_blocks[[length(new_blocks) + 1]] = list(type = '', content = out)
        changed = TRUE
      } else {
        expected_lines = blocks[[k]]$content
        if (!update) {
          if (identical(out, expected_lines)) next
          changed = TRUE; if (is.null(pos)) pos = block$line
        }
        blocks[[k]] = list(type = '', content = out)
      }
    }

    # Write updated markdown if needed
    if (changed) {
      # Determine fence to use
      all_content = unlist(lapply(new_blocks, function(b) b$content))
      fence = get_fence(all_content, TRUE)
      out_lines = unlist(lapply(new_blocks, function(b) {
        if (b$type == 'text') b$content else {
          c(paste0(fence, if (b$type != '') '{r}'), b$content, fence)
        }
      }))
      if (update || is.null(pos)) {
        write_utf8(out_lines, f)
        message('Updated snapshot file: ', f)
      } else {
        tracked = system2(
          'git', c('ls-files', '--error-unmatch', shQuote(f)), stdout = NULL, stderr = NULL
        ) == 0
        if (tracked) {
          write_utf8(out_lines, f)
          system2('git', c('diff', '--color=auto', shQuote(f)))
        } else {
          mini_diff(raw_lines, out_lines)
        }
        stop(
          'Snapshot test failed', error_loc(f, pos), '\n',
          if (tracked) 'If the changes are not expected, revert them in GIT.' else
            'Set R_TESTIT_UPDATE_SNAPSHOTS=true to update.', call. = FALSE
        )
      }
    }
  }
}

capture_output = function(code, envir) {
  # Execute R code and capture output
  out = tryCatch(capture.output({
    exprs = if (length(code)) parse(text = code, keep.source = FALSE)
    for (expr in exprs) {
      res = withVisible(eval(expr, envir = envir))
      if (res$visible) print(res$value)
    }
  }), error = function(e) paste('Error:', conditionMessage(e)))
  # Clean output
  clean_output(out)
}

write_utf8 = function(text, con) {
  opts = options(encoding = "native.enc")
  on.exit(options(opts))
  writeLines(enc2utf8(text), con, useBytes = TRUE)
}

# Output a minimal diff between two character vectors, showing only lines that
# are different and 3 lines of context around them. Lines starting with " " are
# unchanged, "-" are in x1 but not x2, "+" are in x2 but not x1.
mini_diff = function(x1, x2) {
  out = character()
  i = 1; j = 1
  n1 = length(x1); n2 = length(x2)

  # 1. Alignment Loop
  while (i <= n1 || j <= n2) {
    if (i <= n1 && j <= n2 && x1[i] == x2[j]) {
      out = c(out, paste(" ", x1[i])); i = i + 1; j = j + 1
    } else {
      m_i = if (i <= n1 && j <= n2) match(x2[j], x1[i:n1]) else NA
      m_j = if (i <= n1 && j <= n2) match(x1[i], x2[j:n2]) else NA
      if (!is.na(m_i) && (is.na(m_j) || m_i <= m_j)) {
        out = c(out, paste("-", x1[i])); i = i + 1
      } else if (!is.na(m_j)) {
        out = c(out, paste("+", x2[j])); j = j + 1
      } else {
        if (i <= n1) { out = c(out, paste("-", x1[i])); i = i + 1 }
        if (j <= n2) { out = c(out, paste("+", x2[j])); j = j + 1 }
      }
    }
  }

  # 2. Context Filtering (Keep 3 lines around any change)
  if (length(out) > 0) {
    is_change = !startsWith(out, " ")
    change_idx = which(is_change)

    # Identify indices within 3 steps of a change
    keep_idx = unique(as.integer(outer(change_idx, -3:3, "+")))
    keep_idx = sort(keep_idx[keep_idx > 0 & keep_idx <= length(out)])

    # Print with "..." where gaps occur
    last_idx = 0
    for (idx in keep_idx) {
      if (idx > last_idx + 1) cat("  ...\n")
      cat(out[idx], "\n")
      last_idx = idx
    }
  }
}
