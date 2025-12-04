# Boolean Logic Circuit Analysis and Optimization
# R Programming Implementation for Truth Table Generation and K-Map Minimization

library(methods)

# Boolean Expression Parser and Evaluator
BooleanCircuit <- setRefClass("BooleanCircuit",
  fields = list(
    expression = "character",
    variables = "character",
    truth_table = "data.frame",
    kmap = "matrix"
  ),
  
  methods = list(
    initialize = function(expr = "") {
      expression <<- expr
      variables <<- character(0)
      truth_table <<- data.frame()
      kmap <<- matrix()
    },
    
    parse_variables = function() {
      vars <- unique(unlist(regmatches(expression, gregexpr("[A-Za-z]", expression))))
      variables <<- sort(vars)
      return(variables)
    },
    
    evaluate_expression = function(var_values) {
      expr_eval <- expression
      
      # Replace variables with values
      for (var in names(var_values)) {
        expr_eval <- gsub(var, var_values[[var]], expr_eval, fixed = TRUE)
      }
      
      # Replace operators
      expr_eval <- gsub("\\*", "&", expr_eval)
      expr_eval <- gsub("\\+", "|", expr_eval)
      expr_eval <- gsub("'", "!", expr_eval)
      expr_eval <- gsub("⊕", "xor", expr_eval)
      
      tryCatch({
        result <- eval(parse(text = expr_eval))
        return(as.integer(result))
      }, error = function(e) {
        return(NA)
      })
    },
    
    generate_truth_table = function() {
      vars <- parse_variables()
      n_vars <- length(vars)
      n_rows <- 2^n_vars
      
      # Create all combinations
      combinations <- expand.grid(lapply(1:n_vars, function(x) c(0, 1)))
      colnames(combinations) <- vars
      
      # Evaluate expression for each combination
      output <- apply(combinations, 1, function(row) {
        var_values <- as.list(row)
        names(var_values) <- vars
        evaluate_expression(var_values)
      })
      
      truth_table <<- cbind(combinations, Output = output)
      return(truth_table)
    },
    
    generate_karnaugh_map = function() {
      if (nrow(truth_table) == 0) {
        generate_truth_table()
      }
      
      n_vars <- length(variables)
      
      if (n_vars == 2) {
        kmap <<- matrix(truth_table$Output, nrow = 2, byrow = TRUE)
      } else if (n_vars == 3) {
        gray_order <- c(1, 2, 4, 3)
        km <- matrix(0, nrow = 2, ncol = 4)
        for (i in 1:2) {
          for (j in 1:4) {
            idx <- (i - 1) * 4 + gray_order[j]
            km[i, j] <- truth_table$Output[idx]
          }
        }
        kmap <<- km
      } else if (n_vars == 4) {
        gray_order <- c(1, 2, 4, 3, 5, 6, 8, 7, 13, 14, 16, 15, 9, 10, 12, 11)
        km <- matrix(0, nrow = 4, ncol = 4)
        for (i in 1:4) {
          for (j in 1:4) {
            idx <- gray_order[(i - 1) * 4 + j]
            km[i, j] <- truth_table$Output[idx]
          }
        }
        kmap <<- km
      }
      
      return(kmap)
    },
    
    find_prime_implicants = function() {
      if (length(kmap) == 0) {
        generate_karnaugh_map()
      }
      
      ones_positions <- which(kmap == 1, arr.ind = TRUE)
      
      if (nrow(ones_positions) == 0) {
        return(character(0))
      }
      
      implicants <- c()
      n_vars <- length(variables)
      
      # Simplified grouping logic
      if (n_vars == 2) {
        for (i in 1:nrow(ones_positions)) {
          row <- ones_positions[i, 1]
          col <- ones_positions[i, 2]
          term <- paste0(
            ifelse(row == 1, paste0(variables[1], "'"), variables[1]),
            ifelse(col == 1, paste0(variables[2], "'"), variables[2])
          )
          implicants <- c(implicants, term)
        }
      }
      
      return(unique(implicants))
    },
    
    minimize_expression = function() {
      implicants <- find_prime_implicants()
      
      if (length(implicants) == 0) {
        return("0")
      }
      
      minimized <- paste(implicants, collapse = " + ")
      return(minimized)
    },
    
    export_to_verilog = function(module_name = "boolean_circuit") {
      vars <- variables
      verilog_code <- paste0(
        "module ", module_name, "(\n",
        "  input ", paste(vars, collapse = ", "), ",\n",
        "  output reg out\n",
        ");\n\n",
        "always @(*) begin\n"
      )
      
      for (i in 1:nrow(truth_table)) {
        condition <- paste(sapply(1:length(vars), function(j) {
          val <- truth_table[i, j]
          if (val == 1) vars[j] else paste0("!", vars[j])
        }), collapse = " && ")
        
        verilog_code <- paste0(
          verilog_code,
          "  if (", condition, ") out = ", truth_table$Output[i], ";\n"
        )
      }
      
      verilog_code <- paste0(verilog_code, "end\n\nendmodule\n")
      return(verilog_code)
    },
    
    visualize_circuit = function() {
      if (nrow(truth_table) == 0) {
        generate_truth_table()
      }
      
      cat("\n=== Boolean Logic Circuit Analysis ===\n\n")
      cat("Expression:", expression, "\n")
      cat("Variables:", paste(variables, collapse = ", "), "\n\n")
      
      cat("Truth Table:\n")
      print(truth_table)
      cat("\n")
      
      if (length(kmap) > 0) {
        cat("Karnaugh Map:\n")
        print(kmap)
        cat("\n")
      }
      
      minimized <- minimize_expression()
      cat("Minimized Expression:", minimized, "\n")
    }
  )
)

# XOR Gate Implementation
xor_gate <- function(a, b) {
  return(as.integer((a | b) & !(a & b)))
}

# Complete Gate Library
gate_library <- list(
  AND = function(...) as.integer(all(unlist(list(...)))),
  OR = function(...) as.integer(any(unlist(list(...)))),
  NOT = function(x) as.integer(!x),
  NAND = function(...) as.integer(!all(unlist(list(...)))),
  NOR = function(...) as.integer(!any(unlist(list(...)))),
  XOR = xor_gate,
  XNOR = function(a, b) as.integer(!xor_gate(a, b))
)

# Statistical Analysis of Boolean Functions
analyze_boolean_function <- function(truth_table) {
  output <- truth_table$Output
  
  analysis <- list(
    ones_count = sum(output == 1, na.rm = TRUE),
    zeros_count = sum(output == 0, na.rm = TRUE),
    ones_ratio = mean(output == 1, na.rm = TRUE),
    complexity = length(unique(output)),
    balanced = abs(sum(output == 1) - sum(output == 0)) <= 1
  )
  
  return(analysis)
}

# Example Usage and Testing
main <- function() {
  cat("Boolean Logic Circuit Designer - R Implementation\n")
  cat("=================================================\n\n")
  
  # Example 1: Simple AND gate
  circuit1 <- BooleanCircuit$new("A*B")
  circuit1$generate_truth_table()
  circuit1$generate_karnaugh_map()
  circuit1$visualize_circuit()
  
  cat("\n\n")
  
  # Example 2: XOR gate
  circuit2 <- BooleanCircuit$new("A*B' + A'*B")
  circuit2$generate_truth_table()
  circuit2$generate_karnaugh_map()
  cat("XOR Circuit Analysis:\n")
  circuit2$visualize_circuit()
  
  # Statistical analysis
  cat("\n\nStatistical Analysis:\n")
  stats <- analyze_boolean_function(circuit2$truth_table)
  print(stats)
  
  # Verilog export
  cat("\n\nVerilog Code:\n")
  cat(circuit2$export_to_verilog("xor_gate"))
}

# Run main if script is executed directly
if (!interactive()) {
  main()
}