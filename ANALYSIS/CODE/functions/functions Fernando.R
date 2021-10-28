# These are Ferando's functions and are needed for summarising the life expectancy models. 

.DefineMortMatrix <- function(model = "GO", shape = "simple") {
  if (model == "EX") {
    mortfun <- function(theta, x) c(theta) * rep(1, length(x))
  } else if (model == "GO") {
    if (shape == "simple") {
      mortfun <- function(theta, x) {
        exp(theta[ ,"b0"] + theta[, "b1"] * x)
      }
    } else if (shape == "Makeham") {
      mortfun <- function(theta, x) {
        theta[, "c"] + exp(theta[, "b0"] + theta[, "b1"] * x)
      }
    } else {
      mortfun <- function(theta, x) {
        exp(theta[, "a0"] - theta[, "a1"] * x) + theta[, "c"] +
          exp(theta[, "b0"] + theta[, "b1"] * x)
      }
    }
  } else if (model == "WE") {
    if (shape == "simple") {
      mortfun <- function(theta, x) {
        theta[, "b0"] * theta[, "b1"]^theta[, "b0"] *
          x^(theta[, "b0"] - 1)
      }
    } else if (shape == "Makeham") {
      mortfun <- function(theta, x) {
        theta[, "c"] + theta[, "b0"] * theta[, "b1"]^theta[, "b0"] *
          x^(theta[, "b0"] - 1)
      }
    } else {
      mortfun <- function(theta, x) {
        exp(theta[, "a0"] - theta[, "a1"] * x) + theta[, "c"] +
          theta[, "b0"] * theta[, "b1"]^theta[, "b0"] *
          x^(theta[, "b0"] - 1)
      }
    }
  } else if (model == "LO") {
    if (shape == "simple") {
      mortfun <- function(theta, x) {
        exp(theta[, "b0"] + theta[, "b1"] * x) /
          (1 + theta[, "b2"] * exp(theta[, "b0"]) /
             theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))
      }
    } else if (shape == "Makeham") {
      mortfun <- function(theta, x) {
        theta[, "c"] + exp(theta[, "b0"] + theta[, "b1"] * x) /
          (1 + theta[, "b2"] * exp(theta[, "b0"]) /
             theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))
      }
    } else {
      mortfun <- function(theta, x) {
        exp(theta[, "a0"] - theta[, "a1"] * x) + theta[, "c"] +
          exp(theta[, "b0"] + theta[, "b1"] * x) /
          (1 + theta[, "b2"] * exp(theta[, "b0"]) /
             theta[, "b1"] * (exp(theta[, "b1"] * x) - 1))
      }
    }
  }
  return(mortfun)
}

.DefineMortNumeric <- function(model = "GO", shape = "simple") {
  if (model == "EX") {
    mortfun <- function(theta, x) c(theta) * rep(1, length(x))
  } else if (model == "GO") {
    if (shape == "simple") {
      mortfun <- function(theta, x) {
        exp(theta["b0"] + theta["b1"] * x)
      }
    } else if (shape == "Makeham") {
      mortfun <- function(theta, x) {
        theta["c"] + exp(theta["b0"] + theta["b1"] * x)
      }
    } else {
      mortfun <- function(theta, x) {
        exp(theta["a0"] - theta["a1"] * x) + theta["c"] +
          exp(theta["b0"] + theta["b1"] * x)
      }
    }
  } else if (model == "WE") {
    if (shape == "simple") {
      mortfun <- function(theta, x) {
        theta["b0"] * theta["b1"]^theta["b0"] *
          x^(theta["b0"] - 1)
      }
    } else if (shape == "Makeham") {
      mortfun <- function(theta, x) {
        theta["c"] + theta["b0"] * theta["b1"]^theta["b0"] *
          x^(theta["b0"] - 1)
      }
    } else {
      mortfun <- function(theta, x) {
        exp(theta["a0"] - theta["a1"] * x) + theta["c"] +
          theta["b0"] * theta["b1"]^theta["b0"] *
          x^(theta["b0"] - 1)
      }
    }
  } else if (model == "LO") {
    if (shape == "simple") {
      mortfun <- function(theta, x) {
        exp(theta["b0"] + theta["b1"] * x) /
          (1 + theta["b2"] * exp(theta["b0"]) /
             theta["b1"] * (exp(theta["b1"] * x) - 1))
      }
    } else if (shape == "Makeham") {
      mortfun <- function(theta, x) {
        theta["c"] + exp(theta["b0"] + theta["b1"] * x) /
          (1 + theta["b2"] * exp(theta["b0"]) /
             theta["b1"] * (exp(theta["b1"] * x) - 1))
      }
    } else {
      mortfun <- function(theta, x) {
        exp(theta["a0"] - theta["a1"] * x) + theta["c"] +
          exp(theta["b0"] + theta["b1"] * x) /
          (1 + theta["b2"] * exp(theta["b0"]) /
             theta["b1"] * (exp(theta["b1"] * x) - 1))
      }
    }
  }
  return(mortfun)
}

# b) Cummulative hazards:
.DefineCumHazMatrix <- function(model = "GO", shape = "simple") {
  if (model == "EX") {
    cumhazfun <- function(theta, x) c(theta) * x
  } else if (model == "GO") {
    if (shape == "simple") {
      cumhazfun <- function(theta, x) {
        exp(theta[, "b0"]) / theta[, "b1"] *
          (exp(theta[, "b1"] * x) - 1)
      }
    } else if (shape == "Makeham") {
      cumhazfun <- function(theta, x) {
        theta[, "c"] * x + exp(theta[, "b0"]) / theta[, "b1"] *
          (exp(theta[, "b1"] * x) - 1)
      }
    } else {
      cumhazfun <- function(theta, x) {
        exp(theta[, "a0"]) / theta[, "a1"] * (1 - exp(-theta[, "a1"] * x)) +
          theta[, "c"] * x + exp(theta[, "b0"]) / theta[, "b1"] *
          (exp(theta[, "b1"] * x) - 1)
      }
    }
  } else if (model == "WE") {
    if (shape == "simple") {
      cumhazfun <- function(theta, x) {
        (theta[, "b1"] * x)^theta[, "b0"]
      }
    } else if (shape == "Makeham") {
      cumhazfun <- function(theta, x) {
        theta[, "c"] * x + (theta[, "b1"] * x)^theta[, "b0"]
      }
    } else {
      cumhazfun <- function(theta, x) {
        exp(theta[, "a0"]) / theta[, "a1"] * (1 - exp(-theta[, "a1"] * x)) +
          theta[, "c"] * x + (theta[, "b1"] * x)^theta[, "b0"]
      }
    }
  } else if (model == "LO") {
    if (shape == "simple") {
      cumhazfun <- function(theta, x) {
        log(1 + theta[, "b2"] * exp(theta[, "b0"]) / theta[, "b1"] *
              (exp(theta[, "b1"] * x) - 1)) * (1 / theta[, "b2"])
      }
    } else if (shape == "Makeham") {
      cumhazfun <- function(theta, x) {
        theta[, "c"] * x + log(1 + theta[, "b2"] * exp(theta[, "b0"]) /
                                 theta[, "b1"] *
                                 (exp(theta[, "b1"] * x) - 1)) *
          (1 / theta[, "b2"])
      }
    } else {
      cumhazfun <- function(theta, x) {
        exp(theta[, "a0"]) / theta[, "a1"] * (1 - exp(-theta[, "a1"] * x)) +
          theta[, "c"] * x + log(1 + theta[, "b2"] *
                                   exp(theta[, "b0"]) / theta[, "b1"] *
                                   (exp(theta[, "b1"] * x) - 1)) *
          (1 / theta[, "b2"])
      }
    }
  }
  return(cumhazfun)
}

.DefineCumHazNumeric <- function(model = "GO", shape = "simple") {
  if (model == "EX") {
    cumhazfun <- function(theta, x) c(theta) * x
  } else if (model == "GO") {
    if (shape == "simple") {
      cumhazfun <- function(theta, x) {
        exp(theta["b0"]) / theta["b1"] *
          (exp(theta["b1"] * x) - 1)
      }
    } else if (shape == "Makeham") {
      cumhazfun <- function(theta, x) {
        theta["c"] * x + exp(theta["b0"]) / theta["b1"] *
          (exp(theta["b1"] * x) - 1)
      }
    } else {
      cumhazfun <- function(theta, x) {
        exp(theta["a0"]) / theta["a1"] * (1 - exp(-theta["a1"] * x)) +
          theta["c"] * x + exp(theta["b0"]) / theta["b1"] *
          (exp(theta["b1"] * x) - 1)
      }
    }
  } else if (model == "WE") {
    if (shape == "simple") {
      cumhazfun <- function(theta, x) {
        (theta["b1"] * x)^theta["b0"]
      }
    } else if (shape == "Makeham") {
      cumhazfun <- function(theta, x) {
        theta["c"] * x + (theta["b1"] * x)^theta["b0"]
      }
    } else {
      cumhazfun <- function(theta, x) {
        exp(theta["a0"]) / theta["a1"] * (1 - exp(-theta["a1"] * x)) +
          theta["c"] * x + (theta["b1"] * x)^theta["b0"]
      }
    }
  } else if (model == "LO") {
    if (shape == "simple") {
      cumhazfun <- function(theta, x) {
        log(1 + theta["b2"] * exp(theta["b0"]) / theta["b1"] *
              (exp(theta["b1"] * x) - 1)) * (1 / theta["b2"])
      }
    } else if (shape == "Makeham") {
      cumhazfun <- function(theta, x) {
        theta["c"] * x + log(1 + theta["b2"] * exp(theta["b0"]) /
                               theta["b1"] *
                               (exp(theta["b1"] * x) - 1)) *
          (1 / theta["b2"])
      }
    } else {
      cumhazfun <- function(theta, x) {
        exp(theta["a0"]) / theta["a1"] * (1 - exp(-theta["a1"] * x)) +
          theta["c"] * x + log(1 + theta["b2"] *
                                 exp(theta["b0"]) / theta["b1"] *
                                 (exp(theta["b1"] * x) - 1)) *
          (1 / theta["b2"])
      }
    }
  }
  return(cumhazfun)
}
