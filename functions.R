# doorsign
# functions

# fn_version
fn_version <- function() {
  return("v2.0")
}

# validation
fn_validate <- function(input, message1, message2, message3) {
  if (missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing", message1))
  if (gcheck == 1) {
    m1 <- sub("Argument ", "", message1)
    m1 <- sub(" missing.", "", m1)
  }

  if (all(is.null(input))) {
    if (missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if (all(is.na(input))) {
      if (missing(message2)) {
        if (gcheck == 1) message2 <- paste0("Argument ", m1, " is NA.", sep = "")
        if (gcheck != 1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if (all(nchar(input) == 0)) {
      if (missing(message3)) {
        if (gcheck == 1) message3 <- paste0("Argument ", m1, " is empty.", sep = "")
        if (gcheck != 1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

txt_content <- c("
**Bioinformatician, NBIS** \\
Dept. of Cell and Molecular Biology (ICM) \\
Uppsala University \\
john.doe@nbis.se \\
0862634824
")