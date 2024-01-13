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

fn_validate_range <- function(value, min, max, label = NULL) {
  if (!is.numeric(value)) {
    return(paste0("The input '", label, "' must be a number."))
  } else {
    if (value < min || value > max) {
      return(paste0("The input '", label, "' must be between ", min, " and ", max, "."))
    } else {
      NULL
    }
  }
}

# validate image input
fn_validate_im <- function(x) {
  if (!is.null(x)) {
    y <- tolower(sub("^.+[.]", "", basename(x$datapath)))
    if (!y %in% c("jpg", "png", "jpeg", "gif")) {
      return("Image must be one of JPG/JPEG, PNG or GIF formats.")
    }
    if ((x$size / 1024 / 1024) > 1) {
      return("Image must be less than 1MB in size.")
    }
  }
}

sample_data_1 <- list(
  "person-1" = list(
    "name" = "Ingrid Bergqvist",
    "content" =
      "**Manager, NBIS** \\
Dept. of Cell and Molecular Biology (ICM) \\
Uppsala University \\
ingrid.bergqvist@nbis.se \\
0862634824"
  )
)

sample_data_5 <- list(
  "person-1" = list(
    "name" = "Jimmy Bodin",
    "content" =
      "**Systems Developer, NBIS** \\
Dept. of Cell and Molecular Biology (ICM) \\
jimmy.bodin@nbis.se"
  ),
  "person-2" = list(
    "name" = "Sonja Andersson",
    "content" =
      "**Bioinformatician, NBIS** \\
Dept. of Med. Biochem & Microb (IMBIM) \\
sonja.andersson@nbis.se"
  ),
  "person-3" = list(
    "name" = "Fredrik Holmberg",
    "content" =
      "**Bioinformatician, NBIS** \\
Dept. of Med. Biochem & Microb (IMBIM) \\
fredrik.holmberg@nbis.se"
  ),
  "person-4" = list(
    "name" = "Ellinor Berglund",
    "content" =
      "**Bioinformatician, NBIS** \\
Dept. of Imm., Genetics & Pathology (IGP) \\
ellinor.berglund@nbis.se"
  ),
  "person-5" = list(
    "name" = "Inga Magnusson",
    "content" =
      "**Bioinformatician, NBIS** \\
Dept. of Imm., Genetics & Pathology (IGP) \\
inga.magnusson@nbis.se"
  )
)

fname <- function() {
  return(paste0("door-sign-", fn_version(), ".pdf"))
}
