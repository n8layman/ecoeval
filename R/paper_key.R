# What identifies a paper.
#
# Every record has to be attributable to a paper before anything can be
# compared, and the two sources rarely spell that attribution the same way.
# Rather than ask the user which column it is, work down a fixed priority list
# and say what was found:
#
#   1. DOI                     the only identifier in this domain that is
#                              actually unique
#   2. file name               the document handle -- also where a paper id or
#                              document id lands, since those name the file
#   3. title                   stable enough once punctuation is ignored
#   4. first author + year     two columns, and the reason a key is a vector
#                              of columns rather than one
#
# A key is normalised per role, not generically: a DOI loses its resolver
# prefix, a file name loses its directory and extension, a title loses its
# punctuation, an author list is reduced to the first surname. That is what
# makes "https://doi.org/10.1000/P01" and "10.1000/p01" the same paper.

#' Column-name patterns for each identifier role
#'
#' Matched against [squash_name()]d column names, in the order given, so an
#' exact `doi` beats an incidental `doi_source`.
#'
#' @return A named list of regular expression vectors.
#' @keywords internal
#' @noRd
role_patterns <- function() {
  list(
    doi = c("^doi$", "^dois$", "^doiurl$", "doi"),
    file = c("^filename$", "^file$", "^filepath$", "^path$", "^pdf$",
             "^documentid$", "^paperid$", "^docid$", "^document$", "^paper$",
             "^sourcefile$", "^source$", "filename", "documentid", "paperid"),
    title = c("^title$", "^papertitle$", "^documenttitle$", "title"),
    author = c("^firstauthorlastname$", "^firstauthor$", "^author$",
               "^authors$", "^authorlist$", "author"),
    year = c("^year$", "^pubyear$", "^publicationyear$", "^yearpublished$",
             "^publishedyear$", "year")
  )
}

#' The identifier strategies, in priority order
#'
#' @return A named list of `label` and the `roles` each strategy needs.
#' @keywords internal
#' @noRd
key_strategies <- function() {
  list(
    doi         = list(label = "DOI", roles = "doi"),
    filename    = list(label = "File name", roles = "file"),
    title       = list(label = "Title", roles = "title"),
    author_year = list(label = "First author + year", roles = c("author", "year"))
  )
}

#' Which column plays which identifier role
#'
#' One column per role and one role per column: a column claimed by an earlier
#' role is not offered to a later one, so a lone `paper` column is the file
#' handle rather than also standing in for the title.
#'
#' @param nm A character vector of column names.
#' @return A named list, role to column name, holding only the roles found.
#' @keywords internal
#' @noRd
detect_roles <- function(nm) {
  nm <- as.character(nm)
  sq <- squash_name(nm)
  out <- list()
  taken <- character(0)
  for (role in names(role_patterns())) {
    for (pat in role_patterns()[[role]]) {
      hit <- which(grepl(pat, sq) & !(nm %in% taken))
      if (length(hit)) {
        out[[role]] <- nm[[hit[[1L]]]]
        taken <- c(taken, nm[[hit[[1L]]]])
        break
      }
    }
  }
  out
}

#' The role a named column plays, for a key built by hand
#'
#' @param nm A character vector of column names.
#' @return A character vector of roles, `"other"` where nothing matched.
#' @keywords internal
#' @noRd
column_roles <- function(nm) {
  vapply(as.character(nm), function(one) {
    found <- detect_roles(one)
    if (length(found)) names(found)[[1L]] else "other"
  }, character(1), USE.NAMES = FALSE)
}

#' A paper key
#'
#' The columns that identify a paper, plus the role each one plays. Build one
#' from column names with `paper_key()`, or let [suggest_paper_key()] find it.
#'
#' @param columns Column names holding the identifier. More than one is
#'   allowed -- first author and year is the case that needs it.
#' @param roles Roles for those columns; detected from the names when `NULL`.
#'
#' @return An `ecoeval_paper_key`: a list with `columns`, `roles`, `strategy`,
#'   and `label`.
#' @examples
#' paper_key("doi")
#' paper_key(c("first_author", "year"))
#' @export
paper_key <- function(columns, roles = NULL) {
  columns <- as.character(columns)
  columns <- columns[!is.na(columns) & nzchar(columns)]
  if (!length(columns)) return(NULL)
  roles <- if (is.null(roles)) column_roles(columns) else as.character(roles)
  strategy <- "custom"
  label <- "Chosen by hand"
  for (nm in names(key_strategies())) {
    s <- key_strategies()[[nm]]
    if (setequal(roles, s$roles) && length(roles) == length(s$roles)) {
      strategy <- nm
      label <- s$label
      break
    }
  }
  structure(list(columns = columns, roles = roles, strategy = strategy,
                 label = label), class = "ecoeval_paper_key")
}

#' @export
format.ecoeval_paper_key <- function(x, ...) {
  sprintf("%s (%s)", x$label, paste(x$columns, collapse = " + "))
}

#' @export
print.ecoeval_paper_key <- function(x, ...) {
  cat("<paper key> ", format(x), "\n", sep = "")
  invisible(x)
}

#' Every identifier this table could be keyed on
#'
#' @param df A table.
#' @return A named list of `ecoeval_paper_key`s, best first; empty when the
#'   table carries nothing that identifies a paper.
#' @examples
#' names(paper_key_candidates(data.frame(doi = "10.1/x", title = "A", year = 2020)))
#' @export
paper_key_candidates <- function(df) {
  roles <- detect_roles(names(df))
  out <- list()
  for (nm in names(key_strategies())) {
    want <- key_strategies()[[nm]]$roles
    if (!all(want %in% names(roles))) next
    out[[nm]] <- paper_key(unlist(roles[want], use.names = FALSE), want)
  }
  out
}

#' Guess which columns identify the paper
#'
#' DOI, then file name, then title, then first author and year -- the first of
#' those the table can supply wins.
#'
#' @param df A table.
#' @return An `ecoeval_paper_key`, or `NULL` when nothing looks like one.
#' @examples
#' suggest_paper_key(data.frame(Filename = "smith.pdf", Title = "A"))
#' @export
suggest_paper_key <- function(df) {
  cand <- paper_key_candidates(df)
  if (!length(cand)) NULL else cand[[1L]]
}

#' Coerce whatever a caller supplied into a paper key
#'
#' @param x A key, column names, or `NULL` to detect from `df`.
#' @param df The table the key is for.
#' @return An `ecoeval_paper_key`, or `NULL`.
#' @keywords internal
#' @noRd
as_paper_key <- function(x, df = NULL) {
  if (inherits(x, "ecoeval_paper_key")) return(x)
  if (is.null(x) || all(is.na(x))) {
    return(if (is.null(df)) NULL else suggest_paper_key(df))
  }
  paper_key(x)
}

#' The identifier value for every row
#'
#' Each column is normalised for the role it plays, then the parts are joined
#' with `|`. A row missing any part has no key: a half-identified paper would
#' link to the wrong one, which is worse than not linking at all.
#'
#' @param df A table.
#' @param key An `ecoeval_paper_key`, column names, or `NULL` to detect one.
#' @return A character vector, `NA` where the row cannot be identified.
#' @examples
#' paper_key_values(data.frame(doi = "https://doi.org/10.1000/P01"), "doi")
#' @export
paper_key_values <- function(df, key = NULL) {
  key <- as_paper_key(key, df)
  n <- nrow(df)
  if (is.null(key) || !all(key$columns %in% names(df))) {
    return(rep(NA_character_, n))
  }
  parts <- lapply(seq_along(key$columns), function(i) {
    normalise_key_part(df[[key$columns[[i]]]], key$roles[[i]])
  })
  out <- do.call(paste, c(parts, list(sep = "|")))
  out[Reduce(`|`, lapply(parts, is.na))] <- NA_character_
  out
}

#' Normalise one part of a key for the role it plays
#'
#' @param x A column's values.
#' @param role One of `doi`, `file`, `title`, `author`, `year`, `other`.
#' @return A character vector, `NA` where blank.
#' @keywords internal
#' @noRd
normalise_key_part <- function(x, role) {
  x <- as.character(x)
  out <- switch(
    role,
    doi = strip_doi(x),
    file = strip_filename(x),
    author = first_author_surname(x),
    year = year_only(x),
    squash_text(x)
  )
  out[is_blank(out)] <- NA_character_
  out
}

#' Strip a DOI down to the identifier itself
#'
#' @param x A character vector.
#' @return A character vector.
#' @keywords internal
#' @noRd
strip_doi <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  trimws(sub("/+$", "", x))
}

#' Reduce a path to a comparable document name
#'
#' Directory and extension go; separators become spaces, so `smith_2019.pdf`
#' and `Smith 2019.PDF` are the same document.
#'
#' @param x A character vector.
#' @return A character vector.
#' @keywords internal
#' @noRd
strip_filename <- function(x) {
  x <- sub(".*[/\\\\]", "", trimws(x))
  x <- sub("\\.[[:alnum:]]{1,5}$", "", x)
  squash_text(x)
}

#' Lower-case, drop punctuation, squash whitespace
#'
#' Used for titles and for anything unrecognised: two sources punctuate a title
#' differently far more often than they word it differently.
#'
#' @param x A character vector.
#' @return A character vector.
#' @keywords internal
#' @noRd
squash_text <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

#' The four-digit year in a value
#'
#' @param x A character vector.
#' @return A character vector, `NA` where no year was found.
#' @keywords internal
#' @noRd
year_only <- function(x) {
  x <- as.character(x)
  hit <- regexpr("(1[5-9]|2[0-9])[0-9]{2}", x)
  hit[is.na(hit)] <- -1L
  out <- rep(NA_character_, length(x))
  out[hit > 0L] <- regmatches(x, hit)
  out
}

#' The first author's surname
#'
#' Handles `"Smith, J."`, `"Jane Smith"`, `"Smith et al."`, and either
#' separator between authors. Initials are dropped, so `"J. Smith"` and
#' `"Smith, J."` agree.
#'
#' @param x A character vector of author strings.
#' @return A character vector of surnames, `NA` where none could be read.
#' @keywords internal
#' @noRd
first_author_surname <- function(x) {
  x <- as.character(x)
  x <- sub("\\bet\\.?\\s*al\\.?.*$", "", x, ignore.case = TRUE)
  vapply(x, function(one) {
    if (is.na(one) || !nzchar(trimws(one))) return(NA_character_)
    one <- strsplit(one, ";")[[1L]][[1L]]
    one <- strsplit(one, "\\s+and\\s+|\\s*&\\s*")[[1L]][[1L]]
    if (grepl(",", one)) return(squash_text(strsplit(one, ",")[[1L]][[1L]]))
    words <- strsplit(squash_text(one), " ")[[1L]]
    words <- words[nchar(words) > 1L]
    if (!length(words)) NA_character_ else words[[length(words)]]
  }, character(1), USE.NAMES = FALSE)
}

#' Columns worth aligning papers on
#'
#' Paper alignment falls back to metadata when the identifiers themselves do
#' not line up, and the same role detection that finds the key finds these.
#'
#' @param df A paper table.
#' @return A named character vector, canonical role to source column, ready to
#'   hand to [prepare_papers()] as `metadata`.
#' @examples
#' paper_metadata_columns(data.frame(DOI = "x", Title = "y", Year = 2020))
#' @export
paper_metadata_columns <- function(df) {
  roles <- detect_roles(names(df))
  roles <- roles[intersect(c("title", "author", "year"), names(roles))]
  if (!length(roles)) return(character(0))
  stats::setNames(unlist(roles, use.names = FALSE), names(roles))
}
