# Composite-key detection, measured against the primary keys the three sample
# databases declare for themselves. Ground truth comes from PRAGMA table_info
# (the pk column gives each key column's position), so the labels are the
# schema authors', not ours.
root <- Sys.getenv("DBMAPS_ROOT", ".")
source(file.path(root, "R", "groundtruth.R"))
source(file.path(root, "R", "discover.R"))
source(file.path(root, "R", "metadata.R"))
suppressMessages({library(DBI); library(RSQLite); library(data.table)})

dbs <- list(chinook = "Chinook_Sqlite.sqlite", sakila = "sakila.db",
            northwind = "northwind.db")

# declared primary key of every table, in key order
declared_pks <- function(path) {
  con <- dbConnect(RSQLite::SQLite(), path); on.exit(dbDisconnect(con))
  tabs <- dbGetQuery(con, "SELECT name FROM sqlite_master
                           WHERE type='table' AND name NOT LIKE 'sqlite_%'")$name
  out <- list()
  for (t in tabs) {
    ti <- tryCatch(dbGetQuery(con, sprintf('PRAGMA table_info("%s")', t)),
                   error = function(e) NULL)
    if (is.null(ti)) next
    k <- ti[ti$pk > 0, ]
    out[[t]] <- if (nrow(k)) k$name[order(k$pk)] else character(0)
  }
  out
}

# What the module returns for a table passed on its own: single-column key
# first, composite fallback second. The FK route is skipped because with no
# parent tables present there are no foreign keys to find.
detect_alone <- function(tname, dt, max_cols = 3) {
  prof <- .profile_table(dt)
  idcol <- .pick_identifier(tname, prof)
  if (!is.na(idcol)) return(idcol)
  .find_composite_key(dt, prof, max_cols)
}

classify <- function(expected, got) {
  if (length(expected) == 0 && is.null(got))  return("correct (no key, none found)")
  if (length(expected) == 0 && !is.null(got)) return("INVENTED")
  if (is.null(got))                           return("missed")
  if (setequal(tolower(expected), tolower(got))) return("correct")
  "MISMATCH"
}

cat("=== Per-table key detection, each table passed alone ===\n")
cat(sprintf("%-10s %-24s %-30s %-30s %s\n",
            "db", "table", "declared PK", "detected", "verdict"))
cat(strrep("-", 118), "\n")

tally <- c(); comp_total <- 0; comp_correct <- 0
for (dbn in names(dbs)) {
  path <- file.path(root, "data-raw", dbs[[dbn]])
  db <- load_sqlite_db(path)
  pks <- declared_pks(path)
  for (t in names(db$data)) {
    exp <- if (is.null(pks[[t]])) character(0) else pks[[t]]
    got <- detect_alone(t, db$data[[t]])
    v <- classify(exp, got)
    tally <- c(tally, v)
    if (length(exp) > 1) {                      # a declared composite key
      comp_total <- comp_total + 1
      if (v == "correct") comp_correct <- comp_correct + 1
    }
    mark <- if (length(exp) > 1) " *" else ""
    cat(sprintf("%-10s %-24s %-30s %-30s %s%s\n", dbn, t,
                if (length(exp)) paste(exp, collapse = ", ") else "(none)",
                if (is.null(got)) "(none)" else paste(got, collapse = ", "),
                v, mark))
  }
}

cat(strrep("-", 118), "\n")
cat("* = table whose declared PK is composite\n\n")
cat("Verdict counts:\n")
for (k in names(sort(table(tally), decreasing = TRUE)))
  cat(sprintf("  %-30s %d\n", k, sum(tally == k)))
cat(sprintf("\nComposite keys recovered: %d / %d\n", comp_correct, comp_total))

# ---- the rental case: a declared three-column key that two columns satisfy ----
cat("\n=== Case study: sakila.rental, declared 3-column UNIQUE constraint ===\n")
db <- load_sqlite_db(file.path(root, "data-raw", "sakila.db"))
r <- db$data$rental
trip <- c("rental_date", "inventory_id", "customer_id")
cat(sprintf("rows: %d\n", nrow(r)))
for (cc in list(trip[1:2], trip[c(1,3)], trip[2:3], trip))
  cat(sprintf("  %-46s distinct=%6d  %s\n", paste(cc, collapse = " + "),
              nrow(unique(r, by = cc)),
              if (anyDuplicated(r, by = cc) == 0) "UNIQUE" else "has duplicates"))
cat("\nTwo of the three pairs are already unique in this data, so the declared\n",
    "three-column constraint is not minimal here. Business logic needs all three\n",
    "(a customer can rent the same item again later), but the sample data never\n",
    "exercises that, which is accidental uniqueness in the wild.\n", sep = "")

# ---- runtime: does searching triples cost anything? ----
cat("\n=== Runtime, pairs vs triples (10 junction-style tables, 5000 rows each) ===\n")
set.seed(1)
mk <- function(n) data.table(a_id = sample.int(50, n, TRUE),
                             b_id = sample.int(50, n, TRUE),
                             c_id = sample.int(50, n, TRUE),
                             d_id = sample.int(50, n, TRUE),
                             val  = runif(n))
tabs <- lapply(1:10, function(i) mk(5000))
for (mc in c(2, 3)) {
  el <- system.time(for (d in tabs) .find_composite_key(d, .profile_table(d), mc))[["elapsed"]]
  cat(sprintf("  max_key_cols=%d  %.3fs\n", mc, el))
}
