# Build duckdb/hints.duckdb (long "responses" design) from the HINTS 4 Stata files.
# Run from the project root:  Rscript scripts/build_db.R
suppressMessages({library(haven); library(dplyr); library(purrr); library(tibble)
                  library(tidyr); library(DBI); library(duckdb)})

db_path    <- file.path("duckdb", "hints.duckdb")
schema_dir <- "schema"
dir.create(schema_dir, showWarnings = FALSE)

# ---- 1. Read files (labels are Latin-1 encoded) -----------------------------
files <- list.files("stata_data", pattern = "\\.dta$", full.names = TRUE)
# Cycle id = survey + cycle, e.g. hints4_c3, hints5_c1; single-cycle files (hints6, hints7) -> hints6
survey <- sub("^(hints\\d+).*", "\\1", basename(files))
cyc_no <- ifelse(grepl("cycle", basename(files)),
                 paste0("_c", sub(".*cycle_?(\\d).*", "\\1", basename(files))), "")
names(files) <- paste0(survey, cyc_no)
stopifnot(!anyDuplicated(names(files)))
files <- files[order(names(files))]
dfs <- map(files, read_dta, encoding = "latin1")
dfs <- map(dfs, ~ { names(.x) <- tolower(names(.x)); .x })
# HINTS 5+ have no personid (hhid is unique per row) and some cycles lack formtype
dfs <- map(dfs, function(df) {
  if (!"personid" %in% names(df)) df$personid <- df$hhid
  df$personid <- as.character(df$personid)
  if (!"formtype" %in% names(df)) df$formtype <- NA_real_
  df
})

# Weights: person_finwtN normally; hints5_c3 has tg_all_/tg1_/tg2_/tg3_ weight sets
weight_re <- "finwt\\d+$"
main_wt   <- function(df) {                      # full-sample weight used in respondents
  for (nm in c("person_finwt0", "tg_all_finwt0")) if (nm %in% names(df)) return(as.numeric(df[[nm]]))
  rep(NA_real_, nrow(df))
}
core_cols <- c("personid", "hhid", "formtype")

# ---- 2. Metadata ------------------------------------------------------------
get_lab <- function(x) attr(x, "label", exact = TRUE) %||% NA_character_

variables <- imap_dfr(dfs, function(df, cyc) {
  tibble(
    cycle       = cyc,
    name        = names(df),
    label       = map_chr(df, get_lab),
    type        = map_chr(df, ~ if (is.character(.x)) "text"
                              else if (inherits(.x, "Date")) "date"
                              else if (!is.null(attr(.x, "labels", exact = TRUE))) "categorical"
                              else "numeric"),
    is_weight   = grepl(weight_re, names(df)),
    is_id       = names(df) %in% core_cols,
    pct_missing = map_dbl(df, function(x) mean(is.na(x) | (if (is.character(x)) x == "" else FALSE))),
    n_unique    = map_int(df, n_distinct)
  )
})

value_labels <- imap_dfr(dfs, function(df, cyc) {
  imap_dfr(df, function(col, nm) {
    l <- attr(col, "labels", exact = TRUE)
    if (is.null(l) || is.character(col)) return(NULL)
    tibble(cycle = cyc, variable = nm, code = as.numeric(l), label = names(l))
  })
}) |> mutate(is_missing_code = code < 0)

cycles <- tibble(
  cycle       = names(files),
  source_file = basename(files),
  n_rows      = map_int(dfs, nrow),
  n_cols      = map_int(dfs, ncol)
)

# ---- 3. Respondents and weights --------------------------------------------
respondents <- imap_dfr(dfs, function(df, cyc)
  tibble(cycle = cyc, personid = df$personid, hhid = as.character(df$hhid),
         formtype = as.numeric(df$formtype),
         person_finwt0 = main_wt(df)))

weights <- imap_dfr(dfs, function(df, cyc) {
  wcols <- grep(weight_re, names(df), value = TRUE)
  df |> select(personid, all_of(wcols)) |>
    mutate(across(all_of(wcols), as.numeric)) |>
    pivot_longer(-personid, names_to = "wt", values_to = "weight") |>
    mutate(cycle = cyc, wt_set = sub("_?finwt\\d+$", "", wt),
           wt_index = as.integer(sub(".*finwt", "", wt))) |>
    filter(!is.na(weight)) |>
    select(cycle, personid, wt_set, wt_index, weight)
})

# ---- 4. Long responses ------------------------------------------------------
# Numeric / categorical -> value_num (raw code, negatives kept); text and dates -> value_text.
# R-level NA (and empty strings) are dropped; Stata missing codes (-9, -6, ...) are kept.
long_cycle <- function(df, cyc) {
  keep   <- setdiff(names(df), c(core_cols, grep(weight_re, names(df), value = TRUE)))
  is_txt <- map_lgl(df[keep], ~ is.character(.x) || inherits(.x, "Date"))
  parts  <- list()
  num <- keep[!is_txt]
  if (length(num)) parts$num <- df |> select(personid, all_of(num)) |>
    mutate(across(all_of(num), as.numeric)) |>
    pivot_longer(-personid, names_to = "variable", values_to = "value_num",
                 values_drop_na = TRUE) |>
    mutate(value_text = NA_character_)
  txt <- keep[is_txt]
  if (length(txt)) parts$txt <- df |> select(personid, all_of(txt)) |>
    mutate(across(all_of(txt), as.character)) |>
    pivot_longer(-personid, names_to = "variable", values_to = "value_text",
                 values_drop_na = TRUE) |>
    filter(value_text != "") |>
    mutate(value_num = NA_real_)
  bind_rows(parts) |> mutate(cycle = cyc) |>
    select(cycle, personid, variable, value_num, value_text)
}

# ---- 5. Write DuckDB --------------------------------------------------------
if (file.exists(db_path)) file.remove(db_path)
con <- dbConnect(duckdb(), db_path)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

dbWriteTable(con, "cycles", cycles)
dbWriteTable(con, "variables", variables)
dbWriteTable(con, "value_labels", value_labels)
dbWriteTable(con, "respondents", respondents)
dbWriteTable(con, "replicate_weights", weights)

dbExecute(con, "CREATE TABLE responses (cycle VARCHAR, personid VARCHAR, variable VARCHAR,
                                        value_num DOUBLE, value_text VARCHAR)")
iwalk(dfs, function(df, cyc) {
  dbAppendTable(con, "responses", long_cycle(df, cyc))
  message("loaded ", cyc)
})

# Convenience view: decoded categorical answers + missing-code flag
dbExecute(con, "
CREATE VIEW responses_decoded AS
SELECT r.cycle, r.personid, r.variable, r.value_num, r.value_text,
       vl.label AS value_label, COALESCE(vl.is_missing_code, FALSE) AS is_missing_code,
       v.label AS variable_label
FROM responses r
LEFT JOIN value_labels vl ON vl.cycle = r.cycle AND vl.variable = r.variable AND vl.code = r.value_num
LEFT JOIN variables v     ON v.cycle  = r.cycle AND v.name     = r.variable")

# ---- 6. Export metadata for review, and sanity checks -----------------------
write.csv(variables,    file.path(schema_dir, "variables.csv"),    row.names = FALSE)
write.csv(value_labels, file.path(schema_dir, "value_labels.csv"), row.names = FALSE)
write.csv(cycles,       file.path(schema_dir, "cycles.csv"),       row.names = FALSE)

cat("\n== respondents per cycle (DB vs source) ==\n")
print(dbGetQuery(con, "SELECT r.cycle, COUNT(*) AS db_rows, ANY_VALUE(c.n_rows) AS source_rows
                       FROM respondents r JOIN cycles c USING (cycle) GROUP BY r.cycle ORDER BY 1"))
cat("\n== table sizes ==\n")
for (t in c("cycles", "variables", "value_labels", "respondents", "replicate_weights", "responses"))
  cat(sprintf("%-18s %s\n", t, format(dbGetQuery(con, paste("SELECT COUNT(*) n FROM", t))$n, big.mark = ",")))
cat("\n== duplicate (cycle, personid) in respondents ==\n")
print(dbGetQuery(con, "SELECT COUNT(*) AS dups FROM (SELECT cycle, personid FROM respondents
                       GROUP BY 1,2 HAVING COUNT(*) > 1)"))
cat("\n== variables whose type differs across cycles ==\n")
print(dbGetQuery(con, "SELECT name, STRING_AGG(DISTINCT type, ',') AS types FROM variables
                       GROUP BY name HAVING COUNT(DISTINCT type) > 1 ORDER BY name"))
