## De-identify the conjoint choice data for public release.
## Input: the authors' raw merged conjoint files (NOT distributed; local, read-only).
## Output: data/microdata/conjoint_{kr,de,tw}.csv — respondent id replaced by a
## sequential integer (rid), only the modelling columns kept (the eight randomized
## attributes, the profile/task index, and the choice outcome). No direct identifiers
## and no demographic covariates are written. This is the file 02_structural_brms.R
## re-fits to reproduce the structural utility weights.

suppressMessages({library(readr); library(dplyr)})
IFES <- file.path(path.expand("~"), "Documents/github/research/projects/_archive/IFES2023")
NIDX <- file.path(path.expand("~"), "Documents/github/research/projects/_archive/Nat-id-experiments")
OUT  <- file.path(dirname(getwd()), "data", "microdata")   # posts/<slug>/data/microdata
if (!dir.exists(OUT)) OUT <- "posts/coethnic-cross-national/data/microdata"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

ATTR <- c("Origin","Ancestry","Residence","Language","Citizenship","Tradition","Political","Feeling")
KEEP <- c("rid","in_analytic_sample","question_profile", ATTR, "person_choice")

rd <- function(p){d<-suppressMessages(read_csv(p,locale=locale(encoding="UTF-8"),show_col_types=FALSE,name_repair="minimal"));nm<-names(d);d[,!duplicated(nm)&!is.na(nm)&nm!=""]}

cfg <- list(
  kr = list(f=file.path(NIDX,"data/ROK23/merged_df.sk.natidcbc1.csv"), id="ResponseId"),
  de = list(f=file.path(IFES,"Germans/Data/merged_german.natid.csv"),  id="id"),
  tw = list(f=file.path(NIDX,"data/ROC24/merged_df.tw.natidcbc1.csv"),  id="ResponseId")
)

## analytic-sample flag per original id (a boolean, not an identifier):
## KR = survey completers (Finished==1); DE = passed the attention check;
## TW = all respondents (the source study runs no attention filter).
sample_flag <- function(cc, uids) {
  if (cc == "tw") return(rep(TRUE, length(uids)))
  if (cc == "kr") {
    s <- rd(file.path(NIDX,"data/ROK23/AKS 2023 - ROK_February 13, 2024_12.47.csv"))
    ok <- s$ResponseId[suppressWarnings(as.integer(s$Finished)) == 1]
    return(uids %in% ok)
  }
  if (cc == "de") {  # merged id == row index into the choice_text export
    ct <- rd(file.path(IFES,"Germans/Data/choice_text_IFES+2023+-+German_October+24,+2023_13.32.csv"))
    pass <- grepl("berhaupt nicht zu", ct$Q114)               # instructed-response check
    return(pass[uids])
  }
}

for (cc in names(cfg)) {
  P <- cfg[[cc]]
  d <- rd(P$f)
  names(d)[names(d)==P$id] <- "uid"
  d$in_analytic_sample <- sample_flag(cc, d$uid)
  # stable sequential rid in order of first appearance
  d$rid <- match(d$uid, unique(d$uid))
  out <- d %>% select(any_of(KEEP)) %>% arrange(rid, question_profile)
  # guard: no original identifier survives
  stopifnot(!"uid" %in% names(out), !"ResponseId" %in% names(out), !"id" %in% names(out))
  write_csv(out, file.path(OUT, paste0("conjoint_", cc, ".csv")))
  n_keep <- length(unique(out$rid[out$in_analytic_sample]))
  cat(sprintf("%s: %d respondents (%d in analytic sample), %d rows\n",
              cc, length(unique(out$rid)), n_keep, nrow(out)))
}
cat("de-identification complete\n")
