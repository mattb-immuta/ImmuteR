# FY28 churn forecast: the model behind both reports.
#   reports/FY28_churn_forecast_report.Rmd      (HTML / Word, interactive charts)
#   reports/FY28_churn_forecast_report_pdf.Rmd  (PDF, static charts)
# Sourced from each report's `data` chunk with local = knitr::knit_global(), so it
# expects `root` (project root) and `params` (the report's YAML params) to exist,
# and dplyr + the package to be loaded (the reports' setup chunks do both).
# Creates: op_list, snap_date, quarters, plan, plan_fy, p, fc, sims, sims_fy,
# stats, fy, certain_fy, worst_q, best_q, verdict().

# ── op_list: the snapshot named in params, else the newest one ──────────────
snap <- if (nzchar(params$snapshot)) file.path(root, params$snapshot) else {
  f <- list.files(file.path(root, "data"), pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$",
                  full.names = TRUE)
  f[order(f, decreasing = TRUE)][1]
}
load(snap)   # -> op_list
snap_date <- sub("^opportunities_(.*)\\.RData$", "\\1", basename(snap))

quarters <- c("Q1-2028", "Q2-2028", "Q3-2028", "Q4-2028")
plan_tbl <- load_constants()$churn_plan
plan     <- setNames(plan_tbl$company_plan[match(quarters, plan_tbl$fiscal.period)], quarters)
plan_fy  <- sum(plan)
p        <- c(params$p_expected, params$p_at_risk)
set.seed(params$seed)

# ── Q1 / Q2: Salesforce renewals (same buckets as generate_forecast()) ──────
#   certain  : Closed Lost, Verbally Confirmed, Will Churn  -> always churns
#   expected : Expected to Churn                            -> churns with p[1]
#   at risk  : At Risk Renewal (Amount)                     -> churns with p[2]
#   anything else (Expecting to Renew, Closed Won)          -> not modelled
sf_quarter <- function(q) {
  r <- op_list$renewals %>% filter(`Churn Quarter` == q)
  amt <- suppressWarnings(as.numeric(r[[params$sf_amount_field]]))
  amt[is.na(amt)] <- 0
  arr <- suppressWarnings(as.numeric(r$Amount)); arr[is.na(arr)] <- 0
  risk <- trimws(r$`Account Risk`)
  bucket <- case_when(
    risk %in% c("Closed Lost", "Verbally Confirmed", "Will Churn") ~ "Certain churn",
    risk == "Expected to Churn"                                    ~ "Expected to churn",
    risk == "At Risk Renewal"                                      ~ "At risk",
    TRUE                                                           ~ "Not modelled"
  )
  value <- ifelse(bucket == "At risk", arr, ifelse(bucket == "Not modelled", 0, amt))
  tbl <- data.frame(Account = r$`Account Name`, `Account Risk` = risk, Amount = arr,
                    Bucket = bucket, `Modelled $` = value, check.names = FALSE)
  res <- generate_forecast_manual(
    expected_churn = value[bucket == "Expected to churn" & value > 0],
    at_risk        = value[bucket == "At risk" & value > 0],
    static         = sum(value[bucket == "Certain churn"]),
    p = p, n = params$n_sims, label = q, plot = FALSE)
  list(res = res, table = tbl, source = "Salesforce renewals")
}

# ── Q3 / Q4: hand-entered amounts (keep in sync with scripts/FY28_churn_forecast.R)
manual_quarter <- function(q, at_risk, expected) {
  tbl <- data.frame(Account = NA_character_,
                    `Account Risk` = c(rep("At Risk Renewal", length(at_risk)),
                                       rep("Expected to Churn", length(expected))),
                    Amount = c(at_risk, expected),
                    Bucket = c(rep("At risk", length(at_risk)), rep("Expected to churn", length(expected))),
                    `Modelled $` = c(at_risk, expected), check.names = FALSE)
  res <- generate_forecast_manual(expected_churn = expected, at_risk = at_risk, static = 0,
                                  p = p, n = params$n_sims, label = q, plot = FALSE)
  list(res = res, table = tbl, source = "Hand-entered amounts")
}

fc <- list(
  "Q1-2028" = sf_quarter("Q1-2028"),
  "Q2-2028" = sf_quarter("Q2-2028"),
  "Q3-2028" = manual_quarter("Q3-2028",
    at_risk  =c(1300000,267800,200000,1910256,200000,150000,353000,
         333000,1287500,150000,300000,400000),
    expected = c(0, 480000)),
  "Q4-2028" = manual_quarter("Q4-2028",
    at_risk  = c(517000,450000,550032,284677,455612,480000,
         234769,924964,250000,800000,535000,208000,960440),
    expected = c(0, 69695))
)

sims    <- lapply(fc, function(x) x$res$simulations[[1]])
sims_fy <- Reduce(`+`, sims)          # quarters are independent draws, summed per run

stat_row <- function(x, pl) {
  q <- quantile(x, c(.25, .5, .75), names = FALSE)
  data.frame(Plan = pl, Expected = mean(x), P25 = q[1], P50 = q[2], P75 = q[3],
             `Chance at or under plan` = mean(x <= pl), check.names = FALSE)
}
stats <- rbind(
  do.call(rbind, Map(stat_row, sims, plan)),
  "FY28" = stat_row(sims_fy, plan_fy)
)
stats$Period <- rownames(stats)
fy <- stats["FY28", ]
certain_fy <- sum(sapply(fc, function(x) x$res$inputs$static))
worst_q <- stats$Period[1:4][which.min(stats$`Chance at or under plan`[1:4])]
best_q  <- stats$Period[1:4][which.max(stats$`Chance at or under plan`[1:4])]
verdict <- function(pr) if (pr >= .75) "likely" else if (pr >= .5) "more likely than not" else
  if (pr >= .25) "unlikely" else "very unlikely"
