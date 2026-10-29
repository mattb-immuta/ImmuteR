# Agentic Engagement Overview: what changed since the comparison snapshot.
#
# Called from enrich_list_object() as changes$agentic. Both snapshots are rolled
# up one row per account with the same .ao_build() the page uses (churned
# accounts excluded, as the page does), so "added" / "removed" are accounts that
# entered or left the Coverage tab, and field_changes are accounts whose agentic
# fields moved. `prev` is the previous snapshot's rollup, trimmed to what the
# page needs to recompute every roll-up number for the comparison date; the page
# (R/agentic_overview_assets.R) shows the deltas next to the current numbers.
#
# NULL when either snapshot has no agentic_profile (a snapshot from before the
# report was pulled), so the first run after it was added shows no phantom
# "everything added".

# Fields compared, with the labels the page shows.
.AGX_DIFF_FIELDS <- c(owner = "Agentic owner", title = "Title", vision = "Customer Agentic Vision",
                      architecture = "Agentic Architecture", date = "Conversation date",
                      status = "Conversation status", eval = "Evaluation status",
                      deploy = "Deployment status", next_steps = "Next steps",
                      evaluating = "Evaluating", pov = "PoV status")

.diff_agentic <- function(new_list, old_list, diff_date = NA) {
  if (!is.data.frame(new_list$agentic_profile) || !nrow(new_list$agentic_profile) ||
      !is.data.frame(old_list$agentic_profile) || !nrow(old_list$agentic_profile)) return(NULL)

  nb <- tryCatch(.ao_build(new_list$agentic_profile, op_list = new_list)$data, error = function(e) NULL)
  ob <- tryCatch(.ao_build(old_list$agentic_profile, op_list = old_list)$data, error = function(e) NULL)
  if (is.null(nb) || is.null(ob)) return(NULL)

  key <- function(d) tolower(trimws(ifelse(is.na(d$account), d$name, d$account)))
  nb$key <- key(nb)
  ob$key <- key(ob)

  # PoV status per account ("" = no PoV). NA when that snapshot has no PoV data
  # at all, so a snapshot from before the PoV pull doesn't read as "PoV gained".
  pov_map <- function(p) {
    if (!is.data.frame(p) || !"matched" %in% names(p)) return(NULL)
    p <- p[!is.na(p$matched) & p$matched, , drop = FALSE]
    if (!nrow(p)) return(stats::setNames(character(0), character(0)))
    st <- ifelse(is.na(p$status), "No status", p$status)
    s  <- tapply(st, tolower(trimws(p$account)), function(x) paste(sort(unique(x)), collapse = ", "))
    stats::setNames(as.character(s), names(s))
  }
  np <- pov_map(new_list$agentic_pov)
  op <- pov_map(old_list$agentic_pov)
  nb$pov <- if (is.null(np)) NA_character_ else ifelse(is.na(np[nb$key]), "", unname(np[nb$key]))
  ob$pov <- if (is.null(op)) NA_character_ else ifelse(is.na(op[ob$key]), "", unname(op[ob$key]))

  norm <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  ident <- function(d) data.frame(name = d$name, csm = d$csm, region = d$region,
                                  arr = if ("arr" %in% names(d)) d$arr else NA_real_,
                                  stringsAsFactors = FALSE)

  added   <- ident(nb[!nb$key %in% ob$key, , drop = FALSE])
  removed <- ident(ob[!ob$key %in% nb$key, , drop = FALSE])

  # One row per account whose agentic fields changed: `changed` lists the field
  # labels; short fields carry <field>.old / <field>.new for the page to show.
  m <- match(nb$key, ob$key)
  rows <- list()
  for (i in which(!is.na(m))) {
    j <- m[i]
    diffs <- vapply(names(.AGX_DIFF_FIELDS), function(f) {
      if (!f %in% names(nb) || !f %in% names(ob)) return(FALSE)
      a <- nb[[f]][i]; b <- ob[[f]][j]
      if (f == "pov" && (is.na(a) || is.na(b))) return(FALSE)
      norm(a) != norm(b)
    }, logical(1))
    if (!any(diffs)) next
    r <- data.frame(name = nb$name[i], csm = nb$csm[i], region = nb$region[i],
                    changed = paste(.AGX_DIFF_FIELDS[diffs], collapse = ", "), stringsAsFactors = FALSE)
    for (f in c("owner", "date", "status", "eval", "deploy", "evaluating", "pov")) {
      r[[paste0(f, ".old")]] <- if (f %in% names(ob)) norm(ob[[f]][j]) else ""
      r[[paste0(f, ".new")]] <- if (f %in% names(nb)) norm(nb[[f]][i]) else ""
    }
    rows[[length(rows) + 1]] <- r
  }
  field_changes <- if (length(rows)) do.call(rbind, rows) else
    data.frame(name = character(0), csm = character(0), region = character(0), changed = character(0),
               stringsAsFactors = FALSE)

  # The previous rollup, trimmed: long text only needs to say whether it was
  # filled in (and whether it started "TBD"), so it is cut to 40 characters.
  keep <- intersect(c("name", "account", "csm", "region", "arr", "owner", "title", "vision", "architecture",
                      "status", "date", "eval", "deploy", "next_steps", "evaluating"), names(ob))
  prev <- ob[, keep, drop = FALSE]
  for (f in intersect(c("vision", "architecture", "next_steps", "title", "owner"), names(prev))) {
    prev[[f]] <- ifelse(is.na(prev[[f]]), NA_character_, substr(prev[[f]], 1, 40))
  }
  prev_povs <- if (is.data.frame(old_list$agentic_pov) && "matched" %in% names(old_list$agentic_pov)) {
    p <- old_list$agentic_pov
    p[!is.na(p$matched) & p$matched, intersect(c("account", "status", "expected_end"), names(p)), drop = FALSE]
  } else NULL

  message(".. agentic engagement: ", nrow(added), " added, ", nrow(removed), " removed, ",
          nrow(field_changes), " account(s) changed")
  list(prev_date     = if (is.na(diff_date)) NA_character_ else format(as.Date(diff_date), "%Y-%m-%d"),
       added         = added,
       removed       = removed,
       field_changes = field_changes,
       prev          = prev,
       prev_povs     = prev_povs)
}
