# ============================================================================
# Manual tests: Slack snitch (push_slack_snitch) + Gmail mail merge
#
# Run section by section in RStudio (Cmd+Enter / Ctrl+Enter), from the project
# root. Each section says what you should see. Sections are ordered from safe
# (no network) to live (posts / sends) — nothing live happens until Part A4 /
# Part B4, and both only target YOU.
#
#   Part A  Slack
#     A1  Formatting helpers          offline, assertions
#     A2  Synthetic snitch dry run    offline, assertions + preview file
#     A3  Real-data dry run           offline (uses latest .RData snapshot)
#     A4  One live post               posts ONE message to your test channel
#     A5  Error paths                 hits Slack API, posts nothing
#   Part B  Mail merge
#     B1  Helpers                     offline, assertions
#     B2  Dry run                     reads your test Google Doc only
#     B3  Body preview                offline, writes merged HTML to open locally
#     B4  Live send to yourself       sends to TEST_EMAIL only
#     B5  Failure handling            one bad row, one good row
# ============================================================================

# ---- 0. Config — fill these in --------------------------------------------
TEST_SLACK_CHANNEL <- "C0XXXXXXXXX"   # ID of a private test channel (bot invited), or "#your-test-channel"
MY_SLACK_ID        <- "U0XXXXXXXXX"   # your Slack member ID (Profile > ... > Copy member ID)
TEST_EMAIL         <- "matt.benigni@immuta.com"
TEST_TEMPLATE_DOC  <- "https://docs.google.com/document/d/XXXXXXXX/edit"  # see B2 for what to put in it

devtools::load_all()   # note: needs gmailr installed — R/gmail_mail_merge.R calls library(gmailr)

tmp <- file.path(tempdir(), "immuter_manual_tests")
dir.create(tmp, showWarnings = FALSE)
ok <- function(cond, label) { cat(if (isTRUE(cond)) "  PASS " else "  FAIL ", label, "\n", sep = ""); invisible(cond) }


# ============================================================================
# PART A — SLACK
# ============================================================================

# ---- A1. Formatting helpers (offline) --------------------------------------
cat("\nA1 helpers\n")
ok(.slack_escape("A&B <x>") == "A&amp;B &lt;x&gt;",                         "escape & < >")
ok(.md_to_slack("**bold**") == "*bold*",                                   "bold -> *bold*")
ok(.md_to_slack("[SF](https://x.com/a)") == "<https://x.com/a|SF>",        "md link -> slack link")
ok(.md_to_slack("## Head") == "*Head*",                                    "heading -> bold")
ok(.md_to_slack("- item") == "• item",                                "bullet -> •")
ok(.md_to_slack("```\n<owner> **x**\n```\nafter") == "```\n&lt;owner&gt; **x**\n```\nafter",
   "code block: escaped, not formatted")
ok(.md_to_slack("```\ncode\n```") == "```\ncode\n```", "trailing code block keeps closing fence")


# ---- A2. Synthetic snitch dry run (offline) --------------------------------
# Four fake accounts covering every branch:
#   Alpha & Co  : renewals + SaaS + QBR, CSM has a Slack ID, special chars in names
#   Beta        : renewals only, "No next step" + "No MM/DD date" issues, CSM has NO Slack ID
#   Gamma       : QBR only, never had a review (NA)
#   Delta       : renewals, but no Slack channel -> must be skipped
cat("\nA2 synthetic dry run\n")
as_of <- as.Date("2026-10-08")

fake_ns <- data.frame(
  `Customer Success Manager` = c("Test CSM", "No ID CSM", "No ID CSM", "Test CSM"),
  `Account Name`      = c("Alpha & Co", "Beta", "Beta", "Delta"),
  `Opportunity Name`  = c("Alpha <Renewal> FY27", "Beta Renewal", "Beta Add-on", "Delta Renewal"),
  `Churn Quarter`     = "Q4-2027",
  `Account Risk`      = c("At Risk", "Expecting to Renew", "Expecting to Renew", "At Risk"),
  Amount              = c(250000, 90000, NA, 50000),
  `Next Step Date`    = as.Date(c("2026-09-20", NA, NA, "2026-09-01")),
  `Days Since Update` = c(18L, NA, NA, 37L),
  `Allowed Days`      = c(7L, 14L, 14L, 7L),
  Issue               = c("Stale", "No next step", "No MM/DD date at start", "Stale"),
  opportunity_url     = c("https://immuta.lightning.force.com/lightning/r/Opportunity/006TEST/view", NA, "", NA),
  check.names = FALSE, stringsAsFactors = FALSE)

fake_saas <- data.frame(
  `Customer Success Manager` = "Test CSM", `Account Name` = "Alpha & Co",
  `Estimated SaaS Migration Period` = "Q1-2028", `SaaS Migration Pipeline Stage` = "Scoping",
  `Notes Date` = as.Date("2026-09-10"), `Days Since Update` = 28L, `Allowed Days` = 14L,
  Issue = "Stale", check.names = FALSE, stringsAsFactors = FALSE)

fake_qbr <- data.frame(
  `Customer Success Manager` = c("Test CSM", "Test CSM"), `Account Name` = c("Alpha & Co", "Gamma"),
  `Last Review` = as.Date(c("2026-03-01", NA)), `Days Since Review` = c(221L, NA),
  `Next QBR` = as.Date(c("2026-11-15", NA)), `Next EBR` = as.Date(c(NA, NA)),
  check.names = FALSE, stringsAsFactors = FALSE)

fake_snitch <- list(next_steps = fake_ns, saas_notes = fake_saas, no_recent_qbr = fake_qbr)
attr(fake_snitch, "as_of") <- as_of

fake_op_list <- list(accounts = data.frame(
  `Account Name`   = c("Alpha & Co", "Beta", "Gamma", "Delta"),
  slack_channel    = c("test-alpha", "test-beta", "test-gamma", NA),
  slack_channel_id = c("C000ALPHA", NA, "", NA),     # Beta/Gamma fall back to the name
  check.names = FALSE, stringsAsFactors = FALSE))

fake_users <- file.path(tmp, "slack_users_test.csv")
write.csv(data.frame(`Customer Success Manager` = c("Test CSM", "No ID CSM"),
                     slack_user_id = c("U0TESTCSM1", ""), check.names = FALSE),
          fake_users, row.names = FALSE)

a2_preview <- file.path(tmp, "snitch_preview_synthetic.md")
a2 <- push_slack_snitch(fake_snitch, fake_op_list, dry_run = TRUE, include_qbr = TRUE,
                        users_path = fake_users, preview_path = a2_preview)
# Expect 2 warnings: "No Slack channel for: Delta" and "No Slack user ID for CSM(s) No ID CSM"

msg <- setNames(a2$message, a2$`Account Name`)
st  <- setNames(a2$status,  a2$`Account Name`)
ok(nrow(a2) == 4,                                               "4 accounts flagged")
ok(st[["Delta"]] == "skipped: no channel",                      "Delta skipped (no channel)")
ok(all(st[c("Alpha & Co", "Beta", "Gamma")] == "dry run"),      "others = dry run")
ok(a2$channel[a2$`Account Name` == "Beta"]  == "test-beta",     "falls back to channel name when ID is NA")
ok(a2$channel[a2$`Account Name` == "Gamma"] == "test-gamma",    "falls back to channel name when ID is blank")
ok(grepl("<@U0TESTCSM1>", msg[["Alpha & Co"]], fixed = TRUE),   "CSM @-mention uses Slack ID")
ok(grepl("^No ID CSM", msg[["Beta"]]),                          "CSM w/o ID shown as plain name")
ok(grepl("Alpha &amp; Co", msg[["Alpha & Co"]], fixed = TRUE),  "account name escaped")
ok(grepl("Alpha &lt;Renewal&gt; FY27", msg[["Alpha & Co"]], fixed = TRUE), "opp name escaped")
ok(grepl("<https://immuta.lightning.force.com/lightning/r/Opportunity/006TEST/view|", msg[["Alpha & Co"]], fixed = TRUE),
   "opp linked when URL present")
ok(all(sapply(c("Renewal Next Steps", "SaaS migration notes", "QBR/EBR overdue"), grepl, msg[["Alpha & Co"]])),
   "Alpha has all three blocks")
ok(grepl("next scheduled 11/15/2026", msg[["Alpha & Co"]], fixed = TRUE), "QBR next-scheduled date shown")
ok(!grepl("SaaS migration|QBR/EBR", msg[["Beta"]]),             "Beta has renewals block only")
ok(grepl("no Next Step entered", msg[["Beta"]]) && grepl("doesn't start with an MM/DD", msg[["Beta"]]),
   "Beta issue wording")
ok(grepl("no QBR or EBR on record", msg[["Gamma"]]) && !grepl("Renewal Next Steps", msg[["Gamma"]]),
   "Gamma QBR-only, NA last review")
ok(!any(grepl("<!--|\\{\\{", a2$message)),                      "no leftover comments / {{placeholders}}")
ok(!any(grepl("\\*\\*", gsub("(?s)```.*?```", "", a2$message, perl = TRUE))),
   "no leftover **markdown bold** outside code blocks")

# Eyeball: the Markdown preview, and the raw Slack text for one account
file.edit(a2_preview)
cat(msg[["Alpha & Co"]])
# Paste that text into https://app.slack.com/block-kit-builder (as a "text" section,
# mrkdwn) to see roughly how it'll render without posting anything.

# Filters
a2b <- push_slack_snitch(fake_snitch, fake_op_list, accounts = "Gamma", include_qbr = TRUE,
                         users_path = fake_users, preview_path = file.path(tmp, "p2.md"))
ok(identical(a2b$`Account Name`, "Gamma"),                         "accounts= filter")
a2c <- suppressWarnings(push_slack_snitch(fake_snitch, fake_op_list, include_qbr = FALSE, include_saas = FALSE,
                         users_path = fake_users, preview_path = file.path(tmp, "p3.md")))
ok(!"Gamma" %in% a2c$`Account Name` && !any(a2c$qbr) && !any(a2c$saas), "include_qbr/include_saas = FALSE")
a2d <- suppressWarnings(push_slack_snitch(fake_snitch, fake_op_list, users_path = fake_users,
                                          preview_path = file.path(tmp, "p4.md")))
ok(!"Gamma" %in% a2d$`Account Name` && !any(a2d$qbr) && any(a2d$saas), "default: Next Steps + SaaS only, no QBR")


# ---- A3. Real-data dry run (offline) ---------------------------------------
cat("\nA3 real-data dry run\n")
snap <- max(list.files("data", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$", full.names = TRUE))
load(snap)   # -> op_list
message("Using snapshot ", snap)

snitch <- report_next_step_snitch(op_list)
real   <- push_slack_snitch(snitch, op_list)     # writes output/snitch_slack_preview.md
table(real$status)
# Check:
#   - "skipped: no channel" list in the warning = rows still missing in slack_channels.csv
#   - "No Slack user ID" warning = CSMs still missing in slack_users.csv
#   - open the preview and read 3-4 messages end to end
file.edit("output/snitch_slack_preview.md")


# ---- A4. ONE live post to your test channel --------------------------------
# Safety: works on a COPY of op_list where every account's channel is wiped
# except one, which points at TEST_SLACK_CHANNEL; and a users CSV where that
# account's CSM maps to YOUR Slack ID — so the only ping is you.
cat("\nA4 live post\n")
stopifnot(nzchar(Sys.getenv("SLACK_BOT_TOKEN")), !grepl("XXXX", TEST_SLACK_CHANNEL), !grepl("XXXX", MY_SLACK_ID))

test_acct <- real$`Account Name`[which.max(real$renewals + real$saas + real$qbr)]  # richest message
test_csm  <- real$`Customer Success Manager`[real$`Account Name` == test_acct]
message("Test account: ", test_acct, " (CSM ", test_csm, ")")

op_test <- op_list
op_test$accounts$slack_channel    <- NA_character_
op_test$accounts$slack_channel_id <- NA_character_
op_test$accounts$slack_channel_id[trimws(op_test$accounts$`Account Name`) == trimws(test_acct)] <- TEST_SLACK_CHANNEL

me_users <- file.path(tmp, "slack_users_me.csv")
write.csv(data.frame(`Customer Success Manager` = test_csm, slack_user_id = MY_SLACK_ID, check.names = FALSE),
          me_users, row.names = FALSE)

a4 <- push_slack_snitch(snitch, op_test, accounts = test_acct, dry_run = FALSE,
                        users_path = me_users, preview_path = file.path(tmp, "p_live.md"),
                        confirm = TRUE)          # answer y at the prompt
a4[, c("Account Name", "channel", "status", "ts")]
ok(a4$status == "sent" && !is.na(a4$ts), "posted, got a message ts")
# In Slack, check: you got pinged; bold/bullets/links render; the Next Step
# format code block shows literally (with <...> intact); report links work;
# no link previews unfurled.


# ---- A5. Error paths (nothing should post) ---------------------------------
cat("\nA5 error paths\n")
a5a <- suppressWarnings(push_slack_snitch(snitch, op_test, accounts = test_acct, dry_run = FALSE,
                        users_path = me_users, preview_path = file.path(tmp, "p_e1.md"),
                        token = "xoxb-not-a-real-token", confirm = FALSE))
ok(grepl("invalid_auth", a5a$status), paste("bad token ->", a5a$status))

op_bad <- op_test
op_bad$accounts$slack_channel_id[op_bad$accounts$slack_channel_id %in% TEST_SLACK_CHANNEL] <- "C00000NOPE"
a5b <- suppressWarnings(push_slack_snitch(snitch, op_bad, accounts = test_acct, dry_run = FALSE,
                        users_path = me_users, preview_path = file.path(tmp, "p_e2.md"), confirm = FALSE))
ok(grepl("channel_not_found", a5b$status), paste("bad channel ->", a5b$status))

ok(inherits(try(push_slack_snitch(snitch, op_test, accounts = test_acct, dry_run = FALSE,
                                  users_path = me_users, preview_path = file.path(tmp, "p_e0.md"),
                                  token = "", confirm = FALSE), silent = TRUE), "try-error"),
   "empty token -> stop()")

# Cancel path: answer N (or just Enter) at the prompt
a5c <- push_slack_snitch(snitch, op_test, accounts = test_acct, dry_run = FALSE,
                         users_path = me_users, preview_path = file.path(tmp, "p_e3.md"), confirm = TRUE)
ok(a5c$status == "cancelled", "N at prompt -> cancelled")

# Optional: private channel the bot is NOT in -> expect "error: not_in_channel"/"channel_not_found"


# ============================================================================
# PART B — GMAIL MAIL MERGE
# ============================================================================
# One-time auth (if not already cached):
#   gmailr::gm_auth_configure(path = "path/to/client_secret.json")
#   gmailr::gm_auth(email = TEST_EMAIL, scopes = "send")
#   googledrive::drive_auth(email = TEST_EMAIL)

# ---- B1. Helpers (offline) -------------------------------------------------
cat("\nB1 helpers\n")
ok(identical(split_field("a@x.com; b@x.com,c@x.com"), c("a@x.com", "b@x.com", "c@x.com")), "split ; and ,")
ok(identical(split_field(" a@x.com ;; "), "a@x.com"),           "trims, drops empties")
ok(length(split_field(NA)) == 0 && length(split_field("")) == 0, "NA / empty -> character(0)")
ok(identical(split_field(list(c("a", "b"))), c("a", "b")),     "list-column")
ok(tenants_to_html_bullets(c("Acme", "R&D <EU>")) == "<ul><li>Acme</li><li>R&amp;D &lt;EU&gt;</li></ul>",
   "bullets + escaping")
ok(tenants_to_html_bullets(character(0)) == "",                "no tenants -> empty string")


# ---- B2. Dry run against your test Doc -------------------------------------
# Make a Google Doc (TEST_TEMPLATE_DOC) containing, e.g.:
#
#     Hi team,
#     The following tenants are affected:
#     {{affected_tenants}}
#     Reference: {{ticket}}
#     Thanks
#
# Type the {{tokens}} in one go — if Docs autocorrect/spellcheck splits a token
# across formatting runs it won't be replaced (that's one of the things this checks).
cat("\nB2 dry run\n")
stopifnot(!grepl("XXXX", TEST_TEMPLATE_DOC))

rows <- data.frame(
  to               = c(TEST_EMAIL,
                       paste0(TEST_EMAIL, "; ", sub("@", "+merge2@", TEST_EMAIL)),  # 2 recipients, ; delimited
                       ""),                                                           # no recipient -> skipped
  cc               = c(NA, sub("@", "+cc@", TEST_EMAIL), NA),
  subject          = c("[TEST 1] Incident update: {{affected_tenants}}",
                       "[TEST 2] {{ticket}} - multi-recipient",
                       "[TEST 3] should be skipped"),
  affected_tenants = c("Acme Corp; Globex", "R&D <EU>, Initech", "Nobody"),
  ticket           = c("INC-001", "INC-002", "INC-003"),
  stringsAsFactors = FALSE)

b2 <- gmail_mail_merge(rows, TEST_TEMPLATE_DOC, dry_run = TRUE)
b2
ok(identical(b2$status, c("dry_run", "dry_run", "failed")), "statuses: dry_run, dry_run, skipped(failed)")
ok(grepl("missing 'to'", b2$message[3]),                    "row 3 skipped for missing to")


# ---- B3. Body preview (offline, after fetching the Doc once) ---------------
# The dry run doesn't expose the merged body, so rebuild it the same way the
# function does and open it in a browser.
cat("\nB3 body preview\n")
tpl <- fetch_doc_template_html(TEST_TEMPLATE_DOC)
ok(grepl("{{affected_tenants}}", tpl, fixed = TRUE), "token survives Doc -> HTML export intact")
ok(grepl("{{ticket}}", tpl, fixed = TRUE),           "{{ticket}} token intact")

body <- gsub("{{affected_tenants}}", tenants_to_html_bullets(split_field(rows$affected_tenants[2])), tpl, fixed = TRUE)
body <- gsub("{{ticket}}", rows$ticket[2], body, fixed = TRUE)
ok(!grepl("\\{\\{", body), "no unreplaced {{tokens}} in body")
b3_file <- file.path(tmp, "merge_body_row2.html"); writeLines(body, b3_file); browseURL(b3_file)
# Note: Docs exports bullets inside <p> tags; check the list doesn't render
# with odd spacing.


# ---- B4. Live send to yourself ---------------------------------------------
cat("\nB4 live send\n")
b4 <- gmail_mail_merge(rows, TEST_TEMPLATE_DOC, dry_run = FALSE, delay_seconds = 1)
b4
ok(identical(b4$status, c("sent", "sent", "failed")), "2 sent, 1 skipped")
# In Gmail (inbox + Sent), check:
#   [TEST 1] subject reads "Incident update: Acme Corp; Globex"
#   [TEST 2] went to both addresses, cc'd +cc, subject "INC-002 - multi-recipient"
#   body: tenant list is a real bulleted list; "R&D <EU>" shows literally
#   body: {{ticket}} filled; Doc fonts/headings look acceptable in Gmail
#   [TEST 3] did NOT arrive


# ---- B5. Failure handling --------------------------------------------------
cat("\nB5 failure handling\n")
bad <- data.frame(to = c("not-an-email", TEST_EMAIL), subject = c("[TEST 5a] bad", "[TEST 5b] good"),
                  affected_tenants = "X", stringsAsFactors = FALSE)
b5 <- gmail_mail_merge(bad, TEST_TEMPLATE_DOC)
b5
ok(b5$status[1] == "failed" && b5$status[2] == "sent", "bad row fails, merge continues")
ok(inherits(try(gmail_mail_merge(bad, TEST_TEMPLATE_DOC, stop_on_error = TRUE), silent = TRUE), "try-error"),
   "stop_on_error = TRUE stops at first failure")

cat("\nDone. Temp files in ", tmp, "\n")
