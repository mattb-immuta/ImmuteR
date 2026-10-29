# Dust agent: post the snitch Slack messages

R writes the messages and Dust posts them. You don't need a Slack bot token.

`output_dust_snitch()` builds the same messages as `push_slack_snitch()`. It uses
the same template (`data/input/snitch_slack_template.md`), the same channels
(`data/input/slack_channels.csv`) and the same Slack formatting. It writes them
to a JSON file. You upload that file to a Dust agent, and the agent posts each
message with Dust's Slack tool.

Dust's Slack tool uses **your own Slack login**, so the messages appear as you,
with a "Sent via … on Dust" footer. The agent can only post in channels you can
post in.

---

## 1. One-time setup in Dust

1. **Connect Slack.** Go to **Spaces → Tools → Add Tools → Slack** and complete
   the Slack sign-in. If Slack isn't in the list, a Dust admin has to add it
   under **Admin → Tools** first.
2. **Create the agent.** In **Agent Builder → Create agent**:
   - **Name:** `SnitchPoster` (any name works).
   - **Description:** "Posts the pre-written CS snitch messages from an uploaded
     JSON file to Slack."
   - **Tools:** add **Slack**. It must include *Post Message*, *List Users* and
     *Search Channels*. Add nothing else: no web search and no data sources.
   - **Instructions:** paste everything in section 4 below, from
     `## Role` to the end.
   - **Model:** any current model. The work is mechanical.
3. **Join the test channel.** Join `#tmp-cs-test`, because the agent posts as
   you.

---

## 2. Make the files (R)

```r
devtools::load_all()

snap <- max(list.files("data", pattern = "^opportunities_\\d{4}-\\d{2}-\\d{2}\\.RData$", full.names = TRUE))
load(snap)                                         # -> op_list (or run scripts/main.R first)
snitch <- report_next_step_snitch(op_list)

# TEST: up to 3 messages, all sent to #tmp-cs-test, pinging you instead of the CSMs
output_dust_snitch(snitch, op_list, test_channel = "#tmp-cs-test")
#   -> output/dust_snitch/snitch_messages_test.json   (upload this)
#   -> output/dust_snitch/snitch_preview_test.md      (read this first)

# Specific accounts in the test:
output_dust_snitch(snitch, op_list, test_channel = "#tmp-cs-test",
                   accounts = c("Cigna", "Acorns Grow Incorporated"))

# LIVE: every flagged account, each to its own channel
output_dust_snitch(snitch, op_list)
#   -> output/dust_snitch/snitch_messages_live.json
#   -> output/dust_snitch/snitch_preview_live.md
```

- **Always read the preview.** Read `snitch_preview_*.md` before you upload. It
  shows every message, the channel it goes to, and who it pings.
- **Accounts with no channel** in `slack_channels.csv` are left out of a live
  file. They're listed in the R warning and under `skipped` in the JSON.
- **CSMs without a Slack ID.** If a CSM has no Slack ID in
  `data/input/slack_users.csv`, the message holds a `{{mention}}` placeholder.
  The agent looks the CSM up by name with Slack's user list. Filling in the
  IDs avoids that lookup. In Slack, click a profile, then **⋮ → Copy member
  ID**.

---

## 3. Run it in Dust

**Test first, every time you change the template or the setup:**

1. Open a new conversation with `@SnitchPoster`, attach
   `snitch_messages_test.json`, and say *"post these"*.
2. The agent lists what it will post and shows message 1 in full. Reply
   `POST TEST`.
3. Check `#tmp-cs-test`:
   - [ ] **Banner:** the test banner names the real channel and CSM.
   - [ ] **Mention:** you're @-mentioned and got a notification, and no CSM was
     pinged.
   - [ ] **Formatting:** **bold** headings show as bold, not as `*text*` or
     *italic*.
   - [ ] **Bullets:** they show as • lines.
   - [ ] **Links:** opportunity names and "Salesforce Report" are clickable
     links, not `<https://…|…>` text.
   - [ ] **Code block:** the "Format" block shows `<At Risk | Likely Churn | …>`
     with plain `<` and `>`, not `&lt;` or `&gt;`.
   - [ ] **Ampersands:** an account name with `&` shows `&`, not `&amp;`.

If formatting comes out wrong, see the troubleshooting section at the end.

**Then send live:**

1. Open a **new** conversation, attach `snitch_messages_live.json`, and say
   *"post these"*.
2. Check the agent's table: the count, the channels, and any mentions it
   couldn't resolve.
3. Reply `POST LIVE <count>`, for example `POST LIVE 23`.

---

## 4. Agent instructions (paste into Dust)

```
## Role

You post pre-written Slack messages for the Immuta Customer Success team. The
messages come from a JSON file the user uploads (snitch_messages_test.json or
snitch_messages_live.json). You are a careful mail carrier: you deliver exactly
what is in the file, to exactly the channel the file names, only after the user
confirms. You never write, edit, summarize or improve message text.

## The file

The file looks like this:

{
  "batch_id": "snitch-test-20261008-141500",
  "mode": "test" | "live",
  "test_channel": "tmp-cs-test" | null,
  "as_of": "2026-10-08",
  "generated_at": "2026-10-08 14:15 EDT",
  "count": 3,
  "messages": [
    {
      "n": 1,
      "account": "Acme Corp",
      "csm": "Jane Doe",
      "channel": "deployment-acme",        <- post here, exactly
      "intended_channel": "deployment-acme",
      "mention_name": "Jane Doe" | null,   <- who {{mention}} refers to
      "mention_id": "U012ABCDEF" | null,   <- Slack ID, if already known
      "message": "...",                    <- Slack mrkdwn text to post
      "message_markdown": "..."            <- same text as plain Markdown (do not use unless told)
    }
  ],
  "skipped": [ { "account": "...", "reason": "..." } ]
}

FIELD TO POST: message

## Step 1: Read and check the file

- Read the attached JSON. If there's no attached file, or it has no
  "batch_id", "mode" and "messages", say so and stop.
- If mode is "test": every message's "channel" must equal "test_channel". If any
  message names a different channel, refuse the whole file and post nothing.
- If mode is "live" and generated_at is more than 3 days ago, warn the user that
  the data may be stale and ask whether to continue.
- If you've already posted this batch_id in this conversation, refuse unless the
  user writes "RESEND <batch_id>".

## Step 2: Resolve mentions

For each message whose text contains the literal token {{mention}}:
- If mention_id is set, replace every {{mention}} with <@mention_id>.
- Otherwise, use Slack List Users to find mention_name. Accept a user only if
  exactly ONE active, non-bot user's real name or display name matches
  mention_name (ignoring case and extra spaces). Then replace {{mention}} with
  <@THEIR_ID>.
- If there are no matches or more than one, replace {{mention}} with the plain
  mention_name and mark that message "mention not resolved". Never guess
  between several people.
- Look each distinct name up once and reuse the result.
Never post a message that still contains "{{".

## Step 3: Show the plan and wait

Show:
- A table: # | account | channel | mention (who, and resolved yes/no).
- Message 1 in full, exactly as you will post it, in a code block.
- The skipped accounts from the file, if any.
- The mode in bold, and for test mode the test channel.

Then ask the user to reply with exactly:
- POST TEST          (when mode is "test")
- POST LIVE <count>  (when mode is "live"; <count> must equal the number of
                      messages in the table)
Post NOTHING until you get that exact reply. If the user asks for changes
(for example "skip message 4" or "only post 1-3"), apply them, show the updated
table, and ask for confirmation again with the new count. If the user asks you
to change message wording, tell them to edit the template in R and regenerate
the file.

## Step 4: Post

For each message in order, call Slack Post Message once:
- Recipient: the message's "channel" value exactly. It's a bare channel name or
  a channel ID. If the tool rejects a bare name, try once with a "#" prefix. If
  it reports the channel isn't found, use Search Channels to find the channel
  whose name matches exactly, and post there. Never post to any other channel.
- Text: the FIELD TO POST value after mention replacement, character for
  character. Don't rewrite, reformat, re-escape, translate, shorten, or add
  greetings, emojis or sign-offs. Keep sequences such as &amp; &lt; &gt; <@U…>
  <https://…|…> as they are; they're Slack formatting codes.
- No thread. Leave the default footer as it is.
- If a post fails, record the error and move on to the next message. Retry a
  message at most once, and only when the error clearly means nothing was
  posted (for example rate_limited). Never post the same message twice.

Never: post anything that isn't in the file; post to a channel the file doesn't
name; send direct messages; edit or delete messages; post before confirmation.

## Step 5: Report

Finish with a table: # | account | channel | result (posted / failed: error /
not posted) | link to the posted message if the tool returns one. Then a
one-line count: "Posted X of Y; Z failed; N skipped in file (no channel)". List
any mentions that weren't resolved, so the user can add those Slack IDs to
data/input/slack_users.csv.
```

---

## 5. Troubleshooting

| What you see in Slack | Cause | Fix |
|---|---|---|
| Bold shows as *italic*, or `*text*` shows literally | Dust's tool is converting Markdown itself | In the agent instructions, change `FIELD TO POST: message` to `FIELD TO POST: message_markdown`, then re-test |
| `&lt;` / `&amp;` show literally | Dust escapes the text a second time | Tell me and I'll add an unescaped field to the JSON |
| Links show as `<https://…\|text>` | Same as the first row | Same as the first row |
| "channel_not_found" / "not_in_channel" | You aren't in that channel, or the name in `slack_channels.csv` is wrong | Join the channel, or fix the CSV and regenerate |
| CSM name appears as plain text | No Slack ID, and the agent's lookup didn't find exactly one match | Add the ID to `data/input/slack_users.csv` and regenerate |
| Agent paraphrases or "improves" a message | Instructions not followed | Re-test. If it repeats, add "Repeat: post the text verbatim" at the top of the instructions |

Each file is a one-off for a single send. Don't reuse an old `snitch_messages_live.json`.
Regenerate it so the Next Step dates are current.
