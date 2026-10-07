# Account Transition Review: Meeting Agenda

**Length:** 30 minutes per account, on Zoom (recorded and transcribed)
**View for the meeting:** ImmuteR Customer Accounts page (`immuta_accounts.html`), shared on screen
**Framework:** Immuta Account Review Process 2.0: Adoption, Performance, Engagement, and Customer Perceived Value

---

# Part 1: Intent and Preparation

## Why we are doing this

Your accounts are important to us, and we want the customers to be well served after the transition. In 30 minutes per account, we want to capture what you know, especially the things that are not written down in Salesforce, Jira, or ImmuteR, so that the new CSMs, regional Solution Architects, and Field CTOs can pick up each account with context.

We will use ImmuteR as the starting point. It already shows the contract, usage, support, and escalation data. **We are asking you for what the system cannot show:** the history, the relationships, the commitments, and your judgment.

## What happens to the recording

Each call is transcribed with Krisp or Plaud, and we produce summary notes from the transcript. The notes are stored internally with the account's other review notes. They are an internal record for the incoming account team and are not shared with the customer.

## How to prepare (about 10 minutes per account)

1. **Download the latest ImmuteR page.** Open `immuta_accounts.html` in your browser and click **Download Update** (top right) to get the latest published copy. Check the **Updated** date. If the numbers are old, tell us at the start of the call.
2. **Filter to your accounts.** Use the **CSM** pill to show only your accounts, and use the search box to find the account for the meeting.
3. **Open the account card** (Accounts tab) and review the **Overview** tab. Note anything that looks wrong or missing. A **—** means the value is missing in the source system, not zero.
4. **Look at the other tabs for the account:** the card's **Renewals** and **Expansions** tabs, the **Escalations** tab, and the **SaaS Migration** board if the account is self-managed.
5. **Gather what is not in the system.** Think about verbal commitments to the customer, internal politics, relationship history, and anything the customer told you in confidence about their plans.
6. **Be ready to share your screen** with the account card open. We will drive the conversation from it.

## Ground rules

- **Candid is more useful than polite.** We want your real view, including the risks and the problems.
- **Be specific.** Use names, titles, dates, ticket numbers, and say where things are written down. The transcript is the record, so say names aloud.
- **Give evidence for ratings.** For a Champion, an Executive Buyer, or a health color, tell us what the person did or what you saw. "Introduced us to the VP of Data after the QBR" is more useful than "strong champion."
- **Separate what you know from what you think.** Both are valuable, and we will label them differently in the notes.
- **If you do not know, say so.** "I don't know" is a useful answer, and it tells us what to verify.

## Key terms we will use

| Term | Meaning |
|---|---|
| **Adoption** | Use cases in production: an access workflow that lets specific users reach specific data under a specific policy type. Rated for PBAC, Request, Comply, and Agentic. |
| **Performance** | The technical success plan: SaaS migrations, upgrades, support tickets, escalations, blockers, feature requests, and critical deliveries. |
| **Engagement** | Our relationships: Executive Buyer, Champion, other stakeholders, review cadence, partners, and renewal. |
| **Stoplight** | 🟢 Green (healthy), 🟡 Amber (moderate), 🔴 Red (at risk), ⚫ Black (no meaningful adoption). |

---

# Part 2: Agenda (30 minutes)

| Time | Segment | Where to look in ImmuteR |
|---|---|---|
| 0:00 – 0:03 | 1. Snapshot | Account card header, Overview |
| 0:03 – 0:12 | 2. Adoption | Overview: Adoption evidence and ratings; PBAC plot |
| 0:12 – 0:19 | 3. Performance | Overview: Performance; Escalations; SaaS Migration |
| 0:19 – 0:27 | 4. Engagement | Overview: Engagement; Renewals; Expansions |
| 0:27 – 0:30 | 5. Close | n/a |

Questions marked **(core)** are asked on every call. Questions marked **(if time)** are asked for high-ARR, near-renewal, or at-risk accounts.

## 1. Snapshot (3 min)

1. **(core)** In two sentences, what is this account's story? Why did they buy Immuta, and where are they today?
2. **(core)** What overall stoplight would you give this account, and why?
3. **(core)** Can the customer state Immuta's value in their own words? Who could do it best, and what would they say?

## 2. Adoption: use cases in production (9 min)

4. **(core)** Walk us through the use cases: which are **in production**, **in pipeline**, **blocked**, and in the **backlog**? For each, who are the users, what data, and what policy type?
5. **(core)** What pain point does each production use case solve, and what measurable outcome has the customer seen? Is it documented anywhere?
6. **(core)** ImmuteR shows lifecycle stage, PBAC rating, consumption ratio, and complexity score. Do you agree with them? If PBAC is Red, is it an **enablement problem** (platform built, users not there) or a **depth problem** (users doing simple work)?
7. **(core)** Which of these are in production: **multi-platform, data and subscription policies, Sensitive Data Discovery, Domains, guardrails**? Which would be the next one to add, and what is in the way?
8. **(core)** Where are **Request workflows** in the None → Tried → Adopted → Retained stages, and does the Salesforce rating match Looker?
9. **(if time)** Is there interest or activity in **Comply** or **Agentic Governance** (OBO, Request MCP, governed agents)? What is the next step?
10. **(if time)** If the renewal were next week, what adoption evidence would we show the customer?
11. **(if time)** Which licensed users or teams have not registered, and who should be onboarded next?

## 3. Performance: the technical success plan (7 min)

12. **(core)** What is the **deployment** (SaaS or self-managed), **version**, and **support status**? If self-managed, what is the **SaaS migration** status: stage, date, owner, and any assumed churn?
13. **(core)** What **upgrades, migrations, or technical projects** are in flight or promised? Who owns each, and by when?
14. **(core)** What is the **support history**? Are there open or recent **P1s or CUSTX escalations**, and which are stale (older than 90 days)? What does the customer believe about them?
15. **(core)** What **blockers or scale limitations** cap adoption, and what is the workaround or plan?
16. **(core)** What **feature requests or critical deliveries** has the customer asked for? Which are tied to renewal or expansion, what did we tell them about timing, and are they tracked in productboard?
17. **(if time)** Are Connections migrated for all connectors? Are platform or architecture changes coming on the customer side?
18. **(if time)** Can the customer **administer Immuta themselves**, or do they rely on us? Who is their technical owner?
19. **(if time)** Which Immuta technical contact (RSA, Field CTO, support engineer) does the customer trust, and who should the new team contact first?

## 4. Engagement: people, cadence, and renewal (8 min)

20. **(core)** Who is the **Executive Buyer** (name, title, role in the decision)? What is their **relationship** with Immuta and their **influence**, and what is the **evidence** for each?
21. **(core)** Who is the **Champion**? Same questions: relationship, influence, and what they have done to advocate for Immuta when we were not in the room.
22. **(core)** Who else matters: **other stakeholders, detractors, or people who could replace us**? Who is new or leaving, and what organizational changes are coming?
23. **(core)** What is the **QBR/EBR history**: last one, attendees, whether an executive was present, and the next date? Where are the notes stored?
24. **(core)** What is the **renewal status**: stage (0–6), churn quarter, and risk? What has the customer said about renewing, and what expansion opportunities are open?
25. **(if time)** Which **partners** (systems integrators, cloud or data platform partners, resellers) are involved, and who owns the relationship?
26. **(if time)** What personal preferences or sensitivities should the new team know (communication style, topics to avoid, past incidents that damaged trust)?
27. **(if time)** What **advocacy** have we used or could we use (reference calls, CAB, case study, speaking)?

## 5. Close: commitments, risks, next 90 days (3 min)

28. **(core)** What **promises or commitments**, written or verbal, are still open with this customer?
29. **(core)** What are the **top three risks** to this account in the next 12 months?
30. **(core)** What should the new owner do in their **first 30 days**, and what would you do in the next 90 days if you were staying?
31. **(core)** Whom should we meet first, and can you introduce us before you leave?

---

# Part 3: After the Meeting

- The transcript is turned into summary notes, using the Account Health Summary and the QBR note structure from the doctrine.
- Anything that conflicts with ImmuteR, Salesforce, or Jira is flagged for verification.
- Notes are stored internally and linked from the account's Salesforce note.
- Open commitments and top risks go to the incoming owner and the account's Field CTO first.
