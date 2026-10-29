# Immuta Account Review Process: Questions

<!--
Source of the agenda questions for Account_Transition_Review.Rmd, which reads
this file every time it is knitted. Edit the wording here, not in the Rmd.

Format rules the Rmd parser relies on:
  - Each segment starts with "## <n>. <Title>" (n = 1-5, in order). The
    segment number decides which metrics panel sits beside it.
  - Under it, a "- **Time:** <start>–<end> (<m> min)" line and a
    "- **Where to look in Customer360:** <text>" line.
  - Each question is "<number>. **(core)** <text>" or
    "<number>. **(if time)** <text>". A long question can continue on
    indented lines below it.
  - **bold** in a question renders as bold on the page.
  - The data flags on the page point at question numbers (e.g. "Q14:
    3 open CUSTX escalations"). If you renumber questions, tell Claude or
    update build_prompts() in the Rmd so the flags still point at the
    right question.
-->

## 1. Snapshot

- **Time:** 0:00–0:03 (3 min)
- **Where to look in Customer360:** Account card header, Overview

1. **(core)** In two sentences, what is this account's story? Why did they buy Immuta, and where are they today?
2. **(core)** What is your overall health rating for this account, and why?
3. **(core)** Can the customer state Immuta's value in their own words? Who could do it best, and what would they say?

## 2. Adoption: use cases in production

- **Time:** 0:03–0:12 (9 min)
- **Where to look in Customer360:** Overview: Adoption evidence and ratings; PBAC plot

4. **(core)** Walk us through the use cases: which are **in production**, **in pipeline**, **blocked**, and in the **backlog**? For each, who are the users, what data, and what policy type?
5. **(core)** What pain point does each production use case solve, and what measurable outcome has the customer seen? Is it documented anywhere?
6. **(core)** Customer360 shows lifecycle stage, PBAC rating, consumption ratio, and complexity score. Do you agree with them? If PBAC is Red, is it an **enablement problem** (platform built, users not there) or a **depth problem** (users doing simple work)?
7. **(core)** Which of these are in production: **multi-platform, data and subscription policies, Sensitive Data Discovery, Domains, guardrails**? Which would be the next one to add, and what is in the way?
8. **(core)** Where are **Request workflows** in the None → Tried → Adopted → Retained stages, and does the Salesforce rating match Looker?
9. **(if time)** Is there interest or activity in **Comply** or **Agentic Governance** (OBO, Request MCP, governed agents)? What is the next step?
10. **(if time)** If the renewal were next week, what adoption evidence would we show the customer?
11. **(if time)** Which licensed users or teams have not registered, and who should be onboarded next?

## 3. Performance: the technical success plan

- **Time:** 0:12–0:19 (7 min)
- **Where to look in Customer360:** Overview: Performance; Escalations; SaaS Migration

12. **(core)** What is the **deployment** (SaaS or self-managed), **version**, and **support status**? If self-managed, what is the **SaaS migration** status: stage, date, owner, and any assumed churn?
13. **(core)** What **upgrades, migrations, or technical projects** are in flight or promised? Who owns each, and by when?
14. **(core)** What is the **support history**? Are there open or recent **P1s or CUSTX escalations**, and which are stale (older than 90 days)? What does the customer believe about them?
15. **(core)** What **blockers or scale limitations** cap adoption, and what is the workaround or plan?
16. **(core)** What **feature requests or critical deliveries** has the customer asked for? Which are tied to renewal or expansion, what did we tell them about timing, and are they tracked in Productboard?
17. **(if time)** Are Connections migrated for all connectors? Are platform or architecture changes coming on the customer side?
18. **(if time)** Can the customer **administer Immuta themselves**, or do they rely on us? Who is their technical owner?
19. **(if time)** Which Immuta technical contact (RSA, Field CTO, support engineer) does the customer trust, and who should the new team contact first?

## 4. Engagement: people, cadence, and renewal

- **Time:** 0:19–0:27 (8 min)
- **Where to look in Customer360:** Overview: Engagement; Renewals; Expansions

20. **(core)** Who is the **Executive Buyer** (name, title, role in the decision)? What is their **relationship** with Immuta and their **influence**, and what is the **evidence** for each?
21. **(core)** Who is the **Champion**? Same questions: relationship, influence, and what they have done to advocate for Immuta when we were not in the room.
22. **(core)** Who else matters: **other stakeholders, detractors, or people who could replace us**? Who is new or leaving, and what organizational changes are coming?
23. **(core)** What is the **QBR/EBR history**: last one, attendees, whether an executive was present, and the next date? Where are the notes stored?
24. **(core)** What is the **renewal status**: stage (0–6), churn quarter, and risk? What has the customer said about renewing, and what expansion opportunities are open?
25. **(if time)** Which **partners** (systems integrators, cloud or data platform partners, resellers) are involved, and who owns the relationship?
26. **(if time)** What personal preferences or sensitivities should the new team know (communication style, topics to avoid, past incidents that damaged trust)?
27. **(if time)** What **advocacy** have we used or could we use (reference calls, CAB, case study, speaking)?

## 5. Close: commitments, risks, next 90 days

- **Time:** 0:27–0:30 (3 min)
- **Where to look in Customer360:** n/a

28. **(core)** What **promises or commitments**, written or verbal, are still open with this customer?
29. **(core)** What are the **top three risks** to this account in the next 12 months?
30. **(core)** What should the new owner do in their **first 30 days**, and what would you do in the next 90 days if you were staying?
31. **(core)** Whom should we meet first, and can you introduce us before you leave?
