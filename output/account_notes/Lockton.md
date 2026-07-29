# Lockton

## Account Strategy

### Adoption


### Performance


### Engagement


---

## Notes

### 16 June 2026 DBX Summit MTG
#### Ben/Matt Context
BLUF: Dev environment for mocking up request+audit workflows is top priority for next 2 months

Lex wants to use the DEV environment (Immuta+Databricks) to build out some demo workflows and see how they will use it on the business side.
Initial focus is on enabling audit + request to PHI data (insurance claims)
Products in use are:
 - Infolock -- we'd protect PHI coming in from this system
 - Andromeda -- provides auditing; Lex will evaluate which parts of an audit come from Immuta and which come from Andromeda

Lex wanted to use the POV instance initially, but we redirected him to use his Databricks
Recommendation from FCTO
Slightly longer in terms of start, but better for the program overall

Will continue to hold working sessions to get them up and running with this demo env


----

Use Case 1 -- Audit

Hook up to data under use
Explain how to read/use the audit log


Use Case 2 -- Requests

Bucket data assets
Unknown exactly how, but Ben believes the following
by PHI or not
by Client or region

Assign reviewers
Evaluate workflows


Use Case 3 -- Row filtering

They want to filter based on assigned client_id
Classic attribute row-filter
No PHI masking here as practitioners need all fields for their job (allowed to see PHI for them)


Use Case 4 -- PHI Masking

We may be able to allow access to some PHI tables if we mask it
Show more data, but mask sensitive.
We'll have to dive in more to understand if this is required


Use Case 5 -- Guardrails

They have an initial group of "NO-PHI"
Typically we just default to no access for a group like this, but membership may mean they cannot request data with PHI
We'll have to dive in more to understand if this is required.