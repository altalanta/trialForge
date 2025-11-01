# Validation Plan

## Scope
The trialforgeR package provides spec-driven validation utilities, reporting, and a small Shiny interface for synthetic SDTM and ADaM datasets. Validation covers core data checks, SDTM domain rules, reporting outputs, and interface wiring.

## Objectives
- Confirm spec parsing accurately reflects validation requirements.
- Demonstrate automated detection of critical data issues (missing required variables, type mismatches, invalid value sets, domain rules).
- Verify QC report generation persists evidence of validation outcomes.
- Ensure interactive and automated entry points reuse the same validation engine.

## Strategy
- Unit tests using synthetic datasets and specs validate individual rules and integration pathways.
- Snapshot style test ensures HTML report is produced successfully.
- Traceability matrix links requirements to validators and evidence.
- Continuous integration executes R CMD check, unit tests, linting, and coverage reporting.

## Acceptance Criteria
- All automated tests pass with zero errors.
- Validation routines flag seeded failures and confirm expected passes.
- QC report artifacts are reproducible and contain validation summaries.
- Shiny app successfully launches and allows download of generated reports (manual spot check optional).
