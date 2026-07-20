# Data provenance and de-identification

## Sources

Three national-identity conjoint surveys on a common eight-attribute instrument (Denney and Steinhardt, SSRN 7027118):

| Country | Survey | Year | Analytic n |
|---|---|---|---|
| Germany | IFES 2023 | 2023 | 2,079 |
| South Korea | AKS 2023 (ROK) | 2023–24 | 2,000 |
| Taiwan | ROC paired study | 2023–24 | 2,050 |

Each respondent saw pairs of profiles varying eight attributes (origin, ancestry, residence, language, citizenship, tradition, respect for political institutions, and national feeling) and chose the one they considered more truly a national. The focal attribute is co-ethnic ancestry.

## `derived/` — aggregates (no respondent-level data)

Model output and design-based summaries behind the post's tables and figures: `summary_structural.csv` (AMCE, structural weight, prefer/reject and intensity shares per country), `hierarchy.csv` (the eight-attribute structural weights), `battery_dist.csv` (importance-rating distribution), `observational.csv` (this study's battery and ISSP 2023 shares), and the `party_*.csv` supplement.

## `microdata/` — de-identified respondent-level choice data

`conjoint_{kr,de,tw}.csv`, produced by `code/00_deidentify.R` from the authors' raw merged exports. Columns:

- `rid` — sequential integer, one per respondent. The original survey response identifier is dropped and cannot be recovered from this file.
- `in_analytic_sample` — TRUE for the respondents in the reported n. South Korea keeps survey completers, Germany keeps those who passed the instructed-response check, Taiwan keeps all respondents (the source study runs no attention filter).
- `question_profile` — profile/task index within respondent.
- `Origin, Ancestry, Residence, Language, Citizenship, Tradition, Political, Feeling` — the randomized attribute levels shown (in the survey's own language).
- `person_choice` — 1 if the profile was chosen, 0 otherwise.

### De-identification log

Run `code/00_deidentify.R`. For each survey it: reads the raw merged conjoint file; replaces the response identifier (`ResponseId` for Korea and Taiwan, the positional `id` for Germany) with a sequential `rid` by first appearance; derives the boolean `in_analytic_sample` flag from the completers/attention rule; keeps only the modelling columns above; and asserts that no original identifier survives. No demographic covariates, free text, timestamps, or IP/location fields are written. The script re-runs to the same output.

Verification: `grep -lE "ResponseId|R_[0-9A-Za-z]{10}|Finished" microdata/*.csv` returns nothing; respondent counts flagged in-sample are 2,000 / 2,079 / 2,050.

## Rights and reuse

The de-identified choice data is released with the post under CC BY 4.0 (see repository `LICENSE`). The German data originate in the IFES 2023 study; the Korean and Taiwanese data are from the co-authored instrument (Denney and Steinhardt). Reuse should cite the post and the instrument working paper. Raw survey exports are not distributed here and remain with the source studies.
