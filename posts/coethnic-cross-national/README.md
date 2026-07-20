# Which nation is the exception depends on the estimator

Replication materials for the *Pixels and Patterns* post of the same name (Patterns beat, by Steven Denney). The post reads three national-identity conjoint experiments, in Germany, South Korea, and Taiwan, two ways. On the average marginal component effect (AMCE), the co-ethnic ancestry effect makes Taiwan the low outlier and Korea look like Germany. Estimated as a distribution of intensity with a hierarchical-Bayes mixed logit, Korea is the high outlier and Germany clusters with Taiwan. The average and the distribution disagree about which country is exceptional, and only the distribution recovers the ethnic and civic contrast the histories predict.

Read the post: [Pixels and Patterns](https://pixelsandpatterns.substack.com).

## The two papers behind it

- **The method.** Avidit Acharya, Jens Hainmueller, and Yiqing Xu, "Learning Preferences from Conjoint Data: A Structural Deep Learning Approach" (2026), arXiv:2604.10845. The distributional read of a conjoint, and the covariate-indexed estimator the post points to as the next step.
- **The instrument.** Steven Denney and H. Christoph Steinhardt, "Measuring National Identity with Conjoint Experiments Using the Case of Taiwan" (working paper, June 2026), SSRN 7027118, https://ssrn.com/abstract=7027118. The eight-attribute conjoint that embeds the national-identity battery into a forced choice.

## Headline numbers

| Country | n | AMCE | Structural weight | Prefer / reject | Moderate (>0.5) | Strong (>1) |
|---|---|---|---|---|---|---|
| South Korea | 2,000 | +0.082 | 0.69 [0.56, 0.83] | 81 / 19 | 59 [53, 68] | 34 [20, 43] |
| Germany | 2,079 | +0.070 | 0.48 [0.40, 0.56] | 77 / 23 | 48 [42, 53] | 21 [11, 28] |
| Taiwan | 2,050 | +0.042 | 0.43 [0.30, 0.57] | 79 / 21 | 44 [19, 55] | 15 [0, 30] |

## Layout

```
code/       00_deidentify.R   build the de-identified microdata from raw exports (authors only)
            02_structural_brms.R  fit the hierarchical-Bayes mixed logit per country (the slow step)
            03_present.R      tables + main figures from the fitted model and the design-based quantities
            04_party.R        party-bloc supplement
            05_observational.R the direct battery and ISSP 2023
            06_table.py       render Table 1 as an image
data/derived/   the aggregates behind every table and figure (structural weights, hierarchy,
                battery distribution, ISSP shares, party blocs). No respondent-level data.
data/microdata/ conjoint_{kr,de,tw}.csv — de-identified respondent-level choice data
figures/    the figures the post shows (plus two from the companion research note)
post/       the post source (Markdown) and a self-contained HTML copy
```

## Reproducing the analysis

**Figures and tables.** The numbers behind every figure and table are in `data/derived/`. `figures/` holds the rendered outputs. `06_table.py` re-renders Table 1 from `data/derived/summary_structural.csv`.

**The structural model.** `data/microdata/conjoint_{kr,de,tw}.csv` hold the de-identified choice data: one row per profile, the respondent replaced by a sequential `rid`, the eight randomized attributes, the profile index, `person_choice` (1 if the profile was chosen), and `in_analytic_sample` (the completers/attention filter that defines the reported n of 2,000 / 2,079 / 2,050). Fit the mixed logit on the rows where `in_analytic_sample` is true to reproduce the structural utility weights in the table. `02_structural_brms.R` is the script as run against the authors' raw tree; point its input at these files to re-fit.

## Requirements

R with `brms`, `cregg`, `dplyr`, `tidyr`, `readr`, `ggplot2`; Python 3 with `matplotlib` for the table renderer.

## Data provenance and rights

See [`data/README.md`](data/README.md) for the source studies, the de-identification log, and licensing.

## Cite

> Steven Denney, "Which nation is the exception depends on the estimator," *Pixels and Patterns*, 2026, https://pixelsandpatterns.substack.com.
