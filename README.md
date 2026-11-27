# sdg4-indicator-pipeline

Reproducible R pipeline that downloads SDG 4 education indicators from the UNESCO Institute for Statistics, validates country submissions against consistency rules, and generates formatted Excel workbooks with programmatic column headers, color coding, and number formatting. Built as a workflow demonstration for multi-country education data processing.

> **Status: scaffolding.** The environment (renv, uv, config) is set up and
> the three pipeline steps below are designed, but not yet written.

## Pipeline at a glance

```
UIS API / CSV ──► 01_download.R ──► 02_validate.R ──► 03_excel_output.R ──► formatted .xlsx
                   data/raw/          data/interim/        outputs/tables/
```

| Step | Script | What it does | Reads | Writes |
|---|---|---|---|---|
| 1 | `01_download.R` | Fetches SDG 4 indicators from the UIS API, or loads a local CSV export when the API is unavailable | `config.yml` (indicators, countries, years), `.env` (API key if any) | `data/raw/` |
| 2 | `02_validate.R` | Runs consistency checks and flags missing data, per country and indicator | `data/raw/` | `data/interim/` (validated data + flag table) |
| 3 | `03_excel_output.R` | Builds formatted Excel workbooks with `openxlsx`: programmatic column headers, colour coding for flagged values, number formatting | `data/interim/` | `outputs/tables/*.xlsx` |

Each step reads only what the previous step wrote, so any step can be re-run
on its own without repeating the ones before it.

---

## 1. Requirements

| Tool | Version | Purpose |
|---|---|---|
| R | **4.6.x** (minor pinned) | Analysis |
| rig | latest | Installs and switches R versions |
| Python | **3.12.13** (pinned) | Scripting |
| uv | latest | Python dependency and interpreter manager |

You do **not** need to install Python yourself — `uv` downloads the pinned
interpreter automatically.


## 2. Setup

```bash
cp .env.example .env && chmod 600 .env    # then fill in any secrets
Rscript scripts/00-setup.R                # renv: init or restore
uv sync                                   # creates .venv from pyproject.toml
```

Both may already have been run for you by the template's post-generation hook.

## 3. Repository layout

```
.
├── config.yml              Shared settings — read by BOTH R and Python
├── .env                    Secrets (gitignored)
├── .env.example            Template for .env (committed)
├── .Rprofile               Loads .env into R; renv activation
├── DESCRIPTION             R dependencies, DECLARED (edit this to add one)
├── renv.lock               R dependencies, RESOLVED (generated — do not edit)
├── .python-version         Pinned Python patch release
├── pyproject.toml          Python dependencies, declared
├── uv.lock                 Python dependencies, resolved (generated)
│
├── scripts/                Numbered pipeline steps; extension = language
│   ├── 00-setup.R          One-time environment bootstrap (renv)
│   ├── 01_download.R       API / CSV → data/raw/            (planned)
│   ├── 02_validate.R       Consistency checks, missing flags (planned)
│   └── 03_excel_output.R   openxlsx → outputs/tables/       (planned)
│
├── data/
│   ├── raw/                Source data. GITIGNORED.
│   ├── interim/            Intermediate artefacts. GITIGNORED.
│   └── processed/          Small analysis-ready tables. COMMITTED.
│
├── outputs/
│   ├── figures/            COMMITTED
│   └── tables/             COMMITTED
│
├── reports/                Quarto / R Markdown documents
└── docs/                   Background notes, data dictionaries
```

## 4. Running the pipeline

Run the steps in order from the project root:

```bash
Rscript scripts/01_download.R      # 1. fetch indicators  → data/raw/
Rscript scripts/02_validate.R      # 2. check and flag    → data/interim/
Rscript scripts/03_excel_output.R  # 3. build workbooks   → outputs/tables/
```

Scripts assume the **project root** as the working directory. Running R from
elsewhere means `.Rprofile` is never read and `.env` will not be loaded.

### Step 1 — `01_download.R`: get the data

- **Primary source:** the UIS Data API. Indicator codes, country list and the
  year range (`period.start_year` to `period.end_year` in `config.yml`) drive
  the request.
- **Fallback:** a CSV export downloaded by hand from the UIS data browser and
  placed in `data/raw/`. Use it when the API is down, rate-limited, or when you
  need to reproduce a run from a fixed snapshot.
- Raw responses are saved unchanged, so every later step can be re-run without
  hitting the API again.
- Any API key goes in `.env` (see `.env.example`), never in `config.yml`.

### Step 2 — `02_validate.R`: consistency checks and missing-data flags

Data is **flagged, not dropped.** Each check adds a row to a flag table rather
than altering values, so a reviewer can see exactly what was raised and why.
Typical checks:

- **Missing data:** country × indicator × year combinations with no value.
- **Range checks:** rates and percentages outside 0–100.
- **Internal consistency:** e.g. sex-disaggregated values that do not bracket
  the total, or a gender parity index that does not match its components.
- **Time-series breaks:** year-on-year changes large enough to suggest a
  revision or a reporting error.
- **Duplicates:** more than one value for the same key.

Outputs: the validated dataset and the flag table, both in `data/interim/`.

### Step 3 — `03_excel_output.R`: formatted workbooks

Uses `openxlsx` to write workbooks to `outputs/tables/`, with:

- column headers generated from the indicator metadata rather than typed by hand;
- colour coding driven by the flag table from step 2, so flagged cells stand out;
- number formats set per indicator (percentages, ratios, counts);
- a sheet listing every flag, for the reviewer.

### Adding the R packages these steps need

Dependencies are declared in `DESCRIPTION`, not inferred from `library()` calls
(renv runs in `explicit` snapshot mode). Before writing the steps, add the
packages they use, for example:

```
Imports:
    httr2,
    readr,
    dplyr,
    yaml,
    openxlsx
```

then re-run `Rscript scripts/00-setup.R` and commit the updated `renv.lock`.

---

## 5. Decision log

Record every non-obvious choice here, with its reasoning, so a future
maintainer can tell what was deliberate and what was incidental. Add a row,
then a subsection below it if the choice needs more than a line.

| # | Decision | Rationale in brief |
|---|---|---|
|   |          |                    |

<!--
### Decision 1 — <short title>

**Chosen:** what was done.

**Rejected:** the main alternative.

**Why.** The reasoning, including anything non-obvious that would otherwise
be rediscovered the hard way.

**Cost.** What this trade-off gives up. Every real decision has one.
-->

---

## 6. Known gaps

Things carried forward deliberately, so they read as choices rather than
oversights.

- Nothing recorded yet.

