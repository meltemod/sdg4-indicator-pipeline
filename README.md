# sdg4-indicator-pipeline

Reproducible R pipeline that downloads SDG 4 education indicators from the UNESCO Institute for Statistics, validates country submissions against consistency rules, and generates formatted Excel workbooks with programmatic column headers, color coding, and number formatting. Built as a workflow demonstration for multi-country education data processing.

> **Status: scaffolding.** Generated from a project template. Nothing has been
> built yet.

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

```bash
# add pipeline steps here as they are written
```

Scripts assume the **project root** as the working directory. Running R from
elsewhere means `.Rprofile` is never read and `.env` will not be loaded.

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

