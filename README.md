An R Shiny app for documenting and reviewing recurrent urinary tract infection (UTI) episodes patient by patient, from symptom onset, through treatment, to follow-up, culture results and antibiotic resistance.

**Live demo:** [https://greencare-physio.shinyapps.io/UTI-Tracker/](https://greencare-physio.shinyapps.io/UTI-Tracker/) *(fictitious data only; demo data may be reset at any time)*

---

## Why it matters
Recurrent UTI care is longitudinal. A patient may return several times a year, and at every visit the clinician needs the same questions answered quickly: What happened last time? What was prescribed, and for how long? Did the culture grow anything? Which antibiotics has this patient already shown resistance to?

When that information lives in free-text notes, it is slow to find and easy to miss. This tool replaces that with a structured, per-patient timeline so the answers are visible at a glance.

## What it is built to support
- **A clear view of every episode**: onset date, symptoms, risk factors, pain severity (VAS), treatment, course progress and outcome are captured in one place and numbered per patient (Episode #1, #2, ...).
- **Safer prescribing conversations**: an antibiotic resistance vault keeps previous resistant (R) results next to the patient record.
- **Early warning signs**: a dedicated red-flag section (fever, chills, flank pain, back pain, nausea, vomiting) highlights possible upper UTI / pyelonephritis.
- **Nothing falls through the cracks**: pending lab results are flagged separately from "no culture done", and the course tracker shows Day 3 of 5 and the expected end date for the current antibiotic.
- **Handover and referral**: the History page prints as a clean, patient-labelled A4 landscape report.

## Key features
| Area | What it does |
| :--- | :--- |
| **Patient confirmation** | Enter name and a fixed-format 12-digit IC (YYMMDD-PB-####) and press *Confirm Patient*. No record can be added until a patient is confirmed. |
| **Wrong-patient protection** | If the entered name does not match the name already stored for that IC, the app asks which to keep, which helps catch a mistyped IC. Records are always scoped to the confirmed patient. |
| **Episode workflow** | Symptoms -> clinic management (antibiotic, start date, duration) -> follow-up and completion. |
| **Edit and delete** | Any episode can be corrected or deleted, with a confirmation step before deletion. |
| **History** | Completed episodes in a structured table with filters (onset period, pathogen) and print support. |
| **Antibiotic resistance vault** | Record resistant antibiotics per lab test date, per patient. |
| **Local storage** | Everything is stored in a local SQLite file that is created automatically on first run. |

## Technical stack
- **Language**: R
- **Framework**: Shiny
- **Packages**: shiny, shinyjs, jsonlite, DBI, RSQLite
- **Storage**: SQLite (`uti_multistep_records.db`), with automatic schema migration for older database files
- **Front end**: custom CSS and JavaScript (fixed-format IC input mask, print stylesheet)
- **Structure**: single-file app (`app.R`)

## Clinical scope and limitations
- This is a record-keeping and review tool. It does not recommend diagnoses or antibiotics and does not implement a clinical guideline algorithm.
- It is not a medical device and must not be used as the sole basis for clinical decisions.
- Resistant antibiotics are entered manually. The lab report upload field stores only the file name for reference; it does not read the report content.

## Data privacy
- The app stores a patient name and IC number, which are identifiable data. They are saved in plain text in a local SQLite file, and the app has no login or encryption.
- The database file is excluded from version control (`.gitignore` contains `*.db`). Never commit patient data to a repository.
- All screenshots and the live demo use fictitious patients only.
- Before any real clinical use, deploy it only within your organisation's approved environment and add the access control, encryption and audit logging your data-protection policy requires.

## Quick start
### Option 1: try the live demo
Open the [Live Demo Link](https://greencare-physio.shinyapps.io/UTI-Tracker/). No installation needed.

### Option 2: run locally
1. Install dependencies (once):
   ```R
   install.packages(c("shiny", "shinyjs", "jsonlite", "DBI", "RSQLite"))
