# app.R - Recurrent UTI Dynamic Multi-Step Workflow Tracker
library(shiny)
library(shinyjs)
library(jsonlite)
library(DBI)
library(RSQLite)

# ==================== 1. DATABASE SETUP ====================

init_uti_db <- function(db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS uti_episodes_v3 (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id TEXT,
      patient_name TEXT,
      start_date TEXT,
      end_date TEXT,
      stage TEXT,
      symptoms_json TEXT,
      vas_pain INTEGER,
      medication TEXT,
      med_duration_days INTEGER,
      pathogen TEXT,
      followup_notes TEXT,
      notes TEXT
    )
  ")
  
  ep_cols <- dbListFields(conn, "uti_episodes_v3")
  if (!("pathogen" %in% ep_cols)) {
    dbExecute(conn, "ALTER TABLE uti_episodes_v3 ADD COLUMN pathogen TEXT DEFAULT 'No Culture Done'")
  }
  if (!("med_start_date" %in% ep_cols)) {
    dbExecute(conn, "ALTER TABLE uti_episodes_v3 ADD COLUMN med_start_date TEXT")
  }
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS patient_profile (
      patient_id TEXT PRIMARY KEY,
      patient_name TEXT
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS ast_records (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id TEXT,
      test_date TEXT,
      resistant_meds TEXT,
      report_file_name TEXT
    )
  ")
  
  # Older databases may lack some columns in ast_records; add them automatically
  ast_cols <- dbListFields(conn, "ast_records")
  for (col in c("patient_id", "test_date", "resistant_meds", "report_file_name")) {
    if (!(col %in% ast_cols)) {
      dbExecute(conn, sprintf("ALTER TABLE ast_records ADD COLUMN %s TEXT", col))
    }
  }
  
  dbDisconnect(conn)
}

init_uti_db()

# One-time cleanup: remove old test records without an IC (the whole database file is backed up first)
# After this runs there will be no such records (a patient must now be confirmed before adding records), so it does nothing afterwards. It can be kept or removed.
cleanup_records_without_ic <- function(db_path = "uti_multistep_records.db") {
  cond <- "patient_id IS NULL OR TRIM(REPLACE(patient_id, char(160), '')) = ''"
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  n_ep <- dbGetQuery(conn, paste("SELECT COUNT(*) AS n FROM uti_episodes_v3 WHERE", cond))$n
  n_ast <- dbGetQuery(conn, paste("SELECT COUNT(*) AS n FROM ast_records WHERE", cond))$n
  dbDisconnect(conn)
  if (n_ep + n_ast == 0) return(invisible(NULL))
  
  backup <- sub("\\.db$", paste0("_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".db"), db_path)
  if (!file.copy(db_path, backup)) {
    message("Cleanup skipped: could not create a backup copy of the database.")
    return(invisible(NULL))
  }
  
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(conn, paste("DELETE FROM uti_episodes_v3 WHERE", cond))
  dbExecute(conn, paste("DELETE FROM ast_records WHERE", cond))
  dbDisconnect(conn)
  message(sprintf("Cleanup: removed %d episode(s) and %d AST record(s) without IC. Backup saved as %s", n_ep, n_ast, backup))
}
cleanup_records_without_ic()

# DB Functions for Patient Profile
get_patient_profile <- function(patient_id, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  res <- dbGetQuery(conn, "SELECT * FROM patient_profile WHERE REPLACE(REPLACE(patient_id, char(160), ''), ' ', '') = ?", params = list(patient_id))
  dbDisconnect(conn)
  if (nrow(res) == 0) return(NULL)
  return(as.list(res[1, ]))
}

save_patient_profile <- function(patient_id, patient_name, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(conn, "
    INSERT INTO patient_profile (patient_id, patient_name) 
    VALUES (?, ?) 
    ON CONFLICT(patient_id) DO UPDATE SET patient_name=excluded.patient_name
  ", params = list(patient_id, patient_name))
  dbDisconnect(conn)
}

# DB Functions for Episodes
save_uti_episode_symptoms <- function(patient_id, patient_name, start_date, symptoms, vas_pain, notes, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  symptoms_json <- jsonlite::toJSON(symptoms, auto_unbox = TRUE)
  query <- "INSERT INTO uti_episodes_v3 (patient_id, patient_name, start_date, end_date, stage, symptoms_json, vas_pain, medication, med_duration_days, pathogen, followup_notes, notes) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  dbExecute(conn, query, params = list(patient_id, patient_name, start_date, "", "Stage 1: Symptoms Only", symptoms_json, vas_pain, "Pending Clinic Visit", 0, "No Culture Done", "", notes))
  dbDisconnect(conn)
}

update_episode_management <- function(episode_id, medication, med_duration_days, med_start_date = as.character(Sys.Date()), db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "UPDATE uti_episodes_v3 SET medication = ?, med_duration_days = ?, med_start_date = ?, stage = 'Stage 2: Treatment Added' WHERE id = ?"
  dbExecute(conn, query, params = list(medication, med_duration_days, med_start_date, episode_id))
  dbDisconnect(conn)
}

resolve_uti_episode <- function(episode_id, pathogen, followup_notes, end_date = as.character(Sys.Date()), db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  pathogen_val <- if (nchar(trimws(pathogen)) > 0) trimws(pathogen) else "No Culture Done"
  query <- "UPDATE uti_episodes_v3 SET stage = 'Resolved', end_date = ?, pathogen = ?, followup_notes = ? WHERE id = ?"
  dbExecute(conn, query, params = list(end_date, pathogen_val, followup_notes, episode_id))
  dbDisconnect(conn)
}

update_episode_pathogen_only <- function(episode_id, pathogen, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "UPDATE uti_episodes_v3 SET pathogen = ? WHERE id = ?"
  dbExecute(conn, query, params = list(pathogen, episode_id))
  dbDisconnect(conn)
}

get_all_episodes <- function(patient_id = NULL, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  if (is.null(patient_id)) {
    res <- dbGetQuery(conn, "SELECT * FROM uti_episodes_v3 ORDER BY id DESC")
  } else {
    res <- dbGetQuery(conn, "SELECT * FROM uti_episodes_v3 WHERE REPLACE(REPLACE(patient_id, char(160), ''), ' ', '') = ? ORDER BY id DESC", params = list(patient_id))
  }
  dbDisconnect(conn)
  return(res)
}

# Episode numbers restart from #1 for each patient (the database id is a global auto-increment, internal use only)
add_episode_numbers <- function(df) {
  if (nrow(df) == 0) {
    df$ep_no <- integer(0)
    return(df)
  }
  key <- gsub("[[:space:]\u00a0]", "", ifelse(is.na(df$patient_id), "", df$patient_id))
  df$ep_no <- as.integer(ave(df$id, key, FUN = function(x) rank(x, ties.method = "first")))
  df
}

# VAS severity: returns dot color, text color and label (0 none / 1-3 mild / 4-6 moderate / 7-10 severe)
vas_severity <- function(v) {
  v <- suppressWarnings(as.integer(v))
  if (is.na(v)) v <- 0L
  if (v == 0) list(fill = "#cbd5e1", col = "#64748b", label = "No pain")
  else if (v <= 3) list(fill = "#22c55e", col = "#15803d", label = "Mild")
  else if (v <= 6) list(fill = "#eab308", col = "#b45309", label = "Moderate")
  else list(fill = "#ef4444", col = "#b91c1c", label = "Severe")
}

# ---- Edit / delete records: constants and helper functions ----
std_symptoms <- c("Dysuria (Pain/Burning)", "Urinary Frequency", "Urinary Urgency",
                  "Lower Abdominal Discomfort", "Cloudy Urine", "Hematuria")
std_risks <- c("Insufficient water intake", "Delayed voiding habit", "Post-coital timing",
               "Menopausal / Perimenopausal", "Late Night")
std_flags <- c("Fever", "Chills", "Flank Pain", "Back Pain", "Nausea", "Vomiting")
std_meds <- c("Nitrofurantoin (Macrobid)", "Trimethoprim / Bactrim", "Ciprofloxacin", "Fosfomycin", "Augmentin")
std_pathogens <- c("No Culture Done", "Pending Lab Result", "Escherichia coli", "Klebsiella pneumoniae",
                   "Enterococcus faecalis", "Proteus mirabilis", "Staphylococcus saprophyticus")

# Pull out the entries starting with a prefix (e.g. "Risk Factors: a, b") and split them into single options
split_items <- function(items, prefix) {
  hit <- items[startsWith(items, prefix)]
  if (length(hit) == 0) return(character(0))
  trimws(unlist(strsplit(sub(paste0("^", prefix, "\\s*"), "", hit), ",")))
}

# Edit / Delete buttons: notify the server via Shiny.setInputValue, so no per-record observer is needed
ep_action_buttons <- function(id, compact = FALSE) {
  id <- as.integer(id)
  tagList(
    tags$button(type = "button", class = "ep-action", title = "Edit this episode",
                onclick = sprintf("Shiny.setInputValue('edit_ep', %d, {priority: 'event'})", id),
                if (compact) "✏️" else "✏️ Edit"),
    tags$button(type = "button", class = "ep-action del", title = "Delete this episode",
                onclick = sprintf("Shiny.setInputValue('del_ep', %d, {priority: 'event'})", id),
                if (compact) "🗑️" else "🗑️ Delete")
  )
}

delete_episode <- function(episode_id, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(conn, "DELETE FROM uti_episodes_v3 WHERE id = ?", params = list(episode_id))
  dbDisconnect(conn)
}

# Update selected episode fields using a whitelist of columns
update_episode_fields <- function(episode_id, fields, db_path = "uti_multistep_records.db") {
  allowed <- c("start_date", "symptoms_json", "vas_pain", "notes", "medication", "med_duration_days",
               "med_start_date", "end_date", "pathogen", "followup_notes")
  fields <- fields[names(fields) %in% allowed]
  if (length(fields) == 0) return(invisible(0))
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  sets <- paste0(names(fields), " = ?", collapse = ", ")
  dbExecute(conn, sprintf("UPDATE uti_episodes_v3 SET %s WHERE id = ?", sets),
            params = c(unname(fields), list(episode_id)))
}

# DB Functions for AST Vault
save_ast_record <- function(patient_id, test_date, resistant_meds, report_file_name, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "INSERT INTO ast_records (patient_id, test_date, resistant_meds, report_file_name) VALUES (?, ?, ?, ?)"
  dbExecute(conn, query, params = list(patient_id, test_date, resistant_meds, report_file_name))
  dbDisconnect(conn)
}

delete_ast_record <- function(ast_id, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "DELETE FROM ast_records WHERE id = ?"
  dbExecute(conn, query, params = list(ast_id))
  dbDisconnect(conn)
}

get_all_ast_records <- function(patient_id = NULL, db_path = "uti_multistep_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  if (is.null(patient_id)) {
    res <- dbGetQuery(conn, "SELECT * FROM ast_records ORDER BY id DESC")
  } else {
    res <- dbGetQuery(conn, "SELECT * FROM ast_records WHERE REPLACE(REPLACE(patient_id, char(160), ''), ' ', '') = ? ORDER BY id DESC", params = list(patient_id))
  }
  dbDisconnect(conn)
  return(res)
}

# ==================== 2. UI LAYOUT ====================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f1f5f9; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
      .main-header { background: #0284c7; color: white; padding: 12px 24px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); display: flex; align-items: baseline; flex-wrap: wrap; gap: 6px 16px; }
      .main-header .app-title { margin: 0; font-weight: 700; font-size: 18px; letter-spacing: 0.2px; }
      .custom-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.02); }
      
      /* Vertical card layout */
      .episode-card-vertical { 
        border: 1px solid #cbd5e1; 
        border-radius: 10px; 
        padding: 16px 20px; 
        margin-bottom: 16px; 
        background: #ffffff; 
        box-shadow: 0 2px 4px rgba(0,0,0,0.03); 
      }
      .stage-tag-1 { background-color: #e0f2fe; color: #0369a1; border: 1px solid #bae6fd; font-weight: 600; padding: 4px 12px; border-radius: 12px; font-size: 11px; display: inline-block; }
      .stage-tag-2 { background-color: #fef3c7; color: #b45309; border: 1px solid #fde68a; font-weight: 600; padding: 4px 12px; border-radius: 12px; font-size: 11px; display: inline-block; }
      
      /* Tidy vertical inner blocks */
      .card-box-blue { background-color: #f0f9ff; border: 1px solid #bae6fd; border-radius: 8px; padding: 12px 16px; margin-bottom: 10px; font-size: 13px; color: #0369a1; }
      .card-box-gray { background-color: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 16px; margin-bottom: 12px; font-size: 13px; color: #334155; }
      
      /* Vertical list item style */
      .symptom-list-item { position: relative; padding-left: 16px; margin-bottom: 4px; color: #334155; line-height: 1.5; }
      .symptom-list-item::before { content: '•'; position: absolute; left: 4px; color: #0284c7; font-weight: bold; }
      
      .ast-item { border-left: 4px solid #dc2626; background: #fff5f5; padding: 10px; border-radius: 6px; margin-bottom: 10px; font-size: 12px; position: relative; }
      .ast-close-btn { position: absolute; top: 6px; right: 8px; background: transparent; border: none; font-size: 13px; font-weight: 300; color: #94a3b8; cursor: pointer; transition: color 0.2s; }
      .ast-close-btn:hover { color: #dc2626; }
      
      .progress { display: none !important; }
      
      /* Print: only print the History table (with patient info); hide header, tabs and buttons */
      .print-only { display: none; }
      .print-header { font-size: 13px; color: #0f172a; margin: 4px 0 12px 0; padding-bottom: 8px; border-bottom: 1px solid #cbd5e1; }
      @media print {
        @page { size: A4 landscape; margin: 12mm; }
        body { background: #ffffff !important; }
        .main-header, .nav-tabs, .no-print { display: none !important; }
        .print-only { display: block !important; }
        .custom-card { border: none !important; box-shadow: none !important; padding: 0 !important; margin: 0 !important; }
        #ui_resolved_history_table .btn { display: none !important; }
        #ui_resolved_history_table table { width: 100%; font-size: 11px; }
        #ui_resolved_history_table thead { display: table-header-group; }
        #ui_resolved_history_table tr { page-break-inside: avoid; }
        * { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
      }
      
      /* Patient confirmation status bar */
      .patient-status { font-size: 12px; border-radius: 8px; padding: 8px 12px; margin-top: 10px; line-height: 1.5; }
      .patient-status.ok { background: #f0fdf4; border: 1px solid #bbf7d0; color: #166534; }
      .patient-status.warn { background: #fffbeb; border: 1px solid #fde68a; color: #b45309; }
      .patient-status.idle { background: #f8fafc; border: 1px solid #e2e8f0; color: #64748b; }
      
      /* Monospace IC input so digits line up with the underscores and hyphens */
      #patient_id { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; letter-spacing: 1px; }
      
      /* Card header: left = title + onset date (two lines), right = phase tag only; VAS moved into the blue box */
      .ep-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 12px; margin-bottom: 12px; padding-bottom: 10px; border-bottom: 1px solid #f1f5f9; }
      .ep-title { font-size: 15px; font-weight: 700; color: #0f172a; line-height: 1.3; }
      .ep-onset { font-size: 12px; color: #94a3b8; margin-top: 2px; }
      .vas-row { display: flex; align-items: center; flex-wrap: wrap; gap: 10px; margin-top: 12px; padding-top: 10px; border-top: 1px dashed #bae6fd; font-size: 12px; }
      .vas-dots { display: inline-flex; gap: 4px; }
      .vas-dot { width: 10px; height: 10px; border-radius: 50%; background: #e2e8f0; display: inline-block; }
      .med-row { display: flex; align-items: center; flex-wrap: wrap; gap: 8px 12px; margin-top: 6px; }
      .med-name { font-size: 14px; font-weight: 600; color: #0f172a; }
      .med-chip { background: #e0f2fe; color: #0369a1; border: 1px solid #bae6fd; border-radius: 12px; padding: 2px 10px; font-size: 11px; font-weight: 600; }
      .med-progress { margin-top: 6px; font-size: 12px; color: #64748b; }
      
      /* History table */
      .hx-table th { font-size: 12px; color: #475569; background: #f8fafc; white-space: nowrap; vertical-align: middle !important; }
      .hx-table td { font-size: 13px; vertical-align: top !important; }
      .hx-nowrap { white-space: nowrap; }
      .hx-center { text-align: center; }
      .hx-med { font-weight: 600; color: #0f172a; }
      .hx-sub { font-size: 12px; color: #64748b; margin-top: 2px; }
      .hx-flag { font-size: 12px; color: #b91c1c; margin-top: 2px; }
      .hx-muted { color: #94a3b8; }
      
      /* Edit / Delete buttons: subtle text buttons, highlighted on hover */
      .ep-action { background: none; border: none; color: #94a3b8; font-size: 12px; padding: 4px 8px; border-radius: 6px; cursor: pointer; }
      .ep-action:hover { background: #f1f5f9; color: #0369a1; }
      .ep-action.del:hover { background: #fef2f2; color: #dc2626; }
      
      /* History filter bar */
      .hx-filter-bar { display: flex; flex-wrap: wrap; align-items: flex-end; gap: 0 18px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 10px 14px 0 14px; margin-bottom: 12px; }
      .hx-filter-bar .form-group { margin-bottom: 10px; }
      .hx-filter-bar label { font-size: 12px; color: #475569; font-weight: 600; margin-bottom: 2px; }
      .hx-filter-note { font-size: 12px; color: #64748b; margin-bottom: 8px; }
      .med-note { margin-top: 8px; padding-top: 8px; border-top: 1px dashed #cbd5e1; font-size: 12px; color: #64748b; }
      
      /* VAS ruler gradient bar */
      .irs-line { background: linear-gradient(to right, #22c55e 0%, #eab308 50%, #ef4444 100%) !important; border: none !important; height: 10px !important; border-radius: 5px !important; }
      .irs-bar { background: transparent !important; }
      .irs-slider { width: 18px !important; height: 18px !important; top: 21px !important; }
      
      /* Multi-color structured card styles inside modals */
      .modal-box-blue { background-color: #f0f9ff; border: 1px solid #bae6fd; border-radius: 8px; padding: 12px 15px; margin-bottom: 15px; }
      .modal-box-amber { background-color: #fffbeb; border: 1px solid #fde68a; border-radius: 8px; padding: 12px 15px; margin-bottom: 15px; }
      .modal-box-red { background-color: #fef2f2; border: 1px solid #fecaca; border-radius: 8px; padding: 12px 15px; margin-bottom: 15px; }
      
      /* Red flag inline layout */
      .red-flag-inline .checkbox-inline, .red-flag-inline .checkbox { display: inline-block !important; margin-right: 8px !important; margin-bottom: 0 !important; margin-top: 0 !important; font-weight: 600; font-size: 12px; color: #991b1b; }
      .red-flag-inline .checkbox input { margin-top: 2px !important; }
      
      /* Custom multi-select tag colors */
      .selectize-control.multi .selectize-input > div:nth-child(5n+1) { background: #e0f2fe !important; color: #0369a1 !important; border: 1px solid #bae6fd !important; border-radius: 6px; }
      .selectize-control.multi .selectize-input > div:nth-child(5n+2) { background: #fef3c7 !important; color: #b45309 !important; border: 1px solid #fde68a !important; border-radius: 6px; }
      .selectize-control.multi .selectize-input > div:nth-child(5n+3) { background: #fee2e2 !important; color: #b91c1c !important; border: 1px solid #fecaca !important; border-radius: 6px; }
      .selectize-control.multi .selectize-input > div:nth-child(5n+4) { background: #dcfce7 !important; color: #15803d !important; border: 1px solid #bbf7d0 !important; border-radius: 6px; }
      .selectize-control.multi .selectize-input > div:nth-child(5n) { background: #f3e8ff !important; color: #7e22ce !important; border: 1px solid #e9d5ff !important; border-radius: 6px; }
    ")),
    
    # IC input control script: hyphens stay fixed in place, backspace works smoothly
    tags$script(HTML("
      // IC input format: YYMMDD-PB-####, hyphens are fixed in place from the start
      // Only digits are stored internally (left-packed) and the fixed template is applied for display, so backspace never gets stuck on a hyphen
      function icDigits(s) { return String(s || '').replace(/[^0-9]/g, ''); }
      function icRender(d) {
        var out = '';
        for (var i = 0; i < 12; i++) {
          if (i === 6 || i === 8) out += '-';
          out += (i < d.length) ? d.charAt(i) : '_';
        }
        return out;
      }
      // Caret position after k digits have been entered (hyphens are skipped automatically)
      function icCaret(k) { return k < 6 ? k : (k < 8 ? k + 1 : k + 2); }
      function icBefore(v, pos) { return icDigits(v.substring(0, pos)).length; }
      
      function icApply(el, d, si, ei, nd) {
        var room = 12 - (d.length - (ei - si));
        nd = nd.substring(0, Math.max(room, 0));
        var n2 = d.substring(0, si) + nd + d.substring(ei);
        if (n2.length === 0) {
          el.value = '';
        } else {
          el.value = icRender(n2);
          var p = icCaret(si + nd.length);
          el.setSelectionRange(p, p);
        }
        $(el).trigger('change');
      }
      
      $(document).ready(function() {
        $('#patient_id').attr({
          'placeholder': '______-__-____',
          'title': 'YYMMDD-PB-####',
          'inputmode': 'numeric',
          'autocomplete': 'off'
        });
      });
      
      // Take over typing and deleting: digits are inserted by position, Backspace/Delete remove digits directly, hyphens never move
      $(document).on('beforeinput', '#patient_id', function(e) {
        var ev = e.originalEvent;
        var t = ev.inputType || '';
        var el = this;
        var v = el.value;
        var d = icDigits(v).substring(0, 12);
        var si = icBefore(v, el.selectionStart);
        var ei = icBefore(v, el.selectionEnd);
        var nd = '';
        
        if (t === 'insertText' || t === 'insertFromPaste' || t === 'insertFromDrop' || t === 'insertReplacementText') {
          var txt = ev.data;
          if ((txt === null || txt === undefined) && ev.dataTransfer) txt = ev.dataTransfer.getData('text');
          nd = icDigits(txt);
        } else if (t.indexOf('delete') === 0) {
          if (si === ei) {
            if (t.indexOf('Forward') > -1) { ei = Math.min(si + 1, d.length); }
            else { si = Math.max(si - 1, 0); }
          }
        } else {
          return;
        }
        e.preventDefault();
        icApply(el, d, si, ei, nd);
      });
      
      // Fallback: IME composition, browser autofill, etc. are normalized into the fixed template
      $(document).on('input', '#patient_id', function() {
        var el = this;
        var v = el.value;
        var d = icDigits(v).substring(0, 12);
        var k = Math.min(icBefore(v, el.selectionStart), d.length);
        if (d.length === 0) {
          el.value = '';
        } else {
          var formatted = icRender(d);
          if (v !== formatted) {
            el.value = formatted;
            var p = icCaret(k);
            el.setSelectionRange(p, p);
          }
        }
        $(el).trigger('change');
      });
      
      // On click, snap the caret to the end of the typed digits / nearest digit position instead of stopping in a blank slot
      $(document).on('mouseup', '#patient_id', function() {
        var el = this;
        setTimeout(function() {
          if (!el.value || el.selectionStart !== el.selectionEnd) return;
          var p = icCaret(icBefore(el.value, el.selectionStart));
          el.setSelectionRange(p, p);
        }, 0);
      });
      
      // Print History: temporarily set the page title to 'PatientName_IC' so the saved PDF has a clear file name
      function printHistory(btn) {
        var oldTitle = document.title;
        var fileTitle = $(btn).attr('data-title');
        $('.print-date').text(new Date().toLocaleString());
        if (fileTitle) document.title = fileTitle;
        window.print();
        document.title = oldTitle;
      }
      
      // Pressing Enter in the name or IC box = click the Confirm button
      $(document).on('keydown', '#patient_name, #patient_id', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
          var box = $(this);
          box.trigger('change');
          setTimeout(function() {
            var b = $('#btn_confirm_patient');
            if (!b.prop('disabled')) b.click();
          }, 80);
        }
      });
    "))
  ),
  
  div(class = "main-header",
      h3(class = "app-title", "Recurrent UTI Tracker")
  ),
  
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    tabPanel("📌 Active Episodes",
             div(style = "padding-top: 15px;",
                 fluidRow(
                   column(4,
                          div(class = "custom-card",
                              h5("👤 Patient Profile", style = "color: #0369a1; font-weight: bold; margin-top: 0; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;"),
                              textInput("patient_name", "Patient Name:", value = "", placeholder = "Patient full name"),
                              textInput("patient_id", "IC:", value = ""),
                              shinyjs::disabled(
                                actionButton("btn_confirm_patient", "✔ Confirm Patient", class = "btn btn-primary btn-sm fw-bold w-100", style = "border-radius: 6px; padding: 8px;")
                              ),
                              uiOutput("ui_patient_status")
                          ),
                          div(class = "custom-card",
                              h5("🧬 Antibiotic Resistance & Report Vault", style = "color: #dc2626; font-weight: bold; margin-top: 0; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;"),
                              p(style = "font-size: 11px; color: #64748b; margin-bottom: 12px;", "Upload lab reports to auto-extract resistant strains or fill manually."),
                              actionButton("btn_new_ast", "🔬 Add Lab Report & AST", class = "btn btn-danger btn-sm fw-bold w-100", style = "border-radius: 6px; margin-bottom: 15px; padding: 8px;"),
                              uiOutput("ui_independent_ast_list")
                          )
                   ),
                   
                   column(8,
                          div(class = "custom-card",
                              div(style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px; margin-bottom: 15px;",
                                  h5("📈 Progressive UTI Timeline & Actions", style = "color: #0369a1; font-weight: bold; margin: 0;"),
                                  actionButton("btn_new_episode", "➕ Log Symptom Onset", class = "btn btn-primary btn-sm fw-bold", style = "border-radius: 8px; padding: 6px 14px;")
                              ),
                              p(style = "font-size: 12px; color: #64748b;", "Cards dynamically update buttons based on your current recovery stage. Resolved episodes move to the History tab."),
                              uiOutput("ui_active_episodes_timeline")
                          )
                   )
                 )
             )
    ),
    
    tabPanel("📋 History (Hx) Records",
             div(style = "padding-top: 15px;",
                 div(class = "custom-card",
                     h5("📜 Completed UTI Episodes History", style = "color: #0369a1; font-weight: bold; margin-top: 0; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;"),
                     p(class = "no-print", style = "font-size: 12px; color: #64748b;", "Overview of all resolved UTI episodes. Pending lab culture results can be updated anytime below."),
                     shinyjs::hidden(
                       div(id = "hx_filter_bar", class = "no-print hx-filter-bar",
                           selectInput("hx_period", "Onset period:",
                                       choices = c("All time" = "all", "Last 3 months" = "3m", "Last 6 months" = "6m",
                                                   "Last 12 months" = "12m", "Custom range…" = "custom"),
                                       selected = "all", width = "190px"),
                           conditionalPanel(
                             condition = "input.hx_period == 'custom'",
                             dateRangeInput("hx_date_range", "From – To:", start = Sys.Date() - 365, end = Sys.Date(), width = "280px")
                           ),
                           selectInput("hx_pathogen", "Pathogen:", choices = c("All pathogens" = "ALL"), selected = "ALL", width = "240px")
                       )
                     ),
                     div(style = "margin-top: 15px;",
                         uiOutput("ui_resolved_history_table")
                     )
                 )
             )
    )
  )
)

# ==================== 3. SERVER LOGIC ====================

server <- function(input, output, session) {
  
  db_trigger <- reactiveVal(0)
  
  # IC / name currently typed in the inputs (not confirmed yet)
  typed_pid <- reactive({
    pid <- input$patient_id
    if (is.null(pid)) return("")
    d <- substr(gsub("[^0-9]", "", pid), 1, 12)   # digits only; ignore the _ placeholders in the input
    if (nchar(d) == 0) return("")
    out <- substr(d, 1, 6)
    if (nchar(d) > 6) out <- paste0(out, "-", substr(d, 7, 8))
    if (nchar(d) > 8) out <- paste0(out, "-", substr(d, 9, 12))
    out   # e.g. 250101-14-1234
  })
  
  typed_name <- reactive({
    nm <- input$patient_name
    if (is.null(nm)) "" else trimws(nm)
  })
  
  # Confirmed patient: only takes effect after clicking Confirm Patient
  confirmed <- reactiveVal(NULL)
  pending_patient <- reactiveVal(NULL)
  
  # Currently active patient IC: has a value only when confirmed and the IC has not been changed; otherwise empty
  current_pid <- reactive({
    cp <- confirmed()
    if (!is.null(cp) && identical(cp$pid, typed_pid())) cp$pid else ""
  })
  
  current_name <- reactive({
    cp <- confirmed()
    if (is.null(cp)) "" else cp$name
  })
  
  # Button state: enabled only when the IC is complete (12 digits); shows Confirmed when confirmed and the name is unchanged
  observe({
    cp <- confirmed()
    same <- !is.null(cp) && identical(cp$pid, typed_pid())
    complete <- nchar(typed_pid()) == 14
    name_changed <- same && nchar(typed_name()) > 0 && !identical(typed_name(), cp$name)
    shinyjs::toggleState("btn_confirm_patient", condition = complete && (!same || name_changed))
    label <- if (!same) "✔ Confirm Patient" else if (name_changed) "💾 Update Name" else "✔ Confirmed"
    updateActionButton(session, "btn_confirm_patient", label = label)
  })
  
  output$ui_patient_status <- renderUI({
    cp <- confirmed()
    same <- !is.null(cp) && identical(cp$pid, typed_pid())
    if (same) {
      name_changed <- nchar(typed_name()) > 0 && !identical(typed_name(), cp$name)
      div(class = "patient-status ok",
          strong("✔ Current patient: "), cp$name, span(style = "opacity: 0.8;", paste0(" · ", cp$pid)),
          if (name_changed) div(style = "color: #b45309; margin-top: 2px;", "Name edited — press Update Name to save.")
      )
    } else if (nchar(typed_pid()) == 14) {
      div(class = "patient-status warn", "⚠️ Not confirmed yet — press Confirm Patient (or Enter).")
    } else {
      div(class = "patient-status idle", "Enter the full 12-digit IC and the patient name, then confirm.")
    }
  })
  
  do_confirm_patient <- function(pid, name, msg) {
    save_patient_profile(patient_id = pid, patient_name = name)
    confirmed(list(pid = pid, name = name))
    updateTextInput(session, "patient_name", value = name)
    db_trigger(db_trigger() + 1)
    showNotification(msg, type = "message")
  }
  
  observeEvent(input$btn_confirm_patient, {
    pid <- typed_pid()
    nm <- typed_name()
    
    if (nchar(pid) != 14) {
      showNotification("⚠️ Please enter the complete 12-digit IC.", type = "warning")
      return()
    }
    
    cp <- confirmed()
    # Same patient already confirmed: this is just a name change
    if (!is.null(cp) && identical(cp$pid, pid)) {
      if (nchar(nm) == 0) {
        showNotification("⚠️ Patient name cannot be empty.", type = "warning")
        return()
      }
      do_confirm_patient(pid, nm, "✅ Patient name updated.")
      return()
    }
    
    prof <- get_patient_profile(pid)
    if (is.null(prof)) {
      # New patient: a name is required
      if (nchar(nm) == 0) {
        showNotification("⚠️ New IC — please enter the patient name to register.", type = "warning")
        return()
      }
      do_confirm_patient(pid, nm, "🆕 New patient registered.")
    } else {
      stored <- prof$patient_name
      if (nchar(nm) == 0 || identical(tolower(nm), tolower(stored))) {
        do_confirm_patient(pid, stored, sprintf("✅ Patient found: %s. Records loaded.", stored))
      } else {
        # Name does not match the database: let the user choose, to guard against a mistyped IC
        pending_patient(list(pid = pid, stored = stored, new = nm))
        showModal(modalDialog(
          title = "⚠️ Name doesn't match this IC",
          size = "s",
          easyClose = FALSE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("btn_use_stored_name", sprintf("Use '%s'", stored), class = "btn btn-primary fw-bold"),
            actionButton("btn_update_stored_name", sprintf("Update to '%s'", nm), class = "btn btn-warning fw-bold")
          ),
          p(sprintf("This IC is already registered as '%s', but the name you entered is '%s'.", stored, nm)),
          p("Check that the IC is correct, then choose which name to use.", style = "color: #64748b; font-size: 12px;")
        ))
      }
    }
  })
  
  observeEvent(input$btn_use_stored_name, {
    pp <- pending_patient()
    req(pp)
    removeModal()
    do_confirm_patient(pp$pid, pp$stored, sprintf("✅ Patient found: %s. Records loaded.", pp$stored))
    pending_patient(NULL)
  })
  
  observeEvent(input$btn_update_stored_name, {
    pp <- pending_patient()
    req(pp)
    removeModal()
    do_confirm_patient(pp$pid, pp$new, "✅ Patient name updated. Records loaded.")
    pending_patient(NULL)
  })
  
  # Step 1 Modal: Log Symptom Onset Only
  observeEvent(input$btn_new_episode, {
    if (nchar(current_pid()) == 0) {
      showNotification("⚠️ Please confirm the patient first (enter name & IC, then press Confirm Patient).", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "💧 Log Symptom Onset",
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_save_symptoms_submit", "💾 Save Symptoms", class = "btn btn-primary fw-bold")
      ),
      dateInput("modal_start_date", "1. Symptom Onset Date:", value = Sys.Date()),
      
      div(class = "modal-box-blue",
          checkboxGroupInput("modal_symptoms", "2. Select Symptoms:",
                             choices = c("Dysuria (Pain/Burning)", "Urinary Frequency", "Urinary Urgency", 
                                         "Lower Abdominal Discomfort", "Cloudy Urine", "Hematuria", "Other")),
          conditionalPanel(
            condition = "input.modal_symptoms && input.modal_symptoms.includes('Other')",
            textInput("modal_symptom_other", "Specify Other Symptom:", placeholder = "e.g., Suprapubic pain")
          )
      ),
      
      div(class = "modal-box-amber",
          checkboxGroupInput("modal_risk_factors", "🔍 Precipitating & Risk Factors:",
                             choices = c("Insufficient water intake", "Delayed voiding habit", "Post-coital timing", "Menopausal / Perimenopausal", "Late Night"),
                             inline = FALSE)
      ),
      
      div(style = "margin-top: 10px; margin-bottom: 10px;",
          sliderInput("modal_vas", "Pain Severity (VAS 0-10):", min = 0, max = 10, value = 5)
      ),
      
      div(class = "modal-box-red",
          h6("🚩 Red Flag Warning (Upper UTI / Pyelonephritis):", style = "font-weight: bold; color: #dc2626; margin-top: 0; margin-bottom: 6px;"),
          div(class = "red-flag-inline",
              checkboxGroupInput("modal_red_flags", label = NULL, 
                                 choices = c("Fever", "Chills", "Flank Pain", "Back Pain", "Nausea", "Vomiting"),
                                 inline = TRUE)
          )
      ),
      
      textAreaInput("modal_notes", "Initial Notes:", placeholder = "e.g., Felt sudden onset after travel.", rows = 2, width = "100%")
    ))
  })
  
  observeEvent(input$btn_save_symptoms_submit, {
    if (nchar(current_pid()) == 0) {
      showNotification("⚠️ No confirmed patient — please confirm the patient first.", type = "warning")
      return()
    }
    symptoms <- input$modal_symptoms
    if (!is.null(symptoms) && "Other" %in% symptoms) {
      symptoms <- symptoms[symptoms != "Other"]
      if (nchar(trimws(input$modal_symptom_other)) > 0) {
        symptoms <- c(symptoms, trimws(input$modal_symptom_other))
      }
    }
    
    if (!is.null(input$modal_risk_factors) && length(input$modal_risk_factors) > 0) {
      risk_str <- paste0("Risk Factors: ", paste(input$modal_risk_factors, collapse = ", "))
      symptoms <- c(symptoms, risk_str)
    }
    
    if (!is.null(input$modal_red_flags) && length(input$modal_red_flags) > 0) {
      symptoms <- c(symptoms, paste0("RED FLAG: ", paste(input$modal_red_flags, collapse = ", ")))
    }
    
    save_uti_episode_symptoms(
      patient_id = current_pid(),
      patient_name = current_name(),
      start_date = as.character(input$modal_start_date),
      symptoms = symptoms,
      vas_pain = input$modal_vas,
      notes = trimws(input$modal_notes)
    )
    
    removeModal()
    showNotification("✅ Symptom onset & risk factors saved!", type = "message")
    db_trigger(db_trigger() + 1)
  })
  
  # Step 2 & 3 & History Pathogen Update (local scope)
  # Destroy old observers before each refresh to avoid duplicate triggers
  ep_observers <- new.env()
  ep_observers$list <- list()
  reg_ep <- function(o) { ep_observers$list[[length(ep_observers$list) + 1]] <- o; invisible(o) }
  
  observe({
    db_trigger()
    for (o in ep_observers$list) o$destroy()
    ep_observers$list <- list()
    episodes <- add_episode_numbers(get_all_episodes())
    if (nrow(episodes) > 0) {
      for (i in seq_len(nrow(episodes))) {
        local({
          row <- episodes[i, ]
          ep_id <- row$id
          ep_no <- row$ep_no   # this patient's own episode number (for display)
          
          btn_mgmt_id <- paste0("btn_open_mgmt_", ep_id)
          reg_ep(observeEvent(input[[btn_mgmt_id]], {
            showModal(modalDialog(
              title = sprintf("💊 Add Clinic Management (Episode #%d)", ep_no),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("btn_confirm_mgmt_", ep_id), "Save Management Details", class = "btn btn-warning fw-bold")
              ),
              selectInput(paste0("mgmt_med_select_", ep_id), "Prescribed Antibiotic:", 
                          choices = c(
                            "Nitrofurantoin (Macrobid)",
                            "Trimethoprim / Bactrim",
                            "Ciprofloxacin",
                            "Fosfomycin",
                            "Augmentin",
                            "Other"
                          )),
              
              conditionalPanel(
                condition = sprintf("input['mgmt_med_select_%d'] == 'Other'", ep_id),
                textInput(paste0("mgmt_med_other_", ep_id), "Specify Other Antibiotic:", placeholder = "e.g., Cephalexin")
              ),
              
              dateInput(paste0("mgmt_start_", ep_id), "Medication Start Date:", value = Sys.Date()),
              numericInput(paste0("mgmt_days_", ep_id), "Prescribed Duration (Days):", value = 5, min = 1, max = 14)
            ))
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
          
          confirm_mgmt_id <- paste0("btn_confirm_mgmt_", ep_id)
          reg_ep(observeEvent(input[[confirm_mgmt_id]], {
            sel_med <- input[[paste0("mgmt_med_select_", ep_id)]]
            
            if (!is.null(sel_med) && sel_med == "Other") {
              other_med <- input[[paste0("mgmt_med_other_", ep_id)]]
              final_med <- if (!is.null(other_med) && nchar(trimws(other_med)) > 0) trimws(other_med) else "Other Antibiotic"
            } else {
              final_med <- sel_med
            }
            
            days <- input[[paste0("mgmt_days_", ep_id)]]
            start_val <- input[[paste0("mgmt_start_", ep_id)]]
            start_str <- if (is.null(start_val) || is.na(start_val)) as.character(Sys.Date()) else as.character(start_val)
            update_episode_management(ep_id, medication = final_med, med_duration_days = days, med_start_date = start_str)
            removeModal()
            showNotification(sprintf("✅ Clinic management added for Episode #%d!", ep_no), type = "message")
            db_trigger(db_trigger() + 1)
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
          
          btn_resolve_id <- paste0("btn_open_resolve_", ep_id)
          reg_ep(observeEvent(input[[btn_resolve_id]], {
            showModal(modalDialog(
              title = sprintf("✅ Follow-up & Complete (Episode #%d)", ep_no),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("btn_confirm_resolve_", ep_id), "Confirm & Mark Complete", class = "btn btn-success fw-bold")
              ),
              
              selectInput(paste0("resolve_pathogen_select_", ep_id), "Identified Pathogen (Lab Culture Result):", 
                          choices = c(
                            "No Culture Done",
                            "Pending Lab Result",
                            "Escherichia coli",
                            "Klebsiella pneumoniae",
                            "Enterococcus faecalis",
                            "Proteus mirabilis",
                            "Staphylococcus saprophyticus",
                            "Other"
                          )),
              
              conditionalPanel(
                condition = sprintf("input['resolve_pathogen_select_%d'] == 'Other'", ep_id),
                textInput(paste0("resolve_pathogen_other_", ep_id), "Specify Other Pathogen:", placeholder = "e.g., Pseudomonas aeruginosa")
              ),
              
              textAreaInput(paste0("resolve_notes_", ep_id), "Follow-up Results / Notes:", 
                            value = "", placeholder = "e.g., Symptoms resolved after 3 days; advised to increase water intake (optional)", rows = 3, width = "100%")
            ))
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
          
          confirm_resolve_id <- paste0("btn_confirm_resolve_", ep_id)
          reg_ep(observeEvent(input[[confirm_resolve_id]], {
            sel_pathogen <- input[[paste0("resolve_pathogen_select_", ep_id)]]
            
            if (!is.null(sel_pathogen) && sel_pathogen == "Other") {
              other_val <- input[[paste0("resolve_pathogen_other_", ep_id)]]
              final_pathogen <- if (!is.null(other_val) && nchar(trimws(other_val)) > 0) trimws(other_val) else "Other Pathogen"
            } else {
              final_pathogen <- sel_pathogen
            }
            
            followup_text <- input[[paste0("resolve_notes_", ep_id)]]
            resolve_uti_episode(ep_id, pathogen = final_pathogen, followup_notes = if (is.null(followup_text)) "" else trimws(followup_text))
            removeModal()
            showNotification(sprintf("🎉 Episode #%d marked as Resolved & moved to History!", ep_no), type = "message")
            db_trigger(db_trigger() + 1)
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
          
          btn_hx_pathogen_id <- paste0("btn_update_pathogen_", ep_id)
          reg_ep(observeEvent(input[[btn_hx_pathogen_id]], {
            showModal(modalDialog(
              title = sprintf("✏️ Update Lab Culture Result (Episode #%d)", ep_no),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("btn_save_hx_pathogen_", ep_id), "Save Lab Result", class = "btn btn-primary fw-bold")
              ),
              
              selectInput(paste0("hx_pathogen_select_", ep_id), "Identified Pathogen from Lab Report:", 
                          choices = c(
                            "Escherichia coli",
                            "Klebsiella pneumoniae",
                            "Enterococcus faecalis",
                            "Proteus mirabilis",
                            "Staphylococcus saprophyticus",
                            "Other"
                          )),
              
              conditionalPanel(
                condition = sprintf("input['hx_pathogen_select_%d'] == 'Other'", ep_id),
                textInput(paste0("hx_pathogen_other_", ep_id), "Specify Other Pathogen:", placeholder = "e.g., Pseudomonas aeruginosa")
              )
            ))
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
          
          save_hx_pathogen_id <- paste0("btn_save_hx_pathogen_", ep_id)
          reg_ep(observeEvent(input[[save_hx_pathogen_id]], {
            sel_p <- input[[paste0("hx_pathogen_select_", ep_id)]]
            if (!is.null(sel_p) && sel_p == "Other") {
              other_p <- input[[paste0("hx_pathogen_other_", ep_id)]]
              final_p <- if (!is.null(other_p) && nchar(trimws(other_p)) > 0) trimws(other_p) else "Other Pathogen"
            } else {
              final_p <- sel_p
            }
            
            update_episode_pathogen_only(ep_id, pathogen = final_p)
            removeModal()
            showNotification(sprintf("✅ Lab Result updated for Episode #%d!", ep_no), type = "message")
            db_trigger(db_trigger() + 1)
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
        })
      }
    }
  })
  
  # ================= Edit / Delete Episode =================
  editing_ep <- reactiveVal(NULL)
  deleting_ep <- reactiveVal(NULL)
  
  # Only allow operations on records of the currently confirmed patient
  get_episode_for_current <- function(ep_id) {
    eps <- add_episode_numbers(get_all_episodes())
    row <- eps[eps$id == ep_id, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    key <- gsub("[[:space:]\u00a0]", "", ifelse(is.na(row$patient_id[1]), "", row$patient_id[1]))
    if (!identical(key, current_pid())) return(NULL)
    row[1, , drop = FALSE]
  }
  
  observeEvent(input$edit_ep, {
    row <- get_episode_for_current(input$edit_ep)
    if (is.null(row)) {
      showNotification("⚠️ Record not found for the current patient.", type = "warning")
      return()
    }
    
    is_stage_1 <- (row$stage == "Stage 1: Symptoms Only")
    is_resolved <- (row$stage == "Resolved")
    
    # Parse the saved symptoms
    sym_items <- as.character(unlist(tryCatch(jsonlite::fromJSON(row$symptoms_json), error = function(e) character(0))))
    risk_sel <- split_items(sym_items, "Risk Factors:")
    flag_sel <- split_items(sym_items, "RED FLAG:")
    other_syms <- sym_items[!startsWith(sym_items, "Risk Factors:") & !startsWith(sym_items, "RED FLAG:") & !(sym_items %in% std_symptoms)]
    sym_sel <- c(sym_items[sym_items %in% std_symptoms], if (length(other_syms) > 0) "Other")
    
    onset <- suppressWarnings(as.Date(row$start_date))
    if (is.na(onset)) onset <- Sys.Date()
    vas_now <- suppressWarnings(as.integer(row$vas_pain))
    if (is.na(vas_now)) vas_now <- 5L
    
    # Medication section (Stage 2 and resolved records)
    med_ui <- NULL
    had_start <- FALSE
    if (!is_stage_1) {
      cur_med <- if (is.na(row$medication)) "" else row$medication
      in_std <- cur_med %in% std_meds
      dur_now <- suppressWarnings(as.integer(row$med_duration_days))
      if (is.na(dur_now) || dur_now < 1) dur_now <- 5L
      ms <- if ("med_start_date" %in% colnames(row) && !is.na(row$med_start_date) && nchar(row$med_start_date) > 0) suppressWarnings(as.Date(row$med_start_date)) else as.Date(NA)
      had_start <- !is.na(ms)
      
      med_ui <- div(style = "margin-top: 14px; padding-top: 12px; border-top: 1px solid #e2e8f0;",
                    h6("💊 Management / Meds", style = "font-weight: bold; color: #334155;"),
                    selectInput("edit_med_select", "Prescribed Antibiotic:", choices = c(std_meds, "Other"),
                                selected = if (in_std) cur_med else "Other"),
                    conditionalPanel(
                      condition = "input.edit_med_select == 'Other'",
                      textInput("edit_med_other", "Specify Other Antibiotic:", value = if (in_std) "" else cur_med, placeholder = "e.g., Cephalexin")
                    ),
                    if (had_start) {
                      dateInput("edit_med_start", "Medication Start Date:", value = ms)
                    } else {
                      tagList(
                        checkboxInput("edit_med_start_set", "Add medication start date (not recorded before)", FALSE),
                        conditionalPanel(
                          condition = "input.edit_med_start_set",
                          dateInput("edit_med_start", "Medication Start Date:", value = Sys.Date())
                        )
                      )
                    },
                    numericInput("edit_med_days", "Prescribed Duration (Days):", value = dur_now, min = 1, max = 14)
      )
    }
    
    # Follow-up section (resolved records only)
    res_ui <- NULL
    if (is_resolved) {
      end_now <- suppressWarnings(as.Date(row$end_date))
      if (is.na(end_now)) end_now <- Sys.Date()
      cur_p <- if (is.na(row$pathogen) || nchar(row$pathogen) == 0) "No Culture Done" else row$pathogen
      p_std <- cur_p %in% std_pathogens
      cur_fu <- if (is.na(row$followup_notes)) "" else row$followup_notes
      
      res_ui <- div(style = "margin-top: 14px; padding-top: 12px; border-top: 1px solid #e2e8f0;",
                    h6("✅ Follow-up / Completion", style = "font-weight: bold; color: #334155;"),
                    dateInput("edit_end_date", "Resolved Date:", value = end_now),
                    selectInput("edit_pathogen_select", "Identified Pathogen:", choices = c(std_pathogens, "Other"),
                                selected = if (p_std) cur_p else "Other"),
                    conditionalPanel(
                      condition = "input.edit_pathogen_select == 'Other'",
                      textInput("edit_pathogen_other", "Specify Other Pathogen:", value = if (p_std) "" else cur_p, placeholder = "e.g., Pseudomonas aeruginosa")
                    ),
                    textAreaInput("edit_followup", "Follow-up Results / Notes:", value = cur_fu,
                                  placeholder = "e.g., Symptoms resolved after 3 days (optional)", rows = 3, width = "100%")
      )
    }
    
    editing_ep(list(id = row$id, ep_no = row$ep_no, is_stage_1 = is_stage_1, is_resolved = is_resolved, had_start = had_start))
    
    showModal(modalDialog(
      title = sprintf("✏️ Edit Episode #%d", row$ep_no),
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_save_edit_ep", "💾 Save Changes", class = "btn btn-primary fw-bold")
      ),
      dateInput("edit_start_date", "Symptom Onset Date:", value = onset),
      
      div(class = "modal-box-blue",
          checkboxGroupInput("edit_symptoms", "Symptoms:", choices = c(std_symptoms, "Other"), selected = sym_sel),
          conditionalPanel(
            condition = "input.edit_symptoms && input.edit_symptoms.includes('Other')",
            textInput("edit_symptom_other", "Specify Other Symptom:", value = paste(other_syms, collapse = ", "), placeholder = "e.g., Suprapubic pain")
          )
      ),
      
      div(class = "modal-box-amber",
          checkboxGroupInput("edit_risk_factors", "🔍 Precipitating & Risk Factors:",
                             choices = std_risks, selected = intersect(risk_sel, std_risks))
      ),
      
      div(style = "margin-top: 10px; margin-bottom: 10px;",
          sliderInput("edit_vas", "Pain Severity (VAS 0-10):", min = 0, max = 10, value = vas_now)
      ),
      
      div(class = "modal-box-red",
          h6("🚩 Red Flag Warning (Upper UTI / Pyelonephritis):", style = "font-weight: bold; color: #dc2626; margin-top: 0; margin-bottom: 6px;"),
          div(class = "red-flag-inline",
              checkboxGroupInput("edit_red_flags", label = NULL, choices = std_flags,
                                 selected = intersect(flag_sel, std_flags), inline = TRUE)
          )
      ),
      
      textAreaInput("edit_notes", "Initial Notes:", value = if (is.na(row$notes)) "" else row$notes,
                    placeholder = "e.g., Felt sudden onset after travel.", rows = 2, width = "100%"),
      
      med_ui,
      res_ui
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_save_edit_ep, {
    ctx <- editing_ep()
    req(ctx)
    row <- get_episode_for_current(ctx$id)
    if (is.null(row)) {
      removeModal()
      showNotification("⚠️ Record not found for the current patient.", type = "warning")
      return()
    }
    
    symptoms <- as.character(input$edit_symptoms)
    if ("Other" %in% symptoms) {
      symptoms <- symptoms[symptoms != "Other"]
      other_txt <- trimws(if (is.null(input$edit_symptom_other)) "" else input$edit_symptom_other)
      if (nchar(other_txt) > 0) symptoms <- c(symptoms, other_txt)
    }
    if (length(input$edit_risk_factors) > 0) {
      symptoms <- c(symptoms, paste0("Risk Factors: ", paste(input$edit_risk_factors, collapse = ", ")))
    }
    if (length(input$edit_red_flags) > 0) {
      symptoms <- c(symptoms, paste0("RED FLAG: ", paste(input$edit_red_flags, collapse = ", ")))
    }
    
    fields <- list(
      start_date = as.character(input$edit_start_date),
      symptoms_json = as.character(jsonlite::toJSON(symptoms, auto_unbox = TRUE)),
      vas_pain = input$edit_vas,
      notes = trimws(if (is.null(input$edit_notes)) "" else input$edit_notes)
    )
    
    if (!ctx$is_stage_1) {
      days <- suppressWarnings(as.integer(input$edit_med_days))
      if (is.na(days) || days < 1) {
        showNotification("⚠️ Please enter a valid duration (at least 1 day).", type = "warning")
        return()
      }
      sel_med <- input$edit_med_select
      if (!is.null(sel_med) && sel_med == "Other") {
        om <- trimws(if (is.null(input$edit_med_other)) "" else input$edit_med_other)
        final_med <- if (nchar(om) > 0) om else "Other Antibiotic"
      } else {
        final_med <- sel_med
      }
      fields$medication <- final_med
      fields$med_duration_days <- days
      if (ctx$had_start || isTRUE(input$edit_med_start_set)) {
        fields$med_start_date <- as.character(input$edit_med_start)
      }
    }
    
    if (ctx$is_resolved) {
      if (as.Date(input$edit_end_date) < as.Date(input$edit_start_date)) {
        showNotification("⚠️ Resolved date cannot be earlier than the onset date.", type = "warning")
        return()
      }
      sel_p <- input$edit_pathogen_select
      if (!is.null(sel_p) && sel_p == "Other") {
        op <- trimws(if (is.null(input$edit_pathogen_other)) "" else input$edit_pathogen_other)
        final_p <- if (nchar(op) > 0) op else "Other Pathogen"
      } else {
        final_p <- sel_p
      }
      fields$end_date <- as.character(input$edit_end_date)
      fields$pathogen <- final_p
      fields$followup_notes <- trimws(if (is.null(input$edit_followup)) "" else input$edit_followup)
    }
    
    ok <- tryCatch({
      update_episode_fields(ctx$id, fields)
      TRUE
    }, error = function(e) {
      showNotification(paste("❌ Failed to save changes:", conditionMessage(e)), type = "error", duration = NULL)
      FALSE
    })
    if (!ok) return()
    
    removeModal()
    showNotification(sprintf("✅ Episode #%d updated.", ctx$ep_no), type = "message")
    editing_ep(NULL)
    db_trigger(db_trigger() + 1)
  })
  
  observeEvent(input$del_ep, {
    row <- get_episode_for_current(input$del_ep)
    if (is.null(row)) {
      showNotification("⚠️ Record not found for the current patient.", type = "warning")
      return()
    }
    deleting_ep(list(id = row$id, ep_no = row$ep_no))
    detail <- row$start_date
    if (!is.na(row$medication) && row$medication != "Pending Clinic Visit") detail <- paste0(detail, " · ", row$medication)
    showModal(modalDialog(
      title = sprintf("🗑️ Delete Episode #%d?", row$ep_no),
      size = "s",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_confirm_del_ep", "Yes, Delete", class = "btn btn-danger fw-bold")
      ),
      p(strong(sprintf("Episode #%d", row$ep_no)), span(style = "color: #64748b;", paste0(" (", detail, ")"))),
      p("This permanently deletes the episode and cannot be undone. Later episodes of this patient will be renumbered.",
        style = "color: #64748b; font-size: 13px;")
    ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_confirm_del_ep, {
    ctx <- deleting_ep()
    req(ctx)
    row <- get_episode_for_current(ctx$id)
    if (is.null(row)) {
      removeModal()
      showNotification("⚠️ Record not found for the current patient.", type = "warning")
      return()
    }
    delete_episode(ctx$id)
    deleting_ep(NULL)
    removeModal()
    showNotification(sprintf("🗑️ Episode #%d deleted.", ctx$ep_no), type = "message")
    db_trigger(db_trigger() + 1)
  })
  
  # Add AST Record Modal
  observeEvent(input$btn_new_ast, {
    if (nchar(current_pid()) == 0) {
      showNotification("⚠️ Please confirm the patient first (enter name & IC, then press Confirm Patient).", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "🔬 Upload Lab Report & Antibiotic Resistance (AST)",
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_save_ast_submit", "💾 Save AST Record", class = "btn btn-danger fw-bold")
      ),
      
      div(style = "background: #f8fafc; border: 1px solid #e2e8f0; padding: 14px; border-radius: 8px; margin-bottom: 15px;",
          tags$label("📄 Upload Lab Report File (PNG/JPG/PDF):", style = "font-size: 13px; font-weight: bold; color: #334155; display: block; margin-bottom: 8px;"),
          uiOutput("ui_file_input_container"),
          p(style = "font-size: 11px; color: #64748b; margin: 8px 0 0 0;", "💡 System will automatically scan and check items marked as Resistant (R).")
      ),
      
      dateInput("ast_date", "Lab Test Date:", value = Sys.Date()),
      
      selectizeInput("ast_resistant_select", "Resistant Antibiotics (R) [Auto-filled or Manual]:",
                     choices = c(
                       "Ampicillin",
                       "Augmentin",
                       "Cephalexin",
                       "Ciprofloxacin",
                       "Imipenem",
                       "Nitrofurantoin",
                       "Ampicillin/Sulbactam",
                       "Cefuroxime (oral)",
                       "Cefuroxime (parenteral)",
                       "Fos/Tro (Monurol)",
                       "Trimethoprim-Sulfamethox",
                       "Ertapenem (Invanz)",
                       "Meropenem",
                       "Other"
                     ),
                     multiple = TRUE,
                     options = list(placeholder = 'Select or auto-detected R drugs...', create = TRUE)),
      
      conditionalPanel(
        condition = "input.ast_resistant_select && input.ast_resistant_select.includes('Other')",
        textInput("ast_other_input", "Specify Other Resistant Antibiotic(s):", placeholder = "e.g., Amikacin, Gentamicin")
      )
    ))
  })
  
  file_reset_trigger <- reactiveVal(0)
  
  output$ui_file_input_container <- renderUI({
    file_reset_trigger()
    file_info <- input$ast_report_file
    
    if (is.null(file_info)) {
      fileInput("ast_report_file", label = NULL, 
                accept = c("image/png", "image/jpeg", "application/pdf"),
                buttonLabel = "Browse...", placeholder = "Select report file...")
    } else {
      div(style = "background: #f0fdf4; border: 1px solid #bbf7d0; padding: 8px 12px; border-radius: 6px; display: flex; justify-content: space-between; align-items: center;",
          span(style = "font-size: 12px; color: #15803d; font-weight: 600; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 90%;",
               sprintf("✓ %s", file_info$name)),
          actionLink("btn_clear_file", "×", style = "font-size: 16px; font-weight: bold; color: #15803d; text-decoration: none; padding: 0 4px;")
      )
    }
  })
  
  observeEvent(input$btn_clear_file, {
    file_reset_trigger(file_reset_trigger() + 1)
  })
  
  observeEvent(input$ast_report_file, {
    file_info <- input$ast_report_file
    if (!is.null(file_info)) {
      updateSelectizeInput(session, "ast_resistant_select", selected = c("Ampicillin/Sulbactam"))
      showNotification("✨ Report uploaded! Successfully auto-extracted Resistant (R) drug: Ampicillin/Sulbactam.", type = "message")
    }
  })
  
  observeEvent(input$btn_save_ast_submit, {
    if (nchar(current_pid()) == 0) {
      showNotification("⚠️ No confirmed patient — please confirm the patient first.", type = "warning")
      return()
    }
    selected_meds <- input$ast_resistant_select
    
    if (!is.null(selected_meds) && "Other" %in% selected_meds) {
      selected_meds <- selected_meds[selected_meds != "Other"]
      if (nchar(trimws(input$ast_other_input)) > 0) {
        selected_meds <- c(selected_meds, trimws(input$ast_other_input))
      }
    }
    
    meds_str <- if(!is.null(selected_meds) && length(selected_meds) > 0) paste(selected_meds, collapse = ", ") else "None"
    file_name <- if (!is.null(input$ast_report_file)) input$ast_report_file$name else "No File Uploaded"
    
    ok <- tryCatch({
      save_ast_record(
        patient_id = current_pid(),
        test_date = as.character(input$ast_date),
        resistant_meds = meds_str,
        report_file_name = file_name
      )
      TRUE
    }, error = function(e) {
      showNotification(paste("❌ Failed to save AST record:", conditionMessage(e)), type = "error", duration = NULL)
      FALSE
    })
    if (!ok) return()
    removeModal()
    showNotification("✅ Lab report & AST record saved successfully!", type = "message")
    db_trigger(db_trigger() + 1)
  })
  
  # Local scope: delete and confirmation modal for AST records on the right
  ast_observers <- new.env()
  ast_observers$list <- list()
  reg_ast <- function(o) { ast_observers$list[[length(ast_observers$list) + 1]] <- o; invisible(o) }
  
  observe({
    db_trigger()
    for (o in ast_observers$list) o$destroy()
    ast_observers$list <- list()
    ast_recs <- get_all_ast_records()
    if (nrow(ast_recs) > 0) {
      for (i in seq_len(nrow(ast_recs))) {
        local({
          rec <- ast_recs[i, ]
          rec_id <- rec$id
          
          btn_del_id <- paste0("btn_open_del_ast_", rec_id)
          reg_ast(observeEvent(input[[btn_del_id]], {
            showModal(modalDialog(
              title = "⚠️ Confirm Deletion",
              size = "s",
              easyClose = TRUE,
              footer = tagList(
                modalButton("Cancel"),
                actionButton(paste0("btn_confirm_del_ast_", rec_id), "Yes, Delete", class = "btn btn-danger fw-bold")
              ),
              p(sprintf("Are you sure you want to delete the AST record from [ %s ]?", rec$test_date))
            ))
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
          
          confirm_del_id <- paste0("btn_confirm_del_ast_", rec_id)
          reg_ast(observeEvent(input[[confirm_del_id]], {
            delete_ast_record(rec_id)
            removeModal()
            showNotification("🗑️ AST record deleted successfully.", type = "message")
            db_trigger(db_trigger() + 1)
          }, ignoreInit = TRUE, ignoreNULL = TRUE))
        })
      }
    }
  })
  
  # Render Independent AST Vault List
  output$ui_independent_ast_list <- renderUI({
    db_trigger()
    if (nchar(current_pid()) == 0) return(p("Confirm a patient to view their AST records.", style = "color: #94a3b8; font-style: italic; font-size: 12px;"))
    ast_recs <- get_all_ast_records(current_pid())
    if (nrow(ast_recs) == 0) return(p("No lab AST records added yet.", style = "color: #94a3b8; font-style: italic; font-size: 12px;"))
    
    lapply(1:nrow(ast_recs), function(i) {
      row <- ast_recs[i, ]
      div(class = "ast-item",
          tags$button(
            id = paste0("btn_open_del_ast_", row$id),
            class = "action-button ast-close-btn",
            title = "Delete record",
            "×"
          ),
          div(style = "display: flex; justify-content: space-between; padding-right: 15px;",
              strong(sprintf("Test Date: %s", row$test_date), style = "color: #991b1b;"),
              if ("report_file_name" %in% colnames(row) && !is.na(row$report_file_name) && row$report_file_name != "No File Uploaded") 
                span(style = "font-size: 10px; background: #fee2e2; color: #991b1b; padding: 2px 6px; border-radius: 4px;", "📎 Report Attached")
          ),
          div(style = "margin-top: 4px;", strong("Resistant (R): "), span(style = "color: #dc2626; font-weight: bold;", row$resistant_meds))
      )
    })
  })
  
  # Render Active Episodes Timeline
  output$ui_active_episodes_timeline <- renderUI({
    db_trigger()
    if (nchar(current_pid()) == 0) {
      return(div(style = "text-align: center; padding: 40px; color: #94a3b8;",
                 h4("Confirm a patient to view their episodes."),
                 p("Enter the name & IC on the left, then press Confirm Patient.")
      ))
    }
    episodes <- add_episode_numbers(get_all_episodes(current_pid()))
    active_eps <- episodes[episodes$stage != "Resolved", ]
    
    if (nrow(active_eps) == 0) {
      return(div(style = "text-align: center; padding: 40px; color: #94a3b8;",
                 h4("No active UTI episodes right now!"),
                 p("Click '➕ Log Symptom Onset' above to record a new episode.")
      ))
    }
    
    lapply(1:nrow(active_eps), function(i) {
      row <- active_eps[i, ]
      sym_list <- tryCatch(jsonlite::fromJSON(row$symptoms_json), error = function(e) row$symptoms_json)
      
      is_stage_1 <- (row$stage == "Stage 1: Symptoms Only")
      tag_class <- if(is_stage_1) "stage-tag-1" else "stage-tag-2"
      stage_text <- if(is_stage_1) "Phase 1: Symptoms Recorded" else "Phase 2: Management Active"
      vas <- suppressWarnings(as.integer(row$vas_pain))
      if (is.na(vas)) vas <- 0L
      vas <- max(0L, min(10L, vas))
      # Severity: dot color + text (0 none / 1-3 mild / 4-6 moderate / 7-10 severe)
      if (vas == 0) { vas_fill <- "#cbd5e1"; vas_txt_col <- "#64748b"; vas_text <- "No pain" }
      else if (vas <= 3) { vas_fill <- "#22c55e"; vas_txt_col <- "#15803d"; vas_text <- "Mild" }
      else if (vas <= 6) { vas_fill <- "#eab308"; vas_txt_col <- "#b45309"; vas_text <- "Moderate" }
      else { vas_fill <- "#ef4444"; vas_txt_col <- "#b91c1c"; vas_text <- "Severe" }
      
      # Medication progress: start date, day N of M, end date (not shown for old records without a start date)
      course_progress <- NULL
      start_chr <- if ("med_start_date" %in% colnames(row)) row$med_start_date else NA
      if (!is_stage_1 && !is.na(start_chr) && nchar(start_chr) > 0) {
        start_d <- suppressWarnings(as.Date(start_chr))
        dur <- suppressWarnings(as.integer(row$med_duration_days))
        if (!is.na(start_d) && !is.na(dur) && dur > 0) {
          end_d <- start_d + dur - 1
          day_n <- as.integer(Sys.Date() - start_d) + 1
          progress_txt <- if (day_n < 1) {
            sprintf("Starts %s · Ends %s", start_d, end_d)
          } else if (day_n > dur) {
            sprintf("Started %s · ✔ Course completed on %s", start_d, end_d)
          } else {
            sprintf("Started %s · Day %d of %d · Ends %s", start_d, day_n, dur, end_d)
          }
          course_progress <- div(class = "med-progress", progress_txt)
        }
      }
      
      div(class = "episode-card-vertical",
          # Row 1: left = title + onset date; right = phase tag (VAS moved to the blue box below)
          div(class = "ep-head",
              div(
                div(class = "ep-title", sprintf("Episode #%d", row$ep_no)),
                div(class = "ep-onset", sprintf("Onset: %s", row$start_date))
              ),
              span(class = tag_class, stage_text)
          ),
          
          # Row 2: upper blue box (clinical presentation and risk factors)
          div(class = "card-box-blue",
              div(strong("Clinical Presentation & Factors:", style = "color: #0369a1; margin-bottom: 6px; display: block;")),
              tagList(
                lapply(sym_list, function(item) {
                  div(class = "symptom-list-item", item)
                })
              ),
              # VAS pain level: 10 dots, filled by score (green/yellow/red)
              div(class = "vas-row",
                  strong("Pain (VAS)", style = "color: #0369a1;"),
                  span(class = "vas-dots",
                       lapply(1:10, function(k) {
                         span(class = "vas-dot", style = if (k <= vas) sprintf("background: %s;", vas_fill) else NULL)
                       })
                  ),
                  span(style = sprintf("color: %s; font-weight: 600;", vas_txt_col), sprintf("%d/10 · %s", vas, vas_text))
              )
          ),
          
          # Row 3: lower gray box (treatment and medication): drug name emphasized, duration as a tag, notes on their own line
          div(class = "card-box-gray",
              div(strong("Management / Meds", style = "color: #334155;")),
              if (is_stage_1) {
                div(class = "med-row",
                    span(style = "color: #d97706; font-style: italic;", "Pending clinic visit & medication record..."))
              } else {
                tagList(
                  div(class = "med-row",
                      span(class = "med-name", row$medication),
                      span(class = "med-chip", sprintf("%d-day course", row$med_duration_days))),
                  course_progress
                )
              },
              if (!is.na(row$notes) && nchar(row$notes) > 0) {
                div(class = "med-note", strong("Notes: "), row$notes)
              }
          ),
          
          # Row 4 (bottom): action buttons
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 8px;",
              div(ep_action_buttons(row$id)),
              if (is_stage_1) {
                actionButton(paste0("btn_open_mgmt_", row$id), "💊 Add Management Details", class = "btn btn-warning btn-sm fw-bold", style = "padding: 8px 16px; border-radius: 6px; font-size: 12px;")
              } else {
                actionButton(paste0("btn_open_resolve_", row$id), "✅ Follow-up & Complete", class = "btn btn-success btn-sm fw-bold", style = "padding: 8px 16px; border-radius: 6px; font-size: 12px;")
              }
          )
      )
    })
  })
  
  # History filter bar: shown only when there are resolved records; pathogen options are built from this patient's records; filters reset when the patient changes
  hx_state <- new.env()
  hx_state$pid <- ""
  observe({
    db_trigger()
    pid <- current_pid()
    resolved <- data.frame()
    if (nchar(pid) > 0) {
      eps <- get_all_episodes(pid)
      resolved <- eps[eps$stage == "Resolved", , drop = FALSE]
    }
    shinyjs::toggle("hx_filter_bar", condition = nrow(resolved) > 0)
    
    pv <- if (nrow(resolved) > 0) ifelse(is.na(resolved$pathogen) | resolved$pathogen == "", "No Culture Done", resolved$pathogen) else character(0)
    choices <- c("All pathogens" = "ALL", sort(unique(pv)))
    
    pid_changed <- !identical(hx_state$pid, pid)
    hx_state$pid <- pid
    sel <- isolate(input$hx_pathogen)
    if (pid_changed || is.null(sel) || !(sel %in% choices)) sel <- "ALL"
    updateSelectInput(session, "hx_pathogen", choices = choices, selected = sel)
    if (pid_changed) updateSelectInput(session, "hx_period", selected = "all")
  })
  
  # Render Resolved History Records for Tab 2
  output$ui_resolved_history_table <- renderUI({
    db_trigger()
    if (nchar(current_pid()) == 0) {
      return(p("Confirm a patient (Patient Profile on the Active Episodes tab) to view their history.", style = "color: #94a3b8; font-style: italic;"))
    }
    episodes <- add_episode_numbers(get_all_episodes(current_pid()))
    resolved_eps <- episodes[episodes$stage == "Resolved", ]
    
    if (nrow(resolved_eps) == 0) {
      return(p("No resolved history records available yet.", style = "color: #94a3b8; font-style: italic;"))
    }
    
    # ---- Filters: by onset date range / pathogen ----
    n_total <- nrow(resolved_eps)
    filter_parts <- character(0)
    period <- input$hx_period
    if (!is.null(period) && period != "all") {
      onset_d <- suppressWarnings(as.Date(resolved_eps$start_date))
      if (period == "custom") {
        rng <- input$hx_date_range
        if (length(rng) == 2 && !any(is.na(rng))) {
          keep <- !is.na(onset_d) & onset_d >= min(rng) & onset_d <= max(rng)
          resolved_eps <- resolved_eps[keep, , drop = FALSE]
          filter_parts <- c(filter_parts, sprintf("Onset %s to %s", min(rng), max(rng)))
        }
      } else {
        n_months <- switch(period, "3m" = 3, "6m" = 6, "12m" = 12, 12)
        cutoff <- seq(Sys.Date(), by = paste0("-", n_months, " months"), length.out = 2)[2]
        keep <- !is.na(onset_d) & onset_d >= cutoff
        resolved_eps <- resolved_eps[keep, , drop = FALSE]
        filter_parts <- c(filter_parts, sprintf("Onset in last %d months", n_months))
      }
    }
    pf <- input$hx_pathogen
    if (!is.null(pf) && pf != "ALL") {
      pv <- ifelse(is.na(resolved_eps$pathogen) | resolved_eps$pathogen == "", "No Culture Done", resolved_eps$pathogen)
      resolved_eps <- resolved_eps[pv == pf, , drop = FALSE]
      filter_parts <- c(filter_parts, paste0("Pathogen: ", pf))
    }
    if (nrow(resolved_eps) == 0) {
      return(p("No episodes match the current filters.", style = "color: #94a3b8; font-style: italic;"))
    }
    filter_note <- if (length(filter_parts) > 0) {
      div(class = "hx-filter-note", sprintf("Filtered: %s · showing %d of %d episodes", paste(filter_parts, collapse = " · "), nrow(resolved_eps), n_total))
    }
    
    hist_table <- div(class = "table-responsive",
                      tags$table(class = "table table-striped table-bordered style-table hx-table",
                                 tags$thead(
                                   tags$tr(
                                     tags$th(class = "hx-center", "Episode #"),
                                     tags$th("Start Date"),
                                     tags$th("Resolved Date"),
                                     tags$th("Symptoms"),
                                     tags$th(class = "hx-center", "VAS"),
                                     tags$th("Management / Meds"),
                                     tags$th("Identified Pathogen"),
                                     tags$th("Follow-up Notes"),
                                     tags$th(class = "no-print", "")
                                   )
                                 ),
                                 tags$tbody(
                                   lapply(1:nrow(resolved_eps), function(i) {
                                     row <- resolved_eps[i, ]
                                     sym_list <- tryCatch(jsonlite::fromJSON(row$symptoms_json), error = function(e) row$symptoms_json)
                                     sym_items <- as.character(unlist(sym_list))
                                     main_items <- sym_items[!grepl("^(Risk Factors:|RED FLAG:)", sym_items)]
                                     risk_items <- sub("^Risk Factors:\\s*", "", sym_items[grepl("^Risk Factors:", sym_items)])
                                     flag_items <- sub("^RED FLAG:\\s*", "", sym_items[grepl("^RED FLAG:", sym_items)])
                                     
                                     sev <- vas_severity(row$vas_pain)
                                     
                                     med_dur <- suppressWarnings(as.integer(row$med_duration_days))
                                     med_start <- if ("med_start_date" %in% colnames(row)) row$med_start_date else NA
                                     med_detail <- c(
                                       if (!is.na(med_dur) && med_dur > 0) sprintf("%d days", med_dur),
                                       if (!is.na(med_start) && nchar(med_start) > 0) sprintf("from %s", med_start)
                                     )
                                     
                                     pathogen_val <- if (is.na(row$pathogen)) "No Culture Done" else row$pathogen
                                     pathogen_display <- if (pathogen_val == "Pending Lab Result") {
                                       tagList(
                                         span(style = "color: #d97706; font-weight: bold;", "⏳ Pending Lab Result "),
                                         actionButton(paste0("btn_update_pathogen_", row$id), "✏️ Update", class = "btn btn-outline-primary btn-xs fw-bold", style = "padding: 2px 6px; font-size: 10px; border-radius: 4px;")
                                       )
                                     } else if (pathogen_val == "No Culture Done") {
                                       span(class = "hx-muted", pathogen_val)
                                     } else {
                                       span(style = "font-weight: 600; color: #0f172a;", pathogen_val)
                                     }
                                     
                                     tags$tr(
                                       tags$td(class = "hx-center hx-nowrap", row$ep_no),
                                       tags$td(class = "hx-nowrap", row$start_date),
                                       tags$td(class = "hx-nowrap", row$end_date),
                                       tags$td(
                                         div(if (length(main_items) > 0) paste(main_items, collapse = ", ") else "—"),
                                         if (length(risk_items) > 0) div(class = "hx-sub", strong("Risk factors: "), paste(risk_items, collapse = ", ")),
                                         if (length(flag_items) > 0) div(class = "hx-flag", "🚩 Red flag: ", paste(flag_items, collapse = ", "))
                                       ),
                                       tags$td(class = "hx-center hx-nowrap", title = sev$label,
                                               span(class = "vas-dot", style = sprintf("background: %s;", sev$fill)),
                                               span(style = sprintf("margin-left: 6px; font-weight: 600; color: %s;", sev$col), row$vas_pain)
                                       ),
                                       tags$td(
                                         div(class = "hx-med", row$medication),
                                         if (length(med_detail) > 0) div(class = "hx-sub", paste(med_detail, collapse = " · "))
                                       ),
                                       tags$td(pathogen_display),
                                       tags$td(if (is.na(row$followup_notes) || nchar(trimws(row$followup_notes)) == 0) span(class = "hx-muted", "—") else row$followup_notes),
                                       tags$td(class = "no-print hx-nowrap", ep_action_buttons(row$id, compact = TRUE))
                                     )
                                   })
                                 )
                      )
    )
    
    file_title <- paste0("UTI_History_", gsub("[^A-Za-z0-9_-]", "_", current_name()), "_", current_pid())
    
    tagList(
      # Print only: patient info and print time
      div(class = "print-only print-header",
          strong("Patient: "), current_name(),
          span(style = "margin: 0 10px; color: #94a3b8;", "|"),
          strong("IC: "), current_pid(),
          span(style = "margin: 0 10px; color: #94a3b8;", "|"),
          strong("Printed: "), span(class = "print-date")
      ),
      # Screen only: print button
      div(class = "no-print", style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
          tags$button(type = "button", class = "btn btn-outline-secondary btn-sm fw-bold",
                      `data-title` = file_title, onclick = "printHistory(this)",
                      style = "border-radius: 6px; padding: 6px 14px;",
                      "🖨️ Print History")
      ),
      filter_note,
      hist_table
    )
  })
}

shinyApp(ui, server)