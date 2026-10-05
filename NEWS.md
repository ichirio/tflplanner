# tflplanner (development version)

- **Step 2 as lists** (#164).  2-2: [New] [Copy] [Delete...] over the list
  of the report's analyses; an analysis opens below the list when clicked
  (another click closes it), so the step starts as the list alone.  Copy
  makes the analysis again after it, a stack with the ones inside it
  (`copy_analysis()`).  2-1: [Copy] [Delete...] over the analysis data; its
  form opens below the list, not in a dialog (`copy_analysis_data()`).  The
  form's argument headings have an (i): the hint of that argument for that
  function, as a tooltip, instead of a line under each field.

- **Make a report, step 2: the analysis data, then the analyses** (#161).
  "2-1 The analysis data -- what is analysed" has two groups, made in this
  order: the subjects (an analysis set's own data kept to the report's
  subjects: one row a subject, the denominator) and the data analysed (a
  dataset kept to those subjects, the rows a condition keeps, columns taken
  or made, kept, one row per ...), each with its form and a preview.  The
  data a report's analyses read without a name can be given one.  "2-2 The
  analyses -- what is computed": the Data field offers the analysis data;
  choosing one sets a blank denominator to its subjects; a report's first
  analysis is offered its subjects in one step.  The Japanese word is
  解析データ.  In R: `set_analysis_data()` (with `subjects`, `keep`),
  `remove_analysis_data()`, `name_analysis_data()`.

- **The top tabs follow the work** (#157): Study | Data | Report list |
  Make a report | Runs.  **Make a report** has the report chosen on the left
  (the only place a report is chosen) and its steps: 1 Code lists (the
  codelists sheet; optional) | 2 ARD | 3 Content (by its kind, and its data
  code) | 4 Page and output (the page sheets and preview, and the
  program), each with a mark of its state.  What was study-wide on the ARD
  tab is on the Data tab (datasets, analysis sets, analysis data, the study
  ARD, the ARDs taken in) and the Study tab (study keys, the setup code,
  own functions).  The screens themselves are as they were.

- **The sample shows an ard_stack** (#156).  SAMPLE-01 has a report
  T-14-1-1S: T-14-1-1's table, its ARD one `cards::ard_stack()` analysis
  with the continuous and the categorical analyses inside it, the column N
  and the total N made by the stack itself (no BIGN / TOTAL rows).  Its
  table is T-14-1-1's to the byte (but its number); the other reports are
  as they were.  The ARD tab shows it as a stack, to compare with
  T-14-1-1.

- **A report's own code list reaches its ARD** (#151, tflspec #137).  The
  ARD programs, the preview and the ARD's state get the whole codelists
  sheet: a report's ARD program makes factors by the study's rows and the
  report's own (which replace the study's of the same variable and
  value), as its tables do.  The analysis data sheet has tflspec's new
  columns `subjects` (a closed choice: the analysis data) and `keep`.
  Needs tflspec 0.0.24.9046.

- **A temporary home is not remembered** (#150).  `setup_tflplanner(home
  = )` with a folder in the temporary folder (a script's, a test's) uses it
  for the session only: remembered, every later session opened a folder
  that is deleted when the R session ends (a screenshot script did this to
  a user's home).  The screenshot script and the tests keep their own
  `tools::R_user_dir()` folders.

- **A shortcut that security software stops is made another way** (#149).
  On Windows, `add_shortcut()` makes each shortcut on its own, and one the
  VBScript could not write -- security software may stop a script from
  writing to the desktop without a message, and then nothing after it was
  made either -- is made again by PowerShell.  If neither can, the error
  says so and how to allow it or to make the shortcut by hand.

- **Start from the setting a word means** (#144).  A word of the
  function search can mean a setting as well as a function: PROC LOGISTIC
  is `ard_regression()` with `method = "glm", method.args =
  list(family = binomial), exponentiate = TRUE`; PROC FREQ CHISQ is
  `correct = FALSE` (as SAS); Clopper-Pearson is
  `method = "clopper-pearson"`; MMRM is `method = "mmrm", package =
  "mmrm"`.  A function found by such a word shows its setting, and once
  picked, "Start with this setting" fills its argument fields (Apply
  writes them).  Every setting is run in the tests.  The company sheet
  `ard_fn_keywords` may say a setting too (column `args`).

- **The ARD definition's analysis data** (#146, tflspec #135).  The sheet
  `analysis_data` -- named data the analyses read: from a dataset or an
  analysis data above, a population's subjects, a condition, columns of
  the population added, derived, one row per subject and phase -- is
  saved with the study and has a grid of its own on the ARD tab ("Analysis
  data"); an analysis names one in `data`, or as its denominator.  A study
  saved before reads it empty.  The screens to make and use them come
  next.  Needs tflspec 0.0.24.9045.

- **Your own words for the function search** (#140).  A function of
  one's own is found by the comment lines right above it in its file,
  `# tflplanner-keywords: risk difference, PROC FREQ RISKDIFF` (a plain
  comment, not roxygen's `#'`: a package of the company's functions keeps
  them out of its help).  Own functions shows them, and a new function
  from a template has the line to fill in.  The company standards have a
  sheet `ard_fn_keywords` (the dictionary's columns, empty by default)
  for in-house words for any function (`%m_ttest`), added to
  tflplanner's.

- **Analyses are written with the cards function, not a company keyword**
  (#142).  The ARD tab's "Company standard" list shows only the keywords
  a company added or changed; tflspec's own (continuous, categorical,
  hierarchical ...) are their functions (`cards::ard_summary`,
  `cards::ard_tabulate` ...), which get the same statistics, defaults and
  formats (tflspec #133).  The sample study, a new study's first table and
  a stack's subjects per group / total N rows write the function.  A
  study that names a keyword keeps it: it runs as before and stays in its
  list.  Subjects (no function) is with "Subjects and attributes", custom
  code with "Own and code".  Needs tflspec 0.0.24.9044.

- **Closing the app** (#137).  A Close button (where the app runs on this
  computer) asks about unsaved changes -- save and close, close without
  saving, or cancel -- and stops tflplanner.  Inside RStudio's Shiny window
  or Viewer the page no longer holds the window on unsaved changes: they
  show no question, so the window would not close at all.  `run_app()`
  opens the system's own web browser by default, as the desktop shortcut
  does (the address handed to the system, not to RStudio's browser
  option); `launch.browser =` is followed when given.

- **Find a function by what it computes** (#135).  The ARD form's
  function search reads a dictionary of the words statisticians use: a
  statistic (`SMD`, `odds ratio`, `hazard ratio`), a SAS procedure
  (`PROC LOGISTIC`, `PROC FREQ`, `LSMEANS`), an R function (`t.test`), in
  English or Japanese.  Several words narrow it (`t test paired`); full- and
  half-width letters, hiragana and katakana, a long vowel mark or a middle
  dot do not matter; a near spelling (`wilcoxn`) is offered when little
  else is found.  Each row says why it was found (the word, and how: e.g.
  `method = "glm"`, `exponentiate = TRUE` for an odds ratio) and its
  category; a word with no function of its own (MMRM, a correlation)
  leads to the nearest one or to custom code.  A short word (`or`, `cox`)
  is a whole word only.  The chooser itself is smaller: one line once a
  function is chosen (`method` and its name; Change opens the list, a
  pick closes it), and the categories are a filter beside the search
  (all of them by default; both narrow the list); a company with no
  keywords of its own has no Company standard category.

- **An analysis can be deleted on the ARD tab** (#136): its form has
  "Delete the analysis...", asked first -- one of its own, or one inside a
  stack (the stack keeps the others; a stack itself keeps its own Delete).
  `remove_analysis()` does the same in R.  The report's ARD line says what
  its state means and the next step: "needs making again (its definition
  has changed)" with "Make the ARD again", "not made yet" with "Make the
  ARD", the reason in a tooltip (was "Outdated" and "Preview").

- **A report's program is named by its own file** (tflspec #131).  A new
  study's default report row no longer says `program = {output_id}.R`:
  `{PROGRAM}` is the file name of the program that runs, and
  `programs/tfl/<output_id>` only when none is found (rtfreporter's
  `program_fallback`, completed to `.R`).  A study saved with that old
  default on its default report row reads it as blank (it was
  tflplanner's default, not a choice); a report's own `program`, or
  another default, is kept and said.  The program files are still
  `programs/tfl/<output_id>.R`.  Needs tflspec 0.0.24.9043 and rtfreporter
  0.8.2.9024.

- **Tokens of your own on the Page tab** (tflspec #129).  Reports > Page
  has a `tokens` table beside titles and footnotes: one row a token
  (`name` `STUDY`, `value` `ABC-123`), the study's defaults and the
  report's own, `(none)` taking a default out; a header, footer, title or
  footnote then says `{STUDY}`.  The tokens help names them; in Japanese
  too.  Needs tflspec 0.0.24.9042 and rtfreporter 0.8.2.9023.

- **The ARD form's function search ignores case** (#129).  It asked
  `grepl()` for `fixed` and `ignore.case` together, and R drops
  `ignore.case` then: "T TEST" found nothing, and every search warned.
  The figure preview leaves out a dataset that has no file yet without a
  warning (as before, the design's check says so).

- **The tokens help names `{PROGRAM_FULL}`** (tflspec #127), the
  program's absolute path (rtfreporter #560), in Japanese too.  Needs
  tflspec 0.0.24.9041 and rtfreporter 0.8.2.9021.

- **A regression's formula from columns; the data choices with their
  subjects** (#123).  A formula argument (`cardx::ard_regression()`,
  `ard_emmeans_*()` ...) can be written from columns -- the response, the
  terms (the analysis's groups first), the first two's interaction -- and
  `ard_regression()`'s fitting function is chosen from the usual ones.  The
  data choices say how many subjects (and records) each reads
  (`adae_saf: 1191 records, 225 subjects`), counted once when the study is
  opened, on all the rows, as the ARD programs make the data.

- **Own functions, polished** (#122).  The last try keeps the whole ARD's
  rows, its errors, warnings and notes, and who tried it; a name that is a
  cards / cardx function is refused (it would hide it); Try says it is
  running; New function... says when the company's folder cannot be
  written; See the difference marks the lines the other file has not; Use
  in this study says every report's ARD is outdated then; where the title
  and description come from is said; a new function's notice gives its file
  and opens its folder.

- **Less English on the Japanese screens** (#124).
  The arguments of the "(whole script)" figure types (tflspec's
  `tfl_fig_schema()`, 50 labels and help texts) and the old names of six
  cards / cardx functions (an analysis may still name one) are in
  Japanese.  The common errors -- a study or report ID taken, a code list,
  an ARD function's name, the company standards' folder, a report's ARD
  not made yet ... -- get a lead in the app's language, the message itself
  kept after it; an error no pattern knows shows its message alone.

- **The figure guide follows the sample again** (docs).  `ja-figures`
  walks the three ways in order -- a template, then the designer (a new
  F-14-2-4 made like the sample's F-14-2-3: `km_simple` and the median
  line, checked identical to it), a design built from empty, and user code
  -- on the screens as they are (Reports > Content, the first step; Run,
  preview the selection).  New screenshots; `data-raw/make-article-
  screenshots.R` drives the new screens (#120).  The designer's figure-wide
  "Additional calls (plot.add)", the `call` piece's fields and the
  whole-script templates' names (from tflspec) are in Japanese too.

- **A reload no longer brings back the study as it was at start** (#118).
  `run_app("<study>")` opened the study once, when the app started, and
  each session (a new tab, a reload) began from that copy: what was saved
  since -- in the app or outside it -- was not on screen, and a save wrote
  the old state back.  Each session now opens the study from its folder.

- **ARD functions of one's own, on screen** (#115).  A new "Own functions"
  tab on the ARD tab lists the company's (the standards folder) and the
  study's (`programs/ard/functions/`, the study key `source`) side by side
  (`own_ard_functions()`): where each is, whether the study loads it, the
  analyses that use it, and its last try.  A study's copy wins; when it
  differs from the company's, which is newer is said, with "See the
  difference" and "Take the company's..." (`take_company_ard_function()`).
  "Use in this study" copies and loads one; "Try..." runs it in a separate
  R process on the study's data (or `cards::ADSL`), with the files the ARD
  programs load and the arguments an analysis gives
  (`try_ard_function()`): the problems in the app's language, the first
  rows of its ARD, the try kept in `programs/ard/functions/.checks.json`
  (said when the file changed since).  "New function..." writes one from
  tflspec's templates (a summary, a test or model, a free calculation),
  for the study or the company (`new_ard_function()`).  The files are
  read, not run, to be listed, and edited outside the app.  The analysis
  form lists the study's own functions under their own titles and names
  (where each is said), and the company's it does not load faint.  After
  S1's screen review: a try makes the data as the ARD programs do (the
  analysis set from its dataset, the others cut to its subjects) and says
  where it stopped (loading the files, making the data, the function);
  cards' example data is a choice of its own; what changed since the study
  copied a company's function -- the company's, the study's, both -- is
  told by the copy's record (`.copied.json`), not by file times, and taking
  the company's says what is lost and which analyses' ARDs become outdated,
  and refuses a study file that holds other functions too; the files are
  read again when the tab is opened, a function chosen, Try opened, or
  "Read the files again" pressed.

- **The sample study has a figure made with the designer** (#114):
  F-14-2-3, Kaplan-Meier curves of the time to the first dermatologic
  event -- the designer's KM template on the sample's ADTTE, with one layer
  added (the median line) -- so the figure guide's order (template,
  designer, user code) can be followed on SAMPLE-01.  The other reports are
  unchanged.  `register_study()` now reads a study folder's designed
  figures (`spec/figures/`); before, the next save removed them (the sample
  study, or a study folder registered on another computer).

- **The Japanese guide and the README catch up** (docs): the ARD tab's
  outline, parent and post rows, code lists and the order of levels,
  analyses run together, warnings and errors, ARDs made elsewhere, the
  company's own ARD functions, the report list from a TOC and reports
  written as user code; the tabs by their Japanese names and the sample's
  eight reports as they are now.

- **The errors and warnings inside the analyses, listed** (#109).  cards
  keeps an analysis's errors and warnings in the ARD and carries on; the
  Study ARD tab now lists them for the whole study -- the study ARD as last
  made and the ARDs reports take in (`study_ard_conditions()`, from
  `tflspec::tfl_ard_conditions()`: errors first, an errors-only switch),
  apart from a program that stopped as a whole (the table above).  An
  error's row says its statistics are not in the ARD (blank in the table);
  a report whose ARD is outdated is faint; the counts are in the tab's
  title, and a row opens that analysis.  Preview says the report's own.
  The messages stay cards' own, in English.

- **The company's ARD functions made from tflspec's templates are listed**
  (tflspec #126): `company_ard_functions()` reads the standards folder with
  `tflspec::tfl_ard_function_info()` -- a function written as
  `cards::as_cards_fn(function ...)` (the templates) was not found -- and
  gives each one's title, description and declared statistics; test files
  are not read.  The check of an own function (`it does not give ...`, `it
  does not say which statistics it gives`) is in Japanese too.  Needs
  tflspec 0.0.24.9040.

- **Analyses run together, on screen** (`cards::ard_stack()`, #107).  The
  ARD tab opens on an outline of the report's analyses (the sheet folded
  under it): the analyses inside a stack under it, moved up and down (their
  order is the ARD's, and a table's whose variables have no order), and per
  analysis the definition's errors and the ARD's warnings and errors
  (`tflspec::tfl_ard_conditions()`).  A stack's own form says on what it
  runs them (data, analysis set, condition, groups), what it adds (the
  subjects per group, the total N, missing rows, attributes: four ticks,
  written only when not cards' default), lists the
  analyses inside (open, add one), and ungroups or deletes it.  One inside
  has no data of its own (the stack's said, with "take it out" for another
  condition or groups), offers only what can run inside, keeps statistics
  only where tflspec allows, and refuses a variable another one inside
  computes.  "Run together with other analyses..." groups an analysis with
  the others that can (those that cannot listed with why); when the report
  counts the subjects per group already (BIGN), the stack does not
  (`.by_stats = FALSE`: two would break the column headers' N).
  Ungrouping, taking out the last one or deleting a stack keeps, as
  analyses of their own, the subjects per group and the total N it gave.
  The new-table wizard runs its analyses together (STACK, with the
  numbers and the counts inside it), unless a subject of the analysis set
  has no group (said, then one by one).  ard_strata() / ard_pairwise() come
  later.  After S1's screen review: the subjects per group counted twice
  (BIGN and a stack that counts them, or two BIGN) are found and marked in
  the outline, said when the stack's tick is set (with "Delete BIGN"), and
  ungrouping makes no BIGN / TOTAL the report has already; the stack's tick
  says when BIGN counts them; `.overall` is left to the other arguments (a
  table may make its Total column as well); the outline says whether the
  report's ARD is made; the grouping dialog says the stack can count what
  BIGN and TOTAL count, and the stack's ID; inside a stack, only the
  categories that can run there.

- **The ARS of a study names its own ARD functions** (tflspec #120):
  `export_ars()` gives tflspec the study folder, so an analysis whose method
  is a function the study keeps (`programs/ard/functions/`) is written with
  the call the ARD program makes, the file it is defined in, and the
  statistics it declares.  Needs tflspec 0.0.24.9037.

- **Follow-up `R CMD check --as-cran` on R 4.6.1** (#70).  `inst/WORDLIST`
  lists the technical words, so `spelling::spell_check_package()` finds
  nothing; two British spellings in older entries are American, as the
  package declares `Language: en-US`; the `run_app()` and `launch_app()`
  examples are `if (interactive())` rather than `\dontrun{}`.

- **Take in a TOC** (#99).  "Take in a TOC..." on the reports list: the
  file, its sheet and the rows above its header; which column is what,
  from the company standards' new `toc_map` sheet (an item and the column
  names it may have; the map is what `tflspec::tfl_read_toc()` takes), with
  "remember this mapping"; what would change, report by report and line by
  line; and taking it in, saved at once with the TOC's copy and record in
  `input/toc/`.  Taken in again, only what the TOC holds is updated: a line
  edited here that the TOC changed too is asked about (kept unless ticked),
  a report no longer in the TOC is kept, and an existing report's type is
  not changed; a new report's guessed type is marked and can be set.
  `toc_changes()`, `toc_apply()`, `toc_snapshot()`, `toc_imports()` and
  `toc_last()`.
  After S1's screen review: a report's lines are of two kinds, the TOC's
  (compared line by line) and those added here (not compared: they stay,
  after the TOC's lines), so a TOC that gets more lines no longer meets
  the lines added here; a line edited here is asked about only when the
  TOC changed it too; a report once taken in from a TOC is said to be
  missing every time; "Take it in" says it is working and cannot be
  pressed twice, nor while the TOC cannot be read; the last TOC taken in
  is said in the dialog, and the same file again noticed; a report ID on
  two rows is said with its rows; the reports with no change are folded,
  the heading rows skipped and the lines kept are said.
  The reports list takes its own height: a long one no longer covers the
  buttons under it.

- **User-code reports, after their screen review** (#98).  The ARD switch
  says what it reads (the ARD taken in; its analyses, built or not; none
  yet, with a button to the ARD tab) and what `ard` is; the ARD tab of a
  user-code report that does not read one says so.  The program checks
  `content` against the contract and stops naming the item and what it
  was (`content (item 2) is lm`).  "Run the code" names the line of the
  code an error is on and a dataset the code reads but the report does not
  ("Add ADSL to the data it reads"); a data frame's sample has its column
  header; the figure fits.  The Code tab of a user-code report has the one
  field it uses, said so.  The offer to convert figures written by hand
  says why and what Save then means, asks before converting, and "Later"
  holds while the study is open; a hand-written figure's own screen shows
  its two ways first (keep the code: a user-code report; or the designer).
  The kinds are named alike everywhere ("User code" as "Table" ...), with a
  line on each in the add dialog, and the content tab tells a custom
  analysis from a whole report of one's own code.

- **The sample study's figures are user-code reports** (#94): F-14-2-1
  and F-14-2-2 are of the type `user` (their code ends with the figure
  checks and `content <- plot`), so a new sample study has no figure
  written by hand to convert.  Its eight RTFs are the same as before.

- **The fourth kind of report: user code** (#91).  `report_types()` has
  `user`: the report's own code leaves `content` -- a data frame, rtftable
  pages, a ggplot (the program makes it an rtfplot) or a list of them --
  and the report is dressed from its settings as any other (page, header,
  footer, titles, footnotes).  Its program reads the datasets the report
  names and, when the report says so, its ARD as `ard` (its ARD
  definition's rows, or the ARD taken in for it), then runs the code; a
  program whose code leaves no `content` stops saying so.  The content tab
  of such a report has the contract in one line, the datasets, the ARD
  switch, the code (the report's data code, the same as the Code tab's) and
  "Run the code", which runs it in a fresh R process from the study folder
  (`preview_user()`) and shows the first table page and figure.  Its own
  rows of the table sheets are pointed out as not used.  A figure written
  by hand is offered -- once when the study is opened, and on its own screen
  -- to be made a user-code report, and is changed only when asked
  (`make_user_report()`: `content <- plot` added when the code leaves
  `plot`); its program is then to be made again.

- **ARDs made elsewhere are taken in on the ARD tab** (#89).  A new tab,
  "ARDs taken in", lists the record of `input/ard/` (who made each ARD,
  when, for which reports, its check, removed or in use) with the reports
  that use each one.  "Take in an ARD..." reads the file, offers its own
  `output_id` values as the reports it is for, takes it in and shows the
  check; with "Use it for these reports" the reports read it (not when the
  check finds an error).  On a chosen ARD: use it for the report in the
  sidebar, stop using it, compare it with the report's own ARD (double
  programming, `cards::compare_ard()`), replace it (the new file taken in,
  the reports moved to it, the old one kept on the record as removed), or
  remove it (the reports that use it may go back to their own ARD
  definition).  Above the ARD definition, a report that uses an ARD taken
  in says so -- its analyses there are not used -- with Compare when it has
  its own; the study ARD's list says where each report's ARD comes from.
  Making, rebuilding and previewing the ARD never write to `input/ard/`.
  Taking in, using, stopping, replacing and taking out are saved at once
  (the reports' `ard_source` and their programs), as the record is, so the
  two never part; the dialog starts empty each time, takes a file in once,
  and asks for the reports when the file names none; the record keeps the
  name of the file chosen; an ARD taken out that a report still uses, and
  one whose check found errors, are marked in the list (and the first above
  the ARD definition, in red).

- **A change to one report no longer makes every report "outdated"**
  (#96).  `study_status()` called a report outdated when either
  definition workbook was newer than its RTF, so saving any change -- one
  report's title, converting one figure -- marked them all.  The program
  holds the report's whole definition and is rewritten only when that
  changes, so a report is now outdated when its program, or a file it
  sources (the figure setup), is newer than its RTF.

- **The help of a report's `type` in Japanese** (#100): tflspec's column
  help now names `user`; its translation is added.

- **The study's code lists reach the ARD** (tflspec #105, Q5): the ARD
  programs get the study's code lists (the codelists sheet's study rows) and
  make each listed column a factor in their order before the analyses, so
  the ARD keeps the order and **counts a value no record has** (0) -- a table
  shows its row with 0 by default.  The code lists are part of an output's
  fingerprint: changing them makes its ARD "outdated".  To leave those rows
  out of one table, the variables sheet's new `empty_levels` column (`hide`)
  -- its help is in Japanese too.  Needs tflspec 0.0.24.9031.

- **The check of an ARD taken in, in Japanese** (from the review of #90):
  `.check_view()` gives the check's rows with tflspec's messages in the
  session's language (a message of cards' own structure check is introduced
  as such), for the screens to show.  cards' note that the ARD has no
  `method` rows is no longer reported: a report never reads them, and the
  study's own ARD has none either, so every ARD taken in had it.

- **The analysis form's argument hints are in Japanese too** (#85), all 81
  of tflspec's catalog (a test says every hint has its Japanese), and the
  functions the form lists are the ones the catalog offers (its `offered`
  column, tflspec 0.0.24.9029) instead of a list of names.

- **The builder writes the column header line by line** (#82).  The
  "Column header" presets radio is a form: one box a header line, with the
  row-header columns' text (one cell over several, or one each) and the
  value columns' -- the same text on each column, one cell per value of a
  key (a spanner over the outer key of a two-key table), one cell over them
  all, or nothing -- with its alignment, bold and underline.  Lines are
  added above, moved and removed (each line keeps its own fields, wherever
  it moves); "From a preset..." fills them from the company presets, after
  asking.  Tokens are offered as the table has them (`{col}`,
  `{col1}` ... for several keys, `{n}`, `{n:sum}`, `{N}` when the table's
  `header_n` names one), each with what it holds in the preview
  (`{n} = 86 / 84 / 84`, from rtfreporter's `plan_header_tokens()`; needs
  rtfreporter 0.8.2.9012), and go into the field last clicked; whose `{n}` it
  is (`header_n`) is asked when a cell uses one.  The form writes the
  report's own `col_header` rows, only when they change; a header it cannot
  show (column positions, `KEY = value`, styled row-header cells) is left
  to the sheet, with a note.
- The study tab no longer shows an error in place of the study (since the
  `input/ard/` folder came in): each folder's note is looked up by its name,
  so a new folder cannot break it, and `input/ard/` has its note.

- **ARDs made elsewhere** (tflspec #101): `import_ard()` takes an ARD (rds,
  the JSON / YAML of `tflspec::tfl_write_ard()`, XPT, CSV) into the study's
  `input/ard/` -- a folder nothing else writes to, so making the study ARD
  again never overwrites it -- read-only, checked
  (`tflspec::tfl_check_ard()`) and recorded in `input/ard/imports.csv`
  (`ard_imports()`: source, time, user, md5, rows, outputs, the check,
  in use / removed).  `use_imported_ard()` sets a report's `ard_source`;
  its program then reads that file, and `study_ard()` gives a report's ARD
  from wherever it comes.  `compare_imported_ard()` compares it with the
  report's own ARD (`cards::compare_ard()`), `remove_imported_ard()` takes
  one out of use but keeps it on the record.  The screen is a later PR.

- **The company's own ARD functions** (tflspec #102):
  `company_ard_functions()` lists the functions of the standards folder's
  `ard_functions/*.R`; `use_company_ard_function()` copies one into the
  study's `programs/ard/functions/` (a study file of that name wins) and
  adds it to the ARD definition's `source`; `check_company_ard_function()`
  tries it (`tflspec::tfl_check_ard_function()`).

- The ARD definition's two new columns, `parent` (an analysis run inside
  cards::ard_stack() / ard_strata() / ard_pairwise()) and `post` (steps on
  the ARD after the call), have their help in Japanese (tflspec #96, #97).
  They appear in the analyses grid as every column does.  Needs tflspec >=
  0.0.24.9028.

- **The analysis form names any ard_* function, and asks for its arguments**
  (#79).  "What to compute" is a list by category -- the company's keywords
  first, then every function of tflspec's catalog (summaries, hierarchical
  counts, confidence intervals, tests, effect sizes, models, survival ...)
  -- each a heading, its function and one line on what it does, with a
  search across all of them.  The chosen function's own arguments are
  fields (a choice, a number, TRUE / FALSE / the default, columns, a level
  from the code lists or the data, a formula, R code), its defaults shown
  faint and a hint under each; they are written to the analysis's `args`.
  What a field cannot hold stays as R under "Other arguments (R)", so
  `args` written by hand are never lost, and Apply without a change keeps
  them as written.  Old function names show only for the analysis that uses
  one; functions whose package is not installed, and those that run other
  analyses (ard_stack and the like), are listed but cannot be chosen yet;
  the survey-design functions and `ard_formals()` (out of the builder's
  scope) show only for an analysis that names one.  The chosen function
  stays named above the list ("Chosen: ..."); the search reads the English
  headings and the function names as words ("t test") too; a field's empty
  choice names the default ("(default: waldcc)"), and a field without a
  hint points to the function's help.
  The headings and descriptions are in Japanese too.  Needs tflspec
  0.0.24.9027.

- **The new-report wizard writes one analysis per call** (#76).
  `first_table()` (the "first table" form) wrote one analysis per variable;
  it now writes `BIGN`, `CONT` (the numeric variables together) and `CAT`
  (the categorical ones together) -- one cards call each, as an analysis is
  meant to be.  The table is the same: its rows keep the order the variables
  were chosen in (the `variables` sheet's `order`).  Studies already made
  are not touched.  The analysis form says so in one line: add variables to
  an analysis, and make another only when the statistics, the condition or
  the groups differ.

- The `header` sheet's help (and its Japanese) says that a report's line
  `(none)` takes the study's line of that number out (tflspec #91).  Needs
  tflspec >= 0.0.24.9027.

- **The data of an analysis is one choice** (#74).  On the ARD tab's
  analysis form, Data and Analysis set are one choice of a dataset and an
  analysis set -- "ADSL × SAF (pop_saf)", "ADAE × SAF (adae_saf)",
  "ADSL, no analysis set (adsl)" -- named as the program names the data.
  The definition keeps its two columns (`dataset`, blank for the analysis
  set's own data, and `population_id`); "(the analysis set's data)" is gone.

- **Copyright of the sample data, and citation** (#72).  The sample study's
  ADaM data are pharmaverseadam's (Apache License 2.0): its copyright
  holders are listed in `Authors@R` and `inst/COPYRIGHTS`, with what was
  changed.  `citation("tflplanner")` has a CITATION file, and the README
  says how to cite tflplanner (and cards / cardx) and acknowledges the
  packages it builds on.

- The `style` sheet's `align` help (and its Japanese) no longer warns that
  any `border_*` or look column left-aligns every column: from rtfreporter
  0.8.2.9008 a blank `align` keeps each column's default.

- **The table sheets have the columns tflspec 0.0.24.9021 added** (#66): the
  `layout` sheet's `pages_page_by` (BY pages with a row budget inside), the
  `style` sheet's default look (`header_align`, `header_bold`,
  `header_italic`, `align`, `bold`, `italic`, `underline`), table width
  (`table_width_twips`, `table_width_pct`, `table_width_pct_of_writable`)
  and `col_header_align`, and the `cell_styles` sheet's `underline` and
  `indent_twips`.  They appear in the grids as every column does (read
  from tflspec), with their help in Japanese, and a new study's standards
  offer `TRUE` / `FALSE` and `left` / `center` / `right` for them.  Blank
  = as before.

- **A figure template fits the study's data** (#64).  Applied with its
  defaults (KM: parameter OS, flag FASFL, group TRT01P), a template no
  longer draws an error when the data has not got them: for each the user
  left blank, the dataset's own is used -- its first parameter, a
  population flag it has (else ADSL's), a treatment variable it has (else
  ADSL's) -- and a note says which.  The ADSL join takes only what the
  dataset lacks (a flag in both came back as SAFFL.x / SAFFL.y and was
  found by neither name).  SAMPLE-01's KM template now draws as applied.

- **A figure is made one way: the plot designer** (#62).  Its first step
  is "Start from a template" (a type, its data and a few settings, applied
  as the designer's layers at once) or "Start empty"; both say they become
  the designer's layers.  "Apply a template..." on a figure that already
  has a design asks before replacing it.  The designer names its result,
  "This figure's Spec (YAML): spec/figures/<ID>.yml", and its YAML tab is
  "Spec (YAML)".  The plot written by hand is apart, below, as "User code
  (write the ggplot yourself)": open for a figure drawn that way, folded
  for a new one.  No change to the Spec.

- **`update_tflplanner()` behind a firewall or a proxy** (#60).  The
  update's own R process now gets this session's package repositories
  (RStudio's, an internal mirror) and download method -- a fresh Rscript
  had only cloud.r-project.org -- and first checks it can reach them and
  GitHub, naming every address it cannot ("UNREACHABLE: ...").  When the
  update does not finish, it says what failed and the two ways round it:
  `remotes::install_github()` of the three in this session, or
  `update_tflplanner(from = <folder>)`.  When the newest versions cannot
  be looked up, the list shows "?" with a note, and the update goes on.

- **The tabs in the order of the work, and where a report is made** (#58):
  Study, Data, Reports, ARD, Runs (Reports before ARD).  A report chosen
  in the list shows the buttons for its kind -- a table "Go to ARD" and
  "Make the table", a figure "Make the figure", a listing "Make the
  listing".  A top tab the chosen report has nothing on (ARD for a figure
  or a listing) is faded but still opens, and then says why.

- **The study's code list** (#56): Details (sheets) has a `codelists`
  sheet (tflspec 0.0.24.9017: `variable`, `value`, `label`, `order`), and
  its tab reads a code list from an `.xlsx` or `.csv` file into the
  study's defaults -- every table prints the values' text in their order
  (`read_codelist()`, `set_codelist()`).  A report's own rows replace the
  defaults for it; a variable's `levels` on the `variables` sheet, when
  given, is its order instead.

- **The ARD tab says what each field is in the spec** (#54).  A method is
  offered as what it does and the function it calls ("Count of one level
  (ard_dichotomous())"); each field says its spec name and, when the
  method has one, its default ("Percentages of (denominator =
  population)"); the formats have a heading; the data left blank names
  the analysis set's data ("(the analysis set's: ADSL)"); the analysis
  edited is shown as its own code under the form.  A tick shows every
  report's analyses in the grid (an analysis several reports use, such as
  BIGN, at once), edited as the whole sheet; a double click on a report
  in the study ARD's list opens its analyses.  The sidebar's report is
  "Report (output_id)"; in Japanese the sheet tabs read "ヘッダー
  (header)" and the Content tab "内容 (content)".  Data files may be `.rda`
  / `.RData` (one dataset a file; tflspec 0.0.24.9016).

- **Several column variables in the table builder** (#52): the Content
  tab's "Column variables" takes more than one (a group, then a visit
  ...), as `tables$cols` writes them (`A | B`, outermost first).  With
  several, their order is dragged; each one's columns are ordered by
  dragging as before.  The column header presets are named after the
  outermost one.

- **The page takes no clicks while the app works** (#50): opening a
  study, switching a tab or a report, saving.  After 0.4 s of work a light
  veil covers the page (the cursor says it is busy) until it is done; the
  short updates (the builder's preview, the run status) do not show it.

- **Typing in the table builder no longer closes it** (#48).  Every edit
  writes the definition, and the report chosen, the page shown and the
  builder's case were read from the definition: each edit redrew the
  form, closing the variable's panel and losing the text being typed (or,
  in an emptied field, a further BackSpace).  They now pass on only a
  change.  **A double click on the report list opens the report's
  Content**, as the study list opens a study.  **Save says "Saving..."**
  at once and cannot be pressed again until it is done ("Saved").  The
  column header presets offered are those that fit the table (the SOC / PT
  one only for a nested table), named after its column variable instead
  of "Arm".  A figure's Content shows making it from a design first, the
  plot written by hand after, under headings that say so.  The Japanese
  app says Listing throughout (no 一覧表), as the report types.

- **The report list says what each report reads, and its title** (#46).
  The Data column names the datasets a report reads -- a table its ARD
  analyses' data and their populations', a listing its dataset, a figure
  the datasets its design (or its row) reads: `adsl`, `adae / adsl` --
  with "(reworked by its own code)" when the report has data code; the
  ARD's state shows on hover.  The Description column is now the Title:
  the report's titles (the titles sheet's own lines; without them, its
  description), cut short with the whole title on hover.  The report
  types stay Table / Listing / Figure in the Japanese app too.

- **The setup, shortcut and update questions say what they do** (#44).
  Before it asks, `add_shortcut()` lists each shortcut by name and by
  place (Windows: desktop and Start menu; macOS: Applications; Linux: the
  application menu), with its folder, marks one that is already there as
  replaced, says where the launcher files are kept and that
  `remove_shortcut()` removes them; the question names how many ("Make
  these 3 shortcuts?").  Afterwards it names the files and the next step
  (Windows: pin to the taskbar from the Start menu).  `remove_shortcut()`
  separates the shortcuts from the launcher files and says that studies
  and settings are not touched; `update_tflplanner()` says where it
  installs, in what order, the channels, and to restart R; the steps of
  `setup_tflplanner()` say which files and folders they write, where the
  packages go, and how to do a declined step later.  In English and
  Japanese.

- **tflspec's new listing and report columns** (ichirio/tflspec#64): the
  listing sheets' `blank_row`, `wrap`, `sep` and `align`, and the report
  sheet's `watermark`, `figure_width_in` and `figure_height_in` show in
  Details (sheets), with their help in the app's language.  Needs tflspec
  0.0.24.9013.

- **The cell_styles sheet in Details (sheets)** (ichirio/tflspec#64).
  tflspec's table spec has a `cell_styles` sheet (one
  `plan_cell_style()` a row); it is now a table sheet of the study's
  workbook, shown and edited under Details (sheets), a report's own rows
  replacing the defaults whole.

- `preview_html()` is exported as documented (`.page_lines()`, internal,
  was exported in its place: a roxygen block attached to the wrong
  function); `devtools::document()` leaves the Rd as it is.  Needs
  tflspec 0.0.24.9012.

- **Follows tflspec's column names and help** (ichirio/tflspec#64).  The
  table spec's `columns$width` is `rel_width`, and the report's
  `table_font_size` / `title_font_size` / `footnote_font_size` are
  `*_font_size_half_points` -- in the company standards' defaults, their
  draft workbook and the sample study (SAMPLE-01; its eight reports'
  RTFs unchanged).  tflspec's column help is English now; the app shows
  it in Japanese from its own translations.  An ARD built before a
  study's own analysis functions (its key `source`) changed shows as
  outdated.  Needs tflspec 0.0.24.9011.

- **The ARD form's strata and denominator** (ichirio/tflspec#64).  An
  analysis may be repeated within variables ("Repeated within (strata)":
  a subgroup, a parameter by visit) and say what its percentages are of
  ("Percentages of": the analysis set, within a row / column / the whole
  table, another population or a dataset) -- the ARD spec's new
  `strata` and `denominator` columns, also on the analyses grid.  The
  study sheet offers the key `source` (R files of the study's own
  analysis functions).  Needs tflspec 0.0.24.9010.

- **Set up, update and start tflplanner without the console** (#40).  The
  console is needed only for what the app cannot do for itself:
  `setup_tflplanner()` with no arguments asks, step by step, for the home,
  the packages the programs use, and a shortcut.  `add_shortcut()` /
  `remove_shortcut()` make "tflplanner" and "tflplanner (update and
  launch)": on Windows a desktop and Start menu `.lnk` (no console window;
  R is found in the registry at every start, so updating R does not break
  it), on macOS `~/Applications/*.app`, on Linux a `.desktop` menu entry.
  The shortcut opens the app if it already runs on its port
  (`setup_tflplanner(port = )`, default 7470), and closing the browser
  stops it (`run_app(stop_on_close = )`).  `update_tflplanner()` updates
  rtfreporter, tflspec and tflplanner in that order in a separate R
  process -- release (CRAN, else the GitHub release) or `"dev"`, or `from =`
  package files -- and the shortcut's "update and launch" does the same.
  The app never updates itself: when it starts it looks for a newer
  version and says so (off in its settings, or `check_updates = FALSE`).
  `tflplanner_packages()` lists the packages and their versions;
  `launch_app()` (also the RStudio add-in *Launch tflplanner*) starts the
  app in its own R process.

- **Getting started: one way into New study** (#38).  With no study, the
  getting-started card's buttons open the New study dialog: "Try the
  sample study (about 1 minute)..." with the sample chosen (its ID given
  there, no longer fixed to SAMPLE-01), "New study..." (was "Create an
  empty study", which opened the same dialog) with nothing copied.  New
  study... and Register a folder show under the list even with no study;
  Open / Unregister / Refresh wait for one.  The dialog offers "Copy
  another study" only when there is one.  The Japanese "Next steps" card
  names the tabs as the app does (中身, 実行).

- **The study's analyses as CDISC ARS.**  `export_ars()` writes the ARD
  definition, with the table and report definitions, as a CDISC Analysis
  Results Standard reporting event (`tflspec::tfl_ars()`): the ARS JSON,
  CDISC's Excel template of it, and `ars_check.csv` (what the check finds
  and what ARS does not say).  The Study tab has a button for it ("Export
  the analyses as CDISC ARS").  The ARD definition's analyses gain the
  `purpose` / `reason` columns (from tflspec).  Needs tflspec 0.0.24.9009.

- The two definition workbooks are written by tflspec
  (`tfl_write_table_spec()` / `tfl_write_report_spec()`): each holds only
  its own half's sheets, and what a column means is a comment on its
  header cell instead of a `_README` sheet.  The grid's column help reads
  `tflspec::tfl_spec_columns()`.  Needs tflspec 0.0.24.9004.

- Development reopens at 0.0.2.9000, after the 0.0.2 release.

# tflplanner 0.0.2

- **Tables on rtfreporter's plan, adopted.**  The plan engine (ARD
  functions, `table_plan()` and the `plan_*()` verbs) was adopted in
  rtfreporter's pre-CRAN API review and released in rtfreporter 0.8.2;
  tflplanner 0.0.2 needs rtfreporter 0.8.2 and tflspec 0.0.24.  The
  entries below (0.0.1.9000, 0.0.1.9001) are the changes since 0.0.1.

## tflplanner 0.0.1.9001

- **Tables follow rtfreporter's redesigned plan verbs** (rtfreporter
  0.8.1.9003, ichirio/rtfreporter#498) through tflspec 0.0.23.9001.  The
  table definition's `layout` columns are named after the verbs'
  arguments now: `stub_into` is `stub_name`, `group_show` is
  `group_keep`, `colpages_carry` is `colpages_keep`, and `pages_by` is
  gone (one page per value is `group_page = TRUE` with `group_col`).
  - **A study saved with the former names is not read**: opening it
    stops with the columns to rename (in the study's workbook) instead
    of dropping their values.  Company standards with the former
    `default_layout` columns are refused the same way.  The sample study
    shipped with the package (SAMPLE-01) is updated.
  - Code written by hand in a study (data code, normalize code, setup)
    is not rewritten: a call to a former plan verb (`plan_fmt()`,
    `plan_header_style()`, `table_plan(notes = )` ...) needs the change
    by hand -- see rtfreporter's NEWS.
  - Needs rtfreporter 0.8.1.9003 and tflspec 0.0.23.9001.  The sample
    study SAMPLE-01's reports are byte-identical to 0.0.1.9000's.

## tflplanner 0.0.1.9000

- **The table engine is rtfreporter's** (plan E, ichirio/tflspec
  Discussion #23): tflspec 0.0.23.9000 keeps the specifications only, and
  the ARD functions and the plan moved to rtfreporter (0.8.1.9001, as
  experimental) under new names, with no aliases: `tfl_ard_normalize()`
  is now `normalize_ard()`, `tfl_plan()` `table_plan()`, `tfl_plan_*()`
  `plan_*()`, `tfl_apply_plan()` `plan_apply()`.  Generated programs and
  the built-in table template write the new names.
  - **Saved code is rewritten as it is read**: `tfl_ard_normalize(`
    becomes `normalize_ard(` (and `tflspec::tfl_ard_normalize(`
    `rtfreporter::normalize_ard(`) in a study's data code, normalize code
    and setup code -- from the app's saved state and from a study
    workbook's `_tflplanner` sheet -- and in the company standards' code
    templates.  Save the study (and install the standards again) to keep
    the new names; programs the app wrote are written again on save,
    hand-edited ones need the rename by hand.
  - Needs rtfreporter 0.8.1.9001 and tflspec 0.0.23.9000.
  - tflplanner 0.0.1 (tag `v0.0.1`, with tflspec `v0.0.23` and
    rtfreporter `v0.8.1`) is the last version on the former engine.

# tflplanner 0.0.1

- **First release.**  The last version before plan E (the table engine
  moving from tflspec to rtfreporter), where to go back to if plan E is
  undone.  It runs on tflspec 0.0.23 (`Remotes:` pins that tag) and
  rtfreporter 0.8.1 or later.

- Fix: `run_app("SAMPLE-01")` (a study given at start) stopped the session
  on its first page ("Can't access reactive value 'ver' outside of
  reactive consumer"); opening the study from the Studies tab worked.

- **The Figures tab keeps up with editing**: the preview is drawn at
  screen resolution (the saved size; about 1 s instead of several), only
  while the tab shows, 0.6 s after the last change; *Redraw on change* can
  be turned off, and a notice then says the drawing is older than the
  design.  The code of the chosen piece still follows every change at
  once.  `preview_figure()` gains `max_px`.  (Code generation itself got
  about 30 times faster in tflspec's catalog cache, ichirio/tflspec#41.)

- **One tab a kind of report** (#26): the tabs are now *Tables* (the
  table definition and the table builder, as two sub-tabs), *Listings*
  and *Figures* (the Plot Designer), in place of *Table definition*,
  *Table builder (beta)*, *Listing / Figure* and *Plot Designer*.  A
  hand-written figure's datasets and data part moved from *Listing /
  Figure* to the top of *Figures*.  Choosing a report in the sidebar while
  one of these tabs is open opens its own kind's tab.

- The table builder's preview plans the table with
  `tflspec::tfl_table_plan()`: tflspec 0.0.20's `tfl_plan()` no longer
  reads a table definition (`spec =`).  Needs tflspec 0.0.20 (#23).

- **Plot Designer: every figure type, and copies per parameter** (#21).
  The template list (grouped by kind) now covers every type: the 27
  templates in parts (KM, waterfall, swimmer, spider, bar, mean over time,
  spaghetti, box, scatter, PK) with the fields each kind needs on the start
  screen (value, x / y, nominal time, category, responders, duration, the
  visit), and the whole-script templates (forest, AE dot, butterfly, eDISH,
  sankey, sunburst), whose design is one *Whole figure* piece edited on a
  form of the type's own arguments.  *Copy to other parameters* makes one
  new figure report per PARAMCD from a designed figure, its design the
  same but for the parameter (`copy_fig_to_params()`).  Needs tflspec
  0.0.19.

- **Plot Designer: advice and presets** (#19).  The preview now carries
  tflspec's advice on the design (what is usually wanted and is missing or
  unusual: a KM figure without the number at risk, a legend inside the
  panel with many groups, more groups than the palette has colors, text
  visits with no order, a waterfall without its marks ...) together with
  the checks, as an overlay on the figure; where one change would do it,
  *Apply* makes it.  A designed figure can be kept as a company *preset*
  (*Save as preset*: a `.yml` under the home's `standards/figure-presets`),
  and a new design can start from a preset instead of a template.
  `fig_presets()`, `save_fig_preset()`, `read_fig_preset()`,
  `remove_fig_preset()`; `preview_figure()` gains `advice`.  Needs tflspec
  0.0.18.

- **Plot Designer** (#15): a figure can be designed instead of written by
  hand, as tflspec's figure design: data steps from ADaM (read, join, keep a
  PARAMCD or an analysis set, derive, change a time's unit, order values,
  rank, or R code), statistics (a KM fit, summary statistics, or R code),
  the figure-wide settings and the layers in order (KM curves and marks,
  the number at risk, any layer of tflspec's geom catalog, any function by
  name, or R code).  The new *Plot Designer* tab starts a design from a
  template (KM with the number at risk, mean over time, waterfall ...)
  filled in for the study's data; the pieces are then a stack on the left
  (add, move, remove; a piece with a problem is marked), the chosen piece
  is a form on the right (defaults shown; the data's datasets, variables
  and PARAMCDs to pick from), and the middle shows the figure as its
  program saves it (the PNG, at its size), redrawn as it changes, with the
  checks.  The code and the design (YAML) are below.  Designs are saved
  with the study and written as `spec/figures/<output_id>.yml`; the
  figure's program takes its plot from the design.  `fig_design()`,
  `set_fig_design()` and `preview_figure()` do the same from R.  Needs
  tflspec 0.0.15.

- **Report programs carry the expanded code** (#17): instead of reading
  the workbooks at run time (`tfl_read_report_spec()`, `tfl_plan(data, spec
  = spec)`, `tfl_report(spec, ...)`), a report program now says what it
  makes -- the table's `tfl_plan() |> tfl_plan_*()` pipeline
  (`tflspec::tfl_table_code()`), the document's `rtf_document()`,
  `rtf_section()`, `rtf_tables()` / `rtf_figures()`, `rtf_titles()`,
  `rtf_footnotes()` (`tfl_report_code()`) and the output path.  Generate
  the programs again after changing the workbooks.  A definition that does
  not hold yet gives a program whose `stop()` says why.  The reports are
  unchanged (the sample study's RTF are identical).  Requires tflspec
  0.0.14.

- The listing sheets of `listing_figure_spec.xlsx` (`listings`,
  `listing_cols`) are tflspec's listing definition: their columns, reading
  and normalizing come from `tflspec::tfl_listing_spec()` /
  `tfl_read_listing_spec()`, and the listing program from
  `tfl_listing_code()` with that definition (tflspec 0.0.11).  The workbook
  and the programs are unchanged; only the `figures` sheet stays
  tflplanner's own (#13).

- Follows tflspec's `tfl_` prefix (tflspec 0.0.9): the programs it writes
  say `tfl_ard_normalize()`, `tfl_read_report_spec()`, `tfl_plan()`,
  `tfl_report()` and `tfl_report_path()`, and it calls tflspec by the new
  names.  tflspec no longer has the former names, so rework code saved in
  a study that calls them (`data <- ard_normalize(ard)` ...) stops with
  "could not find function": rename the call (`tfl_ard_normalize()`).  The
  reports are unchanged; the programs' checksums change with the names
  (#11).

- *New study* now offers the sample study itself (copied under the new
  study's ID, its ARD and reports made at once) in place of tflspec's
  table-definition examples, which had no ARD definition or data.
  `create_sample_study()` gains `study_id` / `title` / `compound` /
  `phase` / `description`, and writes `spec/ard_spec.xlsx` (an export of
  the ARD definition, to read) so every definition is there as Excel.
- Figures in the company's style, and checked: the company standards gain
  `figure_settings`, `figure_colors` and `figure_markers` (built-in =
  tflspec's figure style, taken from the KM / waterfall / swimmer sample
  programs).  Saving writes `programs/tfl/fig_setup.R` (`theme_tfl()`,
  `scale_colour_tfl()`, `tfl_marker()`, `tfl_save()`, `tfl_km_risk()`,
  `tfl_check()`); every figure program sources it and ends with
  `tfl_check(plot)`, whose warnings the official run counts.
- SAMPLE-01 gains ADTTE (time to the first dermatologic event, derived from
  ADSL / ADAE), a Kaplan-Meier table (T-14-2-2) and a Kaplan-Meier figure
  (F-14-2-2) whose number at risk is the table's ARD.
- The sample study SAMPLE-01 (`create_sample_study()`,
  `setup_tflplanner(sample = TRUE)`, *Add the sample study* in the app):
  the CDISC pilot ADaM data of pharmaverseadam, one study ARD, four
  tables, a listing and a figure, made at once by an official run.
- The ARD definition is kept by the app (web GUI); `ard_spec.xlsx` is an
  optional export / import (`write_ard_spec()`, `export_spec()`,
  `import_spec()`).  Saving writes one ARD program per output
  (`programs/ard/`), `ard_setup.R` and the folder's copy of the definition.
- Official runs (`run_batch()`, `programs/*/autoexec_*.R`) make a dated
  batch folder with each program's logrx log, what the run made and the
  code it ran; a program run on its own is a preview and keeps no log.
- Company standards (`standards_template()`, `setup_tflplanner(standards =)`):
  dropdowns, presets, ARD methods and statistics (`ard_statistics()`), code
  templates (`code_templates()`), the rows a new study starts with.
- ARD statistics beyond cards' own (CV, SE, geometric mean / CV and CIs,
  percentiles, ...) and per-statistic formats giving `stat_fmt`.
- Listings in rows (rtfreporter's `multiline`) and figures that read their
  data, with the plot written by hand.

- Input assistance from the ARD: `fetch_ard()` runs a report's data code
  from the study folder and keeps `ard_meta()` (keys, hierarchy,
  variables, levels, statistics, with labels and value order from the
  source data); `fill_variables()`, `fill_tables()`, `cell_presets()`,
  `header_presets()`, `add_preset()`; dropdowns in the grids.
- A report's data code is two steps: the ARD code (`data_code`, makes
  `ard`) and normalize and rework (`process_code`, makes `data`;
  `data_lines()`).
- The sidebar shows a report, the Study defaults or ALL; a report's grid
  shows the default rows it inherits (`inherited_rows()`).
- The app is in English (default) or Japanese (`tr()`,
  `setup_tflplanner(language = )`).

- tflplanner keeps each study's state in a home of its own
  (`setup_tflplanner()`, `tflplanner_home()`, `tflplanner_config()`): a
  study opens as it was last saved, with a history of earlier saves.  The
  definition workbooks are written from it; `export_spec()` /
  `import_spec()` move them in and out.  `create_study()`,
  `open_study()`, `register_study()`, `unregister_study()` and
  `list_studies()` work on the registered studies; `run_app()` sets the
  home up on first use.
- The code editors no longer write one report's text into another when
  the study or report is switched while the browser is still reporting.

- Studies: each study is a folder (`study.yml`, `data/`, `spec/`, `programs/`,
  `output/ard/`, `output/tfl/`, `logs/`) created, opened, saved, run and
  tracked from the app (`create_study()`, `open_study()`, `save_study()`,
  `run_study()`, `study_status()`).
- Reports have a type (Table / Listing / Figure); programs run from the
  study folder, Tables save their ARD, generated programs carry a checksum
  and follow the definition until edited by hand.
- First version: a 'shiny' editor for rtfreporter's `table_spec.xlsx` and
  `report_spec.xlsx`, writing one R program per report and
  `autoexec_report.R`.
