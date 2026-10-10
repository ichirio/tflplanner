# tflplanner (development version)

- **The sample's race tables are demographics tables, with ASIANSUB**
  (the user's request on #326).  T-14-1-5 and T-14-1-6 show age (n,
  mean (SD), median, min and max), age group, sex and ethnicity as well,
  so RACE is the one variable with rows nested under one of its levels.
  The derived sub-category is `ASIANSUB`, one column per race as an EDC
  collects it (White's would be `WHTSUB`, nested under "White": each its
  own analysis, `plan_nest()` takes several), still footnoted as derived
  for demonstration.  The other reports' programs and RTFs are
  byte-identical.

- **An ARD definition that does not hold is said, not passed over**
  (#323).  Saving writes no ARD program then (a blank `from` of an
  analysis data made by code, for one); it used to say nothing, and the
  official run failed on every ARD program with "cannot open the
  connection".  Now `save_study()` says so in a message and keeps it as
  `$ard_problem` (the app shows it after Save, the rest still saved), the
  Review tab's error row says that no ARD program is written, the runner
  says "no program ...: save the study" for a program that is not there
  instead of running it, and `run_batch()` on a study with no ARD
  programs names the likely cause.

- **A Total column, without a "Total" arm in the data** (tflspec #212,
  rtfreporter `plan_total()`).  `set_total_column(x, output_id, label,
  position)` switches it on in both halves of a table's definition: the
  `tables` sheet's `total` / `total_position` (written as `plan_total()`),
  and `overall = TRUE` on the report's own analyses grouped by the column
  variable (not the rows inside a stack, not `custom` / `subjects`), which
  makes each ARD program run them again without their `by` -- cards' own
  overall rows, with no group.  The ARS then has an analysis over all
  subjects without the grouping, not a `"Total"` group the ADaM does not
  have.  Needs tflspec >= 0.0.24.9083 and rtfreporter >= 0.8.2.9034.
- **Logistic and Poisson regressions on R before 4.4 need MASS** (#319).
  Before R 4.4, `confint()` of a `glm` comes from MASS, and without it
  `cardx::ard_regression()` stops with "Unable to tidy x".  MASS is
  installed with R, so this concerns only an R where it was removed: the
  ARD dictionary's help for these two models says so, and its test skips
  them only where MASS cannot be loaded.
- **Step 3 (page and output): the input on the left again, the page at
  its actual size** (the user's request).  The report's page sheets
  (report, page, header, footer, titles, footnotes, your tokens) are the
  form on the left, as before the SPEC | Code | Result tabs came (that
  change had put them inside the SPEC tab); on the right the tabs open on
  Result: the first page at 100%, scrolling in its pane, with a "Fit to
  width" button (remembered in the browser) and "Full size" as before.
  SPEC shows the report's own rows as they are written to
  `spec/report_spec.xlsx`, read only.  The report's font stays above.
## Upgrading from an earlier version

Update tflplanner and tflspec together, open each study and save it, then
look at its Review tab.

* **This tflplanner needs tflspec 0.0.24.9079 or later** (0.0.2.9157 and
  later needed 0.0.24.9077): update both together.
* **Six functions are no longer tflspec's; the study keeps them in
  `programs/study_helpers.R`.**  `set_levels()`, `tag_ard()`, `fmt_ard()`,
  `keep_stats()`, `fmt_pvalue()` and `save_ard()` (tflspec's up to
  0.0.24.9066) are written there by a save, and the study's setup sources
  it.  A program that calls them as `tflspec::` or after
  `library(tflspec)` without it stops; the review says which (P02): write
  it again (open the study and save) or source
  `programs/study_helpers.R`.
* **A figure design written before reads as it is**: its YAML's old form
  (`stats:`, `data_code`, `stats_code`) is read, and a save writes the new
  one.
* **Figure programs have a new shape, and draw the same figures**: a data
  and a plot section, one `+` chain.  A save writes them again.
* **The steps:** update both packages; open each study and save it (the
  programs and `study_helpers.R` are written again; a program edited by
  hand is copied to `programs/.edited/` first); check the Review tab.

## Changes

- **Race with its Asian sub-categories, nested or in two blocks** (the
  sample's T-14-1-5 and T-14-1-6, with rtfreporter #599 and tflspec
  #213).  The table builder's variable form gets "Rows under a level of
  another variable": a categorical variable's rows go right after the
  chosen level's row of another, one step deeper, without their own
  heading (the `variables` sheet's `under`, written as `plan_nest()`).
  The sample gains the two tables from the same data and analyses:
  - **T-14-1-5:** Race, n (%) with Chinese / Japanese / Korean under Asian.
  - **T-14-1-6:** Race, n (%), then Race Sub Asian.

  They count the enrolled subjects (a new analysis set ENR, all of ADSL:
  the pilot's two Asian subjects are screen failures) by ARM, with a Total
  column. The Asian sub-categories are derived for demonstration (a
  footnote says so; RACE is not changed). The other tables' programs and
  RTFs are byte-identical.
- **The Review tab says in plain words what reading the data does.**  The
  internal "data facts" are gone from the screen and the guide: the button
  is "Review with the data" (データも使って見直す) everywhere, and the tab
  says "The checks against the data have not run yet. Press [Review with
  the data] to read the datasets once (not again until a file changes)."

- **The preview shows a column header line's bold and underline.**  Ticked
  in the table builder (a line's "bold", "underline"), they were in the
  definition, the program and the RTF, but the Result tab's preview drew
  every header cell plain.  It now draws a header cell bold, with its rule
  above or below and its alignment, as the RTF prints it.
- **A figure printing a table's numbers is in the ARS export** (#293
  phase 5, with tflspec #211).  `export_ars()` passes the figures whose
  ARD is a table's, with the analyses their designs print, to
  `tflspec::tfl_ars(references =)`: the sample's F-14-2-3 is an ARS
  `Output` (its titles and RTF) whose list of contents names T-14-2-2's
  KM and HR analyses, no longer "no analyses in the ARD definition".
- **A figure printing a table's numbers follows that table's ARD**
  (#293 phase 4).  Its program records the definition of the ARD it read
  (`ard_built <- ard_fingerprint("T-14-2-2")`, then
  `record_report(report_id, ard = ard_built)`: a column `ard` in
  `output/tfl/report_status.csv`).  `study_status()` marks it `outdated`
  when the table's ARD is not made from the table's definition now, or
  when it was made from another definition than the figure; its new
  column `why` says why a report is to be made again (`program`, `setup`,
  `ard:<id>`), shown in the runs table.  The official run of the reports
  (`programs/batch.R`: `.batch_needs`, `.batch_needs_hash`) and the
  preview (`run_study()`) make the table's ARD first when it is not made
  from its definition -- logged in the same batch under `ard` -- and
  leave it alone when it is.  After this update every report program is
  rewritten once (`record_report()` changed in `report_setup.R`).
- **The column header form, easier to read and to use** (step 2, the
  table builder).  Each line's tools (its number, its shape, alignment,
  bold, underline, the arrows) are on top of its fields, on a grey band
  with a rule above, so where one line ends and the next starts is plain;
  the insert chips and "what the header's {n} counts" are in boxes of their
  own.  "From a preset..." is a button the size of the others, "Load a
  standard column header...", next to "Add a line above"; it opens a list
  of the standard headers, each with its lines in short.
- **What the header's {n} counts, chosen among the ARD's numbers.**  The
  choices are the populations the ARD states for the table's columns
  (rtfreporter's `plan_n_candidates()`), each with its values: one ("Subjects,
  by TRT01A: 86 / 84 / 84", chosen); on pages split by a lab parameter two,
  "Subjects on each page (per PARAMCD, by TRT01A): 22 / 24 / 23" and
  "Analysis set (by TRT01A, the same on every page): 25 / 26 / 24", and
  both.  When the two differ and none is chosen the form says so (making
  the report warns until then); with no ARD yet the three in words.  The
  choice is the `tables` sheet's `header_n`, as before.  Needs rtfreporter
  0.8.2.9033.

- **A new report goes where its id sorts; the list can be sorted by ID.**
  The report list's order is still the order the reports are made in.  A
  report added (Add, Copy, `add_output()`, `copy_output()`) goes where its
  id sorts among the others -- by the numbers in the id, as numbers
  (T-14-1-2 after T-14-1-1 and before T-14-1-10; T-14-0-1 before F-14-2-1,
  the sections' order), then its letters -- instead of last; one ordered
  by hand keeps its order.  A TOC's reports come in the TOC's order
  (`add_output(at = "end")`).  The report list's new "Sort by ID" puts the
  whole list in that order, after asking how many move; the up and down
  arrows still move one.
  `sort_outputs()` does the same in R.

- **The review names a program calling a function tflspec no longer
  has** (P02, area program): one of the six above, as `tflspec::`, or
  after `library(tflspec)` with `programs/study_helpers.R` not sourced on
  the way (an error; to check when another program sources it).  The
  study's `programs/` is looked at, not its copies in `.edited/`.
- **The review lists a figure's ARD problems** (#288).  A figure that
  prints a table's numbers is reviewed as the catalog's F04-F08: its ARD
  source is not a table of the study (F04), the design reads an ARD but
  the figure has none (F05), a piece names an analysis the table no
  longer has (F06), the ARD is not made yet (F07), a piece the ARD cannot
  answer (F08).  A click on one opens the figure's step 2, or the piece in
  the designer.
- **The review's sentences in Japanese, values and all.**  A row carries
  its sentence (`template`) and values (`args`); the app translates the
  sentence and puts the values in.  With tflspec 0.0.24.9078 that covers
  a table against its ARD (T06, T07), a listing's columns (L01, L02) and a
  figure against the data (F03); the app's own figure rules (F04-F07) too.

- **The Review tab** (#288, phase 2).  A tab between Make a report and
  Runs lists the study's review (`study_review()`): what cannot be used,
  what is probably wrong, what to set by hand, report by report (the
  picker on the left, each report with its three counts), filtered by
  level, area and a search.  A click on an item goes to it: the report
  chosen, its step and tab, the analysis, analysis data, code list or
  figure piece opened, and the grid's row and cell flashed.
  - The review is the app's one: made when a study is opened, 2 s after
    edits pause and after a save (no file read: the facts of the data
    kept from the last read); **Review with the data** reads the files
    whose facts are stale; **Check as the programs read it** is the
    former Runs tab's definition check (its card is gone).
  - Its counts where one works: the report head (each a link to the tab,
    that report and level), the report picker (errors / to check / by
    hand after each title, the run's mark as it was), and the analyses'
    badges (an error, or something to check: the subjects per group
    counted twice among them) read the review instead of the ARD
    definition's message.
  - A figure's advice with a one-step fix has an Apply button.
  - The company standards' settings key `review_off` (`A12 | C02`) leaves
    rules out of every study's review; the tab says which.
  - `study_review(progress = )` / `review_facts(progress = )`: told the
    dataset each time one is read.
  - The guide (Japanese) has a section on the review.

- **The sample's F-14-2-3 prints the hazard ratios too** (#311).
  T-14-2-2's ARD has a Cox model's hazard ratios against placebo (a
  `custom` analysis `HR`, `cardx::ard_regression(coxph(), exponentiate =
  TRUE)`), in the ARD and not in the table (its cells name `prob` and
  `time` only; its RTF is unchanged, byte for byte), and F-14-2-3 prints
  each with its CI on one line, through tflspec's labels that name
  several statistics of one address (`HR {estimate} (95% CI {conf.low},
  {conf.high})`, tflspec #206).  `cardx::ard_regression()` needs
  broom.helpers: it is in Suggests, and the sample's official run names
  it among what to install when it is missing.

- **A figure prints the numbers of a table's ARD** (#293 phase 3, with
  tflspec #203).  A figure's step 2 (ARD) chooses where its ARD comes
  from: none (it reads its data only), its own analyses (defined as a
  table's), or a table's ARD (`ard_source = table:<id>` on the report
  row; `set_fig_ard_source()`), shown with the table's analyses, the
  statistics its ARD has and which the figure uses.  The program reads
  it as `ard` at the top of `# ---- data ----`; the Plot Designer offers
  "A number from the ARD" (a median, a hazard ratio ... printed on the
  plot) and "Statistics from the ARD" (a data frame of them) when the
  figure has one, with the analyses, variables and statistics of that ARD
  to choose from, and the preview draws them.  The study's check names a
  source that is not a table, a design that reads an ARD without one, an
  ARD not made yet (a warning), and a piece whose analysis, statistic,
  level or group the ARD does not have.  Renaming a table renames its
  figures' `table:` references.  The sample's F-14-2-3 prints the three
  medians of T-14-2-2's ARD; the number at risk stays the fit's.
- **A review of the whole study** (#288, phase 1: the R functions; the
  Review tab comes next).  `study_review(study)` lists, report by report,
  what cannot be used (`error`), what is valid but probably wrong
  (`check`) and what is missing and has to be set by hand (`hand`), each
  with where it is (sheet, the row's key, column), a hint and its rule:
  tflspec's rules (`tflspec::tfl_review_rules()`) on the study's
  definition, and the report list's and the study folder's own -- a
  report with no title (no title line, no `OUTPUT_TITLE`), no analysis
  set (a table whose analyses name none; its own code chooses itself), an
  analysis set the populations do not have, nothing that makes it (no
  analyses or data code, no listing columns, no figure design), an id
  twice or one a file name cannot hold, two reports writing one file or
  program, a dataset the list names that the catalog lacks (R01-R08);
  analyses of no report, an analysis reading another analysis set than
  its report's, the subjects per group counted twice (A11-A13); a
  dataset with no file, for the study and under each report reading it
  (D01); and, with `deep = TRUE`, the definition read back as the
  programs read it (`check_planner()`, P01).
  - `data = "read"` checks against the data too: the facts of the data
    (`review_facts()`: each column's class and values, each analysis
    set's subjects, each condition's rows -- no records) are kept in the
    store, one file a dataset, and made again only for a dataset whose
    file (path, time, size) or definition (its analysis data, analyses,
    listings) changed.  `data = "cached"` uses them and reads no file (a
    dataset not read yet says so, D02); `"none"`, the definition alone.
  - `ard = TRUE` checks each table against what the study ARD holds and
    lists what went wrong while it was made (kept while `ard.rds` does
    not change).
  - The messages are in the app's language, made from each rule's
    template (one line a rule in `strings.csv`); `message_en` keeps the
    English.  `review_problems(x)` reviews a definition with no study
    (an import's draft).
  - On the sample study: no error and nothing to set by hand; 11 checks
    (9 code list values no record has, the KM figure's two advices).

- **An edit makes the app work out only what depends on it; a save is
  quicker.**  Measured on the sample study (the profile of each action):
  - An edit of a table's cell no longer hashes every report's ARD
    definition again (the ARD's state), rebuilds the report list, or reads
    the own ARD functions' files: each now follows its part of the
    definition (the ARD definition and code lists; what the report list
    shows), not every change of it.  The report list tab used the ARD's
    state worked out a second time: it shares the one.
  - A save gave the study anew, and every output that asks whether a
    study is open -- most -- was drawn again (the table builder's form
    among them, about a second): now only what the save changed is (the
    study list, the marks).
  - A save writes only the workbook whose half changed: a table's cell
    edited, `table_spec.xlsx` alone (and the reports whose definition did
    not change no longer look outdated for the other).  A program's
    checksum is worked out once a program, in one file of the session's
    (each was a new file, made and deleted, two or three times a program).
  - `write_planner(books = )`: `"table"`, `"report"` or both (the default).
- **A designed figure's program reads as one written by hand** (#293,
  with tflspec #201).  Its code has two sections, `# ---- data ----`
  (the code lists, the datasets read, the steps' pipes) and
  `# ---- plot ----` (the palette, then one `+` chain into `plot`), before
  the report's.  The study's `fig_setup.R` attaches the packages the
  figures use (ggplot2, patchwork, dplyr, and ggsurvfit for a KM), so a
  program has no library() of its own; it is sourced at the program's top,
  beside the report setup, and the banner says the design is the place to
  edit.  The number at risk is, by default, a plot of its own below the
  curves, its counts from the fit at the x axis's breaks
  (`summary(fit, times = ...)`), joined with patchwork; its new "Drawn as"
  choice gives ggsurvfit's `add_risktable()` instead.
  The Plot Designer's data steps are one list: a KM fit, summary
  statistics ... are steps that make an object of their own, and the
  steps after one go on in its pipe (the "Statistics" part is gone; a
  design saved before reads as one list).  A step added goes after the
  one chosen, a step on `df` before the first object.  "This piece's
  code" shows the lines the script has with the piece and not without it.
- **A code list edited where it is used.**  The code lists' part of
  1-1's column definitions (part 4), of a listing's data and of a
  figure's data lists each column with a list; a click opens that
  column's values, labels and order in a grid right below, in place --
  one grid at a time (a click on another column moves it, on the same
  one closes it).  A column without a list starts one from the select
  beside the buttons; "Add them to the list" opens the list it adds to;
  Copy... and the dialog of every code list (All code lists..., with
  reading a file) stay.
- **The table builder's rows as the code list of `variable`.**  Under
  the variables in order, "As the code list of variable" opens a grid
  like the code lists': each variable (value), the heading the table
  prints (label) and its place (order).  It is the same thing as the
  list and its labels above, kept in one place -- the variables sheet's
  `label` and `order` -- and the program still writes
  `plan_labels(variable = c(...))`: no new sheet or column.
- The builder's form, drawn again after an edit that changes its shape
  (a heading or the order above, a row of one's own), keeps the page
  where it was instead of jumping to the top.
- **The launcher's icon is the hex logo** (#291).  The desktop and Start
  menu shortcut (`add_shortcut()`) shows the package's logo at 128 px and
  up; at the sizes the desktop and the taskbar use (24 to 64 px) a plain
  hexagon with a "t", at 16 px the hexagon alone, so it stays clear.
  `data-raw/launcher-icons.R` makes the `.ico`, `.png` and `.icns`.
- **The report list shows and edits the named batches** (#301): a Batches
  column, and Batches... for the report chosen -- the names in use to
  choose from, a new one typed.

- **An official run of some of the reports** (#301).  The Runs tab lists
  the reports of a run, all ticked (All / None); a report unticked is left
  out -- its ARD program and its report program -- and a report the run
  makes that reads the ARD of one left out is said before the run.
  `run_batch(exclude = )` does it in R (`only =` stays), the runner takes
  `--exclude=<program>`, and the batch folder's `batch.txt` lists what
  was left out (the runs table too).
- **Named batches** (#301; #299 D6): a set of reports an official run
  takes by name -- Topline, Interim Analysis, Final.  A report's batches
  are the report list's new `batches` column (` | ` between several), so
  a later TOC import can fill it; `batch_sets()`, `set_batch()`,
  `rename_batch()`, `remove_batch()`.  `programs/batch.R` carries them as
  `.batch_sets`; `Rscript programs/autoexec_all.R --batch Topline` and
  `run_batch(batch = "Topline")` run one, into
  `runs/<date>_<time>_all_Topline/`, its `batch.txt` naming it.  In the
  Runs tab: choose a named batch (its reports ticked), save the ticks
  under a name (or over one), rename, delete.  The full run is every
  report and has no name.
- **Edit the definition outside the app: export, edit the copy, import**
  (#274, phase 2).  **Export the definition files** (Study tab, Files)
  gives a zip of `spec/` and `study.yml`, the ARD definition as an Excel
  workbook; **Import definition files** takes an edited copy back
  (workbooks known by their sheets, so a renamed copy works; `.yml`,
  `.json` or the zip).  Before anything changes, the copy is checked in a
  temporary folder: every reader, every cell that holds R (conditions,
  derived columns, `args`, `post`, code, figure-design code) parsed, the
  ARD definition and figure designs checked, and the programs of the
  reports it touches written and parsed.  Errors list file, sheet, row,
  column and problem and change nothing; warnings are shown and do not
  stop it.  The parts that differ are listed to choose from; the study's
  files are copied to `spec/.backup/<date>-<time>/` first, then the study
  is saved.  R: `export_spec_files()`, `preview_spec_import()`,
  `import_spec_files()`.  A direct edit of `spec/` is still found on open
  and goes through the same checks: a study whose files changed opens
  with a notice and "What changed"; files that do not read show a card
  (and a mark in the bar) with **Load from the definition files** and
  **Write the files back from the last save**, and the study cannot be
  saved until one is chosen; a save after an outside edit asks to load
  first; loading keeps unsaved changes and a draft, merged part by part
  (a part both changed asks which to keep).  The Japanese guide's 3.1
  says how.  The Japanese of tflspec's texts that now say "population"
  (tflspec #197: the figure designer, figure advice, five column
  descriptions) follows them.

- **A new study from the sample is ready at once** (#297).  The sample
  is copied (seconds) and opened; its official run, which makes its ARD
  and reports (a few minutes), runs in the background as the Runs tab
  starts one.  The study is in the list straight away, marked "(running)"
  until the run ends, and the app can be used meanwhile (before, it
  waited for the whole run with a progress note).

- R-CMD-check now also runs on R 4.2, the oldest R tested (`Depends: R (>= 4.1)` is kept; cardx, used by the generated ARD code, needs R >= 4.2).

- **A study opens in a fifth of a second** (#296, reported by the user).
  Reading a study from its saved state built every sheet's columns with
  `tflspec::tfl_table_spec()` again, sheet by sheet, on every open and
  twice on every save; they are made once a session now.
  `open_study()`: 1.8 s to 0.15 s (the sample study); a save that changes
  nothing 5.6 s to 2.0 s; copying the sample (without its run) 9 s to 5
  s.  The study list is drawn while the Study tab is hidden too, so a
  study made from another tab is in it when the tab is shown.

- No links to discussions by number, and no code copied from one (#298):
  the tests that run the example workbooks of tflspec (>= 0.0.24.9071)
  follow their new contents.

- **The English screens say "population"** (tflspec #191).  "Analysis set"
  and "population" were mixed; the screens, messages and the company
  standards' sheet description now say population (the Japanese stays
  解析対象集団).  The argument hints follow tflspec's new wording, so their
  Japanese is found again.  The Japanese guide says that a population is
  ICH E9 / CDISC ARS's analysis set (`analysisSetId` in ARS).  The SPEC,
  the ARD and the tokens (`population_id`, the `populations` sheet,
  `{OUTPUT_POPULATION}`) are as they were.

- **A report's code lists are where its data is made**: one place on the
  screen, one in the definition, one in the program.  Step 1 (Code lists)
  is gone, and the steps are 1 ARD, 2 Content, 3 Page and output (1-1,
  1-2 as they were 2-1, 2-2).
  - A table: 1-1's analysis data form has **Column definitions** in the
    order the program makes them -- ① columns added, ② made or
    changed, ③ kept (given, the subject key and the columns the
    analyses read are kept too), ④ the code lists of its columns.
  - A listing: its data, between the condition and the order (the rows
    sort in the lists' order).
  - A figure: the designer's data steps end with **Code lists** (read
    only, the report's); a step that orders a variable with a code list
    no longer offers its own values and labels.
  - Each place shows what the program makes of the values (`F →
    Female`), warns of the values the data has that a list does not (the
    program would stop on them) with a button that adds them to this
    report's list, and opens the editor (Edit...) or the copy (Copy...).
- An analysis's own filter says that a column with a code list holds its
  labels there (`SEX == "Female"`); a level field of an analysis offers the
  labels too.
- The code lists no longer give a variable's heading in the table (step
  2): the variables sheet's `label` does.
- A new study's setup no longer attaches tflspec (the programs call
  `programs/study_helpers.R`), nor do the app's previews.

- **The definition files are the study's source** (#274, phase 1).  A
  save records each definition file's fingerprint (`spec/` and
  `study.yml`, md5 and size) in the study's state; opening the study
  compares them.  Unchanged, it opens from its state as before.  Changed
  outside tflplanner (edited in Excel, copied in), the changed files are
  read and taken in (`$spec` says which files and parts, a history entry
  keeps the state before).  A file that does not read leaves the study as
  last saved, and no save writes over it (class
  `tflplanner_spec_changed`) until it is fixed and read again
  (`reload_from_spec()`) or written back from the last save
  (`write_spec()`, the rejected file kept in `spec/.rejected/`);
  `spec_status()` says which files changed.  A study unregistered and
  registered again takes in what was edited meanwhile.  A save reads the
  workbooks no more when their fingerprints are the recorded ones (about
  3 s less a save).  A figure design file tflplanner did not write is no
  longer deleted by a save.

- **The code lists put their labels on the data the programs make**
  (tflspec >= 0.0.24.9068).  A report's program writes its code lists at
  its head (`cl_race <- c(WHITE = "White", ...)`) and puts them on the
  columns it reads (`set_levels(RACE = cl_race)`): each a factor in the
  list's order, its values the labels, so the ARD (and a listing's or a
  figure's data) holds what prints.  A value the list does not have stops
  the program, naming the column and the value.  A listing's rows sort in
  the code lists' order.
- **`programs/study_helpers.R`**: the functions the programs call
  (`set_levels()`, `tag_ard()`, `fmt_ard()`, `fmt_pvalue()`,
  `keep_stats()`, `save_ard()`), written by tflplanner when the study is
  saved (`study_helpers_code()`, tflspec's `tfl_helpers_code()`) and
  sourced by `programs/study_setup.R` (its part 2).  The programs run
  without tflspec.  The app's previews define them too.
- The code lists' rows of `variable` (an earlier form of the variables'
  headings) move to the variables sheet's `label` when a study opens, and
  the app says so once.

- **Four more sample reports** (SAMPLE-01, made by
  `data-raw/make-sample-study.R`):
  - T-14-1-3: age group and sex, its ARD cards' default statistics (n, N
    and p) and its table n (%) -- the N rows the cells do not name are left
    out;
  - T-14-2-3: the mean change in systolic blood pressure at Week 24 with
    its 95% CI and p-value (`cardx::ard_continuous_ci()`, a one-sample
    t-test), every statistic in the ARD, its method and alternative text;
  - T-14-0-1: the study's information (dictionary versions, the dates of
    the data), with no analysis set (`<All Subjects>` under the title);
  - T-14-1-4: the screen failures' demographics, an analysis set by a
    condition alone (SCRF: `ARM == "Screen Failure"`, no flag).
- The Japanese help of the table layout's `blank_where` says it takes row
  positions too (tflspec >= 0.0.24.9067).

- **The sample's KM figure (F-14-2-2) reads the study ARD through
  `path_ard`** (`readRDS(file.path(path_ard, "ard.rds"))`), the folder's
  variable of the study's setup, as the other programs do (#268).
- **The programs as the study's setup has them** (#268, its second
  stage).  A study's programs read its folders through the variables
  `programs/study_setup.R` defines (`readRDS(file.path(path_adam,
  "adsl.rds"))`, `file.path(path_ard, ...)`) and call the packages it
  attaches without `pkg::`; `ard_setup.R` attaches only what the study's
  setup does not.  The company standards' setup code attaches dplyr too,
  and a table's default data part takes its rows of the study ARD with
  `filter()` / `select()`.  A study made before (no `library(dplyr)` in its
  setup) keeps `dplyr::` and runs as it did.  The app's previews (an
  analysis data, a listing) run the code as before.
- **Step 2-2 says what its fields are** (#280).  "Analysis ID (a set of
  analyses)" (several variables: one row per variable under the same ID in
  the ARD), "Grouping variables (by)" (whether they are the table's
  columns, rows or pages is step 3's) and "Analysis variables
  (variables)".  The levels and order are set in 2-1, for the analysis
  data's columns (the ARD and the table both), and in step 1; 2-2 has no
  button of its own for them.

- **The ARD programs as tflspec now writes them** (#281, tflspec >=
  0.0.24.9066): no function or loop of their own, one pipe an analysis
  from its data, the tidyverse layout, dplyr's verbs, the code lists on
  the data the analyses read.  A program ends in tflspec's
  `bind_rows(...) |> save_ard(output_id, definition = "...")`;
  `ard_setup.R` no longer defines `save_ard()` and sets
  `options(tflspec.ard_sources = c(setup = "programs/study_setup.R"))`, so
  the status still records the study setup each ARD was built with.
- **The subjects per group are GROUPN** (#281), which clinical reporting
  calls big N (was `BIGN`; "Subjects per group", was "Subjects per arm"):
  the first table, a stack's switches and the ARD tab's messages,
  `add_group_n()`, the sample studies.
- **The table builder's column header as a grid** (#278).  The header's
  lines stand under the table's own columns (the row-header columns, then
  one per value of the column variable), each line the same on each column,
  over each value of a key, one cell over all, none, or cell by cell: a
  text for each value, neighbouring cells merged into one (a spanner such
  as "Xanomeline" over two arms) or split again.  Cell by cell is written
  as `cols = "TRT01A = a | TRT01A = b"` and read back.  "What the header's
  {n} counts" says what is asked.  Categorical variables take a format of
  one's own beside n (%), n/N (%) and n (it starts from the one chosen; a
  blank one writes nothing); "A row of your own" is open and says how a
  format is written; each variable's panel shows, faint, what it prints
  (the Statistics card's rows and decimals, or the categorical format),
  written for no variable.

- **The report programs, tidied** (#275).  Their head is the banner and
  one line (`stopifnot()`) that they run from the study folder; the
  report's id is `report_id`.  The data part has one heading, and the
  company's default `table_data` / `table_process` no longer write "Leaves
  ...", "Input data are in ..." or a commented-out rework; a table's
  `saveRDS()` ends the data part.  A figure's plot goes to the report as it
  is (`rtf_figures(doc, plot)`, rtfreporter >= 0.8.2.9029).  The setup's
  helpers are `save_ard(ard, output_id, definition = )` and
  `record_report()` (were `.save_output()`, `.record_report()`); the ARD
  programs' banner is shorter and `batch.R`'s lists are indented.
  `report_setup.R` now writes the study's header and footer once also when
  the study has default header or footer rows (before: only with
  study-wide tokens or a font), and the report programs use them.  The
  sample's header says `{OUTPUT_LABEL}`, `{OUTPUT_TITLE}` and
  `<{OUTPUT_POPULATION}>`, each report's title and analysis set its tokens;
  its reports print as before.

- **Step 3 and the figure designer, faster** (#273).  Step 3's table as it
  prints is made again only when what it is made from changes (the
  report's rows of the table sheets, the rounding, its ARD rows): back to
  a report, its table is there in 0.3 s (from 1.7 to 2.4).  Another report
  shows its table at once (the short wait is for typing).  The figure
  designer shows its form and code first and the figure when it is drawn
  ("Drawing ..." meanwhile; the whole page waited for it), and keeps a
  figure's drawing for the same design, definition and data files: its
  form in about a second (from 4.4 to 5.7 s), back to a figure in under
  one.  And everywhere: a part shown just now (a panel, a form) no longer
  waits seconds for an unrelated timer before it is drawn (shiny resumes a
  hidden output after the update that shows it and schedules no other; the
  page now asks for that update).

- **Step 2: an analysis opens in under a second** (#272).  Its function
  list (a hundred rows, folded until "Change") is made when it is opened,
  not with every form; the functions an analysis can name are made once a
  session (again when the study's own functions change), not for each
  analysis in the outline at each click.  Opening an analysis: 0.6 to 1 s
  (from 1.5); opening its list: 0.2 to 0.5 s.

- **`programs/study_setup.R`: one setup for every program** (part of
  #268).  Every study has `programs/study_setup.R`, which
  `programs/ard/ard_setup.R`, `programs/tfl/report_setup.R` and
  `programs/tfl/fig_setup.R` each source first.  It has three marked
  parts, run in order: (1) the company standard, copied when the file is
  made from the standards' new sheet `setup_code` (one line of R a row;
  by default `library(cards)`, `library(rtfreporter)`,
  `library(tflspec)`); (2) tflplanner's, written again on every save: the
  study's folders (`path_adam <- "data/adam"` ...) and what the study is
  (`study_id`, `study_title` ...); (3) the study's own, never touched.
  Saving rewrites part 2 only (parts 1 and 3 stay byte for byte); a part 2
  edited by hand goes to `programs/.edited/` first.  `report_setup.R` is
  now always written and sourced, and a report program no longer writes
  its own `library()` lines.  The ARD status (`ard_status.csv`) and the
  new report record (`output/tfl/report_status.csv`) keep the fingerprint
  of the study setup an output was built with, so a change to
  `study_setup.R` marks the outputs outdated.  An existing study gets the
  file on its next save.  `setup_tflplanner(standards = )` refuses a
  `setup_code` that does not parse, or has Excel's curly quotes.

- **Step 2: an analysis opens in a second, not in 3 to 12** (#269).  The
  parts of its form were made only when some unrelated timer woke the
  session (shiny resumes an output that was hidden after the update it
  shows in, and schedules no other); they are made with the form now.  And
  every DataTable of the app was fitted again each time a folded part
  opened or an output was drawn -- the form has several -- forcing the
  page's layout for seconds; only the tables in what was shown are fitted,
  once.

- **The screens, easier to follow** (#269).  Step 2: an analysis opened is
  brought into view (its form opened below the window's edge, and nothing
  moved); no empty frame when none is chosen.  Steps 1-4: SPEC | Code |
  Result stays beside the form on a wide screen, so the table as it prints
  is in view while the builder is changed.  Step 3's column header: a line
  is a third as tall (its row-header text beside its value columns, the
  style on one row).  The study tab's settings are four sections -- the
  study, every report, keys and setup code, files -- the first two open
  (each kept as the viewer leaves it); the study list shows the ID, the
  whole title and when it was saved (the compound and phase are in the
  detail).  The Runs tab's batch list and definition check are as tall as
  their rows; step 1's box of every variable is on one line; the data
  tab's empty preview says to click a file.

- **Step 4: this report's font and size** (#266; tflspec 0.0.24.9063).
  Above SPEC | Code | Result, a report may have its own font and size
  (pt); blank, it takes every report's (the study tab), shown greyed.  A
  value is the report's own row of the page sheet (only the field
  changed is written), and its program says it.

- **The reports' font and size, set once** (#264; tflspec 0.0.24.9062).
  The company standards' `settings` gain `font` and `font_size` (in
  points; blank: rtfreporter's, Courier 9 pt): a new study gets them, and
  "Add the company's study defaults" adds them where the study has none.
  The study tab sets them under the header's words (Font, Size (pt); the
  page sheet's study row).  `programs/tfl/report_setup.R` says them once,
  `options(rtfreporter.font = , rtfreporter.font_size_half_points = )`; a
  study with a font or size uses it even without tokens of its own.

- **A designed figure's program, shorter** (#240).  Its figure is `plot`
  itself (no `fig <- p`, `plot <- fig`), its palette the study's figure
  setup's `tfl_colours()`, and the design's code is in the style of the
  other programs (`|>`, one-line parts: tflspec #164).  The sample's
  reports are the same.  Needs tflspec 0.0.24.9061.

- **Code lists from 2-1, 2-2 and step 3; a variable's label from them**
  (#262; tflspec 0.0.24.9060).  [Levels and order (code lists)...] in 2-1
  (the columns the data makes and adds) and 2-2 (the analysis's groups,
  variables and strata), and [Code lists of this table...] in step 3, open
  step 1's editor on those variables; a copy comes back to it.  Step 3's
  drag lists show each level's code list text, faint.  A variable's label
  can be the report's code list of `variable` (`variable / AGE / Age
  (years)`): step 3's label field shows it faint when the variables sheet
  has none, and writes only a label changed there.  A label is a report's:
  there is no dictionary across the study or the standards.

- **A user-code report's program, shorter** (#253).  The function that
  makes its `content` what rtfreporter takes (a ggplot becomes a figure;
  anything else is said) is `report_content()`, written once in the
  study's `programs/tfl/fig_setup.R`; each program ends with
  `content <- report_content(content)` instead of 15 lines of its own.
  The sample's reports are the same.

- **A session that stops on an error no longer stops the app** (#256).
  With `stop_on_close = TRUE` (the launcher's), a page that went grey on
  an error ended its session, which was taken for a closed tab: the app
  stopped 5 seconds later and the page's Reload could not bring it back.
  Such a session now waits 10 minutes.  The launcher keeps what the app
  writes to the console (its errors, with the calls) in `app.log`, beside
  `launcher.log`.  Needs shiny 1.8.1.

- **2-1's columns made, by kind** (#255).  Each column the data makes is
  a line of its own, made by kind: split by conditions (the conditions made
  with the condition builder; one gives `ifelse()`, more give
  `dplyr::case_when()`), cut a number into groups (`cut(..., right =
  FALSE)`), the days between two dates (`as.numeric(END - START)`, + 1 if
  wanted), or an R expression (a variable can be put in).  The sheet keeps
  `NAME = R | ...` (derive) as before, so the definition and the generated
  code do not change; the form reads back these four forms only, anything
  else is an R expression, and a column not opened is saved exactly as
  written.  The R stays at hand under "As R".

- **2-1's form, in plainer words** (#254).  The condition is a filter:
  "Filter (a condition)" (2-1) and "This analysis's own filter" (2-2),
  their help alike.  "Write the condition as R (inside subset())" and
  "Write this analysis data whole as R (code)" tell the two apart.  The
  columns added from the subjects' data say what they do (nothing chosen:
  nothing added) and are not offered while the data is made from the
  analysis set's own data, which has them all.  The condition builder's
  variable box is as tall as the others from the first (it took a line of
  its own until its list was opened).

- **A code list is a report's** (#251; tflspec 0.0.24.9058).  Step 1 edits
  the report's code lists: the variables it uses (its analyses, what its
  data derive, what its table shows; all with a box), [Copy code lists...]
  from the company standards or another report (the rows copied are the
  report's own), and a file read into the report.  The company standards
  have a `codelists` sheet (`standard_codelists()`), with CDISC's usual
  lists by default (SEX, RACE, ETHNIC, AESEV, AESER, AEREL, AEOUT, EOSSTT).
  `set_codelist()` takes the report; `import_codelist()` copies another
  report's.  The Data tab's study code lists are gone.  A report's ARD uses
  the code lists of the variables its analyses read; step 1's result says
  which.  A study of the old format (analysis data or code lists without a
  report) is said so once when it is opened, in the app's language: make
  it again from the sample, or make a new study.  Step 2's 2-1 and 2-2 say
  the same in a line, the checks' messages folded under it.  The sample's tables have their own code lists
  (#248): the arms in each, SEX / AGEGR1 / RACE / ETHNIC in the
  demographics tables (the CRF's values the data have none of print with
  0), EOSSTT in the disposition table; the variables sheet no longer
  says the same order again.

- **Step 3: statistics as rows, each statistic's decimals** (#249).  The
  table builder's Statistics card:
  - Values: the numbers, rounded here, or the ARD's text as step 2
    formatted it (`tables$value`; no decimals to set then).
  - A continuous variable's rows, chosen and ordered by dragging: the
    company standards' rows (single statistics too: Mean, SD, SE, Q1, Q3,
    Min, Max) and a row of your own (a label and a template).
  - The decimals of each statistic, the same for every analysis variable
    (the `digits` sheet), and a variable's own.  They replace "decimals the
    data are collected with": a statistic's decimals are fixed numbers now.
  - Company standards: the `statistics` rows have no digit rules; a new
    `default_digits` gives a new study each statistic's decimals.

- **The `digits` sheet** (#247; tflspec #168).  A study's definition keeps
  each statistic's decimals and a variable's exceptions (step 3's SPEC,
  the tab "digits: decimals"); a table's templates take them where they
  say no format.  Needs tflspec 0.0.24.9057.

- **The sample study, after an audit** (#238).  T-14-2-2 (KM estimates)
  has no "Characteristic" over its rows.  L-16-2-7 lists the
  treatment-emergent severe adverse events (2 of 43 were not), titled so,
  the arms in their order (sorted by `TRT01AN`, which the sample's ADSL and
  ADAE now have, as an ADaM does).  F-14-2-1's code says its parameter.
  The other reports are the same, byte for byte.  The company standards'
  `table_data` code takes a report's rows of the study ARD with
  `subset(ard, output_id == ..., select = -c(output_id, analysis_id,
  population_id))`.

- **An analysis data is a report's** (#244; tflspec 0.0.24.9055).  2-1 lists
  the report's own rows, all of them; delete, "in use" and the names are
  the report's; `set_analysis_data()`, `remove_analysis_data()`,
  `copy_analysis_data()` take the report.  [Copy from another report...]
  copies a report's analysis data under the same names
  (`import_analysis_data()`).  The Data tab shows the datasets and the
  analysis sets only: the analysis data grid is gone (2-1's SPEC tab).  A
  report's analysis set data, the TOC's, and copy / rename / remove of a
  report follow.  The condition builder shows a variable on one line, cut
  with ..., the whole on hover.  The sample's tables each have their own
  rows; the reports and the ARD are the same.  A study made before has no
  report on its rows: tflspec says so (make it again from the sample).

- **Every report's header, defined once** (#223).
  - The package's header for a new study: `{COMPANY}` and
    `{ANALYSIS_TYPE}`, then `PROTOCOL: {STUDY_ID}` and the page, a blank
    line, then the report's `{OUTPUT_LABEL}` ("Table 14.1.1"),
    `{OUTPUT_TITLE}` and `<{OUTPUT_POPULATION}>`.
  - The study's words are set once, in the study tab (Company, Analysis,
    Protocol): the tokens sheet's study rows; the company standards'
    `default_tokens` give a new study's.  A new study's protocol is the
    `{STUDY_ID}` token, not text put in the header.
  - Taking in a TOC writes each report's own tokens (its label -- a new
    item of the map, "Label" / "Display ID" ... --, title, analysis set,
    section), only those the study's header, footer, titles or footnotes
    say; a value changed here is kept the next time.  When the header says
    `{OUTPUT_TITLE}`, the TOC's first title line and its analysis set are
    not title lines as well.
  - `programs/tfl/report_setup.R` (`report_setup_code()`) holds the
    study's tokens, header and footer; each report program sources it and
    says only its own tokens.  A study with no tokens of its own (the
    sample) writes its programs as before.
  - Step 4's header tab says the report has the study's header, with
    "A header of this report's own" (a copy to edit) and "Back to the
    study's".
  - The page sample fills the report's tokens as its program does, and
    leaves out a line they leave empty.
  - Needs rtfreporter 0.8.2.9025 and tflspec 0.0.24.9054.

- **Unregistering a study loses nothing** (#237).  `unregister_study()`
  (Unregister) puts what tflplanner kept about the study -- its saved
  state, history and unsaved changes -- into the study folder
  (`.tflplanner/`) instead of deleting it, and `register_study()` (Register
  a folder) takes it back: the study comes back as it was (when `spec/`
  was changed in between, from `spec/`, the kept state going to its
  history).  The dialog says the folder is not deleted.  tflplanner never
  deletes a study folder.

- **Nothing updates on its own** (#226).  `add_shortcut()` no longer makes
  the "update and launch" shortcut unless asked (`update = TRUE`); the
  Start menu entry stays.  The app's check for a newer version is off
  unless turned on in its settings (or `setup_tflplanner(check_updates =
  TRUE)`; a setting already made is kept), and it only tells: tflplanner
  installs or updates nothing unless you ask -- `update_tflplanner()`, or
  the "update and launch" shortcut.

- **2-2: the method first, as a heading with its function** (#236).  An
  analysis's form starts with its method, large, and the function it calls
  (`cards::ard_stack_hierarchical`, a company keyword's too), then its ID
  and label; the 2-2 list names the function as well.  The arguments under
  it are "Only for <function> (i)" (the data, groups ... above are those
  every method has): the required ones, those the method writes and those
  given first, the rest folded ("n more, at their defaults").  A blank
  says what it gives: the study's subject key, the company method's value,
  none, or the function's own default.  The help of the arguments used
  most says when to use them (tflspec's catalog, translated).  The
  variables, groups and strata show in the definition's order (a
  hierarchy's outermost first), not the data's; the statistic N reads
  "Number of non-missing values".

- **2-1 lists the report's own analysis data** (#235): those its analyses
  read (and what they are made from), the data they read without a name,
  and those made on its 2-1 until an analysis reads them; the study's
  others are on the Data tab.  The sample's tables all read analysis data
  (adsl_saf, advs_w24, adtte_ttde, adae_saf) and have SAF as their
  analysis set: the demographics table's 2-1 shows adsl_saf alone.  From
  S2's look at the sample: T-14-1-1's total N and T-14-1-1S's `.total_n`
  (no table prints them) are gone, advs_w24 says its parameter
  (`PARAMCD == "SYSBP"`), and T-14-2-1's decimals are the table's alone
  (its analysis's `formats` gone).  The reports are the same; the ARD has
  the same numbers, less the two total N rows.

- **The design concept, written down** (#224).  The README has a
  "Concept" section and the Japanese guide a "設計の考え方" section:
  code for people to read and finish, the typical analyses kept simple,
  R code kept in the spec, shared parts defined once, screens => spec =>
  code.  Docs only.

- **A hex logo, shared with rtfreporter and tflspec** (#230), made with the
  site's favicons by `data-raw/logo.R`.

- Added a root `CITATION.cff` so GitHub's "Cite this repository" button
  works (#229).

- **The TOC's datasets** (#220; tflspec's `tfl_read_toc()` datasets).  A
  `datasets` item of the mapping: kept as the report list's datasets
  (shown until the report's definition names its own), a new listing's
  dataset and a new figure's datasets; with "Make the tables' analysis
  data from their datasets" (ticked), a new table gets `<dataset>_<set>`
  kept to `adsl_<set>`'s subjects (found or made), which its first
  analysis reads.  Taken in again, they are made again only when asked.

- **The TOC's population is the reports' analysis set** (#219).  The TOC
  dialog shows each text of its population column with the study's set it
  is -- matched by id, label (the company standards', else the flag's), a
  usual word for the flag or the id in the text: "Safety Population" is
  SAF -- and it can be changed there; taken in, each report's set is set
  as step 2 sets it (`toc_apply(populations =)`, `toc_populations()`).
  The Add dialog gives a new report its set too.

- **A report's analysis set is one value** (#217).  The report list's
  `population` (the TOC's, step 2's): `set_report_population()` writes it,
  makes the data of the set's subjects (`adsl_<set>`) when there is none,
  and moves the report's analyses to the new set's data (and the data kept
  to the old set's subjects, found or made with the same definition; data
  of another kind are left and said).  Step 2 chooses it above 2-1 ("This
  report's analysis set"); 2-1's new data and 2-2's new analysis start
  from it.  A set's flag on another dataset (ADAE's SAFFL) is the set only
  when it agrees with the set's dataset subject by subject; else it is a
  condition, and the form says so.  The company standards' analysis sets
  are those the company may use (a new study gets them all).  The report
  list shows the report's set, else its analyses' (their analysis data's
  too).

- **The sample's ADSL has ITTFL, EFFFL and PPROTFL** (#218), derived by
  data-raw/make-sample-study.R (pharmaverseadam's ADSL has SAFFL only of
  the population flags; the sample's README says how), so step 2 has
  flags to choose from.  The study's analysis sets stay SAF only; the
  reports and the ARD are the same.  The script also writes T-14-3-1 with
  the analysis data the sample has had since #213.

- **2-2's analysis form, after a look at it** (#215).  The card has one
  heading ("Analysis A1 (i)"; "Analysis" while none is chosen); the method
  is "The method (method)" and is described once, under it; the formats'
  (i) is on their heading; the "Write as code" dialog names the button
  "Apply to the analysis".

- **Help, after a review of every screen** (#211).  A heading's (i) opens
  on a click or a tap too (a tablet has no hover) and stays open until it
  is clicked again, something else is clicked or Escape is pressed -- for
  the (i) of step 2 too.  The "About this (i)" lines are a part's heading
  with its (i) now (the Data tab's parts, steps 1 and 4, the table
  builder, a listing), and on the Data tab a sheet's editing note is in
  the same (i) (no two (i) in a row).  The rest of the paragraphs went into
  an (i): the analysis sets, step 1's Code and Result (they say what
  their names say), the data code's labels (their help was a title shown
  only on hover), Preview, the study settings and "add the company's
  defaults", the Add dialog's "Start from the data", and the figure
  designer.  Long tips are shorter (own ARD functions, ARDs taken in); the
  study ARD's no longer repeats the line on the page.  Japanese: one
  missing translation, and the statistics catalog's labels (shown in
  step 2-2).

- **Step 2's help as (i)** (#210).  The paragraphs under the ARD card's
  heading and an analysis's form head are (i) on their headings now; 2-2's
  says what ARD and an analysis set are.  Data, the analysis's own
  condition (now "Rows kept (this analysis's own condition)", the words of
  2-1), the other arguments, the groups, "Repeated within", the
  denominator (what each choice is of, an AE table's example) and the
  format have an (i); so have 2-1's analysis set, condition, subjects tick
  and "Write it as R".  "Run together with other analyses (one call)".
  The statistics catalog's labels go through the translations
  (`stat-label:<label>`).

- **2-1, after trying it** (#206).  What a data is made from and its
  analysis set come first (the name follows them).  The analysis set is the
  condition's first row, put in at once (`SAFFL == "Y"`, changeable, other
  rows under it), chosen from the study's analysis sets and the population
  flags of the data.  Saved, that row as it is is the sheet's
  `population_id` (SAF: not in `where`); changed (another value, "!="), it
  is a condition like the others.  A sheet's `population_id` is shown as
  that row.  The same data as one already there is said so (use it, no
  need to make it again).  2-1's and 2-2's explanations are in a
  tooltip on their headings.  The sample's T-14-3-1 reads two analysis
  data, adsl_saf (the safety set) and adae_saf (its TEAEs), its
  denominator adsl_saf: the same tables and ARD.  The report list names
  the datasets an analysis data is made from (ADAE, ADSL).

- **Long explanations are a heading's (i) now** (#207).  The paragraphs of
  explanation on the screens made them hard to read; they are a tooltip
  on the heading they explain (shown on hover or keyboard focus), or an
  "About this (i)" line where there is no heading: the Study tab (own ARD
  functions, keys and setup code), the Data tab (the study ARD, ARDs taken
  in, datasets, analysis data, code lists), the Report list, steps 1, 3
  (user code, the plot written by hand, a listing's columns, the builder)
  and 4, the Runs tab, and the note under every sheet ("Editing the sheet
  (i)").  Short labels, warnings, errors, dialogs and the statistics notes
  stay on the page.  One component for it (`help_tip()`, `with_tip()`,
  `about_tip()` in R/help_tip.R): a focusable (i) named by its help.

- **The report list has a section (heading)** (#203).  A TOC's heading
  rows ("14.1 Demographics") were passed over; now each report taken in is
  under the heading above it (or the TOC's section column, a new item of
  the mapping and of the company's `toc_map`; tflspec >= 0.0.24.9050).
  The section is a column of the report list, given in the Add dialog
  (the chosen report's by default) or next to the description on step 3's
  Data code tab; one given here stays when the TOC is taken in again.  The
  list on the left of Make a report is folded by the sections (a heading,
  else the ID's numbers as before), headings in the order of the numbers
  they start with.

- **2-1: the analysis set as a field of its own** (#202).  After "Made
  from", "Analysis set": the study's analysis sets, each with its
  condition, or none (written as population_id); the condition builder
  below for the other conditions.  Kept to another data's subjects, the
  field shows that data's analysis set instead.  A new data starts with the
  study's first analysis set and is named after what it is made from and
  the set (adsl_saf, adae_saf).  A population flag of ADSL that is no
  analysis set yet is made one in one click ("Make PPROTFL an analysis
  set": ADSL, `PPROTFL == "Y"`, id PP), chosen at once; the Data tab has
  it too.

- **The condition builder: variables in ADaM's groups, no shortcuts**
  (#199).  The analysis sets' shortcut buttons (the set, and "not" it) are
  gone: they did what "Keep to the subjects of ..." does, and a study has
  many population flags.  A variable is chosen from groups, as ADaM names
  them, each with its label and searched as before: population flags
  first (SAFFL, ITTFL, FASFL, PPROTFL, RANDFL ...; only those the data
  has), analysis flags (ANLxxFL; a flag of data of several rows a subject),
  treatment (TRTxxP/A, TRTP/TRTA, ARM ...), parameter (PARAMCD, PARAM),
  timing (AVISIT, ATPT, APHASE, APERIOD) and the others.  A flag (Y / N)
  offers its blank as a value too, with its count: written `x %in% c("N",
  NA, "")` (a blank is NA or "", as the data was read).
  A flag (*FL) chosen starts as "= (any of) Y", to change from there (Y
  taken out, N or the blank put in); another variable starts empty.
  `condition_builder_server()` has no `shortcuts` any more;
  `.cond_var_kind()` gives each variable's kind (population, analysis,
  treatment, parameter, timing, other) and `.cond_population_flags()` a
  data's population flags in ADaM's order, for forms outside it (2-1).

- **The Study tab's two cards side by side; 2-1's data without a name
  chosen as the others** (#198).  The Study list fills its column (5 of
  12, the settings 7) with the usual gutter between, stacked on a narrow
  screen (it stopped at its table's width).  In 2-1 a data the analyses
  read without a name has the list's mark and opens its settings below
  with a click, the name the program gives it filled in; saving names it
  (the "Give it a name..." link is gone).  2-1's condition has no
  analysis-set shortcuts any more (they repeated "Keep to the subjects of";
  the condition is built from a variable and its values); a new data is
  still named after what it is made from and its condition (ADSL with
  `SAFFL == "Y"`: adsl_saf; a flag set to "Y" names it).

- **An app started before an update says so** (#196).  The shortcut opens
  tflplanner if it already runs, so one started before an update (still
  running) kept showing the old version.  Its page now says, at the top, "A
  newer version is installed: tflplanner X (this window runs Y). Save, then
  Close (top right) and start tflplanner again." -- for tflspec and
  rtfreporter too.  Nothing is stopped by itself (unsaved changes).

- **From the screen review** (#193).
  - The report list's buttons (Add, Copy, Rename, Delete, the arrows,
    Take in a TOC...) are above the list, beside its search: below 200
    rows no one found them.
  - "What do these mean?" on the left of Make a report says what the
    marks after a report's title are (made, to make again, not made yet,
    error).
  - The Runs tab says "Reading the state of N reports..." in its table's
    place until the table is drawn (a study of 200 reports takes seconds;
    the card looked empty and broken).
  - "Choose a report on the left." is now "Choose a report above, or in
    the list on the left (> opens it).": with the left folded it pointed
    to nothing.
  - Step 3, a table with no ARD yet: its Result says only that the button
    above makes the ARD (the note above said it all a second time).

- **Screen review: step 2 and the Data tab** (#192).  Step 2's check
  line says this report's analyses and the study's (it said the study's,
  299, while a report was open); "2-2 Analyses"; side by side, the forms on
  the left get more room; the code under an analysis's form says it is that
  analysis alone (the report's program: Code on the right).  The Data tab
  says its analysis data can be changed there too, and its datasets,
  analysis sets and analysis data have their column help; 2-1's SPEC says a
  new data's row comes when it is saved.

- **Spaces typed at the start or end of a text are kept** (#190).  The
  definition's rule (tflspec's, for text columns) is that such spaces
  count only inside quotes (`"  Total"`), so a pasted cell's stray spaces
  do not print.  A form now writes a text typed with them quoted, and
  shows it without the quotes: the builder's column header (the form that
  writes a text column of the display sheets).  The grids say the rule in
  their note.  Nothing else changes: SAMPLE-01's programs, ARD and RTFs are
  the same, and no fingerprint moves.

- **A program is the definition's: saving writes it again, even one edited
  by hand** (#187; design 13-00).  A report, ARD or run program whose
  checksum no longer matched (edited by hand) used to be kept on save, so
  the program and the definition drifted apart and an official run ran
  the edited one.  Now a save always writes the programs from the
  definition; one edited by hand is first copied to `programs/.edited/`
  (named after its place and the time: `tfl_T-14-1-1_<date>-<time>.R`)
  and the save says so.  What it changed belongs in the definition: the
  data code, a user-code report, a custom analysis (args, post), where /
  derive.  The "Regenerate this program" button and
  `save_study(regenerate =)` are gone.  An ARD program of a report no
  longer defined goes even when edited (its copy kept the same way).  The
  guide says it (11.1): to use a generated program as a template, copy it
  elsewhere and edit it there, outside the tool.  **A study with programs
  edited by hand gets them written again at its next save** (their
  results may change; the edited ones are in `programs/.edited/`).

- **A form writes only what was changed in it** (#187; design 13-2).
  Each form of steps 1, 3 and 4 has a test: a definition with values the
  form cannot show is read, shown, one other field changed and saved, and
  those values are as they were.  What it found:
  - The table builder, only shown, wrote its own cells over the report's:
    a statistic's own template, its condition (`when`), `signif` and
    digits, and renumbered the variables' `order`.  It now writes only the
    parts changed (the key, a label, the order when moved, the levels, the
    statistics, the decimals -- the digits then, the templates kept --,
    the categorical format).
  - The listing form, only shown, erased `blank_row` and `wrap`; a dataset
    the catalog does not have became blank; with no data in the catalog
    the form did not draw.  Its fields are now written into the row as it
    is, each only when changed, and a value the choices lack is offered.
  - The figure designer turned a field's value through the form on first
    show (a number written as text became a number), and an edit of the
    whole figure dropped its arguments the schema does not list.  It
    writes only the fields changed, into the arguments as they are.
  - `builder_write()` has `was`: the description as read (only what
    differs from it is written).

- **2-2: an analysis written as code** (#186; tflspec #143).  "Write
  this analysis as code..." in the analysis form shows the code its fields
  make now and, confirmed, makes it a custom analysis with that code and
  its formats written out: the same ARD, then the code is changed for what
  the fields cannot say.  A custom analysis has its code on the form.  The
  code is part of the definition: the program is never edited.

- **2-1: an analysis data written as R** (#184; tflspec #141).  "Write it
  as R (code)" in the form: R whose value is the data, for what the fields
  cannot say.  Written, the fields that make a data are greyed and saved
  blank; "Start from the generated code" fills it with the lines the
  program makes the data with now.  The R is part of the definition: the
  program is never edited.  `set_analysis_data(code =)`.

- **2-1 in step 2's SPEC | Code | Result** (#182).  The right of step 2
  follows what is open on the left: an analysis data open in 2-1 shows its
  rows of analysis_data in SPEC (the report's and the one open) and its
  preview in Result (rows, subjects, the first rows); closed, or an
  analysis chosen in 2-2, the right is 2-2's as before.  2-1's sheet and
  preview leave the left card.

- **SPEC | Code | Result on the right of each step** (#179).  Every step of
  making a report has the same three tabs on the right, the form (the GUI)
  on the left: SPEC the step's rows of the definition for this report (to
  edit; what was "Details (the sheet)"), Code the program written from them
  (to read, with line numbers and a copy button; a program is never edited
  in the app), Result what running it makes (step 2 the ARD, step 3 the
  table / listing preview, step 4 the page).  The figure designer keeps its
  own layout for now.
  - Step 1: SPEC the code-list sheet, Code the code-list part of the ARD
    program, Result the code lists this report uses (the study's and its
    own).
  - Step 2: the analyses sheet moves from the left card to SPEC; Code and
    Result as before (opens on Code).
  - Step 3: the table builder stays on the left, the sheets, the program
    and the preview to its right (opens on Result); a change in the sheets
    redraws the builder.  A listing's own row is a grid in SPEC; user code
    runs in Result.
  - Step 4: SPEC the report sheets, Code the program, Result the page.
  - The old sub-tabs (Builder / Spec sheets / Program, Page / Program) are
    gone.

- **2-1 as one list of analysis data** (#178).  2-1 no longer has the
  subjects and the data analysed apart: one list (the report's first, the
  study's others after them), a mark on the one chosen, [New analysis data]
  [Copy X] [Delete X...].  The form: the name first; what it is made from;
  "Keep to the subjects of adsl_saf" as a tick (on by default when the
  study has a data of one row a subject, a choice only when it has several);
  the rows kept with the condition builder (#175), the analysis sets and
  their opposite as shortcuts; the columns.  A data of one row a subject
  made later keeps the report's other analysis data to its subjects (said
  so; it moves above them).  2-2's question about making the subjects
  first is gone.  2-1 has its own sheet, the report's rows of
  analysis_data; a `from` the form has no choice for is kept as written.

- **A condition built from rows** (#175).  A component for the analysis
  data's and the analysis sets' `where` (2-1 takes it in next): rows of a
  variable (with its label), an operator (=, not equal, <, <=, >, >=,
  blank, not blank) and values -- a categorical variable's chosen from the
  data's, with their counts, searched as the function search searches --
  ANDed, in "or" groups if need be; shortcuts (the study's analysis sets)
  add their rows.  It writes the R the definition keeps (`SAFFL == "Y" &
  PARAMCD %in% c("ALT", "AST")`; not equal as `!(x %in% ...)`, so a blank
  counts as "not Y") and reads it back; what the rows cannot hold stays
  as R, in a field of its own.

- **The Study tab and the Data tab, after trying them** (#174).
  - The Study tab: "Study list" on the left; on the right one card, "Study
    settings": the study's title, compound, phase, rounding, its keys and
    setup code, and its files.  "Own ARD functions" below, the company's
    (every study) and this study's apart, with how many of each.
  - The study open, its ID and title, is in the bar of the tabs on every
    tab; a click on it goes to the Study tab, where studies are chosen.
  - A table drawn while its tab was hidden (headers one letter a line)
    measures its columns again when the tab shows.
  - The Data tab: "Into" and "Add files" level; the file's "Preview", 50
    rows a page with paging.

- **The runs table is quick for a big study** (#167).  `study_status()`
  wrote every report's program again to see whether it is current, each
  building the whole study's spec: a minute for 200 reports.  The spec is
  now built once and each program kept while the definition is the same
  (10 s the first time, about a second after); the answers are the same.
  Saving a study, which writes every program, is quicker too.

- **Texts name the tabs as they are now** (#171).  About 35 messages and
  help texts still pointed to the old tabs (the ARD tab, the Reports tab, the
  Tables / Figures / Listings tabs, the Results tab, the sidebar ...); they
  now say step 2 (ARD), the Report list, Data > Study ARD, step 3 (Content),
  step 4 (Page), the Runs tab, Study > Own functions or "on the left", in
  English and Japanese.  A few Japanese wordings are made consistent.

- **The Data tab has the study's data in one place** (#168).  On the left
  what there is, on the right the one chosen: the files (SDTM / ADaM / other,
  filtered by kind, a file's first rows), the ARDs (the study ARD with its
  logs and the errors inside the analyses; the ARDs taken in) and the
  definitions (datasets, analysis sets, analysis data, and the study's code
  list -- its rows for every report, read from a file here too).  The data
  catalog is its grid alone, with a line for a file that is not there.

- **Find the report to make** (#165).  The left of Make a report is a
  list to search, for a study of 100 or 200 reports: each report's ID,
  title and state (made / to make again / not made / error); a search on
  the ID, title, analysis set and data (as the function search: case,
  full / half width); the TOC's sections, folded one by one, with their
  counts; a filter by state; the arrow keys and Enter.  "Study defaults"
  and "ALL" stay first.  Folded, a chooser with a search takes its place
  at the top (the browser remembers it folded).  The report list and the
  runs search the same way.  The
  list's state is read from the files (a quick look at 200 reports).



- **Step 2 as lists** (#164).  2-2: [New] [Copy] [Delete...] over the list
  of the report's analyses; an analysis opens below the list when clicked
  (another click closes it), so the step starts as the list alone.  Copy
  makes the analysis again after it, a stack with the ones inside it
  (`copy_analysis()`).  2-1: [Copy] [Delete...] over the analysis data; its
  form opens below the list, not in a dialog (`copy_analysis_data()`).  The
  form's argument headings have an (i): the hint of that argument for that
  function, as a tooltip, instead of a line under each field.

- **Function search settings, reviewed** (#160).  Negative binomial
  starts with `exponentiate = TRUE` (rate ratios, as Poisson); the logistic
  setting says its confidence interval is profile likelihood (SAS's is
  Wald, which `args` alone cannot give); "Start with this setting" says
  when it replaces the analysis's own arguments.

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

- **The table engine is rtfreporter's** (plan E): tflspec 0.0.23.9000 keeps the specifications only, and
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
