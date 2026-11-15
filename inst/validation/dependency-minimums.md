# tflplanner: the minimum versions of its dependencies

Each minimum in `DESCRIPTION` is the first version of that package that has
every function and argument tflplanner uses: in `R/`, in the tests that
always run, and in the code its SPECs and templates write (tflspec writes
that code, so a function it writes counts). For each package the table
names what was found, the version that first has it, and where that is
recorded. The minimum is the latest of those versions, raised to what a
dependency needs in turn (shiny needs bslib, later and htmltools of a
version; processx and testthat need ps of a version; ggplot2 needs withr of
a version).

**The two ends.** *The minimum environment* is R at the minimum
`DESCRIPTION` names (`Depends: R (>= 4.3)`) with every direct dependency at
exactly its minimum (the R-CMD-check job *minimum environment*). The
suggested packages with no minimum, and every indirect dependency, come
from CRAN as it was on **2026-07-07**: the Posit Package Manager snapshot of
that date (`https://packagemanager.posit.co/cran/__linux__/noble/2026-07-07`,
for Ubuntu 24.04). That date is the first snapshot that has the newest of
the minimums, cardx 0.3.4 (on CRAN on 2026-07-06, in the snapshot of 2026-07-07, not yet in that of 2026-07-06; cards 0.8.0, the
next newest, is from 2026-05-28). rtfreporter and tflspec are installed from
their `next`, where their minimums are what `next` has. *The recommended
environment* is the latest R with the latest packages: the release jobs of
the same workflow. When a minimum is raised to a version released later,
the date moves to the first snapshot that has it.

**Required and optional.** cards, cardx, ggplot2, rtfreporter and tflspec
are in `Imports`: the app and the programs it writes cannot work without
them. A package only a template writes (survival, ggsurvfit, patchwork), or
only one kind of input file needs (haven, arrow), stays in `Suggests`.

**The policy for a dependency that raises its own version.** When a
dependency raises its version and tflplanner follows without breaking
anyone who has the older one, that is a minor version of ours; when the
follow cannot be backward compatible, it is a major version.

The rule for what sets a minimum: a use in `R/`, in a test that always runs,
or in the code the SPECs write does. These do not: a use only in a test
that skips when the version is older (`skip_if_not_installed(pkg,
version)`), and a catalogue entry the user may pick (a function of a later
version is listed, not called).

| Package | Minimum | What needs it (function or argument -> version) | Source |
|---|---|---|---|
| R (`Depends`) | 4.3 | R 4.3: rtfreporter (a hard dependency) needs R >= 4.3 (rtfreporter #1044). Tried 4.2 first: cardx 0.3.1 depends on R (>= 4.2) and the minimum environment installed with it, until rtfreporter `next` raised its own floor to 4.3 and made 4.2 impossible. cardx 0.3.1 (below) depends on R (>= 4.2). What tflplanner itself uses needs less: the native pipe and `\(x)` -> 4.1.0; cards 0.8.0 needs R (>= 4.1). | cardx 0.3.1 `DESCRIPTION`; R NEWS 4.1.0 |
| stats, tools, utils (`Imports`) | none | They ship with R, so their version is R's. | -- |
| shiny | 1.11.0 | `updateOn = "blur"` of `textInput()`, `textAreaInput()` and `numericInput()` (the editor's fields: `R/srv_add_report.R`, `R/srv_study.R`, `R/app_ui.R`, `R/srv_figure_page.R`) -> 1.11.0 (it was 1.8.1, for the launcher's `stop_on_close`, NEWS #256); `moduleServer()`, `testServer()` -> 1.5.0; `onStop()` -> 1.0.4; `debounce()` -> 1.0.0. | shiny NEWS 1.11.0 ("`textInput()`, `textAreaInput()`, `numericInput()` ... all gain an `updateOn` option"); `NAMESPACE` of each release |
| bslib | 0.6.0 | `navset_underline()`, `navset_card_underline()` -> 0.6.0; `tooltip()`, `input_switch()` -> 0.5.1; `accordion()`, `sidebar()`, `layout_sidebar()`, `breakpoints()`, `nav_panel()`, `navset_pill_list()` -> 0.5.0. shiny 1.11.0 needs bslib (>= 0.6.0) too. | bslib NEWS 0.6.0 ("new `navset_underline()` & `navset_card_underline()`"); shiny 1.11.0 `DESCRIPTION` |
| DT | 0.27 | `DTOutput(fill = )` (the tables fill their card) -> 0.27; `datatable(fillContainer = )` -> 0.18; `dataTableProxy()`, `selectRows()` -> 0.2; `formatStyle()`, `styleEqual()` -> 0.1. | DT release notes 0.27 ("`dataTableOutput()` gains a new `fill` parameter") |
| htmltools | 0.5.7 | `tagQuery()` -> 0.5.2; `tagAppendChild()`, `attachDependencies()`, `findDependencies()` -> 0.2.4. bslib 0.6.0 needs htmltools (>= 0.5.7). | htmltools NEWS 0.5.2; bslib 0.6.0 `DESCRIPTION` |
| jsonlite | 2.0.0 | `read_json()`, `write_json()` -> 1.2; `base64_enc()` -> 1.0; `toJSON(auto_unbox = , null = , na = )`, `fromJSON(simplifyVector = )` -> before 1.0. Raised to tflspec's minimum, 2.0.0: tflplanner shows and runs what tflspec writes, whose JSON (`toJSON(pretty = TRUE)`) is formatted differently by 1.x. | `NAMESPACE` of each release; tflspec `dependency-minimums.md` |
| later | 1.0.0 | `later()` -> 0.3. shiny 1.11.0 needs later (>= 1.0.0), and the minimum environment installs shiny at its minimum. | shiny 1.11.0 `DESCRIPTION` |
| openxlsx | 4.2.5 | `writeData(keepNA = )`, `getSheetNames()`, `int2col()` -> 3.0.0; `freezePane(firstRow = )`, `saveWorkbook(overwrite = )`, `loadWorkbook()` -> 2.x or earlier. Raised to tflspec's minimum, 4.2.5 (`write.xlsx(overwrite = TRUE)` default) (`type = "list"` drop-downs -> 4.0.17, `write.xlsx()` on R >= 4.2 -> 4.2.4), since the workbooks tflplanner reads and writes are tflspec's. | `NAMESPACE` of each release; tflspec `dependency-minimums.md` |
| processx | 3.8.2 | `run(stderr_to_stdout = )` and `process$new(stderr = "2>&1")` (a program's output with its errors, `R/ard_programs.R`, `R/batch.R`) -> 3.3.0; `run(timeout = , wd = , echo = , error_on_status = )` -> 2.0.0 or earlier. Raised to 3.8.2, which testthat 3.2.1 needs (a test mocks `processx::process`); before that to 3.6.1, which callr (needed by testthat and by rcmdcheck) needs: in the minimum environment, with processx 3.3.0, they could not be installed (CI run of the minimum environment, "Conflicts with processx@3.3.0"). | processx NEWS 3.3.0; callr 3.7.6 `DESCRIPTION` (processx >= 3.6.1) |
| ps | 1.7.5 | `ps_handle()`, `ps_create_time()`, `ps_kill()` -> 1.0.0. processx 3.3.0 needs ps (>= 1.2.0) and testthat 3.2.1 needs ps (>= 1.7.5). | processx 3.3.0 and testthat 3.2.1 `DESCRIPTION`; `NAMESPACE` of each release |
| readxl | 1.2.0 | `read_excel(.name_repair = )` -> 1.2.0, with a current tibble (1.0.0 calls `tibble::as_tibble(validate = )`, which tibble has made defunct; rtfreporter's minimum is the same); `read_excel(range = , n_max = , trim_ws = )`, `cell_rows()` -> 1.0.0; `excel_sheets()` -> 0.1.0. | readxl NEWS 1.2.0; rtfreporter `dependency-minimums.md` |
| rhandsontable | 0.3.4 | The custom parameter `planner_key` passed through `rhandsontable(...)` and read back in the grid's events -> 0.3.4 ("the ability to pass custom parameters to the table"); `selectCallback = `, `hot_col(allowInvalid = , readOnly = )`, `hot_context_menu(allowRowEdit = , allowColEdit = )` -> 0.3.3 or earlier. | rhandsontable NEWS 0.3.4; `R/rhandsontable.R` of 0.3.3 and 0.3.4 |
| sortable | 0.5.0 | `rank_list(orientation = )` (a horizontal list) -> 0.5.0; `bucket_list()`, `add_rank_list()` -> 0.4.2, the first CRAN release. | sortable NEWS 0.5.0 ("switch the orientation of `rank_list()` items to horizontal") |
| writexl | 1.1 | `write_xlsx()` -> 0.1; a list of data frames as sheets -> 0.2; `format_headers =` (the ARS workbook, `tflspec`'s `ars_xlsx.R`) -> 1.1. In the first minimum-environment run 0.1 failed with "is.data.frame(x) is not TRUE" and "unused argument (format_headers = TRUE)", in the examples and 40 tests. | writexl `NEWS` and `R/write_xlsx.R` of each release |
| yaml | 2.3.8 | `verbatim_logical()` (`true` / `false` written unquoted in a study file, `R/study.R`, `R/spec_source.R`) -> 2.3.8; `read_yaml()`, `write_yaml()` -> 2.1.15; `as.yaml(handlers = )` -> 2.2.0. tflspec's minimum (2.3.5: the `quoted` attribute 2.2.3, libyaml 0.2.5 2.2.4, `as.yaml()`'s libyaml changes 2.3.5) is below it. | yaml NEWS 2.3.8 ("Added `verbatim_logical` helper function"); tflspec `dependency-minimums.md` |
| zip | 2.1.0 | `zip(root = )` (a study folder zipped relative to itself) -> 2.1.0; `zipr()` -> 2.0.0. | zip NEWS 2.1.0 ("zip functions now have a `root` argument") |
| cards | 0.8.0 | `compare_ard()`, `is_ard_equal()` (`compare_imported_ard()`) -> 0.8.0; `ard_tabulate_rows()` (the catalogue of the ARD functions and the code the templates write) -> 0.8.0; `ard_tabulate()`, `ard_summary()`, `ard_tabulate_value()`, `ard_mvsummary()` and the `ADLB` data -> 0.7.0; the others -> 0.1.0 to 0.6.1. Not counted: `diff_ard_hierarchical()` -> 0.9.0, which the catalogue lists for the user to pick. | cards NEWS 0.7.0, 0.8.0; `NAMESPACE` of each release |
| cardx | 0.3.4 | `ard_stats_mantelhaen_test()` passing its arguments to `stats::mantelhaen.test()` by name -> 0.3.4 (before it the catalogue's `correct = FALSE` setting failed with "'arg' must be NULL or a character vector", test-ard-fn-search.R:417); `ard_emmeans_emmeans()`, `ard_emmeans_contrast()` (the templates' model rows) -> 0.3.1; `ard_tabulate_max()`, `ard_tabulate_abnormal()` -> 0.3.0; `ard_incidence_rate()` -> 0.2.5; `ard_stats_*()`, `ard_regression()`, `ard_regression_basic()` -> 0.2.0 or earlier. Raised from 0.3.1 in the first minimum-environment run; tflspec keeps 0.3.1 (its tests do not run that function). | cardx NEWS 0.3.1; `NAMESPACE` of each release |
| ggplot2 | 3.5.2 | `coord_radial()` (the sunburst and polar figures tflspec writes) -> 3.5.0; `standardise_aes_names()`, `get_element_tree()` -> 3.3.0; the plain `ggplot()`, `aes()`, `geom_*()` the app writes -> every release. The same minimum as tflspec's `Imports` (3.5.2, the 3.5 release its workflow tests; patchwork at the snapshot does not load with 3.5.0). | ggplot2 NEWS 3.5.0, 3.5.2; tflspec `dependency-minimums.md` |
| rtfreporter | 0.8.2.9100 | What its `next` has: tflplanner and rtfreporter are released together, so the minimum is the version of rtfreporter's `next` when this table was made. In the minimum environment it is installed from `next` (`Remotes`). | rtfreporter `next`, `DESCRIPTION` |
| tflspec | 0.0.24.9238 | What its `next` has, in the same way. | tflspec `next`, `DESCRIPTION` |
| testthat | 3.2.1 | `local_mocked_bindings()` mocking an object that is not a function (`processx::process`, an R6 generator, in test-update-gaps.R) -> 3.2.1 (3.1.7 stops with "All elements of `...` must be functions"); `local_mocked_bindings()` (103 uses in the tests) -> 3.1.7; `expect_no_error()`, `expect_no_warning()`, `expect_no_message()` -> 3.1.5; `expect_no_match()` -> 3.0.3; the third edition and `Config/testthat/parallel` -> 3.0.0. | testthat NEWS 3.1.7, 3.1.5, 3.0.3, 3.0.0 |
| withr | 2.5.0 | `local_tempdir()` -> 2.3.0; `local_tempfile()` -> 2.1.0; `local_options()`, `local_envvar()`, `defer()` -> 2.0.0. ggplot2 3.5.2 needs withr (>= 2.5.0). | `NAMESPACE` of each release; ggplot2 3.5.2 `DESCRIPTION` |
| broom, broom.helpers | 1.0.8, 1.17.0 | `cardx::ard_regression()` (T-14-2-2's hazard ratios) calls them, and the sample study stops without them (`R/sample.R`); cardx asks for broom (>= 1.0.8). Unchanged. | cardx `DESCRIPTION`; `R/sample.R` |
| dplyr | none | `case_when()`, `if_else()` (in the code the templates write) -> 0.5.0. The minimum cards (0.8.0) needs dplyr (>= 1.1.4), which is what is installed. | cards 0.8.0 `DESCRIPTION` |
| arrow, haven | none | `arrow::read_parquet()` -> 0.14.1; `haven::read_xpt()`, `read_sas()` -> 1.1.0 and 0.1.0. Each reads one kind of input file (the app says "Reading .x files needs the x package" when it is missing); an old arrow cannot be built from source without its system library, so both come from the snapshot. | `NAMESPACE` of each release |
| survival, patchwork, httpuv, logrx, xml2, tcltk | none | Only the code the templates write (survival, patchwork), the tests that skip without them (httpuv, xml2, logrx) and the folder chooser (tcltk, which ships with R) use them; each comes from the snapshot. | -- |
| ggsurvfit | none, 1.1.0 in the minimum environment | The Kaplan-Meier template's figure (`ggsurvfit()`, `survfit2()`) -> 0.1.0. ggsurvfit 1.2.0 needs ggplot2 >= 4.0.0, so the minimum environment installs 1.1.0 (the last that works with ggplot2 3.5.x). | ggsurvfit `DESCRIPTION` |
| chromote | none, left out of the minimum environment | chromote (the tests that drive a browser) is suggested; the newest at the snapshot needs later >= 1.1.0, more than the minimum of a direct dependency, so it cannot be installed with the minimums, and the tests that need it skip. | chromote `DESCRIPTION` |

## Changes

The minimums before this table was made, and why each changed: R 4.1 ->
4.3 (rtfreporter #1044; 4.2 first, for cardx 0.3.1); shiny 1.8.1 -> 1.11.0 (`updateOn`); testthat 3.0.0 ->
3.1.7 (`local_mocked_bindings()`); cards, cardx and ggplot2 moved from
`Suggests` to `Imports`, with the minimums above (ggplot2 3.5.2, as tflspec's); jsonlite and openxlsx are tflspec's minimums; every other `Imports`
entry had no minimum and now has the one in the table; withr (none ->
2.5.0) follows ggplot2 3.5.2.
