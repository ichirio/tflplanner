# The computer's own dialogs on Windows, in front of the browser: a folder
# or file dialog of .NET's Windows Forms, its owner a top-most window, run
# by PowerShell (-STA).  utils::choose.dir() / choose.files() can open
# behind the browser, where nobody sees them while the app waits.  NULL
# when PowerShell cannot show it (then utils's dialog is tried); "" when
# the person cancelled.

# A PowerShell string literal (single quotes doubled)
.ps_lit <- function(x) paste0("'", gsub("'", "''", as.character(x %||% ""), fixed = TRUE), "'")

# The script for a dialog: "dir", or "file" (`multi`: several), opening in
# `start`; the paths chosen written one a line (UTF-8)
.ps_dialog_script <- function(kind = c("dir", "file"), start = "", title = "", multi = FALSE,
                              filter = "Excel workbook (*.xlsx;*.xlsm)|*.xlsx;*.xlsm|All files (*.*)|*.*") {
  kind <- match.arg(kind)
  start <- as.character(start %||% "")
  folder <- if (nzchar(start) && dir.exists(start)) start else if (nzchar(start)) dirname(start) else ""
  folder <- if (nzchar(folder) && dir.exists(folder)) normalizePath(folder, "\\", mustWork = FALSE) else ""
  head <- c("[Console]::OutputEncoding = [System.Text.Encoding]::UTF8",
            "Add-Type -AssemblyName System.Windows.Forms",
            "$owner = New-Object System.Windows.Forms.Form",
            "$owner.TopMost = $true; $owner.ShowInTaskbar = $false; $owner.Opacity = 0",
            "$owner.StartPosition = 'CenterScreen'; $owner.Show(); $owner.Activate()")
  body <- if (kind == "dir") c(
    "$d = New-Object System.Windows.Forms.FolderBrowserDialog",
    paste0("$d.Description = ", .ps_lit(title)),
    "$d.ShowNewFolderButton = $false",
    if (nzchar(folder)) paste0("$d.SelectedPath = ", .ps_lit(folder)),
    "if ($d.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) { [Console]::Out.Write($d.SelectedPath) }")
  else c(
    "$d = New-Object System.Windows.Forms.OpenFileDialog",
    paste0("$d.Title = ", .ps_lit(title)),
    paste0("$d.Filter = ", .ps_lit(filter)),
    paste0("$d.Multiselect = $", if (isTRUE(multi)) "true" else "false"),
    if (nzchar(folder)) paste0("$d.InitialDirectory = ", .ps_lit(folder)),
    "if ($d.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) { [Console]::Out.Write(($d.FileNames -join \"`n\")) }")
  c(head, body, "$owner.Close()")
}

# Run the dialog: the paths chosen ("/" separated; character() when
# cancelled), or NULL when PowerShell could not show it
.ps_dialog <- function(kind, start = "", title = "", multi = FALSE,
                       filter = "Excel workbook (*.xlsx;*.xlsm)|*.xlsx;*.xlsm|All files (*.*)|*.*") {
  ps <- Sys.which("powershell")
  if (!nzchar(ps)) return(NULL)
  f <- tempfile("tfl-dialog-", fileext = ".ps1")
  on.exit(unlink(f), add = TRUE)
  # (UTF-8 with its mark: Windows PowerShell 5 reads a script so)
  con <- file(f, open = "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(enc2utf8(paste(.ps_dialog_script(kind, start, title, multi, filter), collapse = "\r\n"))), con)
  close(con)
  out <- tryCatch(suppressWarnings(system2(ps, c("-NoProfile", "-STA", "-ExecutionPolicy", "Bypass", "-File",
                                                 shQuote(f)), stdout = TRUE, stderr = FALSE)),
                  error = function(e) NULL)
  status <- attr(out, "status")
  if (is.null(out) || (!is.null(status) && status != 0L)) return(NULL)
  out <- enc2native(iconv(out, "UTF-8", "UTF-8"))
  out <- trimws(out[nzchar(trimws(out))])
  normalizePath(out, "/", mustWork = FALSE)
}
