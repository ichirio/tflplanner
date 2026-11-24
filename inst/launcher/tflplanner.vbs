' The tflplanner launcher for Windows, run by its shortcuts through
' wscript.exe (tflplanner::add_shortcut() writes this file, as UTF-16).
'
' It is made for one R x.y, which launcher.conf (beside this file) records:
'   r_home=<the R it was made in>   r_xy=<its x.y, "4.6">
' It looks for R at every start, in this order, and never falls back to
' another x.y -- every R it picks has its x.y checked, the remembered one
' too (the rule is .pick_r() in tflplanner's R/launcher.R; the tests run one
' table of cases through it, this script and tflplanner.sh):
'   1. the remembered R, when its bin\Rscript.exe is there and is this x.y;
'   2. the newest R-x.y.* of this x.y under Program Files\R and
'      %LOCALAPPDATA%\Programs\R, and beside the remembered R (its parent
'      folder); the first found among equal versions;
'   3. the first Rscript on PATH (every entry, in order) of this x.y;
'   4. else it says that R x.y is not found, and stops.
' An Rscript's x.y is read from its path (R-4.6.1\bin) where the path says,
' and only else asked (Rscript --version, through cmd.exe).
' It runs launch.R with Rscript.exe without a console window.
' Arguments (--update, --port=N) are passed on to launch.R.
' --which: print the Rscript it would use and start nothing (for the tests;
' TFLPLANNER_R_ROOTS, folders separated by ";", replaces the two usual roots).
Option Explicit
Dim sh, fso, here, rscript, args, i, xy, which, rhome, full
Set sh = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")
here = fso.GetParentFolderName(WScript.ScriptFullName)

args = ""
which = False
For i = 0 To WScript.Arguments.Count - 1
  If WScript.Arguments(i) = "--which" Then
    which = True
  Else
    args = args & " " & Q(WScript.Arguments(i))
  End If
Next

xy = ConfValue("r_xy")
rhome = ConfValue("r_home")
rscript = FindRscript()
If which Then
  If rscript = "" Then WScript.Quit 1
  WScript.Echo rscript
  WScript.Quit 0
End If
If rscript = "" Then
  MsgBox "{{NO_R}}", 16, "tflplanner"
  WScript.Quit 1
End If
' 0 = no window; False = do not wait
sh.Run Q(rscript) & " " & Q(here & "\launch.R") & args, 0, False

Function Q(s)
  Q = Chr(34) & s & Chr(34)
End Function

' A value of launcher.conf (UTF-16, key=value lines); "" when not there
Function ConfValue(key)
  Dim f, ts, ln, p
  ConfValue = ""
  f = here & "\launcher.conf"
  If Not fso.FileExists(f) Then Exit Function
  On Error Resume Next
  Set ts = fso.OpenTextFile(f, 1, False, -1)
  If Err.Number <> 0 Then
    Err.Clear
    On Error GoTo 0
    Exit Function
  End If
  Do Until ts.AtEndOfStream
    ln = ts.ReadLine
    p = InStr(ln, "=")
    If p > 1 Then
      If LCase(Trim(Left(ln, p - 1))) = key Then ConfValue = Trim(Mid(ln, p + 1))
    End If
  Loop
  ts.Close
  Err.Clear
  On Error GoTo 0
End Function

' "4.10.1" > "4.9.3": compare numerically, part by part
Function Part(parts, k)
  Part = 0
  If k <= UBound(parts) Then
    If IsNumeric(parts(k)) Then Part = CLng(parts(k))
  End If
End Function

Function Newer(a, b)
  Dim x, y, k, n
  x = Split(a, ".")
  y = Split(b, ".")
  Newer = False
  n = UBound(x)
  If UBound(y) > n Then n = UBound(y)
  For k = 0 To n
    If Part(x, k) > Part(y, k) Then
      Newer = True
      Exit Function
    End If
    If Part(x, k) < Part(y, k) Then Exit Function
  Next
End Function

' The x.y a folder's name says ("R-4.6.1" -> "4.6"), "" when it says none;
' `full` is its whole version ("4.6.1")
Function FolderXY(name)
  Dim re, m
  FolderXY = ""
  full = ""
  Set re = New RegExp
  re.IgnoreCase = True
  re.Pattern = "^R-(\d+)\.(\d+)((\.\d+)*)"
  If re.Test(name) Then
    Set m = re.Execute(name)(0)
    FolderXY = m.SubMatches(0) & "." & m.SubMatches(1)
    full = m.SubMatches(0) & "." & m.SubMatches(1) & m.SubMatches(2)
  End If
End Function

' the x.y of an Rscript.exe: from its path (...\R-4.6.1\bin\Rscript.exe)
' where the path says, else from what it says of its version
Function XYOf(exe)
  Dim home, tmp, ts, txt, re, m
  XYOf = FolderXY(fso.GetFileName(fso.GetParentFolderName(fso.GetParentFolderName(exe))))
  If XYOf <> "" Then Exit Function
  tmp = sh.ExpandEnvironmentStrings("%TEMP%") & "\tflplanner-" & fso.GetTempName
  On Error Resume Next
  sh.Run "cmd /c " & Chr(34) & Q(exe) & " --version > " & Q(tmp) & " 2>&1" & Chr(34), 0, True
  If fso.FileExists(tmp) Then
    Set ts = fso.OpenTextFile(tmp, 1)
    txt = ts.ReadAll
    ts.Close
    fso.DeleteFile tmp, True
    Set re = New RegExp
    re.Pattern = "version (\d+)\.(\d+)"
    If re.Test(txt) Then
      Set m = re.Execute(txt)(0)
      XYOf = m.SubMatches(0) & "." & m.SubMatches(1)
    End If
  End If
  Err.Clear
  On Error GoTo 0
End Function

' 1. the remembered R, of this x.y
Function FromRemembered()
  FromRemembered = ""
  If rhome <> "" Then
    If fso.FileExists(rhome & "\bin\Rscript.exe") Then
      If XYOf(rhome & "\bin\Rscript.exe") = xy Then FromRemembered = rhome & "\bin\Rscript.exe"
    End If
  End If
End Function

' 2. the newest R-x.y.* of this x.y in the roots
Function FromFolders()
  Dim roots, list, r, d, best, bestv, v
  FromFolders = "" : best = "" : bestv = ""
  If Environment_("TFLPLANNER_R_ROOTS") <> "" Then
    list = Environment_("TFLPLANNER_R_ROOTS")
  Else
    list = sh.ExpandEnvironmentStrings("%ProgramFiles%\R") & ";" & _
           sh.ExpandEnvironmentStrings("%LOCALAPPDATA%\Programs\R")
  End If
  ' beside the remembered R: R-x.y.* in its parent folder
  If rhome <> "" Then list = list & ";" & fso.GetParentFolderName(rhome)
  roots = Split(list, ";")
  For Each r In roots
    If r <> "" Then
      If fso.FolderExists(r) Then
        For Each d In fso.GetFolder(r).SubFolders
          If FolderXY(d.Name) = xy And fso.FileExists(d.Path & "\bin\Rscript.exe") Then
            v = full
            If best = "" Then
              bestv = v : best = d.Path & "\bin\Rscript.exe"
            ElseIf Newer(v, bestv) Then
              bestv = v : best = d.Path & "\bin\Rscript.exe"
            End If
          End If
        Next
      End If
    End If
  Next
  FromFolders = best
End Function

Function Environment_(name)
  Environment_ = sh.Environment("Process")(name)
End Function

' 3. the first Rscript on PATH (every entry, in order) of this x.y
Function FromPath()
  Dim dirs, k, p, exe
  FromPath = ""
  dirs = Split(sh.Environment("Process")("PATH"), ";")
  For k = 0 To UBound(dirs)
    p = Trim(Replace(dirs(k), Chr(34), ""))
    If p <> "" Then
      exe = p & "\Rscript.exe"
      If fso.FileExists(exe) Then
        If XYOf(exe) = xy Then
          FromPath = exe
          Exit Function
        End If
      End If
    End If
  Next
End Function

Function FindRscript()
  FindRscript = ""
  If xy = "" Then Exit Function
  FindRscript = FromRemembered()
  If FindRscript = "" Then FindRscript = FromFolders()
  If FindRscript = "" Then FindRscript = FromPath()
End Function
