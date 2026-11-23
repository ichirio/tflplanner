' The tflplanner launcher for Windows, run by its shortcuts through
' wscript.exe (tflplanner::add_shortcut() writes this file, as UTF-16).
'
' It is made for one R x.y, which launcher.conf (beside this file) records:
'   r_home=<the R it was made in>   r_xy=<its x.y, "4.6">
' It looks for R at every start, in this order, and never falls back to
' another x.y (the rule is .pick_r() in tflplanner's R/launcher.R):
'   1. the remembered R, when its bin\Rscript.exe is there;
'   2. the newest R-x.y.* under Program Files\R and %LOCALAPPDATA%\Programs\R
'      (this covers the folder the registry's InstallPath names, which is one
'      of these);
'   3. Rscript on PATH, only when its x.y matches;
'   4. else it says that R x.y is not found, and stops.
' It runs launch.R with Rscript.exe without a console window.
' Arguments (--update, --port=N) are passed on to launch.R.
Option Explicit
Dim sh, fso, here, rscript, args, i, xy
Set sh = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")
here = fso.GetParentFolderName(WScript.ScriptFullName)

args = ""
For i = 0 To WScript.Arguments.Count - 1
  args = args & " " & Q(WScript.Arguments(i))
Next

xy = ConfValue("r_xy")
rscript = FindRscript()
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

' 1. the remembered R
Function FromRemembered()
  Dim h
  FromRemembered = ""
  h = ConfValue("r_home")
  If h <> "" Then
    If fso.FileExists(h & "\bin\Rscript.exe") Then FromRemembered = h & "\bin\Rscript.exe"
  End If
End Function

' 2. the newest R-x.y.* of this x.y
Function FromFolders()
  Dim roots, r, d, best, bestv, v, re, m
  FromFolders = "" : best = "" : bestv = ""
  Set re = New RegExp
  re.IgnoreCase = True
  re.Pattern = "^R-(\d+)\.(\d+)((\.\d+)*)"
  roots = Array(sh.ExpandEnvironmentStrings("%ProgramFiles%\R"), _
                sh.ExpandEnvironmentStrings("%LOCALAPPDATA%\Programs\R"))
  For Each r In roots
    If fso.FolderExists(r) Then
      For Each d In fso.GetFolder(r).SubFolders
        If re.Test(d.Name) And fso.FileExists(d.Path & "\bin\Rscript.exe") Then
          Set m = re.Execute(d.Name)(0)
          If m.SubMatches(0) & "." & m.SubMatches(1) = xy Then
            v = m.SubMatches(0) & "." & m.SubMatches(1) & m.SubMatches(2)
            If best = "" Then
              bestv = v : best = d.Path & "\bin\Rscript.exe"
            ElseIf Newer(v, bestv) Then
              bestv = v : best = d.Path & "\bin\Rscript.exe"
            End If
          End If
        End If
      Next
    End If
  Next
  FromFolders = best
End Function

' the x.y of an Rscript.exe ("4.6"), from what it says of its version
Function VersionXY(exe)
  Dim tmp, ts, txt, re, m
  VersionXY = ""
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
      VersionXY = m.SubMatches(0) & "." & m.SubMatches(1)
    End If
  End If
  Err.Clear
  On Error GoTo 0
End Function

' 3. Rscript on PATH, only when its x.y matches
Function FromPath()
  Dim dirs, k, p, exe
  FromPath = ""
  dirs = Split(sh.ExpandEnvironmentStrings("%PATH%"), ";")
  For k = 0 To UBound(dirs)
    p = Trim(Replace(dirs(k), Chr(34), ""))
    If p <> "" Then
      exe = p & "\Rscript.exe"
      If fso.FileExists(exe) Then
        If VersionXY(exe) = xy Then
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
