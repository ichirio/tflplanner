' The tflplanner launcher for Windows, run by its shortcuts through
' wscript.exe (tflplanner::add_shortcut() writes this file, as UTF-16).
'
' Finds the newest R at every start -- the registry entry the R installer
' writes, else the newest R-x.y.z folder -- so an R update does not break the
' shortcut, and runs launch.R with Rscript.exe without a console window.
' Arguments (--update, --port=N) are passed on to launch.R.
Option Explicit
Dim sh, fso, here, rscript, args, i
Set sh = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")
here = fso.GetParentFolderName(WScript.ScriptFullName)

args = ""
For i = 0 To WScript.Arguments.Count - 1
  args = args & " " & Q(WScript.Arguments(i))
Next

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

Function FromRegistry(root)
  Dim p
  FromRegistry = ""
  On Error Resume Next
  p = sh.RegRead(root & "\SOFTWARE\R-core\R\InstallPath")
  If Err.Number = 0 Then
    If fso.FileExists(p & "\bin\Rscript.exe") Then FromRegistry = p & "\bin\Rscript.exe"
  End If
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

Function FromFolders()
  Dim roots, r, d, best, bestv, v
  FromFolders = "" : best = "" : bestv = "0"
  roots = Array(sh.ExpandEnvironmentStrings("%ProgramFiles%\R"), _
                sh.ExpandEnvironmentStrings("%LOCALAPPDATA%\Programs\R"))
  For Each r In roots
    If fso.FolderExists(r) Then
      For Each d In fso.GetFolder(r).SubFolders
        If LCase(Left(d.Name, 2)) = "r-" And fso.FileExists(d.Path & "\bin\Rscript.exe") Then
          v = Mid(d.Name, 3)
          If Newer(v, bestv) Then bestv = v : best = d.Path & "\bin\Rscript.exe"
        End If
      Next
    End If
  Next
  FromFolders = best
End Function

Function FindRscript()
  FindRscript = FromRegistry("HKCU")
  If FindRscript = "" Then FindRscript = FromRegistry("HKLM")
  If FindRscript = "" Then FindRscript = FromFolders()
End Function
