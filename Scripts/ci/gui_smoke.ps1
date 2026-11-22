# Smoke test of the Windows GUI build: starts Deskew GUI, waits for the main window,
# saves screenshots of the main window and of the Advanced Options dialog (with one
# tooltip shown) as PNG files and kills the GUI again (no settings are saved).
# Fails when the GUI ends, or the main window or the dialog are not shown in time.
#
# Usage: pwsh -File Scripts\ci\gui_smoke.ps1 [-OutDir <folder>] [-Exe <path to deskewgui.exe>]
param(
  [string]$OutDir = (Join-Path $PSScriptRoot '..\..\Bin\gui-smoke'),
  [string]$Exe = (Join-Path $PSScriptRoot '..\..\Bin\deskewgui.exe'),
  [int]$TimeoutSeconds = 30
)

$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.Drawing
Add-Type @'
using System;
using System.Text;
using System.Runtime.InteropServices;

public static class Win {
  public delegate bool EnumProc(IntPtr hwnd, IntPtr lparam);
  [StructLayout(LayoutKind.Sequential)] public struct RECT { public int Left, Top, Right, Bottom; }

  [DllImport("user32.dll")] public static extern bool EnumWindows(EnumProc f, IntPtr l);
  [DllImport("user32.dll")] public static extern bool EnumChildWindows(IntPtr parent, EnumProc f, IntPtr l);
  [DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);
  [DllImport("user32.dll")] public static extern bool IsWindowVisible(IntPtr h);
  [DllImport("user32.dll")] public static extern bool GetWindowRect(IntPtr h, out RECT r);
  [DllImport("user32.dll")] public static extern bool PostMessage(IntPtr h, uint msg, IntPtr w, IntPtr l);
  [DllImport("user32.dll")] public static extern bool SetForegroundWindow(IntPtr h);
  [DllImport("user32.dll")] public static extern bool SetCursorPos(int x, int y);

  [DllImport("user32.dll", CharSet = CharSet.Unicode)] public static extern IntPtr SendMessage(IntPtr h, uint msg, IntPtr w, StringBuilder l);

  // WM_GETTEXT works for controls of other processes too, GetWindowText does not
  public static string Text(IntPtr h) { var sb = new StringBuilder(512); SendMessage(h, 0x000D, (IntPtr)512, sb); return sb.ToString(); }

  // Top level visible window of the process with title starting with the given text
  public static IntPtr FindTop(uint pid, string title) {
    IntPtr found = IntPtr.Zero;
    EnumWindows((h, l) => {
      uint p; GetWindowThreadProcessId(h, out p);
      RECT r; GetWindowRect(h, out r);
      if (p == pid && IsWindowVisible(h) && r.Right - r.Left > 100 && Text(h).StartsWith(title)) { found = h; return false; }
      return true;
    }, IntPtr.Zero);
    return found;
  }

  // The smallest visible child window at given position relative to the parent window.
  // LCL buttons with glyph support (TBitBtn) have no window text to search for.
  public static IntPtr FindChildAt(IntPtr parent, int x, int y) {
    RECT pr; GetWindowRect(parent, out pr);
    int sx = pr.Left + x, sy = pr.Top + y;
    IntPtr found = IntPtr.Zero; long best = long.MaxValue;
    EnumChildWindows(parent, (h, l) => {
      RECT r; GetWindowRect(h, out r);
      long area = (long)(r.Right - r.Left) * (r.Bottom - r.Top);
      if (IsWindowVisible(h) && sx >= r.Left && sx < r.Right && sy >= r.Top && sy < r.Bottom && area < best) { best = area; found = h; }
      return true;
    }, IntPtr.Zero);
    return found;
  }

  public static IntPtr FindChild(IntPtr parent, string text) {
    IntPtr found = IntPtr.Zero;
    EnumChildWindows(parent, (h, l) => { if (Text(h) == text) { found = h; return false; } return true; }, IntPtr.Zero);
    return found;
  }
}
'@

function Save-WindowShot([IntPtr]$hwnd, [string]$name, [int]$extraBottom = 0) {
  $r = New-Object Win+RECT
  [void][Win]::GetWindowRect($hwnd, [ref]$r)
  $w = $r.Right - $r.Left; $h = $r.Bottom - $r.Top + $extraBottom
  $bmp = New-Object System.Drawing.Bitmap $w, $h
  $g = [System.Drawing.Graphics]::FromImage($bmp)
  $g.CopyFromScreen($r.Left, $r.Top, 0, 0, $bmp.Size)
  $path = Join-Path $OutDir $name
  $bmp.Save($path, [System.Drawing.Imaging.ImageFormat]::Png)
  $g.Dispose(); $bmp.Dispose()
  "  $name ${w}x${h}"
}

# Waits for a top level window of the GUI, fails when the GUI ends or the time is up
function Wait-Window($process, [string]$title) {
  $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
  while ((Get-Date) -lt $deadline) {
    if ($process.HasExited) { throw "GUI ended with exit code $($process.ExitCode) before showing '$title'" }
    $hwnd = [Win]::FindTop($process.Id, $title)
    if ($hwnd -ne [IntPtr]::Zero) { return $hwnd }
    Start-Sleep -Milliseconds 500
  }
  throw "'$title' window not shown within $TimeoutSeconds s"
}

New-Item -ItemType Directory -Force $OutDir | Out-Null
"Starting $Exe"
$p = Start-Process -FilePath $Exe -PassThru
try {
  $main = Wait-Window $p 'Deskew GUI v'
  [void][Win]::SetForegroundWindow($main)
  Start-Sleep -Seconds 1
  Save-WindowShot $main 'main.png'

  # BM_CLICK is posted so the modal dialog does not block this script
  # "Advanced..." button is in the lower right corner, above the big Deskew button
  $mr = New-Object Win+RECT
  [void][Win]::GetWindowRect($main, [ref]$mr)
  $btn = [Win]::FindChildAt($main, ($mr.Right - $mr.Left) - 68, ($mr.Bottom - $mr.Top) - 107)
  if ($btn -eq [IntPtr]::Zero) { throw 'Advanced button not found' }
  [void][Win]::PostMessage($btn, 0x00F5, [IntPtr]::Zero, [IntPtr]::Zero)

  $adv = Wait-Window $p 'Advanced Options'
  [void][Win]::SetForegroundWindow($adv)
  Start-Sleep -Seconds 1
  Save-WindowShot $adv 'advanced.png'

  # Hover over a check box to get its tooltip
  $cb = [Win]::FindChild($adv, 'Force rotation')
  if ($cb -ne [IntPtr]::Zero) {
    $r = New-Object Win+RECT
    [void][Win]::GetWindowRect($cb, [ref]$r)
    [void][Win]::SetCursorPos($r.Left + 20, $r.Top + 8)
    Start-Sleep -Seconds 2
    Save-WindowShot $adv 'advanced-hint.png' 120
  }
  '  OK, main window and Advanced Options shown'
}
finally {
  if (-not $p.HasExited) { Stop-Process -Id $p.Id -Force }
}
