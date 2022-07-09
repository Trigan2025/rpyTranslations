#!powershell
using namespace System.Collections.Generic
$prevOutputEncoding = [Console]::OutputEncoding
$prevInputEncoding = [Console]::InputEncoding
$pyEncoding = $env:PYTHONIOENCODING
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::InputEncoding = [System.Text.Encoding]::UTF8
$env:PYTHONIOENCODING = 'UTF-8'

$cur_dir = Get-Location
$i = 0
$path = "<path_to_rpyTranslations_dir>"#ex: $HOME\Documents\Python3
New-Item -ItemType Directory -Path '.\temp-rpyStats'
New-Item -ItemType Directory -Path '.\temp-rpyStats\uni', '.\temp-rpyStats\rpy'
$uni = $Args[$i]; $rpy = $Args[$i + 1]; $unis = [List[string]]::new(); $rpys = [List[string]]::new()
if ("$($Args[$i + 2])" -eq '%') {
   $withF = $true
   for (($i += 3); $i -lt $Args.Count; ($i++)) {
      $f = $Args[$i]
      $unis.add($f); $rpys.add($f)
   }
} else { $withF = $false; }
$files = [List[string]]::new()
if (!$withF) {
   foreach ($f in Get-ChildItem -Path ".\$($uni)\*.rpy") {
      $_f = Split-Path -Path "$($f)" -Leaf
      $unis.add($(Split-Path -Path "$($f)" -LeafBase))
      New-Item -ItemType HardLink -Path "$("./temp-rpyStats/uni/$_f")" -Target "$($f)"
      $files.add("$("./temp-rpyStats/uni/$_f" |Resolve-Path)")
   }
} else {
   foreach ($f in $unis) {
      $_f = "$($f).rpy"
      New-Item -ItemType HardLink -Path "$("./temp-rpyStats/uni/$_f")" -Target "$(".\$uni\$_f" |Resolve-Path)"
      $files.add("$("./temp-rpyStats/uni/$_f" |Resolve-Path)")
   }
}
Set-Location -Path "$($path)"
python3 -m rpyTranslations check --not-translate -- $files |Write-Host
Set-Location -Path "$($cur_dir)"
$files.clear()
if (!$withF) {
   foreach ($f in Get-ChildItem -Path ".\$($rpy)\*.rpy") {
      $_f = Split-Path -Path "$($f)" -Leaf
      $rpys.add($(Split-Path -Path "$($f)" -LeafBase))
      New-Item -ItemType HardLink -Path "$("./temp-rpyStats/rpy/$_f")" -Target "$($f)"
      $files.add("$("./temp-rpyStats/rpy/$_f" |Resolve-Path)")
   }
} else {
   foreach ($f in $rpys) {
      $_f = "$($f).rpy"
      New-Item -ItemType HardLink -Path "$("./temp-rpyStats/rpy/$_f")" -Target "$(".\$rpy\$_f" |Resolve-Path)"
      $files.add("$("./temp-rpyStats/rpy/$_f" |Resolve-Path)")
   }
}
Set-Location -Path "$($path)"
python3 -m rpyTranslations check --not-translate -- $files |Write-Host
Set-Location -Path "$($cur_dir)"
foreach ($uF in $unis) {
   $uFile = "./temp-rpyStats/uni/$($uF).check-info"
   $uS = (Get-Content "$($uFile)" | Select-String -Pattern "^Not translated:" -List -Raw | Select-String -Pattern "[0-9]+$").Matches.Value
   if ($uF -in $rpys) {
      $rFile = "./temp-rpyStats/rpy/$($uF).check-info"
      $rS = (Get-Content "$($rFile)" | Select-String -Pattern "^Not translated:" -List -Raw | Select-String -Pattern "[0-9]+$").Matches.Value
      $rS = $uS - $rS
      @"
Total: $($uS)
Translated: $($rS)
Percentage: $([int][Math]::Floor(($rS * 100) / $uS))
"@ >".\$($uF).rpy.stat"
   } else {
      @"
Total: $($uS)
Translated: 0
Percentage: 0
"@ >".\$($uF).rpy.stat"
   }
}
Remove-Item -Recurse -Path ./temp-rpyStats

[Console]::OutputEncoding = $prevOutputEncoding
[Console]::InputEncoding = $prevInputEncoding
$env:PYTHONIOENCODING = $pyEncoding
