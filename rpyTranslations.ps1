##
$cur_dir = Get-Location
$p_args = [Collections.Generic.List[string]]::new()
$dbg = $false
if ($Args[0] -ieq '-Help' -or $Args[0] -in $('-h','--help')) {
	Write-Host @"
$($MyInvocation.InvocationName) (populate|fix-empty|check|diff|reorder|--help) [args] [options]
In addition, for the '--help' command and for each options begining by '--', it is possible to use them as case
 insansitive by replace the '--' prefix to a single '-'.
"@
	$p_args.add("--help")
} else {
	$p_args.add("$($Args[0].toLower())")
}
for (($i = 1); $i -lt $Args.Count; ($i++)) {
	if (Test-Path -PathType leaf -Path "$($Args[$i])") {
		$p_args.add("$($Args[$i] | Resolve-Path)")
	} elseif ("$($Args[$i])" -match '^-[a-zA-Z]+[a-zA-Z-]+$') {
		$p_args.add("-$($Args[$i].toLower())")
		if ("$($Args[$i])" -ieq "-Debug") {
			$dbg = $true
		}
	} else {
		$p_args.add("$($Args[$i])")
		if ($Args[$i] -in $('-d','--debug')) {
			$dbg = $true
		}
	}
}
Set-Location -Path "<path_to_rpyTranslations_dir>"#ex: $HOME\Documents\Python3
$prevOutputEncoding = [Console]::OutputEncoding
$prevInputEncoding = [Console]::InputEncoding
$pyEncoding = $env:PYTHONIOENCODING
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::InputEncoding = [System.Text.Encoding]::UTF8
$env:PYTHONIOENCODING = 'UTF-8'
if ($dbg) {
	"DEBUG: PythonEncoding=$env:PYTHONIOENCODING `$OutputEncoding=$($OutputEncoding.BodyName) OutputEncoding={0} InputEncoding={1}" -f [Console]::OutputEncoding.BodyName, [Console]::InputEncoding.BodyName
	"DEBUG::Call: python3 -m rpyTranslations $($p_args -join ' ')"
}
python3 -m rpyTranslations $p_args |Write-Host
[Console]::OutputEncoding = $prevOutputEncoding
[Console]::InputEncoding = $prevInputEncoding
$env:PYTHONIOENCODING = $pyEncoding
if ($dbg) {
	"DEBUG: PythonEncoding=$env:PYTHONIOENCODING `$OutputEncoding=$($OutputEncoding.BodyName) OutputEncoding={0} InputEncoding={1}" -f [Console]::OutputEncoding.BodyName, [Console]::InputEncoding.BodyName
}
Set-Location -Path "$($cur_dir)"
