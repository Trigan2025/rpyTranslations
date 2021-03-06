# Ren'Py Translations python helper tool

The aim of this script is to help (human) translators to work with Ren'Py Translation files (`*.rpy` in `tl∕*` sub-directory).
Use the `--help` command for a summary.

## How to use

First you need to have [python 3](https://www.python.org/) installed.

Then you can open a terminal and put for exemple this command to start:  
`python3 -m rpyTranslations --help`  
_*You need to be where the `.py` file reside.*_

It could be possible to use the python package of Ren'Py, but until Ren'Py 8, it's shipped with python 2, so if you try:  
`"<path_to>/renpy-<version>-sdk/lib/<sys>-<arch>/python" -E rpyTranslations.py --help`  
You would end up with a `SyntaxError`.

#### Possible workflow

* Open Ren'Py, create a new project name like `Translation maker`, click to open the game directory and remove all it's content appart the `tl` directory.  
  Or make this tree yourself in the Ren'Py search path (more efficient).
* Then open the game directory of the project you want to translate and copy all the `.rpy` files it contain _*(obviously, skip the `tl` directory)*_ into the game folder of `Translation maker`.
    * If all you see is `.rpa` files, so you need to you an [unarchiver](https://github.com/Lattyware/unrpa).  
      _*Open one of the `.rpa` file and check the first line to get the rpa key and use it prefixed by `0x` with the `-k` option.*_  
      _*Or more easier, run this command in linux `head -n 1 ./<file_name>.rpa`, and run this one in windows `Get-Content -Path .\<file_name>.rpa -Head 1`.*_

* Then, click the 'Generate Translations' action, indicate the name-code of the language _*(it could depends on the norm of the project)*_ and then click 'Generate Translations'.
* You can remove all the `.rpyc` files and you ready to go.

Another possiblity is to just ask the responsibles to give you these clean translation files.

##### If there is no previous translation made

You can run `reorder` (and I recommend to use the `--proxy` option with it) so similiar translations can be regrouped and comparing each to keep a consistency is eased.  
Note that since `common.rpy`, `options.rpy` and `screens.rpy` (at least) generaly does not contains 'dialogs' translations, it's useless to use it on those.

###### To summarize

It gives something like this:  
**generate new .rpy translation files**, **reorder**, **translate**, **reverse the reordering**, **check all**, eventually **fix empty**.

##### If there are previous translation made

Same as if no, but generaly you would like to run the `populate` command too (after or before is theoricaly not important).

See `populate --help` for more information on it's use.  
But globaly, it take at first argument the list of file you want to be populate (the one in the `tl/*` sub-directory of the `Translation maker` project) followed by the list of file from where to get the previous translations (copied from a previous one copied from here or from the `tl` sub-directory of the original project).

###### To summarize

It gives something like this:  
**generate new .rpy translation files**, use the 'populate' command to **poppulate the new files with the old files**, **reorder**, **translate**, **reverse the reordering**, **check all**, eventually **fix empty**.

##### Also

Since `python3 -m rpyTranslations` is run from where `rpyTranslations.py` reside, the path to the files gived need to be absolute (or relative his directory).

### On Linux

There is bash script made to ease dealing with the list of files but also just dispense the use of the `python3 -m` prefix.  
To use it, put it into `/usr/local/bin`, or into `$HOME/.local/bin` (but you need to ensure that it's in the `$PATH`). You also need to modify the `<path_to_rpyTranslations_dir>` part into it to a correct path and obviously, you also need to make it executable:  
`chmod +x ./rpyTranslations`

However, it's also possible to make inline functions to ease (shortened) the use paths.

Exemple:
```sh
tlFor() { L=$1;shift; while [ $# -gt 0 ]; do printf " %q" "$HOME/Documents/Ren'Py/Translation maker/game/tl/$L/$1.rpy"; shift; done; }
# Or if you are in the 'For' directory:
tlFor() { L=$1;shift; while [ $# -gt 0 ]; do printf " %q" "./tl/$L/$1.rpy"; shift; done; }

tl() { G=$1;ver=$2;L=$3;shift;shift;shift; while [ $# -gt 0 ]; do printf " %q" "<games_abs_path>/$G/$ver/game/tl/$L/$1.rpy"; shift; done; }

sh -c -- "rpyTranslations populate --subdir ../populated `tlFor <lang> common options` `tl <GameProject> <version> <lang> common options`"
```

### On Windows

There is powershell script made to ease dealing with the list of files but also just dispense the use of the `python3 -m` prefix.  
To use it, put it (for exemple) into `"$HOME\PythonExec"` and ensure that this path is in the `$Env:PATH`. You also need to modify the `<path_to_rpyTranslations_dir>` part into it to a correct path.

However, it's also possible to make inline functions to ease (shortened) the use paths.

Exemple:
```powershell
function tlFor() { $L="$($Args[0])"; $A = [Collections.Generic.List[string]]::new(); for (($i=1); $i -lt $Args.Count; ($i++)) { $A.add("'"+$($("$($HOME)\Documents\RenPy\Translation maker\game\tl\$($L)\{0}.rpy" -f "$($Args[$i])") -Replace "'","''")+"'") }; $A }
# Or if you are in the 'For' directory:
function tlFor() { $L="$($Args[0])"; $A = [Collections.Generic.List[string]]::new(); for (($i=1); $i -lt $Args.Count; ($i++)) { $A.add("'"+$($(".\tl\$($L)\{0}.rpy" -f "$($Args[$i])") -Replace "'","''")+"'") }; $A }

function tl() { $G="$($Args[0])";$ver="$($Args[1])";$L="$($Args[2])"; $A = [Collections.Generic.List[string]]::new(); for (($i=3); $i -lt $Args.Count; ($i++)) { $A.add("'"+$($("<games_abs_path>\$($G)\$($ver)\game\tl\$($L)\{0}.rpy" -f "$($Args[$i])") -Replace "'","''")+"'") }; $A }

Invoke-Expression "rpyTranslations populate -SubDir ..\populated $(tlFor <lang> common options) $(tl <GameProject> <version> <lang> common options)"
```

## Limitations

Her are a list of the **current** limitations of this script. It's of courses planed to suppress these as soon as possible, unfortunately, however, this requires a complete rewriting of the part concerning the analysis of lines *which is the central part on which the whole is based*.
To be more explicit, the whole way the regular expressions are used needs to be changed to have something more robust and flexible. *This can take some time.*

Here are they:

- Logical lines for one-line python statement are not fully supported. The line need to end with a backslash '\' to continue on the next line.
- Splited translations are not supported. You need to keep the translation a one statement.
- Conditional instructions are not supported. Certainly the most useful of this list *(although there are alternatives with one-line python statements)* but also the one that more require the above mentioned changes.
    Neitherless, a section as been add in the check command to output how many and where there is such conditional instructions.
