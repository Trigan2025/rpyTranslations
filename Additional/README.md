## Additional scripts

The scripts placed here are intened to give additional specific fonctionalities to work around Ren'Py translations files.

### rpyTr

| Filename  | Language   |
|:----------|:-----------|
| rpyTr     | Bash       |
| rpyTr.ps1 | Powershell |

A script to get statistics between uni and translated files. It will give the total strings that need to be translate, the total strings that are already translate, and the percentage of that.

**Note:** Since this script count empty translations to know how many translations are made, all empty strings need to be present for this to work.

Usage:
```shell
./rpyTr uniDir trDir [% names...]
```

The `%` option allow to indicate in which files to work on rather than on all it found.

Exemple:
```shell
./rpyTr uni . % init screens
```

*Please note that it can take some time since the main script (rpyTranslations) is not very optimised currently (should come with the mentionned rewriting).*
