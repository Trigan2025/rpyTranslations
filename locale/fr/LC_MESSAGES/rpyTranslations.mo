��    )      d  ;   �      �  �  �  7  �  �  �    �  I  �  h  ,  �  �2     7     7     57  5   H7  4   ~7  =   �7  D   �7  M   68     �8     �8  /   �8     �8  s   �8  1   V9  1   �9  }   �9     8:     A:  �  a:    �>  ;  bA    �C    �E  3   �G  .   �G     H  '   >H  %   fH     �H  B   �H  :   �H  <   "I  &   _I    �I  y
  �J  
  U  *  )[  �  Tb    !h  �  -z  :  ǁ     �  +   	�     5�  B   T�  5   ��  >   ͇  E   �  N   R�     ��     ��  7   Ɉ     �  �   �  9   ��  9   �  �   $�     ��  (   Ȋ  K  �  �  =�  �  �  o  ��  h  #�  :   ��  D   ǚ  $   �  1   1�  *   c�     ��  K   ��  G   ��  J   ?�  ,   ��                      %                     )      "                 
                              (                     !       	                       $                       '   &         #                 		python3 -m rpyTranslations ({cmds}) [args] [options]

		Please note that the parentesis are just to englob all of the possible command and should not be put.

		-h, --help
		  Show this general help and exit.
		  Enter a command followed the help one to see their specific help.
		  Please note that help commands obviousely doesn’t accept any args or options even if they still
		   show the requested help.
		args
		  Their are specific for each command and generaly should be put in a specific order.
		  See the specific help of these commands for more information.
		options
		  They generaly can be put everywhere in the argument realm of command.
		  However, though they can be put in random order, they are treated by order of precedence and each
		   option can be specified only one times.
		  The precedence for an option is in right to left order (so, short version of options are always last).
		  Exemple, for `-% n, --multi n, --lists n`, the precedence is:
		      `--lists n` > `--multi n` > `-% n`
		  So if we have this: `-% 2 --multi 5 --lists 3`, only `--list 3` will be treated and retained and the
		   remaining will stay as is.
		  Some commands have inter-option precedences. This means that each options will be treated but only
		   the one with the greater precedence will be retained (see the specific helps for more details).

		  Though commands has their own set of options, here are some general options below:
		  --
		      This option allow to indicate where to stop interpreting options.
		      Exemple, in the following: `-d -- -v`, -d will activates the debug but -v will activates nothing
		       and stay as -v.
		  -:
		      This option should be directly followed (without spaces) by any characters.
		      These characters should each correspond to short version of options that do not requiered
		       arguments.
		      Exemple: `-:vd` correponding to `-v -d`.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process.
		      Please note that the output can be huge.
		  See the specific help of commands to see the other options available.
		 		rpyTranslations check [--help] for_files... [options]

		for_files are the files where to check the translations.

		options:
		  -h, --help
		      Show this specific help for the 'check' command and exit.
		  -n, --skip-empty
		      Deactivate checkings for empty translations.
		  -e, --empty
		      Default. Check how many translation is still empty.
		  -p, --not-translate
		      Check how many translation is still empty or are set with the 'pass' keyword.
		  --where
		      Output or not of where the empty translations are.
		      Please note that this option had no effect if put with the --skip-empty.
		  -f, --format
		      Sometimes leading white-spaces can be typos but generaly they are on purpose (especialy when an
		       'extend' is in use).
		      Check if the translations have keep all kinds of formating and that leading white-spaces are
		       respected.
		      * Formatings like brackets ([]) and braces ({}).
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		* Please note that --skip-empty take precedence over --not-translate that take precedence over --empty.
		 		rpyTranslations diff [--help] for_files... [options]

		for_files and from_files are successions of files and both need to be of equal length.
		for_files are the files where differences of translation need to be compared.
		from_files are the reference files from where other translations is get for comparing.

		Since this command is for comparing adds and removing in the for-files, there is no proxy option, and
		 obviousely, empty translation and the 'pass' keyword are treat as different.

		options:
		  -h, --help
		      Show this specific help for the 'diff' command and exit.
		  --ignore-newpart
		      Indicates if the comparisons of new-part of translations should be output or not.
		  --what
		      Indicates if what is add or is no longer present in the translation files should be output or not.
		  --reflines
		      Indicates if the comparisons of reference-lines should be output or not.
		  --tr-id
		      Indicates if the comparisons of translation-identifiers should be output or not.
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where other translations are get. This include the
		       first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		 		rpyTranslations fix-empty [--help] for_files... [options]

		for_files are the files where the empty translations need to be fix.

		options:
		  -h, --help
		      Show this specific help for the 'fix-empty' command and exit.
		  -a act, --action act
		      This option allow to specify which action take when empty strings are found.
		      act should be one of the following:
		          P   Default. All empty 'dialogs' translation are replaced by the 'pass' keyword and the
		               empty 'strings' translation are removed.
		          C   All empty 'dialogs' translation are replaced by the 'pass' keyword and the empty
		               'strings' translation are commented.
		          R   All empty translation are removed. Please note that this also remove 'dialogs'
		               translation set with the 'pass' keyword.
		  -o dir, --subdir dir
		      If put, it indicates a sub-directory to where the fixed translation files are saved, otherwise
		       on the for-files themself.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		 		rpyTranslations populate [--help] for_files... from_files... [options]

		for_files and from_files are successions of files and both need to be of equal length.
		for_files are the reference files where the translations need to be populate.
		from_files are the reference files from where the translations is get.

		options:
		  -h, --help
		      Show this specific help for the 'populate' command and exit.
		  -b, --bunch
		      Indicates if, when a translation is not found, it should be searched for in other files in the
		       current from-files group.
		  --bulk [n]
		      Indicates if few files should be populate from many (or many from few).
		      If n is puts, it indicate the number of file in the for_files group (default 1).
		  -o dir, --subdir dir
		      If put, it indicates a sub-directory to where the populated translation files are saved,
		       otherwise on the for-files themself.
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where merging multiple translations. This include
		       the first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
		      It defaults to 2 unless --bulk is use, in that case it defaults to 1 because this latter control the
		       first list of files.
		      The internal workflow is like:
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * Where from_files[#] are group of files of the same length as forFiles.
		  -a, --ask
		      If put, a prompt will ask to confirm or no a replacement of translation (only for non-empty).
		      It’s recommand to use the verbose option with that, so you can know for what file the asking is.
		  -F, --force, --overwrite
		      Activate the replacement of existing translations.
		  --proxy lvl
		      If a translation is not found with the normals methods, so this option allow to use other methods
		       based on the proxymity level (lvl).
		      lvl can be one of the following:
		          0   Default. No proxy maked.
		          1   For 'dialogs', allow to focus only in the name part of the dialog identifier.
		          2   For 'dialogs', allow to fully ignore the dialog identifier and as fallback to search in
		               'strings' translations, but not allow for 'strings' to search in 'dialogs' translations.
		          3   Allow to focus only on the strings. With that, 'dialogs' can now be considered as same
		               as 'strings', so 'strings' can now search in 'dialogs' translations.
		          4   The strings are compared to get a least a correspondance of 90%.
		              Please note that this can be a very long process.
		  -W, --no-proxy-warn
		      When attempting to populate empty translation with proxied translation, an asking is made.
		      This option allow to bypass this asking.
		  --proxy-y lvl
		      This option is the same as --proxy but with effect of --no-proxy-warn in addition.
		  -N, --from-nID
		      By default, the identifiant name’ string are takes from for_files, use this option if rather
		       prefere it’s to be from from_files.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		* Please note that --ask take precedence over --force.
		* Please note that --proxy take precedence over --proxy-y (though the side effect of this one is kept)
		   and that --no-proxy-warn had no effect if --proxy-y is use too.

		Please note that `populate --bulk 2 ./common.rpy ./init.rpy -% 1 ../from/common.rpy ../from/init.rpy`
		 result to the same as `populate --bunch ./common.rpy ./init.rpy ../from/common.rpy ../from/init.rpy`
		 but are less efficiency.
		 		rpyTranslations reorder [--help] for_files... [options]

		for_files are the reference files where the translations need to be reordered.

		This command try regrouping similiar translations to ease translations and comprisons of equivalant
		 translations.
		Since 'strings' translations are generaly unique, it should be note that it’s only work for 'dialogs'.
		Also, all occurences are regrouping to the first occurence place.

		options:
		  -h, --help
		      Show this specific help for the 'reorder' command and exit.
		  -o dir, --subdir dir
		      If put, it indicates a sub-directory to where the reordered translation files are saved,
		       otherwise on the for-files themself.
		  -r, --reverse
		      If put, try to reordering as it was originaly (as extracted by Ren'Py) through the
		       reference-line comments (# dir/file.rpy:line).
		      This functionality ignoring translation that are not have a reference-line comment, those
		       selected to be reordered are joined and can be inserted between two group of these
		       ignored translations.
		  --proxy
		      If put, this option make translations to be evaluated with a proxy method.
		      With this method, translations are regrouped function of their normalized alpha-numeric
		       representation.
		      Please note that this option had no effect if put with the --reverse option.
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process.
		 
		Additional informations:

		- About the --ask option:
		    The patern to show askings is the following:
		      `
		      For: [for: old dID]
		      	<for: old string>
		      Attempt to replace: [for: new dID]
		      	<for: new string>
		      With: [with: dID]
		      	<with: string>
		      `
		    The presence of dID parts depends of the translation’ kind (string or dialog translation).
		    Only dialog translations can contain a dID, and if one doesn’t have one, it will be showed as {0}.
		    It’s important to note that the dID contains two parts, the first part is a NID (name identifiant)
		     and the second is image arguments.
		    As mentioned above, only the NID can be affect by the --from-nID option, this means that first, the
		     dID is get in the following order of precedence:
		      [for: new dID] > [for: old dID]
		     and then, if the --from-nID option is use, the NID can be take from the [with: dID] if:
		      1) the previously get dID is not {0} and it NID is of string type
		      2) this [with: dID] is not {0} and it NID is of string type
		     otherwise, the NID from the previously get dID is keep.
		 
  not All files need to be different An error occured:
 Both list of files need to have their files different For:
	{old}
Attempt to replace:
	{new}
With:
	{From} For:
	{old}
Attempt to replace:
	{new}
With: {N_From}
	{From} For: {N_old}
	{old}
Attempt to replace: {N_new}
	{new}
With:
	{From} For: {N_old}
	{old}
Attempt to replace: {N_new}
	{new}
With: {N_From}
	{From} Invalide argument: Missing argument: Please note that the 'With' was found by proxy. Proceed? The file pair lists need to have equal length, {N} gived. The file pair lists need to have equal length, {N} gived. The length of both list of files need to be equal The length of each list of files need to be equal This function try to estimate the proxymity between two strings.
Please note that this can hardly be precise and error-free.
 Warning: doen’t exist or is not a file forFiles: List - The list of files that need to be populated.
FromFiles: List,... - Successions of list of files from where to get the translations.
bunch: Booleen - Indicates if, when a translation is not found, it should be searched for in other files
       in the current fromFiles group.
bulk: Booleen - Indicates if few files should be populate from many (or many from few).
proxy: Integer - From 0 to 4 (includes) to indicate a level of proxy, 0 to diactivate, 4 made it long.
proxyWarn: Booleen - Indicates whether an asking is made or not when attempting to populate empty
           translation with proxied translation.
fromNID: Booleen - Indicates whether the identifiant name’ string is prefered to be from 'fromFiles' or from
         'forFiles'.
overwrite: Char - Should be: 'N' for no, 'A' for ask, or 'F' for force. (verbose is recommand with 'A')
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where differences of translation need to be compared.
FromFiles: List,... - Successions of list of files from where to get other translations.
newpart: Booleen - Output or not comparisons of new-part of translations.
what: Booleen - Output or not of what is add or is no longer present in the translation files.
reflines: Booleen - Output or not comparisons of reference-lines.
trID: Booleen - Output or not comparisons of translation-identifiers.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where empty translations need to be fixed.
action: Char - Should be: 'P' to change empty translation to the 'pass' keyword, 'C' to also comment
        'strings' translations instead of deleting them, or 'R' to remove all empty or passed translation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where translations need to be checked.
untranslated: Integer - Should be: 1 to check empty, 2 to also check passed, or 0 to deactivate.
where: Booleen - Output or not of where the empty translations are.
formats: Booleen - Check or not translations formatting.
         Like leading white-spaces, brackets ([]) and braces ({}).
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where translations need to be reordered.
reverse: Booleen - Reordering as it was originaly (as extracted by Ren'Py) or process normaly.
proxy: Booleen - Active or not the regroupement in function of their normalized alpha-numeric
       representation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicates the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging.
 help commands do not accept any argument or option. the 1st argument need to one of the following: {} option required an argument. {} option should be an absolute integer {} option should be an integer, give: {} parameter should be  {} parameter should be 1 for empty, 2 for pass, or 0 to deactivate {} parameter should be N for no, A for ask, or F for force {} parameter should be an integer between 0 and 4 (includes) {} parameter should be of booleen type MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Project-Id-Version: 1.0
PO-Revision-Date: 2022-06-28 02:55+0200
Last-Translator: TRIGAN <trigan2025@outlook.com>
Language-Team: none
Language: fr
Plural-Forms: nplurals=2; plural=n <= 1 ? 0 : 1;
 		python3 -m rpyTranslations ({cmds}) [args] [options]

		Veuillez noter que les parentèses sont juste pour englober toutes les commandes possibles et ne doivent pas
		 être mises.

		-h, --help
		  Afficher cette aide générale et quitter.
		  Entrez une commande suivie de celle d’aide pour afficher leur aide spécifique.
		  Veuillez noter que les commandes d’aide n’acceptent évidemment pas d’arguments ou d’options, même si elles
		   affichent toujours l’aide demandée.
		args
		  Ils sont spécifiques à chaque commande et doivent généralement être placés dans un ordre spécifique.
		  Consultez l’aide spécifique de ces commandes pour plus d’informations.
		options
		  Elles peuvent généralement être placées partout dans le domaine des arguments de commande.
		  Cependant, bien qu’elles puissent être mises dans un ordre aléatoire, elles sont traitées par ordre de
		   priorité et chaque option ne peut être spécifiée qu’une seule fois.
		  L’ordre de priorité pour une option est de droite à gauche (la version courte des options est donc
		   toujours la dernière).
		  Exemple, pour `-% n, --multi n, --lists n`, la priorité est:
		      `--lists n` > `--multi n` > `-% n`
		  Donc, si nous avons ceci: `-% 2 --multi 5 --lists 3`, seul `--list 3` sera traité et conservé et le reste
		   restera tel quel.
		  Certaines commandes ont des priorités inter-options. Cela signifie que chacune des options sera
		   traitée, mais que seule celle ayant la priorité la plus grande sera conservée (voir les aides
		   spécifiques pour plus de détails).

		  Bien que les commandes aient leur propre jeu d’options, voici quelques options générales:
		  --
		      Cette option permet d’indiquer où arrêter l’interprétation des options.
		      Exemple, dans ce qui suit : `-d -- -v`, -d activera le débogage mais -v n’activera rien et restera
		       en tant que -v.
		  -:
		      Cette option doit être directement suivie (sans espaces) par des caractères.
		      Chacun de ces caractères devrait correspondre à la version courte des options qui ne nécessitent pas
		       d’arguments.
		      Exemple: `-:vd` correpond à `-v -d`.
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus.
		      Veuillez noter que la sortie peut être énorme.
		  Consultez l’aide spécifique des commandes pour voir les autres options disponibles.
		 		rpyTranslations check [--help] for_files... [options]

		for_files sont les fichiers où vérifier les traductions.

		options:
		  -h, --help
		      Affiche cette aide spécifique pour la commande 'check' et quitte.
		  -n, --skip-empty
		      Désactive les vérifications pour les traductions vides.
		  -e, --empty
		      Par défaut. Vérifie combien de traductions sont encore vides.
		  -p, --not-translate
		     Vérifie combien de traductions sont encore vides ou définies avec le mot clé 'pass'.
		  --where
		      Sortie ou non de où se trouvent les traductions vides.
		      Veuillez noter que cette option n’a aucun effet si elle est mise avec l’option --skip-empty.
		  -f, --format
		      Parfois, les espaces de début/fin peuvent être des fautes de frappe, mais généralement ils sont
		       intentionnels (surtout lorsqu’un 'extended' est utilisé).
		      Vérifie que les traductions ont conservé toutes formes de formatage et que les espaces de début/fin
		       sont respectés.
		      * Formatages tels que crochets ([]) et accolades ({}).
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus. Veuillez noter que la sortie peut être énorme.
		* Veuillez noter que --skip-empty a priorité sur --not-translate qui a priorité sur --empty.
		 		rpyTranslations diff [--help] for_files... [options]

		for_files et from_files sont des successions de fichiers dont la longueur doit être égale.
		for_files sont les fichiers dans lesquels les différences de traduction doivent être comparées.
		from_files sont les fichiers de référence à partir desquels les autres traductions sont obtenues pour la
		 comparaison.

		Comme cette commande est destinée à comparer les ajouts et suppressions dans les for-files, il n’y a pas
		 d’option proxy, et évidemment, la traduction vide et le mot clé 'pass' sont traités comme différents.

		options:
		  -h, --help
		      Affiche cette aide spécifique pour la commande 'diff' et quitte.
		  --ignore-newpart
		      Indique si les comparaisons de partie 'new' des traductions doivent être sur la sortie ou non.
		  --what
		      Indique si ce qui est ajouté ou qui n’est plus présent dans les fichiers de traduction doit être en
		       sortie ou non.
		  --reflines
		      Indique si les comparaisons des lignes de référence doivent être sur la sortie ou non.
		  --tr-id
		      Indique si les comparaisons des identifiants de translation doivent être sur la sortie ou non.
		  -% n, --multi n, --lists n
		      Ceci permet d’indiquer le nombre de listes à partir desquelles les autres traductions sont obtenues.
		       Cela inclut le premier groupe de fichiers (le for_files).
		      Donc, pour 2 for_files, vous devez le mettre avec 3.
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus. Veuillez noter que la sortie peut être énorme.
		 		rpyTranslations fix-empty [--help] for_files... [options]

		for_files sont les fichiers où les traductions vides doivent être corrigées.

		options:
		  -h, --help
		      Affiche cette aide spécifique pour la commande 'fix-empty' et quitte.
		  -a act, --action act
		      Cette option permet de spécifier l’action à effectuer lorsque des chaînes vides sont trouvées.
		      act doit être l’un des suivants :
		          P   Par défaut. Toutes les traductions 'dialogues' vides sont remplacées par le mot clé 'pass' et
		               la traduction 'strings' vide est supprimée.
		          C   Toutes les traductions 'dialogues' vides sont remplacées par le mot clé 'pass' et la
		               traduction 'strings' vide est commentée.
		          R   Toutes les traductions vides sont supprimées. Veuillez noter que cela supprime également la
		               traduction 'dialogs' définie avec le mot clé 'pass'.
		  -o dir, --subdir dir
		      Si mis, il indique un sous-répertoire où les fichiers de traduction corrigées sont enregistrés,
		       sinon sur les for-files eux-mêmes.
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus. Veuillez noter que la sortie peut être énorme.
		 		rpyTranslations populate [--help] for_files... from_files... [options]

		for_files et from_files sont des successions de fichiers dont la longueur doit être égale.
		for_files sont les fichiers de référence dans lesquels les traductions doivent être peuplées.
		from_files sont les fichiers de référence à partir desquels les traductions sont obtenues.

		options:
		  -h, --help
		      Affiche cette aide spécifique pour la commande 'populate' et quitte.
		  -b, --bunch
		      Indique si, lorsqu’une traduction n’est pas trouvée, elle doit être recherchée dans les autres
		       fichiers du groupe fromFiles actif.
		  --bulk [n]
		      Indique si quelques fichiers doivent être remplis à partir de plusieurs (ou plusieurs à partir de
		       quelque-uns).
		      Si n est mis, il indique le nombre de fichiers dans le groupe for_files (1 par défaut).
		  -o dir, --subdir dir
		      Si mis, il indique un sous-répertoire où les fichiers de traduction remplis sont enregistrés, sinon
		       sur les for-files eux-mêmes.
		  -% n, --multi n, --lists n
		      Cela permet d’indiquer le nombre de listes à partir desquelles fusionner plusieurs traductions. Cela
		       inclut le premier groupe de fichiers (le for_files).
		      Donc, pour 2 from_files, vous devez le mettre avec 3.
		      Il prend par défaut la valeur 2 sauf si --bulk est utilisé, dans ce cas, il prend la valeur 1 par
		       défaut car ce dernier contrôle la première liste de fichiers.
		      Le flux de travail interne est comme :
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * Où from_files[#] est un groupe de fichiers de même longueur que forFiles.
		  -a, --ask
		      Si mis, une invite demandera de confirmer ou non un remplacement de traduction (uniquement pour les
		       non-vides).
		      Il est recommandé d’utiliser l’option verbose avec ceci, afin que vous puissiez savoir pour quel
		       fichier la demande est.
		  -F, --force, --overwrite
		      Active le remplacement des traductions existantes.
		  --proxy lvl
		      Si une traduction n’est pas trouvée avec les méthodes normales, cette option permet d’utiliser
		       d’autres méthodes basées sur le niveau de proximité (lvl).
		      lvl peut être un des suivants :
		          0   Par défaut. Aucun proxy de fait.
		          1   Pour 'dialogs', autorise à se concentrer uniquement sur la partie nom de l’identificateur de
		               dialogue.
		          2   Pour 'dialogs', permet d’ignorer complètement l’identifiant de dialogue et en dernier recours
		               autorise la recherche dans les traductions 'strings', mais pas pour 'strings' dans les
		               traductions 'dialogs'.
		          3   Permet de se concentrer uniquement sur les chaines. Avec cela, 'dialogs' peut maintenant être
		               considéré comme identique à 'strings', de sorte que 'strings' peut maintenant rechercher
		               dans les traductions 'dialogs'.
		          4   Les chaines sont comparées pour obtenir au moins une correspondance de 90%.
		              Veuillez noter que ce processus peut être très long.
		  -W, --no-proxy-warn
		      Lors d’une tentative de remplissage d’une traduction vide avec une traduction par proxy, une demande
		       est effectuée.
		      Cette option permet d’outrepasser cette demande.
		  --proxy-y lvl
		      Cette option est identique à --proxy mais avec l’effet de --no-proxy-warn en plus.
		  -N, --from-nID
		      Par défaut, la chaine du nom de l’identifiant est prise à partir de for_files, utilisez cette option
		       si vous préférez qu’elle soit prise à partir de from_files.
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus. Veuillez noter que la sortie peut être énorme.
		* Notez que --ask a priorité sur --force.
		* Veuillez noter que --proxy a priorité sur --proxy-y (bien que l’effet secondaire de celle-ci soit
		   conservée) et que --no-proxy-warn n’a aucun effet si --proxy-y est également utilisé.

		Veuillez noter que `populate --bulk 2 ./common.rpy ./init.rpy -% 1 ../from/common.rpy ../from/init.rpy`
		 résulte au même que `populate --bunch ./common.rpy ./init.rpy ../from/common.rpy ../from/init.rpy`
		 mais est moins efficace.
		 		rpyTranslations reorder [--help] for_files... [options]

		for_files sont les fichiers de référence où les traductions doivent être réorganisées.

		Cette commande tente de regrouper des traductions similaires pour faciliter les traductions et les
		 comparaisons de traductions équivalentes.
		Puisque les traductions 'strings' sont généralement uniques, il convient de noter que ça ne fonctionnent
		 que pour les 'dialogues'.
		En outre, toutes les occurrences sont regroupées au premier emplacement d’occurrence.

		options:
		  -h, --help
		      Affiche cette aide spécifique pour la commande 'reorder' et quitte.
		  -o dir, --subdir dir
		      Si mis, il indique un sous-répertoire où les fichiers de traduction réorganisés sont enregistrés,
		       sinon sur les for-files eux-mêmes.
		  -r, --reverse
		      Si mis, essaie de réorganiser comme c’était initialement (tel qu’extrait par Ren'Py) à l’aide des
		       commentaires de référence de ligne (# dir/file.rpy:line).
		      Cette fonctionnalité ignore les traductions qui n’ont pas de commentaire de ligne de référence,
		       celles sélectionnées pour être réorganisées sont jointes et peuvent être insérées entre deux groupes
		       de ces traductions ignorées.
		  --proxy
		      Si mis, cette option fait que les traductions soit évaluées avec une méthode proxy.
		      Avec cette méthode, les traductions sont regroupées en fonction de leur représentation
		       alphanumérique normalisée.
		      Veuillez noter que cette option n’a aucun effet si elle est utilisée avec l’option --reverse.
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus.
		 
		Informations supplémentaires:

		- À propos de l’option --ask:
		    Le modèle utilisé pour afficher les demandes est le suivant :
		      `
		      Pour: [pour: ancien dID]
		      	<pour: ancienne chaine>
		      Tente de remplacer: [pour: nouveau dID]
		      	<pour: nouvelle chaine>
		      Avec: [avec: dID]
		      	<avec: chaine>
		      `
		    La présence de parties dID dépend du type de traduction (traduction de chaine ou de dialogue).
		    Seules les traductions de dialogue peuvent contenir un dID, et si l’une d’elles n’en a pas, elle sera
		     affichée comme {0}.
		    Il est important de noter que le dID contient deux parties, la première partie est un NID (nom
		     identifiant) et la seconde est des arguments d’image.
		    Comme mentionné plus haut, seul le NID peut être affecté par l’option --from-nID, cela signifie que,
		     d’abord, le dID est obtenu dans l’ordre de priorité suivant :
		      [pour: nouveau dID] > [pour: ancien dID]
		     et qu’ensuite, si l’option --from-nID est utilisée, le NID peut être pris de [avec: dID] si :
		      1) le dID obtenu précédemment n’est pas {0} et son NID est de type chaine
		      2) ce [avec: dID] n’est pas {0} et que son NID est de type chaine
		     sinon, le NID du dID obtenu précédemment est conservé.
		 
  pas Tous les fichiers doivent être différents Une erreur s’est produite :
 Les deux listes de fichiers doivent avoir des fichiers différents Pour:
	{old}
Tente de remplacer:
	{new}
Avec:
	{From} Pour:
	{old}
Tente de remplacer:
	{new}
Avec: {N_From}
	{From} Pour: {N_old}
	{old}
Tente de remplacer: {N_new}
	{new}
Avec:
	{From} Pour: {N_old}
	{old}
Tente de remplacer: {N_new}
	{new}
Avec: {N_From}
	{From} Argument invalide : Argument manquant : Veuillez noter que le 'Avec' a été trouvé par proxy. Procéder ? Les listes de paires de fichiers doivent avoir une longueur égale, {N} donnée. Les listes de paires de fichiers doivent avoir une longueur égale, {N} données. La longueur des deux listes de fichiers doit être égale La longueur de chaque liste de fichiers doit être égale Cette fonction essaie d’estimer la proximité entre deux chaines.
Veuillez noter que ceci peut difficilement être précis et exempt d’erreur.
 Avertissement : n’existe pas ou n’est pas un fichier forFiles: Liste - Liste des fichiers qui doivent être peuplés.
FromFiles: Liste,... - Successions de liste de fichiers d’où sont obtenu les traductions.
bunch: Booleen - Indique si, lorsqu’une traduction n’est pas trouvée, elle doit être recherchée dans les
       autres fichiers du groupe fromFiles actif.
bulk: Booléen - Indique si quelques fichiers doivent être remplis à partir de plusieurs (ou plusieurs à
       partir de quelque-uns).
proxy: Entier - De 0 à 4 (inclus) pour indiquer un niveau de proxy, 0 pour le désactiver, 4 pour rendre la
       tâche longue.
proxyWarn: Booléen - Indique si une demande est effectuée ou non lors d’une tentative de remplissage d’une
           traduction vide avec une traduction proxy.
fromNID: Booleen - Indique si la chaine du nom de l’identifiant est préférée être à partir de 'fromFiles'
         ou de 'forFiles'.
overwrite: Caractère - Devrait être : 'N' pour non, 'A' pour demande, ou 'F' pour force. (la verbosité est
           recommandé avec 'A')
outdir: Chaîne - Un chemin relatif aux chemins 'forFiles' pour indiquer où enregistrer le résultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
debug: Booléen - Active ou non le débogage. La quantité sur cette fonction peut être énorme.
 forFiles: Liste - Liste des fichiers dans lesquels les différences de traduction doivent être comparées.
FromFiles: Liste,... - Successions de liste de fichiers d’où obtenir les autres traductions.
newpart: Booléen - Sortie ou non des comparaisons de partie 'new' des traductions.what: Booléen - Sortie ou non de ce qui est ajouté ou n’est plus présent dans les fichiers de traduction.
reflines: Booléen - Sortie ou non des comparaisons de ligne de référence.trID: Booléen - Sortie ou non des comparaisons d’identifiant de traduction.verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
debug: Booléen - Active ou non le débogage. La quantité sur cette fonction peut être énorme.
 forFiles: Liste - Liste des fichiers pour lesquels des traductions vides doivent être corrigées.
action: Caractère - Devrait être : 'P' pour remplacer la traduction vide par le mot clé 'pass', 'C' pour
        commenter également les traductions 'strings' au lieu de les supprimer, ou 'R' pour supprimer
        toutes les traductions vides ou passées.
outdir: Chaîne - Un chemin relatif aux chemins 'forFiles' pour indiquer où enregistrer le résultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
debug: Booléen - Active ou non le débogage. La quantité sur cette fonction peut être énorme.
 forFiles: Liste - Liste des fichiers où les traductions doivent être vérifiées.
untranslated: Entier - Devrait être : 1 pour vérifier si vide, 2 pour vérifier également lorsque passé, ou
              0 pour désactiver.
where: Booléen - Sortie ou non de où se trouvent les traductions vides.
formats: Booléen - Vérifier ou non le formatage des traductions.
         Comme les espaces de début/fin, les crochets ([]) et les accolades ({}).
verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
debug: Booléen - Active ou non le débogage. La quantité sur cette fonction peut être énorme.
 forFiles: Liste - Liste des fichiers dans lesquels les traductions doivent être réorganisées.
reverse: Booléen - Réorganiser telle que c’était à l’origine (telle qu’extrait par Ren'Py) ou procéder
         normalement.
proxy: Booléen - Active ou non le regroupement en fonction de leur représentation alphanumérique
       normalisée.
outdir: Chaîne - Un chemin relatif aux chemins 'forFiles' pour indiquer où enregistrer le résultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
debug: Booléen - Active ou non le débogage.
 les commandes help n’acceptent aucun argument ou option. le 1er argument doit correspondre à l’un des arguments suivants : l’option {} requière un argument. l’option {} doit être un entier absolu, donné l’option {} doit être un entier, donné le paramètre {} doit être  le paramètre {} doit être 1 pour vide, 2 pour pass, ou 0 pour désactiver le paramètre {} doit être N pour non, A pour demander ou F pour force le paramètre {} doit être un nombre entier compris entre 0 et 4 (inclus) le paramètre {} doit être de type booléen 