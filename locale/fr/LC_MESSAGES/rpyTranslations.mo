ή    )      d  ;   ¬        ω    7    ό  Λ    Θ  I  Ρ  h  ,    2     7     7     57  5   H7  4   ~7  =   ³7  D   ρ7  M   68     8     8  /   ©8     Ω8  s   β8  1   V9  1   9  }   Ί9     8:     A:    a:    β>  ;  bA    C    ¦E  3   »G  .   οG     H  '   >H  %   fH     H  B   €H  :   ηH  <   "I  &   _I    I  y
  €J  
  U  *  )[  Μ  Tb    !h    -z  :  Η       +   	     5  B   T  5     >   Ν  E     N   R     ‘     ΅  7   Ι       ’     9   °  9   κ     $     Έ  (   Θ  K  ρ  Ρ  =  £    o  ³  h  #  :     D   Η  $     1   1  *   c       K   «  G   χ  J   ?  ,                         %                     )      "                 
                              (                     !       	                       $                       '   &         #                 		python3 -m rpyTranslations ({cmds}) [args] [options]

		Please note that the parentesis are just to englob all of the possible command and should not be put.

		-h, --help
		  Show this general help and exit.
		  Enter a command followed the help one to see their specific help.
		  Please note that help commands obviousely doesnβt accept any args or options even if they still
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
		      Itβs recommand to use the verbose option with that, so you can know for what file the asking is.
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
		      By default, the identifiant nameβ string are takes from for_files, use this option if rather
		       prefere itβs to be from from_files.
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
		Since 'strings' translations are generaly unique, it should be note that itβs only work for 'dialogs'.
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
		    The presence of dID parts depends of the translationβ kind (string or dialog translation).
		    Only dialog translations can contain a dID, and if one doesnβt have one, it will be showed as {0}.
		    Itβs important to note that the dID contains two parts, the first part is a NID (name identifiant)
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
 Warning: doenβt exist or is not a file forFiles: List - The list of files that need to be populated.
FromFiles: List,... - Successions of list of files from where to get the translations.
bunch: Booleen - Indicates if, when a translation is not found, it should be searched for in other files
       in the current fromFiles group.
bulk: Booleen - Indicates if few files should be populate from many (or many from few).
proxy: Integer - From 0 to 4 (includes) to indicate a level of proxy, 0 to diactivate, 4 made it long.
proxyWarn: Booleen - Indicates whether an asking is made or not when attempting to populate empty
           translation with proxied translation.
fromNID: Booleen - Indicates whether the identifiant nameβ string is prefered to be from 'fromFiles' or from
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

		Veuillez noter que les parentΓ¨ses sont juste pour englober toutes les commandes possibles et ne doivent pas
		 Γͺtre mises.

		-h, --help
		  Afficher cette aide gΓ©nΓ©rale et quitter.
		  Entrez une commande suivie de celle dβaide pour afficher leur aide spΓ©cifique.
		  Veuillez noter que les commandes dβaide nβacceptent Γ©videmment pas dβarguments ou dβoptions, mΓͺme si elles
		   affichent toujours lβaide demandΓ©e.
		args
		  Ils sont spΓ©cifiques Γ  chaque commande et doivent gΓ©nΓ©ralement Γͺtre placΓ©s dans un ordre spΓ©cifique.
		  Consultez lβaide spΓ©cifique de ces commandes pour plus dβinformations.
		options
		  Elles peuvent gΓ©nΓ©ralement Γͺtre placΓ©es partout dans le domaine des arguments de commande.
		  Cependant, bien quβelles puissent Γͺtre mises dans un ordre alΓ©atoire, elles sont traitΓ©es par ordre de
		   prioritΓ© et chaque option ne peut Γͺtre spΓ©cifiΓ©e quβune seule fois.
		  Lβordre de prioritΓ© pour une option est de droite Γ  gauche (la version courte des options est donc
		   toujours la derniΓ¨re).
		  Exemple, pour `-% n, --multi n, --lists n`, la prioritΓ© est:
		      `--lists n` > `--multi n` > `-% n`
		  Donc, si nous avons ceci: `-% 2 --multi 5 --lists 3`, seul `--list 3` sera traitΓ© et conservΓ© et le reste
		   restera tel quel.
		  Certaines commandes ont des prioritΓ©s inter-options. Cela signifie que chacune des options sera
		   traitΓ©e, mais que seule celle ayant la prioritΓ© la plus grande sera conservΓ©e (voir les aides
		   spΓ©cifiques pour plus de dΓ©tails).

		  Bien que les commandes aient leur propre jeu dβoptions, voici quelques options gΓ©nΓ©rales:
		  --
		      Cette option permet dβindiquer oΓΉ arrΓͺter lβinterprΓ©tation des options.
		      Exemple, dans ce qui suit : `-d -- -v`, -d activera le dΓ©bogage mais -v nβactivera rien et restera
		       en tant que -v.
		  -:
		      Cette option doit Γͺtre directement suivie (sans espaces) par des caractΓ¨res.
		      Chacun de ces caractΓ¨res devrait correspondre Γ  la version courte des options qui ne nΓ©cessitent pas
		       dβarguments.
		      Exemple: `-:vd` correpond Γ  `-v -d`.
		  -v [lvl], --verbose [lvl]
		      Active la verbositΓ© durant le processus.
		      lvl peut Γͺtre passΓ© pour dΓ©finir le niveau de verbositΓ©, 1 pour le niveau de base (par dΓ©faut) ou 2
		       pour plus de verbositΓ©.
		  -d, --debug
		      Active les impressions de dΓ©bogage durant le processus.
		      Veuillez noter que la sortie peut Γͺtre Γ©norme.
		  Consultez lβaide spΓ©cifique des commandes pour voir les autres options disponibles.
		 		rpyTranslations check [--help] for_files... [options]

		for_files sont les fichiers oΓΉ vΓ©rifier les traductions.

		options:
		  -h, --help
		      Affiche cette aide spΓ©cifique pour la commande 'check' et quitte.
		  -n, --skip-empty
		      DΓ©sactive les vΓ©rifications pour les traductions vides.
		  -e, --empty
		      Par dΓ©faut. VΓ©rifie combien de traductions sont encore vides.
		  -p, --not-translate
		     VΓ©rifie combien de traductions sont encore vides ou dΓ©finies avec le mot clΓ© 'pass'.
		  --where
		      Sortie ou non de oΓΉ se trouvent les traductions vides.
		      Veuillez noter que cette option nβa aucun effet si elle est mise avec lβoption --skip-empty.
		  -f, --format
		      Parfois, les espaces de dΓ©but/fin peuvent Γͺtre des fautes de frappe, mais gΓ©nΓ©ralement ils sont
		       intentionnels (surtout lorsquβun 'extended' est utilisΓ©).
		      VΓ©rifie que les traductions ont conservΓ© toutes formes de formatage et que les espaces de dΓ©but/fin
		       sont respectΓ©s.
		      * Formatages tels que crochets ([]) et accolades ({}).
		  -v [lvl], --verbose [lvl]
		      Active la verbositΓ© durant le processus.
		      lvl peut Γͺtre passΓ© pour dΓ©finir le niveau de verbositΓ©, 1 pour le niveau de base (par dΓ©faut) ou 2
		       pour plus de verbositΓ©.
		  -d, --debug
		      Active les impressions de dΓ©bogage durant le processus. Veuillez noter que la sortie peut Γͺtre Γ©norme.
		* Veuillez noter que --skip-empty a prioritΓ© sur --not-translate qui a prioritΓ© sur --empty.
		 		rpyTranslations diff [--help] for_files... [options]

		for_files et from_files sont des successions de fichiers dont la longueur doit Γͺtre Γ©gale.
		for_files sont les fichiers dans lesquels les diffΓ©rences de traduction doivent Γͺtre comparΓ©es.
		from_files sont les fichiers de rΓ©fΓ©rence Γ  partir desquels les autres traductions sont obtenues pour la
		 comparaison.

		Comme cette commande est destinΓ©e Γ  comparer les ajouts et suppressions dans les for-files, il nβy a pas
		 dβoption proxy, et Γ©videmment, la traduction vide et le mot clΓ© 'pass' sont traitΓ©s comme diffΓ©rents.

		options:
		  -h, --help
		      Affiche cette aide spΓ©cifique pour la commande 'diff' et quitte.
		  --ignore-newpart
		      Indique si les comparaisons de partie 'new' des traductions doivent Γͺtre sur la sortie ou non.
		  --what
		      Indique si ce qui est ajoutΓ© ou qui nβest plus prΓ©sent dans les fichiers de traduction doit Γͺtre en
		       sortie ou non.
		  --reflines
		      Indique si les comparaisons des lignes de rΓ©fΓ©rence doivent Γͺtre sur la sortie ou non.
		  --tr-id
		      Indique si les comparaisons des identifiants de translation doivent Γͺtre sur la sortie ou non.
		  -% n, --multi n, --lists n
		      Ceci permet dβindiquer le nombre de listes Γ  partir desquelles les autres traductions sont obtenues.
		       Cela inclut le premier groupe de fichiers (le for_files).
		      Donc, pour 2 for_files, vous devez le mettre avec 3.
		  -v [lvl], --verbose [lvl]
		      Active la verbositΓ© durant le processus.
		      lvl peut Γͺtre passΓ© pour dΓ©finir le niveau de verbositΓ©, 1 pour le niveau de base (par dΓ©faut) ou 2
		       pour plus de verbositΓ©.
		  -d, --debug
		      Active les impressions de dΓ©bogage durant le processus. Veuillez noter que la sortie peut Γͺtre Γ©norme.
		 		rpyTranslations fix-empty [--help] for_files... [options]

		for_files sont les fichiers oΓΉ les traductions vides doivent Γͺtre corrigΓ©es.

		options:
		  -h, --help
		      Affiche cette aide spΓ©cifique pour la commande 'fix-empty' et quitte.
		  -a act, --action act
		      Cette option permet de spΓ©cifier lβaction Γ  effectuer lorsque des chaΓ?nes vides sont trouvΓ©es.
		      act doit Γͺtre lβun des suivants :
		          P   Par dΓ©faut. Toutes les traductions 'dialogues' vides sont remplacΓ©es par le mot clΓ© 'pass' et
		               la traduction 'strings' vide est supprimΓ©e.
		          C   Toutes les traductions 'dialogues' vides sont remplacΓ©es par le mot clΓ© 'pass' et la
		               traduction 'strings' vide est commentΓ©e.
		          R   Toutes les traductions vides sont supprimΓ©es. Veuillez noter que cela supprime Γ©galement la
		               traduction 'dialogs' dΓ©finie avec le mot clΓ© 'pass'.
		  -o dir, --subdir dir
		      Si mis, il indique un sous-rΓ©pertoire oΓΉ les fichiers de traduction corrigΓ©es sont enregistrΓ©s,
		       sinon sur les for-files eux-mΓͺmes.
		  -v [lvl], --verbose [lvl]
		      Active la verbositΓ© durant le processus.
		      lvl peut Γͺtre passΓ© pour dΓ©finir le niveau de verbositΓ©, 1 pour le niveau de base (par dΓ©faut) ou 2
		       pour plus de verbositΓ©.
		  -d, --debug
		      Active les impressions de dΓ©bogage durant le processus. Veuillez noter que la sortie peut Γͺtre Γ©norme.
		 		rpyTranslations populate [--help] for_files... from_files... [options]

		for_files et from_files sont des successions de fichiers dont la longueur doit Γͺtre Γ©gale.
		for_files sont les fichiers de rΓ©fΓ©rence dans lesquels les traductions doivent Γͺtre peuplΓ©es.
		from_files sont les fichiers de rΓ©fΓ©rence Γ  partir desquels les traductions sont obtenues.

		options:
		  -h, --help
		      Affiche cette aide spΓ©cifique pour la commande 'populate' et quitte.
		  -b, --bunch
		      Indique si, lorsquβune traduction nβest pas trouvΓ©e, elle doit Γͺtre recherchΓ©e dans les autres
		       fichiers du groupe fromFiles actif.
		  --bulk [n]
		      Indique si quelques fichiers doivent Γͺtre remplis Γ  partir de plusieurs (ou plusieurs Γ  partir de
		       quelque-uns).
		      Si n est mis, il indique le nombre de fichiers dans le groupe for_files (1 par dΓ©faut).
		  -o dir, --subdir dir
		      Si mis, il indique un sous-rΓ©pertoire oΓΉ les fichiers de traduction remplis sont enregistrΓ©s, sinon
		       sur les for-files eux-mΓͺmes.
		  -% n, --multi n, --lists n
		      Cela permet dβindiquer le nombre de listes Γ  partir desquelles fusionner plusieurs traductions. Cela
		       inclut le premier groupe de fichiers (le for_files).
		      Donc, pour 2 from_files, vous devez le mettre avec 3.
		      Il prend par dΓ©faut la valeur 2 sauf si --bulk est utilisΓ©, dans ce cas, il prend la valeur 1 par
		       dΓ©faut car ce dernier contrΓ΄le la premiΓ¨re liste de fichiers.
		      Le flux de travail interne est comme :
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * OΓΉ from_files[#] est un groupe de fichiers de mΓͺme longueur que forFiles.
		  -a, --ask
		      Si mis, une invite demandera de confirmer ou non un remplacement de traduction (uniquement pour les
		       non-vides).
		      Il est recommandΓ© dβutiliser lβoption verbose avec ceci, afin que vous puissiez savoir pour quel
		       fichier la demande est.
		  -F, --force, --overwrite
		      Active le remplacement des traductions existantes.
		  --proxy lvl
		      Si une traduction nβest pas trouvΓ©e avec les mΓ©thodes normales, cette option permet dβutiliser
		       dβautres mΓ©thodes basΓ©es sur le niveau de proximitΓ© (lvl).
		      lvl peut Γͺtre un des suivants :
		          0   Par dΓ©faut. Aucun proxy de fait.
		          1   Pour 'dialogs', autorise Γ  se concentrer uniquement sur la partie nom de lβidentificateur de
		               dialogue.
		          2   Pour 'dialogs', permet dβignorer complΓ¨tement lβidentifiant de dialogue et en dernier recours
		               autorise la recherche dans les traductions 'strings', mais pas pour 'strings' dans les
		               traductions 'dialogs'.
		          3   Permet de se concentrer uniquement sur les chaines. Avec cela, 'dialogs' peut maintenant Γͺtre
		               considΓ©rΓ© comme identique Γ  'strings', de sorte que 'strings' peut maintenant rechercher
		               dans les traductions 'dialogs'.
		          4   Les chaines sont comparΓ©es pour obtenir au moins une correspondance de 90%.
		              Veuillez noter que ce processus peut Γͺtre trΓ¨s long.
		  -W, --no-proxy-warn
		      Lors dβune tentative de remplissage dβune traduction vide avec une traduction par proxy, une demande
		       est effectuΓ©e.
		      Cette option permet dβoutrepasser cette demande.
		  --proxy-y lvl
		      Cette option est identique Γ  --proxy mais avec lβeffet de --no-proxy-warn en plus.
		  -N, --from-nID
		      Par dΓ©faut, la chaine du nom de lβidentifiant est prise Γ  partir de for_files, utilisez cette option
		       si vous prΓ©fΓ©rez quβelle soit prise Γ  partir de from_files.
		  -v [lvl], --verbose [lvl]
		      Active la verbositΓ© durant le processus.
		      lvl peut Γͺtre passΓ© pour dΓ©finir le niveau de verbositΓ©, 1 pour le niveau de base (par dΓ©faut) ou 2
		       pour plus de verbositΓ©.
		  -d, --debug
		      Active les impressions de dΓ©bogage durant le processus. Veuillez noter que la sortie peut Γͺtre Γ©norme.
		* Notez que --ask a prioritΓ© sur --force.
		* Veuillez noter que --proxy a prioritΓ© sur --proxy-y (bien que lβeffet secondaire de celle-ci soit
		   conservΓ©e) et que --no-proxy-warn nβa aucun effet si --proxy-y est Γ©galement utilisΓ©.

		Veuillez noter que `populate --bulk 2 ./common.rpy ./init.rpy -% 1 ../from/common.rpy ../from/init.rpy`
		 rΓ©sulte au mΓͺme que `populate --bunch ./common.rpy ./init.rpy ../from/common.rpy ../from/init.rpy`
		 mais est moins efficace.
		 		rpyTranslations reorder [--help] for_files... [options]

		for_files sont les fichiers de rΓ©fΓ©rence oΓΉ les traductions doivent Γͺtre rΓ©organisΓ©es.

		Cette commande tente de regrouper des traductions similaires pour faciliter les traductions et les
		 comparaisons de traductions Γ©quivalentes.
		Puisque les traductions 'strings' sont gΓ©nΓ©ralement uniques, il convient de noter que Γ§a ne fonctionnent
		 que pour les 'dialogues'.
		En outre, toutes les occurrences sont regroupΓ©es au premier emplacement dβoccurrence.

		options:
		  -h, --help
		      Affiche cette aide spΓ©cifique pour la commande 'reorder' et quitte.
		  -o dir, --subdir dir
		      Si mis, il indique un sous-rΓ©pertoire oΓΉ les fichiers de traduction rΓ©organisΓ©s sont enregistrΓ©s,
		       sinon sur les for-files eux-mΓͺmes.
		  -r, --reverse
		      Si mis, essaie de rΓ©organiser comme cβΓ©tait initialement (tel quβextrait par Ren'Py) Γ  lβaide des
		       commentaires de rΓ©fΓ©rence de ligne (# dir/file.rpy:line).
		      Cette fonctionnalitΓ© ignore les traductions qui nβont pas de commentaire de ligne de rΓ©fΓ©rence,
		       celles sΓ©lectionnΓ©es pour Γͺtre rΓ©organisΓ©es sont jointes et peuvent Γͺtre insΓ©rΓ©es entre deux groupes
		       de ces traductions ignorΓ©es.
		  --proxy
		      Si mis, cette option fait que les traductions soit Γ©valuΓ©es avec une mΓ©thode proxy.
		      Avec cette mΓ©thode, les traductions sont regroupΓ©es en fonction de leur reprΓ©sentation
		       alphanumΓ©rique normalisΓ©e.
		      Veuillez noter que cette option nβa aucun effet si elle est utilisΓ©e avec lβoption --reverse.
		  -v [lvl], --verbose [lvl]
		      Active la verbositΓ© durant le processus.
		      lvl peut Γͺtre passΓ© pour dΓ©finir le niveau de verbositΓ©, 1 pour le niveau de base (par dΓ©faut) ou 2
		       pour plus de verbositΓ©.
		  -d, --debug
		      Active les impressions de dΓ©bogage durant le processus.
		 
		Informations supplΓ©mentaires:

		- Γ propos de lβoption --ask:
		    Le modΓ¨le utilisΓ© pour afficher les demandes est le suivant :
		      `
		      Pour: [pour: ancien dID]
		      	<pour: ancienne chaine>
		      Tente de remplacer: [pour: nouveau dID]
		      	<pour: nouvelle chaine>
		      Avec: [avec: dID]
		      	<avec: chaine>
		      `
		    La prΓ©sence de parties dID dΓ©pend du type de traduction (traduction de chaine ou de dialogue).
		    Seules les traductions de dialogue peuvent contenir un dID, et si lβune dβelles nβen a pas, elle sera
		     affichΓ©e comme {0}.
		    Il est important de noter que le dID contient deux parties, la premiΓ¨re partie est un NID (nom
		     identifiant) et la seconde est des arguments dβimage.
		    Comme mentionnΓ© plus haut, seul le NID peut Γͺtre affectΓ© par lβoption --from-nID, cela signifie que,
		     dβabord, le dID est obtenu dans lβordre de prioritΓ© suivant :
		      [pour: nouveau dID] > [pour: ancien dID]
		     et quβensuite, si lβoption --from-nID est utilisΓ©e, le NID peut Γͺtre pris de [avec: dID] si :
		      1) le dID obtenu prΓ©cΓ©demment nβest pas {0} et son NID est de type chaine
		      2) ce [avec: dID] nβest pas {0} et que son NID est de type chaine
		     sinon, le NID du dID obtenu prΓ©cΓ©demment est conservΓ©.
		 
  pas Tous les fichiers doivent Γͺtre diffΓ©rents Une erreur sβest produite :
 Les deux listes de fichiers doivent avoir des fichiers diffΓ©rents Pour:
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
	{From} Argument invalide : Argument manquant : Veuillez noter que le 'Avec' a Γ©tΓ© trouvΓ© par proxy. ProcΓ©der ? Les listes de paires de fichiers doivent avoir une longueur Γ©gale, {N} donnΓ©e. Les listes de paires de fichiers doivent avoir une longueur Γ©gale, {N} donnΓ©es. La longueur des deux listes de fichiers doit Γͺtre Γ©gale La longueur de chaque liste de fichiers doit Γͺtre Γ©gale Cette fonction essaie dβestimer la proximitΓ© entre deux chaines.
Veuillez noter que ceci peut difficilement Γͺtre prΓ©cis et exempt dβerreur.
 Avertissement : nβexiste pas ou nβest pas un fichier forFiles: Liste - Liste des fichiers qui doivent Γͺtre peuplΓ©s.
FromFiles: Liste,... - Successions de liste de fichiers dβoΓΉ sont obtenu les traductions.
bunch: Booleen - Indique si, lorsquβune traduction nβest pas trouvΓ©e, elle doit Γͺtre recherchΓ©e dans les
       autres fichiers du groupe fromFiles actif.
bulk: BoolΓ©en - Indique si quelques fichiers doivent Γͺtre remplis Γ  partir de plusieurs (ou plusieurs Γ 
       partir de quelque-uns).
proxy: Entier - De 0 Γ  4 (inclus) pour indiquer un niveau de proxy, 0 pour le dΓ©sactiver, 4 pour rendre la
       tΓ’che longue.
proxyWarn: BoolΓ©en - Indique si une demande est effectuΓ©e ou non lors dβune tentative de remplissage dβune
           traduction vide avec une traduction proxy.
fromNID: Booleen - Indique si la chaine du nom de lβidentifiant est prΓ©fΓ©rΓ©e Γͺtre Γ  partir de 'fromFiles'
         ou de 'forFiles'.
overwrite: CaractΓ¨re - Devrait Γͺtre : 'N' pour non, 'A' pour demande, ou 'F' pour force. (la verbositΓ© est
           recommandΓ© avec 'A')
outdir: ChaΓ?ne - Un chemin relatif aux chemins 'forFiles' pour indiquer oΓΉ enregistrer le rΓ©sultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbositΓ©, 0 pour dΓ©sactiver.
debug: BoolΓ©en - Active ou non le dΓ©bogage. La quantitΓ© sur cette fonction peut Γͺtre Γ©norme.
 forFiles: Liste - Liste des fichiers dans lesquels les diffΓ©rences de traduction doivent Γͺtre comparΓ©es.
FromFiles: Liste,... - Successions de liste de fichiers dβoΓΉ obtenir les autres traductions.
newpart: BoolΓ©en - Sortie ou non des comparaisons de partie 'new' des traductions.what: BoolΓ©en - Sortie ou non de ce qui est ajoutΓ© ou nβest plus prΓ©sent dans les fichiers de traduction.
reflines: BoolΓ©en - Sortie ou non des comparaisons de ligne de rΓ©fΓ©rence.trID: BoolΓ©en - Sortie ou non des comparaisons dβidentifiant de traduction.verbose: Entier - Indique le niveau de verbositΓ©, 0 pour dΓ©sactiver.
debug: BoolΓ©en - Active ou non le dΓ©bogage. La quantitΓ© sur cette fonction peut Γͺtre Γ©norme.
 forFiles: Liste - Liste des fichiers pour lesquels des traductions vides doivent Γͺtre corrigΓ©es.
action: CaractΓ¨re - Devrait Γͺtre : 'P' pour remplacer la traduction vide par le mot clΓ© 'pass', 'C' pour
        commenter Γ©galement les traductions 'strings' au lieu de les supprimer, ou 'R' pour supprimer
        toutes les traductions vides ou passΓ©es.
outdir: ChaΓ?ne - Un chemin relatif aux chemins 'forFiles' pour indiquer oΓΉ enregistrer le rΓ©sultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbositΓ©, 0 pour dΓ©sactiver.
debug: BoolΓ©en - Active ou non le dΓ©bogage. La quantitΓ© sur cette fonction peut Γͺtre Γ©norme.
 forFiles: Liste - Liste des fichiers oΓΉ les traductions doivent Γͺtre vΓ©rifiΓ©es.
untranslated: Entier - Devrait Γͺtre : 1 pour vΓ©rifier si vide, 2 pour vΓ©rifier Γ©galement lorsque passΓ©, ou
              0 pour dΓ©sactiver.
where: BoolΓ©en - Sortie ou non de oΓΉ se trouvent les traductions vides.
formats: BoolΓ©en - VΓ©rifier ou non le formatage des traductions.
         Comme les espaces de dΓ©but/fin, les crochets ([]) et les accolades ({}).
verbose: Entier - Indique le niveau de verbositΓ©, 0 pour dΓ©sactiver.
debug: BoolΓ©en - Active ou non le dΓ©bogage. La quantitΓ© sur cette fonction peut Γͺtre Γ©norme.
 forFiles: Liste - Liste des fichiers dans lesquels les traductions doivent Γͺtre rΓ©organisΓ©es.
reverse: BoolΓ©en - RΓ©organiser telle que cβΓ©tait Γ  lβorigine (telle quβextrait par Ren'Py) ou procΓ©der
         normalement.
proxy: BoolΓ©en - Active ou non le regroupement en fonction de leur reprΓ©sentation alphanumΓ©rique
       normalisΓ©e.
outdir: ChaΓ?ne - Un chemin relatif aux chemins 'forFiles' pour indiquer oΓΉ enregistrer le rΓ©sultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbositΓ©, 0 pour dΓ©sactiver.
debug: BoolΓ©en - Active ou non le dΓ©bogage.
 les commandes help nβacceptent aucun argument ou option. le 1er argument doit correspondre Γ  lβun des arguments suivants : lβoption {} requiΓ¨re un argument. lβoption {} doit Γͺtre un entier absolu, donnΓ© lβoption {} doit Γͺtre un entier, donnΓ© le paramΓ¨tre {} doit Γͺtre  le paramΓ¨tre {} doit Γͺtre 1 pour vide, 2 pour pass, ou 0 pour dΓ©sactiver le paramΓ¨tre {} doit Γͺtre N pour non, A pour demander ou F pour force le paramΓ¨tre {} doit Γͺtre un nombre entier compris entre 0 et 4 (inclus) le paramΓ¨tre {} doit Γͺtre de type boolΓ©en 