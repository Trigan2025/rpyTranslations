��    #      4  /   L        a  	  �  k  �  
    �  �	  �  e  :      �&     �&     �&  5   �&  1   '     A'     T'  /   i'     �'  s   �'  1   (  }   H(     �(     �(  `  �(    N+  :  n-  �  �/    s1  3   �3  .   �3  $   �3  p   4     �4  B   �4  :   �4  <   5  &   S5    z5  M  �6  U  �;  �  ;A  �  �G  V  �M  �  �X     �`  +   �`     �`  B   �`  :   #a     ^a     ra  ?   �a     �a  �   �a  9   yb  �   �b     Fc  (   Vc  �  c  b  7f  �  �h  $  >k  h  cm  :   �o  D   p  *   Lp  �   wp     �p  K   q  G   hq  J   �q  ,   �q                                                       "                              
      #             	   !                                                  		python3 -m rpyTranslations ({cmds}) [args] [options]

		Please note that the parentesis are just to englob all of the possible command and should not be put.

		-h, --help
		  Show this general help and exit.
		  Enter a command followed the help one to see their specific help.
		  Please note that help commands obviousely doesn't accept any args or options even if they still
		   show the requested help.
		args
		  Their are specific for each command and generaly should be put in a specific order.
		  See the specific help of these commands for more information.
		options
		  They generaly can be put everywhere in the argument realm of command.
		  Though commands has their own set of options, here are some general options below:
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
		  -f, --format
		      Sometimes leading white-spaces can be typos but generaly they are on purpose (especialy when an
		       'extend' is in use).
		      Check if the translations had keep all kinds of formating and that leading white-spaces are
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
		      Indicate if the comparisons of new-part of translations should be output or not.
		  --reflines
		      Indicate if the comparisons of reference-lines should be output or not.
		  --tr-id
		      Indicate if the comparisons of translation-identifiers should be output or not.
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
		      If put, it indicate a sub-directory to where the fixed translation files are saved, otherwise
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
		  -o dir, --subdir dir
		      If put, it indicate a sub-directory to where the populated translation files are saved,
		       otherwise on the for-files themself.
		  -% n, --multi n, --lists n
		      This allow to give the number of list from where merging multiple translations. This include
		       the first group of files (the for_files).
		      So, for 2 from_files, you should put it with 3.
		      The internal workflow is like:
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * Where from_files[#] are group of files of the same length as forFiles.
		  -a, --ask
		      If put, a prompt will ask to confirm or no a replacement of translation (only for non-empty).
		  -f, --force, --overwrite
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
		  -v [lvl], --verbose [lvl]
		      Activate the verbosity during the process.
		      lvl can be pass to set the verbosity level, 1 for basic (default) or 2 for more verbosity.
		  -d, --debug
		      Activate the debug printings during the process. The output can be huge.
		* Please note that --ask take precedence over --force.
		 		rpyTranslations reorder [--help] for_files... [options]

		for_files are the reference files where the translations need to be reordered.

		This command try regrouping similiar translations to ease translations and comprisons of equivalant
		 translations.
		Since 'strings' translations are generaly unique, it should be note that it's only work for 'dialogs'.
		Also, all occurences are regrouping to the first occurence place.

		options:
		  -h, --help
		      Show this specific help for the 'reorder' command and exit.
		  -o dir, --subdir dir
		      If put, it indicate a sub-directory to where the reordered translation files are saved,
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
  not All files need to be different An error occured:
 Both list of files need to have their files different For: {old}
Attempt to replace: {new}
From: {From} Invalide argument: Invalide parameters: Please note that the 'From' was found by proxy. Proceed? The file pair lists need to have equal length, {N} gived. The file pair lists need to have equal length, {N} gived. The length of both list of files need to be equal This function try to estimate the proxymity between two strings.
Please note that this can hardly be precise and error-free.
 Warning: doen't exist or is not a file forFiles: List - The list of files that need to be populated.
FromFiles: List,... - Successions of list of files from where to get the translations.
proxy: Integer - From 0 to 4 (includes) to indicate a level of proxy, 0 to diactivate, 4 made it long.
overwrite: Char - Should be: 'N' for no, 'A' for ask, or 'F' for force.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where differences of translation need to be compared.
FromFiles: List,... - Successions of list of files from where to get other translations.
newpart: Booleen - Output or not comparisons of new-part of translations.
reflines: Booleen - Output or not comparisons of reference-lines.
trID: Booleen - Output or not comparisons of translation-identifiers.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where empty translations need to be fixed.
action: Char - Should be: 'P' to change empty translation to the 'pass' keyword, 'C' to also comment
        'strings' translations instead of deleting them, or 'R' to remove all empty or passed translation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where translations need to be checked.
untranslated: Integer - Should be: 1 to check empty, 2 to also check passed, or 0 to deactivate.
formats: Booleen - Check or no the formating into translations.
         Like leading white-spaces, brackets ([]) and braces ({}).
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging. The amount in this function can be huge.
 forFiles: List - The list of files where translations need to be reordered.
reverse: Booleen - Reordering as it was originaly (as extracted by Ren'Py) or process normaly.
proxy: Booleen - Active or not the regroupement in function of their normalized alpha-numeric
       representation.
outdir: String - A path relative from the 'forFiles' paths to indicate where to save the result or None
        to save inplace.
verbose: Integer - Indicate the level of verbosity, 0 to deactivate.
debug: Booleen - Active or not the debuging.
 help commands do not accept any argument or option. the 1st argument need to one of the following: {} option should be an integer, give {} parameter need to be set to True or {} parameter need to be greater than 0 to indicate which checking to make {} parameter should be  {} parameter should be 1 for empty, 2 for pass, or 0 to deactivate {} parameter should be N for no, A for ask, or F for force {} parameter should be an integer between 0 and 4 (includes) {} parameter should be of booleen type MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Project-Id-Version: 1.0
PO-Revision-Date: 2022-05-21 16:16+0200
Last-Translator: TRIGAN <trigan2025@outlook.com>
Language-Team: none
Language: fr
Plural-Forms: nplurals=2; plural=n > 1 ? 0 : 1;
 		python3 -m rpyTranslations ({cmds}) [args] [options]

		Veuillez noter que les parentèses sont juste pour englober toutes les commandes possibles et ne doivent pas
		 être mises.

		-h, --help
		  Afficher cette aide générale et quitter.
		  Entrez une commande suivie de celle d’aide pour afficher leur aide spécifique.
		  Veuillez noter que les commandes d’aide n'acceptent évidemment pas d’arguments ou d’options, même si elles
		   affichent toujours l’aide demandée.
		args
		  Ils sont spécifiques à chaque commande et doivent généralement être placés dans un ordre spécifique.
		  Consultez l’aide spécifique de ces commandes pour plus d’informations.
		options
		  Elles peuvent généralement être placées partout dans le domaine des arguments de commande.
		  Bien que les commandes aient leur propre jeu d’options, voici quelques options générales:
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
		      Indique is les comparaisons de partie 'new' des traductions doivent être sur la sortie ou non.
		  --reflines
		      Indique is les comparaisons des lignes de référence doivent être sur la sortie ou non.
		  --tr-id
		      Indique is les comparaisons des identifiants de translation doivent être sur la sortie ou non.
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
		  -o dir, --subdir dir
		      Si mis, il indique un sous-répertoire où les fichiers de traduction remplis sont enregistrés, sinon
		       sur les for-files eux-mêmes.
		  -% n, --multi n, --lists n
		      Cela permet d’indiquer le nombre de listes à partir desquelles fusionner plusieurs traductions. Cela
		       inclut le premier groupe de fichiers (le for_files).
		      Donc, pour 2 from_files, vous devez le mettre avec 3.
		      Le flux de travail interne est comme :
		          for_files - from_files[0] > pop_files ; pop_files - from_files[1] > pop_files ; ...
		          * Où from_files[#] est un groupe de fichiers de même longueur que forFiles.
		  -a, --ask
		      Si mis, une invite demandera de confirmer ou non un remplacement de traduction (uniquement pour les
		       non-vides).
		  -f, --force, --overwrite
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
		          3   Permet de se concentrer uniquement sur les chaînes. Avec cela, 'dialogs' peut maintenant être
		               considéré comme identique à 'strings', de sorte que 'strings' peut maintenant rechercher
		               dans les traductions 'dialogs'.
		          4   Les chaînes sont comparées pour obtenir au moins une correspondance de 90%.
		              Veuillez noter que ce processus peut être très long.
		  -v [lvl], --verbose [lvl]
		      Active la verbosité durant le processus.
		      lvl peut être passé pour définir le niveau de verbosité, 1 pour le niveau de base (par défaut) ou 2
		       pour plus de verbosité.
		  -d, --debug
		      Active les impressions de débogage durant le processus. Veuillez noter que la sortie peut être énorme.
		* Notez que --ask a priorité sur --force.
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
  pas Tous les fichiers doivent être différents Une erreur s'est produite :
 Les deux listes de fichiers doivent avoir des fichiers différents Pour: {old}
Tente de remplacer: {new}
À partir de: {From} Argument invalide : Paramètres invalides : Veuillez noter que le 'À partir de' a été trouvé par proxy. Procéder ? Les listes de paires de fichiers doivent avoir une longueur égale, {N} donnée. Les listes de paires de fichiers doivent avoir une longueur égale, {N} données. La longueur des deux listes de fichiers doit être égale Cette fonction essaie d'estimer la proximité entre deux chaînes.
Veuillez noter que ceci peut difficilement être précis et exempt d’erreur.
 Avertissement : n’existe pas ou n’est pas un fichier forFiles: Liste - Liste des fichiers qui doivent être peuplés.
FromFiles: Liste,... - Successions de liste de fichiers d’où sont obtenu les traductions.
proxy: Entier - De 0 à 4 (inclus) pour indiquer un niveau de proxy, 0 pour le désactiver, 4 pour rendre la
       tâche longue.
overwrite: Caractère - Devrait être : 'N' pour non, 'A' pour demande, ou 'F' pour force.
outdir: Chaîne - Un chemin relatif aux chemins 'forFiles' pour indiquer où enregistrer le résultat ou None
        pour enregistrer sur-place.
verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
debug: Booléen - Active ou non le débogage. La quantité sur cette fonction peut être énorme.
 forFiles: Liste - Liste des fichiers dans lesquels les différences de traduction doivent être comparées.
FromFiles: Liste,... - Successions de liste de fichiers d’où obtenir les autres traductions.
newpart: Booléen - Sortie ou non des comparaisons de partie 'new' des traductions.reflines: Booléen - Sortie ou non des comparaisons de ligne de référence.trID: Booléen - Sortie ou non des comparaisons d’identifiant de traduction.verbose: Entier - Indique le niveau de verbosité, 0 pour désactiver.
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
formats: Booléen - Vérifier ou non le formatage en traductions.
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
 les commandes help n’acceptent aucun argument ou option. le 1er argument doit correspondre à l’un des arguments suivants : l’option {} doit être un entier, donné Le paramètre {} doit être défini sur True ou le paramètre {} doit être supérieur à 0 pour indiquer la vérification à effectuer le paramètre {} doit être  le paramètre {} doit être 1 pour vide, 2 pour pass, ou 0 pour désactiver le paramètre {} doit être N pour non, A pour demander ou F pour force le paramètre {} doit être un nombre entier compris entre 0 et 3 (inclus) le paramètre {} doit être de type booléen 