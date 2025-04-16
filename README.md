# APS

## Description
Ce projet est le projet d'APS pour les étudiants du Master STL.
Il est divisé en quatre parties :
1. **APS0 :** Noyau fonctionnel.
2. **APS1 :** Noyau impératif (ajout d'une mémoire et de nouveaux appels).
3. **APS1a :** Références et valeurs.
4. **APS2 :** Ajout des tableaux et leurs fonctions.

Chaque partie d'APS contient un typeur,
 qui vérifie si le code est bien typé,
et un évaluateur, qui exécute le programme.

## Commandes
Lors du build, il y aura un certain avertissement de cette forme :
`| _ -> failwith "Vector size not well initialized"`.
Cependant, j'ai décidé de le laisser, car cela sert de mécanisme de gestion d'erreur.<br>
Pour la partie `Build` : il suffit d’exécuter `make` dans le terminal.<br>
- Cela produira tous les fichiers de l’évaluateur, ainsi que le prologTerm (utilisé dans le typeur),
requis pour exécuter les programmes. Pour nettoyer les fichiers après les tests, il suffit d’exécuter `make clean`.


### Appel individuel
Après le `make`, pour évaluer un programme, il faut exécuter :
- `./evaluator fichier.aps`

Et pour typer un programme, il faut exécuter :
- `./prologTerm fichier.aps | swipl typeur.pl`

### Appel Automatique

Pour exécuter notre évaluateur ou typeur sur un fichier contenant des programmes APS,
ou sur un répertoire incluant des sous-répertoires contenant ces programmes. <br>

Il faut éxecuter : 
- `./exec.sh <fichier> <option>`
- où  `<fichier>` est le nom de votre fichier.
- et `<option>` peut prendre trois valeurs:
    - **evaluator :**  pour évaluer les programmes.
    - **typeur :** pour typer les programmes.
    - **prologTerm :** pour afficher le code transformé de l'AST vers un langage compréhensible par Prolog.

### Lancement des tests
Dans chaque version d'APS, vous trouverez des tests correspondant à cette version
dans un fichier nommé `SamplesApsV`, où V représente la version. Par exemple :

- **APS 0 :** SamplesAps0.
- **APS 1 :** SamplesAps1.
- **APS 1a :** SamplesAps1a.
- **APS 2 :** SamplesAps2.

Pour exécuter les tests, il suffit d'appeler :
- `./exec.sh SamplesApsV <option>` : où <option> est la même qu'auparavant.

Pour exécuter les tests de la version précédente,
par exemple si vous êtes dans `APS 1` et que
vous voulez les tester sur les tests de `APS 0`, il suffit d'appeler :
- `./exec_prv.sh <option>`

Dans les tests, tous les tests devraient bien s'exécuter, sauf ceux qui se trouvent dans le répertoire `FailTests`.
Cependant, certains d'entre eux s'exécutent dans l'évaluateur mais pas dans le typeur, car dans le typeur, on ne peut pas éxecuter `ECHO true`,
contrairement à l'évaluateur où cela est possible.
En d'autres termes, dans l'évaluateur, il est possible d'afficher des valeurs booléennes, mais ce n'est pas le cas dans le typeur


### WSL
Pour exécuter le programme dans WSL, vous devez d'abord appeler ces commandes:
1. `sudo apt-get install dos2unix`
2. `chmod +x exec.sh`
3. `dos2unix exec.sh`
4. `chmod +x exec_prv.sh`
5. `dos2unix exec_prv.sh`

Pour vous assurer que vos fichiers `.sh` peuvent être exécutés.