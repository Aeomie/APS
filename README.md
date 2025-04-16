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
- `./exec.sh SampleApsV <option>` : où <option> est la même qu'auparavant.

Pour exécuter les tests de la version précédente,
par exemple si vous êtes dans `APS 1` et que
vous voulez les tester sur les tests de `APS 0`, il suffit d'appeler :
- `./exec_prv.sh <option>`
