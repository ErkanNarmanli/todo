Read Me
=======

Installation
------------
* modifier le champ `userName` dans le fichier `todo.hs`.
* compilier le fichier `todo.hs` avec `ghc --male todo`.
* copier le fichier `todo` dans `~/bin`.
* Ajouter les lignes suivantes au début de votre fichier `~/.zshrc` ou `~/.bashrc` :

    ```
    #Todo liste à l'ouverture du terminal
    todo sort
    todo viewTime
    ```

Fonctionnement
--------------
Le programme met tout ses fichiers dans le répertoire `~/.todo`, vous pouvez le changer en modifiant la ligne correspondante dans le fichier `todo.hs`.

Utilisation
-----------
Pour voir une liste explicative des fonctions proposées par le programme, appeler dans votre terminal :

    $ todo help

Améliorations
-------------
Faire en sorte de ne plus avoir à modifier à la main le champ `userName`. Pour cela il faut se servir de la fonction `getUserDocumentsDirectory` et remplacer correctement les occurences (et celles qui en découlent) dans le fichier source.
