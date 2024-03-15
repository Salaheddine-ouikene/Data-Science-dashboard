## Introduction

Ce projet consiste en la création d'une interface en R Shiny pour l'exploration de données et l'entraînement de modèles de classification supervisée. L'objectif principal est de développer une interface qui peut charger divers types de données, effectuer une analyse exploratoire, et entraîner des modèles pour une étude de cas sur le jeu de données "heart".

## Structure des fichiers

- `ui.r`: Ce fichier contient la partie de l'interface utilisateur (UI) de l'application Shiny. Il définit la mise en page et les éléments visuels de l'interface.

- `server.r`: Ce fichier contient la partie serveur de l'application Shiny. Il gère la logique des opérations, le traitement des données, et l'entraînement des modèles.

## Fonctionnalités de l'interface

Cette interface en R Shiny propose les fonctionnalités suivantes :

### Chargement de données

- l'interface permet de charger tout type de données en format CSV.
- Elle détecte automatiquement le type de chaque variable (qualitative, quantitative).
- Elle gère les catégories en cas de variables qualitatives.
- Elle prend en compte la présence d'outliers et de valeurs manquantes.

### Analyse exploratoire des données

- L'interface dispose d'un espace dédié à l'analyse exploratoire des données.
- Elle permet une analyse unidimensionnelle et bidimensionnelle.
- Elle utilise des visualisations appropriées pour explorer les données, y compris les histogrammes, les boxplots, les scatterplots, etc.
- Elle calcule des métriques pour évaluer la force des relations entre les variables.

### Entraînement de modèles

- l'interface contient trois modèles de classification supervisée pour l'entraînement, regression logistique, arbre de decision et foret aleatoire.
- L'interface permet une évaluation comparative des modèles.
- Les résultats sont affichés, y compris la précision Precision, Recall, Fscore, la courbe ROC, et l'aire sous la courbe (AUC).
- Les features les plus importants sont identifiés et affichés.

## Utilisation de l'interface

1. Clonez ce dépôt Git sur votre machine locale.

2. Exécutez l'application Shiny en exécutant `shiny::runApp()` dans RStudio, en vous assurant que les fichiers `ui.r` et `server.r` sont dans le même répertoire.

3. Chargez un jeu de données de votre choix ou utilisez le jeu de données "heart" ou "mammographic_masses"  fourni en exemple.

4. Explorez les données, entraînez les modèles et analysez les résultats en utilisant l'interface.

## Jeu de données

j'ai choisi d'effectuer une étude de cas sur le jeu de données "heart", qui est largement utilisé pour la classification binaire.

Pour plus d'informations sur ce jeu de données, vous pouvez consulter [ici](https://archive.ics.uci.edu/ml/datasets/heart+Disease).
