L'implémentation de la solution au problème de la carte au trésor se trouve dans le dossier Modules.
  
  TreasureMapModels.fs //modélisation des données du problème
    initMap             //initialise la map à partir des lignes lues du fichier en entrée
    parseFileMap        //initialise la map à partir du fichier en entrée
    writeFileMap        //écrit le fichier résultat à partir de la map en entrée.
  
  TreasureMapProcess.fs //implémentation de la résolution du problème
    processMovment      //calcul les différents mouvements des aventuriers
    processTreasures    //collecte les trésors pour les aventuriers
    resolveConflicts    //résoud les conflits de position entre les aventuriers
    processAdventurers  //résoud les comportements des aventuriers
    processTreasureMap  //traite le fichier carte, écrit le fichier résultat et retourne la Map

Les différents tests se trouve dans le dossier tests.
  
  UnitTest1.fs
    Modelisation de la carte au tresor
    Ecriture de la carte au tresor
    1 mouvement sans obstacle
    1 mouvement avec obstacle Montagne 
    1 mouvement avec obstacle Aventurier
    Resoudre un conflit simple sur 1 mouvement
    Resoudre un conflit complexe sur 1 mouvement
    Collecte de tresor sur 1 mouvement
    Test integration 1
    Test integration 2

  Les différents fichiers de test sont dans le dossier Ressources.

Pour exécuter les tests lancer la commande "dotnet test" depuis le dossier racine.
