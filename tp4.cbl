       PROGRAM-ID. TP4-DAVID-HAYAT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RECETTES-IDX  ASSIGN TO "RECETTES.DAT"
               ORGANIZATION         INDEXED
               ACCESS MODE          DYNAMIC
               RECORD KEY           RECETTE-NOM.
           SELECT COMMENTAIRES-IDX ASSIGN TO "COMMENTAIRES.DAT"
               ORGANIZATION        INDEXED
               ACCESS MODE         RANDOM
               RECORD KEY          COMM-NOM-RECETTE.


       DATA DIVISION.
       FILE SECTION.
       FD RECETTES-IDX.
       01  RECETTE.
           05  RECETTE-NOM                 PIC X(50).
           05  RECETTE-DESCRIPTION.
               10  RECETTE-DESCRIPTION-LIGNE1       PIC X(60).
               10  RECETTE-DESCRIPTION-LIGNE2       PIC X(60).
               10  RECETTE-DESCRIPTION-LIGNE3       PIC X(60).
           05  RECETTE-NBRE-INGREDIENTS    PIC 99.
           05  RECETTE-TABLE-INGREDIENTS OCCURS 1 TO 99
                                  DEPENDING ON RECETTE-NBRE-INGREDIENTS.
               10  RCI-QUANTITE            PIC 9999.
               10  RCI-UNITE-MESURE        PIC A(3).
               10  RCI-NOM                 PIC X(30).
           05  RECETTE-PREPARATION.
               10  RECETTE-PREP-LIGNE1     PIC X(67).
               10  RECETTE-PREP-LIGNE2     PIC X(67).
               10  RECETTE-PREP-LIGNE3     PIC X(67).
               10  RECETTE-PREP-LIGNE4     PIC X(67).
               10  RECETTE-PREP-LIGNE5     PIC X(67).
               10  RECETTE-PREP-LIGNE6     PIC X(67).
               10  RECETTE-PREP-LIGNE7     PIC X(67).
               10  RECETTE-PREP-LIGNE8     PIC X(67).
               10  RECETTE-PREP-LIGNE9     PIC X(67).
               10  RECETTE-PREP-LIGNE10    PIC X(67).
               10  RECETTE-PREP-LIGNE11    PIC X(67).
               10  RECETTE-PREP-LIGNE12    PIC X(67).
               10  RECETTE-PREP-LIGNE13    PIC X(67).
               10  RECETTE-PREP-LIGNE14    PIC X(67).
               10  RECETTE-PREP-LIGNE15    PIC X(67).
               10  RECETTE-PREP-LIGNE16    PIC X(67).
               10  RECETTE-PREP-LIGNE17    PIC X(67).
               10  RECETTE-PREP-LIGNE18    PIC X(67).
               10  RECETTE-PREP-LIGNE19    PIC X(67).
               10  RECETTE-PREP-LIGNE20    PIC X(67).
           05  RECETTE-SYSTEME-UNITE       PIC A.
       FD COMMENTAIRES-IDX.
       01  COMMENTAIRE.
           05 COMM-NOM-RECETTE      PIC X(80).
           05 COMM-NBRE             PIC 99.
           05 COMM-TABLE OCCURS 1 TO 99
                              DEPENDING ON COMM-NBRE.
               10  RECETTE-COMM-LIGNE1     PIC X(67).
               10  RECETTE-COMM-LIGNE2     PIC X(67).
               10  RECETTE-COMM-LIGNE3     PIC X(67).
               10  RECETTE-COMM-LIGNE4     PIC X(67).
               10  RECETTE-COMM-LIGNE5     PIC X(67).


       WORKING-STORAGE SECTION.
      ******************************************************************
      * CHOIX DU MENU PRINCIPAL
      ******************************************************************
       01  W-CHOIX-MENU-PRINCIPAL.
           05  W-CHOIX-PRINCIPAL                PIC X   VALUE SPACE.
               88  AFFICHER                     VALUE "1".
               88  SAISIR                       VALUE "2".
               88  MODIFIER                     VALUE "3".
               88  SUPPRIMER                    VALUE "4".
               88  QUITTER                      VALUE "5".
               88  COPIER-FICHIER               VALUE "6".
               88  W-CHOIX-PRINCIPAL-VALIDE
                        VALUE "1" "2" "3" "4" "5" "6".

      ******************************************************************
      * VARIABLE DE MESSAGE
      ******************************************************************
       01 W-MSG                        PIC X(150) VALUE SPACE.

      ******************************************************************
      * TABLEAU DE SAUVEGARDE TEMPORAIRE DES INGREDIENTS LORS DE LA
      * SAISIE D'UNE RECETTE.
      ******************************************************************
       01 W-TABLE-INGREDIENTS OCCURS 99.
           05  W-QUANTITE            PIC 9999 VALUE 0.
           05  W-UNITE-MESURE        PIC A(3).
           05  W-NOM-INGR            PIC X(30).

      ******************************************************************
      * VARIABLE UTILISÉE COMME INDICE DE TABLEAU DANS LE PERFORM
      * VARYING...
      ******************************************************************
       01  W-I                     PIC 999.
       01  W-J                     PIC 999.

      ******************************************************************
      *VARIABLES QUI SERVENT A COMPTER LE NOMBRE D'INGREDIENTS LORS DE
      *LA SAISIE D'UNE RECETTE
      ******************************************************************
       01  W-COMPTEUR-INGR         PIC 99 VALUE 1.

      ******************************************************************
      *VARIABLE DE NUMÉRO DE LIGNE QUI EST INCRÉMENTÉE DANS UNE BOUCLE
      *LORS DE LA SAISIE DES INGRÉDIENTS ET DES ÉTAPES DE PREPARATION
      ******************************************************************
       01 W-NO-LIGNE               PIC 99 VALUE 0.
       01 W-NO-LIGNE2              PIC 99 VALUE 0.

      ******************************************************************
      *TABLEAU QUI ACCUMULE 10 NOMS DE RECETTES LORS LA LECTURE DANS LE
      *FICHIER (LA LECTURE ET L'AFFICHAGE SE FAIT À COUPS DE 10)
      ******************************************************************
       01  W-TABLE-DIX-RECETTE OCCURS 10.
           05  W-NOM-RECETTE1  PIC  X(50).
           05  W-NOM-RECETTE2  PIC  X(50).
           05  W-NOM-RECETTE3  PIC  X(50).
           05  W-NOM-RECETTE4  PIC  X(50).
           05  W-NOM-RECETTE5  PIC  X(50).
           05  W-NOM-RECETTE6  PIC  X(50).
           05  W-NOM-RECETTE7  PIC  X(50).
           05  W-NOM-RECETTE8  PIC  X(50).
           05  W-NOM-RECETTE9  PIC  X(50).
           05  W-NOM-RECETTE10 PIC  X(50).

      ******************************************************************
      *BOOLEEN INDICATEUR DE FIN DE FICHIER
      ******************************************************************
       01 W-IND-FIN-FICHIER        PIC 9 VALUE 0.

      ******************************************************************
      *BOOLEAN MIS A 1 APRES VERIFICATION QUE LE NOM DE RECETTE EST
      *DEJA UTILISE
      ******************************************************************
       01  W-NOM-DEJA-UTILISE     PIC 9 VALUE 0.

      ******************************************************************
      *VARIABLE DE STOCKAGE TEMPORAIRE DU NOM DE LA RECETTE
      ******************************************************************
       01 W-RECETTE-NOM            PIC X(50).

      ******************************************************************
      *VARIABLE DE CHOIX D'UNE RECETTE LORS DE L'AFFICHAGE
      ******************************************************************
       01 W-CHOIX-RECETTE          PIC X.

      ******************************************************************
      *VARIABLES QUI STOCKENT LES 9 NOMS DE RECETTES AFFICHEES LORS DE
      *L'AFFICHAGE DES RECETTES
      ******************************************************************
       01  W-RECETTES-TEMP.
           05 W-RECETTE1               PIC X(50) VALUE SPACE.
           05 W-RECETTE2               PIC X(50) VALUE SPACE.
           05 W-RECETTE3               PIC X(50) VALUE SPACE.
           05 W-RECETTE4               PIC X(50) VALUE SPACE.
           05 W-RECETTE5               PIC X(50) VALUE SPACE.
           05 W-RECETTE6               PIC X(50) VALUE SPACE.
           05 W-RECETTE7               PIC X(50) VALUE SPACE.
           05 W-RECETTE8               PIC X(50) VALUE SPACE.
           05 W-RECETTE9               PIC X(50) VALUE SPACE.

      ******************************************************************
      * VARIABLE VIDE SERVANT JUSTE A ACCEPT POUR CHANGER D'ECRAN
      ******************************************************************
       01  W-ENTREE                PIC A.

      ******************************************************************
      *VARIABLE BOOLEENE QUI INDIQUE A QUEL ECRAN DE L'AFFICHAGE ON SE
      *TROUVE
      ******************************************************************
       01 W-ECRAN-MAIN            PIC 9.
       01 W-ECRAN-ING             PIC 9.
       01 W-ECRAN-PREP            PIC 9.

      ******************************************************************
      *CHOIX DU MENU COMMENTAIRES
      ******************************************************************
       01  W-CHOIX-COMMENTAIRE     PIC X VALUE SPACE.

       SCREEN SECTION.
       01  FOND-ECRAN.
           05          BLANK SCREEN
                       BACKGROUND-COLOR 7
                       FOREGROUND-COLOR 0.
           05          BACKGROUND-COLOR 3
                       LINE 1 COL 1
                       PIC X(80).
           05          LINE 25 COL 1
                       BLANK LINE
                       BACKGROUND-COLOR 3
                       FOREGROUND-COLOR 15.
           05          LINE 1 COL 30
                       BACKGROUND-COLOR 3
                       FOREGROUND-COLOR 15
                       VALUE "Livre de recette COBOL".

       01  ECRAN-MENU.
           05          LINE 3 COL 34
                       VALUE "Menu principal".
           05          LINE 8 COL 25
                       VALUE "1) Afficher une recette".
           05          LINE 10 COL 25
                       VALUE "2) Saisir une recette".
           05          LINE 12 COL 25
                       VALUE "3) Modifier une recette".
           05          LINE 14 COL 25
                       VALUE "4) Supprimer une recette".
           05          LINE 16 COL 25
                       VALUE "5) Quitter le programme".
           05          LINE 18 COL 25
                       VALUE "6) Copier le fichier de recettes".
           05          LINE 21 COL 25
                       VALUE "Votre choix:".
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
           05          LINE 21 COL 38
                       PIC X.
       01  ECRAN-SAISIE-1.
           05          LINE 3 COL 5
                       VALUE "Nom de la recette:"
                       FOREGROUND-COLOR 1.
           05          LINE 6 COL 5
                       VALUE "Description:"
                       FOREGROUND-COLOR 1.
           05          LINE 7 COL 5
       VALUE "(3 lignes max. Appuyer sur ENTER pour ajouter une ligne)"
                       FOREGROUND-COLOR 1.
           05          LINE 15 COL 5
        VALUE "Systeme d'unite de mesure (U pour US, M pour metrique):"
                       FOREGROUND-COLOR 1.
       01  ECRAN-SAISIE-RESAISIR-SYSTEME.
           05          LINE 3 COL 5
                       VALUE "Nom de la recette:"
                       FOREGROUND-COLOR 1.
           05          LINE 3 COL 25
                       PIC X(50) FROM RECETTE-NOM.
           05          LINE 6 COL 5
                       VALUE "Description:"
                       FOREGROUND-COLOR 1.
           05          LINE 7 COL 5
        VALUE "(3 lignes max. Appuyer sur ENTER pour ajouter une ligne)"
                       FOREGROUND-COLOR 1.
           05          line 9 col 7 pic X(60)
                       FROM RECETTE-DESCRIPTION-LIGNE1.
           05          line 10 col 7 pic X(60)
                       FROM RECETTE-DESCRIPTION-LIGNE2.
           05          line 11 col 7 pic X(60)
                       FROM RECETTE-DESCRIPTION-LIGNE3.
           05          LINE 15 COL 5
        VALUE "Systeme d'unite de mesure (U pour US, M pour metrique):"
                       FOREGROUND-COLOR 1.
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-SAISIE-INGREDIENT-METR.
           05          LINE 3 COL 7 VALUE "Quantite".
           05          LINE 3 COL 18 VALUE "Unite".
           05          LINE 3 COL 30 VALUE "Ingredient".
           05          LINE 3 COL 62 VALUE "Unites possibles:".
           05          LINE 5 COL 63 VALUE "Grammes (g)".
           05          LINE 6 COL 63 VALUE "Kilogrammes (kg)".
           05          LINE 7 COL 63 VALUE "Millilitre (ml)".
           05          LINE 8 COL 63 VALUE "Litre (l)".
           05          LINE 9 COL 63 VALUE "Aucune (x)".
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-SAISIE-INGREDIENT-US.
           05          LINE 3 COL 7 VALUE "Quantite".
           05          LINE 3 COL 18 VALUE "Unite".
           05          LINE 3 COL 30 VALUE "Ingredient".
           05          LINE 3 COL 62 VALUE "Unites possibles:".
           05          LINE 5 COL 63 VALUE "Cuil. a the (ct)".
           05          LINE 6 COL 63 VALUE "Cuil. a soupe (cp)".
           05          LINE 7 COL 63 VALUE "Once (oz)".
           05          LINE 8 COL 63 VALUE "Tasse (t)".
           05          LINE 9 COL 63 VALUE "Aucune (x)".
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-SAISIE-PREPARATION.
           05          LINE 3 COL 5
                       VALUE "Preparation:"
                       FOREGROUND-COLOR 1.
           05          LINE 3 COL 18
       VALUE "(20 lignes max. Appuyer sur ENTER pour ajouter une ligne)"
                       FOREGROUND-COLOR 1.
       01  ECRAN-AFFICHAGE-RECETTES.
           05          LINE 3  COL 7 VALUE "1.".
           05          LINE 5  COL 7 VALUE "2.".
           05          LINE 7  COL 7 VALUE "3.".
           05          LINE 9  COL 7 VALUE "4.".
           05          LINE 11 COL 7 VALUE "5.".
           05          LINE 13 COL 7 VALUE "6.".
           05          LINE 15 COL 7 VALUE "7.".
           05          LINE 17 COL 7 VALUE "8.".
           05          LINE 19 COL 7 VALUE "9.".
           05          LINE 22 COL 15 VALUE
                    "Entrer le numero d'une recette pour la choisir:".
           05          LINE 22 COL 65 PIC XX TO W-CHOIX-RECETTE.
       01  ECRAN-AFFICHAGE-RECETTE-MAIN.
           05          LINE 3 COL 5
                       VALUE "Nom de la recette:"
                       FOREGROUND-COLOR 1.
           05          LINE 3 COL 25
                       PIC X(50) FROM RECETTE-NOM.
           05          LINE 6 COL 5
                       VALUE "Description:"
                       FOREGROUND-COLOR 1.
           05          line 9 col 7 pic X(60)
                       FROM RECETTE-DESCRIPTION-LIGNE1.
           05          line 10 col 7 pic X(60)
                       FROM RECETTE-DESCRIPTION-LIGNE2.
           05          line 11 col 7 pic X(60)
                       FROM RECETTE-DESCRIPTION-LIGNE3.
           05          LINE 15 COL 5
        VALUE "Systeme d'unite de mesure (U pour US, M pour metrique):"
                       FOREGROUND-COLOR 1.
           05          LINE 15 COL 62
                       PIC A FROM RECETTE-SYSTEME-UNITE
                       FOREGROUND-COLOR 1.
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-AFFICHAGE-RECETTE-INGR.
           05          LINE 3 COL 7 VALUE "Quantite".
           05          LINE 3 COL 18 VALUE "Unite".
           05          LINE 3 COL 30 VALUE "Ingredient".
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-SAISIE-RECETTE-PREP.
           05          LINE 3 COL 5
                       VALUE "Preparation:"
                       FOREGROUND-COLOR 1.
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-MENU-COMMENTAIRES.
           05           LINE 8 COL 25
                       VALUE "1) Ajouter un commentaire".
           05          LINE 10 COL 25
                       VALUE "2) Consulter les commentaires".
           05          LINE 12 COL 25
                       VALUE "3) Revenir a la recette".
           05          LINE 16 COL 25
                       VALUE "Votre choix:".
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.
       01  ECRAN-SAISIE-COMMENTAIRE.
           05          LINE 3 COL 5
                       VALUE "Commentaire:"
                       FOREGROUND-COLOR 1.
           05          LINE 3 COL 18
       VALUE "(5 lignes max. Appuyer sur ENTER pour ajouter une ligne)"
                       FOREGROUND-COLOR 1.
       01  ECRAN-AFFICHAGE-COMMENTAIRE.
           05          LINE 3 COL 5
                       VALUE "Commentaire:"
                       FOREGROUND-COLOR 1.
       01  ECRAN-MODIFIER-1.
          05          LINE 3 COL 5
                      VALUE "NOM DE LA RECETTE:"
                      FOREGROUND-COLOR 1.
          05          LINE 3 COL 25
                      PIC X(50) FROM RECETTE-NOM.
          05          LINE 6 COL 5
                      VALUE "DESCRIPTION:"
                      FOREGROUND-COLOR 1.
          05          LINE 7 COL 5
       VALUE "(3 LIGNES MAX. APPUYER SUR ENTER POUR AJOUTER UNE LIGNE)"
                      FOREGROUND-COLOR 1.
          05          LINE 9 COL 7 PIC X(60)
                      FROM RECETTE-DESCRIPTION-LIGNE1.

          05          LINE 10 COL 7 PIC X(60)
                      USING RECETTE-DESCRIPTION-LIGNE2.
          05          LINE 11 COL 7 PIC X(60)
                      USING RECETTE-DESCRIPTION-LIGNE3.
          05          LINE 15 COL 5
       VALUE "SYSTEME D'UNITE DE MESURE (U POUR US, M POUR METRIQUE):"
                       FOREGROUND-COLOR 1.
           05          LINE 15 COL 62
                       PIC A FROM RECETTE-SYSTEME-UNITE
                       FOREGROUND-COLOR 1.
           05          LINE 25 COL 1
                       BACKGROUND-COLOR 3
                       PIC X(80) FROM W-MSG.

       PROCEDURE DIVISION.
      *Boucle principale du programme.
       PERFORM UNTIL QUITTER
           PERFORM UNTIL W-CHOIX-PRINCIPAL-VALIDE
               PERFORM 20000-MENU-PRINCIPAL
               IF W-CHOIX-PRINCIPAL-VALIDE THEN
                   EVALUATE TRUE
                   WHEN AFFICHER
                       PERFORM 30000-AFFICHER
                       MOVE SPACE TO W-MSG
                   WHEN SAISIR
                       PERFORM 40000-SAISIR
                       MOVE SPACE TO W-MSG
                   WHEN MODIFIER
                       PERFORM 50000-MODIFIER
                       MOVE SPACE TO W-MSG
                   WHEN SUPPRIMER
                       PERFORM 60000-SUPPRIMER
                       MOVE SPACE TO W-MSG
                   WHEN COPIER-FICHIER
                    PERFORM 23000-COPIER-FICHIER
                       MOVE SPACE TO W-MSG
                   END-EVALUATE
               ELSE
                  MOVE "Choix invalide" TO W-MSG
               END-IF
           END-PERFORM
       END-PERFORM.

       DISPLAY FOND-ECRAN.
       MOVE "Programme termine" TO W-MSG.
       DISPLAY W-MSG AT 2501.
       STOP RUN.

      ******************************************************************
       10000-INITIALISER.
      *Ce paragraphe intialise certaines valeurs.
       MOVE 0 TO W-IND-FIN-FICHIER.
       MOVE 0 TO W-NOM-DEJA-UTILISE.
       MOVE 0 TO W-CHOIX-PRINCIPAL.
       MOVE SPACE TO W-CHOIX-COMMENTAIRE.
       MOVE SPACE TO W-CHOIX-RECETTE.
       INITIALIZE RECETTE.
       DISPLAY FOND-ECRAN.

      ******************************************************************
       20000-MENU-PRINCIPAL.
      *Ce paragraphe affiche le menu principal
       PERFORM 10000-INITIALISER.
       DISPLAY ECRAN-MENU.
       ACCEPT W-CHOIX-PRINCIPAL.

      ******************************************************************
       30000-AFFICHER.
       PERFORM 10000-INITIALISER.
       OPEN INPUT RECETTES-IDX.
       PERFORM UNTIL W-CHOIX-RECETTE = "0" OR "1" OR "2" OR "3" OR "4"
       OR "5" OR "6" OR "7" OR "8" OR "9" OR W-IND-FIN-FICHIER = 1
           DISPLAY FOND-ECRAN
           MOVE
           "Prochaines recettes -> ENTREE     Quitter -> 0" TO W-MSG
           DISPLAY W-MSG AT 2501
           DISPLAY ECRAN-AFFICHAGE-RECETTES
           INITIALIZE W-RECETTES-TEMP
           MOVE 3 TO W-NO-LIGNE
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 9 OR
           W-IND-FIN-FICHIER = 1
               READ RECETTES-IDX NEXT RECORD INTO RECETTE
               AT END MOVE 1 TO W-IND-FIN-FICHIER
               END-READ
               IF W-IND-FIN-FICHIER = 0
               THEN
                   EVALUATE TRUE
                       WHEN W-RECETTE1 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE1
                       WHEN W-RECETTE2 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE2
                       WHEN W-RECETTE3 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE3
                       WHEN W-RECETTE4 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE4
                       WHEN W-RECETTE5 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE5
                       WHEN W-RECETTE6 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE6
                       WHEN W-RECETTE7 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE7
                       WHEN W-RECETTE8 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE8
                       WHEN W-RECETTE9 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE9
                   END-EVALUATE
                   DISPLAY RECETTE-NOM AT LINE W-NO-LIGNE COL 11
                   ADD 2 TO W-NO-LIGNE
               END-IF
           END-PERFORM
           ACCEPT ECRAN-AFFICHAGE-RECETTES
       END-PERFORM.
       CLOSE RECETTES-IDX.
       MOVE SPACE TO W-MSG.
       IF W-CHOIX-RECETTE <> SPACE AND 0
       THEN
           PERFORM 12000-AFFICHER-RECETTE
       ELSE IF W-CHOIX-RECETTE <> 0
           DISPLAY FOND-ECRAN
           DISPLAY "Aucune autre recette a afficher!" AT 2501
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 2580
       END-IF.

      ******************************************************************
       40000-SAISIR.
      *Ce paragraphe gère la saisie d'une nouvelle recette
       PERFORM 10000-INITIALISER.
       INITIALIZE W-RECETTE-NOM.
       DISPLAY ECRAN-SAISIE-1.
       PERFORM 110000-ACCEPTER-VERIFIER-NOM.
       ACCEPT RECETTE-DESCRIPTION-LIGNE1 AT 0907.
       ACCEPT RECETTE-DESCRIPTION-LIGNE2 AT 1007.
       ACCEPT RECETTE-DESCRIPTION-LIGNE3 AT 1107.
       ACCEPT RECETTE-SYSTEME-UNITE AT 1562.

       PERFORM UNTIL
       RECETTE-SYSTEME-UNITE ="U" OR  ="M" OR ="u" OR ="m"
           MOVE "Systeme d'unite invalide" TO W-MSG
           DISPLAY ECRAN-SAISIE-RESAISIR-SYSTEME
           ACCEPT RECETTE-SYSTEME-UNITE AT 1562
       END-PERFORM.

       IF RECETTE-SYSTEME-UNITE = "M" OR "m"
       THEN
           PERFORM 70000-SAISIE-INGREDIENTS-METR
       ELSE IF RECETTE-SYSTEME-UNITE = "U" OR "u"
       THEN
           PERFORM 80000-SAISIE-INGREDIENTS-US
       END-IF.

       PERFORM 90000-SAISIE-PREPARATION.
       PERFORM 100000-ECRIRE-RECETTE-FICHIER.
       MOVE
       "Recette enregistree. Appuyer sur ENTER pour revenir au menu."
           TO W-MSG.
       DISPLAY FOND-ECRAN.
       DISPLAY W-MSG AT 2501.
       ACCEPT W-MSG AT 2580.

      ******************************************************************
       50000-MODIFIER.
       PERFORM 10000-INITIALISER.
       OPEN INPUT RECETTES-IDX.
       PERFORM UNTIL W-CHOIX-RECETTE = "0" OR "1" OR "2" OR "3" OR "4"
       OR "5" OR "6" OR "7" OR "8" OR "9" OR W-IND-FIN-FICHIER = 1
           DISPLAY FOND-ECRAN
           MOVE
           "PROCHAINES RECETTES -> ENTREE     QUITTER -> 0" TO W-MSG
           DISPLAY W-MSG AT 2501
           DISPLAY ECRAN-AFFICHAGE-RECETTES
           INITIALIZE W-RECETTES-TEMP
           MOVE 3 TO W-NO-LIGNE
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 9 OR
           W-IND-FIN-FICHIER = 1
               READ RECETTES-IDX NEXT RECORD INTO RECETTE
               AT END MOVE 1 TO W-IND-FIN-FICHIER
               END-READ
               IF W-IND-FIN-FICHIER = 0
               THEN
                   EVALUATE TRUE
                       WHEN W-RECETTE1 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE1
                       WHEN W-RECETTE2 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE2
                       WHEN W-RECETTE3 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE3
                       WHEN W-RECETTE4 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE4
                       WHEN W-RECETTE5 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE5
                       WHEN W-RECETTE6 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE6
                       WHEN W-RECETTE7 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE7
                       WHEN W-RECETTE8 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE8
                       WHEN W-RECETTE9 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE9
                   END-EVALUATE
                   DISPLAY RECETTE-NOM AT LINE W-NO-LIGNE COL 11
                   ADD 2 TO W-NO-LIGNE
               END-IF
           END-PERFORM
           ACCEPT ECRAN-AFFICHAGE-RECETTES
       END-PERFORM.
       CLOSE RECETTES-IDX.
       MOVE SPACE TO W-MSG.
       IF W-CHOIX-RECETTE <> SPACE AND 0
       THEN
           PERFORM 18000-MODIFIER-RECETTE
           DISPLAY FOND-ECRAN
           PERFORM 100000-ECRIRE-RECETTE-FICHIER
       ELSE IF W-CHOIX-RECETTE <> 0
           DISPLAY FOND-ECRAN
           DISPLAY "Aucune autre recette a afficher!" AT 2501
           ACCEPT W-ENTREE AT 2580
       END-IF.

      ******************************************************************
       60000-SUPPRIMER.
       PERFORM 10000-INITIALISER.
       OPEN INPUT RECETTES-IDX.
       PERFORM UNTIL W-CHOIX-RECETTE = "0" OR "1" OR "2" OR "3" OR "4"
       OR "5" OR "6" OR "7" OR "8" OR "9" OR W-IND-FIN-FICHIER = 1
           DISPLAY FOND-ECRAN
           MOVE
           "Prochaines recettes -> ENTREE     Quitter -> 0" TO W-MSG
           DISPLAY W-MSG AT 2501
           DISPLAY ECRAN-AFFICHAGE-RECETTES
           INITIALIZE W-RECETTES-TEMP
           MOVE 3 TO W-NO-LIGNE
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 9 OR
           W-IND-FIN-FICHIER = 1
               READ RECETTES-IDX NEXT RECORD INTO RECETTE
               AT END MOVE 1 TO W-IND-FIN-FICHIER
               END-READ
               IF W-IND-FIN-FICHIER = 0
               THEN
                   EVALUATE TRUE
                       WHEN W-RECETTE1 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE1
                       WHEN W-RECETTE2 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE2
                       WHEN W-RECETTE3 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE3
                       WHEN W-RECETTE4 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE4
                       WHEN W-RECETTE5 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE5
                       WHEN W-RECETTE6 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE6
                       WHEN W-RECETTE7 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE7
                       WHEN W-RECETTE8 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE8
                       WHEN W-RECETTE9 = SPACE
                           MOVE RECETTE-NOM TO W-RECETTE9
                   END-EVALUATE
                   DISPLAY RECETTE-NOM AT LINE W-NO-LIGNE COL 11
                   ADD 2 TO W-NO-LIGNE
               END-IF
           END-PERFORM
           ACCEPT ECRAN-AFFICHAGE-RECETTES
       END-PERFORM.
       CLOSE RECETTES-IDX.
       MOVE SPACE TO W-MSG.
       IF W-CHOIX-RECETTE <> SPACE AND 0
       THEN
           PERFORM 22000-SUPPRIMER-RECETTE
       ELSE IF W-CHOIX-RECETTE <> 0
           DISPLAY FOND-ECRAN
           DISPLAY "Aucune autre recette a afficher!" AT 2501
           ACCEPT W-ENTREE AT 2580
       END-IF.
      ******************************************************************
       70000-SAISIE-INGREDIENTS-METR.
      *Ce pragraphe sert a saisir les ingrédients d'une recette en
      *mesures métriques

      *REINITIALISER LES VALEURS TU TABLEAU TEMPORAIRE D'INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 99
           INITIALIZE W-TABLE-INGREDIENTS(W-I)



       END-PERFORM.
       MOVE 1 TO W-COMPTEUR-INGR.
       MOVE 5 TO W-NO-LIGNE.
       MOVE "Entrer 9999 dans la quantite lorsque termine" TO W-MSG.
       DISPLAY FOND-ECRAN.
      *SAISIE DES INGREDIENTS JUSQU'A CE QUE LE USER ENTRE 9999
       PERFORM UNTIL W-QUANTITE(W-COMPTEUR-INGR) = 9999
           DISPLAY ECRAN-SAISIE-INGREDIENT-METR
           DISPLAY W-COMPTEUR-INGR AT LINE W-NO-LIGNE COL 03
           DISPLAY "." AT LINE W-NO-LIGNE COL 05
           ACCEPT W-QUANTITE(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 07
           IF W-QUANTITE(W-COMPTEUR-INGR) <> 9999
           THEN
               PERFORM UNTIL W-UNITE-MESURE(W-COMPTEUR-INGR) =
               "g" OR "G" OR "kg" OR "KG" OR "ml" OR "ML" OR "l" OR "L"
               OR "x" OR "X"
                   DISPLAY W-MSG AT 2501
                   ACCEPT W-UNITE-MESURE(W-COMPTEUR-INGR)
                                           AT LINE W-NO-LIGNE COL 18
               MOVE "Choix d'unite invalide, entrer g, kg, ml ou l ou x"
                                                               TO W-MSG
               END-PERFORM
           MOVE "Entrer 9999 dans la quantite lorsque termine" TO W-MSG
           DISPLAY W-MSG AT 2501
           ACCEPT W-NOM-INGR(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 30
           ADD 1 TO W-COMPTEUR-INGR
           ADD 2 TO W-NO-LIGNE
           END-IF
           IF W-COMPTEUR-INGR = 11 OR 21 OR 31 OR 41 OR 51 OR 61 OR 71
           OR 81 OR 91
           THEN
               DISPLAY FOND-ECRAN
               MOVE 5 TO W-NO-LIGNE
           END-IF
       END-PERFORM.

      ******************************************************************
       80000-SAISIE-INGREDIENTS-US.
      *Ce pragraphe sert a saisir les ingrédients d'une recette
      *en mesures US

      *REINITIALISER LES VALEURS DU TABLEAU TEMPORAIRE D'INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 99
           MOVE 0 TO W-QUANTITE(W-I)
           MOVE SPACE TO W-UNITE-MESURE(W-I)
           MOVE SPACE TO W-NOM-INGR(W-I)
       END-PERFORM.
       MOVE 5 TO W-NO-LIGNE.
       MOVE 1 TO W-COMPTEUR-INGR.
       MOVE "Entrer 9999 dans la quantite lorsque termine" TO W-MSG.
       DISPLAY FOND-ECRAN.
      *SAISIE DES INGREDIENTS JUSQU'A CE QUE LE USER ENTRE 9999
       PERFORM UNTIL W-QUANTITE(W-COMPTEUR-INGR) = 9999
           DISPLAY ECRAN-SAISIE-INGREDIENT-US
           DISPLAY W-COMPTEUR-INGR AT LINE W-NO-LIGNE COL 03
           DISPLAY "." AT LINE W-NO-LIGNE COL 05
           ACCEPT W-QUANTITE(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 07
           IF W-QUANTITE(W-COMPTEUR-INGR) <> 9999
           THEN
               PERFORM UNTIL W-UNITE-MESURE(W-COMPTEUR-INGR) =
               "ct" OR "CT" OR "cp" OR "CP" OR "oz" OR "OZ" OR "t"
               OR "T" OR "x" OR "X"
                   DISPLAY W-MSG AT 2501
                   ACCEPT W-UNITE-MESURE(W-COMPTEUR-INGR)
                                           AT LINE W-NO-LIGNE COL 18
               MOVE "Choix d'unite invalide, entrer ct, cp, oz, t ou x"
                                                       TO W-MSG
               END-PERFORM
           MOVE "Entrer 9999 dans la quantite lorsque termine" TO W-MSG
           DISPLAY W-MSG AT 2501
           ACCEPT W-NOM-INGR(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 30
           ADD 1 TO W-COMPTEUR-INGR
           ADD 2 TO W-NO-LIGNE
           END-IF
           IF W-COMPTEUR-INGR = 11 OR 21 OR 31 OR 41 OR 51 OR 61 OR 71
           OR 81 OR 91
           THEN
               DISPLAY FOND-ECRAN
               MOVE 5 TO W-NO-LIGNE
           END-IF
       END-PERFORM.

      ******************************************************************
       90000-SAISIE-PREPARATION.
      *Ce paragraphe sert à saisir les étapes de préparation d'une
      *recette
       INITIALIZE RECETTE-PREPARATION.
       DISPLAY FOND-ECRAN.
       DISPLAY ECRAN-SAISIE-PREPARATION.
       MOVE "Appuyer sur ENTREE sur une ligne vide lorsque termine"
                                                           TO W-MSG.
      *Répétition de saisie (jusqu'à 20 lignes) des étapes de
      *préparation. Le user peut terminer après en faisant ENTER sur
      *une ligne vide.
       DISPLAY W-MSG AT 2501.
       MOVE 5 TO W-NO-LIGNE.
       DISPLAY "1." AT LINE W-NO-LIGNE COL 05.
       ACCEPT RECETTE-PREP-LIGNE1 AT LINE W-NO-LIGNE COL 09
       IF RECETTE-PREP-LIGNE1 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "2." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE2 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE2 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "3." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE3 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE3 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "4." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE4 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE4 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "5." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE5 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE5 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "6." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE6 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE6 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "7." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE7 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE7 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "8." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE8 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE8 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "9." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE9 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE9 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "10." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE10 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE10 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "11." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE11 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE11 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "12." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE12 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE12 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "13." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE13 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE13 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "14." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE14 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE14 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "15." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE15 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE15 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "16." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE16 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE16 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "17." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE17 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE17 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "18." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE18 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE18 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "19." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE19 AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-PREP-LIGNE19 <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "20." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-PREP-LIGNE20 AT LINE W-NO-LIGNE COL 09.

      ******************************************************************
      *Ce paragraphe sert a écrire une recette dans le fichier
       100000-ECRIRE-RECETTE-FICHIER.
       SUBTRACT 1 FROM W-COMPTEUR-INGR.
       MOVE W-COMPTEUR-INGR TO RECETTE-NBRE-INGREDIENTS.
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-COMPTEUR-INGR
           MOVE W-TABLE-INGREDIENTS(W-I)
                                   TO RECETTE-TABLE-INGREDIENTS(W-I)
       END-PERFORM.
       OPEN I-O RECETTES-IDX
       WRITE RECETTE.
       CLOSE RECETTES-IDX.

      ******************************************************************
       110000-ACCEPTER-VERIFIER-NOM.

       ACCEPT W-RECETTE-NOM AT 0325.

      *Le nom de recette constitue la cle du fichier indexé. Verifier
      *que le nom n'est pas déjà utilisé dans le fichier.
       MOVE W-RECETTE-NOM TO RECETTE-NOM.
       OPEN I-O RECETTES-IDX.
       READ RECETTES-IDX KEY IS RECETTE-NOM
       INVALID KEY MOVE 0 TO W-NOM-DEJA-UTILISE
       NOT INVALID KEY MOVE 1 TO W-NOM-DEJA-UTILISE
       END-READ.
       CLOSE RECETTES-IDX.

       IF W-NOM-DEJA-UTILISE = 1
       THEN
         MOVE "Ce nom de recette est existe deja. Choisir un autre nom."
           TO W-MSG
         DISPLAY W-MSG AT 2501
       PERFORM 110000-ACCEPTER-VERIFIER-NOM
       END-IF.

       INITIALIZE RECETTE.
       MOVE W-RECETTE-NOM TO RECETTE-NOM.

      ******************************************************************
       12000-AFFICHER-RECETTE.
       OPEN INPUT RECETTES-IDX.
           EVALUATE TRUE
               WHEN W-CHOIX-RECETTE = 1
                   MOVE W-RECETTE1 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 2
                   MOVE W-RECETTE2 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 3
                   MOVE W-RECETTE3 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 4
                   MOVE W-RECETTE4 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 5
                   MOVE W-RECETTE5 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 6
                   MOVE W-RECETTE6 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 7
                   MOVE W-RECETTE7 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 8
                   MOVE W-RECETTE8 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 9
                   MOVE W-RECETTE9 TO RECETTE-NOM
           END-EVALUATE.
       READ RECETTES-IDX KEY IS RECETTE-NOM.
       CLOSE RECETTES-IDX.
       DISPLAY FOND-ECRAN.
       MOVE SPACE TO W-ENTREE.
       MOVE 1 TO W-ECRAN-MAIN.
      *    ON MET L'AFFICHAGE DES ÉCRANS MAIN, INGREDIENTS ET
      *    PREPARATION DANS UN EVALUATE ET DANS UNE BOUCLE
      *    DE MANIÈRE A POUVOIR PASSER DE 1 À L'AUTRE COMME ON VEUT
       PERFORM UNTIL W-ENTREE = "Q" or "q"
           EVALUATE TRUE
               WHEN W-ECRAN-MAIN = 1
                   MOVE SPACE TO W-ENTREE
                   MOVE 0 TO W-ECRAN-MAIN
                   DISPLAY FOND-ECRAN
                   MOVE
       "ENTREE -> Page suivante     Q -> Quitter"
                                                               TO W-MSG
                   DISPLAY ECRAN-AFFICHAGE-RECETTE-MAIN
                   ACCEPT W-ENTREE AT 2580
                   IF W-ENTREE = SPACE
                   THEN
                       MOVE 1 TO W-ECRAN-ING
                   END-IF


               WHEN W-ECRAN-ING = 1
                   MOVE SPACE TO W-ENTREE
                   MOVE 0 TO W-ECRAN-ING
                   DISPLAY FOND-ECRAN
                   MOVE
       "ENTREE -> Page suivante    R -> Page precedente    Q -> Quitter"
                                                     TO W-MSG
                   DISPLAY ECRAN-AFFICHAGE-RECETTE-INGR
                   PERFORM 13000-AFFICHER-INGREDIENTS
                   ACCEPT W-ENTREE AT 2580
                   IF W-ENTREE = SPACE
                   THEN
                       MOVE 1 TO W-ECRAN-PREP
                   ELSE IF W-ENTREE = "R" or "r"
                   THEN
                       MOVE 1 TO W-ECRAN-MAIN
                   END-IF


                WHEN W-ECRAN-PREP = 1
                   PERFORM UNTIL W-ENTREE = "R" or "r" OR "Q" or "q"
                       MOVE SPACE TO W-ENTREE
                       MOVE 0 TO W-ECRAN-PREP
                       DISPLAY FOND-ECRAN
                       MOVE
       "ENTREE -> Commentaires    R -> Page precedente    Q -> Quitter"
                                                               TO W-MSG
                       DISPLAY ECRAN-SAISIE-RECETTE-PREP
                       PERFORM 14000-AFFICHER-PREP
                       ACCEPT W-ENTREE AT 2580
                       IF W-ENTREE = "R" or "r"
                       THEN
                           MOVE 1 TO W-ECRAN-ING
                       ELSE IF W-ENTREE = SPACE
                       THEN
                           PERFORM 15000-MENU-COMMENTAIRES
                           MOVE 1 TO W-ECRAN-PREP
                       END-IF

                   END-PERFORM
           END-EVALUATE
       END-PERFORM.

      ******************************************************************
       13000-AFFICHER-INGREDIENTS.
      *REINITIALISER LE TABLEAU W-TABLE-INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I <99
           INITIALIZE W-TABLE-INGREDIENTS(W-I)
       END-PERFORM.

      *Copier les données du tableau d'ingredients du fichier dans le
      *tableau w-table-ingredients
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I >
                                               RECETTE-NBRE-INGREDIENTS
           MOVE RECETTE-TABLE-INGREDIENTS(W-I)
                                           TO W-TABLE-INGREDIENTS(W-I)
       END-PERFORM.

      *Afficher les ingrédients à l'écran
       INITIALIZE W-ENTREE.
       MOVE 0 TO W-COMPTEUR-INGR.
       MOVE 5 TO W-NO-LIGNE.
       PERFORM VARYING W-COMPTEUR-INGR FROM 1 BY 1
                     UNTIL (W-COMPTEUR-INGR > RECETTE-NBRE-INGREDIENTS)
                     OR (W-ENTREE = ("R" or "r") or ("q" or "Q"))
           DISPLAY W-COMPTEUR-INGR AT LINE W-NO-LIGNE COL 03
           DISPLAY "." AT LINE W-NO-LIGNE COL 05
           DISPLAY W-QUANTITE(W-COMPTEUR-INGR)
                                       AT LINE W-NO-LIGNE COL 07
           DISPLAY W-UNITE-MESURE(W-COMPTEUR-INGR)
                                   AT LINE W-NO-LIGNE COL 18
           DISPLAY W-NOM-INGR(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 30
           ADD 2 TO W-NO-LIGNE
           IF (W-COMPTEUR-INGR = 10 OR 20 OR 30 OR 40 OR 50 OR 60 OR 70
           OR 80 OR 90) AND (W-ENTREE <> ("R" or "r") AND ("q" or "Q"))
           THEN
               MOVE "ENTREE -> Page suivante   Q -> Quitter" TO W-MSG
               MOVE SPACE TO W-ENTREE
               ACCEPT W-ENTREE AT 2580
               IF W-ENTREE <> "R" OR "Q"
               DISPLAY FOND-ECRAN
               MOVE 5 TO W-NO-LIGNE
               DISPLAY ECRAN-AFFICHAGE-RECETTE-INGR
           END-IF
       END-PERFORM.

      ******************************************************************
       14000-AFFICHER-PREP.
       MOVE 5 TO W-NO-LIGNE.
       DISPLAY "1." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE1 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "2." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE2 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "3." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE3 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "4." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE4 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "5." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE5 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "6." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE6 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "7." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE7 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "8." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE8 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "9." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE9 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "10." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE10 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "11." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE11 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "12." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE12 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "13." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE13 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "14." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE14 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "15." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE15 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "16." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE16 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "17." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE17 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "18." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE18 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "19." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE19 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "20." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE20 AT LINE W-NO-LIGNE COL 8.

      ******************************************************************
       15000-MENU-COMMENTAIRES.
       MOVE SPACE TO W-MSG.
       MOVE SPACE TO W-CHOIX-COMMENTAIRE.
       PERFORM UNTIL W-CHOIX-COMMENTAIRE = "3"
           DISPLAY FOND-ECRAN
           DISPLAY ECRAN-MENU-COMMENTAIRES
           MOVE SPACE TO W-CHOIX-COMMENTAIRE
           ACCEPT W-CHOIX-COMMENTAIRE AT 1638
           EVALUATE TRUE
               WHEN W-CHOIX-COMMENTAIRE = "1"
                   PERFORM 17000-AJOUTER-COMMENTAIRE
               WHEN W-CHOIX-COMMENTAIRE = "2"
                   PERFORM 160000-CONSULTER-COMMENTAIRES
               WHEN W-CHOIX-COMMENTAIRE <> "1" AND "2" AND "3"
                   MOVE "Choix invalide" TO W-MSG
                   DISPLAY W-MSG AT 2501
           END-EVALUATE
       END-PERFORM.

      ******************************************************************
       160000-CONSULTER-COMMENTAIRES.
       OPEN INPUT COMMENTAIRES-IDX.
       MOVE RECETTE-NOM TO COMM-NOM-RECETTE.
       READ COMMENTAIRES-IDX KEY IS COMM-NOM-RECETTE
       INVALID KEY
           DISPLAY FOND-ECRAN
           DISPLAY "Aucun commentaire pour cette recette!" AT 1010
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 2580
       NOT INVALID KEY
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > COMM-NBRE
           DISPLAY FOND-ECRAN
           DISPLAY ECRAN-AFFICHAGE-COMMENTAIRE
           MOVE "ENTREE -> Prochain commentaire" TO W-MSG
           DISPLAY W-MSG AT 2501
           MOVE 5 TO W-NO-LIGNE
           DISPLAY "1." AT LINE W-NO-LIGNE COL 05
           DISPLAY
               RECETTE-COMM-LIGNE1(W-I) AT LINE W-NO-LIGNE COL 09
           ADD 1 TO W-NO-LIGNE
           DISPLAY "2." AT LINE W-NO-LIGNE COL 09
           DISPLAY
               RECETTE-COMM-LIGNE2(W-I) AT LINE W-NO-LIGNE COL 09
           ADD 1 TO W-NO-LIGNE
           DISPLAY "3." AT LINE W-NO-LIGNE COL 09
           DISPLAY
               RECETTE-COMM-LIGNE3(W-I) AT LINE W-NO-LIGNE COL 09
           ADD 1 TO W-NO-LIGNE
           DISPLAY "4." AT LINE W-NO-LIGNE COL 09
           DISPLAY
               RECETTE-COMM-LIGNE4(W-I) AT LINE W-NO-LIGNE COL 09
           ADD 1 TO W-NO-LIGNE
           DISPLAY "5." AT LINE W-NO-LIGNE COL 09
           DISPLAY
               RECETTE-COMM-LIGNE5(W-I) AT LINE W-NO-LIGNE COL 09
           ADD 1 TO W-NO-LIGNE
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 2580
           END-PERFORM
           DISPLAY FOND-ECRAN
           DISPLAY "Aucun autre commentaire" AT 1010
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 2580
       END-READ.
       CLOSE COMMENTAIRES-IDX.



      ******************************************************************
       17000-AJOUTER-COMMENTAIRE.
      *CHERCHER LE NBRE DE COMMENTAIRE DEJA EXISTANT POUR CETTE RECETTE
       OPEN I-O COMMENTAIRES-IDX.
       MOVE RECETTE-NOM TO COMM-NOM-RECETTE.
       READ COMMENTAIRES-IDX KEY IS COMM-NOM-RECETTE
       INVALID KEY MOVE 0 TO COMM-NBRE
       END-READ.
      *AJOUTER 1 AU NBRE DE COMMENTAIRE, PARCE QU'ON EN RAJOUTE 1
       COMPUTE COMM-NBRE = COMM-NBRE + 1.

      *AFFICHER L'ECRAN DE SAISIE
       DISPLAY FOND-ECRAN.
       DISPLAY ECRAN-SAISIE-COMMENTAIRE.
       MOVE "Appuyer sur ENTREE sur une ligne vide lorsque termine"
                                                           TO W-MSG.
       DISPLAY W-MSG AT 2501.
       MOVE 5 TO W-NO-LIGNE.
       DISPLAY "1." AT LINE W-NO-LIGNE COL 05.
       ACCEPT RECETTE-COMM-LIGNE1(COMM-NBRE) AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-COMM-LIGNE1(COMM-NBRE) <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "2." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-COMM-LIGNE2(COMM-NBRE)
               AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-COMM-LIGNE2(COMM-NBRE) <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "3." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-COMM-LIGNE3(COMM-NBRE)
                           AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-COMM-LIGNE3(COMM-NBRE) <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "4." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-COMM-LIGNE4(COMM-NBRE)
                           AT LINE W-NO-LIGNE COL 09.
       IF RECETTE-COMM-LIGNE4(COMM-NBRE) <> SPACE
       THEN
           ADD 1 TO W-NO-LIGNE
           DISPLAY "5." AT LINE W-NO-LIGNE COL 05
           ACCEPT RECETTE-COMM-LIGNE5(COMM-NBRE)
                           AT LINE W-NO-LIGNE COL 09.

      *ECRIRE LE COMMENTAIRE DANS LA FICHIER DE COMMENTAIRES
       IF COMM-NBRE = 1
       THEN
           WRITE COMMENTAIRE
       ELSE IF COMM-NBRE > 1
       THEN
           REWRITE COMMENTAIRE
       END-IF.
       CLOSE COMMENTAIRES-IDX.

      *AFFICHER UN MESSAGE A L'UTILISATEUR CONFIRMANT L'AJOUT DU
      *COMMENTAIRE
       DISPLAY FOND-ECRAN.
       DISPLAY "Commentaire ajoute!" at 1010.
       MOVE SPACE TO W-ENTREE.
       ACCEPT W-ENTREE AT 2580.

      ******************************************************************
       18000-MODIFIER-RECETTE.
       OPEN INPUT RECETTES-IDX.
           EVALUATE TRUE
               WHEN W-CHOIX-RECETTE = 1
                   MOVE W-RECETTE1 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 2
                   MOVE W-RECETTE2 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 3
                   MOVE W-RECETTE3 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 4
                   MOVE W-RECETTE4 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 5
                   MOVE W-RECETTE5 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 6
                   MOVE W-RECETTE6 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 7
                   MOVE W-RECETTE7 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 8
                   MOVE W-RECETTE8 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 9
                   MOVE W-RECETTE9 TO RECETTE-NOM
           END-EVALUATE.
       READ RECETTES-IDX KEY IS RECETTE-NOM.
       CLOSE RECETTES-IDX.
       DISPLAY FOND-ECRAN.
       MOVE SPACE TO W-ENTREE.
       MOVE "MODIFIER LA DESCRIPTION    ENTREE -> PROCHAINE PAGE"
        TO W-MSG.
       DISPLAY ECRAN-MODIFIER-1.
       ACCEPT RECETTE-DESCRIPTION-LIGNE1 AT 0907.
       ACCEPT RECETTE-DESCRIPTION-LIGNE2 AT 1007.
       ACCEPT RECETTE-DESCRIPTION-LIGNE3 AT 1107.
       ACCEPT W-ENTREE AT 2580.
       MOVE SPACE TO W-ENTREE
       MOVE
       "I -> INGREDIENTS        P -> PREPARATION     ENTER -> QUITTER"
        TO W-MSG.
       DISPLAY FOND-ECRAN.
       DISPLAY W-MSG AT 2501.
       ACCEPT W-ENTREE AT 2580.
       IF W-ENTREE = "I" OR "i"
       THEN
           MOVE "MODIFICATION DES INGREDIENTS" TO W-MSG
           DISPLAY W-MSG AT 2501
           IF RECETTE-SYSTEME-UNITE = "M" OR "m"
           THEN
               PERFORM 19000-MODIFIER-INGREDIENTS-METR
           ELSE IF RECETTE-SYSTEME-UNITE = "U" OR "u"
           THEN
               PERFORM 20000-MODIFIER-INGREDIENTS-US
           END-IF
           MOVE SPACE TO W-ENTREE
           MOVE
           "P -> PREPARATION     ENTER -> QUITTER"
           TO W-MSG
           DISPLAY FOND-ECRAN
           DISPLAY W-MSG AT 2501
           ACCEPT W-ENTREE AT 2580
       END-IF.
       IF W-ENTREE = "P" OR "p"
       THEN
           MOVE
           "MODIFICATION DE LA PREPARATION" TO W-MSG
           DISPLAY W-MSG AT 2501
           PERFORM 21000-MODIFIER-PREP.
       IF W-ENTREE <> SPACE
       THEN
       PERFORM 22000-REECRIRE-FICHIER.

      ******************************************************************
       19000-MODIFIER-INGREDIENTS-METR.
       MOVE SPACE TO W-MSG.
      *REINITIALISER LE TABLEAU W-TABLE-INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I <99
           INITIALIZE W-TABLE-INGREDIENTS(W-I)
       END-PERFORM.

      *COPIER LES DONNÉES DU TABLEAU D'INGREDIENTS DU FICHIER DANS LE
      *TABLEAU W-TABLE-INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I >
                                               RECETTE-NBRE-INGREDIENTS
           MOVE RECETTE-TABLE-INGREDIENTS(W-I)
                                           TO W-TABLE-INGREDIENTS(W-I)
       END-PERFORM.

      *AFFICHER LES INGRÉDIENTS À L'ÉCRAN
       INITIALIZE W-ENTREE.
       MOVE 0 TO W-COMPTEUR-INGR.
       MOVE 5 TO W-NO-LIGNE.
       MOVE 5 TO W-NO-LIGNE2.
       MOVE 1 TO W-J.
       PERFORM VARYING W-COMPTEUR-INGR FROM 1 BY 1
                     UNTIL (W-COMPTEUR-INGR > RECETTE-NBRE-INGREDIENTS)
                     OR (W-ENTREE = ("N" OR "P"))
           DISPLAY W-COMPTEUR-INGR AT LINE W-NO-LIGNE COL 03
           DISPLAY "." AT LINE W-NO-LIGNE COL 05
           DISPLAY W-QUANTITE(W-COMPTEUR-INGR)
                                       AT LINE W-NO-LIGNE COL 07
           DISPLAY W-UNITE-MESURE(W-COMPTEUR-INGR)
                                   AT LINE W-NO-LIGNE COL 18
           DISPLAY W-NOM-INGR(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 30
           ADD 2 TO W-NO-LIGNE
           IF (W-COMPTEUR-INGR = 10 OR 20 OR 30 OR 40 OR 50 OR 60 OR 70
           OR 80 OR 90)
           THEN
           MOVE 5 TO W-NO-LIGNE
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 9
               ACCEPT W-QUANTITE(W-J) AT LINE W-NO-LIGNE COL 07
               ACCEPT W-UNITE-MESURE(W-J)
                                       AT LINE W-NO-LIGNE2 COL 18
               PERFORM UNTIL W-UNITE-MESURE(W-J) =
               "g" OR "G" OR "kg" OR "KG" OR "ml" OR "ML" OR "l" OR "L"
               OR "x" OR "X"
                   DISPLAY W-MSG AT 2501
                   ACCEPT W-UNITE-MESURE(W-J)
                                           AT LINE W-NO-LIGNE COL 18
               MOVE "Choix d'unite invalide, entrer g, kg, ml ou l ou x"
                                                               TO W-MSG
               END-PERFORM
               ACCEPT W-NOM-INGR(W-J) AT LINE W-NO-LIGNE COL 30
               ADD 1 TO W-COMPTEUR-INGR
               ADD 2 TO W-NO-LIGNE
               ADD 1 TO W-J
       END-PERFORM.

       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I = W-COMPTEUR-INGR
           ACCEPT W-QUANTITE(W-J) AT LINE W-NO-LIGNE2 COL 07
           ACCEPT W-UNITE-MESURE(W-J)
                                   AT LINE W-NO-LIGNE2 COL 18
           PERFORM UNTIL W-UNITE-MESURE(W-J) =
           "g" OR "G" OR "kg" OR "KG" OR "ml" OR "ML" OR "l" OR "L"
           OR "x" OR "X"
               DISPLAY W-MSG AT 2501
               ACCEPT W-UNITE-MESURE(W-J)
                                       AT LINE W-NO-LIGNE2 COL 18
           MOVE "Choix d'unite invalide, entrer g, kg, ml ou l ou x"
                                                           TO W-MSG
           END-PERFORM
           ACCEPT W-NOM-INGR(W-J) AT LINE W-NO-LIGNE2 COL 30
           ADD 2 TO W-NO-LIGNE2
           ADD 1 TO W-J
       END-PERFORM.

      ******************************************************************
       20000-MODIFIER-INGREDIENTS-US.
       MOVE SPACE TO W-MSG.
      *REINITIALISER LE TABLEAU W-TABLE-INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I <99
           INITIALIZE W-TABLE-INGREDIENTS(W-I)
       END-PERFORM.

      *COPIER LES DONNÉES DU TABLEAU D'INGREDIENTS DU FICHIER DANS LE
      *TABLEAU W-TABLE-INGREDIENTS
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I >
                                               RECETTE-NBRE-INGREDIENTS
           MOVE RECETTE-TABLE-INGREDIENTS(W-I)
                                           TO W-TABLE-INGREDIENTS(W-I)
       END-PERFORM.

      *AFFICHER LES INGRÉDIENTS À L'ÉCRAN
       INITIALIZE W-ENTREE.
       MOVE 0 TO W-COMPTEUR-INGR.
       MOVE 5 TO W-NO-LIGNE.
       MOVE 5 TO W-NO-LIGNE2.
       MOVE 1 TO W-J.
       PERFORM VARYING W-COMPTEUR-INGR FROM 1 BY 1
                     UNTIL (W-COMPTEUR-INGR > RECETTE-NBRE-INGREDIENTS)
                     OR (W-ENTREE = ("N" OR "P"))
           DISPLAY W-COMPTEUR-INGR AT LINE W-NO-LIGNE COL 03
           DISPLAY "." AT LINE W-NO-LIGNE COL 05
           DISPLAY W-QUANTITE(W-COMPTEUR-INGR)
                                       AT LINE W-NO-LIGNE COL 07
           DISPLAY W-UNITE-MESURE(W-COMPTEUR-INGR)
                                   AT LINE W-NO-LIGNE COL 18
           DISPLAY W-NOM-INGR(W-COMPTEUR-INGR) AT LINE W-NO-LIGNE COL 30
           ADD 2 TO W-NO-LIGNE
           IF (W-COMPTEUR-INGR = 10 OR 20 OR 30 OR 40 OR 50 OR 60 OR 70
           OR 80 OR 90)
           THEN
           MOVE 5 TO W-NO-LIGNE
           PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > 9
               ACCEPT W-QUANTITE(W-J) AT LINE W-NO-LIGNE COL 07
               ACCEPT W-UNITE-MESURE(W-J)
                                       AT LINE W-NO-LIGNE2 COL 18
               PERFORM UNTIL W-UNITE-MESURE(W-J) =
               "ct" OR "CT" OR "cp" OR "CP" OR "oz" OR "OZ" OR "t"
               OR "T" OR "x" OR "X"
                   DISPLAY W-MSG AT 2501
                   ACCEPT W-UNITE-MESURE(W-J)
                                           AT LINE W-NO-LIGNE COL 18
               MOVE "Choix d'unite invalide, entrer ct, cp, oz, t ou x"
                                                               TO W-MSG
               END-PERFORM
               ACCEPT W-NOM-INGR(W-J) AT LINE W-NO-LIGNE COL 30
               ADD 1 TO W-COMPTEUR-INGR
               ADD 2 TO W-NO-LIGNE
               ADD 1 TO W-J
       END-PERFORM.

       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I = W-COMPTEUR-INGR
           ACCEPT W-QUANTITE(W-J) AT LINE W-NO-LIGNE2 COL 07
           ACCEPT W-UNITE-MESURE(W-J)
                                   AT LINE W-NO-LIGNE2 COL 18
           PERFORM UNTIL W-UNITE-MESURE(W-J) =
           "ct" OR "CT" OR "cp" OR "CP" OR "oz" OR "OZ" OR "t"
           OR "T" OR "x" OR "X"
               DISPLAY W-MSG AT 2501
               ACCEPT W-UNITE-MESURE(W-J)
                                       AT LINE W-NO-LIGNE2 COL 18
           MOVE "Choix d'unite invalide, entrer ct, cp, oz, t ou x"
                                                           TO W-MSG
           END-PERFORM
           ACCEPT W-NOM-INGR(W-J) AT LINE W-NO-LIGNE2 COL 30
           ADD 2 TO W-NO-LIGNE2
           ADD 1 TO W-J
       END-PERFORM.

      ******************************************************************
       21000-MODIFIER-PREP.
       MOVE 5 TO W-NO-LIGNE.
       DISPLAY "1." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE1 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "2." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE2 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "3." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE3 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "4." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE4 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "5." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE5 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "6." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE6 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "7." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE7 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "8." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE8 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "9." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE9 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "10." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE10 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "11." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE11 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "12." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE12 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "13." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE13 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "14." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE14 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "15." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE15 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "16." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE16 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "17." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE17 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "18." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE18 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "19." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE19 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       DISPLAY "20." AT LINE W-NO-LIGNE COL 5
       DISPLAY RECETTE-PREP-LIGNE20 AT LINE W-NO-LIGNE COL 8.


       MOVE 5 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE1 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE2 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE3 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE4 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE5 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE6 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE7 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE8 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE9 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE10 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE11 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE12 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE13 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE14 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE15 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE16 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE17 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE18 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE19 AT LINE W-NO-LIGNE COL 8.
       ADD 1 TO W-NO-LIGNE.
       ACCEPT RECETTE-PREP-LIGNE20 AT LINE W-NO-LIGNE COL 8.

      ******************************************************************
       22000-REECRIRE-FICHIER.
       PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-COMPTEUR-INGR
           MOVE W-TABLE-INGREDIENTS(W-I)
                                   TO RECETTE-TABLE-INGREDIENTS(W-I)
       END-PERFORM.
       OPEN I-O RECETTES-IDX.
       REWRITE RECETTE.
       CLOSE RECETTES-IDX.

       DISPLAY FOND-ECRAN.
       MOVE "Modification effectuée" TO W-MSG.
       DISPLAY W-MSG AT 2501.
       MOVE SPACE TO W-ENTREE.
       ACCEPT W-ENTREE AT 2580.

      ******************************************************************
       22000-SUPPRIMER-RECETTE.
       OPEN I-O RECETTES-IDX.
           EVALUATE TRUE
               WHEN W-CHOIX-RECETTE = 1
                   MOVE W-RECETTE1 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 2
                   MOVE W-RECETTE2 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 3
                   MOVE W-RECETTE3 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 4
                   MOVE W-RECETTE4 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 5
                   MOVE W-RECETTE5 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 6
                   MOVE W-RECETTE6 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 7
                   MOVE W-RECETTE7 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 8
                   MOVE W-RECETTE8 TO RECETTE-NOM
               WHEN W-CHOIX-RECETTE = 9
                   MOVE W-RECETTE9 TO RECETTE-NOM
           END-EVALUATE
       READ RECETTES-IDX KEY IS RECETTE-NOM.
       DISPLAY FOND-ECRAN.
       DISPLAY "Voulez-vous vraiment supprimer la recette" at 0520.
       DISPLAY RECETTE-NOM AT 0730.
       DISPLAY "O/N?" AT 0932.
       MOVE SPACE TO W-ENTREE.
       PERFORM UNTIL W-ENTREE = "O" OR "o" OR "N" OR "n"
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 0937
       END-PERFORM.
       IF W-ENTREE = "O" OR "o"
       THEN
           DELETE RECETTES-IDX
      *    SUPPRIMER AUSSI LA RECETTE DANS LE FICHIER DE COMMENTAIRES
           OPEN I-O COMMENTAIRES-IDX
           MOVE RECETTE-NOM TO COMM-NOM-RECETTE
           READ COMMENTAIRES-IDX KEY IS COMM-NOM-RECETTE
           NOT INVALID KEY
               DELETE COMMENTAIRES-IDX
           END-READ
           CLOSE COMMENTAIRES-IDX
           DISPLAY FOND-ECRAN
           DISPLAY "RECETTE SUPPRIMEE" AT 1010
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 2580
       END-IF.
       IF W-ENTREE = "N" OR "n"
       THEN
           DISPLAY FOND-ECRAN
           DISPLAY "RECETTE CONSERVEE" AT 1010
           MOVE SPACE TO W-ENTREE
           ACCEPT W-ENTREE AT 2580
       END-IF.
       CLOSE RECETTES-IDX.

      ******************************************************************
       23000-COPIER-FICHIER.
       CALL "CBL_COPY_FILE" USING "RECETTES_BACKUP.DAT"
                                                   "BACKUP\SORTIE.DAT".
       IF RETURN-CODE = 0
          MOVE "COPIE EFFECTUEE. " TO W-MSG
          DISPLAY W-MSG AT 2501
       ELSE
          MOVE "ERREUR DE COPIE: " TO W-MSG
          DISPLAY W-MSG AT 2501
          DISPLAY RETURN-CODE 2514.
