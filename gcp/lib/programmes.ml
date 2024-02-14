open Gcp

(* ------------------------------------------------------------------------- *)
(* Données: définition de programmes                                         *)
(* ------------------------------------------------------------------------- *)

(* B-IFT ------------------------------------------------------------------- *)
(* https://www.fsg.ulaval.ca/etudes/programmes-detudes/baccalaureat-en-informatique *)
let b_ift : programme =
  (* type de programme - B-IFT, 90 cr., en 7 années max. *)
  ( (Bacc, "B-IFT", 90, 7),
    (* titre *)
    "Baccalauréat en informatique",
    (* sessions d'admission *)
    [ Aut; Hiv ],
    (* Activités de formation obligatoires - 51 crédits*)
    ( 51,
      (* En plus des cours obligatoires du programme, l'étudiant doit réussir les
         stages de formation pratique IFT-2580 et IFT-3580 pour obtenir son
         diplôme. Il peut également suivre deux autres stages de formation
         pratique optionnels : IFT-3591 et IFT-3592. Les crédits de ces stages
         sont en sus des crédits exigés du programme. Pour s'inscrire, s'adresser
         à la direction de programme. *)
      [
        ( "Règle 0 - 33 crédits obligatoires: ",
          CoursOB
            ( 33,
              [
                "GIF-1001";
                "IFT-1004";
                "IFT-1111";
                "MAT-1200";
                "MAT-1919";
                "IFT-1000";
                "IFT-1003";
                "STT-1000";
                "IFT-2002";
                "IFT-3000";
                "IFT-3001";
                "IFT-3101";
                (* Stages obligatoires *)
                "IFT-2580";
                "IFT-3580";
              ] ) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2000"; "IFT-2006" ]) );
        ( "Règle 3 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2001"; "IFT-2001" ]) );
        ( "Règle 4 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]) );
        ( "Règle 5 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2004"; "IFT-2007" ]) );
        ( "Règle 6 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "IFT-2004"; "GLO-2005" ]) );
      ] ),
    (* Autres exigences*)
    ( 39,
      [
        ( "Règle 1 - 3 crédits parmi: ",
          (* Réussir le cours ANL-2020 Intermediate English II. L'étudiant qui 
             démontre qu'il a acquis ce niveau (VEPT : 53) lors du test 
             administré par l'École de langues peut choisir un cours d'anglais 
             de niveau supérieur ou, s'il a acquis le niveau Advanced English II 
             (VEPT : 63), un cours d'une autre langue moderne ou tout autre 
             cours de 3 crédits de son choix. Dans ce dernier cas, contacter
             la direction du programme. *)
          PlageCr (3, 3, Cours [ "ANL-2020"; "ANL-3020" ]) );
        ( "Règle 2 - 9 à 12 crédits excluants: ",
          (* les cours qui n'ont pas comme sigle GIF, GLO, IFT, SIO. Le cours 
             GEL-1001, les cours correctifs de français et les cours d'anglais 
             de niveau inférieur à ANL-2020 sont également exclus.

             L'étudiant admis au profil entrepreneurial doit s'inscrire aux 
             cours ENT-1000, ENT-3000 et ENT-3010. *)
          PlageCr
            ( 9,
              12,
              CoursExclus
                [
                  "GIF-*";
                  "GLO-*";
                  "IFT-*";
                  "SIO-*";
                  "FRN-*";
                  "GEL-1001";
                  "ANL-2010";
                  "ANL-1020";
                  "ANL-1010";
                  "ANL-2011";
                  "ANL-2020";
                  "ANL-3020";
                ] ) );
        ( "Règle 3 - 15 à 18 crédits parmi: ",
          PlageCr
            ( 15,
              18,
              Cours
                [
                  "BIF-4007";
                  "GIF-3101";
                  "GIF-4100";
                  "GIF-4101";
                  "GIF-4104";
                  "GIF-4105";
                  "GLO-2003";
                  "GLO-3100";
                  "GLO-3101";
                  "GLO-3102";
                  "GLO-3112";
                  "GLO-3202";
                  "GLO-4000";
                  "GLO-4001";
                  "GLO-4003";
                  "GLO-4008";
                  "GLO-4009";
                  "GLO-4010";
                  "GLO-4027";
                  "GLO-4030";
                  "GLO-4035";
                  "IFT-2101";
                  "IFT-2102";
                  "IFT-2103";
                  "IFT-3002";
                  "IFT-3100";
                  "IFT-3113";
                  "IFT-4001";
                  "IFT-4003";
                  "IFT-4021";
                  "IFT-4022";
                  "IFT-4029";
                  "IFT-4030";
                  "IFT-4100";
                  "IFT-4201";
                  "SIO-2100";
                  "SIO-2102";
                  "SIO-2104";
                  "SIO-2105";
                  "SIO-2107";
                  (* Partie ajoutée par BK pour cohérence ...*)
                  (* "BCM-1001"; "BCM-1003"; "BCM-1005"; "BIF-1000";
                     "BIF-1001"; "PHY-1903"; "STT-2200"; *)
                ] ) );
        ( "Règle 4 - 6 à 9 crédits parmi: ",
          PlageCr (6, 9, Cours [ "GLO-3004"; "GLO-4002"; "IFT-3201" ]) );
        ( "Règle 5 - 3 à 6 crédits parmi: ",
          PlageCr (3, 6, Cours [ "IFT-2003"; "IFT-4102" ]) );
      ] ),
    (* concentrations *)
    [
      ( "Bio-informatique",
        ( 12,
          [
            PlageCr (3, 6, Cours [ "IFT-4001"; "IFT-4102" ]);
            PlageCr
              ( 6,
                9,
                Cours
                  [
                    "BCM-1001";
                    "BCM-1003";
                    "BCM-1005";
                    "BIF-1000";
                    "BIF-1001";
                    "BIF-4007";
                  ] );
          ] ) );
      ( "Génie logiciel",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [
                    "GIF-4104";
                    "GLO-2003";
                    "GLO-3004";
                    "GLO-3100";
                    "GLO-3101";
                    "GLO-3102";
                    "GLO-3112";
                    "GLO-4000";
                    "GLO-4002";
                    "GLO-4003";
                    "GLO-4008";
                    "GLO-4035";
                    "IFT-3201";
                  ] );
          ] ) );
      ( "Internet et applications Web",
        ( 12,
          [
            CoursOB (3, [ "GLO-3102" ]);
            PlageCr
              ( 9,
                9,
                Cours
                  [
                    "GIF-3101";
                    "GLO-3100";
                    "GLO-3112";
                    "GLO-4000";
                    "GLO-4002";
                    "IFT-2101";
                    "IFT-3201";
                  ] );
          ] ) );
      ( "Multimédia et développement de jeux vidéo",
        ( 12,
          [
            PlageCr
              ( 9,
                12,
                Cours
                  [
                    "GIF-3101";
                    "GIF-4100";
                    "GIF-4104";
                    "GIF-4105";
                    "GLO-4000";
                    "IFT-2103";
                    "IFT-3100";
                    "IFT-3113";
                    "IFT-4102";
                  ] );
            PlageCr (0, 3, Cours [ "PHY-1903" ]);
          ] ) );
      ( "Sécurité informatique",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [
                    "GLO-3100";
                    "GLO-3202";
                    "IFT-2102";
                    "IFT-3002";
                    "IFT-3201";
                    "SIO-2102";
                  ] );
          ] ) );
      ( "Systèmes d'information organisationnels",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [ "GLO-3101"; "GLO-4035"; "IFT-4001"; "SIO-2102"; "SIO-2104" ]
              );
          ] ) );
      ( "Systèmes intelligents",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [
                    "GIF-4100";
                    "GIF-4101";
                    "GLO-4001";
                    "GLO-4030";
                    "IFT-2003";
                    "IFT-3100";
                    "IFT-4001";
                    "IFT-4022";
                    "IFT-4030";
                    "IFT-4102";
                  ] );
          ] ) );
      ( "Traitement de données massives",
        ( 12,
          [
            CoursOB (6, [ "GLO-4027"; "GLO-4035" ]);
            PlageCr
              ( 6,
                6,
                Cours
                  [
                    "GIF-4101";
                    "GIF-4104";
                    "GLO-4030";
                    "IFT-4001";
                    "IFT-4102";
                    "STT-2200";
                  ] );
          ] ) );
    ] )

(* B-GLO ------------------------------------------------------------------- *)
let b_glo : programme =
  (* type de programme *)
  ( (Bacc, "B-GLO", 120, 8),
    (* titre *)
    "Baccalauréat en génie logiciel",
    (* sessions d'admission *)
    [ Aut; Hiv ],
    (* Activités de formation obligatoires *)
    ( 96,
      [
        ( "Règle 0 - 93 crédits obligatoires: ",
          CoursOB
            ( 93,
              [
                "GIF-1002";
                "GLO-1111";
                "GLO-1901";
                "MAT-1900";
                "MAT-1919";
                "GEL-1001";
                "GIF-1001";
                "GIF-1003";
                "MAT-1910";
                "GLO-2000";
                "GLO-2004";
                "GLO-2100";
                "GLO-3101";
                "STT-2920";
                "ECN-2901";
                "GLO-2001";
                "GLO-2003";
                "GLO-2005";
                "GLO-3102";
                "GLO-4000";
                "GLO-4002";
                "IFT-3001";
                "MAT-2930";
                "GEL-4799";
                "GLO-3013";
                "GMN-2901";
                "IFT-2002";
                "GLO-3004";
                "GLO-4003";
                "PHI-2910";
                "PHI-3900";
                "GLO-3002";
                "MAT-2910";
                (* Stages obligatoires *)
                "GLO-2580";
                "GLO-2581";
              ] ) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-3100"; "GLO-3202" ]) );
      ] ),
    (* Autres exigences*)
    ( 24,
      [
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "ANL-2020"; "ANL-3020" ]) );
        ( "Règle 2 - 9 crédits parmi: ",
          PlageCr
            ( 9,
              9,
              Cours
                [
                  "BIF-4007";
                  "GEL-1000";
                  "GIF-3000";
                  "GIF-3004";
                  "GIF-3101";
                  "GIF-4100";
                  "GIF-4101";
                  "GIF-4104";
                  "GIF-4105";
                  "GLO-3112";
                  "GLO-4001";
                  "GLO-4007";
                  "GLO-4008";
                  "GLO-4009";
                  "GLO-4010";
                  "GLO-4027";
                  "GLO-4030";
                  "GLO-4035";
                  "IFT-2102";
                  "IFT-2103";
                  "IFT-3000";
                  "IFT-3002";
                  "IFT-3100";
                  "IFT-3101";
                  "IFT-3113";
                  "IFT-3201";
                  "IFT-4001";
                  "IFT-4003";
                  "IFT-4021";
                  "IFT-4022";
                  "IFT-4029";
                  "IFT-4030";
                  "IFT-4102";
                  "IFT-4201";
                  "MAT-2200";
                  "STT-2200";
                  (* Cours ajouté par BK pour cohérence ...*)
                  "MAT-1200";
                ] ) );
        ( "Règle 3 - 3 crédits parmi: ",
          PlageCr
            ( 3,
              3,
              Cours
                [
                  "ANL-3905";
                  "DDU-1000";
                  "EDC-4000";
                  "ENT-1000";
                  "GSF-1020";
                  "GSO-1000";
                  "MRK-1000";
                  "PHI-1900";
                  "RLT-1000";
                  "RLT-1700";
                ] ) );
        ( "Règle 4 - 9 crédits parmi: ",
          PlageCr
            ( 9,
              9,
              Cours
                [
                  "BCM-1001";
                  "BCM-1003";
                  "BIO-1910";
                  "BIO-2003";
                  "BIO-4900";
                  "BIO-4902";
                  "CHM-1000";
                  "CHM-1003";
                  "CHM-1905";
                  "GGR-2305";
                  "GMC-1003";
                  "MCB-1000";
                  "PHY-1000";
                  "PHY-1003";
                  "PHY-1006";
                  "PHY-1007";
                  "PHY-2100";
                  "SBM-1004";
                ] ) );
      ] ),
    (* concentrations *)
    [
      ( "Traitement de données massives",
        ( 9,
          [
            CoursOB (6, [ "GLO-4027"; "GLO-4035" ]);
            PlageCr
              ( 3,
                3,
                Cours
                  [
                    "GIF-4101";
                    "GIF-4104";
                    "GLO-4030";
                    "IFT-4001";
                    "IFT-4021";
                    "IFT-4102";
                    "STT-2200";
                  ] );
          ] ) );
      ( "Jeux vidéo et réalité virtuelle",
        ( 9,
          [
            PlageCr
              ( 9,
                9,
                Cours
                  [
                    "GIF-4100";
                    "GIF-4104";
                    "IFT-2103";
                    "IFT-3100";
                    "IFT-3113";
                    "IFT-4102";
                  ] );
          ] ) );
      ( "Systèmes industriels et embarqués",
        ( 9,
          [
            PlageCr
              ( 9,
                9,
                Cours
                  [
                    "GEL-1000";
                    "GIF-3000";
                    "GIF-3004";
                    "GIF-4104";
                    "GLO-4001";
                    "IFT-4001";
                  ] );
          ] ) );
      ( "Sécurité informatique",
        ( 9,
          [
            PlageCr
              ( 9,
                9,
                Cours
                  [
                    "GLO-3112";
                    "IFT-2102";
                    "IFT-3000";
                    "IFT-3002";
                    "IFT-3101";
                    "IFT-3201";
                  ] );
          ] ) );
    ] )

(* B-IIG ------------------------------------------------------------------- *)
let b_iig : programme =
  ( (Bacc, "B-IIG", 90, 7),
    "Baccalauréat intégré en informatique et gestion",
    [ Hiv; Aut ],
    ( 78,
      [
        ( "Règle 0 - 57 crédits obligatoires: ",
          CoursOB
            ( 57,
              [
                "GIF-1001";
                "IFT-1004";
                "MNG-1000";
                "MQT-1102";
                "SIO-2103";
                "CTB-1000";
                "SIO-2100";
                "SIO-2105";
                "GSO-1000";
                "SIO-2104";
                "SIO-3100";
                "GSF-1000";
                "SIO-2107";
                "GLO-4000";
                "MRK-1000";
                "GLO-2003";
                "GLO-4002";
                "SIO-2102";
                "SIO-3110";
                (* Stage obligatoire *)
                "IFT-2580";
              ] ) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2001"; "IFT-2001" ]) );
        ( "Règle 3 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2000"; "IFT-2006" ]) );
        ( "Règle 4 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2004"; "IFT-2007" ]) );
        ( "Règle 5 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]) );
        ( "Règle 6 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2005"; "IFT-2004" ]) );
        ( "Règle 7 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIN-3060"; "SIO-2110" ]) );
      ] ),
    ( 12,
      [
        ( "Règle 1 - 9 à 12 crédits parmi: ",
          PlageCr
            ( 9,
              12,
              Cours
                [
                  "GIF-3101";
                  "GLO-3100";
                  "GLO-3102";
                  "GLO-3202";
                  "GLO-4003";
                  "GLO-4008";
                  "GLO-4035";
                  "IFT-2002";
                  "IFT-2003";
                  "IFT-2101";
                  "IFT-2102";
                  "IFT-3000";
                  "IFT-3001";
                  "IFT-3002";
                  "IFT-3100";
                  "IFT-3101";
                  "IFT-3201";
                  "IFT-4100";
                  "MAT-1919";
                ] ) );
        ("Règle 2 - 0 à 3 crédits parmi: ", PlageCr (0, 3, Cours [ "ANL-2020" ]));
      ] ),
    [
      ( "Sécurité de l'information",
        ( 12,
          [
            PlageCr
              ( 9,
                9,
                Cours
                  [
                    "GLO-3100";
                    "GLO-3202";
                    "IFT-2102";
                    "IFT-3002";
                    "IFT-3201";
                    "IFT-4100";
                  ] );
            PlageCr (0, 3, Cours [ "ANL-2020" ]);
          ] ) );
    ] )

(* C-IFT ------------------------------------------------------------------- *)
let c_ift : programme =
  let abrev_autres_exigences n1 n2 =
    [
      ( "Règle 1 - 9 à 18 crédits parmi: ",
        PlageCr
          ( n1,
            n2,
            Cours
              [
                "GIF-4104";
                "GLO-2003";
                "GLO-3101";
                "GLO-3102";
                "GLO-3202";
                "GLO-4000";
                "GLO-4002";
                "GLO-4003";
                "GLO-4008";
                "GLO-4009";
                "GLO-4027";
                "GLO-4035";
                "IFT-1003";
                "IFT-1700";
                "IFT-2101";
                "IFT-2102";
                "IFT-2103";
                "IFT-3000";
                "IFT-3002";
                "IFT-3100";
                "IFT-3201";
                "IFT-4029";
                "SIO-2102";
                "GLO-4008";
                "SIO-2104";
                (* Partie ajoutée par BK pour cohérence ...*)
                "GLO-3112";
                "IFT-3113";
              ] ) );
      ( "Règle 2 - 0 à 3 crédits parmi: ",
        PlageCr (0, 3, Cours [ "GLO-2004"; "IFT-2007" ]) );
      ( "Règle 3 - 0 à 3 crédits parmi: ",
        PlageCr (0, 3, Cours [ "GLO-2000"; "IFT-2006" ]) );
      ( "Règle 44 - 0 à 3 crédits parmi: ",
        PlageCr (0, 3, Cours [ "GLO-2001"; "IFT-2001" ]) );
      ( "Règle 5 - 0 à 3 crédits parmi: ",
        PlageCr (0, 3, Cours [ "GLO-2005"; "IFT-2004" ]) );
    ]
  in
  (* type de programme *)
  ( (Cert, "C-IFT", 30, 4),
    (* titre *)
    "Certificat en informatique",
    (* sessions d'admission *)
    [ Aut; Hiv ],
    (* Activités de formation obligatoires *)
    ( 12,
      [
        ("Règle 0 - 3 crédits parmi: ", CoursOB (6, [ "GIF-1001"; "IFT-1004" ]));
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]) );
      ] ),
    (* Autres exigences*)
    (18, abrev_autres_exigences 9 18),
    (* concentrations *)
    [
      ( "Génie logiciel",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [
                    "GIF-4104";
                    "GLO-2003";
                    "GLO-3101";
                    "GLO-3102";
                    "GLO-3112";
                    "GLO-4000";
                    "GLO-4002";
                    "GLO-4003";
                  ] );
          ]
          @ List.map snd (abrev_autres_exigences 0 6) ) );
      ( "Internet et sécurité",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [
                    "GLO-3202";
                    "GLO-4009";
                    "IFT-2101";
                    "IFT-2102";
                    "IFT-3002";
                    "IFT-3201";
                    "IFT-4029";
                    "SIO-2102";
                  ] );
          ]
          @ List.map snd (abrev_autres_exigences 0 6) ) );
      ( "Modélisation, analyse et base de données",
        ( 12,
          [
            PlageCr (3, 3, Cours [ "GLO-2005"; "IFT-2004" ]);
            PlageCr (0, 3, Cours [ "GLO-2004"; "IFT-2007" ]);
            PlageCr (6, 9, Cours [ "GLO-4027"; "GLO-4035"; "IFT-1003" ]);
          ]
          @ List.map snd (abrev_autres_exigences 0 6) ) );
      ( "Multimédia et développement de jeux vidéo",
        ( 12,
          [
            PlageCr
              ( 12,
                12,
                Cours
                  [ "GIF-4104"; "GLO-4000"; "IFT-2103"; "IFT-3100"; "IFT-3113" ]
              );
          ]
          @ List.map snd (abrev_autres_exigences 0 6) ) );
    ] )

(* MP_Base------------------------------------------------------------------- *)
let mp_base : programme =
  (* type de programme *)
  ( (MP, "MP-BASE", 15, 2),
    (* titre *)
    "Microprogramme en informatique de base",
    (* sessions d'admission *)
    [ Aut; Hiv; Ete ],
    (* Activités de formation obligatoires *)
    ( 12,
      [
        ( "Règle 0 - 6 crédits obligatoires: ",
          CoursOB (6, [ "GIF-1001"; "IFT-1004" ]) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]) );
      ] ),
    (* Autres exigences*)
    ( 3,
      [
        ( "Règle 3 - 3 crédits parmi: ",
          PlageCr
            ( 3,
              3,
              Cours
                [
                  "GLO-2004";
                  "IFT-1003";
                  "IFT-1700";
                  "IFT-2001";
                  "IFT-2004";
                  "IFT-2006";
                  "IFT-2007";
                  "IFT-2103";
                  "IFT-3000";
                  "IFT-3002";
                  "IFT-3100";
                ] ) );
      ] ),
    (* concentrations *)
    [] )

(* MP_Glo------------------------------------------------------------------- *)
let mp_glo : programme =
  (* type de programme *)
  ( (MP, "MP-GLO", 18, 2),
    (* titre *)
    "Microprogramme en conception de logiciels",
    (* sessions d'admission *)
    [ Aut; Hiv; Ete ],
    (* Activités de formation obligatoires *)
    ( 18,
      [
        ( "Règle 0 - 12 crédits obligatoires: ",
          CoursOB (12, [ "IFT-1004"; "GLO-2003"; "GLO-4002"; "GLO-4003" ]) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2004"; "IFT-2007" ]) );
      ] ),
    (* Autres exigences*)
    (0, []),
    (* concentrations *)
    [] )

(* MP_Web------------------------------------------------------------------- *)
let mp_web : programme =
  (* type de programme *)
  ( (MP, "MP-WEB", 18, 2),
    (* titre *)
    "Microprogramme en développement d'applications Web",
    (* sessions d'admission *)
    [ Aut; Hiv; Ete ],
    (* Activités de formation obligatoires *)
    ( 18,
      [
        ( "Règle 0 - 12 crédits obligatoires: ",
          CoursOB (12, [ "IFT-1004"; "GLO-3102"; "GLO-3112" ]) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2005"; "IFT-2004" ]) );
        ( "Règle 3 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2004"; "IFT-2007" ]) );
      ] ),
    (* Autres exigences*)
    (0, []),
    (* concentrations *)
    [] )

(* MP_JV------------------------------------------------------------------- *)
let mp_jv : programme =
  (* type de programme *)
  ( (MP, "MP-JV", 18, 2),
    (* titre *)
    "Microprogramme en programmation de jeux vidéo",
    (* sessions d'admission *)
    [ Aut; Hiv; Ete ],
    (* Activités de formation obligatoires *)
    ( 18,
      [
        ( "Règle 0 - 12 crédits obligatoires: ",
          CoursOB (12, [ "IFT-1004"; "IFT-3100"; "IFT-2103"; "IFT-3113" ]) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]) );
      ] ),
    (* Autres exigences*)
    (0, []),
    (* concentrations *)
    [] )

(* MP_TDM------------------------------------------------------------------- *)
let mp_tdm : programme =
  (* type de programme *)
  ( (MP, "MP-TDM", 18, 2),
    (* titre *)
    "Microprogramme en traitement de données massives",
    (* sessions d'admission *)
    [ Aut; Hiv; Ete ],
    (* Activités de formation obligatoires *)
    ( 18,
      [
        ( "Règle 0 - 12 crédits obligatoires: ",
          CoursOB (12, [ "IFT-1004"; "GLO-4035"; "GLO-4027" ]) );
        ( "Règle 1 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GIF-1003"; "IFT-1006" ]) );
        ( "Règle 2 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2005"; "IFT-2004" ]) );
        ( "Règle 3 - 3 crédits parmi: ",
          PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]) );
      ] ),
    (* Autres exigences*)
    (0, []),
    (* concentrations *)
    [] )

(* Tous les programmes ------------------------------------------------------ *)
let l_pgms =
  [
    ("B-IFT", b_ift);
    ("B-IIG", b_iig);
    ("B-GLO", b_glo);
    ("C-IFT", c_ift);
    ("MP-BASE", mp_base);
    ("MP-GLO", mp_glo);
    ("MP-WEB", mp_web);
    ("MP-JV", mp_jv);
    ("MP-TDM", mp_tdm);
  ]
