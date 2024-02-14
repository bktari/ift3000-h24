(******************************************************************************)
(** TP1 Hiver 2024 - Langages de programmation (IFT-3000) 
    - Gestion de cours et de programmes - Dépendances de cours                *)
(******************************************************************************)

(******************************************************************************)
(* Spécification                                                              *)
(******************************************************************************)
open GcpLib
open Gcp

(** {1 Structures de données}*)

type type_cours =
  | OB  (** Cours obligatoires dans le programme *)
  | OP  (** Cours optionnels dans le programme *)
  | Conc  (** Cours mentionnés dans les concentrations *)

(** {1 Interface des fonctions à implanter}*)

val est_prerequis : cours list -> num_cours -> num_cours -> int
(** [est_prerequis lc nc1 nc2] 
    retourne 1 si [nc2] est prérequis à [nc1]; -1 si c'est [nc1] qui est 
    prérequis à [nc2]; 0 sinon.
    
    {b Soulève exception} {e Failure} si un des 2 cours [nc1] ou [nc2] n'est 
    pas défini dans [lc] (l'exception pourrait avoir été soulevée par une autre 
    fonction appelée dans le corps de [est_prerequis]).

    Exemples:

{[# est_prerequis bcours "IFT-2008" "IFT-3000";;
- : int = -1
]}

{[# est_prerequis bcours "IFT-3000" "IFT-2008";;
- : int = 1
]}

{[# est_prerequis bcours "IFT-3000" "IFT-3000";;
- : int = 0
]}

{[# est_prerequis bcours "IFT-3000" "IFT-1004";;
- : int = 0
]}

{[# est_prerequis bcours "GIF-1001" "IFT-1004";;
- : int = 1
]}

{[# est_prerequis bcours "IFT-2008" "a";;
Exception: Failure "Cours <a> inexistant!".
]}
*)

val simp_pre : prealables -> prealables
(** [simpl_pre] 
    simplifie les disjonctions (OU) et les conjections (ET) de préalables 
    ([pre]). Plus précisément, elle élimine les redondances de préalables et les
    imbrications de conjonctions ou de disjonctions; aussi, si une conjonction
    ou une disjonction se limite à un seul préalable, elle sera simplifiée en
    ce préalable; de même, si une conjonction ou une disjonction se retrouve 
    avec une liste vide de préalable, elle est simplifiée en «Aucun».  

    Exemples:

{[# simp_pre (OU [CP "a"; OU [CP "b"; CP "a"]]);;
- : prealables = OU [CP "a"; CP "b"]
]}

{[# simp_pre (OU [CP "a";  CP "a"]);;
- : prealables = CP "a"
]}

{[# simp_pre (OU [CP "a";  CCP "a"]);;
- : prealables = OU [CP "a"; CCP "a"]
]}

{[# simp_pre (ET [CRE 12; CRE 24; CRE 12]);;
- : prealables = ET [CRE 12; CRE 24]
]}

{[# simp_pre (OU [Aucun; OU [Aucun];  Aucun]);;
- : prealables = Aucun
]}

{[# simp_pre (ET [CP "a"; OU [CP "b"; ET [CP "a"; CP "c"]; CP "b"; Aucun]; 
                 ET [Aucun; CP "a"; CP "d"]]);;
- : prealables = ET [CP "a"; OU [CP "b"; ET [CP "a"; CP "c"]]; CP "d"]
]}

{[# simp_pre (ET [CP "a"; OU [CP "b"]]);;
- : prealables = ET [CP "a"; CP "b"]
]}
*)

val seuls_cours_pgm_dans_pre : num_cours list -> prealables -> prealables
(** [seuls_cours_pgm_dans_pre lncp pre] 
    retire d'un préalable [pre] tous les cours qui ne font partie de [lncp] 
    (liste de numéros de cours d'un programme); évidemment, une fois les cours 
    retirés, il faut retourner un nouveau préalable résultant qui aura été 
    préalablement simplifié (voir {!simp_pre}).

    Exemples:

{[# seuls_cours_pgm_dans_pre [] Aucun;;
- : prealables = Aucun
]}

{[# seuls_cours_pgm_dans_pre [] (CRE 12);;
- : prealables = CRE 12
]}

{[# seuls_cours_pgm_dans_pre [] (CP "a");;
- : prealables = Aucun
]}

{[# seuls_cours_pgm_dans_pre ["a"; "b"; "c"; "d"] (CCP "b");;
- : prealables = CCP "b"
]}

{[# seuls_cours_pgm_dans_pre ["a"; "b"; "c"; "d"] (CP "e");;
- : prealables = Aucun
]}

{[# seuls_cours_pgm_dans_pre ["a"; "b"] (OU [CP "b"; CRE 12; CP "e"; CRE 12]);;
- : prealables = OU [CP "b"; CRE 12]
]}

{[# seuls_cours_pgm_dans_pre ["a"; "b"; "c"; "d"] 
      (ET [CCP "b"; OU [CCP "b"; CP "e"]; OU [CP "e"; CP "f"]]);;
- : prealables = CCP "b"
]}
*)

val cours_pgm_par_type : programme -> type_cours -> num_cours list
(** [cours_pgm_par_type pgm ch] 
    retourne une liste de cours du programme [pgm]. Le paramètre [ch] permet
    de préciser davantage les cours ciblés: obligatoires, optionnels ou ceux
    présents dans les concentrations.

    Exemples:

{[# cours_pgm_par_type b_iig OB;;
- : num_cours list =
    ["GIF-1001"; "IFT-1004"; "MNG-1000"; "MQT-1102"; "SIO-2103"; "CTB-1000";
     "SIO-2100"; "SIO-2105"; "GSO-1000"; "SIO-2104"; "SIO-3100"; "GSF-1000";
     "SIO-2107"; "GLO-4000"; "MRK-1000"; "GLO-2003"; "GLO-4002"; "SIO-2102";
     "SIO-3110"; "IFT-2580"; "GIF-1003"; "IFT-1006"; "GLO-2001"; "IFT-2001";
     "GLO-2000"; "IFT-2006"; "GLO-2004"; "IFT-2007"; "GLO-2100"; "IFT-2008";
     "GLO-2005"; "IFT-2004"; "GIN-3060"; "SIO-2110"]
]}

{[# cours_pgm_par_type b_iig OP;;
- : num_cours list =
    ["GIF-3101"; "GLO-3100"; "GLO-3102"; "GLO-3202"; "GLO-4003"; "GLO-4008";
     "GLO-4035"; "IFT-2002"; "IFT-2003"; "IFT-2101"; "IFT-2102"; "IFT-3000";
     "IFT-3001"; "IFT-3002"; "IFT-3100"; "IFT-3101"; "IFT-3201"; "IFT-4100";
     "MAT-1919"; "ANL-2020"]
]}

{[# cours_pgm_par_type b_iig Conc;;
- : num_cours list =
    ["GLO-3100"; "GLO-3202"; "IFT-2102"; "IFT-3002"; "IFT-3201"; "IFT-4100";
     "ANL-2020"]
]}

{[# cours_pgm_par_type mp_base OB;;
- : num_cours list =
    ["GIF-1001"; "IFT-1004"; "GIF-1003"; "IFT-1006"; "GLO-2100"; "IFT-2008"]
]}

{[# cours_pgm_par_type mp_base OP;;
- : num_cours list =
    ["GLO-2004"; "IFT-1003"; "IFT-1700"; "IFT-2001"; "IFT-2004"; "IFT-2006";
    "IFT-2007"; "IFT-2103"; "IFT-3000"; "IFT-3002"; "IFT-3100"]
]}

{[# cours_pgm_par_type mp_base Conc;;
- : num_cours list = []
]}

{[# cours_pgm_par_type mp_jv OP;;
- : num_cours list = []
]}
*)

val cours_pgm : programme -> num_cours list
(** [cours_pgm pgm] 
    retourne la liste de tous les cours du programme [pgm]. Cette liste ne
    doit comprendre qu'une occurrence de chaque cours.

    Exemples:

{[# cours_pgm mp_tdm;;
- : num_cours list =
    ["IFT-1004"; "GLO-4035"; "GLO-4027"; "GIF-1003"; "IFT-1006"; "GLO-2005";
     "IFT-2004"; "GLO-2100"; "IFT-2008"]
]}

{[# cours_pgm b_ift;;
- : num_cours list =
    ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919"; "IFT-1000";
     "IFT-1003"; "STT-1000"; "IFT-2002"; "IFT-3000"; "IFT-3001"; "IFT-3101";
     "IFT-2580"; "IFT-3580"; "GIF-1003"; "IFT-1006"; "GLO-2000"; "IFT-2006";
     "GLO-2001"; "IFT-2001"; "GLO-2100"; "IFT-2008"; "GLO-2004"; "IFT-2007";
     "IFT-2004"; "GLO-2005"; "ANL-2020"; "ANL-3020"; "BIF-4007"; "GIF-3101";
     "GIF-4100"; "GIF-4101"; "GIF-4104"; "GIF-4105"; "GLO-2003"; "GLO-3100";
     "GLO-3101"; "GLO-3102"; "GLO-3112"; "GLO-3202"; "GLO-4000"; "GLO-4001";
     "GLO-4003"; "GLO-4008"; "GLO-4009"; "GLO-4010"; "GLO-4027"; "GLO-4030";
     "GLO-4035"; "IFT-2101"; "IFT-2102"; "IFT-2103"; "IFT-3002"; "IFT-3100";
     "IFT-3113"; "IFT-4001"; "IFT-4003"; "IFT-4021"; "IFT-4022"; "IFT-4029";
     "IFT-4030"; "IFT-4100"; "IFT-4201"; "SIO-2100"; "SIO-2102"; "SIO-2104";
     "SIO-2105"; "SIO-2107"; "GLO-3004"; "GLO-4002"; "IFT-3201"; "IFT-2003";
     "IFT-4102"; "BCM-1001"; "BCM-1003"; "BCM-1005"; "BIF-1000"; "BIF-1001";
     "PHY-1903"; "STT-2200"]
]}

{[# cours_pgm c_ift;;
- : num_cours list =
    ["GIF-1001"; "IFT-1004"; "GIF-1003"; "IFT-1006"; "GLO-2100"; "IFT-2008";
     "GIF-4104"; "GLO-2003"; "GLO-3101"; "GLO-3102"; "GLO-3202"; "GLO-4000";
     "GLO-4002"; "GLO-4003"; "GLO-4008"; "GLO-4009"; "GLO-4027"; "GLO-4035";
     "IFT-1003"; "IFT-1700"; "IFT-2101"; "IFT-2102"; "IFT-2103"; "IFT-3000";
     "IFT-3002"; "IFT-3100"; "IFT-3201"; "IFT-4029"; "SIO-2102"; "SIO-2104";
     "GLO-3112"; "IFT-3113"; "GLO-2004"; "IFT-2007"; "GLO-2000"; "IFT-2006";
     "GLO-2001"; "IFT-2001"; "GLO-2005"; "IFT-2004"]
]}
*)

val cours_contrib_dans_pgm :
  num_cours -> (string * programme) list -> (string * type_cours option) list
(** [cours_contrib_dans_pgm nc lpgms] 
    retourne une liste qui précise, pour chaque programme présent dans [lpgms], 
    si le cours [nc] y est obligatoire («Some OB»), optionnel («Some OP») ou 
    non contributoire («None»).

    Exemples:

{[# cours_contrib_dans_pgm "IFT-3000" l_pgms;;
- : (string * type_cours option) list =
    [("B-IFT", Some OB); ("B-IIG", Some OP); ("B-GLO", Some OP);
     ("C-IFT", Some OP); ("MP-BASE", Some OP); ("MP-GLO", None);
     ("MP-WEB", None); ("MP-JV", None); ("MP-TDM", None)]
]}

{[# cours_contrib_dans_pgm "IFT-1004" l_pgms;;
- : (string * type_cours option) list =
    [("B-IFT", Some OB); ("B-IIG", Some OB); ("B-GLO", None); ("C-IFT", Some OB);
     ("MP-BASE", Some OB); ("MP-GLO", Some OB); ("MP-WEB", Some OB);
     ("MP-JV", Some OB); ("MP-TDM", Some OB)]
]}

{[# cours_contrib_dans_pgm "GLO-4010" l_pgms;;
- : (string * type_cours option) list =
    [("B-IFT", Some OP); ("B-IIG", None); ("B-GLO", Some OP); ("C-IFT", None);
     ("MP-BASE", None); ("MP-GLO", None); ("MP-WEB", None); ("MP-JV", None);
     ("MP-TDM", None)]
]}

{[# cours_contrib_dans_pgm "a" l_pgms;;
- : (string * type_cours option) list =
    [("B-IFT", None); ("B-IIG", None); ("B-GLO", None); ("C-IFT", None);
     ("MP-BASE", None); ("MP-GLO", None); ("MP-WEB", None); ("MP-JV", None);
     ("MP-TDM", None)]
]}

{[# cours_contrib_dans_pgm "IFT-1004" [];;
- : (string * type_cours option) list = []
]}

{[# cours_contrib_dans_pgm "IFT-1004" ["B-GLO",b_glo];;
- : (string * type_cours option) list = [("B-GLO", None)]
]}
*)

val regroupe_cours_equiv : cours list -> num_cours list -> num_cours list list
(** [regroupe_cours_equiv lc lnc] 
    regroupe les cours équivalents présents dans la liste [lnc]; pour un cours 
    présent dans cette liste, si il ne comprend de cours équivalents dans le 
    reste de la liste, on retourne une liste singleton comprenant ce cours; 
    autrement, on retourne une liste comprenant ce cours et tous les autres 
    cours de la liste qui lui sont équivalents.

    À noter que dans les listes contenues dans la liste résultante, lorsque 
    la liste comprend plus d'un cours, il faut que les cours soient triés du 
    plus petit vers le plus grand (utiliser simplement 
    {{: https://v2.ocaml.org/releases/5.1/api/Stdlib.html }Stdlib.compare}).

    {b Soulève exception} {e Failure} si un des cours présent dans [lnc] n'est 
    pas défini dans [lc] (l'exception pourrait avoir été soulevée par une autre 
    fonction appelée dans le corps de [regroupe_cours_equiv]).

    Exemples:

{[# regroupe_cours_equiv bcours [];;
- : num_cours list list = []
]}

{[# regroupe_cours_equiv bcours ["IFT-3000"];;
- : num_cours list list = [["IFT-3000"]]
]}

{[# regroupe_cours_equiv bcours ["IFT-3000"; "IFT-1004"];;
- : num_cours list list = [["IFT-3000"]; ["IFT-1004"]]
]}

{[# regroupe_cours_equiv bcours ["IFT-1004"; "IFT-3000"; "GLO-1901"];;
- : num_cours list list = [["GLO-1901"; "IFT-1004"]; ["IFT-3000"]]
]}

{[# regroupe_cours_equiv bcours ["GLO-2100";"IFT-1004";"IFT-2008";"GLO-1901"];;
- : num_cours list list = [["GLO-2100"; "IFT-2008"]; ["GLO-1901"; "IFT-1004"]]
]}

{[# regroupe_cours_equiv bcours ["GLO-7003"; "GLO-1901"; "GLO-4010"];;
- : num_cours list list = [["GLO-7003"]; ["GLO-1901"]; ["GLO-4010"]]
]}

{[# regroupe_cours_equiv bcours ["a"];;
Exception: Failure "Cours <a> inexistant!".
]}

{[# regroupe_cours_equiv bcours ["IFT-3000";"b"];;
Exception: Failure "Cours <b> inexistant!".
]}
*)
