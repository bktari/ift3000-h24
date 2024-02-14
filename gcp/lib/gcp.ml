(*****************************************************************************)
(*  Langages de programmation (IFT-3000)                                     *)

(*  Gestion de cours et de programmes.                                       *)

(*****************************************************************************)

(*****************************************************************************)
(* Implantation                                                              *)
(*****************************************************************************)
exception Non_Implante of string

(* ------------------------------------------------------------------------- *)
(* Structures de données                                                     *)
(* ------------------------------------------------------------------------- *)
type cours = num_cours * desc_cours

and desc_cours = {
  titre : titre;
  credit : credit;
  conco : num_cours;
  pre : prealables;
  dept : dept;
  multi : num_cours;
  equiv : num_cours;
  offre : (session * fe) list;
  dom : domaine list;
}

and num_cours = string
and titre = string
and credit = Cr of int | St of int

and prealables =
  | Aucun
  | CP of num_cours
  | CCP of num_cours
  | CRE of int
  | OU of prealables list
  | ET of prealables list

and dept = string
and session = Aut | Hiv | Ete | Annee
and fe = P | H | D | C | A | Z1 | Z3 | ZA | FEX

and domaine =
  | GEN
  | PROG
  | SYS
  | MAT
  | STAT
  | LOG
  | THEO
  | ANA_CON
  | NET
  | BD
  | IA
  | SEC
  | GAME
  | WEB
  | BIO
  | ENT
  | SIO
  | LANG
  | CHM
  | DD
  | ECO

type programme =
  desc_pgm
  * 
  titre
  * 
  session list
  * 
  (int * (string * exigences) list)
  * 
  (int * (string * exigences) list)
  * 
  concentration list 

and desc_pgm = type_pro * string * int * int
and type_pro = Bacc | Cert | MP | M | MM | Doct

and exigences =
  | CoursOB of int * num_cours list
  | PlageCr of int * int * exigences_ext

and exigences_ext = Cours of num_cours list | CoursExclus of num_cours list
and concentration = string * (int * exigences list)

type choix = Min | Max | Plein | Interv of int * int

(* Tp1 (Hiver 2021) *)
(* ---------------- *)

(* ------------------------------------------------------------------------- *)
(* Partie réservée aux fonctions utiles ------------------------------------ *)
(* ------------------------------------------------------------------------- *)
open List

let ( ++ ) l1 l2 =
  fold_left (fun acc x -> if mem x acc then acc else acc @ [ x ]) l1 l2

let ( -- ) l1 l2 =
  fold_left (fun acc x -> if mem x l2 then acc else acc @ [ x ]) [] l1

let explode s = init (String.length s) (String.get s)
let eql l1 l2 = fast_sort Stdlib.compare l1 = fast_sort Stdlib.compare l2
let est_alpha c = match c with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let est_num c = match c with '0' .. '9' -> true | _ -> false

let completer_lfe lfe =
  if mem FEX lfe || lfe = [] then [ A; Z1; Z3; ZA; P; H; D; C ]
  else
    (* pour compatibilité entre anciens et nouveaux codes *)
    let lfe = if mem A lfe then P :: lfe else lfe in
    let lfe = if mem P lfe then A :: lfe else lfe in
    let lfe = if mem D lfe then Z1 :: Z3 :: lfe else lfe in
    let lfe = if mem Z1 lfe then D :: lfe else lfe in
    let lfe = if mem Z3 lfe then D :: lfe else lfe in
    let lfe = if mem C lfe then ZA :: lfe else lfe in
    let lfe = if mem ZA lfe then C :: lfe else lfe in
    sort_uniq Stdlib.compare lfe

let completer_ls ls = if mem Annee ls || ls = [] then [ Aut; Hiv; Ete ] else ls

(* ------------------------------------------------------------------------- *)
(* Début partie code (implantation) ---------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* ret_descr : cours list -> num_cours -> desc_cours                         *)
let ret_descr bcours c =
  try assoc c bcours
  with _ -> failwith (Printf.sprintf "Cours <%s> inexistant!" c)

(* ret_descr : cours list -> num_cours -> num_cours -> bool                  *)
let nc_eq bcours nc1 nc2 =
  nc1 = nc2
  || (let { equiv; _ } = ret_descr bcours nc1 in
      nc2 = equiv)
  ||
  let { equiv; _ } = ret_descr bcours nc2 in
  nc1 = equiv

(* ret_sig_num_motif : string -> string * string                             *)
let ret_sig_num_motif pat =
  match String.split_on_char '-' pat with
  | [ s; n ]
    when (s = "*" || (String.length s = 3 && for_all est_alpha (explode s)))
         && (n = "*" || n = "I"
            || (String.length n = 2 && est_num n.[0] && n.[1] = '*')
            || (String.length n = 4 && for_all est_num (explode n))) ->
      (s, n)
  | _ -> failwith (Printf.sprintf "<%s>: mauvais motif!" pat)

(* ret_sig_num : num_cours -> string * string                                *)
let ret_sig_num c =
  match String.split_on_char '-' c with
  | [ s; n ]
    when String.length s = 3
         && String.length n = 4
         && for_all est_alpha (explode s)
         && for_all est_num (explode n) ->
      (s, n)
  | _ -> failwith (Printf.sprintf "<%s>: numéro de cours incorrect!" c)

(* respecte_motif : num_cours -> string -> bool                              *)
let respecte_motif c pat =
  let aux (s, n) (s', n') =
    match (s', n') with
    | _ when n' = "*" -> s' = "*" || s = s'
    | _ when n'.[0] = 'I' ->
        (s' = "*" || s = s') && n.[0] >= '1' && n.[0] <= '4'
    | _ when String.length n' = 2 -> (s' = "*" || s = s') && n.[0] = n'.[0]
    | _ -> (s' = "*" || s = s') && n = n'
  in
  aux (ret_sig_num c) (ret_sig_num_motif pat)

let id nc =
  let s, n = ret_sig_num nc in
  s ^ n

let idl lnc =
  match lnc with
  | [] -> failwith "idl - cas impossible!"
  | [ nc ] -> id nc
  | nc :: _ -> id nc

(* pr2str : prealables -> string                                             *)
let pr2str pr =
  let rec lpr2str c l =
    match l with
    | [] -> ""
    | [ pr ] -> pr2str' pr
    | pr :: reste -> pr2str' pr ^ c ^ lpr2str c reste
  and pr2str' pr =
    match pr with
    | Aucun -> ""
    | CRE n -> string_of_int n ^ " crédits"
    | CP c -> c
    | CCP c -> c ^ "*"
    | OU l -> "(" ^ lpr2str " OU " l ^ ")"
    | ET l -> "(" ^ lpr2str " ET " l ^ ")"
  in
  match pr with
  | OU l -> lpr2str " OU " l
  | ET l -> lpr2str " ET " l
  | _ -> pr2str' pr

(* existe_pr : prealables -> prealables -> bool                             *)
let rec existe_pr pr pr' =
  match (pr, pr') with
  | Aucun, Aucun -> true
  | CRE n, CRE n' -> n = n'
  | CP c, CP c' | CCP c, CCP c' | CP c, CCP c' -> c = c'
  | CP _, OU l
  | CP _, ET l
  | CCP _, OU l
  | CCP _, ET l
  | CRE _, OU l
  | CRE _, ET l
  | OU _, ET l
  | ET _, OU l ->
      exists (existe_pr pr) l
  | OU l1, OU l2 | ET l1, ET l2 ->
      for_all (fun pr1' -> exists (existe_pr pr1') l2) l1
      || exists (existe_pr pr) l2
  | _ -> false

(* lc_dans_pr : prealables -> num_cours list                                 *)
let rec lc_dans_pr pr =
  match pr with
  | Aucun | CRE _ -> []
  | CP c -> [ c ]
  | CCP c -> [ c ]
  | OU l | ET l -> fold_left (fun acc pr -> acc ++ lc_dans_pr pr) [] l

(* respecte_pr: num_cours list -> int -> num_cours list -> prealables -> bool*)
let respecte_pr lco tcr lc pr =
  let rec aux pr =
    match pr with
    | Aucun -> true
    | CP c -> mem c lco
    | CCP c -> mem c lco || mem c lc
    | CRE n -> tcr >= n
    | OU l -> exists aux l
    | ET l -> for_all aux l
  in
  aux pr

(* total_cr : cours list -> num_cours list -> int                            *)
let total_cr ?(avec_stage = false) bcours lc =
  fold_left
    (fun acc c ->
      let ncr =
        match ret_descr bcours c with
        | { credit = Cr n; _ } -> n
        | { credit = St n; _ } -> if avec_stage then n else 0
      in
      acc + ncr)
    0 lc

(* lc_absents : cours list -> num_cours list                                 *)
let lc_absents bcours =
  fold_left
    (fun acc (_, { conco = cc; pre = pr; multi = mc; equiv = eq; _ }) ->
      acc
      ++ filter
           (fun c' -> not (mem_assoc c' bcours))
           (lc_dans_pr pr ++ filter (( <> ) "") [ cc; mc; eq ]))
    [] bcours

(* ret_bcours_lmot : string list -> cours list -> cours list                 *)
let ret_bcours_lmot lmot bcours =
  filter (fun (c, _) -> exists (respecte_motif c) lmot) bcours

(* ret_bcours_lses_lfe : session list -> fe list -> cours list -> cours list *)
let ret_bcours_lses_lfe ls lfe bcours =
  let ls = completer_ls ls and lfe = completer_lfe lfe in
  filter
    (fun (_, { offre = l; _ }) ->
      exists (fun (s, fe) -> mem s ls && mem fe lfe) l)
    bcours

(* ret_bcours_ldom : domaine list -> cours list -> cours list                *)
let ret_bcours_ldom ld bcours =
  filter (fun (_, { dom = l; _ }) -> exists (fun dom -> mem dom l) ld) bcours

(* ret_bcours_pr : prealables -> cours list -> cours list                    *)
let ret_bcours_pr pr bcours =
  filter (fun (_, { pre = pr'; _ }) -> existe_pr pr pr') bcours

(* ret_bcours_admissibles : num_cours list -> int -> cours list ->
                            cours list *)
let ret_bcours_admissibles lco tcr bcours =
  let respecte_pr' = respecte_pr lco tcr [] in
  let bc =
    filter
      (fun (c, { pre = pr; equiv = c'; _ }) ->
        (not (mem c' lco)) && (not (mem c lco)) && respecte_pr' pr)
      bcours
  in
  filter
    (fun (_, { conco = c'; _ }) ->
      c' = "" || mem c' lco || exists (fun (c, _) -> c = c') bc)
    bc

(* Tp2 (Hiver 2021) *)
(* ---------------- *)

(* ------------------------------------------------------------------------- *)
(* Partie réservée aux fonctions utiles ------------------------------------ *)
(* ------------------------------------------------------------------------- *)
let _min = 3
let _max = 18
let _plein = 12

let vers_interv ch =
  match ch with
  | Min -> Interv (_min, _min)
  | Max -> Interv (_max, _max)
  | Plein -> Interv (_plein, _plein)
  | _ -> ch

let rec respecte_interv bcours ch lc =
  match ch with
  | Interv (n1, n2) ->
      let tcr = total_cr ~avec_stage:true bcours lc in
      tcr >= n1 && tcr <= n2
  | _ -> respecte_interv bcours (vers_interv ch) lc

let rec cnm n l =
  match (n, l) with
  | 0, _ -> [ [] ]
  | 1, [] -> []
  | 1, _ -> map (fun x -> [ x ]) l
  | _, [] -> []
  | n, h :: t ->
      if n > length l then []
      else if n = length l then [ l ]
      else map (fun x -> h :: x) (cnm (n - 1) t) @ cnm n t

let construit_liste_choix eq1 eq2 l =
  let rec elimine_doublons eq l =
    match l with
    | [] -> []
    | e :: r ->
        let l' = elimine_doublons eq r in
        if exists (eq e) r then l' else e :: l'
  in
  let rec f n acc = match n with 0 -> acc | _ -> f (n - 1) (acc @ cnm n l) in
  let lres = f (length l) [] in
  let lres' = map (elimine_doublons eq1) lres in
  elimine_doublons eq2 lres'

let filtre_ch_max bcours lch a max_tcr =
  let total_cr' = total_cr bcours
  and total_cr_avec_stage' = total_cr ~avec_stage:true bcours in
  let l1, max_tcr1, max_tcr2, inclus_stage =
    fold_left
      (fun (acc, acc1, acc2, inclus_stage) l ->
        let size1 = total_cr_avec_stage' l in
        let size2 = total_cr' l in
        let max2 = max acc2 size2 in
        let inclus_stage = inclus_stage || size1 <> size2 in
        let acc2' =
          if max2 > max_tcr then acc2
          else if max_tcr - max2 < max_tcr - acc2 then max2
          else acc2
        in
        (acc @ [ (l, size1, size2) ], max acc1 size1, acc2', inclus_stage))
      ([], 0, 0, false) lch
  in
  ( fold_left
      (fun acc (l, size1, size2) ->
        if size1 <> size2 then
          if size1 >= a && size1 = max_tcr1 then acc @ [ l ] else acc
        else if size2 >= a && size2 = max_tcr2 then acc @ [ l ]
        else acc)
      [] l1,
    min max_tcr1 max_tcr2,
    inclus_stage )

let cours_respect_conco bcours lco lc nc =
  let { conco = nc'; _ } = ret_descr bcours nc in
  nc' = "" || mem nc' lco || mem nc' lc

let respect_conco_ccp bcours lco lc =
  let cours_respect_conco' = cours_respect_conco bcours lco in
  let respecte_pr' = respecte_pr lco (total_cr bcours lco) in
  let rec aux lc =
    let lc' =
      lc
      |> filter (cours_respect_conco' lc)
      |> filter (fun c ->
             let { pre = pr; _ } = ret_descr bcours c in
             respecte_pr' lc pr)
    in
    if lc = lc' then lc else aux lc'
  in
  aux lc <> []

(* ------------------------------------------------------------------------- *)
(* Début partie code (implantation) ---------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* respecte_regle : cours list -> num_cours list -> exigences ->
                    int * exigences * num_cours list *)
let respecte_regle bcours lc regle =
  let respecte_regle_e n1 n2 regle_e =
    match regle_e with
    | Cours l ->
        let lc, lc' = partition (fun nc -> exists (respecte_motif nc) l) lc in
        let tcr = total_cr bcours lc in
        (min n2 tcr, max 0 (n1 - tcr), max 0 (n2 - tcr), Cours (l -- lc), lc')
    | CoursExclus l ->
        let lc', lc = partition (fun nc -> exists (respecte_motif nc) l) lc in
        let tcr = total_cr bcours lc in
        (min n2 tcr, max 0 (n1 - tcr), max 0 (n2 - tcr), CoursExclus l, lc')
  in
  match regle with
  | CoursOB (n, l) ->
      let lc, lc' = partition (fun nc -> exists (( = ) nc) l) lc in
      let tcr = total_cr bcours lc in
      (min n tcr, CoursOB (max 0 (n - tcr), l -- lc), lc')
  | PlageCr (n1, n2, regle_e) ->
      let tcr, n1', n2', regle_e', lc' = respecte_regle_e n1 n2 regle_e in
      (tcr, PlageCr (n1', n2', regle_e'), lc')

(* conc_obtenues : cours list -> programme -> num_cours list -> string list  *)
let conc_obtenues bcours (_, _, _, _, _, lconc) lco =
  let rec respecte_regles n n' l_regles =
    match l_regles with
    | [] -> n' >= n
    | regle :: reste ->
        let tcr, _, _ = respecte_regle bcours lco regle in
        respecte_regles n (n' + tcr) reste
  in
  lconc
  |> filter (fun (_, (n, l_regles)) -> respecte_regles n 0 l_regles)
  |> map fst

(* ou_en_suis_je : cours list -> programme -> num_cours list ->
                   (string * int * exigences) list * (int * int) *
                   (string * int * exigences) list * (int * int) *
                   num_cours list *)
let ou_en_suis_je bcours (_, _, _, (n1, regle1), (n2, regle2), _) lco =
  let result1, tcr1, lc1 =
    fold_left
      (fun (acc, n, lco) (str, regle) ->
        let tcr, regle', lc = respecte_regle bcours lco regle in
        (acc @ [ (str, tcr, regle') ], n + tcr, lc))
      ([], 0, lco) regle1
  in
  let result2, tcr2, lc2 =
    fold_left
      (fun (acc, n, lco) (str, regle) ->
        let tcr, regle', lc = respecte_regle bcours lco regle in
        (acc @ [ (str, tcr, regle') ], n + tcr, lc))
      ([], 0, lc1) regle2
  in
  (result1, (n1, tcr1), result2, (n2, tcr2), lc2)

(* respecte_conco : cours list -> num_cours list -> num_cours list -> bool   *)
let respecte_conco bcours lco lc =
  let cours_respect_conco' = cours_respect_conco bcours lco lc in
  for_all cours_respect_conco' lc

(* choix_cours_admissibles : cours list -> num_cours list -> session
                             -> fe list -> choix -> num_cours list ->
                             num_cours list list *)
let choix_cours_admissibles_aux bcours lco session lfe ch lc =
  match vers_interv ch with
  | Interv (a, b) as interv when a >= 0 && b >= a && b <= _max ->
      let tcr = total_cr bcours lco
      and ls = if session = Annee then [ Aut; Hiv; Ete ] else [ session ]
      and lfe = if mem FEX lfe || lfe = [] then [ A; Z1; Z3; ZA; H ] else lfe in
      let lca =
        filter
          (fun c ->
            let { pre = pr; equiv = c'; offre = l; _ } = ret_descr bcours c in
            exists (fun (s, fe) -> mem s ls && mem fe lfe) l
            && (not (mem c' lco))
            && (not (mem c lco))
            (* && respecte_pr lco tcr lc pr) *)
            && respecte_pr lco tcr [] pr)
          lc
      in
      construit_liste_choix (nc_eq bcours) eql lca
      |> filter (respecte_interv bcours interv)
  | _ -> []

let choix_cours_admissibles bcours lco session lfe ch lc =
  choix_cours_admissibles_aux bcours lco session lfe ch lc
  |> filter (respect_conco_ccp bcours lco)

(* planifier_session : cours list -> programme -> num_cours list -> session
                       -> fe list -> choix -> string list ->num_cours list ->
                       (num_cours list * string list) list *)
let ret_lc_regles bcours lpat lc_hp l_regles =
  fold_left
    (fun acc (_, _, regle) ->
      match regle with
      | CoursOB (_, l) when l <> [] ->
          acc ++ filter (fun nc -> exists (respecte_motif nc) lpat) l
      | PlageCr (_, n, Cours l) when n > 0 ->
          acc ++ filter (fun nc -> exists (respecte_motif nc) lpat) l
      | PlageCr (_, n, CoursExclus _) when n > 0 ->
          let _, _, lc = respecte_regle bcours lc_hp regle in
          acc ++ (lc_hp -- lc)
      | _ -> acc)
    [] l_regles

let planifier_session bcours ((_, _, _, (n1, _), (n2, _), _) as pgm) lco sess
    lfe ch lpat lc_hp =
  let ch_c_admissibles = choix_cours_admissibles_aux bcours lco sess lfe in
  let conc_obtenues' = conc_obtenues bcours pgm in
  match vers_interv ch with
  | Interv (a, b) as interv when a >= 0 && b >= a && b <= _max ->
      let res1, (_, tcr1), res2, (_, tcr2), _ = ou_en_suis_je bcours pgm lco in
      let lc_ob = ret_lc_regles bcours lpat lc_hp res1 in
      if tcr1 >= n1 && lc_ob = [] && tcr2 >= n2 then []
        (* Plus rien à faire: programme d'études terminé! *)
      else
        let c_obtenues = conc_obtenues' lco in
        let l1 = ch_c_admissibles (Interv (0, b)) lc_ob in
        let l1, tcr_ob, in_st = filtre_ch_max bcours l1 0 (n1 - tcr1) in
        let l =
          if tcr_ob >= b || n2 - tcr2 <= 0 then l1
          else
            let lc_op = ret_lc_regles bcours lpat lc_hp res2 in
            let tcr_op = min (b - tcr_ob) (n2 - tcr2) in
            let l2 = ch_c_admissibles (Interv (0, tcr_op)) lc_op in
            concat
              (map
                 (fun lcob -> map (fun lcop -> lcob ++ lcop) ([] :: l2))
                 (if l1 = [] || (in_st && tcr_ob = 0) then [] :: l1 else l1))
        in
        l
        |> filter (respecte_interv bcours interv)
        |> filter (respect_conco_ccp bcours lco)
        |> map (fun lc -> (lc, conc_obtenues' (lco ++ lc) -- c_obtenues))
  | _ -> []

(* Tp1 (Hiver 2023) *)
(* ---------------- *)
(* -- À IMPLANTER/COMPLÉTER (8 PTS) ----------------------------------------- *)
let eq_pr (pr : prealables) (pr' : prealables) : bool =
  let open List in
  let rec aux pr pr' =
    match (pr, pr') with
    | Aucun, Aucun -> true
    | CRE n, CRE n' -> n = n'
    | CP c, CP c' | CCP c, CCP c' -> c = c'
    | ET l, ET l' | OU l, OU l' ->
        length l = length l' && for_all (fun x -> exists (aux x) l') l
    | _ -> false
  in
  aux pr pr'

(* -- À IMPLANTER/COMPLÉTER (8 PTS) ---------------------------------------- *)
let rec prerequis (bcours : cours list) (c : num_cours) : prealables =
  let rec aux pr =
    match pr with
    | Aucun | CRE _ -> pr
    | CP c | CCP c ->
        let pr' = prerequis bcours c in
        if pr' = Aucun then pr else ET [ pr; pr' ]
    | OU l -> OU (map aux l)
    | ET l -> ET (map aux l)
  in
  let { pre; _ } = ret_descr bcours c in
  aux pre

(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let extrait_lc (e : exigences) : num_cours list =
  match e with
  | CoursOB (_, lc) -> lc
  | PlageCr (_, _, Cours lc) -> lc
  | _ -> []

(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let extrait_lc_exclus (e : exigences) : num_cours list =
  match e with PlageCr (_, _, CoursExclus lc) -> lc | _ -> []

(* -- À IMPLANTER/COMPLÉTER (6 PTS) ----------------------------------------- *)
let verif_e (bcours : cours list) (e : exigences) : (int * int) * bool =
  match e with
  | CoursOB (n, lc) ->
      let tcr = total_cr bcours lc in
      ((tcr, tcr), tcr = n)
  | PlageCr (n1, n2, Cours lc) ->
      let tcr = total_cr bcours lc in
      ((n1, n2), n1 <= n2 && n2 <= tcr)
  | PlageCr (n1, n2, CoursExclus _) -> ((n1, n2), n1 <= n2)

(* -- À IMPLANTER/COMPLÉTER (6 PTS) ----------------------------------------- *)
let existe_motif (bcours : cours list) (pat : num_cours) : bool =
  exists (fun (nc, _) -> respecte_motif nc pat) bcours

(* -- À IMPLANTER/COMPLÉTER (6 PTS) ----------------------------------------- *)
let existe_cours (bcours : cours list) (lc : num_cours list) : bool =
  for_all (existe_motif bcours) lc

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ----------------------------------------- *)
let verif_conc (bcours : cours list) (conc : concentration) : bool =
  let _, (ncr, le) = conc in
  let cr_min, cr_max, cr_valide =
    fold_left
      (fun (mi, ma, eok) e ->
        let (n1, n2), ok = verif_e bcours e in
        (mi + n1, ma + n2, eok && ok))
      (0, 0, true) le
  in
  cr_valide && cr_min <= ncr && ncr <= cr_max

(* -- À IMPLANTER/COMPLÉTER (15 PTS) ---------------------------------------- *)
let verif_credits (bcours : cours list) (pgm : programme) : bool =
  let (_, _, total, _), _, _, (n1, e1), (n2, e2), concs = pgm in

  let e1_cr, e1_ok =
    fold_left
      (fun (t, aok) (_, e) ->
        let (n, _), ok = verif_e bcours e in
        (t + n, aok && ok))
      (0, true) e1
  in

  let e1_cr_ok = n1 = e1_cr in

  let (e2_mi, e2_ma), e2_ok =
    fold_left
      (fun ((mi, ma), aok) (_, e) ->
        let (n, n'), ok = verif_e bcours e in
        ((mi + n, ma + n'), aok && ok))
      ((0, 0), true)
      e2
  in

  let e2_cr_ok = e2_mi <= n2 && n2 <= e2_ma in

  let concs_ok = for_all (verif_conc bcours) concs in

  let total_ok = n1 + n2 == total in

  e1_ok && e2_ok && e1_cr_ok && e2_cr_ok && concs_ok && total_ok

(* -- À IMPLANTER/COMPLÉTER (12 PTS) ---------------------------------------- *)
let coherence_cours_conc (lconc : concentration list) (lc_ob : num_cours list)
    (lc_op : num_cours list) (lc_exclus : num_cours list) : bool =
  (* mélange *)
  for_all
    (fun (_, (_, le)) ->
      let lc = le |> map extrait_lc |> flatten in
      for_all
        (fun c ->
          (not (mem c lc_ob))
          && (mem c lc_op || not (exists (respecte_motif c) lc_exclus)))
        lc)
    lconc

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ---------------------------------------- *)
let verif_pre (bcours : cours list) (lc1 : num_cours list)
    (lc2 : num_cours list) : bool =
  let rec aux pre =
    match pre with
    | Aucun | CRE _ -> true
    | CP c | CCP c -> mem c lc2
    | ET l -> for_all aux l
    | OU l -> exists aux l
  in
  for_all
    (fun c ->
      let { pre; _ } = ret_descr bcours c in
      aux pre)
    lc1

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ---------------------------------------- *)
let coherence_pgm (bcours : cours list) (pgm : programme) : bool =
  let (_, _, _, _), _, _, (_, e1), (_, e2), concs = pgm in
  let lc_ob = map snd e1 |> map extrait_lc |> flatten in
  let lc_op = map snd e2 |> map extrait_lc |> flatten in
  let lc_exclus = map snd e2 |> map extrait_lc_exclus |> flatten in
  let lc_conc =
    map (fun (_, (_, lch)) -> map extrait_lc lch |> flatten) concs |> flatten
  in

  let ec = existe_cours bcours (lc_ob ++ lc_op ++ lc_exclus ++ lc_conc) in
  let vcr = verif_credits bcours pgm in
  let ccc = coherence_cours_conc concs lc_ob lc_op lc_exclus in
  let vp_ob = verif_pre bcours lc_ob lc_ob in
  let vp_op = verif_pre bcours lc_op (lc_ob ++ lc_op) in

  ec && vcr && ccc && vp_ob && vp_op

(* Exemples d'utilisation des différentes fonctions des Tps de l'H-21

   (* TP1 *)
   # ret_sig_num "IFT-3000";;
     - : string * string = ("IFT", "3000")
   # ret_sig_num "IFT-300";;
     Exception: Failure "<IFT-300>: numéro de cours incorrect!".
   # ret_sig_num "IF1-3000";;
     Exception: Failure "<IF1-3000>: numéro de cours incorrect!".
   # ret_sig_num "IF-3000";;
     Exception: Failure "<IF-3000>: numéro de cours incorrect!".
   # ret_sig_num "IFT-3000-";;
     Exception: Failure "<IFT-3000->: numéro de cours incorrect!".
   # ret_sig_num "IFT-4101";;
   - : string * string = ("IFT", "4101")

   # respecte_motif "IFT-3000" "*-*";;
     - : bool = true
   # respecte_motif "IFT-3000" "*-I";;
     - : bool = true
   # respecte_motif "IFT-3000" "*-3*";;
     - : bool = true
   # respecte_motif "IFT-3000" "*-3000";;
     - : bool = true
   # respecte_motif "IFT-3000" "IFT-*";;
     - : bool = true
   # respecte_motif "IFT-3000" "IFT-I";;
     - : bool = true
   # respecte_motif "IFT-3000" "IFT-3*";;
     - : bool = true
   # respecte_motif "IFT-3000" "IFT-3000";;
     - : bool = true
   # respecte_motif "IFT-3000" "*-4*";;
     - : bool = false
   # respecte_motif "IFT-3000" "*-4000";;
     - : bool = false
   # respecte_motif "IFT-3000" "GLO-*";;
     - : bool = false
   # respecte_motif "IFT-3000" "GLO-I";;
     - : bool = false
   # respecte_motif "IFT-3000" "GLO-3*";;
     - : bool = false
   # respecte_motif "IFT-3000" "GLO-3000";;
     - : bool = false
   # respecte_motif "GLO-7050" "GLO-I";;
     - : bool = false
   # respecte_motif "IFT-3000" "IFT-3---";;
     Exception: Failure "<IFT-3--->: mauvais motif!".
   # respecte_motif "IFT-3000" "IFT3000";;
     Exception: Failure "<IFT3000>: mauvais motif!".
   # respecte_motif "IFT-3000" "IF1-3000";;
     Exception: Failure "<IF1-3000>: mauvais motif!".
   # respecte_motif "IFT-300" "*-*";;
     Exception: Failure "<IFT-300>: numéro de cours incorrect!".
   # respecte_motif "XYZ-1234" "*-*";;
     - : bool = true
   # respecte_motif "XYZ-1234" "XYZ-*";;
   - : bool = true
   # respecte_motif "XYZ-1234" "XYZ-1*";;
   - : bool = true

   # existe_pr Aucun Aucun;;
     - : bool = true
   # existe_pr Aucun (CP "IFT-1004");;
     - : bool = false
   # existe_pr Aucun (OU[CP "IFT-1004"]);;
     - : bool = false
   # existe_pr Aucun (CRE 24);;
     - : bool = false
   # existe_pr (CRE 24) (CRE 24);;
     - : bool = true
   # existe_pr (CRE 24) (CRE 25);;
     - : bool = false
   # existe_pr (CRE 24) (CP "IFT-3000");;
     - : bool = false
   # existe_pr (CP "IFT-1004") (CP "IFT-1004");;
     - : bool = true
   # existe_pr (CP "IFT-1004") (CCP "IFT-1004");;
     - : bool = true
   # existe_pr (CCP "IFT-1004") (CCP "IFT-1004");;
     - : bool = true
   # existe_pr (CCP "IFT-1004") (CP "IFT-1004");;
     - : bool = false
   #  existe_pr (CP "IFT-1004") (OU[CRE 12; CP "GIF-1003"; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (CP "IFT-1004") (ET[CRE 12; CP "GIF-1003"; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (CP "IFT-1004") (ET[CRE 12; CP "GIF-1003"; CCP "IFT-1004"]);;
     - : bool = true
   # existe_pr (CCP "IFT-1004") (ET[CRE 12; CP "GIF-1003"; CCP "IFT-1004"]);;
     - : bool = true
   # existe_pr (CCP "IFT-1004") (ET[CRE 12; CP "GIF-1003"; CP "IFT-1004"]);;
     - : bool = false
   # existe_pr (CRE 12) (ET[OU[CP "GIF-1003"; CRE 12]; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (CRE 12) (ET[CRE 12; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (OU [CP "GIF-1003"]) (ET[OU[CP "GIF-1003"; CRE 12]; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (ET [CP "GIF-1003"]) (OU[ET[CP "GIF-1003"; CRE 12]; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (ET [CP "GIF-1003"]) (ET[ET[CP "GIF-1003"; CRE 12]; CP "IFT-1004"]);;
     - : bool = true
   # existe_pr (OU[CP "IFT-1004"; CP "GLO-1901"]) (ET[CRE 24; CP "IFT-3000"; OU[CP "SIO-1000"; CP "SIO-1101"; CP "IFT-1004"; CP "IFT-1903"; CP "GLO-1901"]]);;
     - : bool = true
   # existe_pr (OU[CP "IFT-1004"; CP "GLO-1901"]) (OU[CRE 24; CP "IFT-3000"; OU[CP "SIO-1000"; CP "SIO-1101"; CP "IFT-1004"; CP "IFT-1903"; CP "GLO-1901"]]);;
     - : bool = true

   # total_cr bcours [];;
     - : int = 0
   # total_cr bcours ["IFT-1004"; "GIF-1003"];;
     - : int = 6
   # total_cr bcours ["IFT-1004"; "BIF-1000"];;
     - : int = 4
   # total_cr bcours ["IFT-1111"; "GLO-1111"];;
     - : int = 0
   # total_cr bcours ["IFT-1004"; "BIF-1000"; "IFT-1111"];;
     - : int = 4
   # total_cr bcours ["IFT-2580"];;
     - : int = 0
   # total_cr ~avec_stage:true bcours ["IFT-2580"];;
     - : int = 9

   # lc_dans_pr Aucun;;
     - : Tp1.num_cours list = []
   # lc_dans_pr (CP "IFT-1004");;
     - : Tp1.num_cours list = ["IFT-1004"]
   # lc_dans_pr (OU[CP "IFT-1004"]);;
     - : Tp1.num_cours list = ["IFT-1004"]
   # lc_dans_pr (CRE 24);;
     - : Tp1.num_cours list = []
   # lc_dans_pr (OU[CRE 12; CP "GIF-1003"; CP "IFT-1004"]);;
     - : Tp1.num_cours list = ["GIF-1003"; "IFT-1004"]
   # lc_dans_pr (ET[OU[CP "GIF-1003"; CRE 12]; CP "IFT-1004"]);;
     - : Tp1.num_cours list = ["GIF-1003"; "IFT-1004"]
   # lc_dans_pr (ET[OU[CP "GIF-1003"; CRE 12]; CP "IFT-1004"; CP "GIF-1003"]);;
     - : Tp1.num_cours list = ["GIF-1003"; "IFT-1004"]

   # lc_absents bcours;;
   - : Tp1.num_cours list = []
   # lc_absents
       [("MAT-2200",
         {titre = "Algèbre linéaire avancée"; credit = Cr 3; conco = "";
          pre = CP "MAT-1200"; dept = "MATSTT"; multi = ""; equiv = "";
          offre = [(Hiv, A)]; dom = [MAT]});
        ("STT-2200",
         {titre = "Analyse des données"; credit = Cr 3; conco = "";
          pre =
           ET
            [OU [CP "STT-1000"; CP "STT-2920"; CP "STT-4000"]; CP "MAT-1200";
             OU [CP "STT-1100"; CP "ECN-2080"; CP "GLO-1901"; CP "IFT-1004"]];
          dept = "SIOFSA"; multi = ""; equiv = ""; offre = [(Aut, A)]; dom = [IA]})
       ];;
     - : Tp1.num_cours list =
     ["MAT-1200"; "STT-1000"; "STT-2920"; "STT-4000"; "STT-1100"; "ECN-2080";
      "GLO-1901"; "IFT-1004"]
   # lc_absents
       [("MAT-2200",
         {titre = "Algèbre linéaire avancée"; credit = Cr 3; conco = "";
          pre = Aucun; dept = "MATSTT"; multi = ""; equiv = "";
          offre = [(Hiv, A)]; dom = [MAT]});
        ("STT-2200",
         {titre = "Analyse des données"; credit = Cr 3; conco = "";
          pre = CP "MAT-2200";
          dept = "SIOFSA"; multi = ""; equiv = ""; offre = [(Aut, A)]; dom = [IA]})
       ];;
     - : Tp1.num_cours list = []
   # lc_absents
       [("MAT-2200",
         {titre = "Algèbre linéaire avancée"; credit = Cr 3; conco = "STT-2920";
          pre = Aucun; dept = "MATSTT"; multi = "GIF-1003"; equiv = "";
          offre = [(Hiv, A)]; dom = [MAT]});
        ("STT-2200",
         {titre = "Analyse des données"; credit = Cr 3; conco = "";
          pre = CP "MAT-2200";
          dept = "SIOFSA"; multi = ""; equiv = "IFT-1004"; offre = [(Aut, A)]; dom = [IA]})
       ];;
     - : Tp1.num_cours list = ["STT-2920"; "GIF-1003"; "IFT-1004"]

   # respecte_pr [] 12 [] Aucun;;
     - : bool = true
   # respecte_pr ["IFT-1004"] 12 [] Aucun;;
     - : bool = true
   # respecte_pr ["IFT-1004"] 12 [] (CP "GIF-1003");;
     - : bool = false
   # respecte_pr ["IFT-1004"] 12 [] (CP "IFT-1004");;
     - : bool = true
   # respecte_pr ["IFT-1004"] 12 [] (CP "GLO-1901");;
     - : bool = false
   # respecte_pr ["IFT-1004"] 12 [] (OU[CP "GLO-1901";CP "IFT-1004"]);;
     - : bool = true
   # respecte_pr ["IFT-1004"] 12 [] (OU[CP "GLO-1901";CRE 12]);;
     - : bool = true
   # respecte_pr ["IFT-1004"] 12 [] (ET[CP "GLO-1901";CRE 12]);;
     - : bool = false
   # respecte_pr ["IFT-1004";"GIF-1003"] 12 []
                 (ET[CP "GIF-1003";OU [CP "GLO-1901"; ET[CRE 12; CP "IFT-1004"]]]);;
     - : bool = true

   # map fst (ret_bcours_lmot ["IFT-*"; "GLO-7*"] bcours);;
     - : Tp1.num_cours list =
     ["IFT-1000"; "IFT-1003"; "IFT-1004"; "IFT-1111"; "IFT-1700"; "IFT-1701";
      "IFT-1903"; "IFT-2001"; "IFT-2002"; "IFT-2003"; "IFT-2004"; "IFT-2006";
      "IFT-2007"; "IFT-2008"; "IFT-2101"; "IFT-2102"; "IFT-2103"; "IFT-2580";
      "IFT-3000"; "IFT-3001"; "IFT-3002"; "IFT-3100"; "IFT-3101"; "IFT-3113";
      "IFT-3201"; "IFT-3333"; "IFT-3580"; "IFT-3591"; "IFT-3592"; "IFT-4001";
      "IFT-4003"; "IFT-4011"; "IFT-4022"; "IFT-4102"; "IFT-4201"; "IFT-6001";
      "IFT-6002"; "IFT-6003"; "GLO-7001"; "IFT-7002"; "IFT-7003"; "GLO-7006";
      "IFT-7008"; "IFT-7009"; "IFT-7010"; "IFT-7012"; "IFT-7014"; "IFT-7020";
      "GLO-7021"; "IFT-7022"; "IFT-7023"; "IFT-7022"; "IFT-7025"; "GLO-7027";
      "GLO-7030"; "GLO-7035"; "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["IFT-*"; "GLO-7*"]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-1000"; "IFT-1003"; "IFT-1004"; "IFT-1111"; "IFT-1700"; "IFT-1701";
      "IFT-1903"; "IFT-2001"; "IFT-2002"; "IFT-2003"; "IFT-2004"; "IFT-2006";
      "IFT-2007"; "IFT-2008"; "IFT-2101"; "IFT-2102"; "IFT-2103"; "IFT-2580";
      "IFT-3000"; "IFT-3001"; "IFT-3002"; "IFT-3100"; "IFT-3101"; "IFT-3113";
      "IFT-3201"; "IFT-3333"; "IFT-3580"; "IFT-3591"; "IFT-3592"; "IFT-4001";
      "IFT-4003"; "IFT-4011"; "IFT-4022"; "IFT-4102"; "IFT-4201"; "IFT-6001";
      "IFT-6002"; "IFT-6003"; "GLO-7001"; "IFT-7002"; "IFT-7003"; "GLO-7006";
      "IFT-7008"; "IFT-7009"; "IFT-7010"; "IFT-7012"; "IFT-7014"; "IFT-7020";
      "GLO-7021"; "IFT-7022"; "IFT-7023"; "IFT-7022"; "IFT-7025"; "GLO-7027";
      "GLO-7030"; "GLO-7035"; "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["IFT-3*"]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-3000"; "IFT-3001"; "IFT-3002"; "IFT-3100"; "IFT-3101"; "IFT-3113";
      "IFT-3201"; "IFT-3333"; "IFT-3580"; "IFT-3591"; "IFT-3592"]
   # bcours
     |> ret_bcours_lmot ["GLO-I"]
     |> map fst;;
     - : Tp1.num_cours list =
     ["GLO-1111"; "GLO-1901"; "GLO-2000"; "GLO-2001"; "GLO-2003"; "GLO-2004";
      "GLO-2005"; "GLO-2100"; "GLO-3002"; "GLO-3004"; "GLO-3013"; "GLO-3100";
      "GLO-3101"; "GLO-3102"; "GLO-3112"; "GLO-3202"; "GLO-4000"; "GLO-4001";
      "GLO-4002"; "GLO-4003"; "GLO-4027"; "GLO-4030"; "GLO-4035"]
   # bcours
     |> ret_bcours_lmot ["GLO-6*"; "GLO-7*"]
     |> map fst;;
     - : Tp1.num_cours list =
     ["GLO-7001"; "GLO-7006"; "GLO-7021"; "GLO-7027"; "GLO-7030"; "GLO-7035"]
   # bcours
     |> ret_bcours_lmot ["IFT-6*"; "IFT-7*"]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-6001"; "IFT-6002"; "IFT-6003"; "IFT-7002"; "IFT-7003"; "IFT-7008";
      "IFT-7009"; "IFT-7010"; "IFT-7012"; "IFT-7014"; "IFT-7020"; "IFT-7022";
      "IFT-7023"; "IFT-7022"; "IFT-7025"; "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["*-4*"]
     |> map fst;;
     - : Tp1.num_cours list =
     ["GLO-4000"; "STT-4000"; "GLO-4001"; "IFT-4001"; "GLO-4002"; "GLO-4003";
      "IFT-4003"; "IFT-4011"; "IFT-4022"; "GLO-4027"; "GLO-4030"; "GLO-4035";
      "GIF-4100"; "GIF-4101"; "IFT-4102"; "GIF-4104"; "GIF-4105"; "IFT-4201"]
   # bcours
     |> ret_bcours_lmot []
     |> map fst;;
     - : Tp1.num_cours list = []
   # bcours
     |> ret_bcours_lmot ["*-*"]
     |> length;;
     - : int = 146
   # bcours
     |> ret_bcours_lmot ["*-I*"]
     |> map fst;;
     Exception: Failure "<*-I*>: mauvais motif!".
   # bcours
     |> ret_bcours_lmot ["*-I*";"*-*" ]
     |> map fst;;
     Exception: Failure "<*-I*>: mauvais motif!".
   # bcours
     |> ret_bcours_lmot ["*-*"; "*-I*"]
     |> map fst;;
     Exception: Failure "<*-I*>: mauvais motif!".

   # bcours
     |> ret_bcours_lses_lfe [Ete] []
     |> map fst;;
     - : Tp1.num_cours list =
     ["SIO-1000"; "IFT-1004"; "ANL-1020"; "MQT-1102"; "PHI-1900"; "IFT-2004";
      "IFT-2008"; "ANL-2011"; "IFT-2101"; "IFT-2102"; "IFT-2580"; "ENT-3000";
      "IFT-3000"; "IFT-3002"; "ENT-3010"; "ANL-3020"; "IFT-3333"; "IFT-3580";
      "IFT-3591"; "IFT-3592"; "IFT-4011"; "IFT-7014"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] [A]
     |> map fst;;
     - : Tp1.num_cours list =
     ["ANL-1020"; "ANL-2011"; "IFT-2580"; "ENT-3000"; "ENT-3010"; "ANL-3020";
      "IFT-3580"; "IFT-3591"; "IFT-3592"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] [Z3;ZA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["SIO-1000"; "IFT-1004"; "MQT-1102"; "PHI-1900"; "IFT-2004"; "IFT-2008";
      "IFT-2101"; "IFT-2102"; "IFT-3000"; "IFT-3002"; "IFT-3333"; "IFT-4011";
      "IFT-7014"]
   # bcours
     |> ret_bcours_lses_lfe [Ete;Aut;Hiv] [ZA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["MQT-1102"; "IFT-3000"; "IFT-4022"; "IFT-4201"; "IFT-7022"; "IFT-7201"]
   # bcours
     |> ret_bcours_lses_lfe [Annee] [ZA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["MQT-1102"; "IFT-3000"; "IFT-4022"; "IFT-4201"; "IFT-7022"; "IFT-7201"]
   # bcours
     |> ret_bcours_lses_lfe [] [ZA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["MQT-1102"; "IFT-3000"; "IFT-4022"; "IFT-4201"; "IFT-7022"; "IFT-7201"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] []
     |> map fst;;
     - : Tp1.num_cours list =
     ["SIO-1000"; "IFT-1004"; "ANL-1020"; "MQT-1102"; "PHI-1900"; "IFT-2004";
      "IFT-2008"; "ANL-2011"; "IFT-2101"; "IFT-2102"; "IFT-2580"; "ENT-3000";
      "IFT-3000"; "IFT-3002"; "ENT-3010"; "ANL-3020"; "IFT-3333"; "IFT-3580";
      "IFT-3591"; "IFT-3592"; "IFT-4011"; "IFT-7014"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] [FEX]
     |> map fst;;
     - : Tp1.num_cours list =
     ["SIO-1000"; "IFT-1004"; "ANL-1020"; "MQT-1102"; "PHI-1900"; "IFT-2004";
      "IFT-2008"; "ANL-2011"; "IFT-2101"; "IFT-2102"; "IFT-2580"; "ENT-3000";
      "IFT-3000"; "IFT-3002"; "ENT-3010"; "ANL-3020"; "IFT-3333"; "IFT-3580";
      "IFT-3591"; "IFT-3592"; "IFT-4011"; "IFT-7014"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] [A;Z1;Z3;ZA;H]
     |> map fst;;
     - : Tp1.num_cours list =
     ["SIO-1000"; "IFT-1004"; "ANL-1020"; "MQT-1102"; "PHI-1900"; "IFT-2004";
      "IFT-2008"; "ANL-2011"; "IFT-2101"; "IFT-2102"; "IFT-2580"; "ENT-3000";
      "IFT-3000"; "IFT-3002"; "ENT-3010"; "ANL-3020"; "IFT-3333"; "IFT-3580";
      "IFT-3591"; "IFT-3592"; "IFT-4011"; "IFT-7014"]
   # bcours
     |> ret_bcours_lmot ["IFT-3*"]
     |> ret_bcours_lses_lfe [Ete] []
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-3000"; "IFT-3002"; "IFT-3333"; "IFT-3580"; "IFT-3591"; "IFT-3592"]
   # bcours
     |> ret_bcours_lmot ["IFT-3*"]
     |> ret_bcours_lses_lfe [Ete] [ZA]
     |> map fst;;
     - : Tp1.num_cours list = []

   # bcours
     |> ret_bcours_ldom [IA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-2003"; "STT-2200"; "IFT-4022"; "GLO-4030"; "GIF-4101"; "IFT-4102";
      "GIF-4104"; "IFT-4201"; "GLO-7001"; "IFT-7002"; "IFT-7008"; "IFT-7022";
      "IFT-7022"; "IFT-7025"; "GLO-7030"; "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["IFT-*"]
     |> ret_bcours_ldom [IA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-2003"; "IFT-4022"; "IFT-4102"; "IFT-4201"; "IFT-7002"; "IFT-7008";
      "IFT-7022"; "IFT-7022"; "IFT-7025"; "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["GLO-*"]
     |> ret_bcours_ldom [IA]
     |> map fst;;
     - : Tp1.num_cours list = ["GLO-4030"; "GLO-7001"; "GLO-7030"]
   # bcours
     |> ret_bcours_lmot ["MAT-*"]
     |> ret_bcours_ldom [IA]
     |> map fst;;
     - : Tp1.num_cours list = []
   # bcours
     |> ret_bcours_lmot ["STT-*"]
     |> ret_bcours_ldom [IA]
     |> map fst;;
     - : Tp1.num_cours list = ["STT-2200"]
   # bcours
     |> ret_bcours_lmot ["IFT-*"]
     |> ret_bcours_ldom [MAT;STAT;IA]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-2003"; "IFT-3001"; "IFT-4001"; "IFT-4022"; "IFT-4102"; "IFT-4201";
      "IFT-7002"; "IFT-7008"; "IFT-7012"; "IFT-7022"; "IFT-7022"; "IFT-7025";
      "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["IFT-*"]
     |> ret_bcours_ldom [PROG]
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-1004"; "IFT-1700"; "IFT-1701"; "IFT-1903"; "IFT-2008"; "IFT-3000"]
   # bcours
     |> ret_bcours_lmot ["IFT-*"]
     |> ret_bcours_ldom []
     |> map fst;;
     - : Tp1.num_cours list = []

   # bcours
     |> ret_bcours_pr Aucun
     |> map fst;;
     - : Tp1.num_cours list =
     ["BIF-1000"; "ENT-1000"; "SIO-1000"; "STT-1000"; "BCM-1001"; "GEL-1001";
      "PHY-1001"; "ACT-1002"; "PHY-1002"; "ACT-1003"; "CHM-1003"; "IFT-1003";
      "IFT-1004"; "ANI-1005"; "GPL-1008"; "ANL-1010"; "ENT-1010"; "ANI-1016";
      "STT-1100"; "SIO-1101"; "MQT-1102"; "MAT-1110"; "GLO-1111"; "IFT-1111";
      "MAT-1200"; "MAT-1310"; "IFT-1700"; "IFT-1701"; "MAT-1900"; "MQT-1900";
      "PHI-1900"; "STT-1900"; "GLO-1901"; "IFT-1903"; "MAT-1903"; "PHY-1903";
      "MAT-1919"; "MAT-1920"; "MUS-2012"; "ANL-2020"; "SIO-2107"; "ANL-3020";
      "GLO-3101"; "ANL-3905"; "GLO-4000"; "IFT-6001"; "IFT-6002"; "IFT-6003";
      "GLO-7001"; "IFT-7002"; "IFT-7003"; "GLO-7006"; "IFT-7008"; "IFT-7009";
      "IFT-7010"; "IFT-7012"; "IFT-7014"; "IFT-7020"; "GLO-7021"; "IFT-7022";
      "IFT-7023"; "IFT-7022"; "IFT-7025"; "GLO-7027"; "GLO-7030"; "GLO-7035";
      "IFT-7201"]
   # bcours
     |> ret_bcours_pr (CRE 24)
     |> map fst;;
     - : Tp1.num_cours list = ["IFT-2580"; "IFT-3333"; "IFT-4011"]
   # bcours
     |> ret_bcours_pr (CRE 12)
     |> map fst;;
     - : Tp1.num_cours list = []
   # bcours
     |> ret_bcours_pr (CP "IFT-3000")
     |> map fst;;
   - : Tp1.num_cours list = []
   # bcours
     |> ret_bcours_pr (CP "GLO-3102")
     |> map fst;;
     - : Tp1.num_cours list = ["GLO-3112"; "GLO-3202"]
   # bcours
     |> ret_bcours_pr (OU[CP "IFT-1004"; CP "GLO-1901"])
     |> map fst;;
     - : Tp1.num_cours list =
     ["GIF-1001"; "GIF-1003"; "IFT-2003"; "IFT-2004"; "GLO-2005"; "SIO-2100";
      "SIO-2102"; "SIO-2103"; "SIO-2104"; "STT-2200"; "MAT-2930"; "GLO-4001";
      "GIF-4105"]
   # bcours
     |> ret_bcours_pr (CP "IFT-1004")
     |> map fst;;
     - : Tp1.num_cours list =
     ["BIF-1001"; "GIF-1001"; "GIF-1003"; "IFT-2003"; "IFT-2004"; "GLO-2005";
      "SIO-2100"; "SIO-2102"; "SIO-2103"; "SIO-2104"; "SIO-2105"; "STT-2200";
      "MAT-2930"; "GLO-4001"; "GIF-4105"]
   # bcours
     |> ret_bcours_pr (CP "GIF-1003")
     |> map fst;;
     - : Tp1.num_cours list =
     ["GLO-2004"; "IFT-2007"; "IFT-2008"; "GLO-2100"; "IFT-2103"; "IFT-3000";
      "GIF-3001"; "GLO-3004"; "IFT-3100"; "GIF-3101"]
   # bcours
     |> ret_bcours_lses_lfe [Hiv] [Z3]
     |> ret_bcours_pr (CP "GIF-1003")
     |> map fst;;
     - : Tp1.num_cours list = ["GLO-2100"; "GLO-3004"; "IFT-3100"]
   # bcours
     |> ret_bcours_lses_lfe [Hiv] []
     |> ret_bcours_pr (CP "GIF-1003")
     |> map fst;;
     - : Tp1.num_cours list =
     ["GLO-2004"; "GLO-2100"; "IFT-3000"; "GIF-3001"; "GLO-3004"; "IFT-3100";
      "GIF-3101"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] []
     |> ret_bcours_pr (CP "GIF-1003")
     |> map fst;;
     - : Tp1.num_cours list = ["IFT-2008"; "IFT-3000"]

   # bcours
     |> ret_bcours_admissibles [] 0
     |> map fst;;
     - : Tp1.num_cours list =
     ["BIF-1000"; "ENT-1000"; "SIO-1000"; "STT-1000"; "BCM-1001"; "GEL-1001";
      "PHY-1001"; "ACT-1002"; "PHY-1002"; "ACT-1003"; "CHM-1003"; "IFT-1003";
      "IFT-1004"; "ANI-1005"; "GPL-1008"; "ANL-1010"; "ENT-1010"; "ANI-1016";
      "STT-1100"; "SIO-1101"; "MQT-1102"; "MAT-1110"; "GLO-1111"; "IFT-1111";
      "MAT-1200"; "MAT-1310"; "IFT-1700"; "IFT-1701"; "MAT-1900"; "MQT-1900";
      "PHI-1900"; "STT-1900"; "GLO-1901"; "IFT-1903"; "MAT-1903"; "PHY-1903";
      "MAT-1919"; "MAT-1920"; "MUS-2012"; "ANL-2020"; "SIO-2107"; "ANL-3020";
      "GLO-3101"; "ANL-3905"; "GLO-4000"; "IFT-6001"; "IFT-6002"; "IFT-6003";
      "GLO-7001"; "IFT-7002"; "IFT-7003"; "GLO-7006"; "IFT-7008"; "IFT-7009";
      "IFT-7010"; "IFT-7012"; "IFT-7014"; "IFT-7020"; "GLO-7021"; "IFT-7022";
      "IFT-7023"; "IFT-7022"; "IFT-7025"; "GLO-7027"; "GLO-7030"; "GLO-7035";
      "IFT-7201"]
   # bcours
     |> ret_bcours_admissibles [] 24
     |> map fst;;
     - : Tp1.num_cours list =
     ["BIF-1000"; "ENT-1000"; "SIO-1000"; "STT-1000"; "BCM-1001"; "GEL-1001";
      "PHY-1001"; "ACT-1002"; "PHY-1002"; "ACT-1003"; "CHM-1003"; "IFT-1003";
      "IFT-1004"; "ANI-1005"; "GPL-1008"; "ANL-1010"; "ENT-1010"; "ANI-1016";
      "STT-1100"; "SIO-1101"; "MQT-1102"; "MAT-1110"; "GLO-1111"; "IFT-1111";
      "MAT-1200"; "MAT-1310"; "IFT-1700"; "IFT-1701"; "MAT-1900"; "MQT-1900";
      "PHI-1900"; "STT-1900"; "GLO-1901"; "IFT-1903"; "MAT-1903"; "PHY-1903";
      "MAT-1919"; "MAT-1920"; "MUS-2012"; "ANL-2020"; "SIO-2107"; "IFT-2580";
      "ANL-3020"; "GLO-3101"; "IFT-3333"; "ANL-3905"; "GLO-4000"; "IFT-4011";
      "IFT-6001"; "IFT-6002"; "IFT-6003"; "GLO-7001"; "IFT-7002"; "IFT-7003";
      "GLO-7006"; "IFT-7008"; "IFT-7009"; "IFT-7010"; "IFT-7012"; "IFT-7014";
      "IFT-7020"; "GLO-7021"; "IFT-7022"; "IFT-7023"; "IFT-7022"; "IFT-7025";
      "GLO-7027"; "GLO-7030"; "GLO-7035"; "IFT-7201"]
   # bcours
     |> ret_bcours_admissibles ["GLO-1901"; "MAT-1919"] 6
     |> map fst;;
     - : Tp1.num_cours list =
     ["BIF-1000"; "ENT-1000"; "IFT-1000"; "SIO-1000"; "STT-1000"; "BCM-1001";
      "GEL-1001"; "GIF-1001"; "PHY-1001"; "ACT-1002"; "PHY-1002"; "ACT-1003";
      "CHM-1003"; "GIF-1003"; "IFT-1003"; "ANI-1005"; "GPL-1008"; "ANL-1010";
      "ENT-1010"; "ANI-1016"; "STT-1100"; "SIO-1101"; "MQT-1102"; "MAT-1110";
      "GLO-1111"; "IFT-1111"; "MAT-1200"; "MAT-1310"; "IFT-1700"; "IFT-1701";
      "MAT-1900"; "MQT-1900"; "PHI-1900"; "STT-1900"; "IFT-1903"; "MAT-1903";
      "PHY-1903"; "MAT-1920"; "IFT-2002"; "IFT-2003"; "IFT-2004"; "GLO-2005";
      "MUS-2012"; "ANL-2020"; "SIO-2100"; "SIO-2102"; "SIO-2103"; "SIO-2104";
      "SIO-2107"; "MAT-2930"; "ANL-3020"; "GLO-3101"; "ANL-3905"; "GLO-4000";
      "IFT-6001"; "IFT-6002"; "IFT-6003"; "GLO-7001"; "IFT-7002"; "IFT-7003";
      "GLO-7006"; "IFT-7008"; "IFT-7009"; "IFT-7010"; "IFT-7012"; "IFT-7014";
      "IFT-7020"; "GLO-7021"; "IFT-7022"; "IFT-7023"; "IFT-7022"; "IFT-7025";
      "GLO-7027"; "GLO-7030"; "GLO-7035"; "IFT-7201"]
   # bcours
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 12
     |> map fst;;
     - : Tp1.num_cours list =
     ["BIF-1000"; "ENT-1000"; "IFT-1000"; "SIO-1000"; "STT-1000"; "BCM-1001";
      "GEL-1001"; "GIF-1001"; "PHY-1001"; "ACT-1002"; "PHY-1002"; "ACT-1003";
      "CHM-1003"; "ANI-1005"; "GPL-1008"; "ANL-1010"; "ENT-1010"; "ANI-1016";
      "STT-1100"; "SIO-1101"; "MQT-1102"; "MAT-1110"; "GLO-1111"; "IFT-1111";
      "MAT-1200"; "MAT-1310"; "IFT-1700"; "IFT-1701"; "MAT-1900"; "MQT-1900";
      "PHI-1900"; "STT-1900"; "IFT-1903"; "MAT-1903"; "PHY-1903"; "MAT-1920";
      "IFT-2002"; "IFT-2003"; "GLO-2004"; "IFT-2004"; "GLO-2005"; "IFT-2007";
      "IFT-2008"; "MUS-2012"; "ANL-2020"; "GLO-2100"; "SIO-2100"; "IFT-2102";
      "SIO-2102"; "SIO-2103"; "SIO-2104"; "SIO-2105"; "SIO-2107"; "MAT-2930";
      "IFT-3000"; "GIF-3001"; "ANL-3020"; "IFT-3100"; "GLO-3101"; "ANL-3905";
      "GLO-4000"; "IFT-6001"; "IFT-6002"; "IFT-6003"; "GLO-7001"; "IFT-7002";
      "IFT-7003"; "GLO-7006"; "IFT-7008"; "IFT-7009"; "IFT-7010"; "IFT-7012";
      "IFT-7014"; "IFT-7020"; "GLO-7021"; "IFT-7022"; "IFT-7023"; "IFT-7022";
      "IFT-7025"; "GLO-7027"; "GLO-7030"; "GLO-7035"; "IFT-7201"]
   # bcours
     |> ret_bcours_lmot ["IFT-I"; "GLO-I"]
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 12
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-1000"; "GLO-1111"; "IFT-1111"; "IFT-1700"; "IFT-1701"; "IFT-1903";
      "IFT-2002"; "IFT-2003"; "GLO-2004"; "IFT-2004"; "GLO-2005"; "IFT-2007";
      "IFT-2008"; "GLO-2100"; "IFT-2102"; "IFT-3000"; "IFT-3100"; "GLO-3101";
      "GLO-4000"]
   # bcours
     |> ret_bcours_lses_lfe [Hiv] []
     |> ret_bcours_lmot ["IFT-I"; "GLO-I"]
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 12
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-1000"; "IFT-1700"; "IFT-2002"; "IFT-2003"; "GLO-2004"; "GLO-2005";
      "GLO-2100"; "IFT-3000"; "IFT-3100"]
   # bcours
     |> ret_bcours_lses_lfe [Hiv] [Z3;Z1;ZA]
     |> ret_bcours_lmot ["IFT-I"; "GLO-I"]
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 12
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-1000"; "IFT-1700"; "GLO-2100"; "IFT-3000"; "IFT-3100"]
   # bcours
     |> ret_bcours_lses_lfe [Hiv] [ZA]
     |> ret_bcours_lmot ["IFT-I"; "GLO-I"]
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 12
     |> map fst;;
     - : Tp1.num_cours list = ["IFT-3000"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] []
     |> ret_bcours_lmot ["IFT-*"]
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 18
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-2004"; "IFT-2008"; "IFT-2102"; "IFT-3000"; "IFT-7014"]
   # bcours
     |> ret_bcours_lses_lfe [Ete] []
     |> ret_bcours_lmot ["IFT-*"]
     |> ret_bcours_admissibles ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1003"] 24
     |> map fst;;
     - : Tp1.num_cours list =
     ["IFT-2004"; "IFT-2008"; "IFT-2102"; "IFT-2580"; "IFT-3000"; "IFT-3333";
      "IFT-4011"; "IFT-7014"]
   # bcours
     |> ret_bcours_lmot ["ACT-*"]
     |> ret_bcours_admissibles ["ACT-1003"] 3
     |> map fst;;
     - : Tp1.num_cours list = []
   # bcours
     |> ret_bcours_lmot ["ACT-*"]
     |> ret_bcours_admissibles ["ACT-1002"] 3
     |> map fst;;
     - : Tp1.num_cours list = ["ACT-1003"]
   # bcours
     |> ret_bcours_lmot ["ACT-*"]
     |> ret_bcours_admissibles [] 0
     |> map fst;;
     - : Tp1.num_cours list = ["ACT-1002"; "ACT-1003"]
   # bcours
     |> ret_bcours_lmot ["ACT-*"; "STT-*"]
     |> ret_bcours_admissibles ["ACT-1002"] 3
     |> map fst;;
     - : Tp1.num_cours list =
     ["STT-1000"; "ACT-1003"; "STT-1100"; "STT-1900"; "STT-4000"]


   (* TP2 *)
   # construit_liste_choix (fun e1 e2 -> e1 = 1 && e2 = 3) eql [1;2;3];;
   - : int list list = [[1; 2]; [2; 3]; [1]; [2]; [3]]

   # construit_liste_choix (nc_eq bcours) eql
       ["IFT-3000"; "GLO-1901"; "IFT-2007"; "IFT-1004"; "GLO-2004"];;
   - : Tp2.num_cours list list =
   [["IFT-3000"; "GLO-1901"; "IFT-2007"]; ["IFT-3000"; "GLO-1901"; "GLO-2004"];
    ["IFT-3000"; "IFT-2007"; "IFT-1004"]; ["IFT-3000"; "IFT-1004"; "GLO-2004"];
    ["IFT-3000"; "GLO-1901"]; ["IFT-3000"; "IFT-2007"];
    ["IFT-3000"; "IFT-1004"]; ["IFT-3000"; "GLO-2004"];
    ["GLO-1901"; "IFT-2007"]; ["GLO-1901"; "GLO-2004"];
    ["IFT-2007"; "IFT-1004"]; ["IFT-1004"; "GLO-2004"]; ["IFT-3000"];
    ["GLO-1901"]; ["IFT-2007"]; ["IFT-1004"]; ["GLO-2004"]]

   # respecte_regle bcours
       ["IFT-1004"; "MAT-1919"]
       (CoursOB (12, ["GIF-1003"; "IFT-1004"; "IFT-1000";"MAT-1919"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (6, CoursOB (6, ["GIF-1003"; "IFT-1000"]), [])

   # respecte_regle bcours ["IFT-1004"; "MAT-1919"; "IFT-3000"]
                           (CoursOB (12, ["GIF-1003"; "IFT-1004"; "IFT-1000";"MAT-1919"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (6, CoursOB (6, ["GIF-1003"; "IFT-1000"]), ["IFT-3000"])

   # respecte_regle bcours ["IFT-1004"; "GIF-1003"; "MAT-1919"; "IFT-1000";"IFT-3000"]
                           (CoursOB (12, ["GIF-1003"; "IFT-1004"; "IFT-1000";"MAT-1919"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (12, CoursOB (0, []), ["IFT-3000"])

   # respecte_regle bcours ["IFT-1004"; "MAT-1919"; "IFT-3000"]
                           (PlageCr (3,6, Cours ["GIF-1003"; "IFT-1004"; "IFT-1000";"MAT-1919"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (6, PlageCr (0, 0, Cours ["GIF-1003"; "IFT-1000"]), ["IFT-3000"])

   # respecte_regle bcours ["IFT-1004"; "MAT-1919"; "IFT-3000"]
                           (PlageCr (0,3, Cours ["GIF-1003"; "IFT-1004"; "IFT-1000";"MAT-1919"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (3, PlageCr (0, 0, Cours ["GIF-1003"; "IFT-1000"]), ["IFT-3000"])

   # respecte_regle bcours ["IFT-1004"; "MAT-1919"; "IFT-3000"]
                           (PlageCr (6,9, Cours ["GIF-1003"; "IFT-1004"; "IFT-1000";"MAT-1919"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (6, PlageCr (0, 3, Cours ["GIF-1003"; "IFT-1000"]), ["IFT-3000"])

   # respecte_regle bcours ["IFT-1004"; "MAT-1919"; "IFT-3000"]
                           (PlageCr (6,9, CoursExclus ["GIF-*"; "MAT-*"]));;
   - : int * Tp2.exigences * Tp2.num_cours list =
   (6, PlageCr (0, 3, CoursExclus ["GIF-*"; "MAT-*"]), ["MAT-1919"])

   # conc_obtenues bcours b_ift
       ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
        "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
        "IFT-3000"; "IFT-3001"; "IFT-3101";
        "GLO-2000";"GLO-2001";"GLO-2100";"GLO-2004";"IFT-2004";
        "IFT-2580"; "IFT-3580";
        "ANL-2020";"BIF-1001"; "PHY-1903"; "MAT-1110";
        "GLO-4002"; "IFT-3201";"IFT-4102"; "GLO-4027";"GLO-4035";
        "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
      ];;
   - : string list =
   ["Génie logiciel";
    "Internet et applications Web";
    "Multimédia et développement de jeux vidéo";
    "Traitement de données massives"]

   # ou_en_suis_je bcours b_ift
       ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
        "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
        "IFT-3000"; "IFT-3001"; "IFT-3101";
        "GLO-2000";"GLO-2001";"GLO-2100";"GLO-2004";"IFT-2004";
        "IFT-2580"; "IFT-3580";
        "ANL-2020";"BIF-1001"; "PHY-1903"; "MAT-1110";
        "GLO-4002"; "IFT-3201";"IFT-4102"; "GLO-4027";"GLO-4035";
        "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
      ];;
   - : (string * int * Tp2.exigences) list * (int * int) *
       (string * int * Tp2.exigences) list * (int * int) * Tp2.num_cours list
   =
   ([("Activités de formation communes - règle 0", 36, CoursOB (0, []));
     ("Activités de formation communes - règle 1", 3,
      PlageCr (0, 0, Cours ["IFT-2006"]));
     ("Activités de formation communes - règle 2", 3,
      PlageCr (0, 0, Cours ["IFT-2001"]));
     ("Activités de formation communes - règle 3", 3,
      PlageCr (0, 0, Cours ["IFT-2008"]));
     ("Activités de formation communes - règle 4", 3,
      PlageCr (0, 0, Cours ["IFT-2007"]));
     ("Activités de formation communes - règle 5", 3,
      PlageCr (0, 0, Cours ["GLO-2005"]))],
    (51, 51),
    [("Autres exigences - règle 1", 3, PlageCr (0, 0, Cours ["ANL-3020"]));
     ("Autres exigences - règle 2", 9,
      PlageCr (0, 3,
       CoursExclus
        ["GIF-*"; "GLO-*"; "IFT-*"; "SIO-*"; "FRN-*"; "GEL-1001"; "ANL-2010";
         "ANL-1020"; "ANL-1010"; "ANL-2011"; "ANL-2020"; "ANL-3020"]));
     ("Autres exigences - règle 3", 18,
      PlageCr (0, 0,
       Cours
        ["GIF-3101"; "GIF-4100"; "GIF-4101"; "GIF-4104"; "GIF-4105"; "GLO-2003";
         "GLO-3100"; "GLO-3101"; "GLO-3112"; "GLO-3202"; "GLO-4001"; "GLO-4003";
         "IFT-2101"; "IFT-2102"; "IFT-3002"; "IFT-3100"; "IFT-3113"; "IFT-4001";
         "IFT-4003"; "SIO-2100"; "SIO-2102"; "SIO-2104"; "SIO-2105"; "SIO-2107";
         "SIO-3110"; "IFT-4022"; "IFT-4201"; "GLO-4008"]));
     ("Autres exigences - règle 4", 6, PlageCr (0, 3, Cours ["GLO-3004"]));
     ("Autres exigences - règle 5", 3, PlageCr (0, 3, Cours ["IFT-2003"]))],
    (39, 39), [])

   # ou_en_suis_je bcours b_ift
       ["GIF-1001";"IFT-1004";"IFT-1111";"MAT-1200";"IFT-1000";"STT-1000";"IFT-2002";"IFT-3101";
        "GLO-2000";"GLO-2100";"GLO-2004";"IFT-2580";
        "ANL-2020";"PHY-1903";
        "GLO-4002";"IFT-3201";"GLO-4027";"GLO-4035";"GLO-4030";"GLO-3102";"GLO-4000";"GEL-1001"
       ];;
   - : (string * int * Tp2.exigences) list * (int * int) *
       (string * int * Tp2.exigences) list * (int * int) * Tp2.num_cours list
   =
   ([("Activités de formation communes - règle 0", 21,
      CoursOB (15,
       ["MAT-1919"; "GIF-1003"; "IFT-1003"; "IFT-3000"; "IFT-3001"; "IFT-3580"]));
     ("Activités de formation communes - règle 1", 3,
      PlageCr (0, 0, Cours ["IFT-2006"]));
     ("Activités de formation communes - règle 2", 0,
      PlageCr (3, 3, Cours ["GLO-2001"; "IFT-2001"]));
     ("Activités de formation communes - règle 3", 3,
      PlageCr (0, 0, Cours ["IFT-2008"]));
     ("Activités de formation communes - règle 4", 3,
      PlageCr (0, 0, Cours ["IFT-2007"]));
     ("Activités de formation communes - règle 5", 0,
      PlageCr (3, 3, Cours ["IFT-2004"; "GLO-2005"]))],
    (51, 30),
    [("Autres exigences - règle 1", 3, PlageCr (0, 0, Cours ["ANL-3020"]));
     ("Autres exigences - règle 2", 3,
      PlageCr (6, 9,
       CoursExclus
        ["GIF-*"; "GLO-*"; "IFT-*"; "SIO-*"; "FRN-*"; "GEL-1001"; "ANL-2010";
         "ANL-1020"; "ANL-1010"; "ANL-2011"; "ANL-2020"; "ANL-3020"]));
     ("Autres exigences - règle 3", 15,
      PlageCr (0, 3,
       Cours
        ["GIF-3101"; "GIF-4100"; "GIF-4101"; "GIF-4104"; "GIF-4105"; "GLO-2003";
         "GLO-3100"; "GLO-3101"; "GLO-3112"; "GLO-3202"; "GLO-4001"; "GLO-4003";
         "IFT-2101"; "IFT-2102"; "IFT-2103"; "IFT-3002"; "IFT-3100"; "IFT-3113";
         "IFT-4001"; "IFT-4003"; "SIO-2100"; "SIO-2102"; "SIO-2104"; "SIO-2105";
         "SIO-2107"; "SIO-3110"; "IFT-4022"; "IFT-4201"; "GLO-4008"]));
     ("Autres exigences - règle 4", 6, PlageCr (0, 3, Cours ["GLO-3004"]));
     ("Autres exigences - règle 5", 0,
      PlageCr (3, 6, Cours ["IFT-2003"; "IFT-4102"]))],
    (39, 27), ["GEL-1001"])

   # respecte_conco bcours ["IFT-1004"] ["ACT-1002";"MAT-1919"];;
   - : bool = false
   # respecte_conco bcours ["IFT-1004"] ["ACT-1002";"MAT-1919";"ACT-1003"];;
   - : bool = true
   # respecte_conco bcours ["IFT-1004"] ["MAT-1919";"ACT-1003"];;
   - : bool = true
   # respecte_conco bcours ["IFT-1004";"ACT-1003"] ["ACT-1002";"MAT-1919"];;
   - : bool = true

   # choix_cours_admissibles
       bcours ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [] Plein
       ["GIF-1003";"GLO-2005";"IFT-2004";"IFT-2002";"IFT-1000";"IFT-3000";"IFT-1111"];;
   - : Tp2.num_cours list list =
   [["GIF-1003"; "GLO-2005"; "IFT-2002"; "IFT-1000"]]

   # choix_cours_admissibles
       bcours ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [] (Interv(9,12))
       ["GIF-1003";"GLO-2005";"IFT-2004";"IFT-2002";"IFT-1000";"IFT-3000";"IFT-1111"];;
   - : Tp2.num_cours list list =
   [["GIF-1003"; "GLO-2005"; "IFT-2002"; "IFT-1000"];
    ["GIF-1003"; "GLO-2005"; "IFT-2002"];
    ["GIF-1003"; "GLO-2005"; "IFT-1000"];
    ["GIF-1003"; "IFT-2002"; "IFT-1000"];
    ["GLO-2005"; "IFT-2002"; "IFT-1000"]]

   # choix_cours_admissibles
       bcours ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [Z3;ZA;Z1] (Interv(6,9))
       ["GIF-1003";"GLO-2005";"IFT-2004";"IFT-2002";"IFT-1000";"IFT-3000";"IFT-1111"];;
   - : Tp2.num_cours list list = [["GIF-1003"; "IFT-1000"]]

   # choix_cours_admissibles
       bcours ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [Z3;ZA;Z1] Plein
       ["GIF-1003";"GLO-2005";"IFT-2004";"IFT-2002";"IFT-1000";"IFT-3000";"IFT-1111"];;
   - : Tp2.num_cours list list = []

   # choix_cours_admissibles
       bcours ["MAT-1919";"GIF-1001"] Aut [] Plein
       ["IFT-1004";"IFT-1000";"GLO-1901";"GLO-4000";"IFT-1003"];;
   - : Tp2.num_cours list list =
   [["IFT-1004"; "IFT-1000"; "GLO-4000"; "IFT-1003"];
    ["IFT-1000"; "GLO-1901"; "GLO-4000"; "IFT-1003"]]

   # choix_cours_admissibles
       bcours ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [] Plein
       ["GIF-1003";"GLO-1901";"GLO-2005";"IFT-2002"];;
   - : Tp2.num_cours list list = []

   # choix_cours_admissibles
       bcours ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
               "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
               "IFT-3000"; "IFT-3001"; "IFT-3101"; "GLO-2000"; "GLO-2100";
               "IFT-2004"; "ANL-2020"; "BIF-1001";
               "MAT-1110"; "GLO-4002"; "GLO-4027";"GLO-4035"; "IFT-4201";
               "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
              ] Hiv [] Plein
              ["IFT-2580"; "GLO-2001";"GLO-2004"; "SIO-1000"; "PHY-1903"];;
   - : Tp2.num_cours list list =
   [["GLO-2001"; "GLO-2004"; "SIO-1000"; "PHY-1903"];
    ["IFT-2580"; "GLO-2001"];
    ["IFT-2580"; "GLO-2004"];
    ["IFT-2580"; "SIO-1000"];
    ["IFT-2580"; "PHY-1903"]]

   # planifier_session
       bcours b_ift ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [] Plein
       ["IFT-*";"GLO-*";"GIF-*"] [];;
   - : (Tp2.num_cours list * string list) list =
   [(["GIF-1003"; "IFT-1000"; "IFT-2002"; "IFT-2006"], []);
    (["GIF-1003"; "IFT-1000"; "IFT-2002"; "GLO-2005"], []);
    (["GIF-1003"; "IFT-1000"; "IFT-2006"; "GLO-2005"], []);
    (["GIF-1003"; "IFT-2002"; "IFT-2006"; "GLO-2005"], []);
    (["IFT-1000"; "IFT-2002"; "IFT-2006"; "GLO-2005"], [])]

   # planifier_session
       bcours b_ift ["IFT-1004";"MAT-1919";"IFT-1003";"GIF-1001"] Hiv [] Plein ["*-*"] [];;
   - : (Tp2.num_cours list * string list) list =
   [(["MAT-1200"; "GIF-1003"; "IFT-1000"; "STT-1000"], []);
    (["MAT-1200"; "GIF-1003"; "IFT-1000"; "IFT-2002"], []);
    (["MAT-1200"; "GIF-1003"; "IFT-1000"; "IFT-2006"], []);
    (["MAT-1200"; "GIF-1003"; "IFT-1000"; "GLO-2005"], []);
    (["MAT-1200"; "GIF-1003"; "STT-1000"; "IFT-2002"], []);
    (["MAT-1200"; "GIF-1003"; "STT-1000"; "IFT-2006"], []);
    (["MAT-1200"; "GIF-1003"; "STT-1000"; "GLO-2005"], []);
    (["MAT-1200"; "GIF-1003"; "IFT-2002"; "IFT-2006"], []);
    (["MAT-1200"; "GIF-1003"; "IFT-2002"; "GLO-2005"], []);
    (["MAT-1200"; "GIF-1003"; "IFT-2006"; "GLO-2005"], []);
    (["MAT-1200"; "IFT-1000"; "STT-1000"; "IFT-2002"], []);
    (["MAT-1200"; "IFT-1000"; "STT-1000"; "IFT-2006"], []);
    (["MAT-1200"; "IFT-1000"; "STT-1000"; "GLO-2005"], []);
    (["MAT-1200"; "IFT-1000"; "IFT-2002"; "IFT-2006"], []);
    (["MAT-1200"; "IFT-1000"; "IFT-2002"; "GLO-2005"], []);
    (["MAT-1200"; "IFT-1000"; "IFT-2006"; "GLO-2005"], []);
    (["MAT-1200"; "STT-1000"; "IFT-2002"; "IFT-2006"], []);
    (["MAT-1200"; "STT-1000"; "IFT-2002"; "GLO-2005"], []);
    (["MAT-1200"; "STT-1000"; "IFT-2006"; "GLO-2005"], []);
    (["MAT-1200"; "IFT-2002"; "IFT-2006"; "GLO-2005"], []);
    (["GIF-1003"; "IFT-1000"; "STT-1000"; "IFT-2002"], []);
    (["GIF-1003"; "IFT-1000"; "STT-1000"; "IFT-2006"], []);
    (["GIF-1003"; "IFT-1000"; "STT-1000"; "GLO-2005"], []);
    (["GIF-1003"; "IFT-1000"; "IFT-2002"; "IFT-2006"], []);
    (["GIF-1003"; "IFT-1000"; "IFT-2002"; "GLO-2005"], []);
    (["GIF-1003"; "IFT-1000"; "IFT-2006"; "GLO-2005"], []);
    (["GIF-1003"; "STT-1000"; "IFT-2002"; "IFT-2006"], []);
    (["GIF-1003"; "STT-1000"; "IFT-2002"; "GLO-2005"], []);
    (["GIF-1003"; "STT-1000"; "IFT-2006"; "GLO-2005"], []);
    (["GIF-1003"; "IFT-2002"; "IFT-2006"; "GLO-2005"], []);
    (["IFT-1000"; "STT-1000"; "IFT-2002"; "IFT-2006"], []);
    (["IFT-1000"; "STT-1000"; "IFT-2002"; "GLO-2005"], []);
    (["IFT-1000"; "STT-1000"; "IFT-2006"; "GLO-2005"], []);
    (["IFT-1000"; "IFT-2002"; "IFT-2006"; "GLO-2005"], []);
    (["STT-1000"; "IFT-2002"; "IFT-2006"; "GLO-2005"], [])]

   # planifier_session
       bcours b_ift ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
                     "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
                     "IFT-3000"; "IFT-3001"; "IFT-3101"; "GLO-2000"; "GLO-2100";
                     "IFT-2004"; "IFT-2580"; "IFT-3580"; "ANL-2020"; "BIF-1001";
                     "MAT-1110"; "GLO-4002"; "GLO-4027";"GLO-4035"; "IFT-4201";
                     "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
                    ]
       Hiv [] Plein ["IFT-*";"GLO-*"] ["SIO-1000"; "PHY-1903"];;
   - : (Tp2.num_cours list * string list) list =
   [(["GLO-2001"; "GLO-2004"; "SIO-1000"; "PHY-1903"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "GLO-3004"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "IFT-2003"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "IFT-4102"],
     ["Traitement de données massives"]);
    (["GLO-2001"; "GLO-2004"; "PHY-1903"; "GLO-3004"], []);
    (["GLO-2001"; "GLO-2004"; "PHY-1903"; "IFT-2003"], []);
    (["GLO-2001"; "GLO-2004"; "PHY-1903"; "IFT-4102"],
     ["Multimédia et développement de jeux vidéo";
      "Traitement de données massives"]);
    (["GLO-2001"; "GLO-2004"; "GLO-3004"; "IFT-2003"], []);
    (["GLO-2001"; "GLO-2004"; "GLO-3004"; "IFT-4102"],
     ["Traitement de données massives"]);
    (["GLO-2001"; "GLO-2004"; "IFT-2003"; "IFT-4102"],
     ["Traitement de données massives"])]

   # conc_obtenues bcours b_ift
       ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
        "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
        "IFT-3000"; "IFT-3001"; "IFT-3101"; "GLO-2000"; "GLO-2100";
        "IFT-2004"; "IFT-2580"; "IFT-3580"; "ANL-2020"; "BIF-1001";
        "MAT-1110"; "GLO-4002"; "GLO-4027";"GLO-4035"; "IFT-4201";
        "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
       ];;
   - : string list = ["Génie logiciel"]

   # planifier_session
       bcours b_ift
         ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
          "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
          "IFT-3000"; "IFT-3001"; "IFT-3101"; "GLO-2000"; "GLO-2100";
          "IFT-2004"; "ANL-2020"; "BIF-1001";
          "MAT-1110"; "GLO-4002"; "GLO-4027";"GLO-4035"; "IFT-4201";
          "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
         ]
         Hiv [] Plein ["IFT-*";"GLO-*"] ["SIO-1000"; "PHY-1903"];;
   - : (Tp2.num_cours list * string list) list =
   [(["IFT-2580"; "GLO-2001"], []);
    (["IFT-2580"; "GLO-2004"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "PHY-1903"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "GLO-3004"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "IFT-2003"], []);
    (["GLO-2001"; "GLO-2004"; "SIO-1000"; "IFT-4102"],
     ["Traitement de données massives"]);
    (["GLO-2001"; "GLO-2004"; "PHY-1903"; "GLO-3004"], []);
    (["GLO-2001"; "GLO-2004"; "PHY-1903"; "IFT-2003"], []);
    (["GLO-2001"; "GLO-2004"; "PHY-1903"; "IFT-4102"],
     ["Multimédia et développement de jeux vidéo";
      "Traitement de données massives"]);
    (["GLO-2001"; "GLO-2004"; "GLO-3004"; "IFT-2003"], []);
    (["GLO-2001"; "GLO-2004"; "GLO-3004"; "IFT-4102"],
     ["Traitement de données massives"]);
    (["GLO-2001"; "GLO-2004"; "IFT-2003"; "IFT-4102"],
     ["Traitement de données massives"])]

   # ou_en_suis_je
       bcours b_ift ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919";
                     "GIF-1003"; "IFT-1000"; "IFT-1003"; "STT-1000"; "IFT-2002";
                     "IFT-3000"; "IFT-3001"; "IFT-3101"; "GLO-2000"; "GLO-2100";
                     "IFT-2004"; "ANL-2020"; "BIF-1001";
                     "MAT-1110"; "GLO-4002"; "GLO-4027";"GLO-4035"; "IFT-4201";
                     "GLO-4030"; "GLO-3102"; "GLO-4000"; "IFT-2103"
                    ];;
                 - : (string * int * Tp2.exigences) list * (int * int) *
       (string * int * Tp2.exigences) list * (int * int) * Tp2.num_cours list
   =
   ([("Activités de formation communes - règle 0", 36,
      CoursOB (0, ["IFT-2580"; "IFT-3580"]));
     ("Activités de formation communes - règle 1", 3,
      PlageCr (0, 0, Cours ["IFT-2006"]));
     ("Activités de formation communes - règle 2", 0,
      PlageCr (3, 3, Cours ["GLO-2001"; "IFT-2001"]));
     ("Activités de formation communes - règle 3", 3,
      PlageCr (0, 0, Cours ["IFT-2008"]));
     ("Activités de formation communes - règle 4", 0,
      PlageCr (3, 3, Cours ["GLO-2004"; "IFT-2007"]));
     ("Activités de formation communes - règle 5", 3,
      PlageCr (0, 0, Cours ["GLO-2005"]))],
    (51, 45),
    [("Autres exigences - règle 1", 3, PlageCr (0, 0, Cours ["ANL-3020"]));
     ("Autres exigences - règle 2", 6,
      PlageCr (3, 6,
       CoursExclus
        ["GIF-*"; "GLO-*"; "IFT-*"; "SIO-*"; "FRN-*"; "GEL-1001"; "ANL-2010";
         "ANL-1020"; "ANL-1010"; "ANL-2011"; "ANL-2020"; "ANL-3020"]));
     ("Autres exigences - règle 3", 18,
      PlageCr (0, 0,
       Cours
        ["GIF-3101"; "GIF-4100"; "GIF-4101"; "GIF-4104"; "GIF-4105"; "GLO-2003";
         "GLO-3100"; "GLO-3101"; "GLO-3112"; "GLO-3202"; "GLO-4001"; "GLO-4003";
         "IFT-2101"; "IFT-2102"; "IFT-3002"; "IFT-3100"; "IFT-3113"; "IFT-4001";
         "IFT-4003"; "SIO-2100"; "SIO-2102"; "SIO-2104"; "SIO-2105"; "SIO-2107";
         "SIO-3110"; "IFT-4022"; "GLO-4008"]));
     ("Autres exigences - règle 4", 3,
      PlageCr (3, 6, Cours ["GLO-3004"; "IFT-3201"]));
     ("Autres exigences - règle 5", 0,
      PlageCr (3, 6, Cours ["IFT-2003"; "IFT-4102"]))],
    (39, 30), [])
*)
