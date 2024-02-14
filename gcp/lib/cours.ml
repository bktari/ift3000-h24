open Gcp

(* ------------------------------------------------------------------------- *)
(* Données: bassin de cours                                                  *)
(* ------------------------------------------------------------------------- *)

let bcours : cours list =
  [
    ( "FRN-0100",
      {
        titre = "Français écrit";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "LANG-FLSH";
        multi = "";
        equiv = "";
        offre = [ (Aut, A); (Aut, Z1); (Hiv, A); (Hiv, Z1); (Ete, Z1) ];
        dom = [ LANG ];
      } );
    ( "BIF-1000",
      {
        titre = "Profession en bio-informatique";
        credit = Cr 1;
        conco = "";
        pre = Aucun;
        dept = "BIOIFT-FSG";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ BIO ];
      } );
    ( "CHM-1000",
      {
        titre = "Structure des atomes et des molécules";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "CHM-FSG";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ CHM ];
      } );
    ( "CTB-1000",
      {
        titre = "Comptabilité générale";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "CPT-FSA";
        multi = "";
        equiv = "";
        offre =
          [
            (Aut, A);
            (Aut, Z3);
            (Aut, Z1);
            (Hiv, A);
            (Hiv, Z3);
            (Hiv, Z1);
            (Ete, Z1);
          ];
        dom = [ CHM ];
      } );
    ( "DDU-1000",
      {
        titre = "Fondements du développement durable";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ScPOL-FSS";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z1); (Ete, Z1); (Hiv, Z1) ];
        dom = [ GEN ];
      } );
    ( "ECN-1000",
      {
        titre = "Principes de microéconomie";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECO-FSS";
        multi = "";
        equiv = "GSF-1020";
        offre = [ (Aut, A); (Aut, H); (Hiv, ZA); (Ete, Z1) ];
        dom = [ ECO ];
      } );
    ( "ENT-1000",
      {
        titre = "Savoir entreprendre : la passion de créer et d'agir";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MNG-FSA";
        multi = "";
        equiv = "ENT-1010";
        offre = [ (Hiv, A); (Hiv, Z1); (Aut, A); (Aut, Z1); (Ete, Z1) ];
        dom = [ ENT ];
      } );
    (* ------------------------------ Reprendre ici ------------------------- *)
    ( "IFT-1000",
      {
        titre = "Logique et techniques de preuve";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "MAT-1310"; CP "MAT-1919" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, A) ];
        dom = [ LOG ];
      } );
    ( "GSO-1000",
      {
        titre = "Opérations et logistique";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "MQT-1102";
              CP "MQT-1100";
              CP "STT-2920";
              CP "MAT-1915";
              CP "STT-1900";
              CP "STT-1000";
            ];
        dept = "OSDFSA";
        multi = "";
        equiv = "GSO-1100";
        offre = [ (Hiv, A); (Aut, Z1); (Hiv, Z1); (Aut, A); (Ete, Z1) ];
        dom = [ GEN ];
      } );
    ( "GSF-1000",
      {
        titre = "Finance";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "OSDFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Hiv, Z1); (Aut, Z1); (Ete, Z1) ];
        dom = [ GEN ];
      } );
    ( "MCB-1000",
      {
        titre = "Microbiologie générale";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOFSG";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ BIO ];
      } );
    ( "MRK-1000",
      {
        titre = "Marketing";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MRKFSA";
        multi = "";
        equiv = "MRK-1101";
        offre = [ (Hiv, Z3); (Aut, Z3); (Ete, Z1) ];
        dom = [ GEN ];
      } );
    ( "MNG-1000",
      {
        titre = "L'entreprise et sa gestion";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MRKFSA";
        multi = "";
        equiv = "MNG-1002";
        offre =
          [
            (Hiv, Z3);
            (Hiv, ZA);
            (Hiv, A);
            (Aut, Z3);
            (Aut, ZA);
            (Aut, A);
            (Ete, Z1);
          ];
        dom = [ GEN ];
      } );
    ( "PHY-1000",
      {
        titre = "Introduction à l'astrophysique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "RLT-1000",
      {
        titre = "Fondements en relations industrielles";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "RIDFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, ZA); (Aut, A) ];
        dom = [ GEN ];
      } );
    ( "SIO-1000",
      {
        titre = "Systèmes et technologies de l'information";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3); (Aut, Z3); (Ete, Z3) ];
        dom = [ SIO ];
      } );
    ( "STT-1000",
      {
        titre = "Probabilités et statistique ";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3); (Aut, A) ];
        dom = [ STAT ];
      } );
    ( "BCM-1001",
      {
        titre = "Biochimie structurale";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ BIO ];
      } );
    ( "BIF-1001",
      {
        titre = "Introduction à la bio-informatique";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "BCM-1001"; CP "IFT-1004" ];
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ BIO ];
      } );
    ( "GEL-1000",
      {
        titre = "Circuits";
        credit = Cr 3;
        conco = "";
        pre = OU [ CCP "MAT-1900"; CCP "PHY-1002" ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "GEL-1001",
      {
        titre = "Design I (méthodologie)";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ ANA_CON ];
      } );
    ( "GIF-1001",
      {
        titre = "Ordinateurs : structure et applications";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1004"; CP "GLO-1901" ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, Z3) ];
        dom = [ SYS ];
      } );
    ( "PHY-1001",
      {
        titre = "Physique mathématique I";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "SIO-1001",
      {
        titre = "Gestion du numérique dans les organisations";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, H); (Aut, H) ];
        dom = [ MAT ];
      } );
    ( "ACT-1002",
      {
        titre = "Analyse probabiliste des risques actuariels";
        credit = Cr 3;
        conco = "ACT-1003";
        pre = Aucun;
        dept = "ACTFSG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "GIF-1002",
      {
        titre = "Circuits logiques";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "MNG-1002",
      {
        titre = "Management";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MRKFSA";
        multi = "";
        equiv = "MNG-1000";
        offre = [ (Aut, A) ];
        dom = [ GEN ];
      } );
    ( "PHY-1002",
      {
        titre = "Physique mathématique II";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "ACT-1003",
      {
        titre = "Compléments de mathématiques";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ACTFSG";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "BCM-1003",
      {
        titre = "Métabolisme et régulation";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "BCM-1001"; CP "CHM-1003" ];
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ BIO ];
      } );
    ( "CHM-1003",
      {
        titre = "Chimie organique I";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "CHMFSG";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ BIO ];
      } );
    ( "GIF-1003",
      {
        titre = "Programmation avancée en C++ pour ingénieurs";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1004"; CP "GLO-1901" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "IFT-1006";
        offre = [ (Hiv, A); (Aut, A) ];
        dom = [ PROG ];
      } );
    ( "GMC-1003",
      {
        titre = "Introduction à la mécanique des fluides";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "MAT-1910"; CP "PHY-1001" ];
        dept = "GMCFSG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GEN ];
      } );
    ( "IFT-1003",
      {
        titre = "Analyse et conception de systémes d'information";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3); (Aut, Z3) ];
        dom = [ ANA_CON ];
      } );
    ( "PHY-1003",
      {
        titre = "Mécanique et relativité restreinte";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "CHM-1004",
      {
        titre = "Thermodynamique et cinétique chimique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "CHMFSG";
        multi = "";
        equiv = "CHM-1905";
        offre = [ (Hiv, H) ];
        dom = [ BIO ];
      } );
    ( "IFT-1004",
      {
        titre = "Introduction à la programmation";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "GLO-1901";
        offre = [ (Hiv, Z3); (Hiv, A); (Aut, A); (Aut, Z3); (Ete, Z3) ];
        dom = [ PROG ];
      } );
    ( "SBM-1004",
      {
        titre = "Introduction à la génétique moléculaire";
        credit = Cr 3;
        conco = "";
        pre = CP "BCM-1001";
        dept = "OPHMED";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GEN ];
      } );
    ( "IFT-1006",
      {
        titre = "Programmation avancée en C++";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1004"; CP "GLO-1901" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "GIF-1003";
        offre = [ (Ete, Z3); (Hiv, Z3); (Aut, Z3) ];
        dom = [ PROG ];
      } );
    ( "ANI-1005",
      {
        titre = "Animation 3D II";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "DESARCP ";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GAME ];
      } );
    ( "BCM-1005",
      {
        titre = "Génétique moléculaire I ";
        credit = Cr 3;
        conco = "";
        pre = CP "BCM-1001";
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ BIO ];
      } );
    ( "BIO-1006",
      {
        titre = "Biostatistique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ BIO; STAT ];
      } );
    ( "PHY-1006",
      {
        titre = "Physique quantique";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "PHY-1002"; CP "MAT-1900" ];
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SYS ];
      } );
    ( "PHY-1007",
      {
        titre = "électromagnétisme";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "PHY-1001"; CP "MAT-1910" ];
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SYS ];
      } );
    ( "GPL-1008",
      {
        titre = "Méthodes statistiques pour sciences sociales";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "ANL-1010",
      {
        titre = "Basic English I";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECLANG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A) ];
        dom = [ LANG ];
      } );
    ( "ECN-1010",
      {
        titre = "Principes de macroéconomie";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ PROG ];
      } );
    ( "ENT-1010",
      {
        titre = "Being Entrepreneurial: Passion for creation and action";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MNGFSA";
        multi = "";
        equiv = "ENT-1000";
        offre = [ (Hiv, A) ];
        dom = [ ENT ];
      } );
    ( "ANI-1016",
      {
        titre = "Fondements pratiques en animation I";
        credit = Cr 6;
        conco = "";
        pre = Aucun;
        dept = "DESARCP ";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GAME ];
      } );
    ( "ANI-1018",
      {
        titre = "Fondements pratiques en animation II";
        credit = Cr 6;
        conco = "";
        pre = CP "ANI-1016";
        dept = "DESARCP ";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GAME ];
      } );
    ( "ANL-1020",
      {
        titre = "Basic English II";
        credit = Cr 3;
        conco = "";
        pre = CP "ANL-1010";
        dept = "ECLANG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Ete, A) ];
        dom = [ LANG ];
      } );
    ( "GSF-1020",
      {
        titre = "économie de l'entreprise";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECOFSA";
        multi = "";
        equiv = "ECN-1000";
        offre = [ (Hiv, A); (Aut, Z1); (Hiv, Z1); (Aut, A); (Ete, Z1) ];
        dom = [ GEN ];
      } );
    ( "GSO-1100",
      {
        titre = "Operations and Logistics";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "MQT-1102";
              CP "MQT-1100";
              CP "STT-2920";
              CP "MAT-1915";
              CP "STT-1900";
              CP "STT-1000";
            ];
        dept = "OSDFSA";
        multi = "";
        equiv = "GSO-1000";
        offre = [ (Hiv, A); (Hiv, Z1) ];
        dom = [ GEN ];
      } );
    ( "MQT-1100",
      {
        titre = "Probability and Statistics for Business";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "MQT-1102";
        offre = [ (Aut, Z1) ];
        dom = [ STAT ];
      } );
    ( "STT-1100",
      {
        titre = "Introduction aux principaux logiciels statistiques";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "MRK-1101",
      {
        titre = "Marketing management";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MRKFSA";
        multi = "";
        equiv = "MRK-1000";
        offre = [ (Hiv, Z3); (Aut, Z3); (Aut, H); (Ete, Z1) ];
        dom = [ GEN ];
      } );
    ( "SIO-1101",
      {
        titre = "Information Systems and Technologies";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ SIO ];
      } );
    ( "MQT-1102",
      {
        titre = "Probabilités et statistique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "MQT-1100";
        offre = [ (Aut, Z3); (Aut, A); (Hiv, Z3); (Hiv, A); (Ete, Z1) ];
        dom = [ STAT ];
      } );
    ( "MAT-1110",
      {
        titre = "Calcul des fonctions de plusieurs variables";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "GLO-1111",
      {
        titre = "Pratique du génie logiciel";
        credit = Cr 0;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ GEN ];
      } );
    ( "IFT-1111",
      {
        titre = "Pratique de l'informatique";
        credit = Cr 0;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A); (Aut, Z3) ];
        dom = [ GEN ];
      } );
    ( "MAT-1200",
      {
        titre = "Introduction à l'algébre linéaire";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "MAT-1300",
      {
        titre = "éléments de mathématiques";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "MAT-1310",
      {
        titre = "Mathématiques discrétes";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ MAT ];
      } );
    ( "GIN-1500",
      {
        titre = "Structure et organisation des entreprises";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "LETLAN";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ LANG ];
      } );
    ( "STT-1500",
      {
        titre = "Probabilités";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "MAT-1110"; CCP "MAT-1910" ];
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "RLT-1700",
      {
        titre = "Aspects administratifs et humains de la gestion";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "RIDFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z1); (Aut, H) ];
        dom = [ GEN ];
      } );
    ( "IFT-1700",
      {
        titre = "Programmation de base en Visual Basic .Net";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3) ];
        dom = [ PROG ];
      } );
    ( "IFT-1701",
      {
        titre = "Introduction à l'algorithmique et à la programmation";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ PROG ];
      } );
    ( "GEL-1799",
      {
        titre = "Dangers de l'électricité";
        credit = Cr 0;
        conco = "";
        pre = Aucun;
        dept = "GELGIF";
        multi = "";
        equiv = "GEL-4799";
        offre = [ (Aut, Z1); (Hiv, Z1) ];
        dom = [ SYS ];
      } );
    ( "MAT-1900",
      {
        titre = "Mathématiques de l'ingénieur I";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Aut, Z3) ];
        dom = [ MAT ];
      } );
    ( "MQT-1900",
      {
        titre = "Méthodes quantitatives pour économistes";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ STAT ];
      } );
    ( "PHI-1900",
      {
        titre = "Principes de logique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHIPHI";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, A); (Aut, Z3); (Ete, Z3) ];
        dom = [ LOG ];
      } );
    ( "STT-1900",
      {
        titre = "Méthodes statistiques pour ingénieurs";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A) ];
        dom = [ STAT ];
      } );
    ( "GLO-1901",
      {
        titre = "Introduction à la programmation avec Python";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "GELGIF";
        multi = "";
        equiv = "IFT-1004";
        offre = [ (Aut, Z3); (Hiv, Z3) ];
        dom = [ PROG ];
      } );
    ( "IFT-1901",
      {
        titre = "Technologies en géomatique I";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "GEOFOR";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3); (Aut, H) ];
        dom = [ PROG ];
      } );
    ( "IFT-1902",
      {
        titre = "Programmation avec R pour l'analyse de données";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ACT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, H); (Aut, H) ];
        dom = [ PROG ];
      } );
    ( "BCM-1903",
      {
        titre = "Biochimie et métabolisme";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z1) ];
        dom = [ BIO ];
      } );
    ( "IFT-1903",
      {
        titre = "Informatique pour l'ingénieur";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "GCLFSG";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ PROG ];
      } );
    ( "MAT-1903",
      {
        titre = "Calcul matriciel";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "PHY-1903",
      {
        titre = "Physique générale";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [];
      } );
    ( "CHM-1905",
      {
        titre = "Thermodynamique et cinétique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "CHMFSG";
        multi = "";
        equiv = "CHM-1004";
        offre = [ (Hiv, A) ];
        dom = [ BIO ];
      } );
    ( "BIO-1910",
      {
        titre = "écologie et pollution";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z1) ];
        dom = [ GEN ];
      } );
    ( "MAT-1910",
      {
        titre = "Mathématiques de l'ingénieur II";
        credit = Cr 3;
        conco = "";
        pre = OU [ CCP "MAT-1900"; CCP "MAT-1920" ];
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3); (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "MAT-1915",
      {
        titre = "Biostatistique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BOIFFG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3) ];
        dom = [ BIO ];
      } );
    ( "MAT-1919",
      {
        titre = "Mathématiques pour informaticien";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3); (Aut, A); (Aut, Z3) ];
        dom = [ MAT ];
      } );
    ( "MAT-1920",
      {
        titre = "Mathématiques pour scientifiques";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "STT-1920",
      {
        titre = "Méthodes statistiques";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MATSTT";
        multi = "";
        equiv = "MQT-1102";
        offre = [ (Aut, A); (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "ACT-2000",
      {
        titre = "Analyse statistique des risques actuariels";
        credit = Cr 3;
        conco = "";
        pre = CP "ACT-1002";
        dept = "ACTFSG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "GLO-2000",
      {
        titre = "Réseaux et ingénierie de la transmission des données";
        credit = Cr 3;
        conco = "";
        pre = CP "GIF-1001";
        dept = "IFTGLO";
        multi = "";
        equiv = "IFT-2006";
        offre = [ (Aut, A) ];
        dom = [ NET ];
      } );
    ( "SIO-2000",
      {
        titre = "Systéme d'information et comptabilité";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "STT-1500"; CP "ACT-1002" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3); (Hiv, Z3); (Hiv, H) ];
        dom = [ NET ];
      } );
    ( "STT-2000",
      {
        titre = "Statistique mathématique";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "STT-1500"; CP "ACT-1002" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ NET ];
      } );
    ( "GLO-2001",
      {
        titre = "Systémes d'exploitation pour ingénieurs";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "GIF-1001"; OU [ CP "GLO-2100"; CP "IFT-2008" ] ];
        dept = "IFTGLO";
        multi = "";
        equiv = "IFT-2001";
        offre = [ (Hiv, A) ];
        dom = [ SYS ];
      } );
    ( "IFT-2001",
      {
        titre = "Systémes d'exploitation";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "GIF-1001"; OU [ CP "IFT-2008"; CP "GLO-2100" ] ];
        dept = "IFTGLO";
        multi = "";
        equiv = "GLO-2001";
        offre = [ (Aut, Z3) ];
        dom = [ SYS ];
      } );
    ( "ACT-2002",
      {
        titre = "Méthodes numériques en actuariat";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "ACT-1002"; CP "IFT-4902" ];
        dept = "ACTFSG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ STAT ];
      } );
    ( "IFT-2002",
      {
        titre = "Informatique théorique";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "MAT-1919"; CP "MAT-1310" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, Z3) ];
        dom = [ THEO ];
      } );
    ( "BIO-2003",
      {
        titre = "Biologie moléculaire";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "BCM-1001"; CP "BCM-1903" ];
        dept = "BIOINF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ BIO ];
      } );
    ( "GLO-2003",
      {
        titre = "Processus du génie logiciel";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GLO-2004"; CP "IFT-2007" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, ZA) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-2003",
      {
        titre = "Intelligence artificielle I";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              (* OU [ CP "IFT-1000"; CP "MAT-1310"; CP "MAT-1919" ]; *)
              CP "IFT-1000";
              OU [ CP "IFT-1004"; CP "GLO-1901" ];
            ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3); (Hiv, A) ];
        dom = [ IA ];
      } );
    ( "GLO-2004",
      {
        titre = "Génie logiciel orienté objet";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIF-1003"; CP "IFT-1006" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "IFT-2007";
        offre = [ (Aut, Z3) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-2004",
      {
        titre = "Modéles et langages des bases de données";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1004"; CP "GLO-1901" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "GLO-2005";
        offre = [ (Aut, Z3); (Ete, Z3) ];
        dom = [ BD ];
      } );
    ( "GLO-2005",
      {
        titre = "Modéles et langages des bases de données pour ingénieurs";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1004"; CP "GLO-1901" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "IFT-2004";
        offre = [ (Hiv, ZA) ];
        dom = [ BD ];
      } );
    ( "IFT-2006",
      {
        titre = "Téléinformatique";
        credit = Cr 3;
        conco = "";
        pre = CP "GIF-1001";
        dept = "IFTGLO";
        multi = "";
        equiv = "GLO-2000";
        offre = [ (Hiv, Z3) ];
        dom = [ NET ];
      } );
    ( "IFT-2007",
      {
        titre = "Analyse et conception des systémes orientés objets";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIF-1003"; CP "IFT-1006" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "GLO-2004";
        offre = [ (Aut, Z3); (Hiv, H) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-2008",
      {
        titre = "Algorithmes et structures de données";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIF-1003"; CP "IFT-1006" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "GLO-2100";
        offre = [ (Ete, Z3); (Hiv, Z3); (Aut, Z3) ];
        dom = [ PROG ];
      } );
    ( "ANL-2010",
      {
        titre = "Intermediate English I";
        credit = Cr 3;
        conco = "";
        pre = CP "ANL-1020";
        dept = "ECLANG";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3) ];
        dom = [ LANG ];
      } );
    ( "GIN-2010",
      {
        titre = "Gestion opérationnelle des systémes d'entreprise";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              ET [ CP "GIN-1500"; CP "STT-1900" ];
              ET [ CP "GIN-1500"; CP "STT-1000" ];
              ET [ CP "GMC-3009"; CP "STT-1900" ];
              ET [ CP "GMC-3009"; CP "STT-1000" ];
            ];
        dept = "LETLAN";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ LANG ];
      } );
    ( "MUS-2012",
      {
        titre = "Musique interactive";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "MUSMUS";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GAME ];
      } );
    ( "ANL-2020",
      {
        titre = "Intermediate English II";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "LETLAN";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3) ];
        dom = [ LANG ];
      } );
    ( "ANL-2011",
      {
        titre = "Intensive English II";
        credit = Cr 3;
        conco = "";
        pre = CP "ANL-1010";
        dept = "ECLANG";
        multi = "";
        equiv = "";
        offre = [ (Ete, A) ];
        dom = [ LANG ];
      } );
    ( "ECN-2080",
      {
        titre = "Introduction à la programmation scientifique pour économiste";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              ET [ CP "GPL-1008"; CP "MQT-1900" ];
              ET [ CP "MAT-1900"; CP "STT-1500" ];
            ];
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ PROG ];
      } );
    ( "ECN-2090",
      {
        titre = "Logiciels et analyse de données";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              CP "ECN-1000";
              CP "ECN-1010";
              OU [ CCP "GPL-1008"; CCP "STT-1000"; CCP "STT-1500" ];
            ];
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Hiv, H) ];
        dom = [ PROG ];
      } );
    ( "GLO-2100",
      {
        titre = "Algorithmes et structures de données pour ingénieurs";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIF-1003"; CP "IFT-1006" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "IFT-2008";
        offre = [ (Aut, A) ];
        dom = [ PROG ];
      } );
    ( "PHY-2100",
      {
        titre = "Sciences de l'espace";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "PHYOPT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SYS ];
      } );
    ( "SIO-2100",
      {
        titre = "Stratégies d'affaires électroniques";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "SIO-1000";
              CP "SIO-1101";
              CP "IFT-1004";
              CP "SIO-1001";
              CP "GLO-1901";
            ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, H) ];
        dom = [ ENT ];
      } );
    ( "IFT-2101",
      {
        titre = "Protocoles et technologies Internet";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-2006"; CP "GIF-3001"; CP "GLO-2000" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ WEB; NET ];
      } );
    ( "IFT-2102",
      {
        titre = "Aspects pratiques de la sécurité informatique";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1003"; CP "SIO-2103"; CP "GLO-2003" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ SEC ];
      } );
    ( "SIO-2102",
      {
        titre = "Sécurité, contréle et gestion du risque ";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "SIO-1000";
              CP "SIO-1101";
              CP "SIO-2000";
              CP "IFT-1004";
              CP "GLO-1901";
            ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Ete, Z3); (Aut, Z3) ];
        dom = [ SEC ];
      } );
    ( "IFT-2103",
      {
        titre = "Programmation de jeux vidéo";
        credit = Cr 3;
        conco = "";
        pre = ET [ OU [ CP "GIF-1003"; CP "IFT-1006" ]; CP "IFT-3100" ];
        (* Devrait se limiter à
              CP "IFT-3100"
           car le 1er préalable est déjé présent dans les préalables du 2e!
        *)
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ GAME ];
      } );
    ( "SIO-2103",
      {
        titre = "Conception des systémes d'information organisationnels I";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "SIO-1000";
              CP "SIO-1101";
              CP "IFT-1004";
              CP "SIO-1001";
              CP "SIO-2000";
              CP "GLO-1901";
            ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, ZA); (Ete, Z3); (Aut, ZA) ];
        dom = [ SIO; ANA_CON ];
      } );
    ( "SIO-2104",
      {
        titre = "Gestion de l'innovation technologique";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "SIO-1000";
              CP "SIO-1101";
              CP "IFT-1004";
              CP "IFT-1903";
              CP "GLO-1901";
            ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [ SIO ];
      } );
    ( "SIO-2105",
      {
        titre = "La fonction conseil en SIO";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "SIO-1000"; CP "SIO-1101"; CP "SIO-1001"; CP "IFT-1004" ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ SIO ];
      } );
    ( "SIO-2107",
      {
        titre = "Gestion de projets, applications SIO";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              CP "SIO-1000";
              CP "SIO-1101";
              CP "SIO-1001";
              CP "IFT-1004";
              CP "GLO-1901";
            ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Hiv, A); (Aut, Z3) ];
        dom = [ SIO ];
      } );
    ( "SIO-2110",
      {
        titre = "Progiciels de gestion intégrés";
        credit = Cr 3;
        conco = "";
        pre = CP "SIO-2103";
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3) ];
        dom = [ SIO ];
      } );
    ( "MAT-2200",
      {
        titre = "Algébre linéaire avancée";
        credit = Cr 3;
        conco = "";
        pre = CP "MAT-1200";
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ MAT ];
      } );
    ( "STT-2200",
      {
        titre = "Analyse des données";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "STT-1000"; CP "ACT-2000"; CP "STT-2920" ];
              OU [ CP "MAT-1200"; CP "ACT-2002" ];
              OU
                [
                  CP "STT-1100";
                  CP "ACT-2002";
                  CP "ECN-2090";
                  CP "GLO-1901";
                  CP "IFT-1004";
                ];
            ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ IA ];
      } );
    ( "GGR-2305",
      {
        titre = "Climatologie";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "FGEGEO";
        multi = "";
        equiv = "";
        offre = [ (Ete, Z1); (Hiv, Z1) ];
        dom = [ GEN ];
      } );
    ( "IFT-2580",
      {
        titre = "Stage en informatique I";
        credit = St 9;
        conco = "";
        pre = CRE 24;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre =
          [ (Aut, A); (Hiv, A); (Ete, A); (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "GLO-2580",
      {
        titre = "Stage en génie logiciel I";
        credit = St 9;
        conco = "";
        pre = CRE 24;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre =
          [ (Aut, A); (Hiv, A); (Ete, A); (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "GLO-2581",
      {
        titre = "Stage en génie logiciel II";
        credit = St 9;
        conco = "";
        pre = CP "GLO-2580";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre =
          [ (Aut, A); (Hiv, A); (Ete, A); (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "ECN-2901",
      {
        titre = "Analyse économique en ingénierie";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Hiv, H); (Ete, H) ];
        dom = [ GEN ];
      } );
    ( "GMN-2901",
      {
        titre = "Santé et sécurité pour ingénieur II";
        credit = Cr 2;
        conco = "";
        pre = Aucun;
        dept = "ECNFSS";
        multi = "";
        equiv = "";
        offre = [ (Hiv, H) ];
        dom = [ GEN ];
      } );
    ( "MAT-2910",
      {
        titre = "Analyse numérique pour l'ingénieur";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "MAT-1110"; CP "MAT-1900"; CP "PHY-1002" ];
              OU
                [
                  CCP "GLO-1901"; CCP "IFT-1004"; CCP "IFT-1903"; CP "IFT-1901";
                ];
            ];
        dept = "MAT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A); (Hiv, A); (Hiv, Z3) ];
        dom = [ MAT ];
      } );
    ( "PHI-2910",
      {
        titre = "Génie et développement durable";
        credit = Cr 3;
        conco = "";
        pre = CRE 30;
        dept = "PHIPHI";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Ete, H) ];
        dom = [ LOG ];
      } );
    ( "STT-2920",
      {
        titre = "Théorie des probabilités";
        credit = Cr 3;
        conco = "";
        pre =
          OU
            [
              ET [ CP "MAT-1900"; CP "MAT-1910" ];
              ET [ CP "PHY-1001"; CP "PHY-1002" ];
            ];
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ STAT ];
      } );
    ( "MAT-2930",
      {
        titre = "Algébre linéaire appliquée";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GLO-1901"; CP "IFT-1004"; CP "IFT-1903" ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ MAT ];
      } );
    ( "ENT-3000",
      {
        titre = "Portfolio entrepreneurial I";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "ENT-1000"; CRE 21 ];
        dept = "MNGFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Ete, A) ];
        dom = [ ENT ];
      } );
    ( "IFT-3000",
      {
        titre = "Langages de programmation";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "GIF-1003"; CP "IFT-1006" ];
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
            ];
        (* Devrait se limiter à
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
           car le 1er préalable est déjé présent dans les préalables du 2e!
        *)
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, ZA); (Ete, Z3) ];
        dom = [ PROG ];
      } );
    ( "GIF-3000",
      {
        titre = "Architecture des microprocesseurs";
        credit = Cr 3;
        conco = "";
        pre = CP "GLO-2001";
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "GIF-3001",
      {
        titre = "Réseaux de transmission de données";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIF-1003"; CP "IFT-1006" ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ NET ];
      } );
    ( "IFT-3001",
      {
        titre = "Conception et analyse d'algorithmes";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
              OU [ CP "STT-1000"; CP "STT-2920"; CP "STT-2000"; CP "MQT-1102" ];
              OU [ CP "MAT-1310"; CP "MAT-1919" ];
            ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, Z3) ];
        dom = [ THEO; MAT ];
      } );
    ( "GLO-3002",
      {
        titre = "Projet en génie logiciel";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "GLO-3101"; CP "GLO-4000"; CP "GLO-4002"; CP "GLO-3013" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-3002",
      {
        titre = "Informatique d'enquéte";
        credit = Cr 3;
        conco = "";
        pre = CP "GIF-1001";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ SEC ];
      } );
    ( "GIF-3004",
      {
        titre = "Systémes embarqués temps réel";
        credit = Cr 3;
        conco = "";
        pre = CP "GLO-2001";
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, H) ];
        dom = [ NET ];
      } );
    ( "GLO-3004",
      {
        titre = "Spécification formelle et vérification de logiciels";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "IFT-2002"; OU [ CP "GIF-1003"; CP "IFT-1006" ] ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, A) ];
        dom = [ THEO ];
      } );
    ( "GMC-3009",
      {
        titre = "Gestion de projets en ingénierie";
        credit = Cr 3;
        conco = "";
        pre = CRE 24;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z1) ];
        dom = [ THEO ];
      } );
    ( "ENT-3010",
      {
        titre = "Portfolio entrepreneurial II";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "ENT-1000"; CP "ENT-3000"; CRE 18 ];
        dept = "MNGFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Ete, A) ];
        dom = [ ENT ];
      } );
    ( "GIN-3010",
      {
        titre = "Conception et implantation des systémes de production";
        credit = Cr 3;
        conco = "";
        pre = CP "GIN-2010";
        dept = "LETLAN";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ LANG ];
      } );
    ( "GLO-3013",
      {
        titre = "Projet de conception multidisciplinaire";
        credit = Cr 4;
        conco = "";
        pre = ET [ CP "GLO-2004"; CP "GEL-1001" ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SYS ];
      } );
    ( "ANL-3020",
      {
        titre = "Advanced English II";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "LETLAN";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A); (Aut, Z3); (Ete, A) ];
        dom = [ LANG ];
      } );
    ( "GIN-3060",
      {
        titre = "Systémes de gestion intégrée";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIN-3010"; ET [ CP "GSO-1000"; CP "MNG-1000" ] ];
        dept = "LETLAN";
        multi = "";
        equiv = "";
        offre = [ (Aut, H) ];
        dom = [ LANG ];
      } );
    ( "GLO-3100",
      {
        titre = "Cryptographie et sécurité informatique";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-2008"; CP "GLO-2100" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ SEC ];
      } );
    ( "IFT-3100",
      {
        titre = "Infographie";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GIF-1003"; CP "IFT-1006" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3) ];
        dom = [ GAME ];
      } );
    ( "SIO-3100",
      {
        titre = "Conception des systémes d'information organisationnels II";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-1003"; CP "SIO-2103" ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ GAME ];
      } );
    ( "GIF-3101",
      {
        titre = "Informatique mobile et applications";
        credit = Cr 3;
        conco = "";
        pre = ET [ OU [ CP "GIF-1003"; CP "IFT-1006" ]; CRE 57 ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ WEB ];
      } );
    ( "GLO-3101",
      {
        titre = "Gestion de projets informatiques : méthodes et outils";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-3101",
      {
        titre = "Compilation et interprétation";
        credit = Cr 3;
        conco = "";
        pre = CP "IFT-2002";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, Z3) ];
        dom = [ THEO ];
      } );
    ( "GLO-3102",
      {
        titre = "Développement d'applications Web";
        credit = Cr 3;
        conco = "";
        pre = OU [ CCP "GLO-2004"; CCP "IFT-2007" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Aut, Z3) ];
        dom = [ WEB ];
      } );
    ( "SIO-3110",
      {
        titre = "Atelier en analyse d'affaires ";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "SIO-2104"; OU [ CP "SIO-2103"; CP "IFT-1003" ] ];
        dept = "SIOFSA";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, H) ];
        dom = [ ENT ];
      } );
    ( "GLO-3112",
      {
        titre = "Développement avancé d'applications Web";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "GLO-3102"; OU [ CP "IFT-2004"; CP "GLO-2005" ] ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3) ];
        dom = [ WEB ];
      } );
    ( "IFT-3113",
      {
        titre = "Projet de jeu vidéo";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-2103"; CP "ANI-1018"; CP "MUS-2012" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3) ];
        dom = [ GAME ];
      } );
    ( "IFT-3201",
      {
        titre = "Sécurité dans les réseaux informatiques";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-2006"; CP "GIF-3001"; CP "GLO-2000" ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ SEC ];
      } );
    ( "GLO-3202",
      {
        titre = "Sécurité des applications Web";
        credit = Cr 3;
        conco = "";
        pre = CP "GLO-3102";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SEC; WEB ];
      } );
    ( "IFT-3333",
      {
        titre = "Projet de recherche";
        credit = Cr 6;
        conco = "";
        pre = CRE 24;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "IFT-3580",
      {
        titre = "Stage en informatique II";
        credit = St 9;
        conco = "";
        pre = CP "IFT-2580";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre =
          [ (Aut, A); (Hiv, A); (Ete, A); (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "IFT-3591",
      {
        titre = "Stage en informatique III";
        credit = St 9;
        conco = "";
        pre = CP "IFT-3580";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre =
          [ (Aut, A); (Hiv, A); (Ete, A); (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "IFT-3592",
      {
        titre = "Stage en informatique IV";
        credit = St 9;
        conco = "";
        pre = CP "IFT-3591";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre =
          [ (Aut, A); (Hiv, A); (Ete, A); (Aut, Z3); (Hiv, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "PHI-3900",
      {
        titre = "éthique et professionnalisme";
        credit = Cr 3;
        conco = "";
        pre = CRE 60;
        dept = "PHIPHI";
        multi = "";
        equiv = "";
        offre = [ (Hiv, ZA); (Aut, ZA); (Ete, Z3) ];
        dom = [ LOG ];
      } );
    ( "ANL-3905",
      {
        titre = "English for academic purposes ";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ LANG ];
      } );
    ( "EDC-4000",
      {
        titre = "Rechercher, sélectionner, présenter l'information";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "EDUEDU";
        multi = "EDC-7011";
        equiv = "";
        offre = [ (Aut, Z1); (Hiv, Z1) ];
        dom = [ GEN ];
      } );
    ( "GLO-4000",
      {
        titre = "Interface personne-machine";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-7006";
        equiv = "";
        offre = [ (Hiv, Z3) ];
        dom = [ GEN ];
      } );
    ( "STT-4000",
      {
        titre = "Statistique mathématique";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "STT-1500"; CP "ACT-1002" ];
        dept = "MATSTT";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ STAT ];
      } );
    ( "GLO-4001",
      {
        titre = "Introduction à la robotique mobile";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "IFT-1004"; CP "GLO-1901" ];
              OU [ CP "STT-1000"; CP "STT-1900"; CP "STT-2920" ];
            ];
        dept = "IFTGLO";
        multi = "GLO-7021";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "IFT-4001",
      {
        titre = "Optimisation combinatoire";
        credit = Cr 3;
        conco = "";
        pre = CP "IFT-3001";
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ MAT; THEO ];
      } );
    ( "GLO-4002",
      {
        titre = "Qualité et métriques du logiciel";
        credit = Cr 3;
        conco = "";
        pre = ET [ OU [ CP "GLO-2004"; CP "IFT-2007" ]; CP "GLO-2003" ];
        (* Devrait se limiter à
              CP "GLO-2003"
           car le 1er préalable est déjé présent dans les préalables du 2e!
        *)
        dept = "IFTGLO";
        multi = "IFT-6002";
        equiv = "";
        offre = [ (Aut, Z3) ];
        dom = [ ANA_CON ];
      } );
    ( "GLO-4003",
      {
        titre = "Architecture logicielle";
        credit = Cr 3;
        conco = "";
        pre = CP "GLO-4002";
        dept = "IFTGLO";
        multi = "IFT-6003";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-4003",
      {
        titre = "Compression de données";
        credit = Cr 3;
        conco = "";
        pre = CP "IFT-3001";
        dept = "IFTGLO";
        multi = "IFT-7023";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GEN ];
      } );
    ( "BIF-4007",
      {
        titre = "Traitement de données omiques par apprentissage automatique";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "STT-1000"; CP "BIO-1006"; CP "STT-1920"; CP "STT-2920" ];
              OU [ CP "IFT-1004"; CP "GLO-1901"; CP "IFT-1902" ];
            ];
        dept = "IFTGLO-FSG";
        multi = "BIF-7007";
        equiv = "";
        offre = [ (Aut, H) ];
        dom = [ PROG; IA; STAT; BIO; MAT ];
      } );
    ( "GLO-4007",
      {
        titre = "Perception 3D pour véhicules autonomes";
        credit = Cr 3;
        conco = "";
        pre = ET [ CP "GLO-4001"; OU [ CP "MAT-1200"; CP "MAT-2930" ] ];
        dept = "IFTGLO";
        multi = "GLO-7007";
        equiv = "";
        offre = [ (Ete, H) ];
        dom = [ NET; SYS ];
      } );
    ( "GLO-4008",
      {
        titre = "Applications infonuagiques natives et DevOps";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              CP "GLO-4002";
              OU [ CP "GLO-2000"; CP "IFT-2006" ];
              OU [ CP "GLO-2001"; CP "IFT-2001" ];
            ];
        dept = "IFTGLO";
        multi = "GLO-7008";
        equiv = "";
        offre = [ (Ete, ZA) ];
        dom = [ NET; WEB; SYS ];
      } );
    ( "GLO-4009",
      {
        titre = "Sécurité des logiciels";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GLO-2100"; CP "IFT-2008" ];
        dept = "IFTGLO";
        multi = "GLO-7009";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SEC ];
      } );
    ( "GLO-4010",
      {
        titre = "Certification de logiciels";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              CP "IFT-3000"; OU [ CP "IFT-1000"; CP "MAT-1300"; CP "MAT-1919" ];
            ];
        dept = "IFTGLO";
        multi = "GLO-7003";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ LANG ];
      } );
    ( "IFT-4011",
      {
        titre =
          "Introduction à la recherche en informatique : communication et \
           méthodologie";
        credit = Cr 3;
        conco = "";
        pre = CRE 24;
        dept = "IFTGLO";
        multi = "IFT-6001";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "IFT-4021",
      {
        titre = "Programmation et mathématiques pour la science des données";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
              OU [ CP "MAT-1200"; CP "MAT-2930" ];
            ];
        dept = "IFTGLO";
        multi = "IFT-7021";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ PROG; IA; MAT; STAT ];
      } );
    ( "IFT-4022",
      {
        titre = "Traitement automatique de la langue naturelle";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "IFT-2002"; CP "GLO-2100"; CP "IFT-2008" ];
              OU [ CP "STT-1000"; CP "STT-2920"; CP "STT-1900"; CP "MQT-1102" ];
            ];
        dept = "IFTGLO";
        multi = "IFT-7022";
        equiv = "";
        offre = [ (Aut, ZA) ];
        dom = [ IA; STAT ];
      } );
    ( "GLO-4027",
      {
        titre = "Analyse et traitement de données massives";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "IFT-2004"; CP "GLO-2005" ];
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
            ];
        dept = "IFTGLO";
        multi = "GLO-7027";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ BD ];
      } );
    ( "IFT-4029",
      {
        titre = "Sécurité de l'Internet des objets";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "GLO-2100"; CP "IFT-2008" ];
              OU [ CP "GLO-2000"; CP "IFT-2006" ];
            ];
        dept = "IFTGLO";
        multi = "IFT-7029";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SEC ];
      } );
    ( "IFT-4030",
      {
        titre = "Apprentissage automatique pour le traitement du signal";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "GLO-1901"; CP "IFT-1004" ];
              OU [ CP "MAT-1200"; CP "MAT-2930" ];
            ];
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, H) ];
        dom = [ SEC ];
      } );
    ( "GLO-4030",
      {
        titre = "Apprentissage par réseaux de neurones profonds";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "MAT-1200"; CP "MAT-2930" ];
              OU [ CCP "IFT-4102"; CCP "GIF-4101" ];
            ];
        (* On pourrait être tenté de changer pour
              OU
                [
                  ET [ OU [ CP "MAT-1200"; CP "MAT-2930" ]; CCP "IFT-4102" ];
                  CCP "GIF-4101";
                ];
           car (MAT-1200 OU MAT-2930) est déjà mentionné dans les préalables
           de GIF-4101; sauf que dans les préalables de GIF-4101, on retrouve
              ... (MAT-1200 OU MAT-1910 OU MAT-2930 OU PHY-1001) ...
           ce qui fait qu'on peut considérer PHY-1011 ou MAT-1910 sans pouvoir
           respecter, pour GLO-4030, (MAT-1200 OU MAT-2930).
        *)
        dept = "IFTGLO";
        multi = "GLO-7030";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ IA ];
      } );
    ( "GLO-4035",
      {
        titre = "Bases de données avancées";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "IFT-2004"; CP "GLO-2005" ];
        dept = "IFTGLO";
        multi = "GLO-7035";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ BD ];
      } );
    ( "IFT-4100",
      {
        titre = "Aspects pratiques de la chaéne de blocs";
        credit = Cr 3;
        conco = "";
        pre = ET [ CRE 21; OU [ CP "IFT-1004"; CP "GLO-1901" ] ];
        dept = "IFTGLO";
        multi = "IFT-7100";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ NET; SEC; WEB ];
      } );
    ( "GIF-4100",
      {
        titre = "Vision numérique";
        credit = Cr 3;
        conco = "";
        pre = ET [ OU [ CP "MAT-1903"; CP "MAT-1200"; CP "MAT-2930" ]; CRE 78 ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ GAME; SYS ];
      } );
    ( "GIF-4101",
      {
        titre = "Introduction à l'apprentissage machine ";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "STT-1000"; CP "STT-1900"; CP "STT-2920" ];
              OU [ CP "MAT-1200"; CP "MAT-1910"; CP "MAT-2930"; CP "PHY-1001" ];
            ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ IA ];
      } );
    ( "IFT-4102",
      {
        titre = "Techniques avancées en intelligence artificielle";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "GLO-2100"; CP "IFT-2008" ];
              OU [ CP "STT-1000"; CP "STT-2920" ];
            ];
        dept = "IFTGLO";
        multi = "IFT-7025";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3) ];
        dom = [ IA ];
      } );
    ( "GIF-4104",
      {
        titre = "Programmation paralléle et distribuée ";
        credit = Cr 3;
        conco = "";
        pre = OU [ CP "GLO-2100"; CP "IFT-2008" ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ PROG; GAME; IA; SYS ];
      } );
    ( "GIF-4105",
      {
        titre = "Photographie algorithmique";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "GLO-1901"; CP "IFT-1004" ];
              OU [ CP "MAT-1903"; CP "MAT-1200"; CP "MAT-2930" ];
              CRE 55;
            ];
        dept = "GELGIF";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ GAME ];
      } );
    ( "IFT-4201",
      {
        titre = "Apprentissage par renforcement";
        credit = Cr 3;
        conco = "";
        pre =
          ET
            [
              OU [ CP "GLO-2100"; CP "IFT-2008" ];
              OU [ CP "MAT-1200"; CP "MAT-2930"; CP "PHY-1001" ];
              OU [ CCP "IFT-4102"; CCP "GIF-4101" ];
            ];
        (* On pourrait être tenté de changer pour
              ET
                [
                  OU [ CP "GLO-2100"; CP "IFT-2008" ];
                  OU
                    [
                      ET
                        [
                          OU [ CP "MAT-1200"; CP "MAT-2930"; CP "PHY-1001" ];
                          CCP "IFT-4102";
                        ];
                      CCP "GIF-4101";
                    ];
                ];
           car (MAT-1200 OU MAT-2930 OU PHY-1001) est déjà mentionné dans les
           préalables de GIF-4101; sauf que dans les préalables de GIF-4101,
           on retrouve
              ... (MAT-1200 OU MAT-1910 OU MAT-2930 OU PHY-1001) ...
           ce qui fait qu'on peut considérer MAT-1910 sans pouvoir
           respecter, pour IFT-4201, (MAT-1200 OU MAT-2930 OU PHY-1001).
        *)
        dept = "IFTGLO";
        multi = "IFT-7201";
        equiv = "";
        offre = [ (Aut, ZA) ];
        dom = [ IA ];
      } );
    ( "GEL-4799",
      {
        titre = "Dangers de l'électricité";
        credit = Cr 0;
        conco = "";
        pre = Aucun;
        dept = "GELGIF";
        multi = "";
        equiv = "GEL-1799";
        offre = [ (Aut, Z1); (Hiv, Z1) ];
        dom = [ SYS ];
      } );
    ( "BIO-4900",
      {
        titre = "écologie et environnement";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "BIO-6901";
        offre = [ (Hiv, A) ];
        dom = [ GEN ];
      } );
    ( "BIO-4902",
      {
        titre = "écologie intégrative des symbioses végétales";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "BIO-7904";
        offre = [ (Aut, Z1) ];
        dom = [ GEN ];
      } );
    ( "IFT-4902",
      {
        titre = "Programmation avec R pour l'analyse de données";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ACTFSG";
        multi = "IFT-7902";
        equiv = "";
        offre = [ (Hiv, H) ];
        dom = [ STAT ];
      } );
    ( "IFT-6001",
      {
        titre =
          "Introduction à la recherche en informatique : communication et \
           méthodologie";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4011";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [];
      } );
    ( "IFT-6002",
      {
        titre = "Assurance qualité du logiciel";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4002";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ ANA_CON ];
      } );
    ( "IFT-6003",
      {
        titre = "Architecture logicielle";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4003";
        equiv = "";
        offre = [ (Hiv, A); (Aut, A) ];
        dom = [ ANA_CON ];
      } );
    ( "BIO-6901",
      {
        titre = "écologie et environnement : actualités";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "BIO-4900";
        offre = [ (Hiv, A) ];
        dom = [ GEN ];
      } );
    ( "GLO-7001",
      {
        titre = "Conception des systémes intelligents";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ IA ];
      } );
    ( "IFT-7002",
      {
        titre = "Fondements de l'apprentissage machine";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3) ];
        dom = [ IA ];
      } );
    ( "IFT-7003",
      {
        titre = "Complexité de calcul et NP-complétude";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ THEO ];
      } );
    ( "GLO-7006",
      {
        titre = "Ingénierie des interfaces personne-machine";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4000";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ GEN ];
      } );
    ( "BIF-7007",
      {
        titre = "Traitement de données omiques par apprentissage automatique";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO-FSG";
        multi = "BIF-4007";
        equiv = "";
        offre = [ (Aut, H) ];
        dom = [ PROG; IA; STAT; BIO; MAT ];
      } );
    ( "GLO-7003",
      {
        titre = "Certification de logiciels";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4010";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ LANG ];
      } );
    ( "GLO-7008",
      {
        titre = "Applications infonuagiques natives et DevOps";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4008";
        equiv = "";
        offre = [ (Ete, ZA) ];
        dom = [ NET; WEB; SYS ];
      } );
    ( "IFT-7008",
      {
        titre = "Représentation des connaissances et modélisation";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ IA ];
      } );
    ( "GLO-7007",
      {
        titre = "Perception 3D pour véhicules autonomes";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4007";
        equiv = "";
        offre = [ (Ete, H) ];
        dom = [ NET; SYS ];
      } );
    ( "GLO-7009",
      {
        titre = "Sécurité des logiciels";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4009";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SEC ];
      } );
    ( "IFT-7009",
      {
        titre = "Réseaux mobiles";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ NET; SYS ];
      } );
    ( "IFT-7010",
      {
        titre = "Sécurité et méthodes formelles";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SEC ];
      } );
    ( "EDC-7011",
      {
        titre = "Rechercher, sélectionner, présenter l'information";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "EDUEDU";
        multi = "EDC-4000";
        equiv = "";
        offre = [ (Aut, Z1); (Hiv, Z1) ];
        dom = [ GEN ];
      } );
    ( "IFT-7012",
      {
        titre = "Théorie algorithmique des graphes";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ MAT ];
      } );
    ( "IFT-7014",
      {
        titre = "Lectures dirigées";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Hiv, Z3); (Aut, Z3); (Ete, Z3) ];
        dom = [];
      } );
    ( "IFT-7020",
      {
        titre = "Optimisation combinatoire";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4001";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ THEO ];
      } );
    ( "GLO-7021",
      {
        titre = "Introduction à la robotique mobile";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4001";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ SYS ];
      } );
    ( "IFT-7022",
      {
        titre =
          "Techniques et applications du traitement de la langue naturelle";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ IA ];
      } );
    ( "IFT-7023",
      {
        titre = "Compression de données";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4003";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [];
      } );
    ( "IFT-7021",
      {
        titre = "Programmation et mathématiques pour la science des données";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4021";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ PROG; IA; MAT; STAT ];
      } );
    ( "IFT-7022",
      {
        titre = "Traitement automatique de la langue naturelle";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4022";
        equiv = "";
        offre = [ (Aut, ZA) ];
        dom = [ IA; STAT ];
      } );
    ( "IFT-7025",
      {
        titre = "Techniques avancées en intelligence artificielle";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4102";
        equiv = "";
        offre = [ (Hiv, A); (Hiv, Z3) ];
        dom = [ IA ];
      } );
    ( "GLO-7027",
      {
        titre = "Analyse et traitement de données massives";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4027";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ BD ];
      } );
    ( "IFT-7029",
      {
        titre = "Sécurité de l'Internet des objets";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4029";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ SEC ];
      } );
    ( "GLO-7030",
      {
        titre = "Apprentissage par réseaux de neurones profonds";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4030";
        equiv = "";
        offre = [ (Hiv, A) ];
        dom = [ IA ];
      } );
    ( "GLO-7035",
      {
        titre = "Bases de données avancées";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "GLO-4035";
        equiv = "";
        offre = [ (Aut, A) ];
        dom = [ BD ];
      } );
    ( "IFT-7100",
      {
        titre = "Aspects pratiques de la chaéne de blocs";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4100";
        equiv = "";
        offre = [ (Ete, Z3) ];
        dom = [ NET; SEC; WEB ];
      } );
    ( "IFT-7201",
      {
        titre = "Apprentissage par renforcement";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "IFTGLO";
        multi = "IFT-4201";
        equiv = "";
        offre = [ (Aut, ZA) ];
        dom = [ IA ];
      } );
    ( "IFT-7902",
      {
        titre = "Programmation avec R pour l'analyse de données";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "ACTFSG";
        multi = "IFT-4902";
        equiv = "";
        offre = [ (Hiv, H) ];
        dom = [ STAT ];
      } );
    ( "BIO-7904",
      {
        titre = "écologie intégrative des symbioses végétales";
        credit = Cr 3;
        conco = "";
        pre = Aucun;
        dept = "BIOINF";
        multi = "";
        equiv = "BIO-4902";
        offre = [ (Aut, Z1) ];
        dom = [ GEN ];
      } );
  ]

(* ------------------------------------------------------------------------- *)
(* Autres données                                                            *)
(* ------------------------------------------------------------------------- *)

let fac =
  [
    ("FLSH", "Faculté des lettres et des sciences humaines");
    ("FSG", "Faculté des sciences et de génie");
    ("FSS", "Faculté des sciences sociales");
    ("FSA", "Faculté des sciences de l'administration");
  ]

let dept =
  [
    ("LANG", "école de langues");
    ( "BIOIFT",
      "Département de biochimie, de microbiologie et de bio-informatique" );
    ("CHM", "Département de chimie");
    ("ScPOL", "Département de science politique");
    ("ECO", "Département d'économique");
    ("MNG", "Département de management");
    ("CPT", "école de comptabilité");
    ("IFTGLO", "Département d'informatique et de génie logiciel");
    ("GELGIF", "Département de génie électrique et génie informatique");
    ("MATSTT", "Département de mathématiques et de statistique");
  ]
