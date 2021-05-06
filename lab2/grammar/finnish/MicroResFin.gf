-- present tense and 3-4 cases is enough!

resource MicroResFin = open Prelude in {

param
  Number = Sg | Pl ;
  Case = Nom | Gen | Ins ; 
  Degree = Pos | Cmp | Sup ;

  Agreement = Agr Number ; ---s Person to be added

  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = Inf | PresSg3 | Past | PastPart | PresPart ;
  Person = P1 | P2 | P3 ;

oper
  Noun : Type = {s : Number => Case => Str} ;

  mkNoun : Str -> Str -> Str -> Str -> Str -> Str -> Noun = \talo,talot,talon,talojen,talossa,taloissa -> {
    s = table {
      Sg => table {Nom => talo ; Gen => talon ; Ins => talossa } ;
      Pl => table {Nom => talot ; Gen => talojen ; Ins => taloissa }
      }
    } ;
    
  -- TODO http://fl.finnlectura.fi/verkkokielioppi/Morfologia/sivu241.htm
  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "t") (sg + "n") 
          (case last sg of { 
           "o" => sg + "jen" ;
           "a" => init sg + "ojen" ; -- vauva
           "ä" => init sg + "ien" ; -- lehmä
            _  => sg + "en"
            })
            (sg + "ssa") 
            (case last sg of { 
            "a" => init sg + "oissa" ; -- vauva
            "ä" => init sg + "issä" ; -- lehmä
              _  => sg + "issa"
            });

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    -- ends in consonant
    hevo + ("nen") => mkNoun sg (hevo + "set") (hevo + "sen") (hevo + "sten") (hevo + "sessa") (hevo + "sissa"); -- nainen hevonen
    x + ("t") => mkNoun sg (x + "et") (x + "en") (x + "iden") (sg + "ssa") (sg + "issa"); -- olut airut neitsyt
    x + ("s") => mkNoun sg (x + "kset") (x + "ksen") (sg + "ten") (sg + "ssa") (sg + "issa") ; -- alus opus 
    eläi + ("n") => mkNoun sg (eläi + "met") (eläi + "men") (sg + "ten") (eläi + "messa") (eläi + "missa") ; -- eläin kaulin
    -- double consonants
    op + ("pi") => mkNoun sg (op + "it") (op + "in") (sg + "en") (op + "issa") (op + "eissa");
    ty + ("t") + ("tö") => mkNoun sg (ty + "töt") (ty + "tön") (sg + "jen") (ty + "tössa") (ty + "töissa"); 
    ku + ("k") + ("ka") => mkNoun sg (ku + "kat") (ku + "kan") (ku + "kkien") (ku + "kassa") (ku + "kissa");
    musii + ("k") + ("ki") => mkNoun sg (musii + "kit") (musii + "kin") (sg + "en") (musii + "kissa") (musii + "keissa");
    -- other "stem changes" or whatever they are called
    x + ("tu") => mkNoun sg (x + "nut") (x + "nun") (x + "tujen") (x + "nussa") (x + "nuissa");
    jo + ("ki") => mkNoun sg (jo + "et") (jo + "en") (sg + "en") (jo + "essa") (jo + "issa");
    ve + ("si") => mkNoun sg (ve + "det") (ve + "den") (sg + "en") (ve + "dessä") (ve + "sissä");
    -- default
    _ => regNoun sg
    -- TODO mies, koira, omena, poika, puu, tietokone, veri
    } ;

  Adjective : Type = {s : Number => Degree => Case => Str} ;

  mkAdj : Str -> Str -> Str -> Str -> Str -> Str -> 
          Str -> Str -> Str -> Str -> Str -> Str -> Adjective =
                             \iso,ison,isompi,isomman,isoin,isoimman,
                              isot,isojen,isommat,isompien,isoimmat,isoimpien-> {
    s = table {
        Sg => table {
          Pos => table { Nom => iso ; Gen => ison ; Ins => "TODO" } ;
          Cmp => table { Nom => isompi ; Gen => isomman ; Ins => "TODO" } ; 
          Sup => table { Nom => isoin ; Gen => isoimman ; Ins => "TODO" }
        } ; 
        Pl => table {
          Pos => table { Nom => isot ; Gen => isojen ; Ins => "TODO" } ;
          Cmp => table { Nom => isommat ; Gen => isompien ; Ins => "TODO" } ; 
          Sup => table { Nom => isoimmat ; Gen => isoimpien ; Ins => "TODO" }
        }
      } ;
    } ;

  irregAdj : (hyvä,hyvän,parempi,paremman,paras,parhaan,hyvät,hyvien,paremmat,parempien,parhaat,parhaiden : Str) -> Adjective =
   \hyvä,hyvän,parempi,paremman,paras,parhaan,hyvät,hyvien,paremmat,parempien,parhaat,parhaiden
       -> mkAdj hyvä hyvän parempi paremman paras parhaan
      hyvät hyvien paremmat parempien parhaat parhaiden ;
  
  smartAdj : Str -> Adjective = \sg -> case sg of {
    keltai + ("nen") => mkAdj sg (keltai + "sen") (keltai + "sempi") (keltai + "semman") (keltai + "sin") (keltai + "simman") 
                (keltai + "set") (keltai + "sten") (keltai + "semmat") (keltai + "sempien") (keltai + "simmat") (keltai + "simpien") ; 
    _ => mkAdj sg (sg + "n") (sg + "mpi") (sg + "mman") (sg + "in") (sg + "imman") 
                (sg + "t") (sg + "jen") (sg + "mmat") (sg + "mpien") (sg + "immat") (sg + "impien") 
    } ;


-- juosta minä juoksen, sinä juokset, hän juoksee, me juoksemme, te juoksette, he juoksevat
  --Verb : Type = {s : VForm => Str} ;
  Verb : Type = {s : Number => Person => Str} ;

  mkVerb : (juosta,juoksen,juokset,juoksee,juoksemme,juoksette,juoksevat : Str) -> Verb
    = \juosta,juoksen,juokset,juoksee,juoksemme,juoksette,juoksevat -> {
    s = table {
        Sg => table {
          P1 => juoksen ;
          P2 => juokset ; 
          P3 => juoksee
        } ;
        Pl => table {
          P1 => juoksemme ;
          P2 => juoksette ; 
          P3 => juoksevat
        } 
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "n") (inf + "t") (inf) (inf + "mme") (inf + "tte") (inf + "vat") ;

  regVerb2 : (inf,sg3 : Str) -> Verb = \inf,sg3 ->
    mkVerb inf (inf + "n") (inf + "t") sg3 (inf + "mme") (inf + "tte") (inf + "vat") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
    juo + ("da" | "dä") => regVerb juo ;
    kävel + ("lä") => regVerb2 (kävel + "e") (kävel + "ee");
    leik + ("kiä") => regVerb2 (leik + "i") (leik + "kii");
    ope + ("t") + ("taa") => regVerb2 (ope + "ta") inf;
    os + ("taa") => regVerb2 (os + "ta") inf;
    tu + ("lla") => regVerb2 (tu + "le") (tu + "lee");
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  --irregVerb : (inf,past,pastpart : Str) -> Verb =
    --\inf,past,pastpart ->
      --let verb = smartVerb inf
      --in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  --be_Verb : Verb = mkVerb "are" "is" "was" "been" "being" ; ---s to be generalized


---s a very simplified verb agreement function for Micro
  agr2vform : Agreement -> VForm = \a -> case a of {
    Agr Sg => PresSg3 ;
    Agr Pl => Inf
    } ;

}