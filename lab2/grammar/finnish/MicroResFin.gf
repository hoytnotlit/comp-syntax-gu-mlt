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


-------------
-- ADVERBS --
-------------
  Adverb : Type = {s : Str} ;

  mkAdv : Str -> Adverb = \x -> {
    s = x
    } ;

-------------
-- PRONOUNS--
-------------
  Pronoun : Type = {s : Case => Str; p : Person ; n : Number} ;

  mkPron : Str -> Str -> Str -> Number -> Pronoun = \hän,hänessä,hänet,num -> {
    s = table {Nom => hän ; Gen => hänet ; Ins => hänessä } ; -- NOTE Gen is Acc
    n = num ;
    p = P3
    } ;

-----------
-- NOUNS --
-----------
  Noun : Type = {s : Number => Case => Str} ;

  mkNoun : Str -> Str -> Str -> Str -> Str -> Str -> Noun = \talo,talot,talon,talojen,talossa,taloissa -> {
    s = table {
      Sg => table {Nom => talo ; Gen => talon ; Ins => talossa } ;
      Pl => table {Nom => talot ; Gen => talojen ; Ins => taloissa }
      }
    } ;
    
  -- TODO http://fl.finnlectura.fi/verkkokielioppi/Morfologia/sivu241.htm
  -- https://fl.finnlectura.fi/verkkosuomi/Morfologia/sivu214.htm
  -- http://jkorpela.fi/suomi/sijataivutus.html
  -- https://en.wikipedia.org/wiki/Finnish_grammar#Irregular_forms
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

----------------
-- ADJECTIVES --
----------------
-- I removed Degree because it messed up my AdjCN function ??

  --Adjective : Type = {s : Number => Degree => Case => Str} ;
  Adjective : Type = {s : Number => Case => Str} ;

  mkAdj : Str -> Str -> Str -> Str -> Str -> Str -> Adjective =
                             \iso,isot,ison,isojen,isossa,isoissa-> {
    s = table {
      Sg => table {Nom => iso ; Gen => ison ; Ins => isossa } ;
      Pl => table {Nom => isot ; Gen => isojen ; Ins => isoissa }
      } ;
    } ;

  irregAdj : (sg,hyvän,parempi,paremman,paras,parhaan,hyvät,hyvien,paremmat,parempien,parhaat,parhaiden : Str) -> Adjective =
   \sg,hyvän,parempi,paremman,paras,parhaan,hyvät,hyvien,paremmat,parempien,parhaat,parhaiden
       -> mkAdj sg (sg + "t") (sg + "n") (sg + "jen") (sg + "ssa") (sg + "issa") ;
  
  smartAdj : Str -> Adjective = \sg -> case sg of {
    keltai + ("nen") => mkAdj sg  (keltai + "set") (keltai + "sen") (keltai + "sten") (keltai + "sessa") (keltai + "sissa") ; 
    _ => mkAdj sg (sg + "t") (sg + "n") (sg + "jen") (sg + "ssa") (sg + "issa") 
    } ;

-----------
-- VERBS --
-----------
  Verb : Type = {s : Number => Person => Str} ;

  be_Verb : Verb = mkVerb "olla" "olen" "olet" "on" "olemme" "olette" "ovat" ;

  mkVerb : (juosta,juoksen,juokset,juoksee,juoksemme,juoksette,juoksevat : Str) -> Verb
    = \juosta,juoksen,juokset,juoksee,juoksemme,juoksette,juoksevat -> {
    s = table {
        Sg => table { P1 => juoksen ; P2 => juokset ; P3 => juoksee } ;
        Pl => table { P1 => juoksemme ; P2 => juoksette ; P3 => juoksevat }
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "n") (inf + "t") (inf) (inf + "mme") (inf + "tte") (inf + "vat") ;
  
  idkVerb : (inf,sg3 : Str) -> Verb = \inf,sg3 ->
      mkVerb inf (inf + "n") (inf + "t") sg3 (inf + "mme") (inf + "tte") (inf + "vat") ;
  
  smartVerb : Str -> Verb = \inf -> case inf of {
    juo + ("sta") => regVerb (juo + "kse") ;
    ui + ("da") => regVerb ui ;
    kävel + ("lä") => idkVerb (kävel + "e") (kävel + "ee");
    leik + ("kiä") => idkVerb (leik + "i") (leik + "kii");
    ope + ("t") + ("taa") => idkVerb (ope + "ta") inf;
    os + ("taa") => idkVerb (os + "ta") inf;
    tu + ("lla") => idkVerb (tu + "le") (tu + "lee");
      _ => regVerb inf
    } ;

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = {s : Number => Person => Str; c : Case } ;

  mkVerb2 : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Case -> Verb2
    = \juosta,juoksen,juokset,juoksee,juoksemme,juoksette,juoksevat,c -> {
    s = table {
        Sg => table { P1 => juoksen ; P2 => juokset ; P3 => juoksee } ;
        Pl => table { P1 => juoksemme ; P2 => juoksette ; P3 => juoksevat } } ; 
    c = c
    } ;

  regVerb2 : Str -> Case -> Verb2 = \inf,c ->
    mkVerb2 inf (inf + "n") (inf + "t") inf (inf + "mme") (inf + "tte") (inf + "vat") c ;

  idkVerb2 : Str -> Str -> Case -> Verb2 = \inf,sg3,c ->
    mkVerb2 inf (inf + "n") (inf + "t") sg3 (inf + "mme") (inf + "tte") (inf + "vat") c ;

  idk2Verb2 : Str -> Str -> Str -> Case -> Verb2 = \inf,sg3,rename,c ->
    mkVerb2 inf (inf + "n") (inf + "t") sg3 (inf + "mme") (inf + "tte") (rename + "vat") c ; 
    -- TODO what was the remove last letter + other function?

  smartVerb2 : Str -> Case -> Verb2 = \inf,c -> case inf of {
    juo + ("da" | "dä") => regVerb2 juo c ;
    kävel + ("lä") => idkVerb2 (kävel + "e") (kävel + "ee") c ;
    ope + ("t") + ("taa") => idk2Verb2 (ope + "ta") inf (ope + "tta")  c ;
    ta + ("p") + ("paa") => idk2Verb2 (ta + "pa") inf (ta + "ppa") c ;
    ri + ("k") + ("koa") => idk2Verb2 (ri + "ko") inf (ri + "kko") c ;
    os + ("taa") => idkVerb2 (os + "ta") inf c ;
     _ => regVerb2 inf c
     } ;
}
