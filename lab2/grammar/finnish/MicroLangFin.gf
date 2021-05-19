--# -path=.:../abstract
concrete MicroLangFin of MicroLang = open MicroResFin, Prelude in {

-- define that this V2 takes this kind of case (e.g. opettaa)

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    Pron = Pronoun ;
    Adv = Adverb ;

    Prep = {s : Str} ;
    Det = {s : Str ; n : Number} ;

    Utt = {s : Str} ;
    S  = {s : Str} ;
    AP = Adjective ;
    CN = Noun ;
    VP = {verb :  Number => Person => Str ; compl : Str} ; -- verb: Verb
    Comp = {s : Str} ;
    --NP = {s : Case => Str } ;
    --NP = {s : Case => Str; p : Person ; n : Number; g : Gender} ;
    NP = {s : Case => Str ; p: Person ; n : Number } ;


  lin
    UttS s = s ;

    -- PredVPS : NP -> VP -> S ;
    -- NP = {s : Case => Str } ;
    --VP = {verb :  Number => Person => Str ; compl : Str} ; 

    -- I buy potatoes
    -- Minä ostan perunoita
    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb ! np.n ! np.p ++ vp.compl
    };

------------------------
-- NOUNS + ADJECTIVES --
------------------------

    UttNP np = { s = np.s ! Nom } ; -- IDK?

    UseN n = n ;

    PositA a = a ;

    AdjCN ap cn = {
      s = table {Sg => table {  Nom => ap.s ! Sg ! Nom ++ cn.s ! Sg ! Nom ; 
                                Gen => ap.s ! Sg ! Gen ++ cn.s ! Sg ! Gen ; 
                                Ins => ap.s ! Sg ! Ins ++ cn.s ! Sg ! Ins };  
                Pl => table {   Nom => ap.s ! Pl ! Nom ++ cn.s ! Pl ! Nom ; 
                                Gen => ap.s ! Pl ! Gen ++ cn.s ! Pl ! Gen   ; 
                                Ins => ap.s ! Pl ! Ins ++ cn.s ! Pl ! Ins } }
    } ;
    
    --CompAP : AP -> Comp ;
    CompAP ap = {s = ap.s ! Sg ! Nom }; -- TODO -sti

    --DetCN : Det -> CN -> NP ;
    --NP = {s : Case => Str ; p: Person ; n : Number } ;
    --Noun : Type = {s : Number => Case => Str} ;
    --DetCN det cn = cn;

-----------
-- VERBS --
-----------
    -- Verb : Type = {s : Number => Person => Str} ;
    
    UseV v = {
      verb = v.s ; -- v
      compl = [] ;
      } ;

    --ComplV2 : V2 -> NP -> VP
    ComplV2 v2 np = {
      verb = v2.s ;
      compl = np.s ! Gen ; -- Acc
    } ;

    --AdvVP : VP -> Adv -> VP ;
    AdvVP vp adv = {
      verb = vp.verb ;
      compl = vp.compl ++ adv.s
    } ;

    --UseComp : Comp -> VP ;
    -- du är grön
    -- de är gröna

    -- sinä olet vihreä (Nom)
    -- he ovat vihreitä (Part)

    --UseComp comp = {
      --verb = be_Verb;
      --compl = comp.s
    --};

      
--------------
-- PRONOUNS --
--------------
    --UsePron pron = pron ;

    UsePron pron = {
      s = table {
        Nom => pron.s ! Nom ;
        Gen => pron.s ! Gen ;
        Ins => pron.s ! Ins } ;
      p = pron.p ;
      n = pron.n
    } ; 

lin he_Pron = mkPron "hän" "hänessä" "hänet" Sg ;
lin she_Pron = he_Pron ;
lin they_Pron = mkPron "he" "heissä" "heidät" Pl ;

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "jo" ;
lin animal_N = mkN "eläin" ;
lin apple_N = mkN "omena" ;
lin baby_N = mkN "vauva" ;
lin bad_A = mkA "paha" ;
lin beer_N = mkN "olut" ;
lin big_A = mkA "iso" ;
lin bike_N = mkN "polkupyörä" ;
lin bird_N = mkN "lintu" ;
lin black_A = mkA "musta" ;
lin blood_N = mkN "veri" ;
lin blue_A = mkA "sininen" ;
lin boat_N = mkN "vene" ;
lin book_N = mkN "kirja" ;
lin boy_N = mkN "poika" ;
lin bread_N = mkN "leipä" ;
lin break_V2 = mkV2 "rikkoa" ;
lin buy_V2 = mkV2 "ostaa" ;
lin car_N = mkN "auto" ;
lin cat_N = mkN "kissa" ;
lin child_N = mkN "lapsi" ;
lin city_N = mkN "kaupunki" ;
lin clean_A = mkA "puhdas" ;
lin clever_A = mkA "nokkela" ;
lin cloud_N = mkN "pilvi" ;
lin cold_A = mkA "kylmä" ;
lin come_V = mkV "tulla" ;
lin computer_N = mkN "tietokone" ;
lin cow_N = mkN "lehmä" ;
lin dirty_A = mkA "likainen" ;
lin dog_N = mkN "koira" ;
lin drink_V2 = mkV2 "juoda" ;
lin eat_V2 = mkV2 "syödä" ;
lin find_V2 = mkV2 "löytää" ;
lin fire_N = mkN "tuli" ;
lin fish_N = mkN "kala" ;
lin flower_N = mkN "kukka" ;
lin friend_N = mkN "kaveri" ;
lin girl_N = mkN "tyttö" ;
lin good_A = mkA "hyvä" "hyvän" "parempi" "paremman" "paras" "parhaan" "hyvät" "hyvien" "paremmat" "parempien" "parhaat" "parhaiden" ;
lin go_V = mkV "mennä" ;
lin grammar_N = mkN "kielioppi" ;
lin green_A = mkA "vihreä" ;
lin heavy_A = mkA "painava" ;
lin horse_N = mkN "hevonen" ;
lin hot_A = mkA "kuuma" ;
lin house_N = mkN "talo" ;
-- lin john_PN = mkPN "John" ;
lin jump_V = mkV "hypätä" ;
lin kill_V2 = mkV2 "tappaa" ;
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "kieli" ;
lin live_V = mkV "elää" ;
lin love_V2 = mkV2 "rakastaa" ;
lin man_N = mkN "mies" ;
lin milk_N = mkN "maito" ;
lin music_N = mkN "musiikki" ;
lin new_A = mkA "uusi" ;
lin now_Adv = mkAdv "nyt" ;
lin old_A = mkA "vanha" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "leikkiä" ;
lin read_V2 = mkV2 "lukea" ;
lin ready_A = mkA "valmis" ;
lin red_A = mkA "punainen" ;
lin river_N = mkN "joki" ;
lin run_V = mkV "juosta" ;
lin sea_N = mkN "meri" ;
lin see_V2 = mkV2 "nähdä" ;
lin ship_N = mkN "alus" ;
lin sleep_V = mkV "nukkua" ;
lin small_A = mkA "pieni" ;
lin star_N = mkN "tähti" ;
lin swim_V = mkV "uida" ;
lin teach_V2 = mkV2 "opettaa" ; -- TODO define here the case that it takes
lin train_N = mkN "juna" ;
lin travel_V = mkV "matkustaa" ;
lin tree_N = mkN "puu" ;
lin understand_V2 = mkV2 "ymmärtää" ;
lin wait_V2 = mkV2 "odottaa" ;
lin walk_V = mkV "kävellä" ;
lin warm_A = mkA "lämmin" ;
lin water_N = mkN "vesi" ;
lin white_A = mkA "valkoinen" ;
lin wine_N = mkN "viini" ;
lin woman_N = mkN "nainen" ;
lin yellow_A = mkA "keltainen" ;
lin young_A = mkA "nuori" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
    } ;

  mkA = overload {
    mkA : Str -> Adjective
      = \a -> lin N (smartAdj a) ;
    mkA : (hyvä,hyvän,parempi,paremman,paras,parhaan,hyvät,hyvien,paremmat,parempien,parhaat,parhaiden : Str) -> Adjective
      = \hyvä,hyvän,parempi,paremman,paras,parhaan,hyvät,hyvien,paremmat,parempien,parhaat,parhaiden 
      -> lin N (irregAdj hyvä hyvän parempi paremman paras parhaan hyvät hyvien paremmat parempien parhaat parhaiden) ;
    } ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

}
