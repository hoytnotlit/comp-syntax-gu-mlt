--# -path=.:../abstract
concrete MicroLangFin of MicroLang = open MicroResFin, Prelude in {

-----------------------------------------------------
---------------- Some unfixed issues ----------------
-----------------------------------------------------
-- In general there are a lot of issues with generating nouns (and verbs). It seems
-- that each word requires a special case and a lot of manual work. Because I wanted to finish
-- the assignment, I did not go ahead and do the manual work for each word.
-- At least these nouns don't generate correctly: mies, koira, omena, poika, puu, tietokone, veri

-- Transitive verbs take a case to specify which case the object should use, however
-- the verbs which should take the partitive case now take nominative case to show that this 
-- functionality is implemented. This is because I did not implement the partitive case for the 
-- mkNoun function in the MicroRes.

-- Another similar issue to the above is in the CompAP definition. Instead of the nominative case
-- translative case should be used, but I haven't implemented the case in the mkNoun function.

-- Lastly I had issues with the UseComp definition. The complement should have changed the number
-- and case basewd on the pronoun 
-- For example, 
-- you are green -> sinä olet vihreä, would use the nominative singular case of the adjective
-- they are green -> he ovat vihreitä, should instead use partitive plural case of the adjective
-- I could not figure out how to fix this.

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

    Prep = {s : Str} ; -- for appraoch 2
    --Prep = {s : Str; c : Case} ; -- for approach 1
    Det = {s : Str ; n : Number} ;

    Utt = {s : Str} ;
    S  = {s : Str} ;
    AP = Adjective ;
    CN = Noun ;
    VP = {verb :  Number => Person => Str ; compl : Str} ;
    Comp = {s : Str} ;
    NP = {s : Case => Str ; p: Person ; n : Number } ;

  lin
    --  UttS : S -> Utt ;
    UttS s = s ;

    -- PredVPS : NP -> VP -> S ;
    PredVPS np vp = {
      s = np.s ! Nom ++ vp.verb ! np.n ! np.p ++ vp.compl
    };

------------------------
-- NOUNS + ADJECTIVES --
------------------------
    -- UttNP : NP -> Utt ;
    UttNP np = { s = np.s ! Nom } ; -- IDK?

    -- UseN : N -> CN ;
    UseN n = n ;

    -- PositA : A -> AP ;
    PositA a = a ;

    -- AdjCN : AP -> CN -> CN ;
    AdjCN ap cn = {
      s = table {Sg => table {  Nom => ap.s ! Sg ! Nom ++ cn.s ! Sg ! Nom ; 
                                Gen => ap.s ! Sg ! Gen ++ cn.s ! Sg ! Gen ; 
                                Ins => ap.s ! Sg ! Ins ++ cn.s ! Sg ! Ins } ;  
                Pl => table {   Nom => ap.s ! Pl ! Nom ++ cn.s ! Pl ! Nom ; 
                                Gen => ap.s ! Pl ! Gen ++ cn.s ! Pl ! Gen ; 
                                Ins => ap.s ! Pl ! Ins ++ cn.s ! Pl ! Ins } }
    } ;
    
    --CompAP : AP -> Comp ;
    CompAP ap = {s = ap.s ! Sg ! Nom } ; --CompAP ap = {s = ap.s ! Sg ! Tra } ;

    --DetCN : Det -> CN -> NP ;
    DetCN det cn = {s = cn.s ! det.n; p = P3; n = det.n } ;

    -- these determiners do not exist in Finnish therefor I have left "s" empty
    a_Det = {s = "" ; n = Sg} ;
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "" ; n = Sg} ;
    thePl_Det = {s = "" ; n = Pl} ;

-------------
-- ADVERBS --
-------------
    -- PrepNP : Prep -> NP -> Adv ;
    -- approach 1 uses cases 
    -- PrepNP prep np = { s = np.s ! prep.c } ;

    --in_Prep = {s = "in"; c = Ins }; -- e.g. he is in the house -> hän on talossa 
    --on_Prep = {s = "on"; c = Ade } ; - e.g. on the roof -> katolla
    --with_Prep = {s = "with"; c = Ade } ; -- e.g. a house with a green roof -> talo vihreällä katolla

    -- approach 2 uses postpositions and works slightly better
    PrepNP prep np = { s = np.s ! Gen ++ prep.s } ;

    in_Prep = {s = "sisällä"} ;
    on_Prep = {s = "päällä"} ;
    with_Prep = {s = "kanssa"} ;

-----------
-- VERBS --
-----------
    -- UseV : V -> VP ;
    UseV v = {
      verb = v.s ;
      compl = [] ;
      } ;

    -- ComplV2 : V2 -> NP -> VP
    ComplV2 v2 np = {
      verb = v2.s ;
      compl = np.s ! v2.c ;
    } ;

    -- AdvVP : VP -> Adv -> VP ;
    AdvVP vp adv = {
      verb = vp.verb ;
      compl = vp.compl ++ adv.s
    } ;

    -- UseComp : Comp -> VP ;
    UseComp comp = {
      verb = be_Verb.s;
      compl = comp.s
    };
      
--------------
-- PRONOUNS --
--------------
    -- UsePron : Pron -> NP ;
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
lin break_V2 = mkV2 "rikkoa" Gen ;
lin buy_V2 = mkV2 "ostaa" Gen ;
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
lin drink_V2 = mkV2 "juoda" Nom ; --Part
lin eat_V2 = mkV2 "syödä" Nom ; --Part
lin find_V2 = mkV2 "löytää" Gen ;
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
lin kill_V2 = mkV2 "tappaa" Gen ; 
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "kieli" ;
lin live_V = mkV "elää" ;
lin love_V2 = mkV2 "rakastaa" Nom ; --Part
lin man_N = mkN "mies" ;
lin milk_N = mkN "maito" ;
lin music_N = mkN "musiikki" ;
lin new_A = mkA "uusi" ;
lin now_Adv = mkAdv "nyt" ;
lin old_A = mkA "vanha" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "leikkiä" ;
lin read_V2 = mkV2 "lukea" Gen ; --or Part (reads the book/is reading a book)
lin ready_A = mkA "valmis" ;
lin red_A = mkA "punainen" ;
lin river_N = mkN "joki" ;
lin run_V = mkV "juosta" ;
lin sea_N = mkN "meri" ;
lin see_V2 = mkV2 "nähdä" Gen ;
lin ship_N = mkN "alus" ;
lin sleep_V = mkV "nukkua" ;
lin small_A = mkA "pieni" ;
lin star_N = mkN "tähti" ;
lin swim_V = mkV "uida" ;
lin teach_V2 = mkV2 "opettaa" Nom ; --Part
lin train_N = mkN "juna" ;
lin travel_V = mkV "matkustaa" ;
lin tree_N = mkN "puu" ;
lin understand_V2 = mkV2 "ymmärtää" Nom ; --Part
lin wait_V2 = mkV2 "odottaa" Nom ; --Part
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
    mkV2 : Str -> Case -> V2
      = \s,c   -> lin V2 (smartVerb2 s c) ;
    } ;

}
