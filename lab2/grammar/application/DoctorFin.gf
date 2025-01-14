concrete DoctorFin of Doctor =
  open
    SyntaxFin,
    ParadigmsFin,
    Prelude
  in {

-- application using standard RGL

lincat
  Phrase = Utt ;
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = mkUtt (mkS fact) ;
  presNegPhrase fact = mkUtt (mkS negativePol fact) ;
  pastPosPhrase fact = mkUtt (mkS anteriorAnt fact) ;
  pastNegPhrase fact = mkUtt (mkS anteriorAnt negativePol fact) ;
  -- presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
  -- pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;
  presQuestionPhrase fact = let p : Utt = mkUtt (mkQS (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = mkUtt (mkQS anteriorAnt (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;


  impPosPhrase action = mkUtt (mkImp action) ;
  impNegPhrase action = mkUtt negativePol (mkImp action) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;

  isProfessionProperty profession = mkVP (mkNP a_Det profession) ;
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det profession ;

  iMascPerson = i_NP ;
  iFemPerson = i_NP ;
  youMascPerson = you_NP ;
  youFemPerson = you_NP ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP (mkV "yskiä") ;
  breatheAction = mkVP (mkV "hengittää") ;
  vomitAction = mkVP (mkV "oksentaa") ;
  sleepAction = mkVP (mkV "nukkua") ;
  undressAction =  mkVP (mkV "riisuutua") ;
  dressAction = mkVP (mkV "pukeutua") ; 
  eatAction = mkVP (mkV "syödä") ;
  drinkAction = mkVP (mkV "juoda") ;
  smokeAction = mkVP (mkV "tupakoida") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "mitata")) (mkNP (mkN "kuume")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "mitata")) (mkNP (mkN "verenpaine")) ;

  hospitalPlace = {at = pAdv "sairaalassa" ; to = pAdv "sairaalaan"} ;
  homePlace = {at = pAdv "kotona" ; to = pAdv "kotiin"} ;
  schoolPlace = {at = pAdv "koulussa" ; to = pAdv "kouluun"} ;
  workPlace = {at = pAdv "töissä" ; to = pAdv "töihin"} ;

  doctorProfession = mkCN (mkN "lääkäri") ;
  nurseProfession = mkCN (mkN "hoitaja") ;
  interpreterProfession = mkCN (mkN "tulkki") ;

  bePregnantProperty = mkVP (mkA "raskaana") ;
  beIllProperty = mkVP (mkA "sairas") ;
  beWellProperty = mkVP (mkA "terve") ;
  beDeadProperty = mkVP (mkA "kuollut") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergia" "allergioita")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "kipu" "kipuja")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "lapsi" "lapsia")) ;

  feverIllness = mkNP a_Det (mkN "kuume") ;
  fluIllness = mkNP a_Det (mkN "flunssa") ;
  headacheIllness = mkNP a_Det (mkN "päänsärky") ;
  diarrheaIllness = mkNP a_Det (mkN "ripuli") ;
  heartDiseaseIllness = mkNP a_Det (mkN "sydäntauti") ;
  lungDiseaseIllness = mkNP a_Det (mkN "keuhkotauti") ;
  hypertensionIllness = mkNP (mkN "verenpainetauti") ;

  alcoholSubstance = mkNP (mkN "alkoholi") ;
  medicineSubstance = mkNP a_Det (mkN "lääke") ;
  drugsSubstance = mkNP aPl_Det (mkN "huumausaine") ;

oper
  pAdv : Str -> Adv = ParadigmsFin.mkAdv ;

  go_V = mkV "mennä" ;
  stay_V = mkV "jäädä" ;
  need_V2 = mkV2 (mkV "tarvita") ;
  take_V2 = mkV2 (mkV "ottaa") ;
  put_V2 = mkV2 (mkV "laittaa") ;
  vaccinate_V2 = mkV2 (mkV "rokottaa") ;
  examine_V2 = mkV2 (mkV "tutkia") ;

}
