module TokiPonaToEsperanto (translate, translateWithDico, isValidExternalWord) where 

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Set as Set

type WordForm = String
type NominalForm = String
type VerbForm = String
type VerbTransiveForm = String
type AdjectiveForm = String
type AdverbForm = String
type IsTransitive = Bool
type IsFirst = Bool
type WordExternalForm = String
type IsOrder = Bool
    
data TokiPonaWord = 
    Pronoun WordForm NominalForm AdjectiveForm
    | Word WordForm NominalForm AdjectiveForm AdverbForm VerbForm VerbTransiveForm
    | Error WordForm
    | Li
    | E
    | POINT String
    | AND
    | O
    | A
    | PI
    | QUOTE String
    | Anu
    | En
    | Kin
    | LA
    | La String
    deriving (Eq, Show)

wile = Word "wile" "volo" "vola" "vole" "volas" "volas" -- infinitive
mute = Word "mute" "multo" "multaj" "multe" "multas" "multigas" -- plural
lon = Word "lon" "ekzistado" "en" "ekziste" "estas" "farigas"
kepeken = Word "kepeken" "uzado" "uza" "per" "uzas" "uzas" -- per
tawa = Word "tawa" "irado" "movigxa" "al" "iras" "movas" -- al
lon_ni = Word "lon ni" "cxi tie" "cxi tie" "cxi tie" "cxi tie" "cxi tie" -- tie
sewi = Word "sewi" "supero" "supera" "supere" "superas" "superigas" -- super
anpa = Word "anpa" "grundo" "grunda" "sub" "malpliigxi" "malpliigi" -- sub
insa = Word "insa" "interno" "ena" "ene" "enigas" "enigas" -- en
monsi = Word "monsi" "malantauxado" "malantauxa" "malantauxe" "malantauxas" "malantauxigas" -- malantaux
poka = Word "poka" "apudo" "apuda" "apude" "apudas" "apudigas" -- apud
sama = Word "sama" "samo" "sama" "same" "egalas" "egalas" -- sama
tan' = Word "tan" "tial" "de" "de" "venas el" "deigi" -- de tial
ala = Word "ala" "nenio" "nea" "ne" "neas" "neigas" -- ne
ken = Word "ken" "eble" "ebla" "eble" "povas" "povas"
jan = Word "jan" "persono" "homa" "home" "homas" "homigas"
mi = Pronoun "mi" "mi" "mia"
sina = Pronoun "sina" "vi" "via"
ona = Pronoun "ona" "li" "lia"
seme = Word "seme" "kio" "kia" "kiel" "kio-estas" "petas"
taso = Word "taso" "sed" "nura" "nur" "nur" "nur"

dico = Map.fromList
    [   -- lesson #3
        ("mi",  mi)
        , ("sina",  sina)
        , ("pona", Word "pona" "bono" "bona" "bone" "bonas" "bonigas")
        , ("suli", Word "suli" "grando" "granda" "grande" "grandas" "grandigas") 
        , ("jan", jan)
        , ("moku", Word "moku" "mangxo" "mangxa" "mangxe" "mangxas" "mangxas")
        , ("suno", Word "suno" "suno" "suna" "sune" "lumas" "lumas")
        , ("telo", Word "telo" "akvo" "malseka" "malseke" "malsekas" "malsekigas")
        , ("li", Li)

        -- lesson #4
        , ("ilo", Word "ilo" "ilo" "ila" "ile" "ilas" "ilas")
        , ("kili", Word "kili" "frukto" "frukta" "frukte" "fruktas" "fruktigas")
        , ("ni", Pronoun "ni" "tio" "tio")
        , ("ona", ona) 
        , ("pipi", Word "pipi" "insekto" "insekta" "insekte" "insekto-estas" "insektas") 
        , ("ma", Word "ma" "lando" "landa" "lande" "landas" "landas")
        , ("ijo", Word "ijo" "io" "objekta" "objekte" "objektigxas" "objetktigas") 
        , ("jo", Word "jo" "havo" "hava" "have" "havas" "havas") 
        , ("lukin", Word "lukin" "vidajxo" "vida" "vide" "vidas" "vidas") 
        , ("pakala", Word "pakala" "detruo" "detruita" "detrue" "detruigxas" "detruas") 
        , ("unpa", Word "unpa" "sekso" "seksa" "sekse" "seksumas" "kunseksumas") 
        , ("wile", wile)
        , ("e", E)
        , (".", POINT ".")

        -- lesson #5
        , ("ike", Word "ike" "malbono" "malbona" "malbone" "malbonas" "malbonigas") 
        , ("nasa", Word "nasa" "frenezo" "freneza" "freneze" "frenezas" "frenezigas") 
        , ("jaki", Word "jaki" "malpuro" "malpura" "malpure" "malpuras" "malpurigas") 
        , ("seli", Word "seli" "varmo" "varma" "varme" "varmas" "varmigas") 
        , ("lawa", Word "lawa" "gravo" "grava" "grave" "gvidas" "gvidas") 
        , ("sewi", sewi) 
        , ("len", Word "len" "vesto" "vesta" "veste" "vestas" "vestas") 
        , ("tomo", Word "tomo" "domo" "doma" "dome" "logxas" "konstruas") 
        , ("lili", Word "lili" "malgrando" "malgranda" "malgrande" "malgrandas" "malgrandigas") 
        , ("utala", Word "utala" "batalo" "batala" "batale" "batalas" "kontrauxbatalas") 
        , ("mute", mute)

        -- lesson #6
        , ("lon", lon) 
        , ("poki", Word "poki" "ujo" "uja" "uje" "ujoestas" "ujo-farigas") 
        , ("!", POINT "!")
        , ("kepeken", kepeken)
        , ("tawa", tawa)
        , ("kiwen", Word "kiwen" "sxtono" "sxtona" "malmole" "sxtono-estas" "malmoligas") 
        , ("toki", Word "toki" "parolo" "parola" "parole" "alparolas" "diras")
        , ("kama", Word "kama" "veno" "venanta" "venante" "venas" "ekigas/ekvenas")
        , ("kon", Word "kon" "aero" "aera" "aere" "venas" "aerumi")
        , ("pana", Word "pana" "dono" "dona" "done" "donas" "donas")
        , (":", POINT ":")

        -- lesson #7
        , ("anpa", anpa)
        , ("insa", insa)
        , ("monsi", monsi)
        , ("poka", poka)
        , ("sama", sama)
        , ("tan", tan')

        -- lesson #8
        , ("ala", ala)
        , ("musi", Word "musi" "amuzo" "amuza" "amuze" "amuzigxas" "amuzas")
        , ("lape", Word "lape" "dormo" "dorma" "dorme" "dormas" "dormigas")
        , ("wawa", Word "wawa" "forto" "forta" "forte" "fortas" "fortigas")
        , ("pali", Word "pali" "laboro" "labora" "labore" "laboras" "faras")
        , ("sona", Word "sona" "scio" "scia" "scie" "scias" "scias")
        , ("ken", ken)
        , ("ale", quickWord "cxio") -- TODO cxiu : mama, jan, meli, mije
        , ("ali", quickWord "cxio") -- TODO cxiu : mama, jan, meli, mije
        , ("?", POINT "?")

        -- lesson #9
        , ("mama", Word "mama" "gepatro" "gepatra" "gepatre" "gepatras" "gepatrigas")
        , ("meli", Word "meli" "ino" "ina" "ine" "ino-estas" "ino-farigas")
        , ("mije", Word "mije" "viro" "vira" "vire" "viro-estas" "viro-farigas")
        , ("a", A)
        , ("nimi", Word "nimi" "nomo" "noma" "nome" "nomas" "nomo-farigas")
        , ("o", O)
        , (",", POINT ",")
        , ("awen", Word "awen" "atendo" "atenda" "atende" "atendas" "atendas")
        , ("mu", Word "mu" "besto-bruo" "besto-brua" "besto-brue" "besto-bruas" "besto-bruigas")
        
        -- lesson #10
        , ("seme", seme)
        , ("supa", Word "supa" "surfaco" "surfaca" "surface" "surfacas" "surfacas")
        , ("suwi", Word "suwi" "dolcxajxo" "dolcxa" "dolcxe" "dolcxas" "dolcxigas")
        , ("sin", Word "sin" "alio" "alia" "alie" "aliigxas" "aliigas")
        , ("namako", Word "namako" "spico" "spica" "spice" "spicas" "spicas")
        , ("olin", Word "olin" "amo" "ama" "ame" "amas" "amas")

        -- lesson #11
        , ("pi", PI)
        , ("kalama", Word "kalama" "bruo" "brua" "brue" "bruas" "bruas")
        , ("kulupu", Word "kulupu" "grupo" "grupa" "grupe" "grupigxas" "gruypigas")
        , ("nasin", Word "nasin" "vojo" "voja" "voje" "vojas" "envojigas")
   
        -- lesson #12
        , ("anu", Anu)
        , ("en", En)
        , ("ante", Word "ante" "alio" "alia" "alie" "aliigxas" "aliigas")
        , ("mani", Word "mani" "mono" "mona" "mone" "negocas" "negocas")
        , ("taso", taso)
        , ("kin", Kin)
        , ("lete", Word "lete" "malvarmo" "malvarma" "malvarme" "malvarmas" "malvarmigas")
        , ("pilin", Word "pilin" "sento" "senta" "sente" "sentas" "pensas")
        , ("lipu", Word "lipu" "papero" "papera" "papere" "legas" "legas")
        
        -- lesson #13
        , ("jelo", Word "jelo" "flavo" "flava" "flave" "flavas" "flavas")
        , ("pimeja", Word "pimeja" "obskuro" "obskura" "obskure" "obskuras" "obskuras")
        , ("laso", Word "laso" "bluo" "blua" "blue" "bluas" "bluas")
        , ("walo", Word "walo" "blanko" "blanka" "blanke" "blankas" "blankas")
        , ("loje", Word "loje" "rugxo" "rugxa" "rugxe" "rugxas" "rugxas")
        , ("kule", Word "kule" "koloro" "kolora" "kolore" "koloras" "koloras")
        , ("sitelen", Word "sitelen" "bildo" "bilda" "bilde" "desegnas" "desegnas")

        -- lesson #14
        , ("akesi", Word "akesi" "reptilio" "reptilia" "reptilie" "reptilias" "reptilo-farigas")
        , ("moli", Word "moli" "morto" "morta" "morte" "mortas" "mortigas")
        , ("kala", Word "kala" "fisxo" "fisxa" "fisxe" "fisxas" "fisxo-farigas")
        , ("soweli", Word "soweli" "besto" "besta" "beste" "bestas" "besto-farigas")
        , ("kasi", Word "kasi" "planto" "planta" "plante" "plantas" "plantas")
        , ("waso", Word "waso" "birdo" "birda" "birde" "bridas" "birdo-farigas")
        , ("monsuta", Word "monsuta" "monstro" "monstra" "monstre" "monstras" "monstro-farigas")
        
        -- lesson #15
        , ("ko", Word "ko" "duonsolido" "duonsolida" "duonsolide" "duonsolidas" "duonsolido-farigas")
        , ("uta", Word "uta" "busxo" "busxa" "busxe" "busxas" "kisas")
        , ("kute", Word "kute" "auxskulto" "auxskulta" "auxskulte" "auxskultas" "auxskultas")
        , ("luka", Word "luka" "mano" "mana" "mane" "enmanigas" "enmanigas")
        , ("selo", Word "selo" "hauxto" "hauxta" "hauxte" "tusxas" "tusxas")
        , ("sijelo", Word "sijelo" "korpo" "korpa" "enkorpige" "enkorpigxas" "enkorpigas")
        , ("palisa", Word "palisa" "paliso" "palisa" "palisume" "palisumas" "palisumas")
        , ("linja", Word "linja" "filamento" "filamenta" "filamente" "filamentas" "filamento-farigas")
        , ("lupa", Word "lupa" "truo" "troua" "troue" "trouas" "trouas")
        , ("sike", Word "sike" "rondo" "ronda" "ronde" "rondas" "rondigas")
        , ("nena", Word "nena" "tubero" "tubera" "tubere" "tuberas" "tuberigas")
        , ("sinpin", Word "sinpin" "faco" "faca" "face" "facas" "faco-farigas")
        , ("noka", Word "noka" "piedo" "pieda" "piede" "marsxas" "marsxas") 
        , ("open", Word "open" "malfermo" "malferma" "malferme" "malfermas" "malfermas")
        , ("oko", Word "oko" "okulo" "okula" "okule" "okulas" "rigardas")

        -- lesson #16
        , ("tu", Word "tu" "du" "dua" "due" "dividas" "dividas")
        , ("wan", Word "wan" "unu" "unua" "unue" "unigas" "unigas")
        , ("tenpo", Word "tenpo" "tempo" "tempa" "dauxre" "dauxras" "dauxrigas")
        , ("nanpa", Word "nanpa" "numero" "numera" "numere" "numeras" "numeras")
        , ("weka", Word "weka" "malproksimo" "malproksima" "malproksime" "malproksimigas" "forigas")

        -- lesson #17
        , ("la", LA)
        , ("open", Word "open" "malfermo" "malferma" "malferme" "malfermas" "malfermas")
        , ("mun", Word "mun" "luno" "luna" "lune" "lunas" "lunas")
        , ("pini", Word "pini" "fino" "fina" "fine" "finas" "finas")
        , ("tenpo", Word "tenpo" "tempo" "tempa" "tempe" "tempas" "tempas")

        -- lesson #18
        , ("alasa", Word "alasa" "cxaso/kolekto" "cxasa/kolekta" "cxase/kolekte" "cxasas/kolektas" "cxasas/kolektas")
        , ("esun", Word "esun" "vendejo" "vendeja" "vendeje" "vendas" "vendas")
        , ("pan", Word "pan" "grajno" "grajna" "grajne" "grajnigxas" "grajnigas")
        , ("pu", Word "pu" "oficiala libro de Tokipona" "libra" "libre" "interagas kun la oficiala libro de Tokipona" "interagas Tokipone oficiale kun")







        -- vortaro
        , ("ike lukin", Word "ike lukin" "malbelo" "malbela" "malbele" "malbeligxas" "malbeligas")

        , ("ilo suno", Word "ilo suno" "lampo" "lampa" "lampe" "lampas" "lampas")
        , ("ilo moku", Word "ilo moku" "mangxilo" "mangxila" "mangxile" "mangxilas" "mangxilas")

        , ("jan ali", quickWord "cxiu")
        , ("jan ale", quickWord "cxiu")
        , ("jan ike", Word "jan ike" "malamiko" "malamika" "malamike" "malamikas" "malamikigas")
        , ("jan kala", Word "jan kala" "sireno" "sirena" "sirene" "sirenas" "sirenigas")
        , ("jan lawa", Word "jan lawa" "gvidisto" "gvidista" "gvidiste" "gvidistas" "gvidistas")
        , ("jan lili", Word "jan lili" "infano" "infana" "infane" "infano-estas" "infanigas")
        , ("jan pona", Word "jan pona" "amiko" "amika" "amike" "amikas" "amikigas")
        , ("jan sama" , Word "jan sama" "gefrato" "gefrata" "gefrate" "gefratas" "gefratigas")
        , ("jan seme" , Word "jan seme" "kiu" "kiu" "kiu" "kiu-estas" "kiu-estas")
        , ("jan sewi", Word "jan sewi" "dio" "dia" "die" "dio-estas" "dio-farigas")
        , ("jan suli", Word "jan suli" "plenkreskulo" "plenkreska" "plenkreske" "plenkreskigxas" "plenkreskulfarigas")
        , ("jan toki", Word "jan toki" "parolisto" "parolista" "paroliste" "parolisto-estas" "parolisto-farigas")
        , ("jan unpa", Word "jan unpa" "amoranto" "amoranta" "amorante" "amorantigxas" "amorantigas")
        , ("jan utala", Word "jan utala" "batalisto" "batalista" "bataliste" "batalistigxas" "batalistigas")
        , ("jan alasa", Word "jan alasa" "cxasisto/kolektisto" "cxasista/kolektista" "cxasiste/kolektiste" "cxasistas/kolektistas" "cxasistas/kolektistas")
        

        , ("kalama musi", Word "kalama musi" "muziko" "muzika" "muzike" "muzikas" "muzikigas") 

        , ("kama jo", Word "kama jo" "ekhavo" "ekhava" "ekhave" "ekhavas" "ekhavas") 
        , ("kama pona", Word "kama pona" "bonveno" "bonvena" "bonvene" "bonveno-estas" "bonveno-farigas")
        , ("kama sona", Word "kama sona" "lerno" "lerna" "lerne" "lernas" "lernas")
        , ("kama moli", Word "kama moli" "ekmortajxanto" "ekmortajxanta" "ekmortajxante" "ekmortigxas" "ekmortigas")
        , ("pi kama moli", Word "pi kama moli" "ekmortajxanto" "ekmortajxanta" "ekmortajxante" "ekmortigxas" "ekmortigas")

        , ("kasi anpa", Word "kasi anpa" "herbo" "herba" "herbe" "herbas" "herbigas")
        , ("kasi kule", Word "kasi kule" "floro" "flora" "flore" "floras" "florigas")
        , ("kasi lili", Word "kasi lili" "arbeto" "arbeta" "arbete" "arbetas" "arbetigas")
        , ("kasi nasa", Word "kasi nasa" "hasxisxo" "hasxisxa" "hasxisxe" "hasxisxas" "hasxisxigas")
        , ("kasi sona", Word "kasi sona" "hasxisxo" "hasxisxa" "hasxisxe" "hasxisxas" "hasxisxigas")
        , ("kasi suli", Word "kasi suli" "arbo" "arba" "arbe" "arbas" "arbigas")

        , ("kiwen uta", Word "kiwen uta" "dento" "denta" "dente" "dentas" "dentigas")

        , ("ken la", La "eble")

        , ("ko jaki", Word "ko jaki" "feko" "feka" "feke" "fekas" "fekigas")

        , ("lape pona", Word "lape pona" "bonanokto" "bonanokta" "bonanokte" "bonanokto-estas" "bonanokto-farigas")

        , ("linja lawa", Word "linja lawa" "hararo" "harara" "harare" "hararas" "hararo-farigas")

        , ("len luka", Word "len luka" "ganto" "ganta" "gante" "gantas" "gantas")
        , ("len noka", Word "len noka" "sxuo" "sxua" "sxue" "sxuas" "sxuas")

        , ("lon ni", lon_ni)

        , ("lupa meli", Word "lupa meli" "vagino" "vagina" "vagine" "vaginas" "vagino-farigas")
        , ("lupa monsi", Word "lupa monsi" "anuso" "anusa" "anuse" "anusas" "anuso-farigas")

        , ("ma kasi", Word "ma kasi" "arbaro" "arbara" "arbare" "arbaras" "arbarigas")
        , ("ma telo", Word "ma telo" "koto" "kota" "kote" "kotas" "kotigas")
        , ("ma tomo", Word "ma tomo" "urbo" "urba" "urbe" "urbas" "urbigas")
        
        , ("mama meli", Word "mama meli" "patrino" "patrina" "patrine" "patrino-estas" "patrino-farigas")
        , ("mama mije", Word "mama mije" "patro" "patra" "patre" "patro-estas" "patro-farigas")

        , ("moku lili",  Word "moku lili" "mangxeto" "mangxeta" "mangxete" "mangxetas" "mangxetas")
        , ("moku pona", Word "moku pona" "bon-apetito" "bon-apetita" "bon-apetite" "bon-apetitas" "bon-apetitas")

        , ("mi mute",  Pronoun "mi mute" "ni" "nia")
        , ("mi taso",  Pronoun "mi taso" "mi nur" "nur mia")

        , ("mute lili", Word "mute lili" "malmulto" "malmulta" "malmulte" "malmultigxas" "malmultigas")

        , ("nasin sewi", Word "nasin sewi" "religio" "religia" "religie" "religias" "religio-farigas")

        , ("nena kute", Word "nena kute" "orelo" "orela" "orele" "auxskultas" "auxskultas")
        , ("nena sike meli", Word "nena sike meli" "mamo" "mama" "mame" "mamas" "mamo-farigas")

        , ("ona mute",  Pronoun "ona mute" "ili" "ilia")

        , ("pona lukin", Word "pona lukin" "belo" "bela" "bele" "belas" "beligas")

        , ("palisa mije", Word "palisa mije" "peniso" "penisa" "penise" "penisas" "peniso-farigas")

        , ("sike mije", Word "sike mije" "testiko" "testika" "testike" "testikas" "testiko-farigas")

        , ("sina mute",  Pronoun "sina mute" "vi" "via")
        , ("sina taso",  Pronoun "sina taso" "vi nur" "nur via")

        , ("sitelen ma", Word "sitelen ma" "mapo" "mapa" "mape" "mapas" "mapas")
        , ("sitelen tawa", Word "sitelen tawa" "video" "videa" "videe" "videas" "videas")

        , ("suno pona", Word "suno pona" "bonatago" "bonataga" "bonatage" "bonatago-estas" "bonatago-farigas")

        , ("supa lape", Word "supa lape" "lito" "lita" "lite" "enlitigxi" "enlitigas")
        , ("supa moku", Word "supa moku" "mangxotablo" "mangxotabla" "mangxotable" "surtabligas" "surtabligas")

        , ("tan seme" , Word "tan seme" "kial" "kial" "kial" "kial" "kial")

        , ("tawa musi", Word "tawa musi" "danco" "danca" "dance" "dancas" "dancigas")
 
        , ("telo jelo", Word "telo jelo" "urino" "urina" "urinante" "urinas" "urinadigas")
        , ("telo moli", Word "telo moli" "veneno" "venena" "venene" "venenigxas" "venenas")
        , ("telo nasa", Word "telo nasa" "alkoholo" "alkohola" "alkoholante" "alkoholas" "alkoholigas")
        , ("telo sijelo loje", Word "telo sijelo loje" "sango" "sanga" "sangade" "sangadas" "sensangigas")
        , ("telo suli", Word "telo suli" "oceano" "oceana" "oceane" "oceano-estas" "oceano-farigas")
        , ("telo walo mije", Word "telo walo mije" "spermo" "sperma" "spermante" "spermas" "spermo-farigas")

        , ("tenpo kama", Word "tenpo kama" "estonto" "estonta" "estonte" "estontas" "estonto-farigas")
        , ("tenpo kama la", La "estontece")
        , ("tenpo kama lili", Word "tenpo lili kama" "baldaux" "baldauxa" "baldauxe" "baldaux estas" "baldaux-farigas")
        , ("tenpo kama lili la", La "baldaux")
        , ("tenpo mute", Word "tenpo mute" "ofte" "ofta" "ofte" "oftas" "oftigas")
        , ("tenpo mute la", La "ofte")
        , ("tenpo ni la", La "nun")
        , ("tenpo pini", Word "tenpo kama" "estonteco" "estonteca" "estontece" "estontecas" "estonteco-farigas")
        , ("tenpo pini la", La "estintece")
        , ("tenpo pimeja", Word "tenpo pimeja" "nokto" "nokta" "nokte" "noktas" "nokto-farigas")
        , ("tenpo pimeja la", La "nokte")
        , ("tenpo pimeja pini la", La "hieraux nokte")
        , ("tenpo suno", Word "tenpo suno" "tago" "taga" "tage" "tagas" "tago-farigas")
        , ("tenpo suno la", La "hodiaux")
        , ("tenpo suno kama", Word "tenpo suno kama" "morgaux" "morgauxa" "morgauxe" "morgaux estas" "morgaux-farigas")
        , ("tenpo suno kama la", La "morgaux")
        , ("tenpo suno pini", Word "tenpo suno pini" "hieraux" "hierauxa" "hierauxe" "hieraux estas" "hieraux-farigas")
        , ("tenpo suno pini la", La "hieraux")
        , ("tenpo suno ni la", La "hodiaux")

        , ("toki utala", Word "toki utala" "debato" "debata" "debate" "debatas" "debatas")

        , ("tomo pi telo nasa", Word "tomo pi telo nasa" "drinkejo" "drinkeja" "drinkeje" "drinkejas" "drinkejigas")
        , ("tomo sona", Word "tomo sona" "lernejo" "lerneja" "lerneje" "lernejas" "lernejigas")
        , ("tomo tawa", Word "tomo tawa" "veturilo" "vetura" "veture" "veturas" "veturigas")
        , ("tomo tawa telo", Word "tomo tawa telo" "sxipo" "sxipa" "sxipe" "sxipas" "sxipigas")
        , ("tomo tawa kon", Word "tomo tawa kon" "aviadilo" "aviadila" "aviadile" "aviadas" "aviadigas")
        , ("tomo telo", Word "tomo telo" "tualetejo" "tualeteja" "bancxambre" "tualetigxas" "tualetigas")
        , ("tomo toki", Word "tomo toki" "babilcxambro" "babila" "babile" "babilas" "babilas")

        , ("weka tan", Word "weka tan" "malproksimo" "malproksima" "malproksime" "malproksimas" "malproksimas")
        , ("weka ala tan", Word "weka ala tan" "proksimo" "proksima" "proksime" "proksimas" "proksimas")

        -- Koloroj
        , ("laso jelo", Word "laso jelo" "verdo" "verda" "verde" "verdas" "verdas")
        , ("laso loje", Word "laso loje" "purpuro" "purpura" "purpure" "purpuras" "purpuras")

        , ("jelo laso", Word "jelo laso" "verdo" "verda" "verde" "verdas" "verdas")
        , ("jelo loje", Word "loje jelo" "orangxkoloro" "orangxkolora" "orangxkolore" "orangxkoloras" "orangxkoloras")

        , ("loje laso", Word "loje laso" "purpuro" "purpura" "purpure" "purpuras" "purpuras")
        , ("loje walo", Word "loje walo" "rozkoloro" "rozkolora" "rozkolore" "rozkoloras" "rozkoloras")
        , ("loje jelo", Word "loje jelo" "orangxkoloro" "orangxkolora" "orangxkolore" "orangxkoloras" "orangxkoloras")

        , ("walo pimeja", Word "walo pimeja" "grizo" "griza" "grize" "grizas" "grizas")
        , ("walo loje", Word "walo loje" "rozkoloro" "rozkolora" "rozkolore" "rozkoloras" "rozkoloras")

        , ("pimeja walo", Word "pimeja walo" "grizo" "griza" "grize" "grizas" "grizas")

        -- , ("jelo", Word "jelo" "flavo" "flava" "flave" "flavas" "flavas")
        -- , ("pimeja", Word "pimeja" "obskuro" "obskura" "obskure" "obskuras" "obskuras")
        -- , ("laso", Word "laso" "bluo" "blua" "blue" "bluas" "bluas")
        -- , ("walo", Word "walo" "blanko" "blanka" "blanke" "blankas" "blankas")
        -- , ("loje", Word "loje" "rugxo" "rugxa" "rugxe" "rugxas" "rugxas")

        -- Religioj
        , ("Kolisu", Word "Kolisu" "Kristiano" "kristiana" "kristiane" "kristianas" "kristiano-farigas")
        , ("Silami", Word "Silami" "Islamo" "islama" "islame" "islamigxas" "islamigas")

        -- Star War
        , ("Kese", Word "Kese" "Kashyyko" "kashyyka" "kashyyke" "kashyykas" "kashyyko-farigas")
        , ("ma Kese", Word "ma Kese" "Kashyyko" "kashyyka" "kashyyke" "kashyykas" "kashyyko-farigas")
        , ("Ento", Word "Ento" "Endoro" "endora" "endore" "endoras" "endoro-farigas")
        , ("ma Ento", Word "ma Ento" "Endoro" "endora" "endore" "endoras" "endoro-farigas")
        , ("jan Waki", Word "jan Waki" "Wookio" "wookia" "wookie" "wookio-estas" "wookio-farigas")
        , ("jan Iwa", Word "jan Iwa" "Ewoko" "ewoka" "ewoke" "ewoko-estas" "ewoko-farigas")
        


        -- Continents	
-- ma Amelika	Americas
-- ma Antasika	Antarctica
-- ma Apika	Africa
-- ma Asija	Asia
-- ma Elopa	Europe
        , ("Elopa", Word "Elopa" "Euxropo" "euxropa" "euxrope" "euxropas" "euxropo-farigas")
        , ("ma Elopa", Word "ma Elopa" "Euxropo" "euxropa" "euxrope" "euxropas" "euxropo-farigas")
-- ma Osejanija	Oceania


        -- Africa	
-- ma Ankola	Angola
-- ma Eliteja	Eritrea
-- ma Isijopija	Ethiopia
-- ma Kamelun	Cameroon
-- ma Kana	Ghana
-- ma Kanpija	Gambia
-- ma Kapon	Gabon
        , ("Kenja", Word "Kenja" "Kenjo" "kenja" "kenje" "kenjas" "kenjo-farigas")
        , ("ma Kenja", Word "ma Kenja" "Kenjo" "kenja" "kenje" "kenjas" "kenjo-farigas")
-- ma Kilipasi	Kiribati
-- ma Kine	Guinea
-- ma Kinejekatolija	Equatorial Guinea
-- ma Kinepisa	Guinea-Bissau
-- ma Komo	Comoros
-- ma Konko	Congo
-- ma Kosiwa	Côte d’Ivoire
-- ma Lapewija	Liberia
-- ma Lesoto	Lesotho
-- ma Lipija	Libya
-- ma Luwanta	Rwanda
-- ma Malakasi	Madagascar
-- ma Malawi	Malawi
-- ma Mali	Mali
-- ma Malipe	Morocco
-- ma Masu	Egypt
-- ma Mosanpi	Mozambique
-- ma Mowisi	Mauritius
-- ma Mulitanija	Mauritania
-- ma Namipija	Namibia
-- ma Naselija	Nigeria
-- ma Nise	Niger
-- ma Penen	Benin
-- ma Posuwana	Botswana
-- ma Pukinapaso	Burkina Faso
-- ma Sanpija	Zambia
-- ma Santapiken	Central African Republic
-- ma Sasali	Algeria
-- ma Sate	Chad
-- ma Sawasi	Swaziland
-- ma Seneka	Senegal
-- ma Setapika	South Africa
-- ma Sijelalijon	Sierra Leone
-- ma Sinpapuwe	Zimbabwe
-- ma Sipusi	Djibouti
-- ma Somalija	Somalia
-- ma Sutan	Sudan
-- ma Tansanija	Tanzania
-- ma Toko	Togo
-- ma Tunisi	Tunisia
-- ma Ukanta	Uganda


        -- Americas	
-- ma Alensina	Argentina
        , ("Alensina", Word "Alensina" "Argentino" "argentina" "argentine" "argentinas" "argentino-farigas")
        , ("ma Alensina", Word "Alensina" "Argentino" "argentina" "argentine" "argentinas" "argentino-farigas")
-- ma Awisi	Haiti
-- ma Ekato	Ecuador
-- ma Kalalinuna	Greenland
        , ("Kanata", Word "Kanata" "Kanado" "kanada" "kanade" "kanadas" "kanado-farigas")
        , ("ma Kanata", Word "ma Kanata" "Kanado" "kanada" "kanade" "kanadas" "kanado-farigas")
-- ma Katemala	Guatemala
-- ma Kenata	Grenada
-- ma Kosalika	Costa Rica
-- ma Kupa	Cuba
        , ("Kupa", Word "Kupa" "Kubo" "kuba" "kube" "kubas" "kubo-farigas")
        , ("ma Kupa", Word "ma Kupa" "Kubo" "kuba" "kube" "kubas" "kubo-farigas")
        , ("Mesiko", Word "Mesiko" "Meksiko" "meksika" "meksike" "meksikas" "meksiko-farigas")
        , ("ma Mesiko", Word "ma Mesiko" "Meksiko" "meksika" "meksike" "meksikas" "meksiko-farigas")
        , ("Mewika", Word "Mewika" "Usono" "usona" "usone" "usonas" "usono-farigas")
        , ("ma Mewika", Word "ma Mewika" "Usono" "usona" "usone" "usonas" "usono-farigas")

-- ma Ontula	Honduras
-- ma Palakawi	Paraguay
-- ma Panama	Panama
-- ma Papeto	Barbados
-- ma Pasila	Brasil
-- ma Pawama	Bahamas
-- ma Pelu	Peru
-- ma Pemuta	Bermuda
-- ma Penesuwela	Venezuela
-- ma Sameka	Jamaica
-- ma Sile	Chile
-- ma Sinita	Trinidad and Tobago
-- ma Tominika	Dominican Republic
-- ma Ulukawi	Uruguay
        , ("Kolonpija", Word "Kolonpija" "Kolombio" "kolombia" "kolombie" "kolombias" "kolombio-farigas")
        , ("ma Kolonpija", Word "ma Kolonpija" "Kolombio" "kolombia" "kolombie" "kolombias" "kolombio-farigas")


        -- Asia	
-- ma Aja	Armenia
-- ma Akanisan	Afghanistan
        , ("Akanisan", Word "Akanisan" "Afganujo" "afganuja" "afganuje" "afganujas" "afganujo-farigas")
        , ("ma Akanisan", Word "ma Akanisan" "Afganujo" "afganuja" "afganuje" "afganujas" "afganujo-farigas")
-- ma Anku	South Korea
-- ma Ilakija	Iraq
-- ma Ilan	Iran
-- ma Intonesija	Indonesia
-- ma Isale	Israel
-- ma Jamanija	Yemen
-- ma Kanpusi	Cambodia
-- ma Katelo	Georgia
-- ma Kuli	Kurdistan
-- ma Kusala	Gujarat
-- ma Kuwasi	Kuwait
-- ma Lanka	Sri Lanka
-- ma Losi	Russia
-- ma Lunpan	Lebanon
-- ma Malasija	Malaysia
-- ma Masu	Egypt
-- ma Mijama	Myanmar
-- ma Nijon	Japan
-- ma Pakisan	Pakistan
-- ma Palani	Bahrain
        , ("Palata", Word "Palata" "Hindujo" "hinduja" "hinduje" "hindujas" "hindujo-farigas")
        , ("ma Palata", Word "ma Palata" "Hindujo" "hinduja" "hinduje" "hindujas" "hindujo-farigas")
-- ma Panla	Bangladesh
-- ma Pilipina	Philippines
-- ma Pilisin	Palestine
-- ma Po	Tibet
-- ma Sawusi	Saudi Arabia
-- ma Sonko	China
-- ma Sulija	Syria
-- ma Tawi	Thailand
-- ma Tuki	Turkey
-- ma Uman	Oman
-- ma Utun	Jordan
-- ma Wije	Vietnam


        -- Europe	
-- ma Alan	Ireland
-- ma Antola	Andorra
-- ma Elena	Greece
-- ma Epanja	Spain
        , ("Epanja", Word "Epanja" "Hispanio" "hispania" "hispanie" "hispanias" "hispanio-farigas")
        , ("ma Epanja", Word "ma Epanja" "Hispanio" "hispania" "hispanie" "hispanias" "hispanio-farigas")
        , ("jan Epanja", Word "jan Epanja" "hispano" "hispana" "hispane" "hispanas" "hispano-farigas")
        --, ("ma tomo ???", Word "ma tomo ???" "Madrido" "madrida" "madride" "madridas" "madrido-farigas")

-- ma Esalasi	Austria
-- ma Esi	Estonia
-- ma Esuka	Basque Country
-- ma Inli	England
        , ("Inli", Word "Inli" "Anglio" "anglia" "anglie" "anglias" "anglio-farigas")
        , ("ma Inli", Word "ma Inli" "Anglio" "anglia" "anglie" "anglias" "anglio-farigas")
        , ("jan Inli", Word "jan Inli" "anglo" "angla" "angle" "anglas" "anglo-farigas")
        , ("ma tomo Lantan", Word "ma tomo Lantan" "Londono" "londona" "londone" "londonas" "londono-farigas")
-- ma Isilan	Iceland
        , ("Italija", Word "Italija" "Italio" "italia" "italie" "italias" "italio-farigas")
        , ("ma Italija", Word "ma Italija" "Italio" "italia" "italie" "italias" "italio-farigas")
        , ("jan Italija", Word "jan Italija" "italo" "itala" "itale" "italas" "italo-farigas")
        , ("ma tomo Loma", Word "ma tomo Loma" "Romo" "roma" "rome" "romas" "romo-farigas")
-- ma Juke	United Kingdom
-- ma Kalalinuna	Greenland
        , ("Kanse", Word "Kanse" "Francio" "francia" "francie" "francias" "francio-farigas")
        , ("ma Kanse", Word "ma Kanse" "Francio" "francia" "francie" "francias" "francio-farigas")
-- ma Katala	Catalan Countries
-- ma Katelo	Georgia
-- ma Kinla	Wales
-- ma Kiposi	Cyprus
-- ma Lawi	Latvia
-- ma Lijatuwa	Lithuania
-- ma Lisensan	Liechtenstein
-- ma Lomani	Romania
-- ma Losi	Russia
        , ("Lowasi", Word "Lowasi" "Kroatio" "kroatia" "kroatie" "kroatias" "kroatio-farigas")
        , ("ma Lowasi", Word "ma Lowasi" "Kroatio" "kroatia" "kroatie" "kroatias" "kroatio-farigas")
-- ma Lowenki	Slovakia
-- ma Lowensina	Slovenia
-- ma Lusepu	Luxembourg
-- ma Maketonija	Macedonia
-- ma Mosijo	Hungary
-- ma Motowa	Moldova
        , ("Netelan", Word "Netelan" "Nederlando" "nederlanda" "nederlande" "nederlandas" "nederlando-farigas")
        , ("ma Netelan", Word "ma Netelan" "Nederlando" "nederlanda" "nederlande" "nederlandas" "nederlando-farigas")
-- ma Nosiki	Norway
-- ma Pelalusi	Belarus
-- ma Pesije	Belgium
-- ma Peson	Brittany
-- ma Pokasi	Bulgaria
        , ("Posan", Word "Posan" "Bosnio" "bosnia" "bosnie" "bosnias" "bosnio-farigas")
        , ("ma Posan", Word "ma Posan" "Bosnio" "bosnia" "bosnie" "bosnias" "bosnio-farigas")
-- ma Posuka	Poland
-- ma Potuke	Portugal
-- ma Samalino	San Marino
-- ma Seki	Czech Republic
-- ma Sipe	Albania
-- ma Sopisi	Serbia
-- ma Sukosi	Scotland
-- ma Sumi	Finland
-- ma Suwasi	Switzerland
-- ma Tansi	Denmark
        , ("ma Tosi", Word "ma Tosi" "Germanio" "germania" "germanie" "germanias" "germanio-farigas")
        , ("jan Tosi", Word "jan Tosi" "germano" "germana" "germane" "germanas" "germano-farigas")
        , ("ma tomo Pelin", Word "ma tomo Lantan" "Berlino" "berlina" "berline" "berlinas" "berlino-farigas")
-- ma Tuki	Turkey
-- ma Ukawina	Ukraine
-- ma Wasikano	Vatican
        , ("Wensa", Word "Wensa" "Svedio" "svedia" "svedie" "svedias" "svedio-farigas")
        , ("ma Wensa", Word "ma Wensa" "Svedio" "svedia" "svedie" "svedias" "svedio-farigas")

        -- Oceania	
-- ma Intonesija	Indonesia
-- ma Nusilan	New Zealand
        , ("Oselija", Word "Oselija" "Auxstralio" "auxstralia" "auxstralie" "auxstralias" "auxstralio-farigas")
        , ("ma Oselija", Word "ma Oselija" "Auxstralio" "auxstralia" "auxstralie" "auxstralias" "auxstralio-farigas")
-- ma Papuwanijukini	Papua New Guinea
-- ma Pisi	Fiji
-- ma Samowa	Samoa
-- ma Tona	Tonga
-- ma Tuwalu	Tuvalu
-- ma Wanuwatu	Vanuatu

        -- Language Names	
-- toki Alapi	Arabic
-- toki Apikan	Afrikaans
-- toki Awasa	Hausa
-- toki Awisi	Haitian Creole
-- toki Elena	Greek
-- toki Epanja	Spanish

-- toki Esi	Estonian
-- toki Esuka	Basque
-- toki Inli	English
        , ("toki Inli", Word "toki Inli" "anglalingvo" "angla" "angle" "alparolas angle" "diras angle")
-- toki Insi	Hindi
-- toki Intonesija	Indonesian
-- toki Inu	Inuit languages
-- toki Ipo	Igbo
-- toki Isilan	Icelandic
-- toki Italija	Italian
-- toki Iwisi	Hebrew
-- toki Jolupa	Yoruba
-- toki Kalike	Scottish Gaelic
-- toki Kanse	French
-- toki Kantun	Cantonese
-- toki Kinla	Welsh
-- toki Lasina	Latin
-- toki Lomani	Romanian
-- toki Losi	Russian
-- toki Lowasi	Croatian
-- toki Mosijo	Hungarian
-- toki Netelan	Dutch
-- toki Nijon	Japanese
-- toki Nosiki	Norwegian (Bokmål)
-- toki Nosiki sin	Norwegian Nynorsk
-- toki Panla	Bengali
-- toki Peson	Breton
-- toki Pokasi	Bulgarian
-- toki Posan	Bosnian
-- toki Potuke	Portuguese
-- toki Sameka	Jamaican Patois
-- toki Seki	Czech
-- toki Sesi	Tsez
-- toki Sikipe	Albanian
-- toki Sonko	Chinese
-- toki Sopisi	Serbian
-- toki Sumi	Finnish
-- toki Tansi	Danish
-- toki Topisin	Tok Pisin
-- toki Tosi	German


        -- Sign Languages	
-- The linguistic publication Ethnologue lists 121 Deaf sign languages used worldwide. To name a sign language in Toki Pona, simply add the country or region name after toki luka (hand language). Here is an incomplete list:
-- toki luka Kanse	French Sign Language
-- toki luka Kepeka	Quebec Sign Language
-- toki luka Mewika	American Sign Language
-- toki luka Oselija	Australian Sign Language
-- toki luka Piten	British Sign Language
-- toki luka Sonko	Chinese Sign Language
-- toki luka Tosi	German Sign Language
-- toki Inli luka	Manually Coded English


        -- Constructed Languages	
-- Of course, Toki Pona is not the only constructed language. Other creations include:
-- toki Apiwili	Afrihili
-- toki Epelanto	Esperanto
-- toki Inli pona	Basic English
-- toki Inota	Lingua Ignota
-- toki Intelinwa	Interlingua
-- toki Ito	Ido
-- toki Kuwenja	Quenya
-- toki Latan	Láadan
-- toki Losupan	Lojban
-- toki Mansi	Mänti
-- toki Nawi	Na’vi
-- toki Olapi	Volapük
-- toki Palepelen	Baôleybelen
-- toki Pasiki	Fyksian
        , ("toki pona", Word "toki pona" "tokipono" "tokipona" "tokipone" "alparolas tokipone" "diras tokipone")
-- toki Selen	Seren
-- toki Semisi	Semitish
-- toki Sinan	Klingon
-- toki Sintalin	Sindarin
-- toki sitelen Anlasi	Unker Non-Linear Writing System
-- toki sitelen Pisinpo	Blissymbols
-- toki Soleso	Solresol
-- toki Soma	Somish
-- toki Tolome	Traumae
-- toki Tosulaki	Dothraki

    ]

errorForm s = "[" ++ s ++ "]"
quickWord w = Word (w++"-gen") w (w++"a") (w++"e") (w++"as") (w++"igas")


translateWithDico :: String -> String
translateWithDico = internalTranslate True

translate :: String -> String
translate = internalTranslate False

internalTranslate :: Bool -> String -> String
internalTranslate useDico phrase =
    unFixPunctuation $ 
    (if useDico then fixNanpa else unwords) $ 
    fixDuaAndUna $
    fixnea2ne $
    fixnura2nur $
    fixCxio $
    reverseAdjectivesAndMuteRuleAndTio $
    map finalize $ 
    fixLa $
    fixJanAndExternalName $
    fixQuestionYesNo $
    analyse False Nothing Nominal $ 
    map convert2TokiPonaWord $ 
    (if useDico then applyDico else id) $
    fixQuote $
    words $ 
    fixPunctuation phrase


fixNanpa :: [String] -> String
fixNanpa [] = ""
fixNanpa strs =
    snd' $
    foldl (\(vacc, sacc, akuzativo, lasto) (v, s, a) -> 
        if v > 0 then (vacc + v, sacc, akuzativo || a, s) 
        else if vacc > 0 && v == 0 then 
            if length sacc > 0 then 
                (0, sacc ++ " " ++ (display vacc akuzativo lasto) ++ " " ++ s, False, s) 
            else 
                (0, (display vacc akuzativo lasto) ++ " " ++ s, False, s)
        else if length sacc > 0 then 
            (0, sacc ++ " " ++ s, False, s) 
        else 
            (0, s, False, s)
        )  (0, "", False, "") $
    map (\n -> 
        case n of 
            "mana" -> (5, "5", False)
            "manaj" -> (5, "5", False)
            "manan" -> (5, "5", True)
            "manajn" -> (5, "5", True)
            "pieda" -> (5, "5", False)
            "piedan" -> (5, "5", True)
            "piedaj" -> (5, "5", False)
            "piedajn" -> (5, "5", True)
            "du" -> (2, "2", False)
            "unu" -> (1, "1", False)
            "cxio" -> (100, "100", False)
            "cxion" -> (100, "100", True)
            "multaj" -> (20, "20", False)
            "multajn" -> (20, "20", True)

            "multoj" -> (20, "multoj", False)
            "manoj" -> (5, "manoj", False)
            "piedoj" -> (5, "piedoj", False)
            _ -> (0, n, False)
    ) $ strs
    where
        display v akuzativo lasto = 
            if (lasto == "piedoj" || lasto == "manoj") && v-5 > 0 then
                show (v-5) ++ " " ++ lasto
            else case v of
                1 -> "unu"
                2 -> "du"
                20 -> if akuzativo then "multajn" else "multaj"
                100 -> if akuzativo then "cxion" else "cxio" 
                _ -> show v
        snd' (a, b, c, d) = 
            if length b == 0 then 
                display a False ""
            else 
                b

fixDuaAndUna :: [String] -> [String]
fixDuaAndUna =
    map (\n -> 
        if n == "unua" || n == "unuaj" || n == "unuan" || n == "unuajn" then
            "unu"
        else if n == "dua" || n == "duaj" || n == "duan" || n == "duajn" then
            "du"
        else
            n 
    )


fixnea2ne :: [String] -> [String]
fixnea2ne =
    map (\n -> 
        if n == "nea" then
            "ne"
        else
            n 
    )

fixnura2nur :: [String] -> [String]
fixnura2nur =
    map (\n -> 
        if n == "nura" || n == "nuraj" || n == "nuran" || n == "nurajn" then
            "nur"
        else
            n 
    )

fixCxio :: [String] -> [String]
fixCxio =
    map (\n -> 
        if n == "cxioa" || n == "cxioaj" then
            "cxio"
        else if n == "cxioan" || n == "cxioajn" then
            "cxion"
        else 
            n 
    )

fixQuote :: [String] -> [String]
fixQuote ws =
    fixQuoteBegin ws
    where
        fixQuoteBegin [] = []
        fixQuoteBegin (x:xs) = 
            if List.isPrefixOf "\"" x then 
                if List.last x == '"' then x : fixQuoteBegin xs
                else fixQuoteEnd x xs
            else x : fixQuoteBegin xs
        fixQuoteEnd w [] = w : []
        fixQuoteEnd w (x:xs) =
            if List.last x == '"' then
                (w ++ " " ++ x) : fixQuoteBegin xs
            else fixQuoteEnd (w++ " " ++ x) xs 
                

applyDico :: [String] -> [String] 
applyDico [] = []
applyDico l =
    applyDico2 $ applyDico3 $ applyDico4 l
    where 
        applyDico2 [] = []
        applyDico2 (x:[]) = x:[]
        applyDico2 (x:y:xs) = 
            let 
                w2 = x++" "++y
            in
            (
                case Map.lookup w2 dico of
                    Nothing -> x : applyDico2 (y:xs) 
                    Just w -> w2 : applyDico2 xs
            )
        applyDico3 [] = []
        applyDico3 (x:[]) = x:[]
        applyDico3 (x:y:[]) = x:y:[]
        applyDico3 (x:y:z:xs) = 
            let 
                w3 = x++" "++y++" "++z
            in
            (
                case Map.lookup w3 dico of
                    Nothing -> x : applyDico3 (y:z:xs) 
                    Just w -> w3 : applyDico3 xs
            )
        applyDico4 [] = []
        applyDico4 (x:[]) = x:[]
        applyDico4 (x:y:[]) = x:y:[]
        applyDico4 (x:y:z:[]) = x:y:z:[]
        applyDico4 (x:y:z:z':xs) = 
            let 
                w4 = x++" "++y++" "++z++" "++z'
            in
            (
                case Map.lookup w4 dico of
                    Nothing -> x : applyDico4 (y:z:z':xs) 
                    Just w -> w4 : applyDico4 xs
            )

-- 1o 1a 2a kaj 2o 3a 4a -> 2a 1a 1o kaj 4a 3a 2o
reverseAdjectivesAndMuteRuleAndTio :: [String] -> [String]
reverseAdjectivesAndMuteRuleAndTio l = 
    map Text.unpack $ reverseAdj [] [] (map Text.pack l) False False False
    where
        o = Text.pack "o"
        a = Text.pack "a"
        j = Text.pack "j"
        n = Text.pack "n"
        jn = Text.pack "jn"
        dua = Text.pack "dua"
        duan = Text.pack "duan"
        mana = Text.pack "mana"
        pieda = Text.pack "pieda"
        multaj = Text.pack "multaj"
        multajn = Text.pack "multajn"
        cxioa = Text.pack "cxioa"
        cxioan = Text.pack "cxioan"
        kaj = Text.pack "kaj"
        tio = Text.pack "tio"
        tion = Text.pack "tion"
        numero = Text.pack "numero"
        endOf x = 
            if  Text.takeEnd 2 x == jn then
                Text.takeEnd 1 $ Text.dropEnd 2 x
            else if Text.takeEnd 1 x == j || Text.takeEnd 1 x == n then 
                Text.takeEnd 1 $ Text.dropEnd 1 x 
            else Text.takeEnd 1 x 
        reverseAdj ps cs [] _ pluralForm accusativForm = ps ++ (applyPlural pluralForm accusativForm cs)
        reverseAdj ps cs (x:xs) nominalFound pluralForm accusativForm = 
            let 
                ex = endOf x
                isPlural = List.any (x==) [multaj, multajn, cxioa,  cxioan, dua, duan, mana, pieda] || pluralForm
                isAccusativForm = accusativForm || Text.takeEnd 1 x == n || Text.takeEnd 2 x == jn
            in
                if x == kaj then reverseAdj (ps ++ (applyPlural isPlural isAccusativForm cs) ++ x:[]) [] xs False False False
                else if x == tio || x == tion then reverseAdj ps (applyPlural isPlural isAccusativForm (x:cs)) xs True isPlural isAccusativForm
                else if ex == o then reverseAdj (ps ++ (applyPlural isPlural isAccusativForm cs)) (x:[]) xs True False isAccusativForm
                else if ex == a then reverseAdj ps (applyPlural isPlural isAccusativForm (x:cs)) xs True isPlural isAccusativForm
                else reverseAdj (ps++ (applyPlural isPlural isAccusativForm cs) ++ x:[]) [] xs False False isAccusativForm
        applyPlural pluralForm accusativForm xs = if pluralForm then map (setPluralForm accusativForm) xs else xs
        setPluralForm accusativForm s =
            if accusativForm then
                if (Text.takeEnd 2 s /= jn) && (Text.takeEnd 1 s == n) && s /= tion then 
                    Text.append (Text.dropEnd 1 s) jn 
                else s
            else if (Text.takeEnd 1 s /= j) && s /= tio && s /= numero then Text.append s j else s
        
fixPunctuation :: String -> String
fixPunctuation str = 
    Text.unpack $ 
    Text.replace (Text.pack ",")  (Text.pack " , ") $ 
    Text.replace (Text.pack "?")  (Text.pack " ? ") $ 
    Text.replace (Text.pack ":")  (Text.pack " : ") $ 
    Text.replace (Text.pack "!")  (Text.pack " ! ") $ 
    Text.replace (Text.pack ".")  (Text.pack " . ") $ 
    (Text.pack str)

unFixPunctuation :: String -> String
unFixPunctuation str = 
    Text.unpack $ 
    Text.replace (Text.pack " ,")  (Text.pack ",") $ 
    Text.replace (Text.pack "  ,")  (Text.pack ",") $ 
    Text.replace (Text.pack " ?")  (Text.pack "?") $ 
    Text.replace (Text.pack " :")  (Text.pack ":") $ 
    Text.replace (Text.pack " !")  (Text.pack "!") $ 
    Text.replace (Text.pack " .")  (Text.pack ".") $ 
    (Text.pack str)

convert2TokiPonaWord :: String -> TokiPonaWord
convert2TokiPonaWord w = 
    case Map.lookup w dico of
        Nothing -> 
            if isValidExternalWord w then 
                Word w w w w (w++"-estas") (w++"-farigas")  
            else if List.isPrefixOf "\"" w then
                QUOTE w 
            else Error $ errorForm w
        Just w -> w

-- j k l m n p s t w
-- a e i o u
-- begining possible : vowel + optional n
-- consonant + vowel + optional n
-- The optional n is forbidden if the next syllable begins with m or n.
-- forbidden : ji, ti, wo, and wu
-- Tepani
isValidExternalWord :: String -> Bool
isValidExternalWord s =
    let
        xs = Text.unpack $ Text.toLower $ Text.pack s
        isTitled = (Text.toTitle $ Text.toLower $ Text.pack s) == Text.pack s
        isForbidenPattern = subOf "ji" xs || subOf "ti" xs || subOf "wo" xs || subOf "wu" xs
    in
    isTitled && (not isForbidenPattern) && managesWord True False False False xs
    where
        managesWord _ _ _ _ [] = True
        managesWord first wasV wasN lastC (x:[]) =
            isV x || (x == 'n' && not first)
        managesWord first wasV wasN lastC (x:xs) =
            if first then
                if isV x then managesWord False True False False xs
                else if isC x then managesWord False False (x=='n') False xs
                else False
            else if wasV then
                if isC x then managesWord False False (x=='n') False xs
                else False
            else if wasN then
                if isC x && x /= 'm' && x /= 'n' then managesWord False False False True xs
                else if isV x then managesWord False True False False xs
                else False
            else if isC x then
                if lastC then False
                else managesWord False False False True xs
            else if isV x then 
                managesWord False True False False xs
            else False
        isV x = Set.member x $ Set.fromList "aeiou"
        isC x = Set.member x $ Set.fromList "jklmnpstw"
        subOf s [] = False 
        subOf s (x:xs) = -- TODO find other method
            if List.isPrefixOf s (x:xs) then True
            else subOf s xs

fixLa :: [(WordType, TokiPonaWord)] -> [(WordType, TokiPonaWord)]
fixLa strs =
    List.concat $ map (\n -> if isLA n then (Nominal, quickWord "dum/se") : n else n) $ toPhrases strs
    where
        isLA [] = False 
        isLA (x:xs) = if snd x == LA then True else isLA xs


toPhrases :: [(WordType, TokiPonaWord)] -> [[(WordType, TokiPonaWord)]]
toPhrases strs =
    reverse $ intToPhrases [] [] strs
    where
        intToPhrases phrase previous [] = (phrase : previous)
        intToPhrases phrase previous (x:xs) =
            case snd x of
                POINT _ -> intToPhrases [] ((phrase++(x:[])) : previous) xs
                _ -> intToPhrases (phrase++(x:[])) previous xs

fixJanAndExternalName :: [(WordType, TokiPonaWord)] -> [(WordType, TokiPonaWord)]
fixJanAndExternalName =
    managesSentences
    where 
        managesSentences [] = []
        managesSentences (x:[]) = x:[]
        managesSentences (x:y:xs) = 
            if snd x == jan 
                && (fst x == Nominal || fst x == Accusative (True || False))
                && isValidExternalWordTokipona (snd y) then
                y : managesSentences xs
            else 
                x : managesSentences (y:xs)
        isValidExternalWordTokipona tpw =
            case tpw of 
                Word word _ _ _ _ _ -> 
                    case Map.lookup word dico of
                        Nothing -> isValidExternalWord word
                        Just _ -> False
                _ -> False


fixQuestionYesNo :: [(WordType, TokiPonaWord)] -> [(WordType, TokiPonaWord)]
fixQuestionYesNo =
    managesSentences
    where 
        sentencefixQuestionYesNo [] = []
        sentencefixQuestionYesNo (x:[]) = x:[]
        sentencefixQuestionYesNo (x:y:[]) = x:y:[]
        sentencefixQuestionYesNo (x:y:z:xs) =
            let
                firstIsVerb = isVerb $ fst x  
                -- isAla = fst y == Adverb && snd y == ala
                isAla = snd y == ala
                -- secondVerbIsAdverb = fst z == Adverb
                -- secondVerbIsInfinitivAndIsWileOrKen =  fst z == Infinitive (True || False) && (snd z == wile || snd z == ken)
                verbAreEqual = snd x == snd z
                isWileOrKen = (snd z == wile || snd z == ken)
                isTransitive = fst x == VerbTransitive (True) (True || False)
                isTawaCase = snd x == tawa   
            in 
            -- if isAla && firstIsVerb &&  (secondVerbIsAdverb || secondVerbIsInfinitivAndIsWileOrKen) && verbAreEqual then
            if isAla && firstIsVerb && verbAreEqual then
                if isWileOrKen then x : changeNextToInfinitive isTransitive xs
                else if isTawaCase then x : z : sentencefixQuestionYesNo xs 
                else x : sentencefixQuestionYesNo xs
            else 
                x : sentencefixQuestionYesNo (y:z:xs)
        isVerb x = 
            case x of
                Verb _ _ -> True
                VerbTransitive _ _ -> True
                _ -> False
        changeNextToInfinitive isTransitive [] = []
        changeNextToInfinitive isTransitive (x:xs) = 
            (Infinitive isTransitive, snd x) : sentencefixQuestionYesNo xs
        managesSentences [] = []
        managesSentences sentences =
            let 
                (sentence, newSentences) =
                    case List.findIndex (\n -> case snd n of
                                                    POINT _ -> True
                                                    _ -> False ) sentences of
                        Nothing -> (sentences, [])
                        Just i -> List.splitAt (i+1) sentences  
                newSentence = sentencefixQuestionYesNo sentence
            in
                if (length newSentence < length sentence) then
                    ((Nominal, quickWord "cxu") : newSentence) ++ managesSentences newSentences
                else 
                    sentence ++ managesSentences newSentences

data WordType = 
    Nominal
    | Adjective
    | Accusative IsFirst
    | Verb IsFirst IsOrder 
    | Infinitive IsTransitive
    | VerbTransitive IsFirst IsOrder
    | Adverb
    | Punctuation
    | Conjonction
    deriving (Eq)

analyse :: Bool -> Maybe TokiPonaWord -> WordType -> [TokiPonaWord] -> [(WordType, TokiPonaWord)]
analyse _ _ _ [] = []
analyse cod was searched (x:xs) = 
    case x of  
        Pronoun _ _ _ ->
            if (searched == Nominal) then (searched, x) : analyse cod (Just x) (Verb True False) xs
            else (searched, x) : analyse cod (Just x) searched xs
        Word _ _ _ _ _ _ -> 
            let 
                isTransitive = fetchIsTransitive xs
                nextIfVerbWord = 
                    if x == tan' then Nominal
                    else if x == sama then if isTransitive then Adverb else Nominal
                    else if (x == wile || x == ken) then Infinitive isTransitive else Adverb
            in
            case (searched, isTransitive) of
                (Verb True order, True) -> (VerbTransitive True order, x) : analyse False (Just x) nextIfVerbWord xs
                (Verb True order, False) -> (Verb True order, x) : (if x == tawa then analyse False (Just x) (Accusative True) xs else analyse False (Just x) nextIfVerbWord xs)
                (Verb False order, True) -> (Conjonction, AND) : (VerbTransitive False order, x) : analyse False (Just x) nextIfVerbWord xs
                (Verb False order, False) -> (Conjonction, AND) : (Verb False order, x) : analyse False (Just x) nextIfVerbWord xs
                _ -> case searched of 
                    Nominal -> wordNominalCase cod was x xs
                    Infinitive transitive -> wordInfinitiveCase cod transitive was x xs 
                    Accusative first -> wordAccusativeCase cod x xs first 
                    Adverb -> wordAdverbCase cod was x xs
                    Adjective -> wordAdjectiveCase cod x xs 
                    _ -> (searched, x) : analyse cod (Just x) searched xs
        Error _ -> (searched, x) : analyse cod (Just x) searched xs
        Li -> case searched of
            Verb False order -> analyse False (Just x) (Verb False order) xs
            Adverb -> analyse False (Just x) (Verb False False) xs
            Accusative _ -> (Conjonction, AND) : analyse False (Just x) (Verb True False) xs
            _ -> analyse False (Just x) (Verb True False) xs
        E -> case searched of
            Accusative False -> (Conjonction, AND) : analyse True (Just x) (Accusative True) xs  
            _ -> analyse True (Just x) (Accusative True) xs
        POINT s -> if s == "," && was == Just (POINT s) then analyse False (Just x) Nominal xs else (Punctuation, x) : analyse False (Just x) Nominal xs
        O -> analyse False (Just x) (Verb True True) xs
        A -> (searched, x) : analyse cod (Just x) searched xs
        PI -> if searched == Adverb then (Nominal, quickWord "pri") : analyse cod (Just x) Nominal xs
            else (Nominal, x) : analyse cod (Just x) Nominal xs
        Anu -> wordAnuOrEn cod searched x xs
        En -> wordAnuOrEn cod searched x xs
        QUOTE _ -> (Nominal, x) : analyse cod (Just x) searched xs
        Kin -> (Nominal, x) : analyse cod (Just x) searched xs
        LA -> (Punctuation, LA) : (Punctuation, POINT ",") : analyse cod (Just (POINT ",")) Nominal xs
        La s -> (Punctuation, La s) : analyse cod (Just x) Nominal xs
    where
        fetchIsTransitive xs = 
            -- Support: verb e || verb li verb e
            case (List.elem E xs, List.elemIndex E xs, List.elemIndex Li xs, fetchNextPointPosition xs) of
                (True, Just posE, Just posLi, Just posPoint) -> (posE < posLi) && (posE < posPoint)
                (True, Just posE, Just posLi, Nothing) -> posE < posLi 
                (True, Just posE, Nothing, Just posPoint) -> posE < posPoint 
                (True, _, _, _) -> True
                _ -> False
        fetchNextPointPosition xs = 
            let 
                positions = filter (Nothing /=) [List.elemIndex (POINT ".") xs, List.elemIndex (POINT "!") xs, List.elemIndex (POINT ":") xs, List.elemIndex (POINT "?") xs, List.elemIndex (POINT ",") xs]
            in 
                if (length positions > 0) then minimum positions else Nothing
        wordAccusativeCase cod x xs first = 
            if x == taso then (Adjective, x) : analyse cod (Just x) (Accusative False) xs
            else if x == kepeken then (Adverb, x) : analyse cod (Just x) Nominal xs 
            else if x == tawa then (Adverb, x) : analyse cod (Just x) Nominal xs
            else if x == lon then if (List.null xs) then (Adverb, x) : [] else analyse cod (Just x) Adverb xs
            else (Accusative first, x) : analyse cod (Just x) (Accusative False) xs
        wordAdverbCase cod was x xs =
            if x == tan' then (Nominal, quickWord "tial") : analyse cod (Just x) Nominal xs
            else if x == sama then (Adverb, x) : analyse cod (Just x) Nominal xs
            else if was == (Just lon) && x == sewi && notFinished xs then (Nominal, quickWord "super") : analyse cod (Just x) Nominal xs 
            else if was == (Just lon) && x == anpa && notFinished xs then (Nominal, quickWord "sub") : analyse cod (Just x) Nominal xs 
            else if was == (Just lon) && x == insa && notFinished xs then (Nominal, quickWord "en") : analyse cod (Just x) Nominal xs 
            else if was == (Just lon) && x == monsi && notFinished xs then (Nominal, quickWord "malantaux") : analyse cod (Just x) Nominal xs 
            else if was == (Just lon) && x == poka && notFinished xs then (Nominal, quickWord "apud") : analyse cod (Just x) Nominal xs 
            else if was == (Just lon) && foundProunonForLon xs then (Adjective, lon) : (Nominal, x)  : analyse cod (Just x) Adjective xs
            else if was == (Just lon) && x == seme then (Nominal, quickWord "kie") : analyse cod (Just x) Nominal xs
            else if was == (Just Anu) && x == seme then (Nominal, x) : analyse cod (Just x) Nominal xs
            else if x == tawa then 
                 analyse cod (Just x) (Accusative True) xs
            else if x == kepeken then 
                (Adverb, x) : analyse cod (Just x) Nominal xs 
            else 
                (Adverb, x) : analyse cod (Just x) Adverb xs
        wordAdjectiveCase cod x xs =
            if (x == tawa) && cod then
                (Adverb, x) : analyse cod (Just x) Nominal xs 
            else if (x == lon) then 
                (Adjective, x) : analyse cod (Just x) Nominal xs 
            else 
                (Adjective, x) : analyse cod (Just x) Adjective xs
        notFinished xs = length (filter (\n -> case n of 
                                                     POINT _ -> False
                                                     _ -> True ) xs) > 0 
        wordInfinitiveCase cod transitive was x xs =
            if (x == ala) then (Adverb, x) : analyse cod (Just x) (Infinitive transitive) xs
            else (if x == mute then (Adverb, x)
            else (Infinitive transitive, x))
            : (if x == tawa then analyse cod (Just x) (Accusative True) xs 
            else analyse cod (Just x) Adverb xs)
        wordNominalCase cod was x xs = 
            if x == taso then (Nominal, x) : analyse cod (Just x) Nominal xs
            else if x == ala && not (notFinished xs) then (Nominal, quickWord "ne") : []  
            else (Nominal, x) : analyse cod (Just x) Adjective xs
        foundProunonForLon [] = False
        foundProunonForLon (x:xs) = 
            if isPoint x then False 
            else if x == mi || x == sina || x == ona then True
            else if x == lon then False
            else foundProunonForLon xs
        isPoint x =
            case x of 
                POINT _ -> True
                _ -> False
        wordAnuOrEn cod searched x xs =
            case searched of
                Verb _ _ -> (Nominal, x) : analyse cod (Just x) Nominal xs
                VerbTransitive _ _ -> (Nominal, x) : analyse cod (Just x) Nominal xs
                Adjective -> (Nominal, x) : analyse cod (Just x) Nominal xs
                Accusative _ -> (Nominal, x) : analyse cod (Just x) (Accusative True) xs
                _ -> (Nominal, x) : analyse cod (Just x) searched xs


finalize :: (WordType, TokiPonaWord) -> String
finalize (wt, tpw) = 
    case wt of 
        Nominal -> case tpw of 
            Pronoun _ nominal _ -> nominal 
            Word _ nominal _ _ _ _ -> nominal 
            A -> "ah"
            PI -> "de" 
            Anu -> "aux"
            En -> "kaj"
            QUOTE quote -> quote
            Kin -> "ja"
            _ -> errorForm $ show tpw       
        Adjective -> case tpw of 
            Pronoun _ _ adjective -> adjective 
            Word _ _ adjective _ _ _ -> adjective 
            A -> "ah"
            PI -> "de" 
            _ -> errorForm $ show tpw
        Accusative isFirst -> case tpw of 
            Pronoun _ nominal adj -> accusative (if isFirst then nominal else adj) 
            Word _ nominal adj _ _ _ -> if tpw == tan' then nominal else accusative (if isFirst then nominal else adj) 
            A -> "ah"
            PI -> "de" 
            _ -> errorForm $ show tpw
        Verb _ order -> case tpw of 
            Word _ _ _ _ verb _ -> if order then orderu verb else verb 
            A -> "ah"
            PI -> "de" 
            _ -> errorForm $ show tpw
        Infinitive isTransitive -> case tpw of
            Word "pu" _ _ _ verb verbTransitive -> if isTransitive then "interagi Tokipone oficiale kun" else "interagi kun la oficiala libro de Tokipona"
            Word "kama" _ _ _ verb verbTransitive -> if isTransitive then "ekigi/ekveni" else infinitive verb
            Word "alasa" _ _ _ _ _ -> "cxasi/kolekti" 
            Word _ _ _ _ verb verbTransitive -> infinitive $ if isTransitive then verbTransitive else verb 
            A -> "ah"
            PI -> "de" 
            _ -> errorForm $ show tpw
        VerbTransitive _ order -> case tpw of 
            Word _ _ _ _ _ verb -> if order then orderu verb else verb 
            A -> "ah"
            PI -> "de" 
            _ -> errorForm $ show tpw
        Adverb -> case tpw of 
            Pronoun _ _ adjective -> adjective 
            Word _ _ _ adverb _ _ -> adverb 
            A -> "ah"
            PI -> "de" 
            _ -> errorForm $ show tpw
        Punctuation -> case tpw of 
            POINT s -> s
            PI -> "de" 
            LA -> ""
            La s -> s
            _ -> errorForm $ show tpw
        Conjonction -> if tpw == AND then "kaj" else errorForm $ show tpw
    where
        accusative s = 
            if isValidExternalWordWithOA s then s ++ "n" 
            else if isValidExternalWord s then s 
            else if s == "kial" then s 
            else s ++ "n"
        infinitive s = (Text.unpack $ Text.dropEnd 2 $ Text.pack s) ++ "i"
        orderu s = (Text.unpack $ Text.dropEnd 2 $ Text.pack s) ++ "u"
        isValidExternalWordWithOA s = isValidExternalWord s && (Text.last (Text.pack s) == 'o' || Text.last (Text.pack s) == 'a')


    