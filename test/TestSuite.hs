module Main where

import Test.HUnit
import TokiPona.TokiPonaToEsperanto

check :: String -> String -> Test
check tokipona esperanto =   
    TestCase $ assertEqual tokipona esperanto $ translate tokipona

check2 :: String -> String -> Test
check2 tokipona esperanto =   
    TestCase $ assertEqual tokipona esperanto $ translateWithDico tokipona

checkall :: String -> String -> String -> Test
checkall tokipona esperanto esperantoShort =   
    TestList [check tokipona esperanto, check2 tokipona esperantoShort]

testWord :: String -> Bool -> Test
testWord word result =
    TestCase $ assertEqual word result $ isValidExternalWord word

lesson3 :: Test
lesson3 = TestList 
    [TestLabel "Lesson #3 http://tokipona.net/tp/janpije/okamasona3.php" $ TestList [
            check "mi" "mi"
            , check "sina" "vi"
            , check "sina" "vi"
            , check "pona" "bono"
            , check "suli" "grando"
            , check "jan" "persono"
            , check "moku" "mangxo"
            , check "suno" "suno"
            , check "telo" "akvo"
            , check "mi pona" "mi bonas"
            , check "mi moku" "mi mangxas"
            , check "sina pona" "vi bonas"
            , check "telo li pona" "akvo bonas"
            , check "suno li suli" "suno grandas"
            , check "moku li pona" "mangxo bonas"
            , check "jan li pona" "persono bonas"
            , check "sina suli" "vi grandas"
            , check "telo li suli" "akvo grandas"
            , check "suno li suli" "suno grandas"
            , check "mi suli" "mi grandas"
            , check "jan li moku" "persono mangxas"
            , checkall "\n" "" ""
            , checkall "" "" ""
        ]]

lesson4 :: Test
lesson4 = TestList 
    [TestLabel "Lesson #4 http://tokipona.net/tp/janpije/okamasona4.php" $ TestList [
            check "ilo" "ilo"
            , check "kili" "frukto"
            , check "ni" "tio"
            , check "ona" "li"
            , check "pipi" "insekto"
            , check "ma" "lando"
            , check "ijo" "io"
            , check "jo" "havo"
            , check "lukin" "vidajxo"
            , check "pakala" "detruo"
            , check "unpa" "sekso"
            , check "wile" "volo"
            , check "mi moku e kili" "mi mangxas frukton"
            , check "ona li lukin e pipi." "li vidas insekton."  
            , check "ona li pona e ilo." "li bonigas ilon."
            , check "mi pona e ijo." "mi bonigas ion."
            , check "mi pona e ijo." "mi bonigas ion."
            , check "mi wile lukin e ma." "mi volas vidi landon."
            , check "mi wile pakala e sina." "mi volas detrui vin."
            , check "pipi li lukin li unpa." "insekto vidas kaj seksumas." 
            , check "mi moku li pakala." "mi mangxas kaj detruigxas."
            , check "mi moku e kili e telo" "mi mangxas frukton kaj akvon"
            , check "mi wile lukin e ma e suno" "mi volas vidi landon kaj sunon"
            , check "mi jo e ilo" "mi havas ilon"
            , check "ona li moku e kili" "li mangxas frukton"
            , check "ijo li lukin e mi" "io vidas min"
            , check "ona li wile pakala e pipi" "li volas detrui insekton"
            , check "kili li moku li pona" "frukto mangxas kaj bonas"
            , check "pipi li wile moku e telo" "insekto volas mangxi akvon" 
            , check "mi lukin e ni." "mi vidas tion."
            , check "mi wile unpa e ona." "mi volas kunseksumi lin."
            , check "jan li wile jo e ma" "persono volas havi landon"
            , check "mi jan li suli." "mi homas kaj grandas."
        ]]

lesson5 :: Test
lesson5 = TestList 
    [TestLabel "Lesson #5 http://tokipona.net/tp/janpije/okamasona5.php" $ TestList [
            check "ike" "malbono"
            , check "nasa" "frenezo"
            , check "jaki" "malpuro"
            , check "seli" "varmo"
            , check "lawa" "gravo"
            , check "sewi" "supero"
            , check "len" "vesto"
            , check "tomo" "domo"
            , check "lili" "malgrando"
            , check "utala" "batalo"
            , check "mute" "multo"
            , check "jan pakala" "detruita persono"
            , check "ilo moku" "mangxa ilo"
            , check "jan utala" "batala persono"
            , check "jan pona" "bona persono"
            , check2 "jan pona" "amiko"
            , check2 "jan utala" "batalisto"
            , check "jan utala pona" "bona batala persono"
            , check2 "jan utala pona" "bona batalisto"
            , check "jan utala pona mute" "multaj bonaj batalaj personoj"
            , check2 "jan utala pona mute" "multaj bonaj batalistoj"
            , check "jan utala pona ni" "tio bona batala persono"
            , check2 "jan pona utala" "batala amiko"
            , check "jan pona utala" "batala bona persono"
            , checkall "pona lukin" "vida bono" "belo"
            , checkall "ike lukin" "vida malbono" "malbelo"
            , checkall "jan ike" "malbona persono" "malamiko"
            , checkall "jan lawa" "grava persono" "gvidisto"
            , checkall "jan lili" "malgranda persono" "infano"
            , checkall "jan sewi" "supera persono" "dio"
            , checkall "jan suli" "granda persono" "plenkreskulo"
            , checkall "jan unpa" "seksa persono" "amoranto"
            , checkall "ma telo" "malseka lando" "koto"
            , checkall "ma tomo" "doma lando" "urbo"
            , checkall "mi mute" "mi multas" "ni"
            , checkall "ona mute" "li multas" "ili"
            , checkall "telo nasa" "freneza akvo" "alkoholo"
            , checkall "tomo telo" "malseka domo" "tualetejo"
            , check2 "jan ni li pona lukin" "tio persono belas"
            , check "tomo mi" "mia domo"
            , check "ma sina" "via lando"
            , check "telo ona" "lia akvo"
            , check "len jan" "homa vesto"
            , check "seli suno" "suna varmo"
            , checkall "mi jo e kili. ona li pona li lili. mi moku lili e kili lili." "mi havas frukton. li bonas kaj malgrandas. mi mangxas malgrande malgrandan frukton." "mi havas frukton. li bonas kaj malgrandas. mi mangxetas malgrandan frukton."
            , check2 "jan lawa li moku e telo jaki" "gvidisto mangxas malpuran akvon"
            , check2 "mi wile e ilo moku" "mi volas mangxilon"
            , check2 "jan ike li utala e ona mute" "malamiko kontrauxbatalas ilin"
            , check2 "jan ike ni li jo e len nasa" "tio malamiko havas frenezan veston"
            , check2 "mi mute li moku e telo nasa mute" "ni mangxas multajn alkoholojn" 
            , check2 "jan lili li lukin e jan suli" "infano vidas plenkreskulon"
            , check2 "mi lukin sewi e tomo suli" "mi vidas supere grandan domon"
            , check2 "seli suno li seli e tomo mi." "suna varmo varmigas mian domon."
            , check2 "jan lili li wile e telo kili" "infano volas fruktan akvon"
            , check2 "ona mute li nasa e jan suli." "ili frenezigas plenkreskulon."
            , check "mi lawa pona e jan." "mi gvidas bone personon."
            , check "mi utala ike" "mi batalas malbone"
            , check "sina lukin sewi e suno" "vi vidas supere sunon"
            , check "ona li wile mute e ni." "li volas multe tion."
            , check2 "mi mute li lukin lili e ona." "ni vidas malgrande lin."
        ]]

lesson6 :: Test
lesson6 = TestList 
    [TestLabel "Lesson #6 http://tokipona.net/tp/janpije/okamasona6.php" $ TestList [
            check "lon" "ekzistado"
            , check "mi lon" "mi estas"
            , check "mi lon tomo" "mi estas dome"
            , check "mi moku lon tomo" "mi mangxas ekziste dome"
            , check "suno li lon sewi." "suno estas supere."
            , checkall "mi telo e mi lon tomo telo" "mi malsekigas min dome malseke" "mi malsekigas min bancxambre"
            , checkall "mi telo e mi li lon tomo telo" "mi malsekigas min kaj estas dome malseke" "mi malsekigas min kaj estas bancxambre"
            , check "kili li lon poki" "frukto estas uje"
            , check "sina lon e wile sina" "vi farigas vian volon"
            , check "sina wile lon e ona!" "vi volas farigi lin!"
            , check "mi kepeken e ilo." "mi uzas ilon."
            , check "sina wile kepeken e ilo." "vi volas uzi ilon."
            , check "mi kepeken e poki ni." "mi uzas tion ujon."
            , checkall "mi moku kepeken ilo moku." "mi mangxas per mangxa ilo." "mi mangxas per mangxilo."
            , checkall "mi lukin kepeken ilo suno." "mi vidas per suna ilo." "mi vidas per lampo."
            , check "mi tawa tomo mi." "mi iras mian domon."
            , check "ona li tawa utala." "li iras batalon."
            , checkall "sina wile tawa telo suli." "vi volas iri grandan akvon." "vi volas iri oceanon."
            , check "ona li tawa sewi kiwen." "li iras sxtonan superon."
            , check "mi tawa e kiwen." "mi movas sxtonon."
            , check "ona li tawa e len mi." "li movas mian veston."
            , check "mi toki tawa sina." "mi alparolas vin."
            , check "ona li lawa e jan tawa ma pona." "li gvidas personon al bona lando."
            , check "ona li kama tawa ma mi." "li venas mian landon."
            , check "ni li pona tawa mi." "tio bonas min."
            , check "ni li ike tawa mi." "tio malbonas min."
            , check "kili li pona tawa mi." "frukto bonas min."
            , check "toki li pona tawa mi." "parolo bonas min."
            , check "tomo li ike tawa mi." "domo malbonas min."
            , checkall "telo suli li ike tawa mi." "granda akvo malbonas min." "oceano malbonas min." 
            , check "mi lukin e ma. ni li pona tawa mi." "mi vidas landon. tio bonas min."
            , checkall "ma li pona lukin." "lando bonas vide." "lando belas."
            , check "mi pona e tomo tawa jan pakala." "mi bonigas domon al detruita persono."
            , checkall "tomo tawa" "movigxa domo" "veturilo"
            , checkall "tomo tawa telo" "malseka movigxa domo" "sxipo"
            , checkall "tomo tawa kon" "aera movigxa domo" "aviadilo"
            , checkall "mi pana e tomo tawa sina." "mi donas domon al vi." "mi donas vian veturilon."
            , check "sina toki e ni tawa mi: sina moku." "vi diras tion al mi: vi mangxas."
            , check "ona li kama tawa tomo mi." "li venas mian domon."
            , check "mi kama e pakala." "mi ekigas/ekvenas detruon."
            , check "sina kama e ni: mi wile moku." "vi ekigas/ekvenas tion: mi volas mangxi."
            , checkall "mi kama jo e telo." "mi ekigas/ekvenas have akvon." "mi ekhavas akvon."
            , checkall "mi pona e ilo suno kepeken ilo lili" "mi bonigas sunan ilon per malgranda ilo" "mi bonigas lampon per malgranda ilo"
            , check "toki pona li pona tawa mi" "bona parolo bonas min"
            , checkall "mi mute li pona e moku tawa ona mute" "mi multas kaj bonigas mangxon al li multas" "ni bonigas mangxon al ili"
            , checkall "ni li tawa jan pona mi" "tio iras mian bonan personon" "tio iras mian amikon"
            , check "ilo li lon poki" "ilo estas uje"
            , check "poki ni li lon jaki" "tio ujo estas malpure"
            , checkall "mi wile tawa tomo ona kepeken tomo tawa mi" "mi volas iri lian domon per mia movigxa domo" "mi volas iri lian domon per mia veturilo"
            , checkall "ona mute li utala toki" "li multas kaj batalas parole" "ili batalas parole"
            , checkall "sina wile kama tawa tomo toki." "vi volas veni parolan domon." "vi volas veni babilcxambron."
            , checkall "jan li toki kepeken toki pona lon tomo toki." "persono alparolas per bona parolo en parola domo." "persono alparolas per tokipono en babilcxambro."
            , checkall "mi tawa tomo toki. ona li pona tawa mi." "mi iras parolan domon. li bonas min." "mi iras babilcxambron. li bonas min."
            , check2 "sina kama jo e jan pona lon ni." "vi ekhavas amikon cxi tien." 
            , check "poki" "ujo"
            , check "!" "!"
            , check "." "."
            , check ":" ":"
            , check "kepeken" "uzado"
            , check "tawa" "irado"
            , check "kiwen" "sxtono"
            , check "toki" "parolo"
            , check "kama" "veno"
            , check "kon" "aero"
            , check "pana" "dono"
        ]]


lesson7 :: Test
lesson7 = TestList 
    [TestLabel "Lesson #7 http://tokipona.net/tp/janpije/okamasona7.php" $ TestList [
            check "ona li lon sewi mi." "li estas super mi."
            , check "pipi li lon anpa mi." "insekto estas sub mi."
            , check "moku li lon insa mi. " "mangxo estas en mi."
            , check "len li lon monsi mi." "vesto estas malantaux mi."
            , check "mi moku lon poka sina." "mi mangxas ekziste apud vi."
            , check "sina lukin e ona lon poka mi." "vi vidas lin apud mi."
            , check "sewi mi." "mia supero."
            , check "mi lon sewi tomo." "mi estas super domo."
            , checkall "mi anpa e jan utala." "mi malpliigi batalan personon." "mi malpliigi bataliston."
            , check "jan ni li sama mi." "tio persono egalas mi."
            , check "ona li lukin sama pipi." "li vidas same insekto."
            , check "sama li ike" "samo malbonas"
            , checkall "jan sama" "sama persono" "gefrato"
            , check "mi moku tan ni: mi wile moku" "mi mangxas tial tio: mi volas mangxi"
            , check "mi tan ma ike." "mi venas el malbona lando."
            , checkall "jan pona mi li lon poka mi" "mia bona persono estas apud mi" "mia amiko estas apud mi"
            , check "suno li lon sewi mi" "suno estas super mi"
            , check "ma li lon anpa mi" "lando estas sub mi"
            , check "ijo ike li lon monsi mi" "malbona io estas malantaux mi"
            , check "mi pona tan mi: mi lon." "mi bonas tial mi: mi estas."
            , check "mi lukin e ma lon poka tomo" "mi vidas landon apud domo"
            --, check "mi lukin e ma lon poka pi jan pona mi"
            , check "jan li lukin sama pipi" "persono vidas same insekto"
            , check "jan li sama lukin e pipi" "persono egalas vide insekton" 
            , check "poka mi li pakala" "mia apudo detruigxas" -- BOF
            , check "mi kepeken e poki e ilo moku." "mi uzas ujon kaj mangxan ilon."
            , check "jan li lon insa tomo." "persono estas en domo."
            , check "anpa" "grundo"
            , check "insa" "interno"
            , check "monsi" "malantauxado"
            , check "poka" "apudo"
            , check "sama" "samo"
            , check "tan" "tial"
        ]]

lesson8 :: Test
lesson8 = TestList 
    [TestLabel "Lesson #8 http://tokipona.net/tp/janpije/okamasona8.php" $ TestList [
            check "ala" "ne"
            , check "musi" "amuzo"
            , check "ale" "cxio"
            , check "ali" "cxio"
            , check "pali" "laboro"
            , check "ken" "eble"
            , check "sona" "scio"
            , check "lape" "dormo"
            , check "wawa" "forto"
            , check "mi lape ala." "mi dormas ne."
            , check "mi musi ala." "mi amuzigxas ne."
            , check "mi wawa ala." "mi fortas ne."
            , checkall "mi wile ala tawa musi." "mi volas ne iri amuzon." "mi volas ne danci."
            , check "jan ala li toki." "ne persono alparolas."
            , check "ijo ala li jaki." "ne io malpuras." -- google :(
            , check "ala li jaki" "nenio malpuras"
            , checkall "jan ali li wile tawa." "cxio personoj volas iri." "cxiu volas iri." -- TODO cxio -> cxiu
            , check "ma ali li pona." "cxio landoj bonas."
            , check "ali li pona" "cxio bonas"
            , check "ijo ali li pona" "cxio ioj bonas" -- google :( but it is ok.
            , check "sina pona ala pona?" "cxu vi bonas?"
            , check "sina pona ala pona" "cxu vi bonas"
            , check "?" "?"
            , check "" ""
            , check "sina pona ala pona? mi wile sona e ni." "cxu vi bonas? mi volas scii tion."
            , check "sina pona ala pona? mi wile sona e ni. sina sona ala sona e ni?" "cxu vi bonas? mi volas scii tion. cxu vi scias tion?"
            , check "suno li suli ala suli?" "cxu suno grandas?"
            , check "len sina li telo ala telo?" "cxu via vesto malsekas?"
            , checkall "tomo tawa sina li pakala ala pakala?" "cxu via movigxa domo detruigxas?" "cxu via veturilo detruigxas?"
            , check "sina ken ala ken lape?" "cxu vi povas dormi?"
            , check "ona li lon ala lon tomo?" "cxu li estas dome?"
            , check "ona li tawa ala tawa ma ike?" "cxu li iras al malbona lando?"
            , checkall "sina pana ala pana e moku tawa jan lili?" "cxu vi donas mangxon al malgranda persono?" "cxu vi donas mangxon al infano?"
            , check "pipi li moku ala moku e kili?" "cxu insekto mangxas frukton?"
            , check "sina wile ala wile moku?" "cxu vi volas mangxi?"
            , check "sina lukin ala lukin e kiwen?" "cxu vi vidas sxtonon?"
            , check "sina sona ala sona e toki mi?" "cxu vi scias mian parolon?"
            , check "mi sona ala e tan." "mi scias ne tial."
            , check "ni li musi ala musi? musi." "cxu tio amuzigxas? amuzo."
            , check "sina wile toki e tan tawa mi." "vi volas diri tial al mi." -- BOF
            , check "pipi li lon ala lon poka mi?" "cxu insekto estas apud mi?"
            , check "mi pona ala pona tawa sina?" "cxu mi bonas vin?"
            , check "mi ken ala lape." "mi povas ne dormi."
            , check "mi wile ala toki tawa sina." "mi volas ne alparoli vin."
            , check "ona li tawa ala telo." "li iras malsekan nenion." -- KO
            , check "sina wile ala wile pali? wile ala." "cxu vi volas labori? ne volo."
            , checkall "jan utala li seli ala seli e tomo?" "cxu batala persono varmigas domon?" "cxu batalisto varmigas domon?" -- CTX
            , checkall "jan lili li ken ala moku e telo nasa." "malgranda persono povas ne mangxi frenezan akvon." "infano povas ne mangxi alkoholon."
            , check "sina kepeken ala kepeken e ni?" "cxu vi uzas tion?"
            , check "sina ken ala ken kama?" "cxu vi povas veni?"
        ]]

lesson9 :: Test
lesson9 = TestList 
    [TestLabel "Lesson #9 http://tokipona.net/tp/janpije/okamasona9.php" $ TestList [
            check "mama" "gepatro"
            , check "meli" "ino"
            , check "mije" "viro"
            , check2 "mama mije" "patro"
            , check2 "mama meli" "patrino"
            , checkall "ma Kanata li pona." "kanada lando bonas." "Kanado bonas."
            , checkall "ma Wensa li nasa." "svedia lando frenezas." "Svedio frenezas."
            , checkall "ma Lowasi li pona lukin." "kroatia lando bonas vide." "Kroatio belas."
            , checkall "mi wile tawa ma Netelan." "mi volas iri nederlandan landon." "mi volas iri Nederlandon."
            , checkall "ma tomo Lantan li suli." "Lantan doma lando grandas." "Londono grandas."
            , checkall "ma tomo Pelin" "doma lando Pelin" "Berlino"
            , check "ma tomo Elena" "Elena doma lando" -- TODO find the name of altantic city in espertanto
            , checkall "ma tomo Loma" "Loma doma lando" "Romo"
            , checkall "ma Inli li pona." "anglia lando bonas." "Anglio bonas."
            , checkall "toki Inli li pona." "anglia parolo bonas." "anglalingvo bonas."
            , checkall "jan Inli li pona." "anglia persono bonas." "anglo bonas."
            , check "meli Italija" "italia ino"
            , check "mije Epanja" "hispania viro"
            , check "jan Lewi li utala e jan Ten." "Lewi kontrauxbatalas Ten."
            , check "jan Pentan li pana e sona tawa mi." "Pentan donas scion al mi."
            , check "jan Mewi li toki tawa mi." "Mewi alparolas min."
            , check "jan Nesan li musi." "Nesan amuzigxas."
            , check "mi jan Pita" "mi homas Pita"
            , check "nimi mi li jan Pita" "mia nomo homas Pita"
            , check "jan Luta o!" "Luta!"
            , check "jan Ken o, pipi li lon len sina." "Ken, insekto estas en via vesto."
            , checkall "jan Keli o, sina pona lukin." "Keli, vi bonas vide." "Keli, vi belas."
            , check "jan Mawen o, sina wile ala wile moku?" "Mawen, cxu vi volas mangxi?"
            , check "jan Tepani o, sina ike tawa mi." "Tepani, vi malbonas min."
            , check "jan Epi o a!" "Epi ah!"
            , check "o pali!" "laboru!"
            , check "o awen!" "atendu!"
            , check "o lukin e ni." "vidu tion."
            , checkall "o tawa ma tomo lon poka mi." "iru doman landon apud mi." "iru urbon apud mi."
            , check "jan San o tawa tomo sina." "San iru vian domon."
            , check "jan Ta o toki ala tawa mi." "Ta alparolu ne min." -- google bad for the name
            , check "jan Sesi o moku e kili ni." "Sesi mangxu tion frukton."
            , check2 "mi mute o tawa." "ni iru."
            , check2 "mi mute o musi." "ni amuzigxu."
            , check "mu!" "besto-bruo!"
            , check "a a a!" "ah ah ah!"
            , checkall "suno pona!" "bona suno!" "bonatago!"
            , checkall "lape pona!" "bona dormo!" "bonanokto!"
            , checkall "moku pona!" "bona mangxo!" "bon-apetito!"
            , check "mi tawa!" "mi iras!"
            , check "tawa pona!" "bona irado!"
            , checkall "kama pona!" "bona veno!" "bonveno!"
            , check "musi pona!" "bona amuzo!"
            , check "jan Susan li nasa" "Susan frenezas" 
            , check "o tawa!" "iru!"
            , checkall "mama meli o tawa!" "ina gepatro iru!" "patrino iru!" 
            , checkall "mi kama tan ma Elopa." "mi venas tial euxropa lando." "mi venas tial Euxropo."
            , check "a a a! ni li musi." "ah ah ah! tio amuzigxas."
            , check "nimi mi li Ken" "mia nomo Ken-estas"
            , check "jan Lisa o, toki!" "Lisa, parolo!"
            , check "pakala!" "detruo!"
            , checkall "mi wile tawa ma Oselija" "mi volas iri auxstralian landon" "mi volas iri Auxstralion"
            , checkall "mi wile kama sona e toki Inli." "mi volas ekigi/ekveni scie anglian parolon." "mi volas lerni anglalingvon."
            , check "jan Ana o pana e moku tawa mi." "Ana donu mangxon al mi."
            , checkall "o tawa musi lon poka mi!" "iru amuzon apud mi!" "dancu ekziste apud mi!"
            , checkall "jan Mose o lawa e mi mute tawa ma pona." "Mose gvidu min multon al bona lando." "Mose gvidu nin al bona lando."
        ]]

lesson2 :: Test
lesson2 = TestList 
    [TestLabel "Lesson #2 http://tokipona.net/tp/janpije/okamasona2.php" $ TestList [
            testWord "A" True
            , testWord "p" False
            , testWord "P" False
            , testWord "N" False
            , testWord "Naap" False
            , testWord "Nap" False
            , testWord "Na" True
            , testWord "Nan" True
            , testWord "Nanù" False
            , testWord "Nanop" False
            , testWord "Nanopo" True
            , testWord "Nanoppu" True
            , testWord "Nanopppu" False
            , testWord "Nanonmu" False
            , testWord "Nanomnu" True
            , testWord "Nanonum" False
            , testWord "Nanonun" True
            , testWord "NanoNun" False
            , testWord "Najinun" False
            , testWord "Natinun" False
            , testWord "Nawonun" False
            , testWord "Nawunun" False
            , testWord "Tepani" True
        ]]


lesson10 :: Test
lesson10 = TestList 
    [TestLabel "Lesson #3 http://tokipona.net/tp/janpije/okamasona10.php" $ TestList [
            check "seme li utala e sina?" "kio kontrauxbatalas vin?"
            , check "seme li moku e kili mi?" "kio mangxas mian frukton?" -- google :(
            , check "seme li lon poka mi?" "kio estas apud mi?"
            , check "seme li lon tomo mi?" "kio estas en mia domo?"
            , check "seme li pona tawa sina?" "kio bonas vin?"
            , check "sina lukin e ijo." "vi vidas ion."
            , check "sina lukin e seme?" "vi vidas kion?"
            , check "seme lukin sina?" "via vida kio?" -- stultas sed bona tradukto
            , check "sina pakala e seme?" "vi detruas kion?"
            , check "sina lon seme?" "vi estas kie?"
            , check "ona li jo e seme?" "li havas kion?"
            , checkall "jan seme li moku?" "kia persono mangxas?" "kiu mangxas?" 
            , checkall "jan seme li tawa lon poka sina?" "kia persono iras apud vi?" "kiu iras apud vi?" -- bad google
            , checkall "sina lukin e jan seme?" "vi vidas kian personon?" "vi vidas kiun?"
            , checkall "sina toki tawa jan seme?" "vi alparolas kian personon?" "vi alparolas kiun?"
            , check "ma seme li pona tawa sina?" "kia lando bonas vin?"
            , check "sina kama tan ma seme?" "vi venas tial kia lando?"
            , checkall "sina ike tan seme?" "vi malbonas tial kio?" "vi malbonas kial?"
            , check "supa" "surfaco"
            , checkall "supa lape" "dorma surfaco" "lito"
            , check "suwi" "dolcxajxo"
            , checkall "jan lili sina li suwi." "via malgranda persono dolcxas." "via infano dolcxas."
            , check "telo kili ni li suwi." "tio frukta akvo dolcxas."
            , check "mi wile e suwi!" "mi volas dolcxajxon!"
            , check "jan sin li kama" "alia persono venas"
            , check "mi wile e suwi sin!" "mi volas alian dolcxajxon!"
            , check "mi olin e sina" "mi amas vin"
            , check2 "sina lukin olin e jan lili sina..." "vi vidas ame vian infanon..."
            , check "sina wile pali e seme?" "vi volas fari kion?"
            , check2 "jan seme li olin e sina?" "kiu amas vin?" 
            , check "ni li suwi ala suwi?" "cxu tio dolcxas?"
            , check2 "mi tawa supa lape" "mi iras liton"
            , check "jan sin li kama ala kama?" "cxu alia persono venas?"
            , check "o pana e suwi tawa mi!" "donu dolcxajxon al mi!"
            , checkall "jan seme li lon?" "kia persono estas?" "kiu estas?"
            , check2 "jan seme li lon ni?" "kiu cxi tie?"
            , check "pipi seme li pakala e sina?" "kia insekto detruas vin?"
            , check "sina tawa poka jan seme?" "vi iras kian homan apudon?"
            , check "moku li pona tawa ona" "mangxo bonas lin"
            , check "jan Ken o, mi olin e sina." "Ken, mi amas vin."
            , check2 "ni li jan seme?" "tio kiu-estas?"
            , check "sina lon seme?" "vi estas kie?"
            , checkall "mi lon tan seme?" "mi estas tial kio?" "mi estas kial?"
            , check2 "jan seme li meli sina?" "kiu ino-estas via?"
            , checkall "sina tawa ma tomo tan seme?" "vi iras doman landon tial kian?" "vi iras urbon kial?"
            , check "sina wile tawa ma seme?" "vi volas iri kian landon?"
        ]]

lesson11 :: Test
lesson11 = TestList 
    [TestLabel "Lesson #11 http://tokipona.net/tp/janpije/okamasona11.php" $ TestList [
            checkall "mi tawa tomo telo nasa." "mi iras frenezan malsekan domon." "mi iras frenezan tualetejon."
            , checkall "mi tawa tomo pi telo nasa." "mi iras domon de freneza akvo." "mi iras drinkejon."
            , checkall "jan pi ma tomo" "persono de doma lando" "persono de urbo"
            , checkall "kulupu pi toki pona" "grupo de bona parolo" "grupo de tokipono" -- bad google
            , checkall "nasin pi toki pona" "vojo de bona parolo" "vojo de tokipono"
            , checkall "jan lawa pi jan utala" "grava persono de batala persono" "gvidisto de batalisto"
            , checkall "jan lawa pi tomo tawa kon" "grava persono de aera movigxa domo" "gvidisto de aviadilo"
            , checkall "jan pi nasin sewi Kolisu" "persono de kristiana supera vojo" "persono de kristiana religio"
            , checkall "jan pi pona lukin" "persono de vida bono" "persono de belo"
            , checkall "jan pi ike lukin" "persono de vida malbono" "persono de malbelo"
            , check "kili mi" "mia frukto"
            , check "kili pi jan Susan" "frukto de Susan"
            , check "ma ona" "lia lando"
            , check "ma pi jan Keli" "lando de Keli"
            , check "len jan" "homa vesto"
            , check "len pi jan Lisa" "vesto de Lisa"
            , check2 "nimi pi mi mute" "nomo de ni"
            , check2 "tomo pi ona mute" "domo de ili"
            , check "jan wawa" "forta persono"
            , check "jan pi wawa ala" "persono de ne forto"
            , check "mi toki pi jan." "mi alparolas pri persono." 
            , check "mi toki jan." "mi alparolas home." -- BAD traduction && BAD Goole ;(
            , check "mi toki pi jan ike" "mi alparolas pri malbona persono"
            , check "jan pi ma" "persono de lando" 
            , checkall "mi kama tan ma Mewika." "mi venas tial usona lando." "mi venas tial Usono."
            , checkall "mi kama pi ma Mewika" "mi venas pri usona lando" "mi venas pri Usono" -- Works but it is a bad usage 
            , check "kili ni li pi mi." "tio frukto de mi."
            , check2 "tomo ni li pi jan Tami." "tio domo de Tami."
            , check "ilo ni li pi sina." "tio ilo de vi."
            , check2 "ma ni li pi jan Tosi." "tio lando de germano."
            , check2 "toki ni li pi mi mute." "tio parolo de ni."
            , check "kalama" "bruo"
            , check "kalama ni li seme?" "tio bruo kio-estas?" 
            , checkall "kalama musi li pona tawa mi." "amuza bruo bonas min." "muziko bonas min."
            , checkall "kalama musi \"Jingle Bells\" li pona tawa mi" "amuza bruo \"Jingle Bells\" bonas min" "muziko \"Jingle Bells\" bonas min"
            , checkall "kalama musi pi jan Elton Jon li nasa. " "amuza bruo de Elton Jon frenezas." "muziko de Elton Jon frenezas."
            , check "mi kalama kepeken ilo. " "mi bruas per ilo."
            , check "o kalama ala!" "bruu ne!"
            , check "sina pali e ni kepeken nasin seme?" "vi faras tion per kia vojo?"
            , check2 "jan lili pi jan Keli li musi" "infano de Keli amuzigxas"
            , checkall "mi jan pi toki pona" "mi homas pri bona parolo" "mi homas pri tokipono"
            , check2 "mi toki pona" "mi alparolas tokipone"
            , checkall "ona li jan pona pi kalama musi" "li homas bone pri amuza bruo" "li amikas pri muziko"
            , checkall "jan lawa pi tomo tawa telo li moku" "grava persono de malseka movigxa domo mangxas" "gvidisto de sxipo mangxas"
            , checkall "kalama musi pi jan Enja li pona" "amuza bruo de Enja bonas" "muziko de Enja bonas"
            , checkall "jan seme pi kulupu ni li suli?" "kia persono de tio grupo grandas?" "kiu de tio grupo grandas?"
            , check2 "tomo pi mi mute li pakala" "domo de ni detruigxas"
            , check "ona li pali ni kepeken nasin seme?" "li laboras tio per kia vojo?"
            , check "kili pi jan Linta li ike." "frukto de Linta malbonas."
            , check "len pi jan Nina li jaki." "vesto de Nina malpuras."
            , check2 "mi sona ala e nimi pi ona mute." "mi scias ne nomon de ili."
            , checkall "mi wile ala toki pi kalama musi." "mi volas ne alparoli pri amuza bruo." "mi volas ne alparoli pri muziko."
            , check "mi wile toki meli." "mi volas alparoli ine."
            , check "mi wile toki pi meli." "mi volas alparoli pri ino."
            , check "sina pakala e ilo kepeken nasin seme?" "vi detruas ilon per kia vojo?"
            , checkall "jan Wasintan li jan lawa pona pi ma Mewika." "Wasintan homas grave bone pri usona lando." "Wasintan gvidistas bone pri Usono."
            , checkall "wile pi jan ike li pakala e ijo." "volo de malbona persono detruas ion." "volo de malamiko detruas ion."
        ]]

lesson12 :: Test
lesson12 = TestList 
    [TestLabel "Lesson #12 http://tokipona.net/tp/janpije/okamasona12.php" $ TestList [
            check "jan Susan anu jan Lisa li moku e suwi?" "Susan aux Lisa mangxas dolcxajxon?"
            , checkall "sina jo e kili anu telo nasa?" "vi havas frukton aux frenezan akvon?" "vi havas frukton aux alkoholon?"
            , check "sina toki tawa mi anu ona?" "vi alparolas min aux lin?"
            , check "ona anu jan ante li ike?" "li aux alia persono malbonas?"
            , check "sina toki pi mama anu jan lili? " "vi alparolas pri gepatro aux malgranda persono?"
            , check "sina kama anu seme?" "vi venas aux kio?"
            , check "sina wile moku anu seme?" "vi volas mangxi aux kio?"
            , check "sina wile e mani anu seme?" "vi volas monon aux kion?"
            , check2 "mi en sina li jan pona." "mi kaj vi amikas."
            , check2 "jan lili en jan suli li toki." "infano kaj plenkreskulo alparolas."
            , check2 "kalama musi en meli li pona tawa mi." "muziko kaj ino bonas min."
            , check "mi wile e moku e telo." "mi volas mangxon kaj akvon."
            , check "mi wile e moku en telo." "mi volas mangxon kaj akvon."
            , check "mi moku e kili li lukin e tomo." "mi mangxas frukton kaj vidas domon."
            , check "mi moku e kili en lukin e tomo." "mi mangxas frukton kaj vidajxon kaj domon." -- stranga sed bona per la tokiponaj regloj
            , check "mi moku e kili, en mi lukin e tomo." "mi mangxas frukton, kaj mi vidas domon."
            , check "tomo pi jan Keli en mije ona li suli." "domo de Keli kaj lia viro grandas." -- ambiguos in all language
            , check2 "jan lili pi jan Ken en jan Melisa li suwi. " "infano de Ken kaj Melisa dolcxas." -- ambiguos in all language          
            , check "mi wile moku. taso mi jo ala e moku." "mi volas mangxi. sed mi havas ne mangxon."
            , check "mi wile lukin e tomo mi. taso mi lon ma ante. " "mi volas vidi mian domon. sed mi estas lande alie."
            , check "mi pona. taso meli mi li pakala." "mi bonas. sed mia ino detruigxas."
            , check "mi pona, taso meli mi li pakala." "mi bonas, sed mia ino detruigxas."
            , check "mi pona taso meli mi li pakala." "mi bonas nur ine mia kaj detruigxas."
            , check "jan Lisa taso li kama." "nur Lisa venas."
            , check "mi sona e ni taso." "mi scias nur tion."
            , check "mi musi taso." "mi amuzigxas nur."
            , check "mi pali taso" "mi laboras nur"
            , check "mi lukin taso e meli ni! ali li pona." "mi vidas nur tion inon! cxio bonas."
            , check "mi tawa ma Elopa. pona! mi tawa kin." "mi iras euxropan landon. bono! mi iras ja."
            , check "mi mute o tawa. mi ken ala. mi moku kin." "mi multas iru. mi povas ne. mi mangxas ja."
            , check "a! sina lukin ala lukin e ijo nasa ni? mi lukin kin e ona." "ah! cxu vi vidas tion frenezan ion? mi vidas ja lin."
            , check "seli li lon" "varmo estas"
            , check "lete li lon" "malvarmo estas"
            , check "seli mute li lon" "multaj varmoj estas"
            , check "lete mute li lon" "multaj malvarmoj estas"
            , check "seli lili li lon" "malgranda varmo estas"
            , check "lete lili li lon" "malgranda malvarmo estas"
            , check "ilo ni li lete pilin." "tio ilo malvarmas sente."
            , check "ni li lete pilin mute." "tio malvarmas sente multe."
            , check "ni li seli pilin lili." "tio varmas sente malgrande."
            , check "mi pilin pona." "mi sentas bone."
            , check "mi pilin ike" "mi sentas malbone"
            , check "sina pilin e seme?" "vi pensas kion?"
            , check "sina pilin seme?" "vi sentas kiel?"
            , check "mi pilin e ni: sina jan ike." "mi pensas tion: vi homas malbone."
            , check "mi pilin ijo" "mi sentas objekte"
            , check "mi pilin pi ijo" "mi sentas pri io"
            , check "mi pilin e ijo" "mi pensas ion"
            , check "mi kama sona e tomo" "mi ekigas/ekvenas scie domon"
            , check "sina wile kama anu seme?" "vi volas veni aux kio?"
            , check "sina wile e moku anu telo?" "vi volas mangxon aux akvon?"
            , check "mi wile kin tawa tomo mi" "mi volas ja iri mian domon"
            , check "lipu ni li lete pilin" "tio papero malvarmas sente"
            , check "mani pi ma ante li pona tawa mi" "mono de alia lando bonas min"
            , check "mi wile tawa. taso mi ken ala" "mi volas iri. sed mi povas ne"
            , check2 "mi taso li lon" "mi nur estas" 
            , check "mi olin kin e sina." "mi amas ja vin."
            , check "mi pilin e ni: ona li jo ala e mani." "mi pensas tion: li havas ne monon."
            , check "mi wile lukin e ma ante." "mi volas vidi alian landon."
            , check "mi wile ala e ijo. mi lukin taso." "mi volas ne ion. mi vidas nur."
            , check "sina wile toki tawa mije anu meli?" "vi volas alparoli viron aux inon?"
        ]]

lesson13 :: Test
lesson13 = TestList 
    [TestLabel "Lesson #13 http://tokipona.net/tp/janpije/okamasona13.php" $ TestList [
            check "len laso" "blua vesto"
            , checkall "len laso jelo" "flava blua vesto" "verda vesto"
            , check "len pi loje en laso" "vesto de rugxo kaj bluo"
            , check2 "len pi loje laso" "vesto de purpuro"
            , check2 "len loje laso" "purpura vesto"
            , check "len loje en laso li pona." "rugxa vesto kaj bluo bonas."
            , check "ni li kule seme?" "tio koloras kiel?"
            , check "mi kule e lipu" "mi koloras paperon"
            , check2 "sitelen tawa \"The Simpsons\" li pona tawa mi." "video \"The Simpsons\" bonas min."
            , check "mi lukin ala e poki laso" "mi vidas ne bluan ujon"
            , check2 "jan jelo laso lili kama tan sewi" "venanta malgranda verda persono de supera"
            , check2 "jan laso jelo lili kama tan sewi" "venanta malgranda verda persono de supera"
            , check2 "kule laso loje li pona tawa mi" "purpura koloro bonas min"
            , check2 "kule loje laso li pona tawa mi" "purpura koloro bonas min"
            , check2 "sewi li laso" "supero bluas"
            , check2 "o lukin e pipi loje ni" "vidu tion rugxan insekton"
            , check2 "mi wile e sitelen ma" "mi volas mapon"
            , check2 "sina lukin ala lukin e sitelen tawa \"Breaking Bas\"?" "cxu vi vidas videon \"Breaking Bas\"?" 
            , check "kule seme li pona tawa sina" "kia koloro bonas vin"
            , check "suno li jelo." "suno flavas."
            , checkall "telo suli li laso." "granda akvo bluas." "oceano bluas."
            , check "mi wile moku e kili loje." "mi volas mangxi rugxan frukton."
            , checkall "ona li kule e tomo tawa." "li koloras domon al." "li koloras veturilon."
            , check2 "ma mi li pimeja. kalama ala li lon. mi lape. mi sona." "mia lando obskuras. ne bruo estas. mi dormas. mi scias."
        ]]

lesson14 :: Test
lesson14 = TestList 
    [TestLabel "Lesson #14 http://tokipona.net/tp/janpije/okamasona14.php" $ TestList [
            checkall "akesi pi telo moli" "reptilio de morta akvo" "reptilio de veneno"
            , checkall "len laso jelo" "flava blua vesto" "verda vesto"
            , check "akesi suli" "granda reptilio"
            , check2 "jan kala" "sireno"
            , check "soweli ni li pona moku" "tio besto bonas mangxe"
            , checkall "mi mute o moku e waso lon tomo \"Chik-Fil-A.\"" "mi multigas mangxu birdon dome \"Chik-Fil-A. \"" "ni mangxu birdon dome \"Chik-Fil-A. \""
            , check "pipi li moli." "insekto mortas."
            , check "jan moli li toki ala" "morta persono alparolas ne"
            , checkall "jan utala pakala li kama moli." "detruita batala persono venas morte." "detruita batalisto ekmortigxas."
            , checkall "jan utala li kama moli e jan ike." "batala persono ekigas/ekvenas morte malbonan personon." "batalisto ekmortigas malamikon."
            , checkall "soweli pi kama moli li kalama ike." "besto de morta veno bruas malbone." "ekmortajxanta besto bruas malbone."
            , checkall "mi pi kama moli li kalama ike." "mi de morta veno bruas malbone." "mi ekmortigxas kaj bruas malbone."
            , check "jan li moli e waso" "persono mortigas birdon"
            , check "mi wile e soweli lili" "mi volas malgrandan beston"
            , check "a! akesi li wile moku e mi!" "ah! reptilio volas mangxi min!"
            , check "pipi li moku lili e mi" "insekto mangxas malgrande min"
            , check "soweli li toki e mu!" "besto diras besto-bruon!" -- google ;(
            , check "waso li tawa lon kon" "birdo iras aere"
            , check2 "mi mute o moku e kala!" "ni mangxu fisxon!"
            , checkall "kasi kule li pona lukin" "kolora planto bonas vide" "floro belas"
            , check "kasi li pona tawa mi" "planto bonas min"
            , check2 "mama ona li kepeken e kasi nasa." "lia gepatro uzas hasxisxon."
            , check2 "akesi li pana e telo moli." "reptilio donas venenon."
            , check "pipi li moku e kasi." "insekto mangxas planton."
            , check2 "soweli mi li kama moli." "mia besto ekmortigxas."
            , check "jan Pawe o, mi wile ala moli." "Pawe, mi volas ne morti."
            , check2 "mi lon ma kasi." "mi estas arbare."
        ]]

lesson15 :: Test
lesson15 = TestList 
    [TestLabel "Lesson #15 http://tokipona.net/tp/janpije/okamasona15.php" $ TestList [
            checkall "mi pana e ko jaki" "mi donas malpuran duonsolidon" "mi donas fekon"
            , checkall "mi ko jaki" "mi duonsolidas malpure" "mi fekas"
            , check "mi kute e sina" "mi auxskultas vin"
            , checkall "mi kute e kalama musi kepeken nena kute mi." "mi auxskultas amuzan bruon per mia auxskulta tubero." "mi auxskultas muzikon per mia orelo."
            , check "o open e lupa!" "malfermu truon!"
            , checkall "jan utala li pana e telo sijelo loje" "batala persono donas rugxan korpan akvon" "batalisto donas sangon"
            , check "o uta e mi!" "kisu min!"
            , check "o pilin e uta mi kepeken uta sina!" "pensu mian busxon per via busxo!"
            , check2 "mi wile pana e telo jelo!" "mi volas doni urinon!"
            , check "linja mi li telo" "mia filamento malsekas"
            , checkall "linja lawa mi li telo" "mia grava filamento malsekas" "mia hararo malsekas"
            , check "ijo li lon oko mi" "io estas en mia okulo"
            , check "mi ken ala kute e toki sina" "mi povas ne auxskulti vian parolon"
            , check2 "mi wile pana e ko jaki" "mi volas doni fekon"
            , check "lupa ni li suli" "tio truo grandas"
            , check2 "ike! telo sijelo loje li kama tan nena kute mi!" "malbono! sango venas tial mia orelo!"
            , check "selo mi li wile e ni: mi pilin e ona." "mia hauxto volas tion: mi pensas lin."
            , check "o pilin e nena." "pensu tuberon."
            , checkall "o moli e pipi kepeken palisa anu len noka sina." "mortigu insekton per paliso aux via pieda vesto." "mortigu insekton per paliso aux via sxuo."
            , check "luka mi li jaki. mi wile telo e ona." "mia mano malpuras. mi volas malsekigi lin."
            , check "o pana e sike tawa mi." "donu rondon al mi."
            , check "mi pilin e seli sijelo sina." "mi pensas vian korpan varmon."
        ]]

lesson16 :: Test
lesson16 = TestList 
    [TestLabel "Lesson #16 http://tokipona.net/tp/janpije/okamasona16.php" $ TestList [
            checkall "ijo luka tu pi pona lukin" "du manaj ioj de vida bono" "7 ioj de belo"
            , checkall "akesi wan" "unu reptilio" "unu reptilio"
            , checkall "kasi tu" "du plantoj" "du plantoj"
            , checkall "nimi luka luka luka luka tu" "du manaj manaj manaj manaj nomoj" "22 nomoj"
            , check2 "tenpo suno luka luka luka luka luka luka luka luka" "40 tagoj"
            , check2 "linja luka luka" "10 filamentoj"
            , check2 "jan pona luka luka tu" "12 amikoj"
            , check2 "tomo tu wan" "3 domoj"
            , check2 "lupa luka tu" "7 truoj"
            , check2 "soweli tu tu" "4 bestoj"
            , check2 "jan mute lili li kama" "malmulta persono venas"
            , check2 "tenpo suno mute luka tu wan" "28 tagoj"
            , checkall "mute mute tu" "du multaj multoj" "42"
            , check2 "jan utala ali ali ali" "300 batalistoj"
            , check2 "jan utala ali" "cxio batalistoj"
            , check2 "mi lukin e jan utala ali" "mi vidas cxion batalistojn"
            , check2 "ma ali mute mute mute mute luka luka luka wan" "196 landoj"
            , check2 "luka luka luka luka" "multaj" -- 20 => multaj
            , check2 "mi lukin e luka luka luka luka" "mi vidas 15 manon" -- manojn ?
            , check2 "luka luka luka luka li lukin e mi" "15 manoj vidas min"
            , check2 "ni li jan lili ona pi nanpa tu wan" "tio infano-estas lia pri 3 numero"
            , check2 "meli mi pi nanpa tu li nasa!" "mia ino de du numero frenezas!" 
            , check2 "o tu e palisa ni." "dividu tion palison." -- Google :(
            , check2 "mi en meli mi li wan." "mi kaj mia ino unigas."
            , check "o weka e len sina." "forigu vian veston."
            , check2 "o weka e jan lili tan ni. ona li wile ala kute e ni." "forigu infanon tial tion. li volas ne auxskulti tion."
            , check "mi weka." "mi malproksimigas."
            , check "mi wile tawa weka." "mi volas iri malproksimon."
            , check2 "tomo mi li weka tan ni." "mia domo malproksimas tio."
            , check2 "ma Posan li weka tan ma Alensina." "Bosnio malproksimas argentine."
            , checkall "ma Mewika li weka ala tan ma Kupa." "usona lando malproksimigas ne tial kuba lando." "Usono proksimas kube."
            , check2 "mi lukin e waso tu wan" "mi vidas 3 birdojn"
            , check2 "jan mute li kama" "multaj personoj venas"
            , check2 "jan pi nanpa wan li lon" "persono de unu numero estas"
            , check2 "mi jo e tomo tawa tu" "mi havas du veturilojn"
            , check2 "jan mute lili li kama" "malmulta persono venas"
            , check2 "o wan!" "unigu!"
            , check2 "mi weka e ijo tu ni." "mi forigas tion du iojn."
            , check2 "o tu." "dividu."
            , check2 "mi lukin e soweli luka." "mi vidas 5 beston."
            , check2 "mi lukin e luka pi soweli luka." "mi vidas manon de 5 besto."
            , check2 "mi weka." "mi malproksimigas."
        ]]

lesson17 :: Test 
lesson17 = TestList 
    [TestLabel "Lesson #17 http://tokipona.net/tp/janpije/okamasona17.php" $ TestList [
            checkall "ken la ilo li pakala." "dum/se eble, ilo detruigxas." "eble ilo detruigxas."
            , checkall "ken la jan Lisa li jo e ona." "dum/se eble, Lisa havas lin." "eble Lisa havas lin."
            , checkall "ken la ona li lape." "dum/se eble, li dormas." "eble li dormas."
            , checkall "ken la mi wile tawa ma Palata." "dum/se eble, mi volas iri hindujan landon." "eble mi volas iri Hindujon."
            , checkall "tenpo suno la mi weka." "dum/se suna tempo, mi malproksimigas." "hodiaux mi malproksimigas."
            , checkall "tenpo suno la, mi weka." "dum/se suna tempo, mi malproksimigas." "hodiaux, mi malproksimigas."
            , check2 "tenpo pini la mi weka." "estintece mi malproksimigas."
            , check2 "tenpo ni la mi lon." "nun mi estas."
            , check2 "tenpo kama la mi lape." "estontece mi dormas."
            , check2 "tenpo pimeja pini la mi kama nasa." "hieraux nokte mi venas freneze."
            , check2 "tenpo suli la jan Metusela li lon." "dum/se granda tempo, Metusela estas."
            , check2 "tenpo suno pi nanpa tu wan la jan Jesu li sewi!" "dum/se tago de 3 numero, Jesu superas!"
            , check2 "tenpo pi mute seme la sina tawa sike e suno?" "dum/se tempo de kia multo, vi movas ronde sunon?"
            , check2 "tenpo pi mute seme la sina sike e suno?" "dum/se tempo de kia multo, vi rondigas sunon?"
            , check2 "tenpo luka luka luka luka luka luka luka luka luka tu tu la mi sike e suno." "dum/se 49 tempoj, mi rondigas sunon."
            , check "tenpo luka luka luka luka luka luka luka luka luka tu tu la mi sike e suno." "dum/se du du manaj manaj manaj manaj manaj manaj manaj manaj manaj tempoj, mi rondigas sunon."
            , check "mama mi li moli la mi pilin ike." "dum/se mia gepatro mortas, mi sentas malbone."
            , check "mi lape la ali li pona." "dum/se mi dormas, cxio bonas."
            , check "sina moku e telo nasa la sina nasa." "dum/se vi mangxas frenezan akvon, vi frenezas."
            , check "sina wawa la sina ken pakala e jan ike." "dum/se vi fortigas, vi povas detrui malbonan personon."
            , check "sina moli la sina ken ala toki." "dum/se vi mortas, vi povas ne alparoli."
            , check "sina sona e jan Jesu la ni li pona tawa mama sewi sina." "dum/se vi scias Jesu, tio bonas vian superan gepatron."
            , checkall "sina unpa la sina ken pali e jan lili." "dum/se vi kunseksumas, vi povas fari malgrandan personon." "dum/se vi kunseksumas, vi povas fari infanon."
            , check2 "ma Mesiko li pona mute. ma Akanisan li pona lili." "Meksiko bonas multe. Afganujo bonas malgrande."
            , check2 "mi suli mute. sina suli lili." "mi grandas multe. vi grandas malgrande."
            , check2 "mi moku mute. sina moku lili." "mi mangxas multe. vi mangxetas."
            , check2 "ken la jan lili li wile moku e telo." "eble infano volas mangxi akvon."
            , check2 "tenpo ali la o kama sona!" "dum/se cxio tempoj, lernu!"
            , check2 "sina sona e toki ni la sina sona e toki pona!" "dum/se vi scias tion parolon, vi scias tokiponon!"
            , check2 "ken la ma Tosi li pona tawa meli mi Sewalen." "eble Germanio bonas mian inon Sewalen."
            , check2 "tenpo pimeja pini la mi lukin e sitelen tawa \"The Walking Dead\"." "hieraux nokte mi vidas videon \"The Walking Dead\"."
            , check2 "jan ike li kama la o seli e lipu ni." "dum/se malamiko ekigas/ekvenas, varmigu tion paperon."
            , check2 "ken la ona li lon tomo sona." "eble li estas lerneje."
            , check2 "tenpo suno kama la mi wile pali." "morgaux mi volas labori."
            , check2 "seli li lon la mi pana e telo tan selo mi." "dum/se varmo farigas, mi donas akvon tial mian hauxtan."
            , check2 "o open e lupa." "malfermu truon."
            , check2 "suno li suli mute. mun li suli lili." "suno grandas multe. luno grandas malgrande."
            , check2 "tenpo pimeja ni la mun li suli." "dum/se tio nokto, luno grandas."
        ]]

lesson18 :: Test
lesson18 = TestList 
    [TestLabel "Lesson #18 http://tokipona.net/tp/janpije/okamasona18.php" $ TestList [
            check2 "o kama sona e toki pona! " "lernu tokiponon!"
            , check2 "tenpo pini la jan ali li alasa e soweli e kili." "estintece cxiu cxasas/kolektas beston kaj frukton."
            , check2 "jan alasa pona li wawa li sona pona e ma ona li pona tawa meli mute." "bona cxasisto/kolektisto fortas kaj scias bone lian landon kaj bonas multajn inojn."
            , check2 "mi pali lon insa esun suli" "mi laboras ekziste en granda vendejo"
            , check2 "mi pali lon esun suli" "mi laboras ekziste vendeje grande"
            , check2 "jan ike utala pi nasin sewi Silami li moli e jan mute lon esun Kenja." "batala malamiko de islama religio mortigas multajn personojn vendeje kenje."
            , check2 "jan utala li moku e moku namako" "batalisto mangxas spican mangxon"
            , check2 "mi pali e pan seli tawa sina" "mi faras varman grajnon al vi"
            , check2 "mi wile pu lon tomo telo" "mi volas interagi kun la oficiala libro de Tokipona ekziste bancxambre"
            , check2 "pan ni li ike moku. o pana e namako tawa mi." "tio grajno malbonas mangxe. donu spicon al mi."
            , check2 "mi pu lon supa moku la lipu kama jaki." "dum/se mi interagas kun la oficiala libro de Tokipona ekziste mangxotable, malpura venanta papero." --google :(
            , check2 "mi wile toki tawa meli Kolonpija namako" "mi volas alparoli spican kolombian inon"
            , check2 "o toki e ni tawa meli lon esun pi telo nasa: ali li pona. o kama tawa tomo" "diru tion al ino en vendejo de alkoholo: cxio bonas. venu domon"
            , check2 "soweli li pakala e jan alasa Mewika" "besto detruas usonan cxasisto/kolektiston"
            , check2 "jan alasa li wile ala e pan tan esun." "cxasisto/kolektisto volas ne grajnon tial vendejan." -- google :(
            , check2 "meli Palata pi unpa lukin li sona namako e moku... e mi kin." "hinduja ino de vida sekso scias spice mangxon... min ja."
            , check2 "tenpo ni la mi ken ala pu. mi wile e pan li wile tawa esun." "nun mi povas ne interagi kun la oficiala libro de Tokipona. mi volas grajnon kaj volas iri vendejon."
        ]]


main :: IO Counts
main = runTestTT $ TestList [
    lesson3 
    , lesson4
    , lesson5 
    , lesson6 
    , lesson7 
    , lesson8 
    , lesson9 
    , lesson2 
    , lesson10 
    , lesson11
    , lesson12
    , lesson13
    , lesson14
    , lesson15
    , lesson16
    , lesson17
    , lesson18
    , check2 "ala! sina sona e nimi ali! pini a! sina sona pona e toki pona. toki pona li pona ala pona tawa sina? mi wile e ni: ona li pona tawa sina. sina wile pali e seme kepeken sona sin sina? o kama tawa kulupu \"Facebook\" pi toki pona. o lukin e lipu ante mi anu lipu pi jan ante. o pali e lipu sin! o pana e sona pi toki pona tawa jan ante. tenpo kama la sina ken tawa kulupu pona pi toki pona. sina kama tawa ma \"Georgia\" lon ma Mewika la o pana e lipu tawa mi tan ni: mi wile lukin e sina! mi lon ma tomo \"Macon\"." 
    "nenio! vi scias cxion nomojn! fino ah! vi scias bone tokiponon. cxu tokipono bonas vin? mi volas tion: li bonas vin. vi volas fari kion per via alia scio? venu grupon \"Facebook\" de tokipono. vidu mian alian paperon aux paperon de alia persono. faru alian paperon! donu scion de tokipono al alia persono. estontece vi povas iri bonan grupon de tokipono. dum/se vi ekigas/ekvenas landon \"Georgia\" en Usono, donu paperon al mi venas el tio: mi volas vidi vin! mi estas urbe \"Macon\"."
    ]
