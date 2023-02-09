module Game (startGame, startFishing) where    

import Control.Concurrent
import System.Random
import Data.Typeable

import Text.Read
import Data.Maybe
import Angler
import Weather
import System.Exit
import Fish

angeln = "Angeln"
ready = "R"
los = "Los!"


-- Start-Funktion des Projekts (Begrüßung)
startGame = do
    putStrLn "Herzlich Willkommen zur Angelsimulation!"
    putStrLn $"Tippe '" ++ angeln ++ "' ein, wenn du bereit bist zu angeln."
    input <- getLine
    checkForValidAngelnInput input

checkWeather :: IO WeatherConditions
checkWeather = do
    weather <- generateWeather
    putStrLn $"Das Wetter: " ++ show(main weather) ++ " und eine Temperatur von " ++ show(temperature weather) ++ " Grad Celsius."        
    showBestFishingSpot $main weather
    let location = setLocation (main weather)
    if main weather == Hagel
        then exitSuccess  -- Programm an dieser Stelle beenden    
    else print $" gehen wir " ++ location ++ " angeln ..."
    return (main weather)

-- Charakter mit Namen und Alter angeben
createAngler = do
    putStrLn "Alles klar, wir wüssten natürlich gerne noch mit wem wir es heute zu tun haben!"
    putStrLn "Bitte gib zunächst deinen Namen ein: "
    anglerName <- getLine
    putStrLn $"Okay, du bist also " ++ anglerName ++ ". Es freut uns dich kennenzulernen!"
    putStrLn "Bitte gib jetzt dein Alter ein: "
    anglerAgeTest <- getLine
    anglerAge <- checkForValidAge anglerAgeTest
    let angler = Angler {name = anglerName, age = anglerAge}
    putStrLn $"Vielen Dank! Du heisst also " ++ name angler ++ " und bist " ++ show(age angler) ++ " Jahre alt."
    putStrLn $"Wenn du loslegen möchtest tippe '" ++ los ++ "' ein."
    input <- getLine
    checkForValidLosInput input


-- Eigentliche GAME-LOOP, Programm läuft prinzipiell nur hier, wenn Begrüßung, Charakter anlegen
-- und Auswahl des Fishing-Spots anhand des Wetter abgerufen wurde
startFishing :: [Fish] -> WeatherConditions -> IO()
startFishing fishBag checkedWeather = do
    putStrLn "Alles klar, Angel wird ausgeworfen..."
    -- Zufällige Zeit bis zu einem Biss wird generiert
    randomDelay <- generateRandomDelay
    print randomDelay
    threadDelay randomDelay

    generatedFish <- generateFish checkedWeather
    let weight = fishWeight generatedFish
    let isBig = checkifBigFish weight
    -- Ausgabe, ob ein großer oder kleiner Fisch angebissen hat
    printSuccessBasedOnWeight isBig generatedFish
    putStrLn $"Möchtest du die/den " ++ show(fishName generatedFish) ++ " in die Tasche stecken? (Ja/Nein)"
    input <- getLine
    -- Tasche in Form von Liste wird erstellt und der Fisch nach Wunsch reingepackt, 
    -- ansonsten wird er freigelassen (zu beachten ist, dass Tasche bei jedem Angel-Durchlauf
    -- neu erstellt wird, weil Variablen in Haskell immutable sind)
    newFishBag <- checkPutInFishBag input generatedFish fishBag
    if fishBag == newFishBag then putStrLn "Fisch wurde freigelassen" else putStrLn "Fisch wurde in Tasche gepackt"
    -- Anzeigen der Fische in der Tasche
    putStrLn $"Deine Tasche: " ++ show newFishBag
    -- Rekursions-Beispiel
    -- Möchte man weiter angeln, wird die Funktion rekursiv aufgerufen und die Tasche übergeben, damit 
    putStrLn $"Du hast noch nicht genug? Wirf die Angeln nochmal aus, indem du '" ++ ready ++ "' eingibst."    
    input <- getLine
    checkForValidReadyInput input newFishBag checkedWeather


printSuccessBasedOnWeight :: Bool -> Fish -> IO()
printSuccessBasedOnWeight isBig generatedFish
    | isBig = putStrLn $"...Nanu, es hat etwas GROßES angebissen! Es ist ein/e " ++ show(fishName generatedFish) ++ " mit einem Gewicht von " ++ show(fishWeight generatedFish) ++ " g und einer Länge von " ++ show(fishLength generatedFish) ++ " cm."
    | otherwise = putStrLn $"...Nanu, es hat nur etwas Kleines angebissen! Es ist ein/e " ++ show(fishName generatedFish) ++ " mit einem Gewicht von " ++ show(fishWeight generatedFish) ++ " g und einer Länge von " ++ show(fishLength generatedFish) ++ " cm."    


-- Prüfen, ob Fisch der Tasche hinzugefügt werden soll oder nicht
checkPutInFishBag :: String -> Fish -> [Fish] -> IO [Fish]
checkPutInFishBag input generatedFish fishBag
    -- ':' ist cons-Operator und fügt Element am Anfang einer Liste ein
    | input == "Ja" = return $generatedFish : fishBag
    | input == "Nein" = return fishBag
    | otherwise = do
        putStrLn "Gib Ja oder Nein ein!"
        input <- getLine
        checkPutInFishBag input generatedFish fishBag


-- ValidierungsMethode für Text-Input
checkForValidAngelnInput :: String -> IO()
checkForValidAngelnInput "Angeln" = createAngler

checkForValidAngelnInput x = do
    putStrLn $"Du musst '" ++ angeln ++ "' eingeben!"
    input <- getLine
    checkForValidAngelnInput input

-- ValidierungsMethode für Text-Input
-- Auslösen der eigentlichen GAME-LOOP (start-fishing-Funktion)
checkForValidLosInput :: String -> IO()
checkForValidLosInput "Los!" = do
    checkedWeather <- checkWeather
    print "Bist du bereit die Angel auszuwerfen? Dann tippe 'Yes' ein"
    input <- getLine
    continueWithFishing input
    startFishing [] checkedWeather
checkForValidLosInput x = do
    putStrLn $"Du musst '" ++ los ++ "' eingeben!"
    input <- getLine
    checkForValidLosInput input  

-- ValidierungsMethode für Text-Input
-- Pattern-Matching Beispiel    
continueWithFishing :: String -> IO()
continueWithFishing "Yes" = return ()
continueWithFishing x = do
    print "Lass dir Zeit, wenn du bereit bist, tippe 'Yes' ein"
    input <- getLine
    continueWithFishing input


-- ValidierungsMethode für Text-Input, um weiter zu angeln
checkForValidReadyInput :: String -> [Fish] -> WeatherConditions -> IO()
checkForValidReadyInput input fishBag checkedWeather
    | input == ready = startFishing fishBag checkedWeather
    | otherwise = do
        putStrLn $"Du musst '" ++ ready ++ "' eingeben!"
        input <- getLine
        checkForValidReadyInput input fishBag checkedWeather    


-- ValidierungsMethode für Text-Input des Alters
checkForValidAge :: String -> IO Int
checkForValidAge anglerAge
    -- isJust prüft, ob der übergebene Wert der Form Just_ entspricht, wobei readMaybe ein Just_ oder ein Nothing zurückgibt, je nachdem ob der String als Int gelesen werden kann
    -- 'return' wandelt Int in IO Int um + 'read' castet String in Int (weil vorher mit Maybe bereits überprüft)
    -- -> HighOrder-Function weil einer Funktion als Parameter eine Funktion übergeben wird
    | isJust (readMaybe anglerAge :: Maybe Int) = return (read anglerAge :: Int)
    | otherwise = do
        putStrLn "Butter bei die Fische, gib ein valides Alter ein..."
        input <- getLine
        checkForValidAge input


-- Gibt eine Ausgabe in Abhängigkeit der WeatherCondition (Eingabeparameter) mit putStrLn zurück: Rückgabetyp IO().
-- Es wird die beste Angler-Location als String ausgegeben. Die Funktion wird in 'checkWeather' aufgerufen.
-- Hier werden Guards zur Unterscheidung verwendet.
showBestFishingSpot :: WeatherConditions -> IO()
showBestFishingSpot weatherCondString
    | weatherCondString == Sonnig = putStrLn "Super! Es ist sonnig, wir gehen ans Meer zum Fischen!"
    | weatherCondString == Wolkig = putStrLn "Das Wetter ist okay, lass uns zu einen Angelversuch am Meer starten!"
    | weatherCondString == Neblig = putStrLn "Es ist neblig. Du könntest dich bei unbekannten Gewässern verirren! Gehen wir lieber an den Rhein!"
    | weatherCondString == Regnerisch =  putStrLn "Es regnet! Wir fahren besser an den nächstgelegenen See!"
    | weatherCondString == Gewitter = putStrLn "Pfui! Es sind starke Gewitter unterwegs. Wir gehen lieber zum Baggersee"
    | weatherCondString == Schneefall = putStrLn "Es schneit! Eisfischen am See ist auch ganz cool!"
    | weatherCondString == Hagel = putStrLn "Willst du wirklich Fischen gehen bei Hagel? Lass es besser!"

-- setzt die Angler-Location abhängig von der WeatherCondition, die im Spiel in der Funktion 'checkWeather' ausgegeben wird. 
-- Verwendet Pattern-matching und hat als Eingabeparameter WeatherConditions
setLocation :: WeatherConditions -> String
setLocation Sonnig = "ans Meer"  
setLocation Wolkig = "ans Meer"  
setLocation Neblig = "zum Fluss" 
setLocation Regnerisch = "zum See" 
setLocation Gewitter = "zum Baggersee"
setLocation Schneefall = "zum vereisten See"
setLocation Hagel = ""

-- Generierung eines zufälligen Werts bis zu einem Biss
-- Beispiel für High-Order-Function + Lazy-Aspekt, weil randomRS normalerweise eine unendliche Liste von Zahlen zurückgibt, aber durch head weiß Haskell
-- dass nur eine Zahl benötigt wird (mehr wird nicht gemacht, weil Haskell zu faul ist und es nicht als nötig ansieht)

generateRandomDelay :: IO Int
generateRandomDelay = do
    gen <- newStdGen
    return (head (randomRs (1000000,10000000) gen) :: Int)
