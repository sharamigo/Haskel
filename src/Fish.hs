-- An dieser Stelle wird festgelegt, welche Methoden nach außen hin sichtbar sein sollen
-- und somit verwendet werden können
module Fish (Fish(..), generateFish, checkifBigFish) where

import System.Random
import Weather

-- Record Syntax zur Erstellung von eigenen Datentypen
data Fish = Fish {
    fishName :: String,
    fishLength :: Int,
    fishWeight :: Int
} deriving (Eq)

-- Thema-ABSTRAKTION
-- Wir erstellen uns eine Instanz vom Datatype 'Fish' und implementieren die Typeclass 'Show'
-- und überschreiben dann die 'show-Methode' (ähnlich zu to-String Methode in z.B. Java, 
-- Typeclasses sind vergleichbar mit Interfaces in Java)
instance Show Fish where
    show fish = fishName fish ++ " mit einem Gewicht von " ++ show (fishWeight fish) ++ " g und einer Länge von " ++ show (fishLength fish) ++ " cm" 

-- Enum von FishTypes
data FishType = Barbe | Brasse | Barsch | Bachforelle | Hasel | Hecht | Karpfen | Regenbogenforelle | Rotauge | Rotfeder | 
    Dorsch | Makrele | Hering | Scholle | Lachs | Thunfisch | Seezunge | Steinbutt | Tintenfisch | Kabeljau deriving (Show)

lakeFish = [Barbe, Brasse, Barsch, Bachforelle, Hasel, Hecht, Karpfen, Regenbogenforelle, Rotauge, Rotfeder]
seaFish = [Dorsch, Makrele, Hering, Scholle, Lachs, Thunfisch, Seezunge, Steinbutt, Tintenfisch, Kabeljau]


generateRandomWeight :: IO Int
generateRandomWeight = do
    gen <- newStdGen
    return (head (randomRs (500,10000) gen) :: Int)

generateRandomLength :: IO Int
generateRandomLength = do
    gen <- newStdGen
    return (head (randomRs (3,100) gen) :: Int)


-- Ein zufälliger FischType wird je nach Wettersituation (und damit Fishing-Spot) aus den
-- oben definierten Listen ermittelt
generateRandomFishName :: WeatherConditions -> IO FishType
generateRandomFishName checkedWeather = do
    gen <- newStdGen
    if checkedWeather == Sonnig || checkedWeather == Wolkig then do
        let number = (head (randomRs (0,length seaFish - 1) gen) :: Int)
        return $seaFish!!number
    else do
        let number = (head (randomRs (0,length lakeFish - 1) gen) :: Int)
        return $lakeFish!!number

-- Rückgabe des erstellten Fisches
generateFish :: WeatherConditions -> IO Fish 
generateFish checkedWeather = do
        fishName <- generateRandomFishName checkedWeather
        let fishNameString = show fishName
        weight <- generateRandomWeight
        length <- generateRandomLength
        return Fish {fishName=fishNameString, fishLength=length, fishWeight=weight}  

-- Prüfen, ob ein großer Fisch gefangen wurde oder nicht
checkifBigFish :: Int -> Bool
checkifBigFish weight
    | weight >= 5000 = True
    | otherwise = False