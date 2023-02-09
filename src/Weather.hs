module Weather where

import System.Random

-- record-Syntax Weather-Object (zwei Attribute: WeatherConditions, Int)
data Weather = Weather {
    main :: WeatherConditions,     
    temperature :: Int
} deriving (Show, Eq)


data WeatherConditions = Sonnig | Wolkig | Neblig | Regnerisch | Gewitter | Schneefall | Hagel deriving (Show, Eq)
weather = [Sonnig,Wolkig,Neblig,Regnerisch,Gewitter,Schneefall,Hagel]


-- Die Funktion generiert eine random-Temperatur in Abhängigkeit der Wetterbedingung
-- damit eine gewisse Logik implementiert ist und nicht Schneefall und 35 Grad erscheinen
generateRandomTemp :: IO Int
generateRandomTemp = do 
    condition <- generateRandomWeatherCondition
    gen <- newStdGen   
    if condition == Sonnig then return (head (randomRs (-20,40) gen) :: Int)
    else if condition == Wolkig then return (head (randomRs (-5,20) gen) :: Int)
    else if condition == Neblig then return (head (randomRs (0,20) gen) :: Int)
    else if condition == Regnerisch then  return (head (randomRs (5,30) gen) :: Int)
    else if condition == Gewitter then return (head (randomRs (20,40) gen) :: Int)
    else if condition == Schneefall then return (head (randomRs (-20,0) gen) :: Int)
    else return (head (randomRs (1,30) gen) :: Int)
        

-- Hier wird eine zufällige Wetterbedingung generiert. Dabei wird aus der Liste 'weather'
-- ein Eintrag mit head randomRs(0, length List weather - 1) genommen und zurückgegeben
-- Rückgabewert ist vom Typ WeatherConditions
generateRandomWeatherCondition :: IO WeatherConditions
generateRandomWeatherCondition = do 
    gen <- newStdGen
    let number = head (randomRs (0,length weather - 1) gen) :: Int
    return $weather!!number

-- generiert ein Objekt vom Typ 'Weather'. Dabei werden die Funktionen 'generateRandomWeatherCondition'
-- und 'generateRandomTemp' aufgerufen um die Attribute mit Zufallswerten zu belegen
generateWeather :: IO Weather
generateWeather = do
    wname <- generateRandomWeatherCondition
    --let weatherCondString = show wname
    temperature <- generateRandomTemp    
    return Weather {main=wname, temperature=temperature}
    