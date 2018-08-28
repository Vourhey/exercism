module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = 
    case planet of
        Mercury -> earth / 0.2408467
        Venus -> earth / 0.61519726
        Earth -> earth
        Mars -> earth / 1.8808158
        Jupiter -> earth / 11.862615
        Saturn -> earth / 29.447498 
        Uranus -> earth / 84.016846
        Neptune -> earth / 164.79132
    where
        earth = seconds / 31557600
