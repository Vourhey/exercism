module Beer (song) where

returnRow :: String -> String -> String
returnRow "1" acc = acc ++ "1 bottle of beer on the wall, 1 bottle of beer.\n\
              \Take it down and pass it around, no more bottles of beer on the wall.\n\
              \\n\
              \No more bottles of beer on the wall, no more bottles of beer.\n\
              \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
returnRow "2" acc = acc ++ "2 bottles of beer on the wall, 2 bottles of beer.\n\
              \Take one down and pass it around, 1 bottle of beer on the wall.\n\
              \\n"
returnRow n acc = acc ++ n ++ " bottles of beer on the wall, " ++ n ++ " bottles of beer.\n\
              \Take one down and pass it around, " ++ prev ++ " bottles of beer on the wall.\n\
              \\n"
              where prev = show $ (read n :: Int) - 1

song :: String
song = foldr (returnRow) "" (map show [1, 2..99])
