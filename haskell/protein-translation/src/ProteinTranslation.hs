module ProteinTranslation(proteins) where

import Data.Text (pack, unpack, chunksOf)
import qualified Data.Map as M
import Data.Maybe (isJust)

codons :: M.Map String String 
codons = M.fromList [ ("AUG", "Methionine")
                    , ("UUU", "Phenylalanine")
                    , ("UUC", "Phenylalanine")
                    , ("UUA", "Leucine")
                    , ("UUG", "Leucine")
                    , ("UCU", "Serine")
                    , ("UCC", "Serine")
                    , ("UCA", "Serine")
                    , ("UCG", "Serine")
                    , ("UAU", "Tyrosine")
                    , ("UAC", "Tyrosine")
                    , ("UGU", "Cysteine")
                    , ("UGC", "Cysteine")
                    , ("UGG", "Tryptophan")
                    ]

translate :: String -> Maybe String
translate "UAA" = Nothing
translate "UAG" = Nothing
translate "UGA" = Nothing
translate codon = M.lookup codon codons

proteins :: String -> Maybe [String]
proteins s = sequence $ takeWhile isJust translated
    where chunks = chunksOf 3 $ pack s
          translated = map (translate . unpack) chunks
