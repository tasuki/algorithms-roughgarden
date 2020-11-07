import A2Inversions

main :: IO ()
main = do
    inversionStr <- readFile("resources/A2-inversions.txt")
    let inversionLst = map (read :: String -> Int) $ words inversionStr
    let (invs, _) = inversions inversionLst
    putStrLn $ show $ invs
