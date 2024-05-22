sumaAlicuota :: Int -> Int
sumaAlicuota n = sum [x | x <- [1..n-1], n `mod` x == 0]

clasificarNumero :: Int -> String
clasificarNumero n
  | suma == n = "Perfect"
  | suma > n  = "Abundant"
  | otherwise = "Deficient"
  where suma = sumaAlicuota n

categoriaPrograma :: Int -> String
categoriaPrograma n
  | clasificacion == "Perfect"   = "Engineering"
  | clasificacion == "Abundant"  = "Administrative"
  | clasificacion == "Deficient" = "Humanities"
  where clasificacion = clasificarNumero n

analizarCodigo :: Int -> String
analizarCodigo codigo = periodo ++ " " ++ categoria ++ " num" ++ orden ++ " " ++ paridad
  where
    periodo   = "20" ++ show (codigo `div` 1000000) ++ "-" ++ show ((codigo `div` 100000) `mod` 10)
    categoria = categoriaPrograma ((codigo `div` 1000) `mod` 100)
    orden     = show (codigo `mod` 1000)
    paridad   = if codigo `mod` 2 == 0 then "even" else "odd"

main :: IO ()
main = interact $ \input ->
    let codigo = read input :: Int
    in analizarCodigo codigo ++ "\n"
