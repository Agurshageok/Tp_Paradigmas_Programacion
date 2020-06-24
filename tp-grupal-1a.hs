import System.IO
import System.Directory
import Control.Monad

main :: IO ()
main = do
    putStrLn "Ingresar ruta de archivo de entrada"
    fileInName <- getLine
    fileExists <- doesFileExist fileInName
    if fileExists
        then do
            contents <- readFile fileInName
            listaDataSet <- procesarArchivo contents
            putStrLn "Ingresar cantidad de espacios de indentación"
            cantEspacios <- getLine
            putStrLn "Ingrese ruta de archivo de salida"
            fileOutName <- getLine
            --calcular totales
            --escribir archivo
        else do 
            putStrLn "No existe archivo"
    putStrLn "Desea realizar otro procesamiento S/N"
    continue <- getChar
    when (continue == 'S') $ do
        main

--Validar: 
-- Formato de filas iguales
-- Sin filas repetidas
-- Orde Lexicográfico
procesarArchivo :: String -> DataSet
procesarArchivo x = if lista = [] then map (validarLinea . cargarArbol) lista else [] where lista = (lines x)

validarLinea :: [String] -> (Maybe [String])
validarLinea = undefined

cargarArbol :: Maybe [String] -> ArbolNivel
cargarArbol = undefined

type Hoja = (Char,Int) 
data ArbolNivel = Nodo Int [ArbolNivel] | Hoja
type DataSet = [ArbolNivel]


