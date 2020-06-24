import System.IO
import System.Directory
import Control.Monad

main :: IO ()
main = do
    hPutStr stderr "Ingresar ruta de archivo de entrada"
    fileInName <- getLine
    fileExists <- doesFileExist fileInName
    if fileExists
        then do
           -- contents <- readFile fileInName
            handle <- openFile fileInName ReadMode
            listaFinal <- dumpFile handle filename 1 []
            hClose handle
            --listaDataSet <- procesarArchivo contents
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
procesarArchivo :: [String] -> DataSet -> DataSet
procesarArchivo [] y = []
procesarArchivo (x:xs) y = -- Si x = "char - numero" entonces Hoja else Nodo x (procesarArchivo xs y)

type Hoja = (Char,Int) 
data ArbolNivel = Nodo Char [ArbolNivel] | Hoja
type DataSet = [ArbolNivel]


dumpFile :: Handle -> FilePath -> Integer -> DataSet -> DataSet
dumpFile handle filename lineNumber listaArbol = do -- show file contents line by line
    end <- hIsEOF handle --Verifica si es el final del archivo
    when ( not end ) $ do
        line <- hGetLine handle --Obtengo una linea del archivo
        listaArbol <- procesarArchivo (split line) listaArbol 
        -- putStrLn $ filename ++ ":" ++ show lineNumber ++ ": " ++ line --Nombrearchivo : muestro linea de archivo : muestro contenido de linea
        dumpFile handle filename $ lineNumber + 1 listaArbol -- vuelvo a llamar a dumpFile

split :: String -> [String] -- EN TEORIA esto convierte un string en una lista de string, Ejemplo: cadena "la luna,el sol,a,b,c", se convierte en ["la luna","el sol","a","b","c"]
split "" = []
split xs = ys : (split . drop 1) zs
   where (ys, zs) = span (/=',') xs

-- main :: IO ()
-- main = do
--  hPutStr stderr "Type a filename: "
--  filename <- getLine
--  handle <- openFile filename ReadMode
--  dumpFile handle filename 1
--  hClose handle
