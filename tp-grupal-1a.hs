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
procesarArchivo xs y = if esRama xs y then insertarRama xs y else generarArbolNuevo xs y


-- es rama de alguno de los arboles?
esRama :: [String] -> DataSet -> Bool
esRama xs [] = False
esRama xs (y:ys) = if esrama2 xs y then True else False

-- es rama de ese arbol en particular
esRama2 :: [String] -> ArbolNivel -> Bool
esRama2 (x:[]) (c,n) = sonIguales (split2 x) (c,n) -- si son iguales que retorne true si no que retorne false (?)
esRama2 (x:xs) (c,n) = True -- Suponemos que llegamos a la hoja del arbol pero la lista de string es una rama mas larga entonces es verdadero igual
esRama2 (x:xs) (Nodo c (y:ys))= if notContains x "-" then (if x == c then esRama2 xs ys else False) else True -- Si es hoja el elemento del string significa que llegue al final de la rama y es una rama del arbol mas chica, por ende verdadero.

-- notContains deberia verificar que x no tiene el - que indicaria que seria la hoja

-- Es igual la hoja del string que la hoja del arbol?
sonIguales :: [String] -> Hoja -> Bool
sonIguales (x:[]) (c,n) = if x == n then True else False
sonIguales (x:xs) (c,n) = if x == c then sonIguales xs (c,n) else False

--busco el arbol al que pertenece la rama
buscarArbol :: [String] -> DataSet -> ArbolNivel 
buscarArbol xs (y:ys) = if esRama2 xs y then y else buscarArbol xs ys

--inserto la rama nueva al arbol pre-existente
insertarRama:: [String] -> DataSet -> DataSet -- esta ok?
insertarRama xs (y:ys) = if esRama2 xs y then ((insertarRama2 xs y): ys) else (y:insertarRama xs ys)

insertarRama2 :: [String] -> ArbolNivel -> ArbolNivel -- esta ok?
insertarRama2 (x:xs) (Nodo c (y:ys)) = if notContains x "-" then (if x == c then (if nodoDe y == head(xs) then Nodo c (insertarRama2 xs (y:ys)) else Nodo c (generarArbol xs :(y:ys)))) 

-- armo arbol ya sea para ser una rama de un arbol o para armar arbol nuevo
generarArbol :: [String] -> ArbolNivel -- no se si estara correcto... habrá que probar
generarArbol (x:[]) = (armarHoja (split2 x): [])
generarArbol (x:xs) = Nodo x (generarArbol xs: []) 

-- Armo la hoja para la rama.
armarHoja :: [String] -> ArbolNivel
armarHoja (x:y) = (x,head(y))

--Insserto el nuevo armo a la lista de arboles
generarArbolNuevo :: [String] -> DataSet -> DataSet
generarArbolNuevo (xs) (ys) = (generarArbol xs : ys)

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

split2 :: String -> [String] -- EN TEORIA esto convierte un string en una lista de string, Ejemplo: cadena "la luna,el sol,a,b,c", se convierte en ["la luna","el sol","a","b","c"]
split "" = []
split xs = ys : (split . drop 1) zs
   where (ys, zs) = span (/='-') xs -- Convierto la hoja qeu estaba en string en una lista de string para manipular los caracteres

--span, aplicado a un predicado p y una lista xs, devuelve una tupla donde el primer elemento es el prefijo más largo (posiblemente vacío) de xs de elementos que satisfacen py el segundo elemento es el resto de la lista

-- main :: IO ()
-- main = do
--  hPutStr stderr "Type a filename: "
--  filename <- getLine
--  handle <- openFile filename ReadMode
--  dumpFile handle filename 1
--  hClose handle
