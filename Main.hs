import JSONParser (parseJson, parseFile, foldrJson, parseJsonM, parseFileM, JSON(..))

sumJSON :: JSON -> Int
sumJSON = foldrJson (const 0) id (const 0) (sum . map snd) sum

allStrings :: JSON -> [String]
allStrings = foldrJson (:[]) (const []) (const []) (concatMap snd) concat

depth :: JSON -> Int
depth = foldrJson (const 0) (const 0) (const 0) (\xs -> 1 + maximum (map snd xs)) (\xs -> 1 + maximum xs)

main :: IO ()
main = do
    jsonConObjetosAnidados <- return "{\"prop1\":\"val prop 1\",\"prop 2\":{\"inner object prop\":\"inner object prop val\"}}"
    json <- parseJson jsonConObjetosAnidados
    print json

    jsonTrue <- parseJson "true"
    jsonFalse <- parseJson "false"

    -- parseJsonM :: String -> IO (Maybe JSON) 
    jsonBoolFail <- parseJsonM "Esto no es un bool"

    print jsonBoolFail
    print jsonTrue
    print jsonFalse

    jsonConObjetosBooleanosYNumeros <- return " { \"           hola\"   :     \"chau\" ,\"que\"  :  {   \"false\": false ,\"true\" : false, \"soyUnNumero\" : 123  }  }"
    json <- parseJson jsonConObjetosBooleanosYNumeros
    print json

    jsonConLista <- return "{  \"soyUnaL   :istaDeInt\"       : [ 1 , 2,3,4    ,  {   \"soy\"  :  \"  unObj {esto es un json}eto  \"  }  ,  5,  \"hola\"] }"
    json <- parseJson jsonConLista
    print ("La suma del json es: " ++ show (sumJSON json))
    print ("Todas las strings del json son: " ++ show (allStrings json))
    print ("La profundidad del json es: " ++ show (depth json))
    print json

parseFileCLI :: IO ()
parseFileCLI = do
    putStrLn "Ingrese el nombre del archivo a parsear"
    fileName <- getLine
    json <- parseFile fileName
    print json

allPropWithKey :: String -> JSON -> [JSON]
allPropWithKey k (JSONBool b) = []
allPropWithKey k (JSONNumber n) = []
allPropWithKey k (JSONString s) = []
allPropWithKey k (JSONObject ps) = concatMap (\(k',v) -> if k == k' then [v] else allPropWithKey k v) ps
allPropWithKey k (JSONList xs) = concatMap (allPropWithKey k) xs

getString :: JSON -> String
getString (JSONString s) = s
getString _ = error "No es un string"

todosLosNombresDePokemones :: IO ()
todosLosNombresDePokemones = do
    json <- parseFile "folder/pokemones.json"
    print (map getString $ allPropWithKey "nombre" json)