import System.Console.ANSI
import System.Console.Terminal.Size
import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char
import Data.Time
import Data.Maybe
import qualified Data.Map as Map

{-- ##### STRUCURES DE DONNÉE ##### --}
-- |Entrée de la timeline
-- Day pour stocker la date limite de la tâche
-- String pour stocker la description de la tâche
type TimeEntry          = (Day,String)
-- |Entrée avec sa mise en page
type FormattedTimeEntry = (Day,String,Maybe (ColorIntensity,Color))

{-- ##### VARIABLES ##### --}
-- |Nom d'utilisateur       --  !!!!!!!!!!!!!!!!!!!!
userName :: String          --      A MODIFIER
userName = "qwann"          --  !!!!!!!!!!!!!!!!!!!!
-- |Emplacement des fichiers
filePrefix::String
filePrefix  = "/home/"++userName++"/.todo/"
-- |Extension des fichiers
fileSuffix::String
fileSuffix  = ".txt"
-- |Emplacement du fichier de la timeline
fileTime::String
fileTime    = "/home/"++userName++"/.todo/"++nameTime++".txt"
-- |Nom du fichier de la timeline
nameTime::String
nameTime    = "timeline"
-- | Machin truc en la date et la tache
prefixTimeTask::String
prefixTimeTask = " :: "
-- |ColorIntensity du texte normal
int_norm::ColorIntensity
int_norm = Dull
-- |Color du texte normal
col_norm::Color
col_norm = Blue

{-- ##### MAIN ##### --}
-- |Permet de répartir selon l'argument reçu par la fonction
dispatch::[(String, [String] -> IO ())]
dispatch =  [ ("help", help)
    , ("add", add)
    , ("view", view)
    , ("remove", remove)
    , ("rm",remove)
    , ("RM", remove_list)
    , ("REMOVE", remove_list)
    , ("addTime", addTime)
    , ("viewTime", viewTime)
    , ("sort", sortTime)
    , ("list", list)
    ]

-- |Fonction principale
main::IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

-- |Fournit une documentation sur l'utilisation de 'todo'
help::[String] -> IO ()
help _ = let max = 10 in do
    afficheHelp "help"      []              
                "Affiche ce message" 
                max
    afficheHelp "add"       ["name","text"] 
                "Ajoute tache <text> à la liste <name>" 
                max
    afficheHelp "view"      ["name"] 
                "Affiche la liste <name>" 
                max
    afficheHelp "list"      [] 
                "Affiche la liste des todo-listes" 
                max
    afficheHelp "remove/rm" ["name","number"] 
                "retire l'élément <number> dans la liste <name>" 
                max
    afficheHelp "REMOVE/RM" ["name"]
                "supprime complétement la liste <name>"
                max
    afficheHelp "viewTime"  [] 
                "Affiche la timeline" 
                max
    afficheHelp "addTime"   ["date","text"] 
                "Ajoute une tache <texte> à la timeline, à la date <date> ou éventuellement à today, tomorow ou demain" 
                max
    afficheHelp "sort"      [] 
                "Trie les éléments de la timeline" 
                max

-- |Affiche la liste des todo listes
-- Fonction en traveaux
list::[String] -> IO ()
list [] = do
    liste <- getDirectoryContents filePrefix
    if null liste
        then ecrire "Aucune todo-liste à afficher, gros teubé.\n" (Dull, Red)
        else do
            ecrire "Voici la liste des todo-listes :\n" (Dull, Yellow)
            print_list_todo $ nettoie liste
    where 
        -- | retire les siffuxes et "." et ".."
        nettoie::[String] -> [String]
        nettoie xs
            | null xs           = []
            | head xs == "."    = nettoie (tail xs) -- on retire .
            | head xs == ".."   = nettoie (tail xs) -- on retire ..
            | fileSuffix `isSuffixOf` (head xs)     -- on retire l'extension   
                = (take ((length (head xs)) - length fileSuffix) (head xs)):(nettoie (tail xs))

-- |Ajoute l'élément <todoItem> à la todo liste <name>
add::[String] -> IO ()
add [name, todoItem] = do
    if all (\c -> isPunctuation c || isSeparator c) todoItem
       then return ()   --on fait rien si c'est pas intéressant
       else appendFile fileName (todoItem ++ "\n")
    where fileName = nomFichier name

-- |Ajoute l'élément <todoItem> à la timeline pour la date <date>
addTime::[String] -> IO ()

addTime ["today", todoItem] = do
    now <- utctDay <$> getCurrentTime
    addTime [dayToString now, todoItem]

addTime ["tomorrow", todoItem] = do
    now <- utctDay <$> getCurrentTime
    addTime [dayToString (addDays 1 now),todoItem]

addTime ["demain", todoItem] = do addTime ["tomorrow", todoItem]
    
addTime [date,todoItem] = do
    appendFile fileTime (date ++ " " ++ todoItem ++ "\n")

-- |Affiche la time ligne, c'est zoli :)
viewTime::[String] -> IO ()
viewTime [] = do
    now <- utctDay <$> getCurrentTime
    todoTasks <- lines <$> (readFile fileTime)
    legende
    afficheAll $ taskColor ((taskNum.taskTrie) todoTasks) now

-- |Affiche la todo liste <name>
view::[String] -> IO ()
view [name] = do
    todoTasks <- lines <$> (readFile fileName)
    ecrire "Tâches sur la TODO" (Dull,Yellow)
    ecrire "::" (Vivid,Red)
    ecrire name (Dull,Green)
    ecrire " :\n" (Dull,Yellow)
    ecritListeTache todoTasks 1
    where 
        fileName = nomFichier name
        -- |Ecrit la liste numérotée des taches
        ecritListeTache::[String] -> Int -> IO ()
        ecritListeTache []      _   = return ()
        ecritListeTache (a:q)   n   = do
            ecrire (show n ++ " - ") (Vivid,Red)
            ecrire (a++"\n") (Dull,Yellow)
            ecritListeTache q (n+1)

-- |retire l'item n° <numberString> à la todo liste <name>
remove::[String] -> IO ()
remove [name, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    todoTasks <- lines <$> (hGetContents handle)
    let number = read numberString
        newTodoItems = delete (todoTasks !! (number-1)) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    if null newTodoItems
       then removeFile tempName
       else renameFile tempName fileName
    where fileName = nomFichier name

-- |Supprime la liste <list> en entier
-- Demande à l'utilisateur s'il est sûr
-- Empeche de supprimer "timeline"
remove_list::[String] -> IO ()
remove_list [liste] = do
    list_todo <- nettoie <$> (getDirectoryContents filePrefix)
    if liste == nameTime
       then ecrire "Impossible de supprimer la TimeLine !\n" (Dull, Yellow)
       else if liste `elem` list_todo
            then do
                ecrire "Voulez-vous vraiment supprimer la liste " (Vivid,Blue)
                ecrire liste (Dull,Green)
                ecrire " en entier ? (yes/no)\n" (Vivid,Blue)
                entree <- getLine
                if entree == "yes"
                        then do
                            removeFile (nomFichier liste)
                            ecrire "Liste supprimée.\n" (Dull,Yellow)
                        else ecrire "Liste non supprimée !!\n" (Dull,Yellow)
            else do
                ecrire "La liste " (Dull,Yellow)
                ecrire liste (Dull,Green)
                ecrire " n'existe pas !\n" (Dull,Yellow)

    where 
        -- | retire les siffixes et "." et ".."
        nettoie::[String] -> [String]
        nettoie xs
            | null xs           = []
            | head xs == "."    = nettoie (tail xs)
            | head xs == ".."   = nettoie (tail xs)
            | fileSuffix `isSuffixOf` (head xs)    
                = (take ((length (head xs)) - length fileSuffix) (head xs)):(nettoie (tail xs))


-- |trie la timeline selon l'ordre chronologique
sortTime::[String] -> IO ()
sortTime [] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp_timeline"
    todoTasks <- lines <$> (hGetContents handle)
    let newTodoTasks= writeTask $ taskTrie todoTasks
    hPutStr tempHandle $ unlines newTodoTasks
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
        where fileName = fileTime

{-- ##### FONCTIONS AUXILIAIRES ##### --}
{-- ===== Fonctions d'affichage ===== --}
-- |Affiche une entrée de la fonction help
afficheHelp::String -> [String] -> String -> Int -> IO ()
afficheHelp fonction args desc max = do
    ecrire (spacefillBefore (' ':fonction) max) (Dull,White)
    ecrireColorie args_color 
    putStr "\n"
    ecrire (replicate (max+1) ' ') (Dull, White)
    ecrire_desc $ words desc
    putStr ".\n"
    where 
        args_color :: [(String, (ColorIntensity, Color))]
        args_color = colorie args
        colorList :: [(ColorIntensity, Color)]
        colorList = [(Dull, Green), (Vivid, Red), (Dull, Red), (Dull, Blue)]
        colorie :: [String] -> [(String, (ColorIntensity, Color))]
        colorie []      = []
        colorie liste   = zip liste colorList
        ecrireColorie :: [(String, (ColorIntensity, Color))] -> IO()
        ecrireColorie []            = do
            ecrire "   -" (Vivid, Blue)
        ecrireColorie [(t,(i,c))]   = do
            ecrire (' ':'<':t++">") (i,c)
        ecrireColorie ((t,(i,c)):q) = do
            ecrire (' ':'<':t++">") (i,c)
            ecrireColorie q
        ecrire_desc :: [String] -> IO()
        ecrire_desc []      = do return()
        ecrire_desc [a]     = do
            if length a > 2
                then ecrire a $ extract_col $ lookup (tail $ init a) args_color
                else ecrire a (Vivid, Blue)
        ecrire_desc (a:q)   = do
            if length a > 2
                then ecrire (a++" ") $ extract_col $ lookup (tail $ init a) args_color
                else ecrire (a++" ") (Vivid, Blue)
            ecrire_desc q
        extract_col :: Maybe (ColorIntensity, Color) -> (ColorIntensity, Color)
        extract_col (Just x)    = x
        extract_col Nothing     = (Vivid, Blue)

-- |Affiche la date (pour la timeline)
-- Affiche <day> à l'intensité <int> et la couleur <col>
-- Ne revient pas à la ligne !
afficheDay::Day -> ColorIntensity -> Color -> IO ()
afficheDay day int col = do
    setSGR [SetColor Foreground int col]
    putStr $ dayToString' day
        
-- |Affiche une tache (pour la timeline)
-- Affiche <task> à l'intensité <int> et la couleur <col>
-- Revient à la ligne ! Utiliser afficheTask' pour ne pas avoir de \n
afficheTask::String -> ColorIntensity -> Color -> IO ()
afficheTask task int col     = do
    setSGR [SetColor Foreground int col]
    putStr $ task ++ "\n"

-- |Affiche une tache (pour la timeline)
-- Affiche <task> à l'intensité <int> et la couleur <col>
-- Ne revient à la ligne ! Utiliser afficheTask pour avoir un \n
afficheTask'::String -> ColorIntensity -> Color -> IO ()
afficheTask' task int col    = do
    setSGR [SetColor Foreground int col]
    putStr $ task

-- |Affiche du texte selon l'intensité <int> et la couleur <col>
ecrire::String -> (ColorIntensity,Color) -> IO ()
ecrire texte (int,col) = do
    setSGR [SetColor Foreground int col]
    putStr texte

-- |Ecrit la legende couleur
legende::IO ()
legende = do
    largeur <- (width . fromJust) <$> (hSize stdout)
    let taille  = length " ■ today ■ tomorow ■ week ■ later "
        restant = (largeur - taille)
        moitie  = restant `div` 2
    ecrire (replicate (moitie) '-') (Dull,Blue)
    ecrire " ■" (fromJust (lookup "today" couleur))
    ecrire " today " (Dull,Blue) 
    ecrire "■" (fromJust (lookup "tomorow" couleur))
    ecrire " tomorow " (Dull,Blue) 
    ecrire "■" (fromJust (lookup "week" couleur))
    ecrire " week " (Dull,Blue) 
    ecrire "■" (fromJust (lookup "late" couleur))
    ecrire " later " (Dull,Blue)
    ecrire (replicate (restant - moitie) '-') (Dull,Blue)
    putStr "\n"

-- |Prend la liste des entrées pour les imprimer
-- Les entrées sont les taches, dates, mis en forme
-- Les entrées doivent être triées
-- Affiche sur une colonne si moins (<=) de 5 entrées
-- Affiche sur deux colonnes si plus (>) de 5 entrées
afficheAll::[FormattedTimeEntry] -> IO ()
afficheAll list = let nb_taches = length list
                    in if (nb_taches <= 5)
                          then afficheTout list
                          else let (twisted,max) = twist list
                            in afficheToutTwist twisted (max + 2)
    where
        -- |renvoie la taille de l'entrée la plus grande
        maxTaille::[FormattedTimeEntry] -> Int
        maxTaille list = maximum $ (\(_,t,_) -> length t) <$> list
        -- |Affiche dans le cas d'une petite liste
        afficheTout::[FormattedTimeEntry] -> IO ()
        afficheTout []                      = return ()
        afficheTout ((jour,tache,Just (i,c)):q)  = do
            afficheDay jour int_norm col_norm
            afficheTask (prefixTimeTask++tache) i c
            afficheTout q
        -- |Affiche dans le cas d'une grande liste (sur deux colonnes)
        -- change l'ordre pour pouvoir l'afficher correctement
        afficheToutTwist::[FormattedTimeEntry] -> Int -> IO ()
        afficheToutTwist [] _                           = return ()
        afficheToutTwist [(jour,tache,Just (i,c))] max  = do
            afficheDay jour int_norm col_norm
            afficheTask' (prefixTimeTask++(spacefill tache max)) i c
            ecrire "| \n" (int_norm, col_norm)
        afficheToutTwist ((j1,t1,Just(i1,c1)):(j2,t2,Just(i2,c2)):q) max = do
            afficheDay j1 int_norm col_norm
            afficheTask' (prefixTimeTask++(spacefill t1 max)) i1 c1
            ecrire "| " (int_norm, col_norm)
            afficheDay j2 int_norm col_norm
            afficheTask  (prefixTimeTask++t2) i2 c2
            afficheToutTwist q max
        -- |change l'ordre pour l'afficher sur deux colonnes
        -- Rend aussi la taille de la colonne de droite
        twist::[FormattedTimeEntry] -> ([FormattedTimeEntry],Int)
        twist xs = let (a,b)  = splitAt ((length xs + 1) `div` 2) xs
                    in (concat $ transpose [a,b],maxTaille a) 

twist::[t] -> [t]
twist xs = let (a,b)  = splitAt ((length xs + 1) `div` 2) xs
    in concat $ transpose [a,b]



{-- ===== Fonctions de lecture ===== --}
-- |Transforme une ligne au format TimeEntry
-- Permet de lire le fichier qui stocke les entréés
litLigne::String -> TimeEntry
litLigne ligne  = (stringToDay day,tail todo)
    where (day,todo) = break (==' ') ligne

{-- ===== traitement des données ===== --}
-- |Trie les tâches du plus récent au plus ancien
taskTrie::[String] -> [TimeEntry]
taskTrie lines  = sort taches 
    where taches = map litLigne lines

-- |Ajoute un numéro au début du texte
-- |La liste doit être triée
taskNum::[TimeEntry] -> [TimeEntry]
taskNum tasks = taskNum' tasks 1
    where
        taskNum' :: [TimeEntry] -> Int -> [TimeEntry]
        taskNum' [] _               = []
        taskNum' ((day,task):q)  nb = 
            (day,('(':show nb)++") "++task):taskNum' q (nb+1) 

-- |Fonction inverse de litLignes, pour une liste
writeTask::[TimeEntry] -> [String]
writeTask []            = []
writeTask ((day,task):q)= ((dayToString day)++(' ':task)):(writeTask q)

-- |Pour écrire dans le fichier
dayToString::Day -> String
dayToString day = jour++"/"++mois++"/"++annee
    where 
        (a,m,j)             = toGregorian day
        (annee,mois,jour)   = (zfill (fromInteger a) 4, zfill m 2, zfill j 2)

-- |Pour écrire dans le terminal
dayToString'::Day -> String
dayToString' day = jour++"/"++mois
    where 
        (_,m,j)             = toGregorian day
        (mois,jour)   = (zfill m 2, zfill j 2)


-- |rend la date à partir d'un String sous la forme "JJ/MM/AAAA"
stringToDay::String -> Day
stringToDay entree = parseTimeOrError True defaultTimeLocale "%d/%m/%Y" entree :: Day

-- |Rend le nom du fichier de la todo liste demandée
nomFichier::String -> String
nomFichier nom = filePrefix++nom++fileSuffix

-- |associe la couleur à une tache en fonction du temps restant
taskColor::[TimeEntry] -> Day -> [FormattedTimeEntry]
taskColor [] _  = []
taskColor ((day,task):q) now 
    | diffDays day now < 0      
        = (day,task,lookup "past" couleur):(taskColor q now)
    | diffDays day now == 0     
        = (day,task,lookup "today" couleur):(taskColor q now)
    | diffDays day now == 1     
        = (day,task,lookup "tomorow" couleur):(taskColor q now)
    | diffDays day now <= 7     
        = (day,task,lookup "week" couleur):(taskColor q now)
    | otherwise                
        = (day,task,lookup "late" couleur):(taskColor q now)

-- |Definit l'association couleur/mise en forme
couleur::[(String,(ColorIntensity,Color))]
couleur = [("today",(Dull,Red))
    , ("tomorow",(Vivid,Red))
    , ("week",(Dull,Yellow))
    , ("late",(Dull,Green))
    , ("past",(Vivid,Green))   
    ]



{-- ===== Autres fonctions ===== --}
-- |Rajoute des espaces à la fin pour avoir un string de taille fixe
-- Ajoute des espaces après <text> pour avoir une taille <taille>
spacefill::String -> Int -> String
spacefill text taille = text ++ (replicate nb ' ')
    where nb = taille - (length text)

-- |Rajoute des espace au début pour avoir un string de taille fixe
-- Ajoute des espaces avant <text> pour avoir une taille <taille>
spacefillBefore::String -> Int -> String
spacefillBefore text taille = (replicate nb ' ') ++ text
    where nb = taille - (length text)

-- |Ajoute des zéros au début d'un chriffre pour un string de taille fixe
-- Affiche <nombre> pour un string de taille <taille>
zfill::Int -> Int -> String
zfill nombre taille = (replicate nb '0') ++ (show nombre)
    where nb = taille - (logMy nombre) 

-- |Logarithme entier
logMy::Int -> Int
logMy n
        | n < 10        = 1
        | otherwise     = 1 + (logMy (n `div` 10))

-- |Affiche la liste <(a:q)> des todo-listes
print_list_todo::[String] -> IO ()
print_list_todo []      = return ()
print_list_todo (a:q)   = do
            ecrire ":: " (Vivid, Red)
            ecrire a (Dull, Green)
            if a == nameTime
               then ecrire " | ne pas supprimer" (Vivid, Green)
               else return ()
            putStr "\n"
            print_list_todo q
