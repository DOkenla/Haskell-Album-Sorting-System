-- 
-- MATHFUN
-- Album Listings
-- UP817474
--

import System.IO
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map (fromListWith, toList)
import Data.List
import Data.Char
import Control.Monad()
import Text.PrettyPrint
import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

--
-- Types
--

type Title      = String
type Artist     = String
type Year       = Int
type Sold       = Int

-- Define Album type here 
data Album      = Album Title Artist Year Sold
                  deriving(Eq,Ord,Show,Read)

testData :: [Album]
testData = [Album "Greatest Hits"                         "Queen"               1981   6300000,
            Album "Gold: Greatest Hits"                   "ABBA"                1992   5400000,
            Album "Sgt. Pepper's Lonely Hearts Club Band" "The Beatles"         1967   5340000,
            Album "21"                                    "Adele"               2011   5110000,
            Album "(What's the Story) Morning Glory?"     "Oasis"               1995   4940000,
            Album "Thriller"                              "Michael Jackson"     1982   4470000,
            Album "The Dark Side of the Moon"             "Pink Floyd"          1973   4470000,
            Album "Brothers in Arms"                      "Dire Straits"        1985   4350000,
            Album "Bad"                                   "Michael Jackson"     1987   4140000,
            Album "Rumours"                               "Fleetwood Mac"       1977   4090000,
            Album "Greatest Hits II"                      "Queen"               1991   3990000,
            Album "Back to Black"                         "Amy Winehouse"       2006   3940000,
            Album "The Immaculate Collection"             "Madonna"             1990   3700000,
            Album "25"                                    "Adele"               2015   3500000,
            Album "Stars"                                 "Simply Red"          1991   3450000,
            Album "Come On Over"                          "Shania Twain"        1998   3430000,
            Album "x"                                     "Ed Sheeran"          2014   3380000,
            Album "Legend"                                "Bob Marley"          1984   3380000,
            Album "Bat Out of Hell"                       "Meat Loaf"           1977   3370000,
            Album "Back to Bedlam"                        "James Blunt"         2004   3360000,
            Album "Urban Hymns"                           "The Verve"           1997   3340000,
            Album "Bridge over Troubled Water"            "Simon & Garfunkel"   1970   3260000,
            Album "1"                                     "The Beatles"         2000   3230000,
            Album "Spirit"                                "Leona Lewis"         2007   3170000,
            Album "Crazy Love"                            "Michael Bublé"       2009   3130000,
            Album "No Angel"                              "Dido"                2000   3090000,
            Album "White Ladder"                          "David Gray"          1998   3020000,
            Album "The Fame"                              "Lady Gaga"           2009   2990000,
            Album "Only by the Night"                     "Kings of Leon"       2008   2980000,
            Album "A Rush of Blood to the Head"           "Coldplay"            2002   2960000,
            Album "Talk on Corners"                       "The Corrs"           1997   2960000,
            Album "Spice"                                 "Spice Girls"         1996   2960000,
            Album "Life for Rent"                         "Dido"                2003   2900000,
            Album "Beautiful World"                       "Take That"           2006   2880000,
            Album "The Joshua Tree"                       "U2"                  1987   2880000,
            Album "Hopes and Fears"                       "Keane"               2004   2860000,
            Album "The War of the Worlds"                 "Jeff Wayne"          1978   2800000,
            Album "X&Y"                                   "Coldplay"            2005   2790000,
            Album "Jagged Little Pill"                    "Alanis Morissette"   1995   2780000,
            Album "Tubular Bells"                         "Mike Oldfield"       1973   2760000,
            Album "Scissor Sisters"                       "Scissor Sisters"     2004   2760000,
            Album "...But Seriously"                      "Phil Collins"        1989   2750000,
            Album "Tracy Chapman"                         "Tracy Chapman"       1988   2710000,
            Album "Parachutes"                            "Coldplay"            2000   2710000,
            Album "The Man Who"                           "Travis"              1999   2687500,
            Album "Greatest Hits"                         "ABBA"                1975   2606000,
            Album "I've Been Expecting You"               "Robbie Williams"     1998   2586500,
            Album "Come Away with Me"                     "Norah Jones"         2002   2556650,
            Album "Graceland"                             "Paul Simon"          1986   2500000,
            Album "Ladies & Gentlemen: The Best of"       "George Michael"      1998   2500000]

-- 
--
--  Your functional code goes here
--
--


-- I

albumsToString :: [Album] -> String
albumsToString [] = ""
albumsToString ((Album title artist year sold) : xs) = "\n" ++ " Title: " ++ title ++ "  Artist: " ++ artist ++ "  Release Date: " ++ show year ++ "  Number of units sold: " ++ show sold ++ "\n" ++ albumsToString xs

getAlbums :: [Album] -> IO ()
getAlbums [] = putStr "You need to enter a valid albumData"
getAlbums albums = putStrLn (albumsToString albums)

--II

top10 :: [Album] -> [Album]
top10 testData = take 10 testData

--III

albumsBetween :: [Album] -> Year -> Year -> [Album]
albumsBetween [] _ _ = []
albumsBetween testData yearb yeare = (filter (\(Album _ _ y _) -> yeare >= y && y >= yearb) testData)

--IV

albumPrefix :: [Album] -> Title -> [Album]
albumPrefix [] _ = []
albumPrefix testData title = filter (\(Album t _ _ _) -> isPrefixOf title t) testData


--V

artistSales :: [Album] -> Sold
artistSales testData = sum ([sold | (Album _ _ _ sold) <- testData])

albumsBy :: [Album] -> Artist -> [Album]
albumsBy testData artist = filter(\(Album _ a _ _) -> a == artist) testData

salesToString :: Sold -> String
salesToString sold = "\n" ++ "  Number of Units Sold: " ++ show sold ++ "\n"


--VI


allAlbums :: [Album] -> [String]
allAlbums testData = nub([artist | (Album _ artist _ _) <- testData])

artistFreq :: [Artist] -> [(Artist, Int)]
artistFreq testData = toList (fromListWith (+) [(x, 1) | x <- testData])

showArtists :: [Album] -> [Artist]
showArtists testData = ([artist | (Album _ artist _ _) <- testData])

freqToString :: [(Artist, Int)] -> String
freqToString [] = ""
freqToString ((artist, int) : xs) = "\n" ++ " Artist: " ++ artist ++ "  Number of Albums in Top Fifty: " ++ show int ++ "\n" ++ freqToString xs


--VII

sortAlbums (Album t1 _ _ s1) (Album t2 _ _ s2)
  | s1 < s2 = GT
  | s1 > s2 = LT
  | s1 == s2 = compare t1 t2

removeLast :: [Album] -> [Album]
removeLast testData = init testData

addEntry :: [Album] -> Album -> [Album]
addEntry testData record = record : testData

sortTop50 :: [Album] -> [Album]
sortTop50 testData = sortBy sortAlbums testData

--VIII

albumToAdjust :: [Album] -> Title -> Artist -> [Album]
albumToAdjust testData title artist = filter(\(Album t a _ _) -> a == artist && t == title) testData

updateAlbum :: [Album] -> Album -> [Album]
updateAlbum testData newAlbum  = newAlbum : testData

showAlbum :: [Album] -> Album
showAlbum testData = head (testData)

changeSales :: Album -> Sold -> Album
changeSales (Album t a y s) x = (Album t a y (s + x))

removeOldBy :: [Album] -> [Album] -> [Album]
removeOldBy testData oldAlbum = testData \\ oldAlbum

-- Demo function to test basic functionality (without persistence - i.e. 
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (albumsToString (albumsBetween testData 2000 2008))
demo 4  = putStrLn (albumsToString (albumPrefix testData "Th"))
demo 5  = putStrLn (salesToString  (artistSales (albumsBy testData "Queen")))
demo 6  = putStrLn (freqToString   (artistFreq (showArtists testData)))
demo 7  = putStrLn (albumsToString (sortTop50 (addEntry (removeLast testData) (Album "Progress" "Take That" 2010 2700000))))
demo 8  = putStrLn (albumsToString (sortTop50 ((changeSales (showAlbum (albumToAdjust testData "21" "Adele")) 400000) : (removeOldBy testData (albumToAdjust testData "21" "Adele")))))

--
--
-- Your user interface (and loading/saving) code goes here
--
--

main :: IO ()
main = do 
    albumData <- loadDB
    putStrLn "-------------------------------------------------------------------------------\n"
    putStrLn "                           UOP's Top 50 Albums"
    putStrLn "                             Author: 817474"
    putStrLn "\n-------------------------------------------------------------------------------\n"
    putStrLn (albumsToString albumData)
    menu albumData

loadDB :: IO [Album]
loadDB = do 
    albumData <- readFile "albums.txt"
    return (read albumData)

menu :: [Album] -> IO ()
menu albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                    Top 50\n"
    putStrLn "===============================================================================\n"
    putStrLn "\n===============================================================================\n"
    putStrLn "                                    Menu\n"
    putStrLn "===============================================================================\n"
    putStrLn " Please choose from the options below:\n"
    putStrLn " 1 - Top 50"
    putStrLn " 2 - Top 10"
    putStrLn " 3 - Show albums released in a range"
    putStrLn " 4 - Search albums by prefix"
    putStrLn " 5 - Show sales of a particular artist"
    putStrLn " 6 - Show how many albums each artist has in the top 50"
    putStrLn " 7 - Remove Album with Least Sales and Add New Entry"
    putStrLn " 8 - Increase an album's sales figures"
    putStrLn " cancel - Exit and discard changes"
    putStrLn " exit - Exit and save changes"
    putStrLn "\n-------------------------------------------------------------------------------\n"
    putStr " Option: "
    option <- getLine
    putStrLn "\n-------------------------------------------------------------------------------\n"
    processOption option albumData

processOption :: String -> [Album] -> IO ()
processOption "1" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                    Top 50\n"
    putStrLn "===============================================================================\n"
    putStrLn (albumsToString albumData)
    menu albumData
processOption "2" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                    Top 10\n"
    putStrLn "===============================================================================\n"
    putStrLn (albumsToString (top10 albumData))
    menu albumData
processOption "3" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                               Albums Release Between\n"
    putStrLn "===============================================================================\n"
    albumsBetweenIO albumData
    menu albumData
processOption "4" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                Album Prefix Search\n"
    putStrLn "===============================================================================\n"
    albumPrefixIO albumData
    menu albumData
processOption "5" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                Sales Search by Artist\n"
    putStrLn "===============================================================================\n"
    artistSalesIO albumData
    menu albumData
processOption "6" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                  Artist Frequency\n"
    putStrLn "===============================================================================\n"
    putStrLn (freqToString (artistFreq (showArtists albumData)))
    menu albumData
processOption "7" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                    New Entry\n"
    putStrLn "===============================================================================\n"
    updatedAlbumData <- addToTop50IO albumData
    menu updatedAlbumData
processOption "8" albumData = do
    putStrLn "===============================================================================\n"
    putStrLn "                                   Change Album Sales\n"
    putStrLn "===============================================================================\n"
    updatedAlbumData <- adjustAlbumSalesIO albumData
    menu updatedAlbumData
processOption "cancel" albumData   = do
    putStrLn "===============================================================================\n"
    putStrLn "                         Exit and Discard Changes?\n"
    putStrLn "===============================================================================\n"
    putStr " Confirm Exit Without Saving: Y to confirm - "
    choice <- getLine
    if (choice == "Y") then putStrLn "\n See you next time! \n" else menu albumData
processOption "exit" albumData   = do
    putStrLn "===============================================================================\n"
    putStrLn "                          Exit and Save Changes?\n"
    putStrLn "===============================================================================\n"
    putStr " Confirm Save and Exit: Y to confirm - "
    choice <- getLine
    if (choice == "Y") then saveAndQuit albumData else menu albumData
processOption _ albumData   = do
    putStrLn "===============================================================================\n"
    putStrLn "                         Please select an option 1-8, cancel or exit\n"
    putStrLn "===============================================================================\n"
    menu albumData


--Manual Write to File

loadDBs :: IO [Album]
loadDBs = do 
    albumData <- readFile "albums.txt"
    return (read albumData)

createTxt :: [Album] -> IO ()
createTxt testData  = do
    writeFile "albums.txt" (show testData)
    putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n" 
    putStrLn "                               Fresh Album Saved!"
    putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
    putStrLn " Exiting...Done! \n"

--IO Support Versions

--Option 3


albumsBetweenIO :: [Album] -> IO ()
albumsBetweenIO albumData    = do
    putStr " Between the Year : "
    year <- getLine
    putStr " and the Year : "
    year2 <- getLine
    if ((isValidYear year) && (isValidYear year2) && length (albumsBetween albumData (read year :: Int) (read year2 :: Int)) > 0)
        then do
            putStrLn (albumsToString (albumsBetween albumData (read year :: Int) (read year2 :: Int)))
        else do
            putStrLn "\n No Albums In Range/No Albums Found \n"


isValidYear :: [Char] -> Bool
isValidYear year 
    | length year /= 4            = False
    | otherwise                    = True

--Option 4

albumPrefixIO :: [Album] -> IO ()
albumPrefixIO albumData    = do
    putStr " Enter Title Prefix/Name (Case-Sensitive) : "
    title <- getLine
    if (length ((albumPrefix albumData title)) == 0)
        then do putStrLn "                    No Albums Found!"
        else do putStrLn (albumsToString (albumPrefix albumData title))
    

--Option 5

artistSalesIO :: [Album] -> IO ()
artistSalesIO albumData    = do
    putStr " Enter Artist (Case-Sensitive): "
    artist <- getLine
    if ((artistSales (albumsBy albumData artist)) == 0)
        then do putStrLn "\n Artist not found\n"
        else do putStrLn (salesToString  (artistSales (albumsBy albumData artist)))


--Option 7

addToTop50IO :: [Album] -> IO [Album]
addToTop50IO albumData    = do
    putStr " Title: "
    title <- getLine
    putStr " Artist: "
    artist <- getLine
    putStr " Year of Release: "
    year <- getLine
    putStr " Number of Sales: "
    sold <- getLine
    let updatedAlbumData = (sortTop50 (addEntry (removeLast testData) (Album title artist (read year :: Int) (read sold :: Int))))
    return updatedAlbumData

--Option 8

adjustAlbumSalesIO :: [Album] -> IO [Album]
adjustAlbumSalesIO albumData   = do
    putStr " Title: "
    title <- getLine
    putStr " Artist: "
    artist <- getLine
    putStr " Increase Sales By: "
    ns <- getLine
    let updatedAlbumData = (sortTop50 ((changeSales (showAlbum (albumToAdjust albumData title artist)) (read ns :: Int)) : (removeOldBy albumData (albumToAdjust albumData title artist))))
    return updatedAlbumData


--Exiting

saveAndQuit :: [Album] -> IO ()
saveAndQuit testData  = do
    writeFile "albums.txt" (show testData)
    putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n" 
    putStrLn "                               Changes Saved!"
    putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
    putStrLn " Exiting...Done! \n"
