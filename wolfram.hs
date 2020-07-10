--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019 [WSL: Ubuntu]
-- File description:
-- wolfram
--

import System.Exit
import System.IO
import System.Environment

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

exitHelp :: IO a
exitHelp = exitWithErrorMessage "error arg" (ExitFailure 84)

intcheck :: String -> Bool
intcheck a = case reads a :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

begin :: (Num a, Ord a) => a -> [Char] -> [Char]
begin incre string
    | incre == 40 = begin (incre + 1) (string ++ "*")
    | incre < 381 = begin (incre + 1) (string ++ " ")
    | otherwise = string

new_liner ::
  (Ord t1, Num t1) =>
  t2 -> t1 -> [Char] -> [Char] -> Int -> [Char] -> [Char]
new_liner start incre string new_string elem rule
    | incre < 380 = rule_checker start incre string new_string elem rule
    | incre == 380 = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | otherwise = new_string

linesa :: (Read p, Num p) => [[Char]] -> Int -> p
linesa args incre
    | (args !! incre == "--lines") = read (args !! (incre + 1))
    | ((incre + 1) == length args) = 30000
    | otherwise = linesa args (incre + 1)

start :: (Read p, Num p) => [[Char]] -> Int -> p
start args incre
    | (args !! incre == "--start") = read (args !! (incre + 1))
    | ((incre + 1) == length args) = 0
    | otherwise = start args (incre + 1)

fullish ::
  (Ord t, Num t) => t -> [Char] -> [Char] -> t -> t -> t -> IO ()
fullish start string rule incre_start incre lines =
    if (incre_start >= (start - 1)) then do
        putStrLn (take 80 (drop 300 (new_liner start (-300) string [] 0 rule)))
        init_wolf lines (incre + 1) start (incre_start + 1) (new_liner start (-300) string [] 0 rule) rule
    else init_wolf lines (incre + 1) start (incre_start + 1) (new_liner start (-300) string [] 0 rule) rule

init_wolf ::
  (Ord a, Num a) => a -> a -> a -> a -> [Char] -> [Char] -> IO ()
init_wolf lines incre start incre_start string rule
    | incre < (lines + start) = fullish start string rule incre_start incre lines
    | otherwise = return ()

rule_checker ::
  (Ord a, Num a) =>
  t -> a -> [Char] -> [Char] -> Int -> [Char] -> [Char]
rule_checker start incre string new_string elem rule
    | elem == 0 = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "30" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "110" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "90" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "30" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "110" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "90" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == ' ' && string !! (elem - 1) == ' ' && rule == "30" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "110" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "90" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == '*' && string !! (elem - 1) == ' ' && rule == "30" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "110" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "90" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "30" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == '*' && string !! (elem - 1) == ' ' && rule == "110" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == '*' && string !! (elem - 1) == ' ' && rule == "90" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "30" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == ' ' && string !! (elem - 1) == ' ' && rule == "110" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == '*' && string !! (elem + 1) == ' ' && string !! (elem - 1) == ' ' && rule == "90" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == ' ' && string !! (elem - 1) == '*' && rule == "30" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == '*' && string !! (elem - 1) == ' ' && rule == "110" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == '*' && string !! (elem - 1) == ' ' && rule == "90" = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == '*' && string !! (elem - 1) == '*' && rule == "30" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == ' ' && string !! (elem - 1) == ' ' && rule == "110" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    | string !! elem == ' ' && string !! (elem + 1) == ' ' && string !! (elem - 1) == ' ' && rule == "90" = new_liner start (incre + 1) string (new_string ++ " ") (elem + 1) rule
    |otherwise = new_liner start (incre + 1) string (new_string ++ "*") (elem + 1) rule

origine :: (Num t, Ord t) => [[Char]] -> t -> t -> Int -> IO ()
origine args lines start incre =
    if args !! incre == "--rule" && ((args !! (incre + 1) == "30") || (args !! (incre + 1) == "90") || (args !! (incre + 1) == "110")) && start == 0 then do
       putStr (take 80 (drop 300 (begin (-300) [])))
       putChar '\n'
       init_wolf lines 0 start 0 (begin (-300) []) (args !! (incre + 1))
    else if args !! incre == "--rule" && ((args !! (incre + 1) == "30") || (args !! (incre + 1) == "90") || (args !! (incre + 1) == "110")) && start /= 0 then do
       init_wolf lines 0 start 0 (begin (-300) []) (args !! (incre + 1))
    else origine args lines start (incre + 1)

erge :: Int -> [[Char]] -> IO ()
erge incre args
    |   (length args) == 0 = exitHelp
    | (((args !! incre) /= "--start") 
        &&  ((args !! incre) /= "--window") 
        && ((args !! incre) /= "--move") 
        &&  ((args !! incre) /= "--lines") 
        && ((args !! incre) /= "--rule") 
        && (intcheck (args !! incre) == False)) = exitHelp
    |  (incre + 1) == length args = return ()
    | otherwise = erge (incre + 1) args

main :: IO ()
main = do
    args <- getArgs
    erge 0 args
    origine args ((linesa args 0) - 1) (start args 0) 0