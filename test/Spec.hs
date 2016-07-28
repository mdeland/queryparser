import System.Console.ANSI
import Parse
import Types
import Extract
import PrintQuery


import qualified Data.HashSet as HS
import Data.List


runFormatTest :: (String, [CommentData], String) -> IO ()
runFormatTest (q, cd, exp) = do
    p <- runParse q
    let res = formatQuery p cd
    if res == exp
        then do
                setSGR [SetColor Foreground Vivid Green]
                putStrLn "Passed"
                setSGR [Reset]
        else do
                setSGR [SetColor Foreground Vivid Red]
                putStrLn "Failed: "
                putStrLn $ "query   : " ++ q
                putStrLn $ "expected: " ++ exp
                putStrLn $ "received: " ++ res
                setSGR [Reset]

formatTests :: [(String, [CommentData], String)]
formatTests = [
        -- basic
         ("select a from b", [], "SELECT a\nFROM b"),
         ("\n\n\nselect \n\n\n\n1\n\n\n", [], "SELECT 1"),
         ("select a1, a2 from b", [], "SELECT\n\ta1,\n\ta2\nFROM b"),
         ("select a + b from t", [], "SELECT a + b\nFROM t"),
        -- function
         ("select a, sum(c) from b group by 1", [],
            "SELECT\n\ta,\n\tsum(c)\nFROM b\nGROUP BY 1"),
        -- group
         ("select a, b, sum(c) from b group by 1, 2", [],
            "SELECT\n\ta,\n\tb,\n\tsum(c)\nFROM b\nGROUP BY\n\t1,\n\t2"),
        -- select *
         ("select * from tab", [], "SELECT *\nFROM tab"),
        -- select * with alias
         ("select a.* from tab as a", [], "SELECT a.*\nFROM tab AS a"),
        -- table.column
         ("select a.b, a.c from tab as a", [], "SELECT\n\ta.b,\n\ta.c\nFROM tab AS a"),
        -- where
         ("select a from b where x = 1", [], "SELECT a\nFROM b\nWHERE\n\tx = 1"),
        -- conjunction clause
         ("select a from b where x = 1 and y = 2", [], "SELECT a\nFROM b\nWHERE\n\tx = 1\n\tAND\n\ty = 2"),
        -- conjuction with a disjunction, unspecified
         ("select a from b where x = 1 and y = 2 or z = 3", [], "SELECT a\nFROM b\nWHERE\n\t(\n\t\tx = 1\n\t\tAND\n\t\ty = 2\n\t)\n\tOR\n\tz = 3"),
        -- specified conjunction precedence
         ("select a from b where (x = 1 and y = 2) or (z = 3 and w = 4)", [], "SELECT a\nFROM b\nWHERE\n\t(\n\t\tx = 1\n\t\tAND\n\t\ty = 2\n\t)\n\tOR\n\t(\n\t\tz = 3\n\t\tAND\n\t\tw = 4\n\t)"),
        -- limit and offset
         ("select * from tab limit 10 offset 7", [], "SELECT *\nFROM tab\nLIMIT 10\nOFFSET 7"),
        -- inner select
         ("select * from (select 1) t", [], "SELECT *\nFROM (\n\tSELECT 1\n) AS t")
        -- comment
         -- ("select 1  --comment\nfrom t", [CommentData ("--comment", 10)],
         --    "SELECT 1 --comment\nFROM t")
        ]

runTableExtractTest :: (String, [String]) -> IO ()
runTableExtractTest (q, exp) = do
    p <- runParse q
    let res = extractTables p
    if res == HS.fromList exp
        then do
                setSGR [SetColor Foreground Vivid Green]
                putStrLn "Passed"
                setSGR [Reset]
        else do
                setSGR [SetColor Foreground Vivid Red]
                putStrLn "Failed: "
                putStrLn $ "query   : " ++ q
                putStrLn $ "expected: " ++ (unwords exp)
                putStrLn $ "received: " ++ (unwords $ HS.toList res)
                setSGR [Reset]

tableExtractTests =
    [ ("select * from a", ["a"])
    , ("select * from a, b", ["a", "b"])
    , ("select * from a join b on a.x = b.y", ["a", "b"])
    , ("select * from s.a join t.b on a.x = b.y", ["s.a", "t.b"])
    , ("select * from (select * from a) x", ["a"])
    ]


main :: IO ()
main = do
    -- mapM_ runFormatTest formatTests
    mapM_ runTableExtractTest tableExtractTests
