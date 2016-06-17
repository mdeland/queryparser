import System.Console.ANSI
import Parse


runTest :: (String, String) -> IO ()
runTest (q, exp) = do
    res <- runParse q
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

tests :: [(String, String)]
tests = [("select a from b", "SELECT a\nFROM b"),
         ("\n\n\nselect \n\n\n\n1\n\n\n", "SELECT 1"),
         ("select a1, a2 from b", "SELECT\n\ta1,\n\ta2\nFROM b"),
         ("select a, sum(c) from b group by 1",
          "SELECT\n\ta,\n\tsum(c)\nFROM b\nGROUP BY 1"),
         ("select a, b, sum(c) from b group by 1, 2",
          "SELECT\n\ta,\n\tb,\n\tsum(c)\nFROM b\nGROUP BY\n\t1,\n\t2"),
         ("select * from tab", "SELECT *\nFROM tab"),
         ("select a.* from tab as a", "SELECT a.*\nFROM tab AS a"),
         ("select a.b, a.c from tab as a", "SELECT\n\ta.b,\n\ta.c\nFROM tab AS a"),
         ("select a from b where x = 1", "SELECT a\nFROM b\nWHERE\n\tx = 1"),
         ("select a from b where x = 1 and y = 2", "SELECT a\nFROM b\nWHERE\n\tx = 1\n\tAND y = 2"),
         ("select a from b where x = 1 and y = 2 or z = 3", "SELECT a\nFROM b\nWHERE\n\tx = 1\n\tAND y = 2\n\tOR z = 3"),
         ("select * from (select 1) t", "SELECT *\nFROM (\n\tSELECT 1\n) AS t")
        ]

main :: IO ()
main = do
    print ""
    mapM_ runTest tests
