
{-
  As stated in the ants.hs file, I used a profiling guide: https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/profiling.html

  For generating executable: 
  ghc -prof -fno-prof-count-entries -o executables/TESTNAME profile.hs

  For running executable (and producing the performance info) (need to be IN EXECUTABLES):
  ./TESTNAME +RTS -p

  Drag the generated prof file into profiles

  When executing the tests, try and ensure a minimum of other processes running. 
  Test averages are calculated as a mean average of 10 tests. 
-}

{-  PROFILE RESULTS
    
    You can inspect each of these manually (in the profiles folder).

    TEST 1: applyRuleWithRecursion vs applyRuleWithZip 

    applyRuleWithZip (s): 0.50, 0.55, 0.49, 0.50, 0.47, 0.50, 0.51, 0.49, 0.47, 0.48
    applyRuleWithZip(avg in s): 0.496

    applyRuleWithRecursion (s): 0.43, 0.40, 0.40, 0.41, 0.40, 0.40, 0.41, 0.41, 0.40, 0.41
    applyRuleWithRecursion (avg in s): 0.407

    applyRuleWithZip takes ~22% more time, on average, to execute than applyRuleWithRecursion
    Even if we consider the fastest zip run (0.47s), it is slower than the slowest recursion run (0.43s)
      
    So applyRuleWithRecursion is faster 

    TEST 2: applyRuleWithRecursion vs tailCallApplyRuleWithRecursion 

    Also I realise that starting it with "tailCall" is confusing because you might think it refers to "proper tail calls" 
    however I have already written the tests and it is MADE CLEAR HERE THAT IT IS NOT THAT. 
    It is just called that because it takes advantage of the function tail. 

    tailCallApplyRuleWithRecursion (s): 0.41, 0.46, 0.44, 0.43, 0.44, 0.45, 0.50, 0.49, 0.41, 0.42 
    tailCallApplyRuleWithRecursion (avg in s): 0.445
    We can reuse the applyRuleWithRecursion benchmarks from before: 

    applyRuleWithRecursion (s): 0.43, 0.40, 0.40, 0.41, 0.40, 0.40, 0.41, 0.41, 0.40, 0.41
    applyRuleWithRecursion (avg in s): 0.407

    So tailCallApplyRuleWithRecursion takes ~9.3% more time, on average, to execute than applyRuleWithRecursion 
    tailCallApplyRuleWithRecursion's times are quite inconsistent however with a large variation. 
    If we consider best cases: the fastest tailCall run (0.41s), it is slower than the fastest applyRuleWithRecursion run (0.40s)
    
    So applyRuleWithRecursion is faster 
-}

-- DEPENDANT DEFINITIONS FOR TESTS

data CellState = Off | On
  deriving (Eq, Ord, Show, Read)

newtype LineState = LS [CellState]
  deriving (Eq, Ord)

instance Show LineState where 
  show :: LineState -> String 
  show (LS cellList) = '|' : map convertCellToChar cellList ++ "|"
    where 
          convertCellToChar :: CellState -> Char
          convertCellToChar x = if x == Off then ' ' else '█'

initialLineState :: LineState 
initialLineState = LS (replicate 50 Off ++ [On] ++ replicate 50 Off)

ruleX :: CellState -> CellState -> CellState -> CellState
ruleX l _ r 
  | exactlyOneOn = On
  | otherwise = Off
  where 
        exactlyOneOn :: Bool
        exactlyOneOn = l /= r -- exactly one neighbour on variable

-- THE PERFORMANCE TESTS 

-- TEST 1 DEFINITIONS

applyRuleWithZip :: (CellState -> CellState -> CellState -> CellState) -> LineState -> LineState
applyRuleWithZip rule (LS cells) = LS (zipWith3 rule leftCells cells rightCells) 
  where 
        rightCells = drop 1 cells ++ [Off] 
        leftCells = Off : cells

applyRuleWithRecursion givenRule (LS cells) = LS (recursiveApplyRule givenRule Off Off cells)
  where 
        recursiveApplyRule :: (CellState -> CellState -> CellState -> CellState)
          -> CellState 
          -> CellState 
          -> [CellState] 
          -> [CellState]
          
        recursiveApplyRule _ _ _ [] = []
        recursiveApplyRule rule l r [x] = [rule l x r]
        recursiveApplyRule rule l r [x,y] = [rule l x y, rule x y r] 
        recursiveApplyRule rule l r [x,y,z] = [rule l x y, rule x y z, rule y z r] -- recursion base case for |cells| > 3 (and solution for |cells| = 3)
        recursiveApplyRule rule l r (x:y:z:xs) = rule l x y : recursiveApplyRule rule x r (y:z:xs) -- recursion step

-- runs apply rule a lot of times 
performanceCheckApplyRule :: (CellState -> CellState -> CellState -> CellState) -> String 
performanceCheckApplyRule rule = show.last $ take 100000 lineStates
    where 
          lineStates :: [LineState] 
          lineStates = iterate (tailCallApplyRuleWithRecursion rule) initialLineState 

-- TEST 2 DEFINITIONS 

-- we only need to additionally add the old definition of applyRuleWithRecursion 

tailCallApplyRuleWithRecursion givenRule (LS cells) = LS (recursiveApplyRule givenRule Off Off cells)
  where 
        recursiveApplyRule :: (CellState -> CellState -> CellState -> CellState)
          -> CellState 
          -> CellState 
          -> [CellState] 
          -> [CellState]
          
        recursiveApplyRule _ _ _ [] = []
        recursiveApplyRule rule l r [x] = [rule l x r]
        recursiveApplyRule rule l r [x,y] = [rule l x y, rule x y r] 
        recursiveApplyRule rule l r [x,y,z] = [rule l x y, rule x y z, rule y z r] -- recursion base case for |cells| > 3 (and solution for |cells| = 3)
        recursiveApplyRule rule l r wholeList@(x:y:_:_) = rule l x y : recursiveApplyRule rule x r (tail wholeList) -- recursion step

main :: IO ()
main = do 
    putStrLn $ performanceCheckApplyRule ruleX 