module Ants where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 5515605
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

{-  RESOURCES USED

  I used the documentation for Data.Set found at https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html 
  This was for learning how to use Set including how to construct Set and knowledge of its methods 

  I used tips for elegance from https://wiki.haskell.org/Haskell_programming_tips

  I figured out profiling from https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/profiling.html

  For information (used for the loopedAt comments) about HashSet: https://hackage.haskell.org/package/unordered-containers-0.2.20/docs/Data-HashSet.html 

  To verify what the automata look like, I used https://mathworld.wolfram.com/ElementaryCellularAutomaton.html
-}

import Ants.Types
import Images ( ant, blank, rect )
import Layout ( (<@>), Image )
import Transforms ( offset, rotate )
import Data.Bifoldable (bifoldl1)
import Data.Bits (Bits(xor))

-- | Uncomment these two lines once you get to Exercise 6.
import Data.Set (Set)
import qualified Data.Set as Set

{-  PROFILING
    
    To justify one function's performance over another, I have done some profiling. 
    
    You can view these performance tests inside the profiling folder, at profiling/profiles 
    There are comments about the specifics of the profiling inside of profiling/profile.hs 
-}

{-  TESTING 

    There is additional testing below as some tests are insufficient and I also wanted to convince myself 
    In particular, there are visualisations inside of visualisations. 
    The process for generating these is explained just after Ex 5. 
-}

{-  COMMENTS ON COMMENTS (PLEASE READ FIRST)

    I have written comments as specified in the specification:
    - does it explain what the code does? 
    - does it explain what Haskell language features are used? 
    - does it show us that the author understands the work? 
    - is it clear and easy to read?

    Where possible, I have also included a discussion of *why* I chose the approach that I did and in some cases,
    a discussion of the alternative approaches and why these were rejected in favour of the selected approach. 
    When the discussion becomes lengthy, I make a separation between the comments fitting the specification
    and the more involved parts with the following divider: 

    /==--==/

    I also separate analysis of specific topics using:

    // 
-}

{-| 
  Ex. 1: Implement a Show instance for LineState.

  (The pipe character and full block character are | and █)

  The chosen solution uses pattern matching to access the cellList :: [CellState] wrapped in the LineState instance.
  We use a where binding to declare the helper function convertCellToChar that converts each CellState to its corresponding character.  
  We have to do this because CellState already derives Show, so we cannot use ad-hoc polymorphism. 

  cellList is mapped over with convertCellToChar to generate a list of characters (a String). 
  That string is prepended with the Char '|' using the cons operator. 
  As we cannot append with cons, we use list concatenation to append the String "|"

  /==--==/

  We generally use the where binding as opposed to a top-level definition of helper functions as it's good practice because it groups together 
  the relevant logic together and also doesn't expose the function to the outside where it's not needed to be seen. 
  This is especially relevant here where CellState already has a standard Show implementation, so convertCellToString is very specific to our function.
  //

  Instead of prepending the Char '|' to the String with the cons operator, we could've used the String "|" and used list concatenation. 
  These should both work performantly even though usually cons is usually favoured for performance,
  because the String "|" is a 1-element list so appending to the end is cheap. 
  It is used because I favour the pattern of using cons where possible, due to the potential performance benefits from more complex cases.
  //

  We could've also written the main body of show with a list comprehension:

  show (LS cellList) = '|' : [convertCellToChar x | x <- cellList] ++ "|"

  I wrote it using the higher-order function map because it is good practice to use library functions where relevant to simplify code. 
  Also because the library functions are probably written in a more optimised way. 
  //

  If we wanted to later replace cell representations with strings (say <Off> and <On>) instead of single characters, then we might have used a more extendable solution:

  show (LS cellList) = "|" ++ concatMap convertCellToString cellList ++ "|"
    where convertCellToString :: CellState -> String
          convertCellToString x = if x == Off then "<Off>" else "<On>"

  We could've also used foldr or explicit recursion to accomplish this however this would be under-utilising library functions (mentioned above). 
  
  concatMap applies a function (as map does) to each element, and then concatenates each transformed element into a single list. 
  As such, each transformed element must be a list so the function must return a list. 

  concatMap is of type Foldable t => (a -> [b]) -> t a -> [b]
  In this case, a is CellState and b is Char. 
  Therefore [b] is a String, so the helper function return type and main function return type is String. 

  However, we do not use this more flexible solution because it is not needed. 
  Instead, the very slightly more performant and more appropriate solution is used. 
-}

instance Show LineState where 
  show :: LineState -> String 
  show (LS cellList) = '|' : map convertCellToChar cellList ++ "|"
    where 
          convertCellToChar :: CellState -> Char
          convertCellToChar x = if x == Off then ' ' else '█'

{-|
  Ex. 2: Implement ruleX, which turns a cell on if it has exactly one neighbour which was on in the previous step. The three arguments are the states of the left neighbour, current cell, and right neighbour respectively.

  RuleX depends entirely on the state of the left and right cells so we can ignore the state of the cell itself so we use _ to discard the argument 
  We use the not equal operator (which is the XOR operator) as it is only true when exactly one of the neighbours is on
  We can see this truth table below: 

  A | B | A XOR B 
  F | F | F
  F | T | T 
  T | F | T
  F | F | F 

  Therefore, we can verify that it is true when exactly one of {A, B} is on 

  /==--==/

  We could've used the boolean algebra formula for XOR from CS130 and written: 

  ruleX l _ r = (l == On && r == Off) || (l == Off && r == On)

  However I use XOR because it's already defined. 

  I could've also used top-level pattern matching (basically the truth table): 

  ruleX Off _ Off = Off 
  ruleX Off _ On = On 
  ruleX On _ Off = On 
  ruleX On _ On = Off

  This might've actually been a more readable solution and it fits with how we define rules in Exercise 5 (allRules). 
  However, I opted for the XOR approach because the behaviour of the rule is more obvious and it is written in a more concise way.
  So in this way, the readability of the solution is actually improved with the XOR if the goal of reading is to understand what ruleX's behaviour is. 
-}

-- returns On if exactly one neighbour (left or right) is On 
ruleX :: CellState -> CellState -> CellState -> CellState
ruleX l _ r = if l /= r then On else Off 

{-|
  Ex. 3: Implement applyRule, which, given a rule and a line state, applies the rule to each cell in the state and returns the updated state.

  In the following explanation, TRANSFORMING is the same as APPLYING the rule. 

  Generally, applyRule recurses through the list of CellStates and builds a list of transformed CellStates starting by building the transformed list from the rightermost elements. 
  It has pattern matching to handle cases for lists of length 0, 1, 2, and 3. These list lengths are handled without recursion. 
  Any bigger list recurses downwards to the case of list 3 where it then builds the list from rightermost elements. 

  //

  applyRule has a where binding where it defines recursiveApplyRule which does all the heavylifting
  recursiveApplyRule requires a rule, left element, right element, and a list of CellStates 
  The left element is the state of the element immediately to the left of the list of CellStates 
  //

  It will return a list of CellStates that is the same cardinality of the list passed in, however it requires left and right element so that 
  it knows how to treat the transformation of the leftermost and rightermost cells inside the list passed in. 
  This is because a leftermost element does not have an element to the left of it so the rule would not have enough arguments. 
  If you tried, it would look like:

  list = [leftermost, nextElement, ... ]

  rule ? leftermost nextElement

  And you would have no appropriate value for "?". So we introduce the left element. Now the call looks like:

  rule leftElement leftermost nextElement

  Keep in mind that leftElement is provided as "l" in the arguments of recursiveApplyRule. 
  Similarly, the transformation for the rightermost element will look like: 

  list = [... prevElement, rightermostElement]

  rule prevElement rightermostElement rightElement

  Again, rightElement is provided as "r" in the arguments of recursiveApplyRule.  
  //

  It pattern matches for all the possible cases:

  - The first is the case of an empty list where we just returns an empty list. All arguments are discarded as no computation is needed. 

  - The second is the case of a single element list. In this case, the single element is the leftermost and rightermost element. 
  Therefore, we need to use leftElement and rightElement in its application of the rule. When transforming this element, you can imagine its neighbourhood to be l [x] r. 
  
  - The third is the case of a list with two elements. The first element is the leftermost element so leftElement needs to be used. However, we actually
  have a right element that's inside the list when we apply the rule. So the neighbourhood of the first element is l [x, y]. 
  Similarly, the second element is the rightermost element so rightElement needs to be used. Neighbourhood of second element is [x, y] r. 

  - The fourth is the case of a list with three elements. It is also the base case for recursion of higher element lists. This means that all lists with cardinality > 3 
  will eventually call the case of the list with three elements as its recursive base case. For the first and last elements, we use leftElement and rightElement respectively. 
  For the middle element, we have left and right elements within the list so we can simply call it with those as arguments. For a list [x,y,z], the neighbourhood 
  of the middle element "y" will be x y z. 

  - The fifth case is for any list with cardinality greater than 3. We apply the rule to the leftermost element and then we recurse: we call the function on the list but with the leftermost
  element removed since we've already applied the rule to that. With each function call (recurse), we reduce the cardinality by 1. When the cardinality reaches 3, it pattern matches 
  with the fourth case therefore falling into the base case. When it reaches the base case, it returns the transformed list of the right three elements and then uses cons to 
  prepend all the other elements (transformed). 
  When we're recursing downwards, we need to continually pass rightElement so that we can eventually use it when we apply the rule to the rightermost element. 
  We pass leftElement as the old value of the leftermost element that we're removing when we recurse. It needs to be the old value so that we update all the values at the same time;
  otherwise the behaviour of the automaton is dependent on which direction we iterate through. It also just wouldn't behave as specified. 

  The pattern match is from the smallest case to the biggest case (in terms of cardinality) so that it tries to match the least general first, and then goes to the general cases. 
  For instance, if it starts with the general case then it will recurse until it can reach the base case. 

  Finally, we call recursiveApplyRule in applyRule's body. We call it with leftElement and rightElement being Off because when we transform the leftermost and rightermost elements, 
  we assume cells outside the range to be Off (as mentioned a couple times already). 

  /==--==/

  The reason that recursiveApplyRule does not just use givenRule is because I didn't want multiple different versions of the function 
  for several different calls to applyRule. So instead, we pass givenRule to recursiveApplyRule. 

  We used to define the fifth case for recursiveApplyRule as: 

  recursiveApplyRule rule l r wholeList@(x:y:_:_) = rule l x y : recursiveApplyRule rule x r (tail wholeList) -- recursion step

  Our current approach requires us to reconstruct the tail of the list by connecting y and z back to the rest of the list (xs), 
  So I thought using just tail would make it more performant which is why it was originally used. However, after benchmarking it, it seems that it's actually slower.  
  Probably due to having to write to both wholeList and do the pattern matching as well looking up the tail could possibly be slower than reconstructing the tail too
  because cons is very fast. 

  
  For the benchmark of using tail vs our current approach, SEE TEST 2 in profiling. 
  As shown in the test, the old/rejected/tail approach takes ~9% more time to run than applyRuleWithRecursion (chosen approach). 
  //

  We also could've defined applyRule much more easily with higher order functions. 
  However, we did not use this implementation because the explicit recursion version is faster. 
  To see the evidence, PLEASE REFER TO TEST 1 in profiling. 

  As shown in the test, applyRuleWithZip takes ~22% more time to run than applyRuleWithRecursion (chosen approach). 
  
  The higher order function approach: 

  applyRule rule (LS cells) = LS (zipWith3 rule leftCells cells rightCells) 
    where 
          rightCells = drop 1 cells ++ [Off] 
          leftCells = Off : cells

  zipWith3 here does most of the heavylifting. It iterates through each of the 3 lists provided until one of them runs out of elements. 
  During this iteration, it applies the function provided on 3 arguments (one from each list). The results of this function are stored into the list returned. 

  If we consider applying a rule to each cell, we need to consider its neighbourhood. So we need left, middle (itself), and right cells. 
  If we have a list corresponding to the left cells, one for the middle cells, and one for the right cells then we can simply use zipWith3 
  to iterate through each of those lists to return the applied cells. 

  We need those lists to look like this (although we'll use ints instead of CellState to illustrate this clearly): 

  ExampleList:  1 2 3 4 5 

  Left:         0 1 2 3 4 

  Middle:       1 2 3 4 5 

  Right:        2 3 4 5 0

  Here, 0 is treated as the cells which are not in the actual list (which is Off with CellStates). 

  To get the Left list, we just need to prepend Off. We do not need to cut off the last element because zipWith3 stops 
  when the shortest list runs out of elements to iterate through. 

  To get the Middle list, we simply use the list itself. 

  To get the Right list, we cut off the first element and append Off. We do this using drop (which drops n elements) and then use list concatenation to add Off (in a new list). 

  Also notice that the explicit recursion approach is more readable. 
  To read the zipWith3 approach, you have to understand what zipWith3 does first. 
  With the recursion approach, each case is defined clearly and you just have to be familiar with recursion for it to be readable. 
-}

applyRule :: (CellState -> CellState -> CellState -> CellState)
          -> LineState
          -> LineState

applyRule givenRule (LS cells) = LS (recursiveApplyRule givenRule Off Off cells)
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

{-|
  Ex. 4: Implement the loopedAt function, which takes a rule and a starting configuration, and returns the number of the iteration at which the automaton first revisits a state.
  
  LoopedAt generally works by keeping a set of visited states. When we inspect a new state, either it is the same as a previously visited state (in which case we return the length of the set).
  Otherwise, we need to keep checking further states for looping so we recurse further until we find a new state that is the same as a previously visited state. 

  The size (or cardinality or length) of the set works as the return value because this means n states have been visited. Therefore, the next state (n+1) being the same means that 
  the matched state is generation n because generation has indexing beginning at 0. So generation n has n+1 visited states with the n+1th state being the same as a previous state. 
  We use Set.size for this. 

  To check if a state is the same as a previously visited state, we use Set.member (which functions the same as elem for lists). 
  We construct the initial Set (which just contains the state passed to loopedAt) with Set.singleton which constructs a Set with one element in it. 
  We use Set.insert to insert into a Set (it returns a Set with the inserted element). 

  We use a where binding as nextState is used many times and is separated clearly from the body of recursiveLoopedAt. 
  Similarly, the where binding is used so that outside users can't see recursiveLoopedAt so they are forced to interact with it through loopedAt. 

  /==--==/

  The test for this function in insufficient: the test only checks if a function can detect that the next LineState is the same as the current one. 
  So we can write a function that just tests this and it will pass the test:

  loopedAt rule lineState 
    | nextState == lineState = 1
    | otherwise = 1 + loopedAt rule nextState 
    where 
          nextState = applyRule rule lineState
  
  This function simply checks whether the nextState is the same as the current one and if not, then recurse with the nextState as the initialState. 
  Therefore it will run infinitely for rules where the immediate next state is never the same as the previous state. 
  So it will run infinitely for rules such as Rule 1. Of course, our chosen function does not do this and computes it correctly. 

  We talk about how we tested for this in the testing section (just after Exercise 5). 
  // 

  We use sets instead of lists. This is because using elem on a list has O(n) time complexity. 
  However, using Set's member has O(log(n)) time complexity (according to aforementioned sources aka Hoogle) 
  and also because it's underlying structure is ordered which allows for fast checking (e.g. binary search). 

  This allows for checking previous states for whether the current state has been visited before much quicker. 
  The tradeoff is that Set.insert will be slower than prepending to a list however it is worth the cost for the sake of fast indexing. 
  // 

  Using HashSet would have led to even faster runtimes than sets. This is because, according to the page for HashSet, 
  "The implementation uses a large base (i.e. 16) so in practice these operations are constant time". 

  However, HashSet was not used as it would require additional setup from outside of the ants.hs file. 
  // 

  We do not do something like: 

  nextLineState = Set.insert nextState stateSet 

  Even though this is more readable, it does not make sense to shove this under the where binding where nextState is stored. 
  This is because it's never used in the case of the nextState being a member of the currently recorded states. 
  It also makes the code more verbose to use a let just to signal to the reader that it is the nextLineState. 

  Similarly, we do not declare something like:

  cardinalityOfSet = Set.size stateSet 

  Same reasoning: it may not be used in the case of the nextState not being within the set of previously visited sets. 
-}  

loopedAt :: (CellState -> CellState -> CellState -> CellState)
  -> LineState
  -> Int

loopedAt rule lineState = recursiveLoopedAt (Set.singleton lineState) lineState  
  where 
        recursiveLoopedAt :: Set LineState -> LineState -> Int
        recursiveLoopedAt stateSet currentLineState
          | Set.member nextState stateSet = Set.size stateSet
          | otherwise = recursiveLoopedAt (Set.insert nextState stateSet) nextState  
          where 
                nextState :: LineState 
                nextState = applyRule rule currentLineState 

{-|
  Ex. 5: Implement allRules, which returns all 256 possible rules.

  Generally, allRules works by mapping the ints from 0-255 to 8-bit binary representations which are then mapped to a rule. 

  The reason this works is because there are 3 inputs (left, middle, right) to each rule and each input has 2 possible states (On or Off). 
  So there are 2^3 = 8 possible input states. 
  For each of these possible input states, there are 2 possible output states (On or Off). 
  So there are 2^8 possible rules. 
  We can imagine the outputs to each of these possible input states as a bit in a binary number: 

  000 -> ? 
  001 -> ? 
  .
  .
  .
  111 -> ? 

  We achieve this using the higher order function map and getRuleFromInt to convert the integers into a list of rules. 

  getRuleFromInt is a composition of functions which feeds the int to convertIntTo8BitBinary first and then feeds the binary number into getRuleFromBinary. 
  It also uses eta reduction so even though n (int passed) isn't explicitly written, n is still passed to getRuleFromInt. 

  convertIntTo8BitBinary also uses eta reduction (so n isn't explicitly written) and uses a where binding for the helper functions convertIntToReversed8BitBinary and addLeadingOffs. 
  It also uses a ton of function composition to convert into reversed binary, reverse it (so it's normal), and then add leading Offs (explained later). 
  convertIntToReversed8BitBinary does most of the heavylifting and uses the division algorithm we learn in CS131 to convert an integer to a binary number. 

  convertIntToReversed8BitBinary uses explicit recursion. It has a base case for when the number is 0 (after all the divisions) which is just the empty list. 
  If not, then it divides and (as per the algorithm) adds the modulus of the calculation to the number. In this case, a 0 corresponds to Off and a 1 corresponds to On. 
  This is what cellState achieves using an if statement. 
  We use a let binding to cleanly retrieve the integer division result and modulus value as well as prepend the bit to the list.
  The only problem is that this algorithm generates the bits for the binary number in reverse so we must pipe the output of this function through reverse. 
  
  The function also leads to smaller numbers having smaller length than full 8-bit numbers. For example, 3 is represented as just [1,1] in this. To make them proper 
  binary numbers so that we can use them for rules, we have to add leading 0s (or leading Offs). 
  We use addLeadingOffs which takes advantage of replicate. 
  Replicate repeats a value n times and then puts the value into a list. So we generate a list of Offs (leading Offs).  
  We then attach the leading Offs to the list using the list concatenation operator (++). 
  We need the leading Offs plus the binary number generated to have a length of 8 so the leading Offs must have a length: 8 - length binaryNumber 

  We represent the binary number as a list of CellStates since CellStates can be either Off or On (analagous to 0 and 1). This way, we can directly index 
  the binary number when we get to building the rule without having to convert from 0 to Off and 1 to On. 
  It being a list also allows us to use reverse. 
  //

  getRuleFromBinary uses partial function application to generate rules. We pass only a binary number (represented as a list of CellStates) and then it gives us a rule.
  If we fully apply the arguments (i.e. pass in the states as well), then we'd get the output for the rule defined by the binary number. 

  Since how a rule behaves is defined by the binary number, the rule simply indexes each bit in the binary number for each possible input state. 
  It does this using top-level pattern matching. The order in which the patterns are written is irrelevant. 

  Note that head bn is the same as bn !! 0. It is just that head bn is preferred over indexing where possible (and the linter complained at me)

  /==--==/ 
  
  Since all rules does not have to be in a certain order, it is not necessary for us to reverse the 8 bit binary number. 
  In other words, it doesn't matter if rule 1 actually behaves like rule 128. 

  However, I wanted getRuleFromInt to behave as the original elementary cellular automata do so that it's easier to test. 
  It is much easier for a ruleOne to behave the same as getRuleFromInt 1. 
  Other programmers would also expect the rules to behave as they are widely known to. 

  I also did not want to keep convertIntTo8BitBinary unreversed and reverse the order head bn, bn !! 1 ... bn !! 7 for getRuleFromBinary 
  because this would make convertIntTo8BitBinary return the reversed binary number. This would make it unmaintainable as the expected behaviour,
  to another programmer (or future me), would be that it returns the proper binary representation of the number. 
  //

  An alternative (old) version of writing convertIntTo8BitBinary is the following: 

  convertIntTo8BitBinary = reverse.convertIntToReversedNBitBinary 8
    where
          convertIntToReversedNBitBinary :: (Integral a) => a -> a -> [CellState]
          convertIntToReversedNBitBinary nBits nToConvert 
            | nBits == 0 = []
            | otherwise = 
              let (value, remainder) = nToConvert `divMod` 2 -- value is result of integer division 

                  cellState :: CellState
                  cellState = if remainder == 0 then Off else On 

              in cellState : convertIntToReversedNBitBinary (nBits - 1) value 

  Instead of stopping recursion when there are no more divisions because it's reached 0, it stops recursing when nBits hits 0. 
  This means that, for 8 bits, it always runs for 8 iterations. Even if the number is 1, it will run many times after with calls where nToConvert is 0. 
  This means that it does not have to add leading zeros because the leading zeros are added by the constant dividing of 0 by 2 to produce a string of Offs. 

  We opted for the current approach because it separates the concerns of adding the leading zero (to fit the shape of an 8 bit number) and calculating the digits better. 
  convertIntToReversedNBitBinary also requires an additional argument to be passed (nBits) and is unnecessarily complicated. It will probably run slower as well
  since doing division to produce a string of Offs is probably slower than just repeating Off a bunch of times.  
  // 

  We use eta reduction a couple times (getRuleFromInt, convertIntTo8BitBinary) which leverages the idea that functions are first class values. 
  This is a good way to write functions in Haskell and we use specifically point-free style here which is writing the functions without explicit arguments. 
  This is good because functions are written in a more concise form. It is clear that getRuleFromInt just converts an int to a binary and then the binary to a rule. 
  Also, you can infer the arguments from the types given so it's fine from the perspective of calling the function. 
-}

convertIntTo8BitBinary :: Int -> [CellState]
convertIntTo8BitBinary = addLeadingOffs.reverse.convertIntToReversed8BitBinary
  where
        convertIntToReversed8BitBinary :: (Integral a) => a -> [CellState]
        convertIntToReversed8BitBinary 0 = []
        convertIntToReversed8BitBinary nToConvert = 
            let (value, remainder) = nToConvert `divMod` 2 -- value is result of integer division 

                cellState :: CellState
                cellState = if remainder == 0 then Off else On 

            in cellState : convertIntToReversed8BitBinary value 
        
        addLeadingOffs :: [CellState] -> [CellState] -- this is specific to 8-bit numbers
        addLeadingOffs bn = replicate (8 - length bn) Off ++ bn

getRuleFromInt :: Int -> (CellState -> CellState -> CellState -> CellState)
getRuleFromInt = getRuleFromBinary.convertIntTo8BitBinary
  where 
        getRuleFromBinary :: [CellState] -> (CellState -> CellState -> CellState -> CellState)
        getRuleFromBinary bn Off Off Off   = bn !! 7
        getRuleFromBinary bn Off Off On    = bn !! 6
        getRuleFromBinary bn Off On  Off   = bn !! 5
        getRuleFromBinary bn Off On  On    = bn !! 4
        getRuleFromBinary bn On  Off Off   = bn !! 3
        getRuleFromBinary bn On  Off On    = bn !! 2
        getRuleFromBinary bn On  On  Off   = bn !! 1
        getRuleFromBinary bn On  On  On    = head bn -- i.e. bn !! 0 

allRules :: [ CellState -> CellState -> CellState -> CellState ]
allRules = map getRuleFromInt [0..255]   

{-  TESTING FOR PART ONE
  
    To check whether the cellular automata works properly, we visualise each rule. 
    We can also check that each rule generated from getRuleFromInt matches with the int passed to the function. 
    This is not required behaviour for the specification, but makes it better for other programmers who might expect this behaviour. 

    These visualisations are also important for checking that each rule generates a visual without error and that they are different rules. 
    To this end, we just check whether they match up to how the rules are meant to look (refer to the wolfram math link at the top)

    The visualisations are also used to check that loopedAt works as detailed below. 

    //  HOW TO RUN/HOW FILES WERE MADE 

    To generate each visualisation, run stack ghci and type in one of the tests (e.g. allRulesMassiveTest).  

    Because it displays the characters as numbers (probably the unicode representations), we have to pipe it through putStrLn to see it properly in the console. 
    An example of this would be:

    putStrLn allRulesSmallTest

    However, this is a very temporary record and the console does not let us scroll back very far. Therefore, we can use writeFile to see it all:

    writeFile "visualisations/AllRulesMassiveTest.txt" allRulesMassiveTest

    I omit including allRulesMassiveTest.txt so you can run the command here to check that it works :)
    Also it's a relatively large file 

    // SPECIFIC EXERCISES TESTED 

    Looped At (Ex 4): As mentioned in the exercise, the looped at test insufficient. 
    We test it by generating all of its output values with the following code in GHCi: 

    [(x, loopedAt (getRuleFromInt x) initialLineState) | x <- [0..255]]

    Using the generated large test and the standard initialLineState (as they use the same width),
    I manually checked for a handful of rules whether loopedAt returns the right value. 
    The testing is insufficient for when the repeat is never the next immediate linestate (e.g. it oscillates between two states)
    so we pay special attention to rules like Rule 1 where one encounters generation 0 (initial line state) at generation 2 (so loopedAt should return 2). 

    applyRule (Ex 3), allRules (Ex 5): We can inspect each rule's output and check whether it matches with how the rules are meant to look. 
    Since they look the same, we can say that both applyRule and allRules functions properly. This is an integration test for most functionality written for this part of the coursework. 

    // BREAKDOWN OF CODE 

    Due to the varying dimensions of the tests, we have to be able to generate different widths for the initial line state. 
    We use just a central cell turned on and all other cells off. We need a cell on for some of the rules to exhibit their behaviour, 
    and the other cells off are so that we can see how certain rules can "spread". 

    Therefore I've written nSizedInitialLineState which constructs a LineState of width (2n + 1) cells. 
    It accomplishes this by creating a list of Off cells of length n and then prepending and appending (both with ++) it to the list of just the On cell. 

    Another key function is visualiseNGenerations which successively applies rule to the state initially passed and prepends using ++. 
    It also newlines between generations so you can see them clearly and how the generations evolve. 
    It uses explicit recursion (base case being 0; when we show the final state achieved).

    The final key function is visualiseAllNGenerations which visualises the generations for each rule, adding a bunch of newlines between each visualisation of a rule
    and also text displaying the rule number. 

    The rest of the functions/constants (including the tests) are based off utilising the above core functions. 
    
    // WHY THE DIMENSIONS OF SMALL, MEDIUM, LARGE, MASSIVE 

    The dimensions were chosen depending on your need. If you wanted to see the behaviour of smaller versions of the automata, 
    then you can use small or medium. 

    If you want to see what the full thing looks like then you use large. Unlike the smaller tests, it also lets you see far more generations
    and therefore lets you see where a lot of the more complicated rules start looping or "end". 

    The massive test is in case you want to see a very large rendering of the image but also serves as a way to torture your computer. 
-}

-- Generates an initial line state of width (2n + 1); has a central cell on and all other cells are off
nSizedInitialLineState :: Int -> LineState
nSizedInitialLineState n = LS (offCells ++ [On] ++ offCells)
  where 
        offCells = replicate n Off

-- Standard initial line state; 101 cell wide initial state 
initialLineState :: LineState 
initialLineState = nSizedInitialLineState 50 

-- Visualise up to N generations of a given rule from an initial line state 
visualiseNGenerations :: Int -> (CellState -> CellState -> CellState -> CellState) -> LineState -> String 
visualiseNGenerations 0 _ lineState = show lineState 
visualiseNGenerations n rule lineState = show lineState ++ "\n" ++ visualiseNGenerations (n-1) rule (applyRule rule lineState) 

-- Visualise up to 50 generations of a given rule (from integer) from the standard initial state; for quick on-the-fly testing
visualiseRule :: Int -> String 
visualiseRule ruleInt = visualiseNGenerations 50 rule initialLineState
  where 
        rule :: (CellState -> CellState -> CellState -> CellState)
        rule = getRuleFromInt ruleInt 

-- Visualise up to N generations from a given initial lineState with the rule number appearing just before the visualisation 
visualiseAllNGenerations :: Int -> LineState -> String 
visualiseAllNGenerations n lineState = concat ["RULE " ++ show x ++ "\n\n\n" ++ visualiseNGenerations n (getRuleFromInt x) lineState ++ "\n\n\n" | x <- [0..255]]

-- standard visualisation for on-the-fly demonstration 
visualiseAllRules :: String 
visualiseAllRules = visualiseAllNGenerations 50 initialLineState

-- small visualisation: 21 cell wide states, 5 generations 
allRulesSmallTest :: String 
allRulesSmallTest = visualiseAllNGenerations 5 smallInitialLineState 
  where smallInitialLineState = nSizedInitialLineState 10 

-- medium visualisation: 81 cell wide states, 20 generations 
allRulesMediumTest :: String 
allRulesMediumTest = visualiseAllNGenerations 20 mediumInitialLineState
  where 
        mediumInitialLineState = nSizedInitialLineState 40

-- large visualisation: 101 cell wide states, 80 generations 
allRulesLargeTest :: String 
allRulesLargeTest = visualiseAllNGenerations 80 largeInitialLineState 
  where 
        largeInitialLineState = nSizedInitialLineState 50 -- same width as the standard initialLineState

-- MASSIVE visualisation: 201 cell wide states, 150 generations 
allRulesMassiveTest :: String 
allRulesMassiveTest = visualiseAllNGenerations 150 massiveInitialLineState 
  where 
        massiveInitialLineState = nSizedInitialLineState 100

{-|
  Ex. 6: Implement initialState, which returns the initial configuration of Langton's Ant.

  Constructs AntState facing West, with position (0, 0), and no black squares. 
-}

initialState :: AntState
initialState = AS West (0,0) Set.empty 

{-|
  Ex. 7: Define the functions leftOf and rightOf, which return the direction to the left and right of the given direction, respectively. Follow the additional constraints given in the specification, and answer the written question here.

  leftOf uses top-level pattern matching. It defines the anti-clockwise rotation. 

  rightOf uses guards and defines the clockwise rotation. 

  I prefer top-level pattern matching here because it's the simplest and most readable solution. It is also probably the fastest. 
  Guards here exposes unnecessary logic with the equality operator (==). It's also a longer solution and takes a bit longer to read. 
  Introducing the variable dir makes it a tiny bit more complicated. 

  Also, the power of guards is just unnecessary. Top-level pattern matching should be used here when it's sufficient. 
  We do not need to perform any operations on any inputs besides equality so it is enough. 

  Guards also requires us to define an otherwise case instead of dir == West which would be more readable. 
  This is because of inexhaustive patterns (since Haskell is not smart enough to see that using the equality operator on all possible values will cover all cases). 
  This adds another thin layer of making-it-harder-to-read.

  In general, I prefer pattern matching when it is enough. 
  If it is clear that there is a more efficient solution or a more extensible solution (for something that is very likely to be extended), then that would be preferred. 
  And of course, prefer something else if it is required (pattern matching is insufficient to accomplish the task). 
-}

leftOf :: Direction -> Direction
leftOf North = West 
leftOf West = South 
leftOf South = East 
leftOf East = North 

rightOf :: Direction -> Direction
rightOf dir 
  | dir == North = East 
  | dir == East = South 
  | dir == South = West 
  | otherwise = North -- i.e. dir == West 


{-|
  Ex. 8: Implement the step function, which takes the ant state and applies the logic given in the specification to return the new ant state.

  Generally, step checks whether the current cell is black. 
  If the current cell is black, then it makes it white by removing it from the list of black cells. 
  If the current cell is white, then it makes it black by adding it to the list of black cells. 
  If the current cell is black, then the left direction is the new direction (using leftOf). 
  If the current cell is white, then the right direction is the new direction (using rightOf). 
  Ant's next position is determined by the ant's new direction (after updating)
  //

  It does this by first deconstructing the current AntState using pattern matching. 
  We then construct a new AntState using the new states: the new direction, the new position, and the new set of black cells 

  We use a where binding to put all the logic for the new direction, new position, and new set of cells to nicely hide away all the horrors from users of the step function 

  We use guards to determine nextDir and nextBlackCells from the isBlack constant (although it gets redeclared each time step is called)
  This is mainly because it's neater than if statements and case statements in my opinion 
  
  nextDir simply determines the next direction based off the fruits of Ex 6 (the leftOf and rightOf functions)

  In nextBlackCells, we use Set.delete which returns the new Set after removing the position of the previously black cell (but is now white)
  We use Set.insert to return the new Set after adding the position of the previously white cell (but is now black hence being a part of the black cells set)

  We determine the next position with the help of the helper function nextPositionWithDirection 
  We just pattern match for all the different directions and add or subtract from x or y depending on the direction:

  NORTH +1y 
  SOUTH -1y

  EAST +1x
  WEST -1x

  /==--==/
  We create isBlack so that we don't have to use member twice because it's relatively expensive (not as expensive as if we were using a list though!)
  //

  We use pattern matching for the helper function nextPositionWithDirection because it is the simplest and neatest way to write it
  We could define more complicated behaviour to achieve this however it would be less readable and probably run slower
  In this case, there's not a huge chance of having to extend this to more dimensions so it is just not worth the tradeoff at all 
-}

step :: AntState -> AntState
step (AS dir pos blackCells) = AS nextDir nextPos nextBlackCells 
  where 
        isBlack :: Bool 
        isBlack = Set.member pos blackCells 

        nextDir :: Direction 
        nextDir 
          | isBlack = leftOf dir 
          | otherwise = rightOf dir  

        nextBlackCells :: Set (Int, Int)
        nextBlackCells 
          | isBlack = Set.delete pos blackCells 
          | otherwise = Set.insert pos blackCells

        nextPos :: (Int, Int)
        nextPos = nextPositionWithDirection pos nextDir

        nextPositionWithDirection :: (Int, Int) -> Direction -> (Int, Int)
        nextPositionWithDirection (x,y) North   = ( x  , y+1 )
        nextPositionWithDirection (x,y) East    = ( x+1, y   )
        nextPositionWithDirection (x,y) West    = ( x-1, y   )
        nextPositionWithDirection (x,y) South   = ( x  , y-1 )      

{-|
  Ex. 9: Visualise the behaviour of Langton's Ant by implementing the "animation" function. It takes a number (the step) and you must return a picture that represents the state of the automaton at that step.

  There are no tests for this. You can use `stack run` to test your implementation.

  Generally, animation works by getting state from getStateAtN and then simply running a bunch of helper functions on that state to render the ant and squares. 

  // State 

  getStateAtN uses explicit recursion and has a base case of 0 (the initial state). For all other cases, we recurse downwards to the base case:
  To get the current state, we can consider it the previous state with the step function applied. 
  We used function composition here instead of multiple function applications ($) because it's more compact. 
  We also use the function application operator because brackets are a sin among programmers (for clarity)

  We get the state from getStateAtN and we deconstruct it using pattern matching so that we can access currentDir, currentPos, currentBlackCells. 
  Now that we have access to the state, we have the joy of visualising it. 

  // Drawing Ant 

  We need the position and direction of the ant to draw it so we receive these parameters. We need to rotate the ant image, and then move it to its position. 

  We use a where binding to declare the helper function mapDirectionToRotation. We pattern match each cardinal direction to a rotation value. 
  Since the default rotation is North (see the ant asset), North has a value of 0. rotate's angle function accepts ints as degrees which rotate clockwise. 
  Therefore the next clockwise direction (East) has a rotation of 90 (degrees). And so on. 

  We can then rotate the image. Rotate returns an image. After rotating the image, we want to move it so we pass it through offset.
  Because the ant (and each cell) is bigger than one pixel, we have to multiply by the size of a cell, to how much we move it

  // Drawing Cells

  The building block for this is a function to draw each cell to the screen. 
  This is drawBlackCell: we accept a position (as a pair) as a parameter, and create a rectangle of the pixelsPerCell size defined by the constant. 
  Since position is in terms of cells, we need to size the position up by pixelsPerCell. 

  drawBlackCellsFromSet folds over the set of positions to transform each position into an image and combine them all with each other into one image. 
  It does this by passing "blank" as the initial element. This is a kind of identity element for image superimposition: it doesn't do anything.
  This is good because we don't want some random default image. 

  We need a way to actually convert the positions into images and combine them so that foldr can do its magic. 
  Therefore, we use a where binding to declare the helper function drawAndCombine. 
  It converts the position into an image and combines it with the other image (either the blank image or the composition of the previous cells).     

  // Main

  Now that we have an image for cells and an image for the ant, we can simply combine them using the superimposition operator <@>. 

  /==--==/

  We could've used an implementation using superimposeAll.
  However, the difficulty with using superimposeAll is that it is only written for lists and not Sets. 
  We could've mapped over Set using Set.map and turned the set of positions into the set of black cell images, and then converted using toList to a list of images. 
  This would've been more confusing and less performant than simply using Set.foldr and drawAndCombine. 

  In other words, doing a bunch of operations to force our set to squeeze into a list would've been a lot more computation than simply folding over a set
  and superimposeAll just folds over a list anyway. It's not quite re-inventing the wheel because superimposeAll was never defined for sets. 
-}

animation :: Int -> Image
animation t =     drawBlackCellsFromSet currentBlackCells
              <@> drawAnt currentPos currentDir
  where (AS currentDir currentPos currentBlackCells) = getStateAtN t

        pixelsPerCell :: Int -- cell size has to fit around ant size 
        pixelsPerCell = 10

        getStateAtN :: Int -> AntState
        getStateAtN 0 = initialState -- base case
        getStateAtN n = step.getStateAtN $ n-1 -- recursive step

        drawBlackCell :: (Int, Int) -> Image 
        drawBlackCell (x,y) = offset (x * pixelsPerCell) (y * pixelsPerCell) $ rect pixelsPerCell pixelsPerCell

        drawBlackCellsFromSet :: Set (Int,Int) -> Image 
        drawBlackCellsFromSet = Set.foldr drawAndCombine blank 
          where 
                drawAndCombine :: (Int, Int) -> Image -> Image 
                drawAndCombine pos image = drawBlackCell pos <@> image

        drawAnt :: (Int, Int) -> Direction -> Image
        drawAnt (x,y) dir = offset (x * pixelsPerCell) (y * pixelsPerCell) $ rotate (mapDirectionToRotation dir) ant 
          where 
              mapDirectionToRotation :: Direction -> Int 
              mapDirectionToRotation North = 0
              mapDirectionToRotation East = 90
              mapDirectionToRotation South = 180
              mapDirectionToRotation West = 270
