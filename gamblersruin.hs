import System.Random
import Control.Applicative
import Control.Monad

newtype State s a = State { runState :: s -> (a,s) }

type RandState a = State StdGen a

instance Functor (State s) where
    fmap f ma = State $ \s ->
        let (a,t) = runState ma s
        in (f a,t)

instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    af <*> aa = State $ \s ->
        let (f,t) = runState af s
            (a,u) = runState aa t
        in (f a, u)

instance Monad (State s) where
    return = pure
    ma >>= f = State $ \s ->
        let (a,t) = runState ma s
            mb = f a
            (b,u) = runState mb t
        in (b,u)

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put t = State $ \s -> ((),t)

runRandom :: RandState a -> StdGen -> a
runRandom (State f) s = res
    where (res, state) = f s

randR :: Random a => (a, a) -> RandState a
randR (a,b) = do
    oldGenerator <- get
    let (x, newGenerator) = randomR (a,b) oldGenerator
    put newGenerator
    return x

gambleTrial :: Int -> Int -> RandState Bool
gambleTrial 100 p = return True
gambleTrial 0 p = return False
gambleTrial k p = do
    val <- randR (1,100)
    let result = if (val <= p)
        then gambleTrial (k+1) p
        else gambleTrial (k-1) p
    result

gambleTrials :: Int -> RandState Bool -> RandState Int
gambleTrials n trial = do
    boolList <- (replicateM n trial)
    let result = (sum $ fmap (fromEnum) boolList)
    return result

gamblersRuin :: (Int, Int) -> IO ()
gamblersRuin (k,p) = do
    let n = 500
    gen <- newStdGen
    let r = gambleTrials n (gambleTrial k p)
    let successes = fromIntegral (runRandom r gen) :: Double
    let chance = (successes / (fromIntegral n :: Double)) * 100
    let goodChance = chance >= 50
    let advice = if (goodChance)    then "Play."
                                    else "Do Not Play."
    putStrLn $ advice ++ " The probability of you winning is: " ++ (show chance) ++ "%"
