# 3. Choice of Design

* Status: accepted
* Deciders: Fabien Boucher
* Date: 2021-04-12

## Context and Problem Statement

Haskell supports multiple architecture.
Which language features should we use to organize the code.

## Decision Drivers

* Implementation correctness.
* Maintanability.

## Considered Options

* IO Monad.
* Handles.
* Type class contrains, MTL style.
* ReaderT pattern, RIO.
* Algebraic effects.

## Decision Outcome

Chosen option: "MTL type class", because it comes out best (see below).

## IO Monad

Given these primitive functions:

```haskell
searchBugs :: SearchExpr -> IO [Bug]
logMessage :: Text -> IO ()
monocleAmend :: Change -> BugUrl -> IO ()
```

The worker logic can be implemented using:

```haskell
amend :: Bug -> IO ()
amend bug =
  if hasExternalLink bug
    then monocleAmend (changeUrl bug) (bugUrl bug)
    else logMessage ("Bug does not have external link: " <> show bug)

worker :: WorkerConfig -> IO ()
worker conf = do
  logMessage "Start processing"
  bugs <- searchBugs (searchExpr conf)
  mapM_ amend bugs
  logMessage "End processing"
```

* Good, because it is the simplest option.
* Bad, hard to test, implementation is not declarative: the signature does not give enough details.

## Handles

Using an abstract monad and concret handle, we can inject the dependencies.

Given these handles:

```haskell
data MonocleHandle m = MonocleHandle {
  monocleAmend' :: Change -> BugUrl -> m ()
}

data LogHandle m = LogHandle {
  logMessage' :: Text -> m ()
}
```

The worker logic can be implemented using:

```haskell
amend' :: Monad m => MonocleHandle m -> LogHandle m -> Bug -> m ()
amend' handleMonocle handleLog bug =
  if hasExternalLink bug
    then (monocleAmend' handleMonocle) (changeUrl bug) (bugUrl bug)
    else (logMessage' handleLog) ("Bug does not have external link: " <> show bug)
```

Can be used like so:

```haskell
mainAmend = amend' (MonocleHandle monocleAmend) (LogHandle logMessage)
```

And in test:

```haskell
testAmend = amend' (MonocleHandle mock) (LogHandle writer) exampleBug
```

* Good, because the implementation is confined to the available handlers.
* Bad, it's tedious to thread the handle down the call stack.

* Reference: https://jaspervdj.be/posts/2018-03-08-handle-pattern.html

## Type class constrain, MTL style

Given these type classes:

```haskell
class Monad m => MonadMonocle m where
  monocleAmend'' :: Change -> BugUrl -> m ()

class Monad m => MonadLog m where
  logMessage'' :: Text -> m ()
```

The worker logic can be implemented using:

```haskell
amend'' :: (MonadMonocle m, MonadLog m) => Bug -> m ()
amend'' bug =
  if hasExternalLink bug
    then monocleAmend'' (changeUrl bug) (bugUrl bug)
    else logMessage'' ("Bug does not have external link: " <> show bug)
```

And interpreted with:

```haskell
instance MonadMonocle IO where
  monocleAmend'' = monocleAmend

instance MonadLog IO where
  logMessage'' = logMessage
```

* Good, the dependencies are provided implicitely
* Bad, it's tricky to implement non IO implementation.

* References: https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/

## ReaderT pattern

Use the RIO monad to inject dependencies with the ReaderT pattern.

* References: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/

## Algebraic effect

See polysemy or fused-effect, where effects are given like so:

```
amend :: (Has (Reader Monocle) sig m, Has (Log Text) sig m) => Bug -> m ()
```


> This document is a literate Haskell file,
> Here are the required defitions to validate the above snippet

```haskell
-- Types
type Text = String
type Change = Text
type BugUrl = Text
type SearchExpr = [Text]

data Bug = Bug { hasExternalLink :: Bool, changeUrl :: Change, bugUrl :: BugUrl }
  deriving Show

data WorkerConfig = WorkerConfig { searchExpr :: SearchExpr }

-- Fake definition
searchBugs _searchExpr = pure []
logMessage msg = putStrLn msg
monocleAmend _ _ = pure ()
exampleBug = Bug True "https://review.opendev.org/42" "RHBZ#23"
mock :: Change -> BugUrl -> Maybe ()
mock = undefined
writer = undefined

-- A test entry point
main :: IO ()
main = do
  putStrLn "IO test"
  amend exampleBug
  putStrLn "Handle test"
  mainAmend exampleBug
  putStrLn "MTL test"
  amend'' exampleBug
```
