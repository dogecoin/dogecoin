{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import           Prelude hiding (id)

import           Data.Aeson
import           Data.Function ((&))
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Printing as G
import           Data.List (tails)
import           Data.Maybe (isJust)
import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import           GHC.Generics

import           Options.Applicative

import qualified System.IO as IO


data TopoParams = TopoParams
  { tpSize      :: Int
  , tpLocations :: [Location]
  , tpIdPools   :: Int -> Maybe Int
  }

data Spec = Spec
  { id     :: Int
  , loc    :: Location
  , mpools :: Maybe Int
  , links  :: [Int]
  }
  deriving (Generic, Show)

data Location
  = LO | AP | EU | US
  deriving (Bounded, Eq, Enum, Ord, Read, Show)

mkTopology :: TopoParams -> [Spec]
mkTopology TopoParams{..} =
  concat phase3
 where
   -- Assign locations and pool counts;  set initial links.
   phase0 = zipWith mkInitial specIds specLocs
   -- Split into per-location lists.
   phase1 = [ filter ((== l) . loc) phase0
            | l <- tpLocations ]
            & filter (not . null)
   -- Establish intra-location connections.
   phase2 = intraConnect <$> phase1
   -- Establish inter-location connections.
   phase3 = if length phase2 > 1
            then interConnect phase2
            else phase2

   interConnect :: [[Spec]] -> [[Spec]]
   interConnect xss =
     case nlocs of
       0 -> xss
       1 -> xss
       _ -> take nlocs $
            fmap linker (tails $ cycle xss)
    where
      nlocs = length xss
      linker (xs:xss') =
        [ x { links = ids <> links x }
        | (x, i) <- zip xs [0..]
        , let ids = idOf i <$> rings ]
       where rings = take (nlocs - 1) $ cycle <$> xss'
             idOf n xs' = id (xs' !! n)
      linker [] = error "Invariant failure: empty list of specs"

   intraConnect :: [Spec] -> [Spec]
   intraConnect specs =
     case len of
       0 -> []
       1 -> specs
       2 -> connect 1
            specs
       _ -> connect 1                             -- next
          $ connect (len - 1)                     -- prev
          $ if len < 6
            then specs
            else connect (len `div` 3)            -- chord 1
               $ if len < 9
                 then specs
                 else connect ((len * 2) `div` 3) -- chord 2
                      specs
    where
      len = length specs
      connect :: Int -> [Spec] -> [Spec]
      connect offt xs =
          take (length xs) $
          fmap linker (tails ring)
        where linker (x:xs') =
                x { links = idOf (offt - 1) xs'
                          : links x }
              linker [] = error "Invariant failure: empty list of specs"
              ring = cycle xs
              idOf n xs' = id (xs' !! n)

   mkInitial :: Int -> Location -> Spec
   mkInitial id loc =
     Spec{ links = []
         , mpools = tpIdPools id
         , ..}
   specIds = [0..(tpSize - 1)]
   specLocs = take tpSize $ cycle tpLocations

main :: IO ()
main = do
  (topoParams, topoJson, topoDot) <- execParser opts

  let topoSpec = mkTopology topoParams
      topo = mkNode <$> topoSpec

  writeTopo topo topoJson
  maybe (pure ()) (writeDot  topoSpec) topoDot
 where
   cliParser :: Parser (TopoParams, FilePath, Maybe FilePath)
   cliParser =
     (,,) <$> topoParamsParser
          <*> strOption
             ( long "topology-output"
            <> help "Topology file to write"
            <> metavar "OUTFILE" )
          <*> optional
              (strOption
              ( long "dot-output"
              <> help "Dot file to write"
              <> metavar "OUTFILE" ))

   topoParamsParser = TopoParams
      <$> option auto
      ( long "size"
        <> metavar "SIZE"
        <> help "Node count" )
      <*> some
      (option auto
        ( long "loc"
          <> help "Region (at least one)"
          <> metavar "LOCNAME" ))
      <*> pure defaultRoleSelector

   defaultRoleSelector = \case
      0 -> Nothing -- BFT node has no pools
      1 -> Just 1  -- Regular pools have just 1 pool
      _ -> Just 2  -- Dense pools have any amount >1 as marker

   opts = info (cliParser <**> helper)
     ( fullDesc
    <> progDesc "Cardano topology generator"
    <> header "make-topology - generate Cardano node topologies" )

--- * To JSON topology
---
writeTopo :: [Node] -> FilePath -> IO ()
writeTopo topo f =
  IO.withFile f IO.WriteMode $ \hnd ->
    LBS.hPutStrLn hnd . encode $ Topology topo []

mkNode :: Spec -> Node
mkNode Spec{..} = Node{..}
  where
    name = idName nodeId
    org = "IOHK"
    nodeId = id
    pools = mpools
    stakePool = isJust mpools
    region = locationRegion loc
    producers = idName <$> links

data Topology = Topology
  { coreNodes  :: [Node]
  , relayNodes :: [Node]
  }
  deriving (Generic, Show)

data Node = Node
  { name      :: String
  , org       :: String
  , region    :: String
  , producers :: [String]
  , nodeId    :: Int
  , pools     :: Maybe Int
  , stakePool :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Topology
instance ToJSON Node where
  toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

--- * To Graphviz
---
writeDot :: [Spec] -> FilePath -> IO ()
writeDot topo f =
  IO.withFile f IO.WriteMode $ \hnd ->
    T.hPutStrLn hnd $
      G.renderDot $ G.toDot $
        uncurry (G.graphElemsToDot params) (toGV topo)
 where
   params = G.nonClusteredParams
     { G.globalAttributes =
       [ G.GraphAttrs
         [G.Scale $ G.DVal 5]
       ]
     , G.fmtNode =
       \(_, Spec{..})->
         [ G.FillColor . G.toColorList . (:[]) $
           case id of
             0 -> G.RGB 250 250 150
             1 -> G.RGB 150 250 250
             _ -> locationColor loc
         , G.Style [G.SItem G.Filled []]
         ]
     }

toGV :: [Spec] -> ([(String, Spec)], [(String, String, String)])
toGV xs =
  (,) ((\s@Spec{..} -> ("node-" <> show id, s)) <$> xs)
      (concat $
       (\Spec{..} -> ("node-" <> show id, , "")
                     . ("node-" <>)
                     . show <$> links) <$> xs)

--- * Aux
---
idName :: Int -> String
idName = ("node-" <>) . show

locationRegion :: Location -> String
locationRegion = \case
  EU -> "eu-central-1"
  AP -> "ap-southeast-2"
  US -> "us-east-1"
  LO -> "loopback"

locationColor :: Location -> G.Color
locationColor = \case
  AP -> G.RGB 250 200 200
  EU -> G.RGB 200 200 250
  US -> G.RGB 200 250 200
  LO -> G.RGB 200 200 250
