{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.Tracer
  ( initDefaultTracers
  ) where

import           "contra-tracer" Control.Tracer (Tracer(..))

import           Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map
import           Data.Text as Text

import           Cardano.Api
import           Cardano.Logging

import           Cardano.BM.Data.Tracer (TracingVerbosity(..), toObject)

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.LegacyTracer ()
import           Cardano.Benchmarking.Types

initDefaultTracers :: IO BenchTracers
initDefaultTracers = do
  t <-  standardTracer
  benchTracer <- appendName "benchmark" <$> machineFormatter (Just "cardano") t
  n2nSubmitTracer <- appendName "submitN2N" <$> machineFormatter (Just "cardano") t
  connectTracer <- appendName "connect" <$> machineFormatter (Just "cardano") t
  submitTracer <- appendName "submit" <$> machineFormatter (Just "cardano") t
  lowLevelSubmitTracer <- appendName "llSubmit" <$> machineFormatter (Just "cardano") t

  configureTracers initialTraceConfig logsDocumented [t]
  traceWith t $ FormattedHuman False "logging start up h"
  traceWith benchTracer $ TraceBenchTxSubDebug "logging start up m1"
  traceWith benchTracer $ TraceBenchTxSubDebug "logging start up m2"
  traceWith benchTracer $ TraceBenchTxSubDebug "logging start up m3"
  return $ BenchTracers
    { btTxSubmit_    = Tracer $ traceWith benchTracer
    , btConnect_     = Tracer $ traceWith connectTracer
    , btSubmission2_ = Tracer $ traceWith submitTracer
    , btLowLevel_    = Tracer $ traceWith lowLevelSubmitTracer
    , btN2N_         = Tracer $ traceWith n2nSubmitTracer
    }

logsDocumented :: Documented FormattedMessage
logsDocumented = Documented
  [ emptyDoc "benchmark"
  , emptyDoc "submitN2N"
  , emptyDoc "connect"
  , emptyDoc "submit"
  , emptyDoc "llSubmit"
  ]
  where
    emptyDoc :: Text -> DocMsg FormattedMessage
    emptyDoc tr = DocMsg [ tr ] [] "ToDo: write benchmark tracer docs"

initialTraceConfig :: TraceConfig
initialTraceConfig = TraceConfig {
      tcOptions = Map.fromList
          [ ([], [ConfSeverity (SeverityF Nothing)])
          , initConf "benchmark"
          , initConf "submitN2N"
          , initConf "connect"
          , initConf "submit"
          , (["llSubmit"], [ConfDetail DMinimal])
          ]
    , tcForwarder = defaultForwarder
    , tcNodeName = Nothing
    , tcPeerFrequency = Just 2000 -- Every 2 seconds
    , tcResourceFrequency = Just 1000 -- Every second
    }
  where
    initConf :: Text -> (Namespace, [ConfigOption])
    initConf tr = ([tr], [ConfDetail DMaximum])

instance LogFormatting (TraceBenchTxSubmit TxId) where
  forHuman = Text.pack . show
  forMachine DMinimal _ = mempty
  forMachine DNormal t = case t of
    TraceBenchTxSubRecv _      -> mconcat ["kind" .= A.String "TraceBenchTxSubRecv"]
    TraceBenchTxSubStart _     -> mconcat ["kind" .= A.String "TraceBenchTxSubStart"]
    TraceBenchTxSubServAnn _   -> mconcat ["kind" .= A.String "TraceBenchTxSubServAnn"]
    TraceBenchTxSubServReq _   -> mconcat ["kind" .= A.String "TraceBenchTxSubServReq"]
    TraceBenchTxSubServAck _   -> mconcat ["kind" .= A.String "TraceBenchTxSubServAck"]
    TraceBenchTxSubServDrop _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServDrop"]
    TraceBenchTxSubServOuts _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServOuts"]
    TraceBenchTxSubServUnav _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServUnav"]
    TraceBenchTxSubServFed _ _ -> mconcat ["kind" .= A.String "TraceBenchTxSubServFed"]
    TraceBenchTxSubServCons _  -> mconcat ["kind" .= A.String "TraceBenchTxSubServCons"]
    TraceBenchTxSubIdle        -> mconcat ["kind" .= A.String "TraceBenchTxSubIdle"]
    TraceBenchTxSubRateLimit _ -> mconcat ["kind" .= A.String "TraceBenchTxSubRateLimit"]
    TraceBenchTxSubSummary _   -> mconcat ["kind" .= A.String "TraceBenchTxSubSummary"]
    TraceBenchTxSubDebug _     -> mconcat ["kind" .= A.String "TraceBenchTxSubDebug"]
    TraceBenchTxSubError _     -> mconcat ["kind" .= A.String "TraceBenchTxSubError"]
  forMachine DDetailed t = forMachine DMaximum t
  forMachine DMaximum t = case t of
    TraceBenchTxSubRecv txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubRecv"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubStart txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubStart"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServAnn txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServAnn"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServReq txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServReq"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServAck txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServAck"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServDrop txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServDrop"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServOuts txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServOuts"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServUnav txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServUnav"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubServFed txIds ix ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServFed"
              , "txIds" .= toJSON txIds
              , "index" .= toJSON ix
              ]
    TraceBenchTxSubServCons txIds ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubServCons"
              , "txIds" .= toJSON txIds
              ]
    TraceBenchTxSubIdle ->
      mconcat [ "kind" .= A.String "TraceBenchTxSubIdle"
              ]
    TraceBenchTxSubRateLimit limit ->
      mconcat [ "kind"  .= A.String "TraceBenchTxSubRateLimit"
              , "limit" .= toJSON limit
              ]
    TraceBenchTxSubSummary summary ->
      mconcat [ "kind"    .= A.String "TraceBenchTxSubSummary"
              , "summary" .= toJSON summary
              ]
    TraceBenchTxSubDebug s ->
      mconcat [ "kind" .= A.String "TraceBenchTxSubDebug"
              , "msg"  .= A.String (Text.pack s)
              ]
    TraceBenchTxSubError s ->
      mconcat [ "kind" .= A.String "TraceBenchTxSubError"
              , "msg"  .= A.String s
              ]

instance LogFormatting NodeToNodeSubmissionTrace where  
  forHuman = Text.pack . show
  forMachine _verb = \case
    ReqIdsBlocking (Ack ack) (Req req) -> KeyMap.fromList
      [ "kind" .= A.String "ReqIdsBlocking"
      , "ack"  .= A.toJSON ack
      , "req"  .= A.toJSON req ]
    IdsListBlocking sent -> KeyMap.fromList
      [ "kind" .= A.String "IdsListBlocking"
      , "sent" .= A.toJSON sent ]
    ReqIdsPrompt (Ack ack) (Req req) -> KeyMap.fromList
      [ "kind" .= A.String "ReqIdsPrompt"
      , "ack"  .= A.toJSON ack
      , "req"  .= A.toJSON req ]
    IdsListPrompt sent -> KeyMap.fromList
      [ "kind" .= A.String "IdsListPrompt"
      , "sent" .= A.toJSON sent ]
    EndOfProtocol -> KeyMap.fromList [ "kind" .= A.String "EndOfProtocol" ]
    ReqTxs req -> KeyMap.fromList
       [ "kind" .= A.String "ReqTxs"
       , "req"  .= A.toJSON req ]
    TxList sent -> KeyMap.fromList
       [ "kind" .= A.String "TxList"
       , "sent" .= A.toJSON sent ]

mapVerbosity :: DetailLevel -> TracingVerbosity
mapVerbosity v = case v of
  DMinimal  -> MinimalVerbosity
  DNormal   -> NormalVerbosity
  DDetailed -> MaximalVerbosity
  DMaximum  -> MaximalVerbosity

instance LogFormatting SendRecvConnect where
  forHuman = Text.pack . show
  forMachine v t = toObject (mapVerbosity v) t

instance LogFormatting SendRecvTxSubmission2 where
  forHuman = Text.pack . show
  forMachine v t = toObject (mapVerbosity v) t

instance LogFormatting TraceLowLevelSubmit where
  forHuman = Text.pack . show
  forMachine v t = toObject (mapVerbosity v) t
