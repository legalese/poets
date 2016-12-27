{-# LANGUAGE TupleSections, ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Reporting.Language.Parrot.Typing.Simplify.SubtypeConstraints
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Patrick Bahr, Michael Werk
-- Stability   :  unknown
-- Portability :  unknown
--
-- This is a utility modules to facilitate graph based calculations 
-- on atomic subtype constraints. 
--
--------------------------------------------------------------------------------

module Poets.Reporting.Language.Parrot.Typing.Simplify.SubtypeConstraints 
       ( ASubtypeConstraint
       , Key
       , Node
       , KeyLookup
       , VertexLookup
       , fromSubtypeConstraint
       , splitSubtypings
       , SubtypeOf (..)
       , reaching
       , removeVertex
       , mergeVertices
       , vertices'
       , createGraph
       ) where

import Poets.Reporting.Language.Parrot.Syntax
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicTypes 
import Poets.Reporting.Language.Parrot.Typing.Simplify.AtomicDecomp 

import Data.Maybe
import Data.Graph
import Data.Array
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type ASubtypeConstraint = (AtomicPType, AtomicPType, SrcPos)

type Key = AtomicType
type Node = AtomicPType
type KeyLookup = Key -> Maybe Vertex
type VertexLookup = Vertex -> (Node, Key, [Key])

{-| Convert an atomic subtype constraint back to a atomic constraint -}

fromSubtypeConstraint :: ASubtypeConstraint -> AConstraint a
fromSubtypeConstraint (a, b, p) = ASubType a b :&: p 

{-| This function splits the given list of atimoc constraints into a list of
(atomic) subtyping constraints, and a list of the remaining
constraints. -}

splitSubtypings :: [AConstraint a] -> ([ASubtypeConstraint], [AConstraint a])
splitSubtypings [] = ([],[])
splitSubtypings (c:cs) =
  let (subs, others) = splitSubtypings cs
  in case c of 
    ASubType s t :&: pos -> ((s, t, pos):subs, others)
    _ -> (subs, c:others)


class SubtypeOf d a b where
  -- | Determine if @a@ is a subtype if @b@ in @d@ 
  subtypeOf :: d -> a -> b -> Bool
  
instance SubtypeOf ([ASubtypeConstraint]) AtomicPType AtomicPType where
  subtypeOf cs a b = 
    let a' = atomicDecomp a :: AtomicType
        b' = atomicDecomp b :: AtomicType
    in subtypeOf cs a' b'
  
instance SubtypeOf ([ASubtypeConstraint]) AtomicType AtomicType where
  subtypeOf cs a b = fromMaybe False $ do
    let (graph, _, keyLookup) = graphFromEdges $ nodes cs
    nodeA <- keyLookup a
    nodeB <- keyLookup b
    return $ subtypeOf graph nodeA nodeB
        

instance SubtypeOf Graph Vertex Vertex where
  subtypeOf = path
      

reaching :: Graph -> Vertex -> Set Vertex
reaching g s = Set.fromList $ s : reachable g s


{-| Removes a vertex from a graph by removing the vertex and connecting the vertexes 
from the ingoing edges to the vertexes of the outgoing edges. Inefficient. 
TODO: the vertex is not remove from the underlying array and will be returned by 
'Data.Graph.vertices'. It is however not possible to change the array indices 
as this would require recalculation og the lookup function. Consider uding 
a better graph library-}

removeVertex :: Vertex -> Graph -> Graph
removeVertex vertex graph = buildG (Array.bounds graph) $ [(v, w) | (v, _) <- ingoing, 
                                                           (_, w) <- outgoing] ++ unrelated    
  where
    ingoing = [(v, w) | (v, w) <- edges graph, w == vertex]
    outgoing = [(v, w) | (v, w) <- edges graph, v == vertex]
    unrelated = [(v, w) | (v, w) <- edges graph, w /= vertex, v /= vertex]

{-| merge two vertices in the graph. As with 'removeVertex' the first 
vertex @a@ is actually not removed from the graph but only isolated 
and will therefore be returned by 'Data.Graph.vertices' -}

mergeVertices :: Graph -> Vertex -> Vertex -> Graph
mergeVertices graph a b = 
  let es = edges graph
      es' = filter (flip notElem [(a,b), (b,a)]) es
      mapPair f (a, b) = (f a, f b)
      subst a b x = if (a==x) then b else x
      substPairs = map $ mapPair (subst a b)
  in buildG (Array.bounds graph) (substPairs es')
      

{-| Function that remedies the flow in 'removeVertex'. It only finds 
vertices with in or out going edges -}

vertices' :: Graph -> [Vertex]
vertices' g = [v | v <- vertices g, indegree g ! v /= 0 || outdegree g ! v /= 0]

{-| Creates a directed graph where nodes points to thair super types -} 

createGraph :: [ASubtypeConstraint] -> (Graph, VertexLookup, KeyLookup)
createGraph cs = graphFromEdges (nodes cs)

{-| This function generates the nodes of a graph that represents the
given atomic subtyping constraints. -}

nodes :: [ASubtypeConstraint] -> [(AtomicPType,AtomicType,[AtomicType])]
nodes subs = map trans $ Map.toList $ nodes'  subs
    where trans (t,ts) = (t, atomicDecomp t,ts)


{-| This function generates the nodes of a graph that represents the
given atomic subtyping constraints. -}

nodes' :: [ASubtypeConstraint] -> Map AtomicPType [AtomicType]
nodes' subs = Map.unionWith (++) (Map.fromListWith (++) (map forewardEdge subs))
              (Map.fromList (map noBackwardEdge subs))
               where forewardEdge (s, t, _) = (s, [atomicDecomp t])
                     noBackwardEdge (_,t,_) = (t,[])
                     
