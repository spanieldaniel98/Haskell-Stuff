module HamiltonianPaths where 

import Data.List
import Data.Maybe
import RemoveElem
import ContainsDuplicates
import IsCycle

-- return a list of Hamiltonian paths in a list of lists representing a graph of nodes and edges therefrom
hamiltonianPaths :: [[Int]] -> [[Int]]
hamiltonianPaths graph = filter (\x -> length x == length graph && (not.containsDuplicates) x) (possiblePaths graph)

possiblePaths :: [[Int]] -> [[Int]]
possiblePaths graph = concat [ paths [[startingVertexNum]] startingVertex graph | startingVertex <- graph, 
                               let startingVertexNum = fromJust $ elemIndex startingVertex graph ]

paths :: [[Int]] -> [Int] -> [[Int]] -> [[Int]]
paths pathList [] graph = pathList
paths pathList currentVertex graph = 
    concat [ paths (map (++[nextVertexNum]) pathList) nextVertex updatedGraph | nextVertexNum <- currentVertex, 
                                                                                let nextVertex = (graph !! nextVertexNum)
                                                                                    updatedGraph = (removeElem nextVertexNum currentVertex):(removeElem currentVertex graph) ]  
                                              
--take in 0-indexed list like e.g. [[2,3],[1,0],[4,1],[0]]
--try each vertex as starting vertex
--for all edges therefrom, go to that edge and add it to each path in the pathlist, deleting from original graph
--add vertex to path

--if vertex appears twice in list, list is invalid
--if a path has one of each vertex in list, stop
--return all paths

-- return a list of Hamiltonian cycles in a list of lists representing a graph of nodes and edges therefrom

hamiltonianCycles :: [[Int]] -> [[Int]]
hamiltonianCycles graph = filter isCycle (hamiltonianPaths graph)