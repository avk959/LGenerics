## LGenerics

  Collection of generic algorithms and data structures entirely written in/for FPC and Lazarus. 
  Started as a self-education project, it now seems quite comfortable and fast.
  In order to use it (FPC 3.1.1 and higher and Lazarus 1.9.0 and higher):
  
  - open and compile package LGenerics/packages/LGenerics.lpk.
  
  - add LGenerics package to project dependencies.
#### Implemented primitives:
  - stack(unit LGStack)
  - queue(unit LGQueue)
  - deque(unit LGDeque)
  - vector(unit LGVector)
  - vector of bits(unit LGVector)
  - priority queue based on binary heap(unit LGPriorityQueue)
  - priority queue with key update and melding based on pairing heap(unit LGPriorityQueue)
  - sorted list(unit LGList)
  - hashed list - array based list with the ability to fast search by key(unit LGList)  
  - hashset(unit LGHashSet)
  - sorted set(unit LGTreeSet)
  - hash multiset(unit LGHashMultiSet)
  - sorted multiset(unit LGTreeMultiSet)
  - hashmap(unit LGHashMap)
  - sorted map(unit LGTreeMap)
  - hash multimap(unit LGMultiMap)
  - tree multimap(unit LGMultiMap)
  - list miltimap(unit LGMultiMap)
  - bijective map(unit LGBiMap)
  - sparse 2D table(unit LGTable2D)
  - disjoint set(unit LGHashSet)
  - sparse labeled undirected graph(unit LGSimpleGraph)
  - sparse labeled directed graph(unit LGSimpleDigraph)

  **features**:
  - extended IEnumearble interface
  - *lite* containers based on advanced records
#### Implemented graph features:
  - core functions:
    + vertices/edges addition/removal/query/enumeration, edge contraction, degree
    + load/save to own binary format, primitive export to DOT format
  - connectivity:
    + connected/strongly connected components, bipartite detection 
    + articulation points, bridges, biconnected components 
    + edge-connectivity 
  - traversals:
    + BFS/DFS traversals with visitors, 
    + cycle/negative cycle detection, 
    + topological sort
  - operations: 
    + induced subgraphs, complement, reverse, union, intersect, symmetric difference,
  - distance within graph: 
    + eccentricity, radius, diameter, center, periphery 
  - matching:
    + maximum cardinality matching on bipartite/arbitrary graphs  
    + minimum/maximum weight matching on bipartite graphs
  - some suggestions for NP-hard problems: 
    + maximum independent set, independent sets enumeration 
    + maximum clique, cliques enumeration
    + vertex coloring, approximations and exact
    + minimum dominating set
    + Hamiltonian cycles and paths
    + local search TSP approximations, BnB TSP solver
  - minimum spanning trees: Prims's and Kruskal's algorithms
  - single source shortest paths: 
    + Dijkstra with pairing heap, A*, Bellman-Ford-Moor with Tarjan's subtree disassembly(BFMT)
  - all pairs shortest paths: 
    + Floyd–Warshall, Johnson, BFMT  
  - networks:
    + maximum flow: push/relabel, capacity scaling Dinitz
    + minimum-cost flow: Busacker-Gowen, cost scaling push/relabel algorithm
    + global minimum cut: Stoer–Wagner, Nagamochi-Ibaraki    
#### Algorithms on arrays and vectors(mostly unit LGArrayHelpers):
  - reverse, right/left cyclic shifts
  - permutations
  - binary search
  - N-th order statistics
  - distinct values selection
  - quicksort
  - introsort
  - dual pivot quicksort
  - mergesort
  - timsort(unit LGMiscUtils)
  - counting sort
  - ...
##### Other:
  - non-cryptogarphic hashes(unit LGHash):
    + Yann Collet's xxHash32, xxHash64
    + Austin Appleby's MurmurHash2, MurmurHash2A, MurmurHash3_x86_32, MurmurHash64A
  - brief and dirty implementation of futures concept(unit LGAsync)