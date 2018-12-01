# LGenerics

Collection of generic algorithms and data structures.
It is always under development.

System requirements: FPC 3.1.1 and higher, Lazarus 1.9.0 and higher.
   
Installation and usage:
  1. Open and compile package LGenerics/packages/LGenerics.lpk.
  2. Add LGenerics to project dependencies.

Contains:  
  1. Algorithms on arrays and vectors(mostly unit LGArrayHelpers):
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

  2. Algorithms on graphs:
      - traversals
      - connected components
      - bipartite graph detection
      - articulation points
      - bridges
      - biconnected components
      - edge-connectivity 
      - cycle detection
      - negative cycle detection
      - fundamental cycles 
      - shortest paths 
      - all pairs shortest paths 
      - minimum spanning trees
      - strongly connected components
      - minimum dominating set 
      - maximum independent set
      - listing all maximal independent sets
      - maximum clique 
      - listing all maximal cliques
      - vertex coloring
      - maximum flow through a directed network
      - minimum-cost flow through a directed network
      - minimum s-t cut in a directed network
      - global minimum cut in a weighted graph
      - maximum cardinality matching in bipartite graph
      - maximum cardinality matching in arbitrary graph
      - weighted matching in bipartite graph
      - some TSP approaches

  3. Other algorithms:
      - some non-cryptogarphic hashes(unit LGHash)

  4. Data structures:
     - stack(unit LGStack)
     - queue(unit LGQueue)
     - deque(unit LGDeque)
     - vector(unit LGVector)
     - vector of bits(unit LGVector)
     - priority queue(unit LGPriorityQueue)
     - full featured priority queue with key update and melding (unit LGPriorityQueue)
     - sorted list(unit LGList)
     - hashed list - array based list with the ability to fast search by key (unit LGList)  
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
     - sparse 2d table(unit LGTable2D)
     - disjoint set(unit LGHashSet)
     - sparse undirected graph(unit LGSimpleGraph)
     - sparse undirected weighted graph(unit LGSimpleGraph)
     - sparse directed graph(unit LGSimpleDigraph)
     - sparse directed weighted graph(unit LGSimpleDigraph)
  
  5. Others:
     - simply command line parser (unit LGMiscUtils)
     - brief and dirty implementation of futures concept(unit LGAsync)
     - simplest blocking channel impementation (unit LGAsync)
