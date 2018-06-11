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
      - connected components finding
      - bipartite graph detection
      - articulation points finding
      - bridges finding
      - biconnected components finding
      - cycle detection
      - fundamental cycles selection
      - shortest path finding
      - minimum spanning tree finding 

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
     - disjoint set(unit LGraphUtils)
     - sparse undirected graph(unit LGSimpleGraph)
     - sparse undirected weighed graph(unit LGSimpleGraph)

  
  5. Others:
     - simply command line parser (unit LGMiscUtils)
     - brief and dirty implementation of futures concept(unit LGAsync)
     - simplest blocking channel impementation (unit LGAsync)
