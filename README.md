## LGenerics

  Collection of generic algorithms and data structures entirely written in/for FPC and Lazarus. 
  Started as a self-education project, it now seems quite comfortable and fast.
  In order to use it (FPC 3.2 and higher and Lazarus 1.9.0 and higher):
  
  - open and compile package lgenerics/LGenerics.lpk.
  
  - add LGenerics package to project dependencies.
#### Implemented primitives:
  - stack(unit lgStack)
  - queue(unit lgQueue)
  - deque(unit lgDeque)
  - vector(unit lgVector)
  - vector of bits(unit lgVector)
  - priority queue based on binary heap(unit lgPriorityQueue)
  - priority queue with key update and melding based on pairing heap(unit lgPriorityQueue)
  - sorted list(unit lgList)
  - hashed list - array based list with the ability to fast search by key(unit lgList)  
  - hashset(unit lgHashSet)
  - fine-grained concurrent hashset(unit lgHashSet)
  - sorted set(unit lgTreeSet)
  - set of arbitrary size(unit lgUtil, TGSet)
  - hash multiset(unit lgHashMultiSet)
  - fine-grained concurrent hashmultiset(unit lgHashMultiSet)
  - sorted multiset(unit lgTreeMultiSet)
  - hashmap(unit lgHashMap)
  - fine-grained concurrent hashmap(unit lgHashMap)
  - sorted map(unit lgTreeMap)
  - hash multimap(unit lgMultiMap)
  - tree multimap(unit lgMultiMap)
  - list miltimap(unit lgMultiMap)
  - bijective map(unit lgBiMap)
  - sparse 2D table(unit lgTable2D)
  - disjoint set(unit lgHashSet)
  - AVL tree(unit lgAvlTree)
  - red-black tree(unit lgRbTree)
  - some treap variants(unit lgTreap)
  - general rooted tree(unit lgRootTree)
  - sparse labeled undirected graph(unit lgSimpleGraph)
  - sparse labeled directed graph(unit lgSimpleDigraph)

  **features**:
  - extended IEnumearble interface - filtering, mapping, etc.
  - *lite* containers based on advanced records
#### Implemented graph features:
  - core functions:
    + vertices/edges addition/removal/query/enumeration, edge contraction, degree
    + load/save to own binary format, primitive export to DOT format
  - connectivity:
    + connected/strongly connected components, bipartite detection, degeneracy, k-core
    + articulation points, bridges, biconnected components 
    + edge-connectivity 
  - traversals:
    + BFS/DFS traversals with visitors, 
    + cycle/negative cycle detection, 
    + topological sort
  - operations: 
    + induced subgraphs, complement, reverse, union, intersect, symmetric difference,
  - chordality testing
  - planarity testing: FMR Left-Right Planarity algorithm
  - distance within graph: 
    + eccentricity, radius, diameter, center, periphery 
  - matching:
    + maximum cardinality matching on bipartite/arbitrary graphs  
    + minimum/maximum weight matching on bipartite graphs
  - dominators in flowgraps: simple iterative and Semi-NCA algorithms
  - some suggestions for NP-hard problems: 
    + maximum independent set, maximal independent sets enumeration 
    + maximum clique, cliques enumeration
    + minimum vertex cover, minimal vertex covers enumeration
    + vertex coloring, approximations and exact
    + minimum dominating set
    + Hamiltonian cycles and paths
    + local search TSP approximations, BnB TSP solver
  - minimum spanning trees: Prims's and Kruskal's algorithms
  - single source shortest paths: 
    + Dijkstra with pairing heap, Bellman-Ford-Moor with Tarjan's subtree disassembly(BFMT)
  - single pair shortest paths:
    + Dijkstra with binary heap, BFMT, bidirection Dijkstra, A*, NBA* 
  - all pairs shortest paths: 
    + Floyd–Warshall, Johnson, BFMT  
  - networks:
    + maximum flow: push/relabel, capacity scaling Dinitz
    + minimum-cost flow: Busacker-Gowen, cost scaling push/relabel algorithm
    + global minimum cut: Stoer–Wagner, Nagamochi-Ibaraki    
#### Algorithms on arrays and vectors(mostly unit lgArrayHelpers):
  - reverse, right/left cyclic shifts
  - permutations
  - binary search
  - N-th order statistics
  - inversion counting
  - distinct values selection
  - quicksort
  - introsort
  - dual pivot quicksort
  - mergesort
  - timsort(unit lgMiscUtils)
  - counting sort
  - radix sort
  - translation of Orson Peters' PDQSort algorithm
  - static segment tree
  - longest increasing subsequence
  - ...
#### Algorithms on strings and sequences(units lgStrHelpers, lgSeqUtils)
  - Boyer-Moore string matching algorithm(in Fast Search variant), case sensitive and case insensitive
  - Boyer-Moore-Horspool-Raita algorithm
  - longest common subsequence of two sequences:
    + reducing the LCS problem to LIS
    + Kumar-Rangan algorithm for LCS
    + Myers algorithm for LCS
  - the Levenshtein distance:
    + simple DP algorithm
    + modified Berghel-Roach algorithm
    + Myers bit-vector algorithm with cut-off heuristic
  - LCS distance:
    + Myers algorithm for LCS distance
  - fuzzy string matching(k differences)
    + Ukkonen EDP algorithm
  - fuzzy string matching with preprocessing(something similar to fuzzywuzzy)
#### Other:
  - non-cryptogarphic hashes(unit lgHash):
    + Yann Collet's xxHash32, xxHash64
    + Austin Appleby's MurmurHash2, MurmurHash2A, MurmurHash3_x86_32, MurmurHash64A
  - brief and dirty implementation of futures concept(unit lgAsync)
  - brief channel implementation(unit lgAsync)
  - brief implementation of thread pool(unit lgAsync)
  - 128-bit integers(unit lgInt128)
  - JSON validator/parser/generator(unit lgJson)
  - JSON Patch/Diff(unit lgJson)
  - CSV document processing(unit lgCsvUtils)
  - Eisel-Lemire fast string-to-double conversion algorithm(unit lgJson)
  - Ryū double-to-string conversion algorithm(unit lgJson)
