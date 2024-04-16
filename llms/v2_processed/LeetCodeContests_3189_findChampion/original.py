from typing import List

def findChampion(n: int, edges: List[List[int]]) -> int:
    """
    There are n teams numbered from 0 to n - 1 in a tournament; each team is also a node in a DAG.
    You are given the integer n and a 0-indexed 2D integer array edges of length m representing the DAG, where
    edges[i] = [ui, vi] indicates that there is a directed edge from team ui to team vi in the graph.
    A directed edge from a to b in the graph means that team a is stronger than team b and team b is weaker than team a.
    Team a will be the champion of the tournament if there is no team b that is stronger than team a.
    Return the team that will be the champion of the tournament if there is a unique champion, otherwise, return -1.
    """
    # Create a list to keep track of the number of incoming edges for each team
    in_degree = [0] * n
    
    # Count the in-degrees for each node
    for u, v in edges:
        in_degree[v] += 1
    
    # Find the nodes with zero in-degrees
    zero_in_degree = [i for i in range(n) if in_degree[i] == 0]
    
    # If there is exactly one team with zero in-degrees, that's our champion
    if len(zero_in_degree) == 1:
        return zero_in_degree[0]
    else:
        # If there are no teams or more than one with zero in-degrees, return -1
        return -1

# Example usage:
print(findChampion(3, [[0, 1], [1, 2]]))  # Output: 0
print(findChampion(4, [[0, 2], [1, 3], [1, 2]]))  # Output: -1