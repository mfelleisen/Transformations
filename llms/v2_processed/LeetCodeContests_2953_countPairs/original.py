from typing import List

def countPairs(coordinates: List[List[int]], k: int) -> int:
    """
    You are given a 2D integer array coordinates and an integer k, where coordinates[i] = [xi, yi] are the coordinates 
    of the ith point in a 2D plane. We define the distance between two points (x1, y1) and (x2, y2) as (x1 XOR x2) + 
    (y1 XOR y2) where XOR is the bitwise XOR operation. Return the number of pairs (i, j) such that i < j and the 
    distance between points i and j is equal to k.
    """
    count = 0
    for i in range(len(coordinates)):
        for j in range(i + 1, len(coordinates)):
            x1, y1 = coordinates[i]
            x2, y2 = coordinates[j]
            if (x1 ^ x2) + (y1 ^ y2) == k:
                count += 1
    return count