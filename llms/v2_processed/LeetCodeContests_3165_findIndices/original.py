from typing import List

def findIndices(nums: List[int], indexDifference: int, valueDifference: int) -> List[int]:
    n = len(nums)
    
    # Iterate through all possible pairs of indices (i, j)
    for i in range(n):
        for j in range(n):
            if abs(i - j) >= indexDifference and abs(nums[i] - nums[j]) >= valueDifference:
                return [i, j]
    
    # If no valid pair is found
    return [-1, -1]