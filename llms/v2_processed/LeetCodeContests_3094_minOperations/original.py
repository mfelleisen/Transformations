from typing import List
from collections import Counter

def minOperations(nums: List[int]) -> int:
    """
    Given an array `nums` of positive integers, calculate the minimum number of operations
    to delete all elements using the allowed operations (deleting pairs or triplets of equal values).
    Return -1 if it's impossible to empty the array.
    """
    # Count occurrences of each number
    count = Counter(nums)
    ops = 0

    # Calculate the minimum number of operations
    for num, freq in count.items():
        if freq % 3 == 0:
            ops += freq // 3
        elif freq % 3 == 1:
            if freq >= 4:  # We need at least 4 to perform one triplet removal and one pair removal
                ops += (freq - 4) // 3 + 2
            else:
                return -1
        elif freq % 3 == 2:
            if freq >= 2:  # We need at least 2 to perform one pair removal
                ops += (freq - 2) // 3 + 1
            else:
                return -1
    
    return ops

# Example usage:
print(minOperations([2,3,3,2,2,4,2,3,4]))  # Output: 4
print(minOperations([2,1,2,2,3,3]))  # Output: -1