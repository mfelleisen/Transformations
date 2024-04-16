from typing import List
from collections import Counter

def isGood(nums: List[int]) -> bool:
    """
    Determines if the given array is a "good" array, meaning it is a permutation of
    the base array [1, 2, ..., n - 1, n, n] where n is the maximum element in nums.
    """
    # Getting the maximum element in the list, which will act as 'n'
    n = max(nums)
    # Count the occurrences of each element in nums
    count = Counter(nums)
    
    # Check if all integers from 1 to n-1 are present exactly once, and n is present exactly twice
    for i in range(1, n):
        if count[i] != 1:
            return False
    if count[n] != 2:
        return False
    
    # Ensuring the length matches exactly n + 1
    if len(nums) != n + 1:
        return False
    
    return True

# Example usage:
print(isGood([2, 1, 3]))  # Output: false
print(isGood([1, 3, 3, 2]))  # Output: true
print(isGood([1, 1]))  # Output: true
print(isGood([3, 4, 4, 1, 2, 1]))  # Output: false