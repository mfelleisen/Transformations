from typing import List

def divideArray(nums: List[int], k: int) -> List[List[int]]:
    """
    You are given an integer array nums of size n and a positive integer k.
    Divide the array into one or more arrays of size 3 satisfying the following conditions:
    Each element of nums should be in exactly one array.
    The difference between any two elements in one array is less than or equal to k.
    Return a 2D array containing all the arrays. If it is impossible to satisfy the conditions, return an empty array.
    """
    n = len(nums)
    if n % 3 != 0:
        return []

    nums.sort()
    result = []
    
    for i in range(0, n, 3):
        # Check the subset of three elements
        if nums[i+2] - nums[i] > k:
            return []  # If the condition is not met, return an empty list
        result.append(nums[i:i+3])
    
    return result

# Example usage
nums1 = [1, 3, 4, 8, 7, 9, 3, 5, 1]
k1 = 2
print(divideArray(nums1, k1))  # Output: [[1, 1, 3], [3, 4, 5], [7, 8, 9]]

nums2 = [1, 3, 3, 2, 7, 3]
k2 = 3
print(divideArray(nums2, k2))  # Output: []