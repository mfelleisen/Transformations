from typing import List

def countPairs(nums: List[int], target: int) -> int:
    """
    Given a 0-indexed integer array nums of length n and an integer target, return the number of pairs (i, j) where
    0 <= i < j < n and nums[i] + nums[j] < target.
    
    :param nums: List of integers.
    :param target: Integer target value.
    :return: Number of pairs (i, j) such that nums[i] + nums[j] < target.
    """
    count = 0
    n = len(nums)
    for i in range(n):
        for j in range(i + 1, n):
            if nums[i] + nums[j] < target:
                count += 1
    return count

# Example usage:
nums1 = [-1, 1, 2, 3, 1]
target1 = 2
print(countPairs(nums1, target1))  # Output: 3

nums2 = [-6, 2, 5, -2, -7, -1, 3]
target2 = -2
print(countPairs(nums2, target2))  # Output: 10