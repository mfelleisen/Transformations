from typing import List

def minimumSum(nums: List[int]) -> int:
    """You are given a 0-indexed array nums of integers.
    A triplet of indices (i, j, k) is a mountain if:
     * i < j < k
     * nums[i] < nums[j] and nums[k] < nums[j]
    Return the minimum possible sum of a mountain triplet of nums. If no such triplet exists, return -1.
    """
    n = len(nums)
    if n < 3:
        return -1

    # To store the minimum sum of mountain triplets
    min_sum = float('inf')
    found = False

    # Iterate through each possible 'j' as the peak of the mountain
    for j in range(1, n-1):
        left_min = float('inf')
        right_min = float('inf')

        # Find the smallest number on the left of 'j' such that nums[i] < nums[j]
        for i in range(0, j):
            if nums[i] < nums[j]:
                left_min = min(left_min, nums[i])

        # Find the smallest number on the right of 'j' such that nums[k] < nums[j]
        for k in range(j+1, n):
            if nums[k] < nums[j]:
                right_min = min(right_min, nums[k])

        # If we found valid 'i' and 'k', update the minimum sum
        if left_min != float('inf') and right_min != float('inf'):
            min_sum = min(min_sum, left_min + nums[j] + right_min)
            found = True

    # Return the result
    return min_sum if found else -1

# Example usages
print(minimumSum([8,6,1,5,3]))  # Output: 9
print(minimumSum([5,4,8,7,10,2]))  # Output: 13
print(minimumSum([6,5,4,3,4,5]))  # Output: -1