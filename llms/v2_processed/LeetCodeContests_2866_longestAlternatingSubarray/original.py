from typing import List

def longestAlternatingSubarray(nums: List[int], threshold: int) -> int:
    """
    You are given a 0-indexed integer array nums and an integer threshold.
    Find the length of the longest subarray of nums starting at index l and ending at index r (0 <= l <= r < nums.length) 
    that satisfies the following conditions:
     * nums[l] % 2 == 0
     * For all indices i in the range [l, r - 1], nums[i] % 2 != nums[i + 1] % 2
     * For all indices i in the range [l, r], nums[i] <= threshold
    Return an integer denoting the length of the longest such subarray.
    """

    max_length = 0
    n = len(nums)

    for l in range(n):
        if nums[l] % 2 == 0 and nums[l] <= threshold:
            current_length = 1
            for r in range(l + 1, n):
                if nums[r] <= threshold and nums[r - 1] % 2 != nums[r] % 2:
                    current_length += 1
                else:
                    break
            max_length = max(max_length, current_length)
    
    return max_length

# Example usage:
print(longestAlternatingSubarray([3,2,5,4], 5))  # Output: 3
print(longestAlternatingSubarray([1,2], 2))       # Output: 1
print(longestAlternatingSubarray([2,3,4,5], 4))  # Output: 3