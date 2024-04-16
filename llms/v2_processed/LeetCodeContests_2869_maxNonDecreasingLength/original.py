from typing import List

def maxNonDecreasingLength(nums1: List[int], nums2: List[int]) -> int:
    n = len(nums1)
    dp1 = [1] * n  # dp1[i] is the max length ending with nums1[i]
    dp2 = [1] * n  # dp2[i] is the max length ending with nums2[i]
    
    max_len = 1  # Initialize to 1 since each element alone is a non-decreasing subarray
    
    for i in range(1, n):
        if nums1[i] >= nums1[i-1]:
            dp1[i] = max(dp1[i], dp1[i-1] + 1)
        if nums1[i] >= nums2[i-1]:
            dp1[i] = max(dp1[i], dp2[i-1] + 1)
        if nums2[i] >= nums1[i-1]:
            dp2[i] = max(dp2[i], dp1[i-1] + 1)
        if nums2[i] >= nums2[i-1]:
            dp2[i] = max(dp2[i], dp2[i-1] + 1)
        
        # Keep track of the maximum length found so far
        max_len = max(max_len, dp1[i], dp2[i])
    
    return max_len

# Example usage
nums1 = [2, 3, 1]
nums2 = [1, 2, 1]
print(maxNonDecreasingLength(nums1, nums2))  # Output: 2

nums1 = [1, 3, 2, 1]
nums2 = [2, 2, 3, 4]
print(maxNonDecreasingLength(nums1, nums2))  # Output: 4

nums1 = [1, 1]
nums2 = [2, 2]
print(maxNonDecreasingLength(nums1, nums2))  # Output: 2