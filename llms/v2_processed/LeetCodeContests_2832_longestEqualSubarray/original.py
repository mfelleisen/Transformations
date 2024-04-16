from typing import List
from collections import defaultdict

def longestEqualSubarray(nums: List[int], k: int) -> int:
    """
    Return the length of the longest possible equal subarray after deleting at most k elements from nums.
    """
    max_length = 0
    left = 0
    count = defaultdict(int)
    
    for right in range(len(nums)):
        count[nums[right]] += 1
        
        # Maximum frequency of any number in the window
        max_freq = max(count.values())
        
        # Length of the current window
        window_length = right - left + 1
        
        # If the current window size minus the max frequency is greater than k,
        # it means we need to delete more than k elements to make all elements equal
        # so we shrink the window from the left
        if window_length - max_freq > k:
            count[nums[left]] -= 1
            left += 1
        
        # Update the max_length if the current window can be made equal by deleting at most k elements
        max_length = max(max_length, window_length - (window_length - max_freq))
    
    return max_length

# Example usage:
nums1 = [1,3,2,3,1,3]
k1 = 3
print(longestEqualSubarray(nums1, k1))  # Output: 3

nums2 = [1,1,2,2,1,1]
k2 = 2
print(longestEqualSubarray(nums2, k2))  # Output: 4