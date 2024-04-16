from typing import List
from collections import defaultdict

def maxSum(nums: List[int], m: int, k: int) -> int:
    """
    You are given an integer array nums and two positive integers m and k.
    Return the maximum sum out of all almost unique subarrays of length k of nums. 
    If no such subarray exists, return 0.
    A subarray of nums is almost unique if it contains at least m distinct elements.
    A subarray is a contiguous non-empty sequence of elements within an array.
    """
    # Base case check
    if len(nums) < k:
        return 0
    
    max_sum = 0
    # Sliding window to maintain a window of size k
    window_sum = sum(nums[:k])
    distinct_count = len(set(nums[:k]))

    # Use a dictionary to count occurrences of elements in the current window
    freq = defaultdict(int)
    for num in nums[:k]:
        freq[num] += 1

    # Function to check if the window has at least m distinct elements
    def has_at_least_m_distinct(freq, m):
        count = sum(1 for v in freq.values() if v > 0)
        return count >= m

    # First window check
    if has_at_least_m_distinct(freq, m):
        max_sum = max(max_sum, window_sum)
    
    # Process the rest of the array
    for i in range(k, len(nums)):
        # Slide the window, remove the element going out and add the element coming in
        outgoing = nums[i - k]
        incoming = nums[i]
        
        # Update the window sum
        window_sum = window_sum - outgoing + incoming
        
        # Update frequencies
        freq[outgoing] -= 1
        if freq[outgoing] == 0:
            distinct_count -= 1
        
        if freq[incoming] == 0:
            distinct_count += 1
        freq[incoming] += 1
        
        # Check if the current window is valid
        if has_at_least_m_distinct(freq, m):
            max_sum = max(max_sum, window_sum)
    
    return max_sum

# Example usage
print(maxSum([2,6,7,3,1,7], 3, 4))  # Output: 18
print(maxSum([5,9,9,2,4,5,4], 1, 3))  # Output: 23
print(maxSum([1,2,1,2,1,2,1], 3, 3))  # Output: 0