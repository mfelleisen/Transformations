from typing import List

def minimumCost(nums: List[int]) -> int:
    """
    You are given an array of integers nums of length n.
    The cost of an array is the value of its first element.
    You need to divide nums into 3 disjoint contiguous subarrays.
    Return the minimum possible sum of the cost of these subarrays.
    """
    n = len(nums)
    if n == 3:
        return sum(nums)  # Each element forms its own subarray
    
    # Initialize a large number for comparison, assuming constraints make the maximum cost 50 * 3 = 150
    min_cost = float('inf')
    
    # We need to find two breakpoints i and j where 0 < i < j < n to split the array into 3 parts
    # The three subarrays will be nums[0:i], nums[i:j], nums[j:n]
    for i in range(1, n-1):
        for j in range(i+1, n):
            # Calculate the sum of the cost of the three subarrays
            cost = nums[0] + nums[i] + nums[j]
            if cost < min_cost:
                min_cost = cost
                
    return min_cost

# Examples
print(minimumCost([1,2,3,12]))  # Output: 6
print(minimumCost([5,4,3]))     # Output: 12
print(minimumCost([10,3,1,1]))  # Output: 12