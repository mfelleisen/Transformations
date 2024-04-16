from typing import List
import sys

def minimumCost(nums: List[int], k: int, dist: int) -> int:
    n = len(nums)
    if k == 1:
        return nums[0]
    
    # Initialize the minimum cost to a large number
    min_cost = sys.maxsize
    
    # Function to calculate cost given specific starting indices for subarrays
    def calculate_cost(start_indices):
        cost = 0
        for start in start_indices:
            cost += nums[start]
        return cost
    
    # Helper function to generate all valid k partitions with the last k-1 starts within dist
    def dfs(start_index, depth, current_indices):
        nonlocal min_cost
        if depth == k:
            if current_indices[-1] - current_indices[1] <= dist:
                cost = calculate_cost(current_indices)
                min_cost = min(min_cost, cost)
            return
        
        # Set the limit for the next start index
        next_start_limit = n - (k - depth)
        
        for next_start in range(start_index + 1, next_start_limit + 1):
            dfs(next_start, depth + 1, current_indices + [next_start])
    
    # Start the DFS process for all possible first partitions
    for first_end in range(1, n - k + 2):
        dfs(first_end, 2, [0, first_end])  # Always start at index 0
    
    return min_cost

# Example usage
print(minimumCost([1,3,2,6,4,2], 3, 3))  # Output: 5
print(minimumCost([10,1,2,2,2,1], 4, 3))  # Output: 15
print(minimumCost([10,8,18,9], 3, 1))     # Output: 36