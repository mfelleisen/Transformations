from typing import List

def maximumTripletValue(nums: List[int]) -> int:
    n = len(nums)
    max_value = float('-inf')
    
    # Iterate over all possible triplets (i, j, k) with i < j < k
    for i in range(n - 2):
        for j in range(i + 1, n - 1):
            for k in range(j + 1, n):
                # Calculate the value of the current triplet
                current_value = (nums[i] - nums[j]) * nums[k]
                # Update max_value if the current value is greater
                max_value = max(max_value, current_value)
    
    # Return the maximum value found or 0 if all values are negative
    return max(max_value, 0)

# Example usage
print(maximumTripletValue([12,6,1,2,7]))  # Output: 77
print(maximumTripletValue([1,10,3,4,19]))  # Output: 133
print(maximumTripletValue([1,2,3]))  # Output: 0