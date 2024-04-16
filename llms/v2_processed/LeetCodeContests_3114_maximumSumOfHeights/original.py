from typing import List

def maximumSumOfHeights(maxHeights: List[int]) -> int:
    n = len(maxHeights)
    
    # Function to calculate the max sum with a fixed peak position
    def calculate_max_sum_with_peak(peak):
        # Starting from the peak, expand to the left and right
        # Initialize the heights array with 0's
        heights = [0] * n
        
        # Set the peak height
        heights[peak] = maxHeights[peak]
        
        # Expand to the left of the peak
        for i in range(peak - 1, -1, -1):
            heights[i] = min(maxHeights[i], heights[i + 1])
        
        # Expand to the right of the peak
        for i in range(peak + 1, n):
            heights[i] = min(maxHeights[i], heights[i - 1])
        
        # Calculate the sum of the heights array
        return sum(heights)
    
    max_sum = 0
    # Try every position as a peak
    for i in range(n):
        max_sum = max(max_sum, calculate_max_sum_with_peak(i))
    
    return max_sum

# Example usage
print(maximumSumOfHeights([5,3,4,1,1]))  # Output: 13
print(maximumSumOfHeights([6,5,3,9,2,7]))  # Output: 22
print(maximumSumOfHeights([3,2,5,5,2,3]))  # Output: 18