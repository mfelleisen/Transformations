from typing import List
from collections import defaultdict

def maximumBeauty(nums: List[int], k: int) -> int:
    """
    This function returns the maximum possible beauty of the array nums after applying the operations defined.
    """
    # Dictionary to store frequency of each potential target value
    freq_map = defaultdict(int)

    # Populate the frequency map
    for num in nums:
        # Increment the possible target range by one for each number
        # Using the range [num - k, num + k + 1) for the possible new values
        for possible_value in range(num - k, num + k + 1):
            freq_map[possible_value] += 1

    # The result is the maximum frequency found in the map
    return max(freq_map.values())

# Example usage
print(maximumBeauty([4, 6, 1, 2], k=2))  # Output: 3
print(maximumBeauty([1, 1, 1, 1], k=10))  # Output: 4