from typing import List

def maxIncreasingGroups(usageLimits: List[int]) -> int:
    # The key insight is that the growth of group sizes must be strictly increasing,
    # and the number of each element we can use is limited by usageLimits.
    # We must find the maximum number of increasing groups.

    # Sort usageLimits to facilitate the creation of groups in increasing order of their lengths
    usageLimits.sort()

    # This variable will hold the current minimum size that the next group should have
    current_min_size = 1

    # This variable will keep track of the number of groups we can form
    num_groups = 0

    # We'll use the total counter to check how many times we can use each number considering the current group size
    total = 0

    # Iterate over each limit in the sorted usageLimits
    for limit in usageLimits:
        # Add the current limit to total
        total += limit
        # Check if we can form a new group of at least `current_min_size`
        if total >= current_min_size:
            # Form a new group
            num_groups += 1
            # Update the total to reflect the formation of a group of size current_min_size
            total -= current_min_size
            # Increment the minimum size required for the next group
            current_min_size += 1

    return num_groups

# Example usage:
print(maxIncreasingGroups([1,2,5]))  # Output: 3
print(maxIncreasingGroups([2,1,2]))  # Output: 2
print(maxIncreasingGroups([1,1]))    # Output: 1