from typing import List

def findMissingAndRepeatedValues(grid: List[List[int]]) -> List[int]:
    """
    You are given a 0-indexed 2D integer matrix grid of size n * n with values in the range [1, n^2]. 
    Each integer appears exactly once except one which appears twice (a) and one which is missing (b). 
    The task is to find the repeating and missing numbers a and b.
    Return a 0-indexed integer array ans of size 2 where ans[0] equals to a and ans[1] equals to b.
    """
    n = len(grid)
    max_val = n * n
    seen = set()
    repeated = -1
    
    # Flatten the grid to make frequency counting easier
    flat_list = [item for sublist in grid for item in sublist]
    
    # Count occurrences and find the repeated number
    for num in flat_list:
        if num in seen:
            repeated = num
        seen.add(num)
    
    # Find the missing number by checking all numbers from 1 to n^2
    missing = -1
    for i in range(1, max_val + 1):
        if i not in seen:
            missing = i
            break
    
    return [repeated, missing]

# Example usage:
grid1 = [[1, 3], [2, 2]]
grid2 = [[9, 1, 7], [8, 9, 2], [3, 4, 6]]
print(findMissingAndRepeatedValues(grid1))  # Output: [2, 4]
print(findMissingAndRepeatedValues(grid2))  # Output: [9, 5]