from typing import List
import math

def areaOfMaxDiagonal(dimensions: List[List[int]]) -> int:
    max_diagonal = 0
    max_area = 0
    
    for length, width in dimensions:
        # Calculate the square of the diagonal length
        diagonal_squared = length**2 + width**2
        # Calculate the area
        area = length * width
        
        # Check if this diagonal is longer than the current max or same length but larger area
        if (diagonal_squared > max_diagonal) or (diagonal_squared == max_diagonal and area > max_area):
            max_diagonal = diagonal_squared
            max_area = area

    return max_area

# Example usage:
dimensions1 = [[9, 3], [8, 6]]
print(areaOfMaxDiagonal(dimensions1))  # Output: 48

dimensions2 = [[3, 4], [4, 3]]
print(areaOfMaxDiagonal(dimensions2))  # Output: 12