from typing import List

def numberOfPoints(nums: List[List[int]]) -> int:
    """
    You are given a 0-indexed 2D integer array nums representing the coordinates of the cars parking on a number line. 
    For any index i, nums[i] = [starti, endi] where starti is the starting point of the ith car and endi is the ending point of the ith car.
    Return the number of integer points on the line that are covered with any part of a car.
    """
    # Create a set to keep track of all unique points covered by cars
    points_covered = set()

    # Iterate over each car's start and end points
    for start, end in nums:
        # Add all points from start to end (inclusive) to the set
        for point in range(start, end + 1):
            points_covered.add(point)
    
    # The size of the set is the number of unique points covered
    return len(points_covered)