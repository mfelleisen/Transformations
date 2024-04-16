from typing import List

def findPeaks(mountain: List[int]) -> List[int]:
    """
    You are given a 0-indexed array mountain. Your task is to find all the peaks in the mountain array.
    Return an array that consists of indices of peaks in the given array in any order.
    
    A peak is defined as an element that is strictly greater than its neighboring elements.
    The first and last elements of the array are not considered peaks.
    
    Args:
    mountain: List[int] - A list of integers representing the mountain heights.
    
    Returns:
    List[int] - Indices of all the peak elements in the mountain list.
    """
    peaks = []
    # Check for peaks from index 1 to len(mountain) - 2 (to avoid checking the first and last elements)
    for i in range(1, len(mountain) - 1):
        if mountain[i] > mountain[i - 1] and mountain[i] > mountain[i + 1]:
            peaks.append(i)
    return peaks