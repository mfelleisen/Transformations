from typing import List
from collections import Counter

def maxFrequencyElements(nums: List[int]) -> int:
    """
    You are given an array nums consisting of positive integers.
    Return the total frequencies of elements in nums such that those elements all have the maximum frequency.
    The frequency of an element is the number of occurrences of that element in the array.
    """
    # Using Counter to count frequencies of each element
    frequency = Counter(nums)
    
    # Finding the maximum frequency
    max_freq = max(frequency.values())
    
    # Counting how many times the maximum frequency occurs
    count_max_freq_elements = sum(freq for elem, freq in frequency.items() if freq == max_freq)
    
    return count_max_freq_elements