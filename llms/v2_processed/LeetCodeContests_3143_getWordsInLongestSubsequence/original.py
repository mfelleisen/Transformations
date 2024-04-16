from typing import List

def getWordsInLongestSubsequence(n: int, words: List[str], groups: List[int]) -> List[str]:
    if n == 0:
        return []
    
    # Initialize the longest subsequence
    longest_subsequence = [words[0]]
    
    # Track the last group added to the subsequence
    last_group = groups[0]
    
    for i in range(1, n):
        # Only add the current word if the group is different from the last group in the subsequence
        if groups[i] != last_group:
            longest_subsequence.append(words[i])
            last_group = groups[i]
    
    return longest_subsequence

# Example usage:
n = 4
words = ["a", "b", "c", "d"]
groups = [1, 0, 1, 1]
output = getWordsInLongestSubsequence(n, words, groups)
print(output)  # Output can be ["a", "b", "c"] or ["a", "b", "d"] depending on the exact requirements