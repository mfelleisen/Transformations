def k_similarity(s1: str, s2: str) -> int:
    """
    Strings s1 and s2 are k-similar (for some non-negative integer k) if we can swap the positions of two letters in s1 exactly k times so that the resulting string equals s2.
    Given two anagrams s1 and s2, return the smallest k for which s1 and s2 are k-similar.
 
    Example 1:

    Input: s1 = "ab", s2 = "ba"
    Output: 1
    Explanation: The two string are 1-similar because we can use one swap to change s1 to s2: "ab" --> "ba".

    Example 2:

    Input: s1 = "abc", s2 = "bca"
    Output: 2
    Explanation: The two strings are 2-similar because we can use two swaps to change s1 to s2: "abc" --> "bac" --> "bca".

 
    Constraints:

    1 <= s1.length <= 20
    s2.length == s1.length
    s1 and s2 contain only lowercase letters from the set {'a', 'b', 'c', 'd', 'e', 'f'}.
    s2 is an anagram of s1.

    """
    k = 0
    a, b = list(s1), list(s2)
    for i in range(len(a)):
        if a[i] != b[i]:
            for j in range(i + 1, len(b)):
                if a[i] == b[j] and a[j] != b[j]:
                    a[i], a[j] = a[j], a[i]
                    k += 1
                    break
    return k