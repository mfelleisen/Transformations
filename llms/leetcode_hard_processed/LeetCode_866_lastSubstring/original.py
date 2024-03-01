def lastSubstring(s: str) -> str:
    """
    Given a string s, return the last substring of s in lexicographical order.
 
    Example 1:

    Input: s = "abab"
    Output: "bab"
    Explanation: The substrings are ["a", "ab", "aba", "abab", "b", "ba", "bab"]. The lexicographically maximum substring is "bab".

    Example 2:

    Input: s = "leetcode"
    Output: "tcode"

 
    Constraints:

    1 <= s.length <= 4 * 105
    s contains only lowercase English letters.

    """
    maxIndex = 0
    curIndex = 1
    while curIndex < len(s):
        i = 0
        while curIndex + i < len(s) and s[maxIndex + i] == s[curIndex + i]:
            i += 1
        if curIndex + i == len(s):
            break
        if s[maxIndex + i] < s[curIndex + i]:
            maxIndex = curIndex
        curIndex += 1
    return s[maxIndex:]