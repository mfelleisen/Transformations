def shortest_palindrome(s: str) -> str:
    """
    You are given a string s. You can convert s to a palindrome by adding characters in front of it.
    Return the shortest palindrome you can find by performing this transformation.
 
    Example 1:
    Input: s = "aacecaaa"
    Output: "aaacecaaa"
    Example 2:
    Input: s = "abcd"
    Output: "dcbabcd"

 
    Constraints:

    0 <= s.length <= 5 * 104
    s consists of lowercase English letters only.

    """
    n = len(s)
    rev_s = s[::-1]

    for i in range(n):
        if s[:n - i] == rev_s[i:]:
            return rev_s[:i] + s
    return ""