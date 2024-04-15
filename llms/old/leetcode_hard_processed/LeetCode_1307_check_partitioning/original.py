def check_partitioning(s: str) -> bool:
    """
    Given a string s, return true if it is possible to split the string s into three non-empty palindromic substrings. Otherwise, return false.​​​​​
    A string is said to be palindrome if it the same string when reversed.
 
    Example 1:

    Input: s = "abcbdd"
    Output: true
    Explanation: "abcbdd" = "a" + "bcb" + "dd", and all three substrings are palindromes.

    Example 2:

    Input: s = "bcbddxy"
    Output: false
    Explanation: s cannot be split into 3 palindromes.

 
    Constraints:

    3 <= s.length <= 2000
    s​​​​​​ consists only of lowercase English letters.

    """
    def is_palindrome(s, start, end):
        while start < end:
            if s[start] != s[end]:
                return False
            start += 1
            end -= 1
        return True
    n = len(s)
    for i in range(0, n - 2):
        if is_palindrome(s, 0, i):
            for j in range(i + 1, n - 1):
                if is_palindrome(s, i + 1, j) and is_palindrome(s, j + 1, n - 1):
                    return True
    return False