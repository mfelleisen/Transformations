def longest_valid_parentheses(s: str) -> int:
    """
    Given a string containing just the characters '(' and ')', return the length of the longest valid (well-formed) parentheses substring.
 
    Example 1:

    Input: s = "(()"
    Output: 2
    Explanation: The longest valid parentheses substring is "()".

    Example 2:

    Input: s = ")()())"
    Output: 4
    Explanation: The longest valid parentheses substring is "()()".

    Example 3:

    Input: s = ""
    Output: 0

 
    Constraints:

    0 <= s.length <= 3 * 104
    s[i] is '(', or ')'.

    """
    n = len(s)
    result = 0
    st = []

    for i in range(n):
        if s[i] == '(':
            st.append(i)
        else:
            if st and s[st[-1]] == '(':
                st.pop()
            else:
                st.append(i)

    if not st:
        result = n
    else:
        right, left = n, 0
        while st:
            left = st.pop()
            result = max(result, right - left - 1)
            right = left
        result = max(result, right)

    return result