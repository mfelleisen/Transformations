def numberOfWays(n: int, x: int) -> int:
    MOD = 10**9 + 7
    # Create a list to store number of ways to express each number up to n using xth powers
    dp = [0] * (n + 1)
    dp[0] = 1  # There's one way to sum up to zero: using no numbers at all

    # Maximum base that raised to the power x does not exceed n
    max_base = int(n ** (1/x))

    # Iterate over each possible base and update dp
    for base in range(1, max_base + 1):
        current_power = base ** x
        # Update dp values from n down to current_power
        for i in range(n, current_power - 1, -1):
            dp[i] = (dp[i] + dp[i - current_power]) % MOD

    return dp[n]

# Example usages
print(numberOfWays(10, 2))  # Output: 1
print(numberOfWays(4, 1))   # Output: 2