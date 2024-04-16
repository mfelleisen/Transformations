from typing import List

def maximizeTheProfit(n: int, offers: List[List[int]]) -> int:
    # Sorting offers based on their ending time to process them in order
    offers.sort(key=lambda x: x[1])

    # Helper function to find the last non-conflicting offer using binary search
    def findLastNonConflicting(offers, i):
        low, high = 0, i - 1
        while low <= high:
            mid = (low + high) // 2
            if offers[mid][1] < offers[i][0]:
                if offers[mid + 1][1] < offers[i][0]:
                    low = mid + 1
                else:
                    return mid
            else:
                high = mid - 1
        return -1

    # Initialize DP array where dp[i] will store the maximum profit using the first i offers
    dp = [0] * len(offers)

    # Base case: the profit of the first offer is just its gold value
    dp[0] = offers[0][2]

    # Fill the dp array
    for i in range(1, len(offers)):
        # Include current offer's gold
        profit_including_current = offers[i][2]
        # Find last non-overlapping offer's index
        l = findLastNonConflicting(offers, i)
        if l != -1:
            profit_including_current += dp[l]
        
        # Store the maximum profit by either including or excluding the current offer
        dp[i] = max(dp[i-1], profit_including_current)

    # The last element in dp array will have the answer to the problem
    return dp[-1]

# Example usage:
print(maximizeTheProfit(5, [[0,0,1],[0,2,2],[1,3,2]]))  # Output: 3
print(maximizeTheProfit(5, [[0,0,1],[0,2,10],[1,3,2]]))  # Output: 10