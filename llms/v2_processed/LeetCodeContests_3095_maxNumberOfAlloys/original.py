from typing import List

def maxNumberOfAlloys(n: int, k: int, budget: int, composition: List[List[int]], stock: List[int], cost: List[int]) -> int:
    max_alloys = 0
    
    # Check each machine
    for machine in range(k):
        machine_max_possible = float('inf')  # Start with a large number, assuming infinite possible alloys
        
        # Calculate the maximum number of alloys this machine can produce
        for metal_type in range(n):
            required_per_alloy = composition[machine][metal_type]
            if required_per_alloy > 0:
                # Determine how many such alloys we can afford to make
                max_alloys_with_current_metal = (stock[metal_type] + budget // cost[metal_type]) // required_per_alloy
                machine_max_possible = min(machine_max_possible, max_alloys_with_current_metal)
        
        # Calculate the total cost for the max possible alloys for this machine
        total_cost = 0
        for metal_type in range(n):
            required_units = composition[machine][metal_type] * machine_max_possible
            needed_units = max(0, required_units - stock[metal_type])
            total_cost += needed_units * cost[metal_type]
        
        # Check if this total cost is within the budget
        if total_cost <= budget:
            max_alloys = max(max_alloys, machine_max_possible)
        else:
            # Try to reduce the number of alloys produced to fit the budget
            # Binary search could be used here for more efficiency in some cases
            while total_cost > budget and machine_max_possible > 0:
                machine_max_possible -= 1
                total_cost = 0
                for metal_type in range(n):
                    required_units = composition[machine][metal_type] * machine_max_possible
                    needed_units = max(0, required_units - stock[metal_type])
                    total_cost += needed_units * cost[metal_type]
            max_alloys = max(max_alloys, machine_max_possible)

    return max_alloys

# Example usage
n = 3
k = 2
budget = 15
composition = [[1, 1, 1], [1, 1, 10]]
stock = [0, 0, 100]
cost = [1, 2, 3]
print(maxNumberOfAlloys(n, k, budget, composition, stock, cost))  # Output: 5