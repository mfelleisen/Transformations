from typing import List

def findChampion(grid: List[List[int]]) -> int:
    """
    This function takes a grid representing the strengths of teams in a tournament and returns the champion team.
    """
    n = len(grid)
    potential_champion = 0
    
    # Iterate over each team to find the potential champion
    for i in range(1, n):
        if grid[potential_champion][i] == 1:
            # If potential_champion is stronger than i, continue
            continue
        else:
            # If i is stronger than potential_champion, update potential_champion
            potential_champion = i
    
    # Verify potential champion by ensuring no other team is stronger
    for j in range(n):
        if j != potential_champion and grid[j][potential_champion] == 1:
            return -1  # If any team is stronger, there's no clear champion
    
    return potential_champion

# Example Usage
print(findChampion([[0,1],[0,0]]))  # Output: 0
print(findChampion([[0,0,1],[1,0,1],[0,0,0]]))  # Output: 1