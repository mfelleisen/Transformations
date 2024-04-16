from typing import List

def countTestedDevices(batteryPercentages: List[int]) -> int:
    """
    This function takes a list of battery percentages of devices and returns the number
    of devices that were tested. A device is tested if its battery percentage is greater
    than 0. After testing a device, the battery percentage of all subsequent devices
    is reduced by 1, but not below 0.
    """
    tested_count = 0
    n = len(batteryPercentages)
    
    for i in range(n):
        if batteryPercentages[i] > 0:
            tested_count += 1  # Increment the tested device count
            # Decrease the battery percentage of subsequent devices
            for j in range(i + 1, n):
                batteryPercentages[j] = max(0, batteryPercentages[j] - 1)
    
    return tested_count

# Example usage:
print(countTestedDevices([1, 1, 2, 1, 3]))  # Output: 3
print(countTestedDevices([0, 1, 2]))        # Output: 2