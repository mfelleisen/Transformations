from typing import List

def numberOfEmployeesWhoMetTarget(hours: List[int], target: int) -> int:
    """
    This function returns the number of employees who have met or exceeded the target working hours.

    :param hours: List[int] - a list of integers representing the number of hours each employee has worked.
    :param target: int - an integer representing the target number of hours each employee should work.
    :return: int - the number of employees who worked at least the target number of hours.
    """
    count = 0
    for hour in hours:
        if hour >= target:
            count += 1
    return count

# Example use cases
print(numberOfEmployeesWhoMetTarget([0, 1, 2, 3, 4], 2))  # Output: 3
print(numberOfEmployeesWhoMetTarget([5, 1, 4, 2, 2], 6))  # Output: 0