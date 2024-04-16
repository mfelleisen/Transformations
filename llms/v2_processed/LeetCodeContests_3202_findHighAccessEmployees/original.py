from typing import List
from collections import defaultdict

def findHighAccessEmployees(access_times: List[List[str]]) -> List[str]:
    """
    This function takes a list of employee access times and returns a list of employee names who have accessed
    the system three or more times within any one-hour period.
    """
    # Dictionary to map each employee to a sorted list of their access times
    employee_times = defaultdict(list)

    # Populate the dictionary
    for employee, time in access_times:
        employee_times[employee].append(time)
    
    # Sort times for each employee
    for times in employee_times.values():
        times.sort()
    
    # Set to hold the names of high-access employees
    high_access_employees = set()
    
    # Check for each employee if they have high access
    for employee, times in employee_times.items():
        # Use a sliding window approach to determine if there are 3 or more accesses within any one-hour period
        for i in range(len(times)):
            start_time = times[i]
            count = 1  # Start with the current time
            # Compare with subsequent times
            for j in range(i + 1, len(times)):
                if within_one_hour(start_time, times[j]):
                    count += 1
                else:
                    break  # Further times cannot be within one hour from start_time
            if count >= 3:
                high_access_employees.add(employee)
                break  # No need to check further for this employee

    return list(high_access_employees)

def within_one_hour(start: str, end: str) -> bool:
    """ Helper function to check if the end time is within one hour of the start time """
    start_hour, start_minute = int(start[:2]), int(start[2:])
    end_hour, end_minute = int(end[:2]), int(end[2:])
    
    # Calculate the minutes from start of the day
    start_total_minutes = start_hour * 60 + start_minute
    end_total_minutes = end_hour * 60 + end_minute
    
    # Check if the end time is within 60 minutes of the start time
    return 0 <= (end_total_minutes - start_total_minutes) < 60

# Example usage
access_times_1 = [["a", "0549"], ["b", "0457"], ["a", "0532"], ["a", "0621"], ["b", "0540"]]
access_times_2 = [["d", "0002"], ["c", "0808"], ["c", "0829"], ["e", "0215"], ["d", "1508"], ["d", "1444"], ["d", "1410"], ["c", "0809"]]
access_times_3 = [["cd", "1025"], ["ab", "1025"], ["cd", "1046"], ["cd", "1055"], ["ab", "1124"], ["ab", "1120"]]

print(findHighAccessEmployees(access_times_1))  # Output: ["a"]
print(findHighAccessEmployees(access_times_2))  # Output: ["c", "d"]
print(findHighAccessEmployees(access_times_3))  # Output: ["ab", "cd"]