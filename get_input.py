import requests
import sys

year = 2025

def fetch_input(day, session_cookie):
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    cookies = {'session': session_cookie}
    response = requests.get(url, cookies=cookies)
    if response.ok:
        return response.text
    else:
        print("Error fetching input data")
        return None

def save_input(day, session_cookie):
    data = fetch_input(day, session_cookie)
    if not data:
        return
    with open(f"{day}.in", "w") as f:
        f.write(data)
    print(f"Input data for {year}/{day} saved to {day}.in")

day = sys.argv[1]
session_cookie = open("SESSION_COOKIE.txt").read().strip()
save_input(day, session_cookie)
