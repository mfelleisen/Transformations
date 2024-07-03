from pathlib import Path
from tqdm import tqdm
import random
import os
from typing import List
import openai
import pandas as pd


better_cache = {}

SYSTEM_RERANK = """You are a Racket expert. You are given two racket programs, and your task is to determine which program is more idiomatic and modern. You can assume that both programs are correct and that they have the same behavior.
Idiomatic Racket code favors immutability, recursion, accumulators, traversals, and higher-order functions. It is discouraged to use mutable data structures, loops, imperative programming constructs, and direct list indexing (e.g. list-ref).
Modern idiomatic Racket additionally discourages let and let* in favor of define, and prefers pattern matching over cond in some cases.
Additionally, modern Racket utilizes for/fold, for/list, ..., instead of for loops and loop functions.

You will respond with either 1 or 2, where 1 indicates that the first program is more idiomatic and modern, and 2 indicates that the second program is more idiomatic and modern."""

MATTHIAS_DELIM = ";; ---------------------------------------------------------------------------------------------------"


def get_openai_key() -> str:
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    else:
        raise ValueError("OPENAI_API_KEY not found in environment variables")


def clean_code(code: str) -> str:
    # remove tests
    code = code.split("(require rackunit)")[0]
    # remove delim if present
    code = code.split(MATTHIAS_DELIM)[0]

    # find if -AI in name
    lines = code.split("\n")
    keeplines = []
    for l in lines:
        if "-AI" in l:
            break
        keeplines.append(l)
    code = "\n".join(keeplines)
    return code


def is_better_than(client, model, a, b) -> bool:
    if (a, b) in better_cache:
        return better_cache[(a, b)]

    acode = clean_code(a.read_text())
    bcode = clean_code(b.read_text())

    convo = [
        {
            "role": "system",
            "content": SYSTEM_RERANK
        },
        {
            "role": "user",
            "content": f"""Please determine which of the following programs is more idiomatic and modern. Start your response with the first line being either 'Answer: 1' or 'Answer: 2'.
```racket
{acode}
```

```racket
{bcode}
```
"""
        },
    ]

    completion = client.chat.completions.create(
        model=model,
        messages=convo,
        n=1,
        temperature=0.0
    )
    # extract the answer
    content = completion.choices[0].message.content
    answer = content.split("\n")[0].split(" ")[1]

    if answer not in ["1", "2"]:
        # if not 1 or 2, then pick randomly
        print(f"WARNING: Randomly picking, answer was {answer}")
        res = random.choice([True, False])
    else:
        res = answer == "1"

    better_cache[(a, b)] = res
    return res


def partition(client, model, arr: List, low: int, high: int) -> int:
    i = low - 1
    pivot = arr[high]
    for j in range(low, high):
        if is_better_than(client, model, arr[j], pivot):
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1


def quicksort(client, model, arr: List, low: int, high: int) -> List:
    if low < high:
        pi = partition(client, model, arr, low, high)
        quicksort(client, model, arr, low, pi - 1)
        quicksort(client, model, arr, pi + 1, high)
    return arr


def main(args):
    random.seed(42)
    tables = list(Path(args.programs_dir).rglob("heuristic.csv"))
    client = openai.Client(api_key=get_openai_key())
    for table in tqdm(tables):
        d = table.parent
        reranking_out = d / "reranking.txt"
        if reranking_out.exists():
            print(f"Skipping {d} as it has already been reranked")
            continue

        df = pd.read_csv(table)
        df = df.sort_values("heuristic", ascending=False)
        df = df.head(args.top_k)
        ids = list(map(lambda i: i.split("_"), df["id"].tolist()))
        paths = []
        for i in ids:
            paths.append(d / f"{i[0]}_refactored" / f"refactor_{i[1]}.rkt")

        paths = quicksort(client, args.model, paths, 0, len(paths) - 1)
        paths_str = "\n".join(map(lambda p: str(p.relative_to(d)), paths))
        with open(reranking_out, "w") as f:
            f.write(paths_str)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--programs-dir", type=str, default="./v2_processed")
    parser.add_argument("--model", type=str, default="gpt-4o")
    parser.add_argument("--top-k", type=int, default=10)
    args = parser.parse_args()
    main(args)
