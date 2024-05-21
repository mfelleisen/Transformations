from pathlib import Path
import openai
import re
from typing import List, Tuple

SOLN_DELIM = ";; ---------------------------------------------------------------------------------------------------"


def clean_top_level_comments(ex: str) -> str:
    # stop cleaning as soon as you find "(..."
    splits = ex.split("\n")
    start = 0
    for i, line in enumerate(splits):
        if line.startswith("("):
            start = i
            break

    cleaned = splits[start:]
    return "\n".join(cleaned).strip()


def get_low_to_high_examples(high_dir_str: str) -> List[Tuple[str, str, str]]:
    high_dir = Path(high_dir_str)
    assert high_dir.exists(), f"{high_dir} does not exist"
    assert high_dir.is_dir(), f"{high_dir} is not a directory"
    examples = []
    for file in high_dir.glob("*.rkt"):
        contents = file.read_text()
        prompt = file.with_suffix(".txt").read_text()
        high_ex = None
        low_ex = None
        for ex in contents.split(SOLN_DELIM):
            if "\n(test" in ex:
                continue
            elif "-HIGH" in ex:
                high_ex = ex
            elif "-ai" in ex:
                low_ex = ex

        assert high_ex is not None, f"Could not find high example in {file}"
        assert low_ex is not None, f"Could not find low example in {file}"
        # remove "-HIGH" from the high example
        high_ex = high_ex.replace("-HIGH", "")
        # and -ai{number}  (if there is any number) from the low example
        low_ex = re.sub(r"-ai\d+", "", low_ex)
        low_ex = clean_top_level_comments(low_ex)
        high_ex = clean_top_level_comments(high_ex)
        examples.append((prompt, low_ex, high_ex))
    return examples

SYSTEM = "You are a Racket expert. You are given a unidiomatic Racket program, and your task is to refactor the program to make it more idiomatic. You can assume that the input program is correct and that it has the same behavior as the refactored program."

def main(args):
    examples = get_low_to_high_examples(args.high_dir)
    for prompt, low, high in examples:
        print(low)
        print(high)
        print()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--high-dir", type=str, default="../Plain/")
    parser.add_argument("--programs-dir", type=str, default="../programs/")
    args = parser.parse_args()
    main(args)
