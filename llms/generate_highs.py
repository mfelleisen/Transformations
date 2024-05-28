from pathlib import Path
import os
import random
import time
import openai
import re
from typing import Dict, List, Tuple
from code_exec_server.code_exec_reqs import exec_test_batched, check_executor_alive
from tqdm import tqdm

SOLN_DELIM = ";; ---------------------------------------------------------------------------------------------------"


def clean_example(ex: str) -> str:
    # stop cleaning as soon as you find "(..."
    splits = ex.split("\n")
    start = 0
    for i, line in enumerate(splits):
        if line.startswith("("):
            start = i
            break

    cleaned = splits[start:]
    cleaned = "\n".join(cleaned).strip()
    # if it doesn't have "#lang racket" add it
    if not cleaned.startswith("#lang racket"):
        cleaned = "#lang racket\n" + cleaned
    return cleaned


def get_low_to_high_examples(high_dir_str: str) -> List[Tuple[str, str, str]]:
    high_dir = Path(high_dir_str)
    assert high_dir.exists(), f"{high_dir} does not exist"
    assert high_dir.is_dir(), f"{high_dir} is not a directory"
    examples = []
    for file in high_dir.glob("*.rkt"):
        contents = file.read_text()
        prompt = file.with_suffix(".txt").read_text()
        high_ex = None
        low_exs = []
        for ex in contents.split(SOLN_DELIM):
            if "aiHIGH" in ex:
                continue
            if "\n(test" in ex:
                continue
            elif "-HIGH" in ex:
                high_ex = ex
            elif "-ai" in ex:
                low_exs.append(ex)

        assert high_ex is not None, f"Could not find high example in {file}"
        assert len(low_exs) != 0, f"Could not find low examples in {file}"
        low_ex = random.choice(low_exs)
        # remove "-HIGH" from the high example
        high_ex = high_ex.replace("-HIGH", "")
        # and -ai{number}  (if there is any number) from the low example
        low_ex = re.sub(r"-ai\d+", "", low_ex)
        low_ex = clean_example(low_ex)
        high_ex = clean_example(high_ex)
        examples.append((prompt, low_ex, high_ex))
    random.shuffle(examples)
    return examples


SYSTEM = """You are a Racket expert. You are given a unidiomatic Racket program, and your task is to refactor the program to make it more modern and idiomatic. You can assume that the input program is correct and that it has the same behavior as the refactored program.
Idiomatic Racket code favors immutability, recursion, accumulators, traversals, and higher-order functions. It is discouraged to use mutable data structures, loops, imperative programming constructs, and direct list indexing (e.g. list-ref).
Modern idiomatic Racket additionally discourages let and let* in favor of define, and prefers pattern matching over cond if possible.
Extremely important: do not change the name of the function, the number of arguments, or the behavior of the function. The refactored program should have the same behavior as the input program."""


def markdown_codeblock_extract(response: str) -> str:
    lines = response.split("\n")
    buf = ""
    in_codeblock = False
    for ln in lines:
        if ln.startswith("```"):
            if in_codeblock:
                break
            else:
                in_codeblock = True
        elif in_codeblock:
            buf += ln + "\n"
    return buf


def prompt_with_examples(examples: List[Tuple[str, str, str]], code: str) -> List[Dict[str, str]]:
    prefix = """Please refactor the following Racket program to make it more modern and idiomatic. You can assume that the input program is correct and that it has the same behavior as the refactored program.
Idiomatic Racket code favors immutability, recursion, accumulators, traversals, and higher-order functions. It is discouraged to use mutable data structures, loops, imperative programming constructs, and direct list indexing (e.g. list-ref).
Modern idiomatic Racket additionally discourages let and let* in favor of define, and prefers pattern matching over cond if possible.
Extremely important: do not change the name of the function, the number of arguments, or the behavior of the function. The refactored program should have the same behavior as the input program."""
    postfix = """Absolutely! Here is the refactored program, rewritten to be more idiomatic:"""
    convo = [
        {
            "role": "system",
            "content": SYSTEM
        }
    ]
    for prompt, low, high in examples:
        prompt = f"{prefix}\n```racket\n{prompt}\n{low}\n```"
        convo.append(
            {
                "role": "user",
                "content": prompt

            }
        )
        response = f"{postfix}\n```racket\n" + high + "\n```"
        convo.append(
            {
                "role": "assistant",
                "content": response
            }
        )

    convo.append({
        "role": "user",
        "content": f"{prefix}\n```racket\n{code}\n```"
    })
    return convo


def get_openai_key() -> str:
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    else:
        raise ValueError("OPENAI_API_KEY not found in environment variables")


def get_racket_docstring(code: str) -> str:
    docstring = ""
    for line in code.split("\n"):
        if line.startswith(";;"):
            docstring += line + "\n"
        elif "#lang" in line or line.strip() == "":
            continue
        else:
            break
    return docstring


def main(args):
    random.seed(42)
    client = openai.Client(api_key=get_openai_key())
    dirs = list(Path(args.programs_dir).iterdir())
    for d in tqdm(dirs):
        # check if we did this already
        skip = False
        for refactored in d.glob("*_refactored"):
            if len(list(refactored.glob("*.rkt"))) > 0:
                skip = True
                break
        if skip:
            print(f"Skipping {d} as it has already been refactored")
            continue

        racket_files = list(d.glob("*.rkt"))
        racket_files = racket_files[:args.max_per_problem]
        for picked in racket_files:
            code = picked.read_text()
            # separate from tests
            split = code.split("(require rackunit)")
            code = split[0]
            tests = "(require rackunit)\n" + split[1]
            prompt = prompt_with_examples(
                get_low_to_high_examples(args.high_dir), code)

            while True:
                try:
                    completion = client.chat.completions.create(
                        model=args.model,
                        messages=prompt,  # type: ignore
                        n=args.attempts,
                        temperature=0.75,
                        top_p=0.95,
                    )
                    break
                except Exception as e:
                    print("Error:", e)
                    print("Retrying in 20 seconds")
                    time.sleep(20)

            # create dir for refactors
            refactored_dir = d / (picked.stem + "_refactored")

            refactors = []
            for choice in completion.choices:
                response = choice.message.content
                if response is None:
                    print("No response from: ", args.model)
                    continue

                refactored = markdown_codeblock_extract(
                    response) + "\n" + tests
                refactored = clean_example(refactored).replace(
                    "#lang racket", "").strip()
                refactored = get_racket_docstring(code) + refactored
                refactored = "#lang racket\n\n" + refactored
                refactors.append(refactored)

            # execute the refactors
            assert check_executor_alive(
                args.executor), "Executor is not alive. Please start it in code_exec_server"
            results = exec_test_batched(args.executor, refactors, [
                                        ""] * len(refactors), lang="racket")
            for i, (refact, result) in enumerate(zip(refactors, results)):
                if not result[0]:
                    # didn't pass
                    continue

                refactored_dir.mkdir(exist_ok=True)
                refactored_file = refactored_dir / f"refactor_{i}.rkt"
                refactored_file.write_text(refact)
                print(f"Refactored program {i} written to {refactored_file}")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--high-dir", type=str, default="../Plain/")
    parser.add_argument("--programs-dir", type=str, default="./v2_processed")
    parser.add_argument("--model", type=str, default="gpt-4o")
    parser.add_argument("--attempts", type=int, default=25)
    parser.add_argument("--max-per-problem", type=int, default=5)
    parser.add_argument("--executor", type=str,
                        default="http://127.0.0.1:8000")
    args = parser.parse_args()
    main(args)
