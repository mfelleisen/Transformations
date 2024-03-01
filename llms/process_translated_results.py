from typing import Optional
from pathlib import Path
from utils import gunzip_json_read


def proc_success(program: str, original_prompt: str) -> Optional[str]:
    # Steps:
    # 1. Get the original purpose statement from original_prompt and the name of the function
    # 2. From the generated program, find the location of the function signature
    # 3. Remove all comments before the function signature
    # 4. Transplant the original purpose statement to the function signature

    # Step 1
    original_prompt = original_prompt.strip()
    splits = original_prompt.split("(define (")
    original_prompt_lines = splits[0].strip().split(
        "\n")[1:]  # remove the first line, #lang racket
    fun_name = splits[1].split(" ")[0]

    # Step 2 -- we assume fun sig is only 1 line
    lines = program.split("\n")
    fun_sig_loc = -1
    for i, line in enumerate(lines):
        if f"define" in line and fun_name in line:
            fun_sig_loc = i
            break
    if fun_sig_loc == -1:
        return None

    # Step 3
    # the comments end before the function signature, find where they start
    # there may not be any comments too
    comment_start = -1
    # go in reverse
    for i in range(fun_sig_loc, -1, -1):
        if ";" in lines[i]:
            comment_start = i
        else:
            break

    # Step 4 -- reassemble the program
    if comment_start == -1:
        lines = lines[:fun_sig_loc] + \
            original_prompt_lines + lines[fun_sig_loc:]
    else:
        lines = lines[:comment_start] + \
            original_prompt_lines + lines[fun_sig_loc:]
    program = "\n".join(lines)
    return program


def main(args):
    files = list(args.results.glob("*.results.json.gz"))
    outdir = Path(args.output)
    outdir.mkdir(parents=True, exist_ok=True)
    for file in files:
        data = gunzip_json_read(file)
        if data is not None:
            results = data["results"]
            name = data["name"]
            path_to_write = outdir / f"{name}"
            successes = []
            for result in results:
                if result["status"] == "OK":
                    processed = proc_success(
                        result["program"], data["original_prompt"])
                    if processed is not None:
                        successes.append(processed)
                    else:
                        print(f"Failed to process {result['program']}")

            if len(successes) > 0:
                path_to_write.mkdir(parents=True, exist_ok=True)
                for i, success in enumerate(successes):
                    with open(path_to_write / f"{i}.rkt", "w") as f:
                        f.write(success)

                with open(path_to_write / "original.py", "w") as f:
                    f.write(data["original"])


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--results", type=Path, required=True,
                        help="Path to the results directory")
    parser.add_argument("--output", type=str, required=True,
                        help="Path to the output directory")
    args = parser.parse_args()
    main(args)
