from pathlib import Path
from typing import Tuple
import re


def heuristic(code) -> Tuple[int, str]:
    # extract code
    defn_idx = code.find("(define")
    assert defn_idx != -1, "Could not find (define in code"
    code = code[defn_idx:]
    code = code.split("(require rackunit)")[0]
    # remove comments
    code = re.sub(r";.*", "", code)
    # remove strings
    code = re.sub(r'".*?"', "", code)

    # use of mutation
    mut = len(re.findall(r"\(\w*! ", code))
    # use of indexing
    ref = len(re.findall(r"\(\w*-ref ", code))

    hofs = ["map", "filter", "reduce", "fold", "lambda", "apply"]
    hof = 0
    for h in hofs:
        hof += len(re.findall(f"\\({h}", code))

    for_hofs = [f"for/{h}" for h in hofs + ["sum", "list"]]
    for_hof = 0
    for h in for_hofs:
        for_hof += len(re.findall(f"\\({h}", code))

    helper = code.count("(define (") - 1

    # bad: mutation and indexing
    # good: hof, for_hof, helper
    h = 0
    h -= mut
    h -= ref
    h += hof
    h += for_hof
    h += helper

    return h, f"{mut},{ref},{hof},{for_hof},{helper}"


def main(args):
    header = "id,heuristic,mutation,indexing,hof,for_hof,helpers\n"
    for problem_dir in Path(args.problems_path).iterdir():
        contents = header

        highest_h = float("-inf")
        highest_h_id = None

        for refactors in sorted(problem_dir.iterdir()):
            if not refactors.is_dir():
                continue
            for refactor in sorted(refactors.iterdir(), key=lambda x: int(x.stem.split("_")[-1])):
                h, info = heuristic(refactor.read_text())
                r_id = str(refactor.stem).split("_")[-1]
                p_id = str(refactors.stem).split("_")[0]
                contents += f"{p_id}_{r_id},{h},{info}\n"
                if h > highest_h:
                    highest_h = h
                    highest_h_id = f"{p_id}_{r_id}"

        outpath = problem_dir / "heuristic.csv"
        with open(outpath, "w") as f:
            print(f"Writing to {outpath}")
            f.write(contents)

        with open(problem_dir / "highest_heuristic.txt", "w") as f:
            f.write(f"{highest_h_id} {highest_h}")


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--problems-path", type=str, default="./v2_processed")
    args = parser.parse_args()
    main(args)
