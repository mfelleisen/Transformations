from pathlib import Path
import re


def heuristic(code) -> str:
    return "todo"


def main(args):
    header = "id,heuristic\n"
    for problem_dir in Path(args.problems_path).iterdir():
        contents = header
        for refactors in sorted(problem_dir.iterdir()):
            if not refactors.is_dir():
                continue
            for refactor in sorted(refactors.iterdir(), key=lambda x: int(x.stem.split("_")[-1])):
                h = heuristic(refactor.read_text())
                r_id = str(refactor.stem).split("_")[-1]
                p_id = str(refactors.stem).split("_")[0]
                contents += f"{p_id}_{r_id},{h}\n"
        outpath = problem_dir / "heuristic.csv"
        with open(outpath, "w") as f:
            print(f"Writing to {outpath}")
            f.write(contents)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--problems-path", type=str, default="./v2_processed")
    args = parser.parse_args()
    main(args)
