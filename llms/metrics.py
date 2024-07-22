from utils import gunzip_json_read
import numpy as np
from pathlib import Path

def pass_at_k(n: int, c: int, k: int) -> float:
    """
    Calculates 1 - comb(n - c, k) / comb(n, k).
    """
    if n - c < k:
        return 1.0
    return 1.0 - np.prod(1.0 - k / np.arange(n - c + 1, n + 1))

def get_fmt_at_k_e(example, e, k):
    results = example["results"]
    # filter to only results with e examples
    results = [r for r in results if r["num_examples"] == e]
    # get num correct
    num_correct = sum([r["correct"] for r in results])
    return pass_at_k(len(results), num_correct, k)
    

def per_file_metrics(file: Path, e: int, k: int) -> str:
    obj = gunzip_json_read(file)
    assert obj is not None, f"Failed to read {file}"
    items = obj["items"]
    hi_accs = []
    lo_accs = []
    for item in items:
        hi_accs.append(np.mean([get_fmt_at_k_e(hi, e, k) for hi in item["his"]]))
        lo_accs.append(np.mean([get_fmt_at_k_e(lo, e, k) for lo in item["los"]]))

    name = file.name.split(".")[0]
    return f"{name},{len(items)},{e},{k},{np.mean(hi_accs)},{np.mean(lo_accs)}"

    



def main(args):
    header = "name,dataset size,e,k,lo fmt@{k|e},hi fmt@{k|e}"
    print(header)
    for file in args.inputs:
        print(
            per_file_metrics(
                file=Path(file),
                e=args.e,
                k=args.k
            )
        )


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "inputs",
        type=str,
        nargs="+",
        help="Input files"
    )
    parser.add_argument(
        "-e",
        type=int,
        default=0,
        help="Number of examples per test to consider",
    )
    parser.add_argument(
        "-k",
        type=int,
        default=1,
        help="k for pass@k"
    )
    args = parser.parse_args()
    main(args)