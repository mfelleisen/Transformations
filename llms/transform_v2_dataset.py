from pathlib import Path
import datasets
import json
import ast


def process_tests(tests_str, func_name):
    tests_raw = tests_str.split("test_input =")[1:]
    tests_proc = ""
    for test in tests_raw:
        ins_raw, outs_dirty = test.strip().split("\n")
        ins = json.loads(ins_raw)
        outs = outs_dirty.split("==")[1].strip()

        # we don't care about the keys for ins, just keep same order
        ins = list(ins.values())

        # format ins as "assert candidate(ins...) == outs"
        ins_str = ", ".join(map(str, ins))
        test_proc = f"    assert candidate({ins_str}) == {outs}\n"
        tests_proc += test_proc

    tests_proc = f"def check(candidate):\n{tests_proc}\n\n"
    tests_proc += f"def test_check():\n    check({func_name})\n"
    tests_proc = "### Unit tests below ###\n" + tests_proc

    return tests_proc


def process_docstring(prompt):
    prompt = prompt.strip()
    docsplits = prompt.split('"""')
    assert len(docsplits) == 3, f"Expected 3 splits, got {len(docsplits)}"
    docstring = docsplits[1].strip()
    # indent all lines by 4 spaces
    docstring = '"""\n' + docstring + '\n"""'
    docstring = "    " + docstring.replace("\n", "\n    ")
    return docstring


def process_signature(prompt):
    sig = "def " + prompt.split("    def")[1].strip()
    assert "\n" not in sig, f"Unexpected newline in signature: {sig}"
    # remove "self" argument
    sig = sig.replace("self, ", "")
    assert "self" not in sig, f"Unexpected 'self' in signature: {sig}"
    return sig


def process_imports(sig):
    needs_import = "List[" in sig
    if needs_import:
        return "from typing import List\n\n"
    return ""


def main(args):
    ds = datasets.load_dataset(
        "json", data_files=args.input_json, split="train")
    # filter out examples with extra defs:
    ds = ds.filter(lambda ex: ex["prompt"].count("    def") == 1)

    outdir = Path(args.output_dir)
    outdir.mkdir(parents=True, exist_ok=True)

    dupes = set()
    for ex in ds:
        sig = process_signature(ex["prompt"])
        if sig in dupes:
            print(f"Skipping duplicate: {sig}")
            continue
        name = sig.split("(")[0].split("def ")[1].strip()
        tests = process_tests(ex["test"], name)
        docstring = process_docstring(ex["prompt"])
        dupes.add(sig)
        imports = process_imports(sig)
        top = f"{imports}{sig}\n{docstring}\n    ### Canonical solution below ###\n    pass"

        # compose
        out = f"{top}\n\n{tests}"

        # metadata block
        meta = ex["meta"]
        qid = meta["questionId"]
        title = meta["title"]
        title_slug = meta["titleSlug"]
        difficulty = meta["difficulty"]
        category = meta["categoryTitle"]
        likes = meta["likes"]
        dislikes = meta["dislikes"]
        meta_block = f"""### Metadata below ###
# question_id = {qid}
# question_title = {title}
# question_title_slug = {title_slug}
# question_difficulty = {difficulty}
# question_category = {category}
# question_likes = {likes}
# question_dislikes = {dislikes}"""
        out += f"\n\n{meta_block}"

        file_name = f"LeetCodeContests_{qid}_{title_slug}.py"
        if args.difficulty is not None and difficulty.lower() != args.difficulty.lower():
            continue

        # sanity check with parsing
        try:
            ast.parse(out)
        except Exception as e:
            print(f"Failed to parse {file_name}: {e}")
            continue

        with open(outdir / file_name, "w") as f:
            f.write(out)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-json", type=str, required=True)
    parser.add_argument("--output-dir", type=str, required=True)
    parser.add_argument("--difficulty", type=str, default=None)
    args = parser.parse_args()
    main(args)
