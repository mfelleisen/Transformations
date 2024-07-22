import os
import random
from pathlib import Path
from typing import List, Literal, Union
from utils import chunkify, gunzip_json_write
from model import model_factory
from tqdm import tqdm
from code_exec_server.code_exec_reqs import exec_test_batched

THIS_DIR = os.path.dirname(os.path.abspath(__file__))


FEW_SHOT_PROGS = [
"""
(define (find-peaks mountain)
  (define len (length mountain))
  (if (< len 3)
      '()
      (let loop ((i 1)
                 (peaks '()))
        (if (>= i (- len 1))
            (reverse peaks)
            (if (and (> (list-ref mountain i) (list-ref mountain (- i 1)))
                     (> (list-ref mountain i) (list-ref mountain (+ i 1))))
                (loop (+ i 1) (cons i peaks))
                (loop (+ i 1) peaks))))))
""".strip(),
"""
(define (max-jump nums target)
  (define n (length nums))
  (define dp (cons 0 (make-list (sub1 n) -inf.0)))
  (define (update-dp i dp)
    (when (not (= (list-ref dp i) -inf.0))
      (for ([j (in-range (add1 i) n)])
        (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
          (set! dp (list-set dp j (max (list-ref dp j) (add1 (list-ref dp i))))))))
    dp)
  (for ([i (in-range n)])
    (set! dp (update-dp i dp)))
  (define final-value (list-ref dp (sub1 n)))
  (if (= final-value -inf.0) #false (inexact->exact final-value)))
""".strip(),
]

FEW_SHOT_TESTS = [
    [
      "(check-equal? (peaks (list 1 4 3 8 5)) (list 1 3))",
      "(check-equal? (peaks (list 1 4 1)) (list 1))",
      "(check-equal? (peaks (list 1 4 3)) (list 1))",
      "(check-equal? (peaks (list 1 6 4)) (list 1))",
      "(check-equal? (peaks (list 3 5 1)) (list 1))",
      "(check-equal? (peaks (list 3 5 4)) (list 1))",
      "(check-equal? (peaks (list 1 3 6 5)) (list 2))",
      "(check-equal? (peaks (list 1 8 1 8)) (list 1))",
      "(check-equal? (peaks (list 2 3 7 6)) (list 2))",
    ],
    [
      "(check-equal? (max-jump (list 1 3 6 4 1 2) 3) 5)",
      "(check-equal? (max-jump (list 1 3 6 4 1 2) 0) #false)",
      "(check-equal? (max-jump (list 0 1) 0) #false)",
      "(check-equal? (max-jump (list 0 1) 1) 1)",
      "(check-equal? (max-jump (list 1 0) 0) #false)",
      "(check-equal? (max-jump (list 1 0) 2) 1)",
      "(check-equal? (max-jump (list 0 1 2) 0) #false)",
      "(check-equal? (max-jump (list 0 1 2) 1) 2)",
      "(check-equal? (max-jump (list 0 1 2) 2) 2)",
      "(check-equal? (max-jump (list 0 1 2) 3) 2)",
      "(check-equal? (max-jump (list 0 2 1) 0) #false)",
      "(check-equal? (max-jump (list 0 2 1) 1) 1)",
    ]
]


Classification = Literal["hi", "lo"]
Sexp = List[Union[str, List["Sexp"]]]
Format = Literal["input", "output"]

def parse_sexp(text: str) -> Sexp:
    stack = []
    current_list = []
    token = ''
    in_string = False

    i = 0
    while i < len(text):
        char = text[i]

        if char == '"' and (i == 0 or text[i-1] != '\\'):
            if not in_string:
                in_string = True
                token += char
            else:
                in_string = False
                token += char
                current_list.append(token)
                token = ''
        elif in_string:
            token += char
        elif char == '(':
            if token:
                current_list.append(token)
                token = ''
            stack.append(current_list)
            current_list = []
        elif char == ')':
            if token:
                current_list.append(token)
                token = ''
            if stack:
                temp = current_list
                current_list = stack.pop()
                current_list.append(temp)
        elif char.isspace():
            if token:
                current_list.append(token)
                token = ''
        else:
            token += char

        i += 1

    if token:
        current_list.append(token)

    return current_list[0] if current_list else []


def sexp_to_string(sexp: Sexp) -> str:
    if isinstance(sexp, list):
        if not sexp:
            return "()"
        elements = [sexp_to_string(elem) for elem in sexp]
        return f"({' '.join(elements)})"
    elif isinstance(sexp, str):
        if any(char.isspace() or char in '()' for char in sexp):
            return f'"{sexp}"'
        return sexp
    else:
        return str(sexp)


class TestCase:
    def __init__(self, original: str, inputs: Sexp, output: Sexp):
        self.original = original
        self.inputs = inputs
        self.output = output


    @classmethod
    def from_str(cls, s: str) -> "TestCase":
        sexp = parse_sexp(s)
        inps = sexp[1][1:]
        exp = sexp[2]
        return cls(s, inps, exp)

    def __str__(self):
        return f"({sexp_to_string(self.inputs)} {sexp_to_string(self.output)})"

    def __repr__(self):
        return f"TestCase({str(self)})"

    def format_for_predict_input(self, entrypoint: str) -> str:
        return f"(check-equal? {sexp_to_string(self.output)} ({entrypoint} "

    def format_for_prior_input(self, entrypoint: str) -> str:
        return f"(check-equal? {sexp_to_string(self.output)} {sexp_to_string([entrypoint] + self.inputs)})"

    def format_for_predict_output(self, entrypoint: str) -> str:
        return f"(check-equal? {sexp_to_string([entrypoint] + self.inputs)} "

    def format_for_prior_output(self, entrypoint: str) -> str:
        return f"(check-equal? {sexp_to_string([entrypoint] + self.inputs)} {sexp_to_string(self.output)})"

    def format_for_predict(self, format: Format, entrypoint: str) -> str:
        if format == "input":
            return self.format_for_predict_input(entrypoint)
        elif format == "output":
            return self.format_for_predict_output(entrypoint)
        else:
            raise ValueError(f"Unknown format: {format}")

    def format_for_prior(self, format: Format, entrypoint: str) -> str:
        if format == "input":
            return self.format_for_prior_input(entrypoint)
        elif format == "output":
            return self.format_for_prior_output(entrypoint)
        else:
            raise ValueError(f"Unknown format: {format}")


class Result:
    def __init__(self, to_predict: TestCase, completion: str, correct: bool, num_examples: int):
        self.to_predict = to_predict
        self.completion = completion
        self.correct = correct
        self.num_examples = num_examples

    def to_dict(self):
        return {
            "to_predict": str(self.to_predict),
            "completion": self.completion,
            "correct": self.correct,
            "num_examples": self.num_examples
        }

class Example:
    def __init__(
            self,
            original: str,
            code: str,
            description: str,
            tests: List[TestCase],
            entrypoint: str,
            classification: Classification
    ):
        self.original = original
        self.code = code
        # NOTE: description is in Racket ";;" comments and prefixed with "#lang racket"
        self.description = description
        self.tests = tests
        # NOTE: footer is simply "))"
        self.entrypoint = entrypoint  # name of the function to call
        self.classification = classification
        self.results = []

    @classmethod
    def from_code(cls, code: str, classification: Classification) -> "Example":
        # step 1: extract tests and parse them out
        og = code
        footer = "(require rackunit)"
        codesplit = code.split(footer)
        code = codesplit[0].strip()
        tests_raw = (footer + codesplit[1]).strip()
        testsplit = tests_raw.split("))")
        tests_header = testsplit[0].strip() + "))"
        entrypoint = tests_header.split("candidate")[1].split(")")[0].strip()
        tests_raw = "))".join(testsplit[1:-1]).strip()
        tests_raw = tests_raw.split("\n")
        tests = []
        for test in tests_raw:
            tests.append(TestCase.from_str(test))
        # step 2: extract description and clip code
        base_description = "#lang racket\n"
        description = base_description
        nlsplit = code.split("\n")
        for i, line in enumerate(nlsplit):
            if line.startswith(";"):
                description += line + "\n"
            elif description == base_description:  # skip over initial whitespace
                continue
            else:
                description = description.strip()
                code = "\n".join(nlsplit[i:]).strip()
                break
        assert description != base_description, "Could not find description in " + code

        # step 3: convert tab to spaces, remove any comments (leading and trailing from code) or empty lines
        code = code.replace("\t", "  ")
        nlsplit = code.split("\n")
        newcode = ""
        for i, line in enumerate(nlsplit):
            if line.startswith(";"):
                continue
            else:
                # see if we can find a comment in the line
                comment_idx = line.find(";")
                if comment_idx != -1:
                    line = line[:comment_idx].rstrip()
            if line.strip() != "":
                newcode += line + "\n"

        code = newcode.strip()
        return cls(og, code, description, tests, entrypoint, classification)

    def to_dict(self):
        return {
            "original": self.original,
            "code": self.code,
            "description": self.description,
            "tests": [str(test) for test in self.tests],
            "entrypoint": self.entrypoint,
            "classification": self.classification,
            "results": [result.to_dict() for result in self.results]
        }


class HiLoItem:
    def __init__(self, name: str, his: List[Example], los: List[Example]):
        self.name = name
        self.his = his
        self.los = los

    def to_dict(self):
        return {
            "name": self.name,
            "his": [example.to_dict() for example in self.his],
            "los": [example.to_dict() for example in self.los]
        }

class HiLoDataset:
    def __init__(self, path: Path):
        items = []
        for d in path.iterdir():
            if d.is_dir():
                name = d.stem
                his = []
                los = []
                for hi_path in d.rglob("hi/*.rkt"):
                    his.append(Example.from_code(hi_path.read_text(), "hi"))
                for lo_path in d.rglob("lo/*.rkt"):
                    los.append(Example.from_code(lo_path.read_text(), "lo"))
                if len(his) == 0 or len(los) == 0:
                    continue
                items.append(HiLoItem(name, his, los))
        print(f"Loaded {len(items)} items")
        self.items: List[HiLoItem] = items

    def to_dict(self):
        return {
            "items": [item.to_dict() for item in self.items]
        }

END_FOOTER = ";;;;;;;;; END PROGRAM ;;;;;;;;;;"

def format_few_shot(example: Example, priors: List[TestCase], to_predict: TestCase, fmt: Format) -> str:
    buf = "#lang racket\n\n(require rackunit)\n\n" # just to not make it freak out
    num_tests = len(priors) + 1
    tests_header = f"; {num_tests} test case{'' if num_tests == 1 else 's'} below\n"
    for few_code, few_tests in zip(FEW_SHOT_PROGS, FEW_SHOT_TESTS):
        buf += few_code + "\n\n"
        buf += tests_header
        pool = [TestCase.from_str(test) for test in random.sample(few_tests, num_tests)]
        test = pool[0]
        few_priors = pool[1:]
        for prior in few_priors:
            buf += prior.format_for_prior(fmt, example.entrypoint) + "\n"
        buf += test.format_for_prior(fmt, example.entrypoint) + "\n"
        buf += f"\n{END_FOOTER}\n\n"

    buf += example.code + "\n\n"
    buf += tests_header
    for prior in priors:
        buf += prior.format_for_prior(fmt, example.entrypoint) + "\n"
    buf += to_predict.format_for_predict(fmt, example.entrypoint)
    
    return buf

    

def main(args):
    random.seed(42)
    ds = HiLoDataset(Path(THIS_DIR) / "golden")
    if args.sample is not None:
        ds.items = ds.items[:args.sample]

    model = model_factory(args.model, args.model_kind)
    
    for item in tqdm(ds.items):
        for example in item.los + item.his:
            batch = {
                "prompts": [], # actual prompts
                "to_predict": [], # to_predict for each prompt
                "examples": [], # num examples for each prompt
                "responses": [], # responses for each prompt
                "correct": [] # whether each response is correct
            }
            # get prompts
            for _ in range(args.completion_limit): # NOTE: loops because of random sampling
                for p in range(0, args.max_priors+1):
                    pool = random.sample(example.tests, p+1)
                    to_predict = pool[0]
                    priors = pool[1:]
                    prompt = format_few_shot(example, priors, to_predict, args.format)
                    batch["prompts"].append(prompt)
                    batch["examples"].append(p)
                    batch["to_predict"].append(to_predict)

            # generate completions
            for mini_batch in chunkify(batch["prompts"], args.batch_size):
                batch["responses"].extend(model.generate(
                    mini_batch,
                    batch_size=args.batch_size,
                    max_tokens=args.max_tokens,
                    temperature=args.temperature,
                    top_p=args.top_p,
                    stop=[END_FOOTER]
                ))

            # evaluate completions
            batch["correct"] = [False] * len(batch["responses"])
            for indices in chunkify(range(len(batch["responses"])), os.cpu_count()):
                resps = [batch["responses"][i] for i in indices]
                to_predicts = [batch["to_predict"][i] for i in indices]
                codes = []
                for resp, to_predict in zip(resps, to_predicts):
                    code = "#lang racket\n\n(require rackunit)\n\n"
                    code += example.code + "\n\n"
                    code += to_predict.format_for_predict(args.format, example.entrypoint) + resp
                    codes.append(code)
                results = exec_test_batched(args.executor, codes, [""] * len(codes), lang="racket")
                correct = []
                for p, out in results:
                    if not p or "FAILURE" in out or "ERROR" in out:
                        correct.append(False)
                    else:
                        correct.append(True)
                for i, c in enumerate(correct):
                    batch["correct"][indices[i]] = c

            for resp, to_predict, correct, examples in zip(batch["responses"], batch["to_predict"], batch["correct"], batch["examples"]):
                example.results.append(Result(to_predict, resp, correct, examples))

    d = ds.to_dict()
    # dump args that we care about
    d["model"] = args.model
    d["max_priors"] = args.max_priors
    d["format"] = args.format
    d["temperature"] = args.temperature
    d["top_p"] = args.top_p
    d["max_tokens"] = args.max_tokens
    d["completion_limit"] = args.completion_limit
    outpath = Path(args.output)
    outpath.parent.mkdir(parents=True, exist_ok=True)
    gunzip_json_write(outpath, d)
    print(f"Wrote to {outpath}")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", type=str, required=True, help="Model to evaluate")
    parser.add_argument("--output", type=str, required=True, help="Output file")
    parser.add_argument("--model-kind", type=str, default="hf", choices=["hf"], help="Model kind")
    parser.add_argument("--batch-size", type=int, default=512, help="Batch size")
    parser.add_argument("--format", type=str, default="output", choices=["input", "output"], help="Format for predictions")
    parser.add_argument("--temperature", type=float, default=0.2, help="Temperature")
    parser.add_argument("--top-p", type=float, default=0.95, help="Top-p")
    parser.add_argument("--max-tokens", type=int, default=128, help="Max tokens")
    parser.add_argument("--max-priors", type=int, default=5, help="Max test examples drawn from previous ground truth I/O")
    parser.add_argument("--completion-limit", type=int, default=200, help="Max completions per test")
    parser.add_argument("--sample", type=int, default=None, help="Evaluate a sample of N only")
    parser.add_argument("--executor", type=str,
                        default="http://127.0.0.1:8000",
                        help="Code exec server URL")
    args = parser.parse_args()
    main(args)
