"""
Translates a MultiPL-E dataset from Python to Racket using OpenAI's chat completions API.
"""

import openai
import random
import os
from pathlib import Path
from typing import List, Dict
from tqdm import tqdm
from utils import gunzip_json_read, jsonl_reader, gunzip_json_write, markdown_codeblock_extract


def make_convo_prompt(original: str, prompt: str) -> List[Dict[str, str]]:
    return [
        {
            "role": "system",
            "content": "You are a helpful programming assistant designed to complete code snippets in Racket."
        },
        {
            "role": "user",
            "content": f"""
I have the following Python code that I'd like to convert to **idiomatic** Racket.
Only provide the translated function in Racket, no additional code is needed.
Make sure to write inline comments to explain the code.
Can you help me with that?
```python
{original}
```

Please generate code to complete the following problem in Racket given the implementation of the function above.
Make sure to be as idiomatic as possible; avoid mutation if you can, and try to use higher-order functions where appropriate.
Here is the signature of the function in Racket:
```racket
{prompt}
```"""
        },
    ]


def get_original(example, completed_originals=None):
    if completed_originals:
        path = Path(completed_originals) / f"{example['name']}.results.json.gz"
        if not path.exists():
            print(
                f"Skipping {example['name']} -- does not exist in completed originals")
            return None
        obj = gunzip_json_read(path)
        assert obj is not None
        # get passing results
        passing = []
        for r in obj["results"]:
            if r["status"] == "OK":
                passing.append(r["program"].split(
                    "def check(candidate):")[0].strip())
        # randomly select one of the passing results
        if passing:
            original = random.choice(passing)
        else:
            print(
                f"Skipping {example['name']} -- no passing results in completed originals")
            return None
    else:
        original_path = Path(example["original"])
        if args.processed:
            processed_path = Path(args.processed) / f"{example['name']}"
            if processed_path.exists():
                print(
                    f"Skipping {example['name']} -- exists in processed dataset")
                return None

        # check if the file exists -- report nicely if it doesn't
        if not original_path.exists():
            raise FileNotFoundError(
                f"Original file {original_path} does not exist. Make sure the dataset is locally generated.")

        with open(original_path, "r") as f:
            original = f.read().split("### Unit tests below ###")[
                0].replace("    ### Canonical solution below ###\n", "").strip()

    return original


def main(args):
    random.seed()
    # NOTE: very important. the dataset should be locally generated, because the
    # "original" field points to the file path of the original text
    data = list(jsonl_reader(args.input))
    client = openai.Client(api_key=os.environ["OPENAI_API_KEY"])
    print(f"Loaded {len(data)} examples from {args.input}")
    # make output directory
    output_dir = Path(args.output)
    output_dir.mkdir(parents=True, exist_ok=True)
    for example in tqdm(data):
        original = get_original(example, args.completed_originals)
        if not original:
            continue
        output_path = output_dir / f"{example['name']}.json.gz"
        prompt = example["prompt"]
        convo_prompt = make_convo_prompt(original, prompt)
        response = client.chat.completions.create(
            model=args.model,
            messages=convo_prompt,  # type: ignore
            max_tokens=args.max_tokens,
            temperature=args.temperature,
            top_p=args.top_p,
            n=args.n,
        )
        completions = []
        for choice in response.choices:
            content = choice.message.content
            if content:
                new = markdown_codeblock_extract(content).strip()
                completions.append(new)

        result = {
            "name": example["name"],
            "language": "rkt",
            "temperature": args.temperature,
            "top_p": args.top_p,
            "original": original,
            "original_prompt": prompt,
            "prompt": "",  # dummy; becuase of chat completions and docker executor
            "completions": completions,
            "tests": example["tests"],
            # signals start of another top-level expression
            "stop_tokens": ["\n("]
        }
        gunzip_json_write(output_path, result)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path, required=True,
                        help="Path to the input dataset")
    parser.add_argument("--output", type=str, required=True,
                        help="Output directory to write the translated examples")
    parser.add_argument("--processed", type=str, default=None,
                        help="Path to the processed dataset if it exists. Can be used to only generate the missing examples.")
    parser.add_argument(
        "--model", type=str, default="gpt-4-turbo-preview", help="OpenAI chat model to use")
    parser.add_argument(
        "--completed_originals", type=str, default=None, help="Path to the completed originals dataset")
    parser.add_argument("--temperature", type=float,
                        default=0.45, help="Temperature for sampling")
    parser.add_argument("--top-p", type=float, default=0.95,
                        help="Top-p for sampling")
    parser.add_argument("--max-tokens", type=int,
                        default=2048, help="Max tokens to generate")
    parser.add_argument("--n", type=int, default=20,
                        help="Number of completions per example")
    args = parser.parse_args()
    main(args)
