- `translate.py` translates a [MultiPL-E](https://github.com/nuprl/MultiPL-E) dataset from Python to Racket
  using the OpenAI API. OPENAI_API_KEY must be set as an environment variable.
- `process_translated_results.py` processes the translated results from `translate.py`
  by filtering out failures and normalizing purpose statements.
  Saves the results to a directory.
- `leetcode_hard_processed/` contains the processed results of translating the hard
  problems from the LeetCode dataset.
  The problems were translated using GPT-4-Turbo with temperature 0.8 at 20
  translations per problem. It's roughly 57% of the original dataset. Given more attempts, more translations are possible.
