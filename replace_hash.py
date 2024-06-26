"""This scipt is used to replace specified placeholders(string) in a file(stored under ./output) with hashed random strings.
Usage:
      (bash) python3 replace_hash.py <./output/filename>
"""

import re
import random
import string
import sys
from hashlib import sha256

def in_file_replace(filepath, hash_placeholders_list=[f"__HASH__PLACEHOLDER__", f"__SOME_HASH__"]):
  """Replace specified placeholders in a file.

  This function replaces all hash placeholders in a file with an actual hash of a secret.

  Args:
      filepath (str): The path of the file (under ./output) to modify.
      hash_placeholders_list (list, optional): A list of placeholders to be replaces, default as
          ["__HASH__PLACEHOLDER__", "__SOME_HASH__"].

  Raises:
      FileNotFoundError: If the specified file is not found.
  
  """
  try:
    with open(filepath, "r") as f:
      racket_code = f.read()

    modified_code = replace_strings(racket_code, hash_placeholders_list)

    with open(filepath, "w") as f:
      f.write(modified_code)

    print(f"Successfully modified strings in '{filepath}'.")

  except FileNotFoundError:
    print(f"Error: File '{filepath}' not found.")


def replace_strings(racket_code, placeholders):
  """Replaces specified placeholders in a string of Racket code with hashed random strings.

  Args:
          racket_code(str): The Racket code string to be modified.
          placeholders(list): A list of placeholder strings to be replaced.

  Returns:
          result(str): The modified racket code, with placeholders replaced by hashed random strings.
  """
  result = ""
  for line in racket_code.splitlines():
    for p in placeholders:
      matches = re.findall(p, line)  
      for match in matches:
        random_string = generate_random_string()    
        hash_string = sha256(random_string.encode('utf-8')).hexdigest()   
        line = line.replace(p, hash_string)
    
    result += line + "\n" 
  return result


def generate_random_string(result_length=30):
  """Generates a random string of the specified length

  Args:
          result_length(int, default 30): Desired length of the generated random string.

  Returns:
          result(str): A random generated string, containig letters, digits and special symbols.
  """
  result = ''
  sample_alphabet = string.ascii_letters + string.digits + '!@#$%^&*()-+=.'
  result = result.join(random.sample(sample_alphabet, result_length))
  return result


"""Execute as main module (only if run as a script)

Read arguments in bash. If file path not given, program exits with error message. 
Otherwise it executes the hash replacing function.
"""
if __name__ == "__main__":
  args = sys.argv

  if (len(args) < 2):
    sys.exit("No file path given!")
  param_file_path = args[1] 

  in_file_replace(param_file_path)