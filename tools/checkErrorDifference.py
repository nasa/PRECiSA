#!/usr/bin/env python3

import argparse
from decimal import Decimal
import logging
import math
import sys


def get_least_significant_exponent(n, verbose=False):
    """
    Computes the exponent of the least significant non-zero digit of a number.

    Args:
        n: The fractional number (as a string, float, or Decimal).

    Returns:
        The integer exponent of the least significant non-zero digit.
        Returns None for zero.
    """
    if verbose:
        logging.debug(f"get_least_significant_exponent(n): n = {n}")

    # Use the Decimal module for precise arithmetic
    if isinstance(n, float):
        num = Decimal(str(n))
    else:
        num = Decimal(n)

    if num == 0:
        return None

    # Get the decimal representation as a string
    s_num = f"{num:.20f}"

    if verbose:
        logging.debug(f"num (type): {num} ({type(num)})")
        logging.debug(f"s_num: {s_num}")

    # If the number is a whole number, the last non-zero digit is found differently
    if "." not in s_num:
        s_num = s_num.rstrip("0")
        exponent = len(s_num) - 1 - s_num.rfind("0") if "0" in s_num else 0

        if verbose:
            logging.debug(f"(return) exponent: {exponent}")

        return exponent

    # Extract the fractional part of the string
    fractional_part = s_num.split(".")[-1]

    if verbose:
        logging.debug(f"fractional_part: {fractional_part}")
        logging.debug(f"len(fractional_part): {len(fractional_part)}")

    # Find the index of the last non-zero digit in the fractional part
    last_nonzero_index = -1
    for i in range(len(fractional_part) - 1, -1, -1):
        if verbose:
            logging.debug(f"i: {i} -> fractional_part[i]: {fractional_part[i]}")

        if fractional_part[i] != "0":
            last_nonzero_index = i
            break
    if verbose:
        logging.debug(f"last_nonzero_index: {last_nonzero_index}")

    # If a non-zero digit was found, compute its exponent
    if last_nonzero_index != -1:
        # The exponent is the negative of the number of digits after the decimal point
        # including the non-zero one.
        exponent = -(last_nonzero_index + 1)
        return exponent
    else:
        # This case handles numbers like 0.000, where no non-zero digit exists
        # in the fractional part. We need to check the whole number part.

        # Split the string representation at the decimal point
        parts = s_num.split(".")
        whole_part = parts[0]

        if whole_part.lstrip("-0") == "":
            if verbose:
                logging.debug(f"(return) None (because the number is 0)")

            return None

        # Find the last non-zero digit in the integer part
        whole_part_stripped = whole_part.rstrip("0")

        exponent = len(whole_part) - len(whole_part_stripped)

        if verbose:
            logging.debug(f"whole_part_stripped: {whole_part_stripped}")
            logging.debug(f"(return) exponent: {exponent}")

        return exponent


def is_diff_less_than_exponent_precision(a, b, exponent_precision, verbose=False):
    abs_tol = 10**exponent_precision

    if verbose:
        logging.debug(f"a: {a:.18f}")
        logging.debug(f"b: {b:.18f}")
        logging.debug(f"abs_tol: {abs_tol}")

    return math.isclose(a, b, abs_tol=abs_tol)


parser = argparse.ArgumentParser(
    description="Checks the second error to be within a one-unit distance of the less significative digit of the first error"
)
parser.add_argument("first_error", type=float, help="The first error.")
parser.add_argument("second_error", type=float, help="The second error.")
parser.add_argument("-v", "--verbose", action='store_true', help="Verbose output.")
args = parser.parse_args()

first_error = args.first_error
second_error = args.second_error
verbose = args.verbose

if verbose:
    logging.basicConfig(level=logging.DEBUG)
    logging.debug(f"first_error: {first_error}")
    logging.debug(f"second_error: {second_error}")
    logging.debug(f"verbose: {verbose}")

least_significant_exponent = get_least_significant_exponent(first_error, verbose=verbose)

if verbose:
    logging.debug(f"least_significant_exponent: {least_significant_exponent}")

if not is_diff_less_than_exponent_precision(
    first_error, second_error, least_significant_exponent
):
    print(f"{first_error} and {second_error} are very different", file=sys.stderr)
    sys.exit(1)
