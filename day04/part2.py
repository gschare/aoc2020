import sys
import re

def split_passports(text):
    return text.split("\n\n")

def validate_byr(val):
    if len(val) != 4 or not val.isnumeric():
        return False
    val = int(val)
    return 1920 <= val <= 2002

def validate_iyr(val):
    if len(val) != 4 or not val.isnumeric():
        return False
    val = int(val)
    return 2010 <= val <= 2020

def validate_eyr(val):
    if len(val) != 4 or not val.isnumeric():
        return False
    val = int(val)
    return 2020 <= val <= 2030

def validate_hgt(val):
    if val=="":
        return False

    match = re.match("^([0-9]+)([a-z]+)$", val)
    if not match:
        return False
    value, unit = match.group(1,2)

    if unit=="cm":
        return 150<=int(value)<=193
    elif unit=="in":
        return 59<=int(value)<=76
    else:
        return False

def validate_hcl(val):
    if val=="":
        return False
    if len(val) != 7:
        return False

    match = re.match("^#[0-9a-f]+$", val)
    return True if match else False

def validate_ecl(val):
    colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    return val in colors

def validate_pid(val):
    return len(val)==9 and val.isnumeric()

def validate_cid(val):
    return True

def validate_passport(passport):
    validation_dispatch = {
        "byr":validate_byr,
        "iyr":validate_iyr,
        "eyr":validate_eyr,
        "hgt":validate_hgt,
        "hcl":validate_hcl,
        "ecl":validate_ecl,
        "pid":validate_pid,
        "cid":validate_cid
    }

    info = {k:False for k in validation_dispatch.keys()}
    info["cid"] = True

    passport = passport.replace("\n", " ")
    passport = passport.strip()

    for entry in passport.split(" "):
        key, _, value = entry.partition(":")
        info[key] = validation_dispatch[key](value)
    return all(v==True for v in info.values())

def main():
    if len(sys.argv) != 2:
        raise Exception("missing command line argument: no input file given")

    with open(sys.argv[1], "r") as fp:
        text = fp.read()

    passports = split_passports(text)
    count = 0
    for passport in passports:
        count += validate_passport(passport)

    print('{}/{}'.format(count, len(passports)))

if __name__ == "__main__":
    main()
