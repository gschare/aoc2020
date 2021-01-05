import sys

def split_passports(text):
    return text.split("\n\n")

def validate_passport(passport):
    info = {
        "byr":"",
        "iyr":"",
        "eyr":"",
        "hgt":"",
        "hcl":"",
        "ecl":"",
        "pid":"",
        "cid":""
    }
    passport = passport.replace("\n", " ")
    passport = passport.strip()
    print(passport.encode())
    for entry in passport.split(" "):
        key, _, value = entry.partition(":")
        info[key] = value
    if any(v=="" and k!="cid" for k,v in info.items()):
        print("bad, missing:", [k for k,v in info.items() if v==""])
        return False
    print("good")
    return True

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
