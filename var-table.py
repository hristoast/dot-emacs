#!/usr/bin/env python3
import os
import sys

this_dir = os.path.abspath(os.path.dirname(sys.argv[0]))
var_list = []
pkg_specific_list = []


def main():
    with open(os.path.join(this_dir, "init.el"), "r") as f:
        init_lines = f.readlines()

    for line in init_lines:
        if "EMACS_NO_" in line:
            varname = line.split('"')[1]
            filename = line.split('"')[3]
            var_list.append({"var": varname, "file": filename})

    for var_dict in var_list:
        el_file = os.path.join(this_dir, var_dict["file"] + ".el")
        pkg_list = []
        with open(el_file, "r") as f:
            lines = f.readlines()

        var_dict.update({"comment": lines[0].split("---")[-1].strip().rstrip()})

        for line in lines:
            if "EMACS_NO_" in line:
                pkg_specific_list.append(
                    {
                        "var": line.split('"')[1],
                        "file": el_file,
                        "comment": line.split(" ;; ")[-1].rstrip().strip(),
                    }
                )

            if "(use-package " in line:
                pkg_list.append(
                    line.split("(use-package ")[1]
                    .split(" ")[0]
                    .strip()
                    .rstrip()
                    .rstrip(")")
                )
                var_dict.update({"packages": pkg_list})

    for v in var_list:
        if "packages" in v.keys():
            print(
                "`{var}` | Packages: {desc}".format(
                    var=v["var"], desc=", ".join(v["packages"])
                )
            )
        else:
            print("`{var}` | {desc}".format(var=v["var"], desc=v["comment"]))

    for v in pkg_specific_list:
        print("`{var}` | {desc}".format(var=v["var"], desc=v["comment"]))


if __name__ == "__main__":
    main()
