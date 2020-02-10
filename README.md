Json Util
=========

Description
-----------

CLI Utility which helps to format JSON files.

Usage
-----

```bash
# Print help
jsonutil --help

# Print program verson
jsonutil --version

# Read json file and pretty print its content to STDOUT
jsonutil --input file.json --pretty

# Read json file and pretty print its content to other file
jsonutil --input file.json --output pretty.json --pretty

# Read json file and print its minified content to other file
jsonutil --input file.json --output min.json
```

Command Line Arguments
----------------------

- help : Prints help.
- input : Defines path to JSON file to read.
- output : Defines path to JSON file to write.
- pretty : Sets output format to [pretty printing].
- version : Prints program version.

