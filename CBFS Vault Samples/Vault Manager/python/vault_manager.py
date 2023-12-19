# 
# CBFS Vault 2022 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of CBFS Vault in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.callback.com/cbfsvault
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from cbfsvault import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

import fnmatch
import math
import os
import errno
import sys

# Global variables, used later. They are pre-set here to check if they have been changed via user-passed parameters
from six import unichr

command = ""
vault_path = ""
files = list()
processed = list()

opt_lowercase = False
opt_uppercase = False
opt_delete_after_archiving = False
opt_compression_level = 0
opt_exclude_paths = False
opt_vault_fixed_size = 0
opt_vault_page_size = 0
opt_password = ""
opt_output_path = ""
opt_recursive = False
opt_overwrite_vault = None
opt_always_yes = False
opt_overwrite_files = None

vault = None

buffer_size = 1024 * 1024  # 1 MB


# ---------------------------------
#        Helper functions
# ---------------------------------


def six_input(prompt):
    if sys.version_info[0] == 3:
        return input(prompt)
    else:
        return raw_input(prompt)


def to_char(value):
    if sys.version_info[0] == 3:
        return str(chr(value))
    else:
        return str(unichr(value))


def is_empty(s):
    if s is None:
        return True
    if type(s) is not str:
        s = str(s)
    return len(s) == 0


def ask_yes_no_all(prompt):
    """Asks the given question and returns 'y' for YES answer, 'n' for NO answer and 'a' for ALL"""
    while True:
        answer = six_input(prompt + "(Yes/No/All): ").lower()
        if answer == 'y' or answer == "yes":
            return 'y'
        if answer == 'n' or answer == "no":
            return 'n'
        if answer == 'a' or answer == "all":
            return 'a'
        print("Unrecognized answer '%s'" % answer)


def set_opt_lowercase():
    global opt_lowercase
    opt_lowercase = True
    if opt_uppercase:
        print("ERROR: -l and -u switches may not be specified together")
        return False
    return True


def set_opt_uppercase():
    global opt_uppercase
    opt_uppercase = True
    if opt_lowercase:
        print("ERROR: -l and -u switches may not be specified together")
        return False
    return True


def set_opt_compression_level(value):
    global opt_compression_level
    if value.isnumeric():
        opt_compression_level = int(value)
        if 0 <= opt_compression_level <= 9:
            return True
    print("ERROR: -c switch (compression level) requires a numeric value 0-9 after 'c='.")
    return False


def set_opt_vault_fixed_size(value):
    global opt_vault_fixed_size
    if value.isnumeric():
        opt_vault_fixed_size = int(value) * 1024 * 1024
        if opt_vault_fixed_size > 0:
            return True
    print("ERROR: -f switch (fixed vault size) requires a positive numeric value in megabytes after 'f='.")
    return False


def set_opt_vault_page_size(value):
    global opt_vault_page_size
    if value.isnumeric():
        opt_vault_page_size = int(value)
        if (512 <= opt_vault_page_size <= 65536) and (math.modf(math.log(opt_vault_page_size, 2))[0] == 0.0):
            return True
    print("ERROR: -g switch (page size) requires a numeric value after 'g='."
          "Page size must be a power of 2 between 512 and 65536 bytes.")
    return False


def set_opt_password(value):
    global opt_password
    opt_password = value
    if not is_empty(opt_password):
        return True
    print("ERROR: -p switch (password) requires a password or passphrase after 'p='.")
    return False


def set_opt_output_path(value):
    global opt_output_path
    opt_output_path = convert_relative_path_to_absolute(value)
    if not is_empty(opt_output_path) and os.path.isdir(opt_output_path):
        return True
    print("ERROR: -o switch (output directory) requires the valid directory after 'o='.")
    return False


def split_switch(value):
    index = value.find("=")
    if index < 0:
        return tuple()
    if index == len(value) - 1:
        return value, ""
    return value[:index], value[index + 1:]


def to_vault_path(name):
    global vault
    global opt_lowercase
    global opt_uppercase

    if is_empty(name):
        return ""

    result = name
    if (len(result) > 2) and (result[1:2] == ':'):
        result = result[2:]
    vault_sep = to_char(vault.path_separator)
    result = result.replace(os.path.sep, vault_sep)
    if not result.startswith(vault_sep):
        result = vault_sep + result
    if opt_lowercase:
        result = result.lower()
    elif opt_uppercase:
        result = result.upper()
    return result


def is_bit_set(value, bit):
    return (value & bit) == bit


# ---------------------------------
#        Vault access functions
# ---------------------------------


def open_vault(must_exist):
    global vault
    global vault_path
    global opt_overwrite_vault
    global opt_vault_page_size
    global opt_vault_fixed_size

    vault = CBVault()
    vault.vault_file = vault_path
    try:
        if must_exist:
            vault.open_vault(VAULT_OM_OPEN_EXISTING, VAULT_JM_METADATA)
        else:
            if opt_overwrite_vault or not os.path.isfile(vault_path):
                if opt_vault_page_size > 0:
                    vault.page_size = opt_vault_page_size

                if opt_vault_fixed_size > 0:
                    vault.vault_size_max = opt_vault_fixed_size
                    vault.vault_size_min = opt_vault_fixed_size

                vault.open_vault(VAULT_OM_CREATE_ALWAYS, VAULT_JM_METADATA)
            else:
                vault.open_vault(VAULT_OM_OPEN_ALWAYS, VAULT_JM_METADATA)
        return True
    except CBFSVaultCbvaultError as err:
        print("ERROR: failed to create or open the vault\n  -> (%d) %s" % (err.code, err.message))
        vault = None
        return False


def close_vault():
    global vault
    if vault is None:
        exit()
    if vault.active:
        vault.close_vault(False)
    vault = None


# --------------------------------------
#    Vault files processing functions
# --------------------------------------


def delete_file(name, is_dir):
    global vault

    if is_dir:
        print("Deleting directory", name)
        if not vault.is_directory_empty(name):
            print(" - ERROR: the directory is not empty")
            return False
    else:
        print("Deleting file", name)

    try:
        vault.delete_file(name)
        print(" - OK")
        return True
    except CBFSVaultCbvaultError as err:
        print(" - ERROR: (%d) %s" % (err.code, err.message))
        return False


def create_os_directory(name):
    global vault
    global opt_exclude_paths
    global opt_output_path

    if opt_exclude_paths:
        return True

    print("Extracting directory", name)
    try:
        os_name = os.path.join(opt_output_path, name.replace(to_char(vault.path_separator), os.sep)[1:])
        if not os.path.exists(os_name):
            os.makedirs(os_name)
        print(" - OK")
        return True
    except IOError as err:
        print(" - ERROR: (%d) %s" % (err.errno, err.strerror))
        return False


def should_extract_file(filename):
    """Checks if the file exists; if so, checks the provided options or asks the user in order to
    make a decision if the file should be processed or skipped"""
    global vault
    global opt_always_yes
    global opt_overwrite_files

    if opt_overwrite_files or opt_always_yes or not os.path.isfile(filename):
        return True

    answer = ask_yes_no_all("File " + filename + "already exists. Overwrite?")
    if answer == 'y':
        return True
    if answer == 'a':
        opt_overwrite_files = True
        return True
    print("File %s skipped" % filename)
    return False


def extract_file_from_vault(name):
    global vault
    global opt_exclude_paths
    global opt_output_path

    os_name = name.replace(to_char(vault.path_separator), os.sep)
    if opt_exclude_paths:
        os_name = os.path.join(opt_output_path, os.path.basename(os_name))
    else:
        os_name = os.path.join(opt_output_path, os_name[1:])

    if not should_extract_file(os_name):
        return True

    print("Extracting file", name)
    try:
        source = vault.open_file(name, VAULT_FOM_OPEN_EXISTING, True, False, opt_password)
        try:
            dir_name = os.path.dirname(os_name)
            if not os.path.exists(dir_name):
                try:
                    os.makedirs(dir_name)
                except OSError as e:
                    if e.errno != errno.EEXIST:
                        raise

            dest = open(os_name, "wb")
            try:
                file_size = source.length
                read = 0
                while read < file_size:
                    buffer = source.read(buffer_size)
                    if (buffer is None) or (len(buffer) == 0):
                        break
                    dest.write(buffer)
                    print(".")
                    read += len(buffer)

                print(" - OK")
                return True
            finally:
                dest.close()
        finally:
            source.close()
    except CBFSVaultCbvaultError as err:
        print(" - ERROR: (%d) %s" % (err.code, err.message))
        return False
    except IOError as err:
        print(" - ERROR: (%d) %s" % (err.errno, err.strerror))
        return False


def extract_file(name, is_dir):
    if is_dir:
        return create_os_directory(name)

    return extract_file_from_vault(name)


def list_file(name, is_dir):
    global vault

    if is_dir:
        print("%-68s%12s" % (name, "<DIR>"))
    else:
        print("%-68s%12s" % (name, vault.get_file_size(name)))
    return True


def get_result_template(used):
    if used < 80:
        return "%" + str(80 - used) + "s"
    return " %s"


def test_file(name, is_dir):
    global vault
    global opt_password

    if is_dir:
        return True

    print(name, "")
    dots = 1
    try:
        stream = vault.open_file(name, VAULT_FOM_OPEN_EXISTING, True, False, opt_password)
        try:
            file_size = stream.length
            read = 0
            while read < file_size:
                buffer = stream.read(buffer_size)
                if (buffer is None) or (len(buffer) == 0):
                    break
                print(".")
                dots += 1
                read += len(buffer)

            print(get_result_template(len(name) + dots) % "OK")
            return True

        finally:
            stream.close()
    except CBFSVaultCbvaultError as err:
        print(get_result_template(len(name) + dots), "ERROR")
        print("  -> (%d) %s" % (err.code, err.message))
        return False
    except IOError as err:
        print(get_result_template(len(name) + dots), "ERROR")
        print("  -> (%d) %s" % (err.errno, err.strerror))
        return False


def process_child_files(path, mask, process_action):
    global vault

    handle = vault.find_first(path + to_char(vault.path_separator) + mask, VAULT_FATTR_FILE, 0)
    if handle != -1:
        try:
            while True:
                if not process_action(vault.get_search_result_full_name(handle), False):
                    return False
                if not vault.find_next(handle):
                    break
        finally:
            vault.find_close(handle)
    return True


def process_child_dirs(name, mask, reverse_order, process_action):
    global vault

    handle = vault.find_first(name + to_char(vault.path_separator) + "*", VAULT_FATTR_DIRECTORY, 0)
    if handle != -1:
        try:
            while True:
                new_name = vault.get_search_result_full_name(handle) + to_char(vault.path_separator) + mask
                if not process_vault_files(new_name, reverse_order, process_action):
                    return False
                if not vault.find_next(handle):
                    break
        finally:
            vault.find_close(handle)
    return True


def process_vault_files(name, reverse_order, process_action):
    global vault
    global opt_recursive

    splitted = split_path_with_mask(name, to_char(vault.path_separator))
    path = splitted[0]
    mask = splitted[1]

    if is_empty(mask):
        try:
            attrs = vault.get_file_attributes(path)
        except CBFSVaultCbvaultError as err:
            if err.code == VAULT_ERR_FILE_NOT_FOUND:
                print("ERROR: directory or file '%s' not found" % path)
                return False
            raise
        if not is_bit_set(attrs, VAULT_FATTR_DIRECTORY):
            # process the specified file
            return process_action(path, False)
        mask = "*"

    if not reverse_order:
        # process the directory itself
        if not is_empty(path):
            if not process_action(path, True):
                return False

        # process files in the directory (optionally filtered with a mask)
        if not process_child_files(path, mask, process_action):
            return False

        # process subdirectories if a recursive scan is requested
        if opt_recursive:
            if not process_child_dirs(path, mask, False, process_action):
                return False
    else:
        # process subdirectories if a recursive scan is requested
        if opt_recursive:
            if not process_child_dirs(path, mask, True, process_action):
                return False

        # process files in the directory (optionally filtered with a mask)
        if not process_child_files(path, mask, process_action):
            return False

        # process the directory itself
        if not is_empty(path):
            if not process_action(path, True):
                return False

    return True


# -----------------------------------
#    OS files processing functions
# -----------------------------------


def split_path_with_mask(path, separator):
    if ('*' not in path) and ('?' not in path):
        return path, ""

    index = path.rfind(separator)
    if index < 0:
        return "", path

    return path[:index], path[index + 1:]


def get_directories(path):
    """ Find all subdirectories in the directory, specified in path """
    return [entry for entry in os.listdir(path)
            if os.path.isdir(os.path.join(path, entry))]


def get_files(path, mask):
    """ Find all files matching the mask in the directory, specified in path """
    return [entry for entry in os.listdir(path)
            if fnmatch.fnmatch(entry, mask) and os.path.isfile(os.path.join(path, entry))]


def create_vault_directory(dirname):
    global vault

    print("Adding directory", dirname)
    try:
        vault.create_directory(to_vault_path(dirname), True)
        print(" - OK")
        return True
    except CBFSVaultCbvaultError as err:
        print(" - ERROR: (%d) %s" % (err.code, err.message))
        return False
    except IOError as err:
        print(" - ERROR: (%d) %s" % (err.errno, err.strerror))
        return False


def should_add_file(filename):
    """Checks if the file exists in the vault; if so, checks the provided options or asks the user in order to
    make a decision if the file should be processed or skipped"""
    global vault
    global opt_always_yes
    global opt_overwrite_files

    if opt_overwrite_files or opt_always_yes or not vault.file_exists(filename):
        return True

    answer = ask_yes_no_all("File " + filename + "already exists. Overwrite?")
    if answer == 'y':
        return True
    if answer == 'a':
        opt_overwrite_files = True
        return True
    print("File %s skipped" % filename)
    return False


def copy_file_to_vault(name):
    global vault
    global opt_exclude_paths
    global opt_password
    global opt_compression_level

    if opt_exclude_paths:
        vault_filename = to_vault_path(os.path.basename(name))
    else:
        vault_filename = to_vault_path(name)

    if not should_add_file(vault_filename):
        return True

    dir_name = to_vault_path(os.path.dirname(name))
    if not vault.file_exists(dir_name):
        print("Adding directory", dir_name)
        try:
            vault.create_directory(dir_name, True)
            print(" - OK")
        except CBFSVaultCbvaultError as err:
            print(" - ERROR: (%d) %s" % (err.code, err.message))
            return False
        except IOError as err:
            print(" - ERROR: (%d) %s" % (err.errno, err.strerror))
            return False

    print("Adding file", name)
    try:
        source = open(name, "rb")
        try:
            encryption = VAULT_EM_NONE
            if not is_empty(opt_password):
                encryption = VAULT_EM_DEFAULT
            compression = VAULT_CM_NONE
            if opt_compression_level > 0:
                compression = VAULT_CM_ZLIB

            dest = vault.open_file_ex(vault_filename, VAULT_FOM_CREATE_ALWAYS, False, True, False, True,
                                      encryption, opt_password, compression, opt_compression_level, 1)
            try:
                while True:
                    buffer = source.read(buffer_size)
                    if (buffer is None) or (len(buffer) == 0):
                        break
                    dest.write(buffer)
                    print(".")

                print(" - OK")
                return True
            finally:
                dest.close()
        finally:
            source.close()
    except CBFSVaultCbvaultError as err:
        print(" - ERROR: (%d) %s" % (err.code, err.message))
        return False
    except IOError as err:
        print(" - ERROR: (%d) %s" % (err.errno, err.strerror))
        return False


def add_file(name, is_directory):
    global opt_delete_after_archiving

    if is_directory:
        if not create_vault_directory(name):
            return False
    else:
        if not copy_file_to_vault(name):
            return False

    if opt_delete_after_archiving:
        processed.append(name)
    return True


def process_os_files(name, process_action):
    global opt_recursive

    splitted = split_path_with_mask(name, os.path.sep)
    path = splitted[0]
    mask = splitted[1]

    if is_empty(mask):
        if not os.path.isdir(path):
            # process the specified file
            return process_action(path, False)
        mask = "*"

    # process the directory itself
    if not process_action(path, True):
        return False

    # process files in the directory (optionally filtered with a mask)
    files_in_dir = get_files(path, mask)
    for file in files_in_dir:
        if not process_action(os.path.join(path, file), False):
            return False

    # process subdirectories if a recursive scan is requested
    if opt_recursive:
        subdirs = get_directories(path)
        for subdir in subdirs:
            new_name = os.path.join(path, subdir, mask)
            if not process_os_files(new_name, process_action):
                return False

    return True


# ---------------------------------
#           Main code
# ---------------------------------


def banner():
    print("CBFS Vault Manager Demo")
    print("Application for managing the contents of a CBFS Vault vault file.")
    print("Copyright (c) 2017-2023, Callback Technologies, Inc.")


def print_usage():
    print()
    print("Usage: %s <command> [-<switch 1> ... -<switch N>] <vault> [<files>...]\n" % os.path.basename(sys.argv[0]))
    print("<Commands>")
    print("  a             Add files to vault")
    print("  d             Delete files from vault")
    print("  e             Extract files from vault")
    print("  l             List contents of vault")
    print("  t             Test integrity of vault")
    print("  x             eXtract files with full pathname")

    print("<Switches>")
    print("  -             Stop switches scanning")

    print("  l             convert names to Lower case")
    print("  u             convert names to Upper case")
    print("  d             Delete files after archiving")
    print("  e             Exclude paths from names")
    print("  r             Recurse subdirectories")
    print("  s             overwrite exiSting vault")
    print("  y             assume Yes on all queries")
    print("  w             overWrite existing files")
    print("  c=<0-9>       set Compression level (0 - default)")
    print("  f=<size>      set the Fixed size of the vault in MB")
    print("  g=<size>      set size of the paGe for new vault (4096 - default)")
    print("  p=<password>  set Password")
    print("  o=<path>      set Output directory")


def handle_command():
    global command
    command = sys.argv[1]

    if (len(command) == 1) and (command in "adeltx"):
        return True

    print("ERROR: Invalid command -", command)
    print_usage()
    return False


def handle_switch(switch):
    global opt_delete_after_archiving
    global opt_exclude_paths
    global opt_recursive
    global opt_overwrite_vault
    global opt_always_yes
    global opt_overwrite_files

    # convert to lower case
    if switch == "-l":
        return set_opt_lowercase()

    # convert to upper case
    if switch == "-u":
        return set_opt_uppercase()

    # delete files after archiving
    if switch == "-d":
        opt_delete_after_archiving = True
        return True

    # exclude paths from names
    if switch == "-e":
        opt_exclude_paths = True
        return True

    # recurse subdirectories
    if switch == "-r":
        opt_recursive = True
        return True

    # overwrite existing vault
    if switch == "-s":
        opt_overwrite_vault = True
        return True

    # assume 'yes' for all queries
    if switch == "-y":
        opt_always_yes = True
        return True

    # overwrite existing files
    if switch == "-w":
        opt_overwrite_files = True
        return True

    if '=' in switch:
        parts = split_switch(switch)

        # compression level
        if parts[0] == "-c":
            return set_opt_compression_level(parts[1])

        # vault fixed size
        if parts[0] == "-f":
            return set_opt_vault_fixed_size(parts[1])

        # vault page size
        if parts[0] == "-g":
            return set_opt_vault_page_size(parts[1])

        # password
        if parts[0] == "-p":
            return set_opt_password(parts[1])

        # output path
        if parts[0] == "-o":
            return set_opt_output_path(parts[1])

    print("ERROR: unknown switch", switch)
    return False


def handle_parameters():
    global vault_path
    global files
    skip_switch = False

    for param in sys.argv[2:]:
        if param.startswith("-"):
            if not skip_switch:
                if param == "--":
                    skip_switch = True
                elif not handle_switch(param):
                    return False
        else:
            if is_empty(vault_path):
                vault_path = convert_relative_path_to_absolute(param)
            else:
                files.append(param)

    if not is_empty(vault_path):
        return True

    print("ERROR: no vault name specified")
    return False


def remove_dir(name):
    if not os.path.exists(name):
        return True
    try:
        os.rmdir(name)
    except OSError:
        return False
    return True


def delete_processed_files():
    global processed

    deletion_failed = False
    print("Deleting processed files")
    for name in reversed(processed):
        try:
            if os.path.isdir(name):
                if not remove_dir(name):
                    deletion_failed = True
            else:
                os.remove(name)
            print(".")
        except OSError:
            # skip directory or file removing
            deletion_failed = True
    if deletion_failed:
        print(" - not all files or directoires were removed successfully")
    else:
        print(" - OK")


def add_files():
    global files

    if not open_vault(False):
        return False
    try:
        for name in files:
            if not process_os_files(name, add_file):
                return False

        if opt_delete_after_archiving:
            delete_processed_files()

        return True
    finally:
        close_vault()


def delete_files():
    global opt_recursive

    if not open_vault(True):
        return False
    try:
        opt_recursive = True

        for name in files:
            if not process_vault_files(name, True, delete_file):
                return False

        return True
    finally:
        close_vault()


def extract_files(use_paths):
    global opt_recursive
    global opt_output_path
    global opt_exclude_paths

    opt_exclude_paths = not use_paths

    if is_empty(opt_output_path):
        opt_output_path = os.getcwd()
    else:
        opt_output_path = os.path.abspath(opt_output_path)

    if not open_vault(True):
        return False
    try:
        if len(files) == 0:
            files.append("*")
            opt_recursive = True

        for name in files:
            if not process_vault_files(name, False, extract_file):
                return False

        return True
    finally:
        close_vault()


def list_files():
    global files
    global opt_recursive

    if not open_vault(True):
        return False
    try:
        if len(files) == 0:
            files.append("*")
            opt_recursive = True

        print("%-68s%12s" % ("Name", "Size"))
        print("-" * 80)

        for name in files:
            if not process_vault_files(name, False, list_file):
                return False

        return True
    finally:
        close_vault()


def test_files():
    global files
    global opt_recursive

    if not open_vault(True):
        return False
    try:
        if len(files) == 0:
            files.append("*")
            opt_recursive = True

        print("%-68s%12s" % ("Name", "Result"))
        print("-" * 80)

        for name in files:
            if not process_vault_files(name, False, test_file):
                return False

        return True
    finally:
        close_vault()


def is_drive_letter(path_str):
    if not path_str:
        return False

    c = path_str[0]
    if c.isalpha() and len(path_str) == 2 and path_str[1] == ':':
        return True
    else:
        return False


def convert_relative_path_to_absolute(path_str):
    res = None
    if path_str and path_str.strip():
        res = path_str
        # Linux-specific case of using a home directory
        if path_str == "~" or path_str.startswith("~/"):
            home_dir = os.path.expanduser("~")
            if path_str == "~":
                return home_dir
            else:
                return os.path.join(home_dir, path_str[1:])
        elif not is_drive_letter(path_str):
            res = os.path.abspath(path_str)

            if res.startswith("\\\\") and not os.path.exists(res):
                print(f"The network folder '{res}' does not exist.")
            elif not os.path.exists(res):
                print(f"The path '{res}' does not exist.")

    return res


# ---------------------------------
#   Program body

banner()

if len(sys.argv) < 3:
    print_usage()
    exit(1)

if not handle_command():
    exit(1)

if not handle_parameters():
    exit(1)

try:
    if command == "a":
        add_files()
    elif command == "d":
        delete_files()
    elif command == "e":
        extract_files(False)
    elif command == "l":
        list_files()
    elif command == "t":
        test_files()
    elif command == "x":
        extract_files(True)
except CBFSVaultCbvaultError as e:
    print("ERROR: (%d) %s " % (e.code, e.message))
    exit(3)


