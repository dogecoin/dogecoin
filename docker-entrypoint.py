#!/usr/bin/env python3

import argparse
import os
import shutil
import sys
import subprocess

def container_executable_path():
    return "/{}/bin/{}".format(os.environ['USER'], EXECUTABLE)

def container_executable_man():
    user = os.environ['USER']
    return "/{}/share/man/man1/{}.1".format(user, EXECUTABLE)

def container_executable():
    """
    Get the executable used as a docker command, or
    use dogecoind by default.
    """
    dogecoin_executables = [
                "dogecoind",
                "dogecoin-qt",
                "dogecoin-cli",
                "dogecoin-tx",
            ]

    executable = "dogecoind"
    if len(sys.argv) >= 2 and sys.argv[1] in dogecoin_executables:
        executable = sys.argv.pop(1)

    return executable

def executable_options():
    """
    Retrieve available options for container executable, using his man.

    Allow to use existing options as environment variables.
    """
    # Use dogecoind --help menu to find available options
    man_path = container_executable_man()
    raw_options = subprocess.check_output(
            f"bash -c \"man {man_path} | grep '^ *-'\"",
            shell=True
            ).decode().strip()

    #Cleanup options fields, keep name and remove description
    options = []
    for option_entry in raw_options.split("\n"):
        option_entry = option_entry.strip().split("=")[0]
        options.append(option_entry[1:])

    return options

def create_datadir():
    """
    Create data directory user by dogecoind.
    Get configuration from arguments or environment variables.

    Do not let dogecoind create the directory,
    allow creation for docker volumes while root.
    """
    #Find datadir within argv
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("-datadir", "--datadir")
    argv, _ = parser.parse_known_args()

    #Select between datadir from argv or environment, env by default
    datadir = argv.datadir or os.environ.get("DATADIR")

    os.makedirs(datadir, exist_ok=True)

    #Grant user rights to dogecoind datadir
    user = os.environ["USER"]
    subprocess.run(["chown", "-R", f"{user}:{user}", datadir])

def convert_env():
    """
    Convert environment variables into dogecoind arguments.

    Environment variables used for configuration are options from dogecoind
    man, in upper case and "-" convert to "_".

    Exemple:
    -rpcuser is RPCUSER
    -help-debug is HELP_DEBUG

    Environment variables can be used with an empty value if the
    corresponding option do not expect a value.
    """
    dogecoind_options = executable_options()
    option_to_env = lambda opt_value : opt_value.upper().replace("-", "_")

    cli_arguments = []
    for option in dogecoind_options:
        env_option = os.environ.get(option_to_env(option))

        if env_option is not None:
            cli_option = "-" + option
            cli_option += "=" + env_option if env_option else ""
            cli_arguments.append(cli_option)

    return cli_arguments

def run_daemon(executable_args):
    """
    Run dogecoind with given arguments.
    Use container user (dogecoin by default), use
    gosu to manage permission.
    """
    if EXECUTABLE in ["dogecoind", "dogecoin-qt"]:
        executable_args.append("-printtoconsole")

    gosu_executable = shutil.which("gosu")
    dogecoin_executable = container_executable_path()
    gosu_args = [gosu_executable, os.environ['USER'], dogecoin_executable]
    gosu_args += executable_args

    os.execve(gosu_executable, gosu_args, os.environ)

if __name__ == "__main__":
    EXECUTABLE = container_executable()

    create_datadir()

    EXECUTABLE_ARGS = convert_env()
    EXECUTABLE_ARGS += sys.argv[1:]

    run_daemon(EXECUTABLE_ARGS)
