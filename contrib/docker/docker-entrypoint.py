#!/usr/bin/env python3

import argparse
import os
import shutil
import sys
import subprocess

def container_executable():
    """
    Get the executable used in the docker command, or
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
    """
    raw_options = subprocess.check_output(
            f"bash -c \"man {EXECUTABLE} | grep '^ *-'\"",
            shell=True
            ).decode().strip()

    #Cleanup options fields
    options = []
    for option_entry in raw_options.split("\n"):
        option_entry = option_entry.strip().split("=")[0]
        options.append(option_entry[1:])

    return options

def create_datadir():
    """
    Create data directory used by dogecoin daemon.

    Create manually the directory while root at container creation,
    root rights needed to create folder with host volume.

    Use of `gosu` instead of USER instruction in Dockerfile
    for this particular reason.
    """
    #Try to get datadir from argv
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("-datadir", "--datadir")
    argv, _ = parser.parse_known_args()

    #Try to get datadir from environment
    datadir = argv.datadir or os.environ.get("DATADIR")

    os.makedirs(datadir, exist_ok=True)

    user = os.environ["USER"]
    subprocess.run(["chown", "-R", f"{user}:{user}", datadir])

def convert_env():
    """
    Convert existing environment variables into command line arguments.

    Options from executable man pages are searched in the environment,
    converting options in upper case and convert "-" to "_".

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

def run_executable(executable_args):
    """
    Run selected dogecoin executable with arguments from environment and
    command line. Use `gosu` to switch from root needed at startup
    to unprivileged user.
    """
    if EXECUTABLE in ["dogecoind", "dogecoin-qt"]:
        executable_args.append("-printtoconsole")

    gosu_executable = shutil.which("gosu")
    dogecoin_executable = shutil.which(EXECUTABLE)
    gosu_args = [gosu_executable, os.environ['USER'], dogecoin_executable]
    gosu_args += executable_args

    os.execve(gosu_executable, gosu_args, os.environ)

if __name__ == "__main__":
    EXECUTABLE = container_executable()

    create_datadir()

    EXECUTABLE_ARGS = convert_env()
    EXECUTABLE_ARGS += sys.argv[1:]

    run_executable(EXECUTABLE_ARGS)
