#!/usr/bin/env python3

import argparse
import os
import pwd
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
    command line. Switch manually from root rights needed at startup
    to unprivileged user.

    Manually execve + setuid/setgid to run process as pid 1,
    to manage a single process in a container & more predictive
    signal handling.
    """
    #Prepare execve(2) arguments
    if EXECUTABLE in ["dogecoind", "dogecoin-qt"]:
        executable_args.append("-printtoconsole")

    dogecoin_executable = shutil.which(EXECUTABLE)
    execve_args = [dogecoin_executable] + executable_args

    #Switch process from root to user.
    #Equivalent to use gosu or su-exec
    user_info = pwd.getpwnam(os.environ['USER'])
    os.setgid(user_info.pw_gid)
    os.setuid(user_info.pw_uid)

    #Run process and remove environment by security.
    os.execve(dogecoin_executable, execve_args, {})

if __name__ == "__main__":
    EXECUTABLE = container_executable()

    create_datadir()

    EXECUTABLE_ARGS = convert_env()
    EXECUTABLE_ARGS += sys.argv[1:]

    run_executable(EXECUTABLE_ARGS)
