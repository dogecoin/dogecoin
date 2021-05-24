{
  python3Packages
, cardano-cli
}:

python3Packages.buildPythonPackage {
  version = "1.0.0";
  pname = "cardano-lib-py";
  src = ./.;
  propagatedBuildInputs = [
    cardano-cli
  ];
}
