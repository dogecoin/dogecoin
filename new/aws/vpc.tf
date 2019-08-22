
data "aws_vpc" "monica_vpc" {}

resource "aws_vpc" "main" {
  cidr_block = "172.31.0.0/16"
  tags = {
    Name = "monica_vpc"
  }

}
