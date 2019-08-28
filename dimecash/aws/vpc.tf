resource "aws_vpc" "dimecash_vpc" {
  cidr_block = "172.31.0.0/16"
  tags = {
    Name = "dimecash_vpc"
  }

}
