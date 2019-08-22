data "aws_subnet_ids" "monica" {
  vpc_id = aws_vpc.monica_vpc.id
}

resource "aws_subnet" "private_subnet" {
  vpc_id     = "${aws_vpc.monica_vpc.id}"
  cidr_block = "172.31.0.0/16"
  tags = {  Name="monica_priv" } 


#    Network = 172.31.0.0
# Usable IPs = 172.31.0.1 to 172.31.255.254 for 65534 
#  Broadcast = 172.31.255.255
#    Netmask = 255.255.0.0
# Wildcard Mask = 0.0.255.255
}

# resource "aws_subnet" "public_subnet" {
#  vpc_id    = "vpc-03f1e34b71124a15f"
#  cidr_block = "172.31.128.0/28"

#    Network = 172.31.128.0
# Usable IPs = 172.31.128.1 to 172.31.128.14 for 14
#  Broadcast = 172.31.128.15
#    Netmask = 255.255.255.240
# Wildcard Mask = 0.0.0.15
# }
