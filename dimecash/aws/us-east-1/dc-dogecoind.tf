resource "aws_instance" "dc-dogecoind" {

  ami           = "ami-030cd17b75425e48d"
  instance_type = "t2.small"
  key_name      = "dogecoind"
  security_groups = [ "ssh.tcp.22.open" ]
  associate_public_ip_address = "true"

  tags = {
    Name = ""
  }

}
