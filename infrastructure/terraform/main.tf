// These comments are to document my learning so I understand AWS and Terraform better

// creates a variable, this is my IP address
data "http" "my_ip" {
  url = "https://ipv4.icanhazip.com"
}

// tells Terraform that AWS is the provider, and what region we are in
provider "aws" {
  region = var.aws_region
}

// documents the VPC that we are using
// a VPC is a internal network for the EC2 instance to run in
// the CIDR block essentially allocates what IP addresses are to be allocated to the VPC
resource "aws_vpc" "main" {
  cidr_block = "10.0.0.0/16"
}

// a subnet is a smaller group of IP addresses inside the VPC
// this is in the zone us-east-1a
resource "aws_subnet" "main" {
  vpc_id            = aws_vpc.main.id // links to the VPC we created
  cidr_block        = "10.0.1.0/24"
  availability_zone = "us-east-1a"
}

// enables outbound internet access to the VPC we provisioned
// we need this because EC2 instances to DO NOT have access to the internet by default
resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.main.id // just links the VPC
}

// "A route table contains a set of rules, called routes, that determine where network traffic from your subnet or gateway is directed." from AWS website
// basically this is how we tell AWS to route external traffic (like SSH or API calls) through the IGW, allows public access to the EC2 instance
// NOTE: this only cares about OUTBOUND traffic, has nothing to do with inbound
// therefore, this helps with routing WITHIN the VPC
resource "aws_route_table" "r" {
  vpc_id = aws_vpc.main.id // link to the VPC

  // route basically says send all IPv4 addresses not in the VPC through the internet gateway
  route {
    cidr_block = "0.0.0.0/0" // match all IPv4
    gateway_id = aws_internet_gateway.gw.id
  }
}

// Attaches the route table we made to the subnet, applying those internet routing rules to it
resource "aws_route_table_association" "a" {
  subnet_id      = aws_subnet.main.id // tells the subnet how to route traffic
  route_table_id = aws_route_table.r.id
}


// creates a secruity group, which acts as a firewall of sorts
// controls what is allowed to talk to the instance, and what the instance can talk to
// this is based on port, protocol, direction (inbound vs outbound), source/desitination ip
resource "aws_security_group" "ssh_sg" {
  // just a name visible in the AWS console, means mnothing
  name        = "allow_ssh"
  description = "Allow SSH inbound traffic"
  // security groups are only created INSIDE a vpc, cannot use the same security group in another vpc
  vpc_id = aws_vpc.main.id

  // what is allowed to talk to the resource (inbound traffic)
  // this is for SSH
  ingress {
    description = "SSH"
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["${trimspace(data.http.my_ip.body)}/32"] // only my IP address can now ssh into this
  }
  // this is for HTTPS
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  // what the instance is allowed to talk to (outbound traffic)
  egress {
    // 0, 0, -1 is special syntax, means that this can talk to anything on the internet, allows you to donwload packages, talk to 3rd party API's etc.
    // not a secruity concern
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}


// super simple, just defines what the AWS resource is allowed to acces
resource "aws_iam_role" "ec2_role" {
  name = "my-istance-role-ec2"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })
}

// policy defines what a role is allowed to do
resource "aws_iam_role_policy_attachment" "attach_s3_readonly" {
  role = aws_iam_role.ec2_role.name
  // read only access to S3 bucket (all S3 buckets)
  policy_arn = "arn:aws:iam::aws:policy/AmazonS3ReadOnlyAccess"
}

// this is what provisions a ec2 instance for us
resource "aws_instance" "web" {
  ami                    = "ami-0c101f26f147fa7fd" // Amazon Linux 2023 (t3.micro) - update based on region
  instance_type          = var.instance_type
  subnet_id              = aws_subnet.main.id
  key_name               = var.key_name
  vpc_security_group_ids = [aws_security_group.ssh_sg.id] // links the secruity group we made to this

  // attach the iam profile to the ec2 instance
  iam_instance_profile = aws_iam_instance_profile.ec2_instance_profile.name

  tags = {
    Name = "api-ec2"
  }
}

