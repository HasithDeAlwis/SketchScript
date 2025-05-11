// -----------------------------
// AWS region
// -----------------------------
variable "aws_region" {
  description = "The AWS region to deploy to"
  type        = string
  default     = "us-east-1"
}

// -----------------------------
// EC2 instance type
// -----------------------------
variable "instance_type" {
  description = "The EC2 instance type"
  type        = string
  default     = "t3.micro" // want to use for now for free-tier eligibility
}

// -----------------------------
// SSH key pair name
// -----------------------------
variable "key_name" {
  description = "Name of the SSH key pair in AWS to access the instance"
  type        = string
}
