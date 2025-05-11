// -----------------------------
// Public IP of EC2
// -----------------------------
output "instance_public_ip" {
  description = "The public IP address of the EC2 instance"
  value       = aws_instance.web.public_ip
}

// -----------------------------
// My current IP
// -----------------------------
output "my_ip_address" {
  description = "The public IP Terraform detected from your machine"
  value       = "${trimspace(data.http.my_ip.body)}/32"
}
