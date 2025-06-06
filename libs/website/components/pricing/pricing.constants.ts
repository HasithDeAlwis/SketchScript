export const pricingConstants = {
  title: "Simple, Transparent Pricing",
  subtitle: "Choose the plan that works best for you and your team.",
  plans: [
    {
      name: "Starter",
      price: "Free",
      description: "Perfect for trying out SketchScript",
      features: ["Basic wireframing", "5 projects", "Export as PNG", "Community support"],
      cta: "Get Started",
      popular: false,
      disabled: false,
    },
    {
      name: "Pro",
      price: "$12",
      period: "/month",
      description: "For professional designers and developers",
      features: [
        "Unlimited projects",
        "Access to workspace",
        "Collaboration for up to 3 users",
      ],
      cta: "Coming Soon",
      popular: true,
      disabled: true,
    },
    {
      name: "Team",
      price: "$49",
      period: "/month",
      disabled: true,
      description: "For design teams and agencies",
      features: [
        "Everything in Pro",
        "Team collaboration",
        "Version history",
        "Dedicated support",
      ],
      cta: "Coming Soon",
      popular: false,
    },
  ],
}
