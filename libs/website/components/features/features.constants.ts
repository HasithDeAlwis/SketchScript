export const featuresConstants = {
  title: "Why SketchScript?",
  subtitle:
    "SketchScript combines the precision of code with the freedom of design, creating a unique wireframing experience.",
  features: [
    {
      id: "code",
      icon: "Code",
      title: "Code-Based Design",
      description: "Design wireframes using a simple, intuitive domain-specific language.",
      codeExample: `sketch "Dashboard" {
  header {
    logo "SketchScript"
    nav ["Home", "Projects", "Settings"]
  }
  
  sidebar {
    menu ["Dashboard", "Analytics", "Users"]
  }
  
  main {
    h1 "Welcome back!"
    stats {
      card "Total Users" value="1,234"
      card "Active Projects" value="42"
      card "Completion Rate" value="87%"
    }
  }
}`,
    },
    {
      id: "speed",
      icon: "Zap",
      title: "Lightning Fast",
      description: "Create complex wireframes in minutes instead of hours.",
      stats: [
        { value: "10x", label: "Faster than traditional wireframing" },
        { value: "80%", label: "Time saved on revisions" },
      ],
    },
    {
      id: "creative",
      icon: "Paintbrush",
      title: "Unleash Creativity",
      description: "Focus on design thinking, not on dragging and dropping elements.",
    },
    {
      id: "share",
      icon: "Share2",
      title: "Seamless Sharing",
      description: "Share your designs with a simple link or export to various formats.",
      activities: [
        { user: "JS", name: "John", action: 'shared "Dashboard" with you' },
        { user: "AL", name: "Alice", action: 'exported "Login Page" as PNG' },
      ],
    },
    {
      id: "extensible",
      icon: "Puzzle",
      title: "Extensible",
      description: "Create your own components and share them with the community.",
      componentExample: {
        definition: `component DataTable {
  props: [data, columns]
  render: {
    table {
      thead { ... }
      tbody { ... }
    }
  }
}`,
        usage: `DataTable {
  data: users
  columns: [
    "Name",
    "Email",
    "Role"
  ]
}`,
      },
    },
    {
      id: "ai",
      icon: "Sparkles",
      title: "AI-Powered",
      description: "Generate wireframes from text descriptions or improve existing designs.",
      aiExample: {
        prompt: "Create a dashboard with a sidebar, stats cards, and a recent activity feed",
        generated: `sketch "AI Dashboard" {
  layout grid(12) {
    sidebar(span: 3) { ... }
    main(span: 9) { ... }
  }
}`,
      },
    },
  ],
  cards: [
    {
      icon: "Code",
      title: "Code-Based Design",
      description: "Design wireframes using a simple, intuitive domain-specific language.",
    },
    {
      icon: "Zap",
      title: "Lightning Fast",
      description: "Create complex wireframes in minutes instead of hours.",
    },
    {
      icon: "Paintbrush",
      title: "Unleash Creativity",
      description: "Focus on design thinking, not on dragging and dropping elements.",
    },
  ],
}
