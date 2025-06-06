import { useState } from "react"
import { Card, CardDescription, CardHeader, CardTitle } from "@shared/ui/card"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@shared/ui/tabs"
import { Code, Paintbrush, Zap, Share2, Puzzle, Sparkles } from "lucide-react"

const features = [
  {
    id: "code",
    icon: <Code className="w-5 h-5" />,
    title: "Code-Based Design",
    description: "Design wireframes using a simple, intuitive domain-specific language.",
    //     content: (
    //       <div className="p-4 font-mono text-sm rounded-lg bg-muted/50">
    //         <pre>{`sketch "Dashboard" {
    //   header {
    //     logo "SketchScript"
    //     nav ["Home", "Projects", "Settings"]
    //   }

    //   sidebar {
    //     menu ["Dashboard", "Analytics", "Users"]
    //   }

    //   main {
    //     h1 "Welcome back!"
    //     stats {
    //       card "Total Users" value="1,234"
    //       card "Active Projects" value="42"
    //       card "Completion Rate" value="87%"
    //     }
    //   }
    // }`}</pre>
    //       </div>
    //     ),
  },
  {
    id: "speed",
    icon: <Zap className="w-5 h-5" />,
    title: "Lightning Fast",
    description: "Create complex wireframes in minutes instead of hours.",
    // content: (
    //   <div className="grid grid-cols-2 gap-4">
    //     <div className="flex flex-col items-center justify-center p-4 rounded-lg bg-muted/30">
    //       <div className="text-4xl font-bold text-secondary">10x</div>
    //       <div className="mt-2 text-sm text-center text-foreground/70">Faster than traditional wireframing</div>
    //     </div>
    //     <div className="flex flex-col items-center justify-center p-4 rounded-lg bg-muted/30">
    //       <div className="text-4xl font-bold text-accent">80%</div>
    //       <div className="mt-2 text-sm text-center text-foreground/70">Time saved on revisions</div>
    //     </div>
    //   </div>
    // ),
  },
  {
    id: "creative",
    icon: <Paintbrush className="w-5 h-5" />,
    title: "Unleash Creativity",
    description: "Focus on design thinking, not on dragging and dropping elements.",
    // content: (
    //   <div className="grid grid-cols-3 gap-2">
    //     {[1, 2, 3, 4, 5, 6].map((i) => (
    //       <div
    //         key={i}
    //         className="flex items-center justify-center rounded-lg aspect-square bg-gradient-to-br from-secondary/20 to-accent/20"
    //       >
    //         <div
    //           className={`w-8 h-8 rounded-md bg-foreground/10 transform ${i % 2 === 0 ? "rotate-3" : "-rotate-3"}`}
    //         ></div>
    //       </div>
    //     ))}
    //   </div>
    // ),
  },
  {
    id: "share",
    icon: <Share2 className="w-5 h-5" />,
    title: "Seamless Sharing",
    description: "Share your designs with a simple link or export to various formats.",
    // content: (
    //   <div className="flex flex-col space-y-2">
    //     <div className="flex items-center p-2 space-x-2 rounded-lg bg-muted/30">
    //       <div className="flex items-center justify-center w-8 h-8 rounded-full bg-secondary/20">
    //         <span className="text-xs">JS</span>
    //       </div>
    //       <div className="text-sm">John shared "Dashboard" with you</div>
    //     </div>
    //     <div className="flex items-center p-2 space-x-2 rounded-lg bg-muted/30">
    //       <div className="flex items-center justify-center w-8 h-8 rounded-full bg-accent/20">
    //         <span className="text-xs">AL</span>
    //       </div>
    //       <div className="text-sm">Alice exported "Login Page" as PNG</div>
    //     </div>
    //   </div>
    // ),
  },
  {
    id: "extensible",
    icon: <Puzzle className="w-5 h-5" />,
    title: "Extensible",
    description: "Create your own components and share them with the community.",
    //     content: (
    //       <div className="grid grid-cols-2 gap-2">
    //         <div className="p-3 rounded-lg bg-muted/30">
    //           <div className="mb-1 text-xs font-medium">Custom Component</div>
    //           <div className="font-mono text-xs">
    //             <pre>{`component DataTable {
    //   props: [data, columns]
    //   render: {
    //     table {
    //       thead { ... }
    //       tbody { ... }
    //     }
    //   }
    // }`}</pre>
    //           </div>
    //         </div>
    //         <div className="p-3 rounded-lg bg-muted/30">
    //           <div className="mb-1 text-xs font-medium">Usage</div>
    //           <div className="font-mono text-xs">
    //             <pre>{`DataTable {
    //   data: users
    //   columns: [
    //     "Name",
    //     "Email",
    //     "Role"
    //   ]
    // }`}</pre>
    //           </div>
    //         </div>
    //       </div>
    //     ),
  },
  {
    id: "ai",
    icon: <Sparkles className="w-5 h-5" />,
    title: "AI-Powered",
    description: "Generate wireframes from text descriptions or improve existing designs.",
    //     content: (
    //       <div className="p-3 rounded-lg bg-muted/30">
    //         <div className="mb-2 text-xs font-medium">Prompt</div>
    //         <div className="mb-3 text-xs italic">
    //           "Create a dashboard with a sidebar, stats cards, and a recent activity feed"
    //         </div>
    //         <div className="mb-1 text-xs font-medium">Generated Code</div>
    //         <div className="font-mono text-xs">
    //           <pre>{`sketch "AI Dashboard" {
    //   layout grid(12) {
    //     sidebar(span: 3) { ... }
    //     main(span: 9) { ... }
    //   }
    // }`}</pre>
    //         </div>
    //       </div>
    //     ),
  },
]

export function FeaturesSection() {
  const [activeTab, setActiveTab] = useState("code")

  return (
    <section id="features" className="py-20">
      <div className="container">
        <div className="mb-16 text-center">
          <h2 className="mb-4 text-3xl font-bold md:text-4xl">Why SketchScript?</h2>
          <p className="max-w-2xl mx-auto text-lg text-foreground/80">
            SketchScript combines the precision of code with the freedom of design, creating a unique wireframing
            experience.
          </p>
        </div>

        {/* <Tabs defaultValue="code" value={activeTab} onValueChange={setActiveTab} className="w-full max-w-4xl mx-auto">
          <TabsList className="grid grid-cols-3 mb-8 md:grid-cols-6">
            {features.map((feature) => (
              <TabsTrigger
                key={feature.id}
                value={feature.id}
                className="flex flex-col items-center py-3 px-2 space-y-2 data-[state=active]:bg-secondary/10"
              >
                <div className="p-1.5 rounded-full bg-muted">{feature.icon}</div>
                <span className="text-xs font-medium">{feature.title}</span>
              </TabsTrigger>
            ))}
          </TabsList>

          <div className="relative p-6 border rounded-lg bg-muted/20 border-muted">
            {features.map((feature) => (
              <TabsContent key={feature.id} value={feature.id} className="space-y-4">
                <div className="flex flex-col gap-6 md:flex-row md:items-start">
                  <div className="md:w-1/3">
                    <h3 className="mb-2 text-xl font-bold">{feature.title}</h3>
                    <p className="text-foreground/80">{feature.description}</p>
                  </div>
                  <div className="md:w-2/3">{feature.content}</div>
                </div>
              </TabsContent>
            ))}
          </div>
        </Tabs> */}

        <div className="grid grid-cols-1 gap-6 mt-16 md:grid-cols-3">
          {features.slice(0, 3).map((feature) => (
            <Card key={feature.id} className="card-hover border-muted/50">
              <CardHeader>
                <div className="flex items-center justify-center w-10 h-10 mb-2 rounded-full bg-secondary/10">
                  {feature.icon}
                </div>
                <CardTitle>{feature.title}</CardTitle>
                <CardDescription>{feature.description}</CardDescription>
              </CardHeader>
            </Card>
          ))}
        </div>
      </div>
    </section>
  )
}
