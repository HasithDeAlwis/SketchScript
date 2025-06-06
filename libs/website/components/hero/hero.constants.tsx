import { ArrowRight, Github, FileText } from "lucide-react";

export const heroConstants = {
  badge: "Introducing SketchScript 1.0",
  title: {
    line1: "Design wireframes with",
    highlight: "code",
  },
  subtitle:
    "A DSL for creating wireframes to help designers and front-end developers speed up and improve their workflows.",
  buttons: [
    {
      text: "Get Started",
      href: "https://app.sketchscript.dev",
      variant: "primary" as const,
      icon: <ArrowRight className="w-4 h-4 ml-2" />,
    },
    {
      text: "GitHub",
      href: "https://github.com/HasithDeAlwis/sketchscript",
      variant: "outline" as const,
      icon: <Github className="w-4 h-4 mr-2" />,
    },
    {
      text: "Documentation",
      href: "https://docs.sketchscript.dev",
      variant: "ghost" as const,
      icon: <FileText className="w-4 h-4 mr-2" />,
    },
  ],
  codeSnippet: {
    filename: "start-now.sks",
    code: `<Box 1.0 flex start>
  <Box 0.2 flex-h>
    <Img 0.2 "SketchScript" />
    <Text 0.7 "Home, Features, Pricing"] />

  <Box 0.4 hero>
    <Text 0.5 "Design with code" h1 />
    <Text 0.6 "Fast, flexible, and fun." />
    <Button 0.7 "Get Started" primary />
`,
  },
}
