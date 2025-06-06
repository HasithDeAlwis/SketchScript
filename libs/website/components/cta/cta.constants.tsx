import { ArrowRight, FileText } from "lucide-react"

export const ctaConstants = {
  badge: "Ready to transform your workflow?",
  title: "Start Building with SketchScript Today",
  subtitle:
    "Join designers and developers who are creating beautiful wireframes faster than ever. No more dragging and dropping—just write code and watch your designs come to life.",
  buttons: [
    {
      text: "Start Building",
      href: "https://sketchscript.dev",
      variant: "default" as const,
      icon: (
        <ArrowRight className="w-4 h-4 ml-2 transition-transform group-hover:translate-x-1" />
      ),
    },
    {
      text: "Read the Docs",
      href: "https://docs.sketchscript.dev",
      variant: "outline" as const,
      icon: (
        <FileText className="w-4 h-4 ml-2 transition-transform group-hover:translate-x-1" />
      ),
    },
  ],
  benefits: [
    { text: "Free to start" },
    { text: "No credit card required" },
    { text: "Cancel anytime" },
  ],
  codeSnippet: {
    code: `<Box 1.0 flex start>
  <Text 0.2 "Start Building">
  <Text 0.3 "Join designers...">
  <Button 0.4 "Get Started" />`,
  },
}
