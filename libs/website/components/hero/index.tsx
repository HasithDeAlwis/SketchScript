import { useEffect, useRef } from "react"
import { Link } from "react-router";
import { Button } from "@shared/ui/button"
import { heroConstants } from "./hero.constants"

export function HeroSection() {
  const heroRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const handleMouseMove = (e: MouseEvent) => {
      if (!heroRef.current) return
      const { clientX, clientY } = e
      const { left, top, width, height } = heroRef.current.getBoundingClientRect()
      const x = (clientX - left) / width
      const y = (clientY - top) / height
      const moveX = (x - 0.5) * 100
      const moveY = (y - 0.5) * 100
      const elements = heroRef.current.querySelectorAll(".floating-element")
      elements.forEach((el) => {
        const htmlEl = el as HTMLElement
        const speedX = Number.parseFloat(htmlEl.dataset.speedX || "1")
        const speedY = Number.parseFloat(htmlEl.dataset.speedY || "1")
        htmlEl.style.transform = `translate(${moveX * speedX}px, ${moveY * speedY}px)`
      })
    }
    document.addEventListener("mousemove", handleMouseMove)
    return () => document.removeEventListener("mousemove", handleMouseMove)
  }, [])

  return (
    <section className="relative min-h-screen pt-20 overflow-hidden" ref={heroRef}>
      {/* Background elements */}
      <div className="absolute inset-0 hero-gradient"></div>
      <div
        className="absolute w-64 h-64 rounded-full top-1/4 left-1/4 bg-secondary opacity-5 floating-element"
        data-speed-x="0.5"
        data-speed-y="0.8"
      ></div>
      <div
        className="absolute w-48 h-48 rounded-full bottom-1/3 right-1/4 bg-accent opacity-5 floating-element"
        data-speed-x="-0.7"
        data-speed-y="-0.3"
      ></div>

      {/* Content */}
      <div className="container relative z-10 flex flex-col items-center justify-center min-h-[calc(100vh-80px)] text-center">
        <div className="inline-block px-4 py-1.5 mb-6 text-sm font-medium bg-secondary/10 text-secondary rounded-full">
          {heroConstants.badge}
        </div>

        <h1 className="max-w-4xl mb-6 text-5xl font-bold tracking-tight md:text-7xl">
          <span className="block">{heroConstants.title.line1}</span>
          <span className="relative inline-block">
            <span className="relative z-10">{heroConstants.title.highlight}</span>
            <span className="absolute left-0 right-0 h-3 transform bottom-2 bg-secondary/30 -z-10 -rotate-1"></span>
          </span>
        </h1>

        <p className="max-w-2xl mb-10 text-lg md:text-xl text-foreground/80">
          {heroConstants.subtitle}
        </p>

        <div className="flex flex-wrap justify-center gap-4">
          {heroConstants.buttons.map((btn) => (
            <Button
              key={btn.text}
              asChild
              size="lg"
              variant={btn.variant === "primary" ? "default" : btn.variant}
              className="rounded-full"
            >
              <Link to={btn.href} target={btn.href.startsWith('http') ? "_blank" : undefined}>
                {btn.variant === "outline" || btn.variant === "ghost" ? btn.icon : null}
                {btn.text}
                {btn.variant === "primary" ? btn.icon : null}
              </Link>
            </Button>
          ))}
        </div>

        {/* Code snippet (keep from current hero section) */}
        <div className="w-full max-w-lg mt-16">
          <div className="p-4 overflow-hidden font-mono text-sm text-left border rounded-lg bg-muted/50 backdrop-blur-sm border-muted">
            <div className="flex items-center mb-2 text-muted-foreground">
              <div className="w-3 h-3 mr-2 rounded-full bg-secondary"></div>
              <div className="w-3 h-3 mr-2 rounded-full bg-accent"></div>
              <div className="w-3 h-3 rounded-full bg-[#FFEFA1] mr-2"></div>
              <span className="text-xs">{heroConstants.codeSnippet.filename}</span>
            </div>
            <pre className="text-foreground/90">
              <code>{heroConstants.codeSnippet.code}</code>
            </pre>
          </div>
        </div>
      </div>
      {/* Scroll indicator ... */}
    </section>
  )
}
