import { useState } from "react"
import { Link } from "react-router";
import { Button } from "@shared/ui/button"
import { Sparkles } from "lucide-react"
import { ctaConstants } from "./cta.constants"

export function CtaSection() {
  const [isHovering, setIsHovering] = useState(false)

  return (
    <section className="relative py-24 overflow-hidden">
      {/* Background elements */}
      <div className="absolute inset-0 opacity-50 hero-gradient"></div>
      <div className="absolute top-0 left-0 w-full h-full">
        <div className="absolute top-[15%] left-[10%] w-16 h-16 rounded-full bg-secondary opacity-10"></div>
        <div className="absolute top-[60%] right-[15%] w-24 h-24 rounded-full bg-accent opacity-10"></div>
        <div className="absolute bottom-[20%] left-[20%] w-20 h-20 rounded-full bg-[#FFEFA1] opacity-10"></div>
      </div>

      <div className="container relative z-10">
        <div className="max-w-4xl mx-auto text-center">
          <div
            className="relative inline-block mb-6"
            onMouseEnter={() => setIsHovering(true)}
            onMouseLeave={() => setIsHovering(false)}
          >
            <Sparkles
              className={`h-6 w-6 text-secondary absolute -top-3 -left-4 transition-all duration-500 ${isHovering ? "opacity-100 transform rotate-12 scale-110" : "opacity-50"
                }`}
            />
            <span className="text-sm font-medium bg-secondary/10 text-secondary px-4 py-1.5 rounded-full">
              Ready to transform your workflow?
            </span>
            <Sparkles
              className={`h-6 w-6 text-accent absolute -top-3 -right-4 transition-all duration-500 ${isHovering ? "opacity-100 transform -rotate-12 scale-110" : "opacity-50"
                }`}
            />
          </div>

          <h2 className="mb-6 text-3xl font-bold md:text-5xl">{ctaConstants.title}</h2>

          <p className="max-w-2xl mx-auto mb-10 text-lg md:text-xl text-foreground/80">
            {ctaConstants.subtitle}
          </p>

          <div className="flex flex-col items-center justify-center gap-4 mb-12 sm:flex-row">
            {ctaConstants.buttons.map((btn) => (
              <Button
                key={btn.text}
                asChild
                size="lg"
                className={`px-8 rounded-full ${btn.variant === 'outline' ? 'border-primary/20 hover:border-primary/40 text-background' : 'bg-secondary text-background hover:bg-secondary/90'} `}
              >
                <Link to={btn.href}>
                  {btn.text}
                  {btn.icon}
                </Link>
              </Button>
            ))}
          </div>

          <div className="flex flex-wrap items-center justify-center mb-8">
            {ctaConstants.benefits.map((benefit, idx) => (
              <div key={benefit.text}>
                <span
                  className="px-1 py-1 text-xs font-medium rounded-full"
                >
                  {benefit.text}
                </span>
                {idx < ctaConstants.benefits.length - 1 && (
                  <span className="mx-1 text-xs text-gray-400 align-middle select-none" aria-hidden="true">&bull;</span>
                )}
              </div>
            ))}
          </div>

          <article className="max-w-xs mx-auto mt-16 transform opacity-30 -rotate-3">
            <div className="p-3 overflow-hidden font-mono text-xs text-left border rounded-lg bg-muted/50 backdrop-blur-sm border-muted">
              <pre className="text-foreground/90">
                <code>{ctaConstants.codeSnippet.code}</code>
              </pre>
            </div>
          </article>

        </div>
      </div>
    </section>
  )
}
