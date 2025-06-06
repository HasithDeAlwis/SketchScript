import { quoteConstants } from "./quote.constants"

export function QuoteSection() {
  return (
    <section className="py-20">
      <div className="container">
        <div className="max-w-4xl mx-auto text-center">
          <div className="mb-6 font-serif text-6xl text-secondary">"</div>
          <blockquote className="mb-8 text-2xl italic font-medium md:text-3xl">
            {quoteConstants.quote}
          </blockquote>
          <div className="flex items-center justify-center">
            <div className="flex items-center justify-center w-12 h-12 mr-4 rounded-full bg-muted">
              <span className="text-lg font-medium">{quoteConstants.author.initials}</span>
            </div>
            <div className="text-left">
              <div className="font-medium">{quoteConstants.author.name}</div>
              <div className="text-sm text-foreground/70">{quoteConstants.author.title}</div>
            </div>
          </div>
        </div>
      </div>
    </section>
  )
}
