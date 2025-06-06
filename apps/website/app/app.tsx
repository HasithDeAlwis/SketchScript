import { HeroSection } from "@website/components/hero"
import { FeaturesSection } from "@website/components/features"
import { RoadmapSection } from "@website/components/roadmap"
import { QuoteSection } from "@website/components/quotes"
import { PricingSection } from "@website/components/pricing"
import { CtaSection } from "@website/components/cta"
import { Navbar } from "@website/components/navbar"
import { Footer } from "@website/components/footer"
import { DemoSection } from "@website/components/demo"

export default function Home() {
  return (
    <main className="min-h-screen">
      <Navbar />
      <HeroSection />
      <FeaturesSection />
      <DemoSection />
      <QuoteSection />
      <RoadmapSection />
      <PricingSection />
      <CtaSection />
      <Footer />
    </main>
  )
}
