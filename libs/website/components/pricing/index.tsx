import { Check } from "lucide-react"
import { Button } from "@shared/ui/button"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@shared/ui/card"
import { pricingConstants } from "./pricing.constants"

export function PricingSection() {
  return (
    <section id="pricing" className="py-20">
      <div className="container">
        <div className="mb-16 text-center">
          <h2 className="mb-4 text-3xl font-bold md:text-4xl">{pricingConstants.title}</h2>
          <p className="max-w-2xl mx-auto text-lg text-foreground/80">
            {pricingConstants.subtitle}
          </p>
        </div>

        <div className="grid max-w-5xl grid-cols-1 gap-8 mx-auto md:grid-cols-3">
          {pricingConstants.plans.map((plan) => (
            <Card
              key={plan.name}
              className={`relative overflow-hidden ${plan.popular ? "border-secondary shadow-lg shadow-secondary/10" : "border-muted"
                }`}
            >
              {plan.popular && (
                <div className="absolute top-0 right-0">
                  <div className="px-3 py-1 text-xs font-medium rounded-bl-lg bg-secondary text-background">
                    Most Popular
                  </div>
                </div>
              )}
              <CardHeader>
                <CardTitle className="text-xl">{plan.name}</CardTitle>
                <CardDescription>{plan.description}</CardDescription>
                <div className="mt-4">
                  <span className="text-3xl font-bold">{plan.price}</span>
                  {plan.period && <span className="ml-1 text-foreground/70">{plan.period}</span>}
                </div>
              </CardHeader>
              <CardContent>
                <ul className="space-y-2">
                  {plan.features.map((feature) => (
                    <li key={feature} className="flex items-start">
                      <Check className="w-5 h-5 mr-2 text-green-500 shrink-0" />
                      <span className="text-foreground/80">{feature}</span>
                    </li>
                  ))}
                </ul>
              </CardContent>
              <CardFooter>
                <Button className={`w-full ${plan.popular ? "bg-secondary text-background hover:bg-secondary/90" : "text-background"}`} disabled={plan.disabled}>
                  {plan.cta}
                </Button>
              </CardFooter>
            </Card>
          ))}
        </div>
      </div>
    </section>
  )
}
